package Devel::Xref;

our $VERSION = '0.55';

=head1 NAME

Devel::Xref - Generates cross reference database for use by Perl programs.

=head1 SYNOPSIS

    use Devel::Xref qw(rebuild defs callers);

    rebuild;
    for (defs 'foo') {
        printf "%s:%d: sub %s\::foo() defined\n", @{$_}[0..2];
    }

    for (callers 'foo') {
        printf "%s:%d: sub foo() called by %s\::%s().\n", @{$_}[0..3];
    }

=head1 DESCRIPTION

C<Devel::Xref> is intended as a programmatic interface to the
information supplied by L<B::Xref>.  It is intended to be a component
for interactive Perl development, with other packages providing a
friendly interface to the raw information it extracts.  C<B::Xref>
could be seen as an example of this sort of user-level tool, if it
weren't for the fact that this module was created later, and stole
most of its code.

=cut

use strict;
use warnings;
no warnings 'uninitialized';
use Config;
use Cwd 'abs_path';
use B qw(peekop class comppadlist main_start svref_2object walksymtable
         OPpLVAL_INTRO SVf_POK OPpOUR_INTRO OPf_MOD OPpDEREF_HV OPpDEREF_AV
	 cstring);

=head2 Variables

=over

=item C<%call>

A map of subs to call locations and callers

=item C<%callby>

A map of subs to subs called.

=item C<%def>

A map of subs to definitions.

=item C<%module_subs>

A map of packages to subs defined.

=item C<%var_use>

A map of global/package variables to uses.

=item C<%var_def>

A map of global/package variables to definitions (usually empty, since
it only picks up local (...) declarations.

=item C<%module_files>

A map of module names to containing files.

=item C<%file_modules>

A map of files to module names.

=back

=cut

our %call;
our %callby;
our %def;
our %module_subs;
our %var_def;
our %var_use;
our %module_files;
our %file_modules;

require Exporter;
our @ISA = qw(Exporter);
my @most = qw(redefined forget rebuild callers callees defs
	      var_defs var_uses
	      mod_subs mod_files mod_decls mod_apropos
	      apropos var_apropos file_apropos);
our @EXPORT_OK = (@most,
    qw(xref_definitions xref_object xref_main
       %call %callby %def %module_subs
       %var_use %var_def %module_files %file_modules));

our %EXPORT_TAGS =
    (all => \@EXPORT_OK,
     most => \@most);

######################################################################
## Xref state variables:

sub UNKNOWN { ["?", "?", "?"] }

my @pad;			# lexicals in current pad
				# as ["(lexical)", type, name]
my @padval;
our %done;			# keyed by $$op: set when each $op is done
my $top = UNKNOWN;		# shadows top element of stack as
				# [pack, type, name] (pack can be "(lexical)")
our $file;			# shadows current filename
my $line;			# shadows current line number
our $subname;			# shadows current sub name
our @todo = ();			# List of CVs that need processing

our $DEBUG = 0;
sub dprint {
    my $type = shift;
    my $res = "@_";
    $res =~ s/%//g;
    print STDERR "@_" if $DEBUG =~ /$type/;
}

my %code = (intro => "i", used => "",
	    subdef => "s", subused => "&",
	    formdef => "f", meth => "->");


=item C<guess_module_file($pack, $ofile)>

XXX: it turns out that rooting around trying to figure out the file
ourselves is more reliable than what we grab from the op.  Are we
doing this wrong?

=cut

sub guess_module_file {
    my ($pack, $ofile) = @_;
    my $file;

    # XXX: is this why we get the bogus defs?
    return undef if $ofile =~ /Exporter\.pm$/;
    # Try for standard translation in %INC:
    (my $fn = $pack) =~ s/::/\//g;
    if (exists $INC{"$fn.pm"}) {
	return $INC{"$fn.pm"};
    }

    # Try what they told us:
    chomp $ofile;
    return $ofile if -f $ofile;

    # Try our earlier guess of a module file:
    if (exists $module_files{$pack}
	&& scalar(keys %{$module_files{$pack}}) == 1) {
	my ($m) = grep /\Q$ofile\E/, keys %{$module_files{$pack}};
	return $m if $m;
    }

    # Try "parent" packages:
    while ($fn =~ s|/?[^/]+$|| && !$file) {
	$file ||= $INC{"$fn.pm"};
    }

    if ($file && $file !~ /^\//) {
	$file = abs_path($file);
    }

    if (!$file || !-f $file) {
	undef $file;
    }
    $file;
}

# XXX: should weed through the code below so it only generates decent
# package names, but this will fix it for now.
sub realpack {
    my $p = shift;
    if (!defined $p || $p eq '?' || $p eq '(method)') {
	return undef;
    } elsif ($p eq '') {
	return 'main';
    } else {
	return $p;
    }
}

# Turn a possibly-qualified name into a package and basename.
sub split_name {
    local $_ = shift;
    my ($p, $s);
    if (/^(.*)::(.+)$/) {
	($p, $s) = ($1, $2);
    } else {
	($p, $s) = ('main', $_);
    }
    undef $s if $s eq '?';
    ($p, $s);
}

sub process {
    my ($var, $event) = @_;
    my ($pack, $type, $name) = @$var;
    $pack = realpack($pack);
    dprint 'loud', "Processing $event: @$var ($subname)";
    if ($type eq "*") {
	if ($event eq "used" || $event eq 'set') {
	    return;
	} elsif ($event eq "subused") {
	    $type = "&";
	} elsif ($event eq "meth") {
	    $type = '->';
	}
    }
    $type =~ s/(.)\*$/$1/g;
    $file = guess_module_file($pack, $file);
    if (defined($file)) {
	if ($pack) {
	    $module_files{$pack}{$file}++;
	    $file_modules{$file}{$pack}++;
	}
    }

    if (($type eq '&' || $type eq '->') && $subname ne '(definitions)') {
	# Handle caller/callee relations
	my ($spack, $sname) = split_name($subname);
	# XXX: this is gross, but otherwise Expoerter seems to fool us.
	if ($file && !exists $def{$sname} || !exists $def{$sname}{$spack}) {
	    $def{$sname}{$spack} = { file => $file, line => undef };
	}

	push @{$call{$name}{$pack}},
	{ # file => $file,	# This is actually $sname's file...
	  sub => $sname,
	  package => $spack,
	  line => $line
	};

	push @{$callby{$sname}{$spack}}, { sub => $name, package => $pack };
    } elsif ($type eq 's' || $subname eq '(definitions)') {
	# Handle definition
	if ($file) {
	    my $obj = { file => $file, line => $line };
	    $module_subs{$pack}{$name} = $obj;
	    $def{$name}{$pack} = $obj;
	    dprint 'def', "$pack\::$name defined at $line\n";
	}
    } elsif ($name !~ /^[\x00-\x1f^] | ^\d+$ | ^[\W_]$
		       | ^(?:ENV|INC|STD(?:IN|OUT|ERR)|SIG)$ /x
	     && realpack($pack)) {
	# Variables, but ignore specials and lexicals
	my ($spack, $sname) = split_name($subname);
	if ($event eq 'intro') {
	    $var_def{$name}{$pack} =
	    { file => $file,
	      package => $spack,
	      line => $line,
	      sub => $sname,
	    };
	} elsif ($event eq 'used' || $event eq 'set') {
	    push @{$var_use{$name}{$pack}},
	    { file => $file,
	      package => $spack,
	      line => $line,
	      sub => $sname,
	      assign => ($event eq 'set'),
	    };
	} else {
	    dprint 'ignore', "Ignoring var event $event";
	}
    } else {
	dprint 'ignore', "Ignoring $type event $event";
    }
}

# Because the CV's line number points to the end of the sub, we guess
# a line number based on the first pp_nextstate seen in the sub.
# XXX: unused for now -- fix these up later.
sub update_line_number {
    my ($pack, $name) = split_name($subname);
    my $found;
    my $l = $line - 1; # because we usually see "sub foo {\n    first_stmt...}"
    if ($pack && exists $def{$name} && exists $def{$name}{$pack}
	&& $l < $def{$name}{$pack}{line}) {
	$def{$name}{$pack}{line} = $l;
    }
}

sub load_pad {
    my $padlist = shift;
    my ($namelistav, $vallistav, @namelist, $ix);
    @pad = ();
    @padval = ();
    return if class($padlist) eq "SPECIAL";
    ($namelistav,$vallistav) = $padlist->ARRAY;
    @namelist = $namelistav->ARRAY;
    for ($ix = 1; $ix < @namelist; $ix++) {
	my $namesv = $namelist[$ix];
	next if class($namesv) eq "SPECIAL";
	my ($type, $name) = $namesv->PV =~ /^(.)([^\0]*)(\0.*)?$/;
	$pad[$ix] = [undef, $type, $name];
    }
    if ($Config{useithreads}) {
	my (@vallist);
	@vallist = $vallistav->ARRAY;
	for ($ix = 1; $ix < @vallist; $ix++) {
	    my $valsv = $vallist[$ix];
	    next unless class($valsv) eq "GV";
	    # these pad GVs don't have corresponding names, so same @pad
	    # array can be used without collisions
	    $pad[$ix] = [$valsv->STASH->NAME, "*", $valsv->NAME];
	}
    }
    @padval = $vallistav->ARRAY;
}

sub xref {
    my $start = shift;
    my $op;
    for ($op = $start; $$op; $op = $op->next) {
	last if $done{$$op}++;
	my $opname = $op->name;
	if ($opname =~ /^(or|and|mapwhile|grepwhile|range|cond_expr)$/) {
	    xref($op->other);
	} elsif ($opname eq "match" || $opname eq "subst") {
	    xref($op->pmreplstart);
	} elsif ($opname eq "substcont") {
	    xref($op->other->pmreplstart);
	    $op = $op->other;
	    redo;
	} elsif ($opname eq "enterloop") {
	    xref($op->redoop);
	    xref($op->nextop);
	    xref($op->lastop);
	} elsif ($opname eq "subst") {
	    xref($op->pmreplstart);
	} else {
	    no strict 'refs';
	    my $ppname = "pp_$opname";
	    &$ppname($op) if defined(&$ppname);
	}
    }
}

sub xref_cv {
    my $cv = shift;
    my $pack = $cv->GV->STASH->NAME;
    local $subname = ($pack eq "main" ? "" : "$pack\::") . $cv->GV->NAME;
    load_pad($cv->PADLIST);
    xref($cv->START);
}

sub xref_object {
    my $cvref = shift;
    local (@todo, %done);
    my $cv = svref_2object($cvref);
    xref_cv($cv);
    dprint 'todo', "todo = (@todo)";
    my $gv = $cv->GV;
    process([$gv->STASH->NAME, '&', $gv->NAME], 'subdef');
}

sub xref_main {
    $subname = "(main)";
    load_pad(comppadlist);
    xref(main_start);
    while (@todo) {
	xref_cv(shift @todo);
    }
}

sub pp_nextstate {
    my $op = shift;
    $file = $op->file;
    die "pp_nextstate: $file" if $file =~ /::/;
    $line = $op->line;
#    update_line_number;
    $top = UNKNOWN;
}

sub use_type($) {
    my ($op) = @_;
    if ($op->private & (OPpLVAL_INTRO | OPpOUR_INTRO)) {
	'intro';
    } elsif ($op->flags & OPf_MOD
	     && !($op->private & (OPpDEREF_HV | OPpDEREF_AV))) {
	'set';
    } else {
	'used';
    }
}

sub pp_padsv {
    my $op = shift;
     $top = $pad[$op->targ];
#     process($top, $op->private & OPpLVAL_INTRO ? "intro" : "used");
}

sub pp_padav { pp_padsv(@_) }
sub pp_padhv { pp_padsv(@_) }

sub deref {
    my ($op, $var, $as) = @_;
    $var->[1] = $as . $var->[1];
    process($var, use_type $op);
}

sub pp_rv2cv { deref(shift, $top, "&"); }
sub pp_rv2hv { deref(shift, $top, "%"); }
sub pp_rv2sv { deref(shift, $top, "\$"); }
sub pp_rv2av { deref(shift, $top, "\@"); }
sub pp_rv2gv { deref(shift, $top, "*"); }

sub pp_gvsv {
    my $op = shift;
    my $gv;
    if ($Config{useithreads}) {
	$top = $pad[$op->padix];
	$top = UNKNOWN unless $top;
	$top->[1] = '$';
    }
    else {
	$gv = $op->gv;
	$top = [$gv->STASH->NAME, '$', $gv->SAFENAME];
    }
    process($top, use_type $op);
}

sub pp_gv {
    my $op = shift;
    my $gv;
    if ($Config{useithreads}) {
	$top = $pad[$op->padix];
	$top = UNKNOWN unless $top;
	$top->[1] = '*';
    }
    else {
	$gv = $op->gv;
	$top = [$gv->STASH->NAME, "*", $gv->SAFENAME];
    }
    process($top, use_type $op);
}

my $lastclass;

sub pp_const {
    my $op = shift;
    my $sv = $op->sv;
    # constant could be in the pad (under useithreads)
    if ($$sv) {
	$top = [undef, "",
		(class($sv) ne "SPECIAL" && $sv->FLAGS & SVf_POK)
		? $sv->PV : undef];
    }
    else {
	$top = $pad[$op->targ];
	my $pv = $padval[$op->targ];
	if (class($pv) eq 'PV') {
	    $pv = $pv->PV;
	    $lastclass = $pv if class($sv) eq 'SPECIAL'
		&& ($op->private & 64); # bareword
	} else {
	    $pv = "XXX: ".class($pv);
	}
	dprint 'method', "blah constant ".$op->targ." pad = `$top'/`$pv'";
	$top = UNKNOWN unless $top;
    }
}

sub pp_method {
    my $op = shift;
    $top = [$lastclass || "(method)", "->".$top->[1], $top->[2]];
    dprint 'method', "pp_method($top->[1])";
    undef $lastclass;
}

sub pp_method_named {
    use Data::Dumper;
    my $op = shift;
    my $sv = $op->sv;
    my $pviv = $padval[$op->targ];
    if ($pviv && class($pviv) =~ /^PV/) {
	my $name = $pviv->PV;
	dprint 'method_named', $op->targ.": $name";
	undef $top->[2] if $top->[2] eq '?';
	$top = [$top->[2] || $lastclass || "(method)", '->', $name];
	undef $lastclass;
    } else {
	warn "method_named: wtf: sizeof padval = ".@padval;
    }
}

sub pp_entersub {
    my $op = shift;
    if ($top->[1] =~ /^(?:m$|->)/) {
	dprint 'method', "call to (@$top) from $subname";
	process($top, "meth");
    } else {
	process($top, "subused");
    }
    undef $lastclass;
    $top = UNKNOWN;
}

#
# Stuff for cross referencing definitions of variables and subs
#

sub B::GV::xref {
    my $gv = shift;
    my $cv = $gv->CV;
    $file = $gv->FILE;
    # XXX: sometimes the "file" is a module.  Why?
    $line = $gv->LINE;
    if ($$cv) {
	#return if $done{$$cv}++;
	process([$gv->STASH->NAME, "&", $gv->NAME], "subdef");
	push(@todo, $cv);
    }
    my $form = $gv->FORM;
    if ($$form) {
	return if $done{$$form}++;
	process([$gv->STASH->NAME, "", $gv->NAME], "formdef");
    }
}

sub xref_definitions {
    my ($pack, %exclude);
    $subname = "(definitions)";
    foreach $pack (qw(B O AutoLoader DynaLoader XSLoader Config DB VMS
		      strict vars FileHandle Exporter Carp PerlIO::Layer
		      attributes utf8 warnings)) {
        $exclude{$pack."::"} = 1;
    }
    no strict qw(vars refs);
    walksymtable(\%{"main::"}, "xref", sub { !defined($exclude{$_[0]}) });
}

=head2 Functions

=over

=item C<rebuild()>

Rebuild the Xref database.

=cut

sub rebuild {
    %call = (); %callby = (); %def = (); %module_subs = ();
    %var_def = (); %var_use = ();
    %module_files = (); %file_modules = ();
    local (@todo, %done);
    xref_definitions;
    xref_main;
    1;
}

sub unmention {
    my ($h, $K, $V, $pack) = @_;
    dprint 'unmention', "Unmentioning $K => $V";
    while (my ($k, $v) = each %$h) {
	while (my ($k2, $v2) = each %$v) {
	    if (ref $v2 eq 'ARRAY') {
		$v->{$k2} = [grep {
		    $_->{$K} ne $V || !$pack || $pack ne $_->{package}
		} @$v2];
		delete $v->{$k2} unless @{$v->{$k2}};
	    } else {
		delete $v->{$k2} if $k2 eq $V;
	    }
	}
	delete $h->{$k} unless keys %{$h->{$k}};
    }
}

sub unmention_sub {
    my ($h, $sub, $pack) = @_;
    dprint 'unmention', "Unmentioning $pack\::$sub";
    if ($pack) {
	delete $h->{$sub}{$pack};
	delete $h->{$sub} unless keys %{$h->{$sub}};
    } else {
	delete $h->{$sub};
    }
}

=item C<forget($func [, $mod])>

Forget that C<$func> was defined.

=cut

sub forget {
    my ($obj, $pack) = @_;
    unmention_sub \%def, @_;
    unmention_sub \%callby, @_;
    unmention \%call, 'sub', @_;
    delete $module_subs{$pack}{$obj};
    delete $module_subs{$pack} unless keys %{$module_subs{$pack}};
    unmention \%var_use, 'sub', @_;
    unmention \%var_def, 'sub', @_;
}

=item C<redefined($func [, $pack])>

Recompute xref info for C<$func>, or C<$pack::$func> if C<$pack> given.

=cut

sub redefined {
    forget @_;
    {
	no strict 'refs';
	my ($sub, $pack) = @_;
	$pack ||= 'main';
	$sub = $pack eq 'main' ? $sub : "$pack\::$sub";
	local $subname = '(definitions)';
	xref_object \&$sub;
    }
}

=item C<mod_redefined($m)>

Recompute Xref information for module C<$m>.

=cut

sub mod_redefined {
    my $mod = shift;
    redefined $_, $mod for keys %{$module_subs{$mod}};
}

######################################################################
# Apropos and definition-finding:

sub _ret_list {
    my ($l, $mod, $sub) = @_;
    my @mod;
    if ($mod) {
	@mod = ($mod);
    } else {
	@mod = keys %$l;
    }
    my @r = map {
	my $lm = $l->{$_};
	$mod = $_;
	map {
	    [$_->{file} || undef, $_->{line}, $_->{sub} || $sub,
	     $_->{package} || $mod ]
	} (ref($lm) eq 'ARRAY' ? @$lm : $lm);
    } @mod;
    @r = grep { $_->[0] !~ /Exporter\.pm$/ } @r
	unless $mod && $mod eq 'Exporter';
    return wantarray ? @r : \@r;
}

=item C<callers($func)>

List callers of C<$func>.

=cut

sub callers {
    my $f = shift;
    return _ret_list $call{$f}, @_;
}

=item C<defs($func)>

Find locations where C<$func> is defined.

=cut

sub defs {
    my ($f, $pack) = @_;
    $f =~ s/.*:://;
    return _ret_list $def{$f}, $pack, $f;
}

=item C<callees($func)>

List callees of C<$func>.

=cut

sub callees {
    my ($f, $pack) = @_;
    my @r = map {
	defs($_->{sub});
    } ($pack ? @{$callby{$f}{$pack}} : map @$_, values %{$callby{$f}});
    return wantarray ? @r : \@r;
}

=item C<var_defs($var)>

Find locations where C<$var> is defined.

=cut

sub var_defs {
    my $v = shift;
    $v =~ s/.*:://;
    return _ret_list $var_def{$v}, @_;
}

=item C<var_uses($var)>

Find locations where C<$var> is used.

=cut

sub var_uses {
    my $v = shift;
    $v =~ s/.*:://;
    return _ret_list $var_use{$v}, @_;
}

=item C<var_assigns($var)>

Find locations where C<$var> is assigned to.

=cut

sub var_assigns {
    my ($v, $pack) = @_;
    if ($v =~ /^(.*)::(.+)$/) {
	$v = $2;
	$pack = $1;
    }
    return _ret_list [ grep $_->{assign},
		       $pack ? @{$var_use{$v}{$pack}}
		       : map @$_, values %{$var_use{$v}} ], $pack;
}

=item C<mod_subs($pack)>

Find subs in package C<$pack>.

=cut

sub mod_subs {
    my $p = shift;
    return _ret_list $module_subs{$p};
}

=item C<mod_decls($pack)>

Generate a list of declarations for all subroutines in package
C<$pack>.

=cut

sub mod_decls {
    my $pack = shift;
    no strict 'refs';
    my @ret = map {
	my $sn = $_->[3];
	my $proto = prototype(\&{"$pack\::$sn"});
	$proto = defined($proto) ? "($proto)" : '';
	"sub $sn $proto;\n";
    } Devel::Xref::mod_subs($pack);
    return wantarray ? @ret : join '', @ret;
}

=item C<mod_files($mod)>

Find file for module C<$mod>.

=cut

sub mod_files {
    my $m = shift;
    return sort keys %{$module_files{$m}}
	if exists $module_files{$m};
    return undef;
}

=item C<file_modules($file)>

List the modules defined in file C<$file>.

=cut

sub file_modules {
    my $f = shift;
    return sort keys %{$file_modules{$f}}
	if exists $file_modules{$f};
    return undef;
}

=item C<apropos($expr)>

Find subs matching C<$expr>.

=cut

sub _apropos_re($) {
    # Do that crazy multi-word identifier completion thing:
    my $re = shift;
    if ($re !~ /[^\w\d_^:]/) {
	$re =~ s/(?<=[A-Za-z\d])([^A-Za-z\d])/[A-Za-z\\d]*$1+/g;
    }
    qr/$re/;
}

sub _apropos {
    my ($h, $re, $mod) = @_;
    my @r = do {
	if($re) {
	    $re = _apropos_re($re);
	    sort grep /$re/, keys %$h;
	} else {
	    sort keys %$h;
	}
    };
    if ($mod) {
	$mod = _apropos_re($mod);
	my %r;
	for (@r) {
	    my $sn = $_;
	    for (keys %{$h->{$_}}) {
		$r{"$_\::$sn"} = 1 if /$mod/;
	    }
	}
	@r = sort keys %r;
    }
    return wantarray ? @r : \@r;
}

sub apropos {
    _apropos \%def, @_;
}

=item C<var_apropos($expr)>

Find variables matching C<$expr>.

=cut

sub var_apropos {
    _apropos \%var_use, @_;
}

=item C<mod_apropos($expr)>

Find modules matching C<$expr>.

=cut

sub mod_apropos {
    _apropos \%module_files, @_;
}

=item C<file_apropos($expr)>

Find modules matching C<$expr>.

=cut

sub file_apropos {
    _apropos \%file_modules, @_;
}

=item C<completions($string)>

=cut

sub completions {
    no strict;
    my ($str) = @_;
    if (my ($pfx, $name) = $str =~ /^([\%\$\@]?)(.+)/) {
        my @nameparts = split /:+/, $name;
        local *_completions = sub {
            no strict;
            my ($stash, $part, @rest) = @_;
            $part = join '[^_]*_', split /_/, $part;
            if (@rest) {
                map {
                    _completions("$stash$_", @rest)
                } grep /^$part.*\::$/, keys %$stash;
            } else {
                map { "$stash$_" } grep /^$part/, keys %$stash;
            }
        };

        my $type = ($pfx eq '$' ? 'SCALAR'
                    : $pfx eq '@' ? 'ARRAY'
                    : $pfx eq '&' ? 'CODE'
                    : $pfx eq '%' ? 'HASH'
                    : undef);
        map {
            s/^::/$pfx/;$_
        } grep {
            !$type || defined(*{$_}{$type})
        } _completions('::', @nameparts);
    }
}

=item C<location($name)>

=cut

sub location {
    no strict;
    my ($str) = @_;
    if (my ($pfx, $name) = $str =~ /^([\%\$\@]?)(.+)/) {
        if ($pfx) {
            print STDERR "Sorry -- can't lookup variables.";
            ();
        } else {
            my $cv = B::svref_2object(\&{$name});
            if ($cv && ($cv = $cv->START) && !$cv->isa('B::NULL')) {
                my ($file, $line) = ($cv->file, $cv->line);
                if ($file !~ /^\//) {
                    for (@INC) {
                        if (-f "$_/$file") {
                            $file = "$_/$file";
                            last;
                        }
                    }
                }
                my ($shortname) = $name =~ /^(?:.*::)([^:]+)$/;
                (Cwd::abs_path($file), $line, $shortname)
            } else {
                ();
            }
        }
    }
}

=item C<find_item($name)>

=cut

sub my_walksymtable(&*) {
    no strict;
    my ($f, $st) = @_;
    local *_walk = sub {
        local ($stash) = @_;
        &$f for keys %$stash;
        _walk("$stash$_") for grep /(?<!main)::$/, keys %$stash;
    };
    _walk($st);
}

sub find_item {
    no strict;
    my ($it, $re) = @_;
    my @ret;
    my $findre = $re ? qr/^\Q$it\E$/ : qr/$re/;
    my_walksymtable {
        push @ret, "$stash$_" if /$findre/;
    } '::';
    map { s/^:://;$_ } @ret;
}

1;

__END__

=back

=head1 EXPORTS

Nothing by default, but all sub and variable described above can be
imported.  C<Devel::Xref> also defines the tags C<:most> for the
above-listed functions, and C<:all> for those and the variables as
well.

=head1 BUGS

See L<B::Xref>.  Also, we currently ignore module names when looking
up a sub by name.  Finally, there is some evil in the way we guess
file and line numbers, both of which should be done more cleanly and
effectively.

=head1 SEE ALSO

L<B::Xref>, from which C<Devel::Xref> is heavily derivative.

=head1 AUTHOR

L<B::Xref> by Malcolm Beattie, m(angl|odifi)ed by Sean O'Rourke
(seano@cpan.org).

=cut
