######################################################################
package Sepia::Xref;

our $VERSION = '0.56';

=head1 NAME

Sepia::Xref - Generates cross reference database for use by Perl programs.

=head1 SYNOPSIS

    use Sepia::Xref qw(rebuild defs callers);

    rebuild;
    for (defs 'foo') {
        printf "%s:%d: sub %s\::foo() defined\n", @{$_}[0..2];
    }

    for (callers 'foo') {
        printf "%s:%d: sub foo() called by %s\::%s().\n", @{$_}[0..3];
    }

=head1 DESCRIPTION

C<Sepia::Xref> is intended as a programmatic interface to the
information supplied by L<B::Xref>.  It is intended to be a component
for interactive Perl development, with other packages providing a
friendly interface to the raw information it extracts.  C<B::Xref>
could be seen as an example of this sort of user-level tool, if it
weren't for the fact that this module was created later, and stole
most of its code.

=cut

use strict;
use Config;
use Cwd 'abs_path';
use B qw(peekop class comppadlist main_start svref_2object walksymtable
         OPpLVAL_INTRO SVf_POK OPpOUR_INTRO OPf_MOD OPpDEREF_HV OPpDEREF_AV
	 cstring);
# use Sepia '_apropos_re';
require Sepia;
*_apropos_re = *Sepia::_apropos_re;

=head2 Variables

=over

=item C<%call>

A map of subs to call locations and callers

=item C<%callby>

A map of subs to subs called.

=item C<%var_use>

A map of global/package variables to uses.

=item C<%var_def>

A map of global/package variables to definitions (usually empty, since
it only picks up local (...) declarations.

=back

=cut

our %call;
our %callby;
our %var_def;
our %var_use;

require Exporter;
our @ISA = qw(Exporter);
my @most = qw(redefined forget rebuild callers callees
	      var_defs var_uses
	      mod_subs mod_files mod_decls mod_apropos
	      apropos var_apropos file_apropos);
our @EXPORT_OK = (@most,
    qw(xref_definitions xref_object xref_main
       %call %callby %var_use %var_def));

our %EXPORT_TAGS =
    (':all' => \@EXPORT_OK,
     ':most' => \@most);

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
my $lastclass;                  # last bareword seen after entersub.

our $DEBUG = 0;
sub dprint {
    my $type = shift;
    my $res = "@_";
    $res =~ s/%//g;             # XXX: work around EPL's misuse of (message)
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

    if (($type eq '&' || $type eq '->') && $subname ne '(definitions)') {
	# Handle caller/callee relations
	my ($spack, $sname) = split_name($subname);

	$call{$name}{$pack}{$subname} = 1;

	$callby{$sname}{$spack}{"$pack\::$name"} = 1;
    } elsif ($type eq 's' || $subname eq '(definitions)') {
	# definition
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
#             print STDERR $opname;
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

sub pp_pushmark {
    my $op = shift;
    my ($class, $meth);
    if (($class = $op->next)->name eq 'const') {
        my $sv = $class->sv;
        my ($classname, $methname);
        # constant could be in the pad (under useithreads)
        if (class($sv) ne "SPECIAL" && $sv->FLAGS & SVf_POK) {
            $classname = $sv->PV;
        } else {
            my $pv = $padval[$class->targ];
            if (class($pv) =~ /^PV/ && class($sv) eq 'SPECIAL'
                ## bareword flag -- should use this?
#                 && ($op->private & 64)
               ) {
                $classname = $pv->PV;
            }
        }
        $lastclass = $classname;
    }
}

sub pp_nextstate {
    my $op = shift;
    $file = $op->file;
    die "pp_nextstate: $file" if $file =~ /::/;
    $line = $op->line;
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
	$top = [$lastclass || "(method)", '->', $name];
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
    %call = (); %callby = ();
    %var_def = (); %var_use = ();
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
    unmention_sub \%callby, @_;
    unmention \%call, 'sub', @_;
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

######################################################################
# Apropos and definition-finding:

sub _ret_list {
    my ($l, $mod, $sub) = @_;
    my @r;
    if ($mod) {
        @r = keys %{$l->{$mod}};
    } else {
        my %h;
        @h{keys %$_} = 1 for values %$l;
        @r = keys %h;
    }
    @r = sort @r;
    return wantarray ? @r : \@r;
}

=item C<callers($func)>

List callers of C<$func>.

=cut

sub callers {
    my $f = shift;
    if ($f =~ /^(.*)::([^:]+)$/) {
        unshift @_, $1;
        $f = $2;
    }
    return _ret_list $call{$f}, @_;
}

=item C<callees($func)>

List callees of C<$func>.

=cut

sub callees {
    my $f = shift;
    if ($f =~ /^(.*)::([^:]+)$/) {
        unshift @_, $1;
        $f = $2;
    }
    _ret_list $callby{$f}, @_;
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

=item C<mod_files($mod)>

Find file for module C<$mod>.

=cut

sub mod_files {
#     my $m = shift;
#     return sort keys %{$module_files{$m}}
# 	if exists $module_files{$m};
#     return undef;
}

=item C<file_modules($file)>

List the modules defined in file C<$file>.

=cut

sub file_modules {
#     my $f = shift;
#     return sort keys %{$file_modules{$f}}
# 	if exists $file_modules{$f};
#     return undef;
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

=item C<var_apropos($expr)>

Find variables matching C<$expr>.

=cut

sub var_apropos {
    _apropos \%var_use, @_;
}

1;

__END__

=back

=head1 EXPORTS

Nothing by default, but all sub and variable described above can be
imported.  C<Sepia::Xref> also defines the tags C<:most> for the
above-listed functions, and C<:all> for those and the variables as
well.

=head1 BUGS

See L<B::Xref>.  Also, we currently ignore module names when looking
up a sub by name.  Finally, there is some evil in the way we guess
file and line numbers, both of which should be done more cleanly and
effectively.

=head1 SEE ALSO

L<B::Xref>, from which C<Sepia::Xref> is heavily derivative.

=head1 AUTHOR

L<B::Xref> by Malcolm Beattie, m(angl|odifi)ed by Sean O'Rourke
(seano@cpan.org).

=cut
