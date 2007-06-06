package Sepia;

=head1 NAME

Sepia - Simple Emacs-Perl Interface

=head1 SYNOPSIS

From inside Emacs:

   M-x load-library RET sepia RET
   M-x sepia-repl RET

At the prompt in the C<*sepia-repl*> buffer:

   main @> ,help

For more information, please see F<sepia/index.html>.

=cut

$VERSION = '0.90';
@ISA = qw(Exporter);

require Exporter;
use strict;
use B;
use Sepia::Debug;               # THIS TURNS ON DEBUGGING INFORMATION!
use Cwd 'abs_path';
use Scalar::Util 'looks_like_number';
use Module::Info;
use Text::Abbrev;

use vars qw($PS1 %REPL %RK %REPL_DOC
            $REPL_LEVEL $REPL_IN $REPL_OUT
            $PACKAGE $WANTARRAY $PRINTER $STRICT $PRINT_PRETTY
            $ISEVAL);

BEGIN {
    eval { require Lexical::Persistence; import Lexical::Persistence };
    if ($@) {
        *repl_strict = sub {
            print STDERR "Strict mode requires Lexical::Persistence.\n";
            0;
        };
    } else {
        *repl_strict = sub {
            my $x = as_boolean(shift, $STRICT);
            if ($x && !$STRICT) {
                $STRICT = new Lexical::Persistence;
            } elsif (!$x) {
                undef $STRICT;
            }
            0;
        };
    }
    eval { require Module::CoreList };
    if ($@) {
        *Sepia::core_version = sub { '???' };
    } else {
        *Sepia::core_version = sub { Module::CoreList->first_release(@_) };
    }
}

=head1 DESCRIPTION

Sepia is a set of features to make Emacs a better tool for Perl
development.  This package contains the Perl side of the
implementation, including all user-serviceable parts (for the
cross-referencing facility see L<Sepia::Xref>).  This document is
aimed as Sepia developers; for user documentation, see
L<sepia/index.html>.

Though not intended to be used independent of the Emacs interface, the
Sepia module's functionality can be used through a rough procedural
interface.

=head2 C<@compls = completions($string [, $type])>

Find a list of completions for C<$string> with glob type C<$type>,
which may be "SCALAR", "HASH", "ARRAY", "CODE", "IO", or the special
value "VARIABLE", which means either scalar, hash, or array.
Completion operates on word subparts separated by [:_], so
e.g. "S:m_w" completes to "Sepia::my_walksymtable".

=head2 C<@compls = method_completions($expr, $string [,$eval])>

Complete among methods on the object returned by C<$expr>.  The
C<$eval> argument, if present, is a function used to do the
evaluation; the default is C<eval>, but for example the Sepia REPL
uses C<Sepia::repl_eval>.  B<Warning>: Since it has to evaluate
C<$expr>, method completion can be extremely problematic.  Use with
care.

=cut

sub _apropos_re($)
{
    # Do that crazy multi-word identifier completion thing:
    my $re = shift;
    return qr/.*/ if $re eq '';
    if (wantarray) {
        map {
            s/(?:^|(?<=[A-Za-z\d]))(([^A-Za-z\d])\2*)/[A-Za-z\\d]*$2+/g;
            qr/^$_/
        } split /:+/, $re, -1;
    } else {
        if ($re !~ /[^\w\d_^:]/) {
            $re =~ s/(?<=[A-Za-z\d])(([^A-Za-z\d])\2*)/[A-Za-z\\d]*$2+/g;
        }
        qr/$re/;
    }
}

sub _completions1
{
    no strict;
    my $stash = shift;
    my $re = shift || '';
    $re = qr/$re/;
    if (@_ == 0 || !defined $_[0]) {
        map "$stash$_", grep /$re/, keys %$stash;
    } else {
        map {
            _completions1("$stash$_", @_);
        } grep /$re.*::$/, keys %$stash;
    };
}

sub _completions
{
    _completions1 '::', _apropos_re($_[0]);
}

my %sigil;
BEGIN {
    %sigil = qw(ARRAY @ SCALAR $ HASH %);
}

sub completions
{
    no strict;
    my ($str, $type, $infunc) = @_;
    my @ret;

    if (!$type) {
        @ret = grep {
            defined *{$_}{CODE} || defined *{$_}{IO}
                || (/::$/ && defined *{$_}{HASH});
        } _completions $str;
    } else {
        @ret = grep {
            if ($type eq 'SCALAR') {
                defined ${$_};
            } elsif ($type eq 'VARIABLE') {
                defined ${$_} || defined *{$_}{HASH} || defined *{$_}{ARRAY};
            } else {
                defined *{$_}{$type}
            }
        } _completions $str;
        if (defined $infunc && defined *{$infunc}{CODE}) {
            my ($apre) = _apropos_re($str);
            my $st = $sigil{$type};
            push @ret, grep {
                (my $tmp = $_) =~ s/^\Q$st//;
                $tmp =~ /$apre/;
            } lexicals($infunc);
        }
    }

    ## Complete "simple" sequences as abbreviations, e.g.:
    ##   wtci -> Want_To_Complete_It, NOT
    ##        -> WaTCh_trIpe
    if (!@ret && $str !~ /[^\w\d]/) {
        my $broad = join '.*', map "\\b$_", split '', $str;
        if ($type) {
            @ret = grep {
                defined *{$_}{CODE} || defined *{$_}{IO}
                    || (/::$/ && defined *{$_}{HASH});
            } _completions1 '::', qr/$broad/;
        } else {
            @ret = grep {
                $type eq 'SCALAR' ? defined ${$_} : defined *{$_}{$type}
            } _completions1 '::', qr/$broad/;
        }
        if (defined $infunc && defined *{$infunc}{CODE}) {
            my $st = $sigil{$type};
            grep {
                (my $tmp = $_) =~ s/^\Q$st//;
                $tmp =~ /$broad/;
            } lexicals($infunc);
        }
    }
    ## Complete packages so e.g. "new B:T" -> "new Blah::Thing"
    ## instead of "new Blah::Thing::"
    if (!$type) {
        @ret = map { /(.*)::$/ ? ($1, $_) : $_ } @ret;
    }
    ## XXX: Control characters, $", and $1, etc. confuse Emacs, so
    ## remove them.
    grep {
        length > 0 && !looks_like_number $_ && !/^[^\w\d_]$/ && !/^_</ && !/^[[:cntrl:]]/
    } map { s/^:://; $_ } @ret;
}

sub method_completions
{
    my ($x, $fn, $eval) = @_;
    $x =~ s/^\s+//;
    $x =~ s/\s+$//;
    $eval ||= 'CORE::eval';
    no strict;
    return unless ($x =~ /^\$/ && ($x = $eval->("ref($x)")))
        || $eval->('defined(%{'.$x.'::})');
    unless ($@) {
        my $re = _apropos_re $fn;
        ## Filter out overload methods "(..."
        return sort { $a cmp $b } map { s/.*:://; $_ }
            grep { defined *{$_}{CODE} && /::$re/ && !/\(/ }
                methods($x, 1);
    }
}

=head2 C<@locs = location(@names)>

Return a list of [file, line, name] triples, one for each function
name in C<@names>.

=cut

sub location
{
    no strict;
    my @x= map {
        my $str = $_;
        if (my ($pfx, $name) = $str =~ /^([\%\$\@]?)(.+)/) {
            if ($pfx) {
                warn "Sorry -- can't lookup variables.";
                [];
            } else {
                # XXX: svref_2object only seems to work with a package
                # tacked on, but that should probably be done
                # elsewhere...
                $name = 'main::'.$name unless $name =~ /::/;
                my $cv = B::svref_2object(\&{$name});
                if ($cv && defined($cv = $cv->START) && !$cv->isa('B::NULL')) {
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
                    [Cwd::abs_path($file), $line, $shortname || $name]
                } else {
#                    warn "Bad CV for $name: $cv";
                    [];
                }
            }
        } else {
            []
        }
    } @_;
    return @x;
}

=head2 C<@matches = apropos($name [, $is_regex])>

Search for function C<$name>, either in all packages or, if C<$name>
is qualified, only in one package.  If C<$is_regex> is true, the
non-package part of C<$name> is a regular expression.

=cut

sub my_walksymtable(&*)
{
    no strict;
    my ($f, $st) = @_;
    local *_walk = sub {
        local ($stash) = @_;
        &$f for keys %$stash;
        _walk("$stash$_") for grep /(?<!main)::$/, keys %$stash;
    };
    _walk($st);
}

sub apropos
{
    my ($it, $re, @types) = @_;
    my $stashp;
    if (@types) {
        $stashp = grep /STASH/, @types;
        @types = grep !/STASH/, @types;
    } else {
        @types = qw(CODE);
    }
    no strict;
    if ($it =~ /^(.*::)([^:]+)$/) {
        my ($stash, $name) = ($1, $2);
        if (!defined %$stash) {
            return;
        }
        if ($re) {
            my $name = qr/^$name/;
            map {
                "$stash$_"
            }
            grep {
                my $stashnm = "$stash$_";
                /$name/ &&
                    (($stashp && /::$/)
                     || scalar grep { defined *{$stashnm}{$_} } @types)
            } keys %$stash;
        } else {
            defined &$it ? $it : ();
        }
    } else {
        my @ret;
        my $findre = $re ? qr/$it/ : qr/^\Q$it\E$/;
        my_walksymtable {
            push @ret, "$stash$_" if /$findre/;
        } '::';
        map { s/^:*(?:main:+)*//;$_ } @ret;
    }
}

=head2 C<@names = mod_subs($pack)>

Find subs in package C<$pack>.

=cut

sub mod_subs
{
    no strict;
    my $p = shift;
    my $stash = \%{"$p\::"};
    if (defined $stash) {
        grep { defined &{"$p\::$_"} } keys %$stash;
    }
}

=head2 C<@decls = mod_decls($pack)>

Generate a list of declarations for all subroutines in package
C<$pack>.

=cut

sub mod_decls
{
    my $pack = shift;
    no strict 'refs';
    my @ret = map {
	my $sn = $_;
	my $proto = prototype(\&{"$pack\::$sn"});
	$proto = defined($proto) ? "($proto)" : '';
	"sub $sn $proto;";
    } mod_subs($pack);
    return wantarray ? @ret : join '', @ret;
}

=head2 C<$info = module_info($module, $type)>

Emacs-called function to get module information.

=cut

sub module_info($$)
{
    my ($m, $func) = @_;
    my $info;
    if (-f $m) {
	$info = Module::Info->new_from_file($m);
    } else {
	(my $file = $m) =~ s|::|/|g;
	$file .= '.pm';
	if (exists $INC{$file}) {
	    $info = Module::Info->new_from_loaded($m);
	} else {
	    $info = Module::Info->new_from_module($m);
	}
    }
    if ($info) {
        return $info->$func;
    }
}

=head2 C<$file = mod_file($mod)>

Find the likely file owner for module C<$mod>.

=cut

sub mod_file
{
    my $m = shift;
    $m =~ s/::/\//g;
    while ($m && !exists $INC{"$m.pm"}) {
        $m =~ s#(?:^|/)[^/]+$##;
    }
    $m ? $INC{"$m.pm"} : undef;
}

=head2 C<@mods = package_list>

Gather a list of all distributions on the system. XXX UNUSED

=cut

our $INST;
sub inst()
{
    unless ($INST) {
        eval 'require ExtUtils::Installed';
        $INST = new ExtUtils::Installed;
    }
    $INST;
}

sub package_list
{
    sort { $a cmp $b } inst()->modules;
}

=head2 C<@mods = module_list>

Gather a list of all packages (.pm files, really) installed on the
system, grouped by distribution. XXX UNUSED

=cut

sub module_list
{
    @_ = package_list unless @_;
    my $incre = join '|', map quotemeta, @INC;
    $incre = qr|(?:$incre)/|;
    my $inst = inst;
    map {
        [$_, sort map {
            s/$incre//; s|/|::|g;$_
        } grep /\.pm$/, $inst->files($_)]
    } @_;
}

=head2 C<@mods = doc_list>

Gather a list of all documented packages (.?pm files, really)
installed on the system, grouped by distribution. XXX UNUSED

=cut

sub doc_list
{
    @_ = package_list unless @_;
    my $inst = inst;
    map {
        [$_, sort map {
            s/.*man.\///; s|/|::|g;s/\..?pm//; $_
        } grep /\..pm$/, $inst->files($_)]
    } @_;
}

=head2 C<lexicals($subname)>

Return a list of C<$subname>'s lexical variables.  Note that this
includes all nested scopes -- I don't know if or how Perl
distinguishes inner blocks.

=cut

sub lexicals
{
    my $cv = B::svref_2object(\&{+shift});
    return unless $cv && ($cv = $cv->PADLIST);
    my ($names, $vals) = $cv->ARRAY;
    map {
        my $name = $_->PV; $name =~ s/\0.*$//; $name
    } grep B::class($_) ne 'SPECIAL', $names->ARRAY;
}

=head2 C<$lisp = tolisp($perl)>

Convert a Perl scalar to some ELisp equivalent.

=cut

sub tolisp($)
{
    my $thing = @_ == 1 ? shift : \@_;
    my $t = ref $thing;
    if (!$t) {
        if (!defined $thing) {
            'nil'
        } elsif (looks_like_number $thing) {
            ''.(0+$thing);
        } else {
            ## XXX Elisp and perl have slightly different
            ## escaping conventions, so we do this crap instead.
            $thing =~ s/["\\]/\\$1/g;
            qq{"$thing"};
        }
    } elsif ($t eq 'GLOB') {
        (my $name = $$thing) =~ s/\*main:://;
        $name;
    } elsif ($t eq 'ARRAY') {
        '(' . join(' ', map { tolisp($_) } @$thing).')'
    } elsif ($t eq 'HASH') {
        '(' . join(' ', map {
            '(' . tolisp($_) . " . " . tolisp($thing->{$_}) . ')'
        } keys %$thing).')'
    } elsif ($t eq 'Regexp') {
        "'(regexp . \"" . quotemeta($thing) . '")';
#     } elsif ($t eq 'IO') {
    } else {
        qq{"$thing"};
    }
}

=head2 C<printer(\@res, $wantarray)>

Print C<@res> appropriately on the current filehandle.  If C<$ISEVAL>
is true, use terse format.  Otherwise, use human-readable format,
which can use either L<Data::Dumper>, L<YAML>, or L<Data::Dump>.

=cut

sub print_dumper
{
    eval { require Data::Dumper };
    local $Data::Dumper::Deparse = 1;
    local $Data::Dumper::Indent = 0;
    local $_;
    no strict;
    my $thing = @res > 1 ? \@res : $res[0];
    eval {
        $_ = Data::Dumper::Dumper($thing);
        s/^\$VAR1 = //;
        s/;$//;
    };
    if (length $_ > ($ENV{COLUMNS} || 80)) {
        $Data::Dumper::Indent = 2;
        eval {
            $_ = Data::Dumper::Dumper($thing);
            s/\A\$VAR1 = //;
            s/;\Z//;
        };
        s/\A\$VAR1 = //;
        s/;\Z//;
    }
    $_;
}

sub print_plain
{
    no strict;
    "@res";
}

sub print_yaml
{
    no strict;
    eval { require YAML };
    if ($@) {
        print_dumper;
    } else {
        YAML::Dump(\@res);
    }
}

sub print_dump
{
    no strict;
    eval { require Data::Dump };
    if ($@) {
        print_dumper;
    } else {
        Data::Dump::dump(\@res);
    }
}

sub printer
{
    no strict;
    local *res = shift;
    my ($wantarray) = @_;
    @::__ = @res;
    $::__ = @res == 1 ? $res[0] : [@res];
    my $str;
    if ($ISEVAL) {
        $res = "@res";
    } elsif (@res == 1 && UNIVERSAL::can($res[0], '()')) {
        # overloaded?
        $res = $res[0];
    } elsif (!$ISEVAL && $PRINT_PRETTY && @res > 1 && !grep ref, @res) {
        $res = columnate(sort @res);
        print $res;
        return;
    } else {
        $res = $PRINTER->();
    }
    if ($ISEVAL) {
        print ';;;', length $res, "\n$res\n";
    } else {
        print "=> $res\n";
    }
}

=head2 C<repl(\*FH)>

Execute a command interpreter on FH.  The prompt has a few bells and
whistles, including:

  * Obviously-incomplete lines are treated as multiline input (press
    'return' twice or 'C-c' to discard).

  * C<die> is overridden to enter a recursive interpreter at the point
    C<die> is called.  From within this interpreter, you can examine a
    backtrace by calling "bt", return from C<die> with "r EXPR", or
    go ahead and die by pressing Control-c.

Behavior is controlled in part through the following package-globals:

=over 4

=item C<$PACKAGE> -- evaluation package

=item C<$PRINTER> -- result printer (default: print_dumper)

=item C<$PS1> -- the default prompt

=item C<$STRICT> -- whether 'use strict' is applied to input

=item C<$WANTARRAY> -- evaluation context

=item C<$PRINT_PRETTY> -- format some output nicely (default = 1)

Format some values nicely, independent of $PRINTER.  Currently, this
displays arrays of scalars as columns.

=item C<%REPL> -- maps shortcut names to handlers

=item C<%REPL_DOC> -- maps shortcut names to documentation

=back

=cut

BEGIN {
    no strict;
    $PS1 = "> ";
    $PACKAGE = 'main';
    $WANTARRAY = 1;
    $PRINTER = \&Sepia::print_dumper;
    $PRINT_PRETTY = 1;
    %REPL = (help => \&Sepia::repl_help,
             cd => \&Sepia::repl_chdir,
             methods => \&Sepia::repl_methods,
             package => \&Sepia::repl_package,
             who => \&Sepia::repl_who,
             wantarray => \&Sepia::repl_wantarray,
             format => \&Sepia::repl_format,
             strict => \&Sepia::repl_strict,
             quit => \&Sepia::repl_quit,
             reload => \&Sepia::repl_reload,
             shell => \&Sepia::repl_shell,
         );
    %REPL_DOC = (
        cd =>
    'cd DIR             Change directory to DIR',
        format =>
    'format [dumper|dump|yaml|plain]
                       Set output formatter (default: dumper)',
        help =>
    'help               Display this message',
        methods => <<EOS,
methods X [RE]     List methods for reference or package X,
                       matching optional pattern RE.
EOS
        package =>
    'package PACKAGE    Set evaluation package to PACKAGE',
        quit =>
    'quit               Quit the REPL',
        shell =>
    'shell CMD ...      Run CMD in the shell.',
        strict =>
    'strict [0|1]       Turn \'use strict\' mode on or off',
        wantarray =>
    'wantarray [0|1]    Set or toggle evaluation context',
        who => <<EOS,
who PACKAGE [RE]   List variables and subs in PACKAGE matching optional
                       pattern RE.
EOS
        reload =>
    'reload             Reload Sepia.pm and relaunch the REPL.',
    );
}

sub prompt()
{
    "$PACKAGE ".($WANTARRAY ? '@' : '$').$PS1
}

sub Dump {
    eval {
        Data::Dumper->Dump([$_[0]], [$_[1]]);
    };
}

sub repl_help
{
    print "REPL commands (prefixed with ','):\n";
    for (sort keys %REPL) {
        print "    ",
            exists $REPL_DOC{$_} ? "$REPL_DOC{$_}\n": "$_    (undocumented)\n";
    }
    0;
}

sub repl_format
{
    my $t = shift;
    chomp $t;
    $t = 'dumper' if $t eq '';
    my %formats = abbrev qw(dumper dump yaml plain);
    if (exists $formats{$t}) {
        no strict;
        $PRINTER = \&{'print_'.$formats{$t}};
    } else {
        warn "No such format '$t' (dumper, dump, yaml, plain).\n";
    }
    0;
}

sub repl_chdir
{
    chomp(my $dir = shift);
    $dir =~ s/^~\//$ENV{HOME}\//;
    $dir =~ s/\$HOME/$ENV{HOME}/;
    if (-d $dir) {

        chdir $dir;
        my $ecmd = '(cd "'.Cwd::getcwd().'")';
        print ";;;###".length($ecmd)."\n$ecmd\n";
    } else {
        warn "Can't chdir\n";
    }
    0;
}

sub who
{
    my ($pack, $re) = @_;
    $re ||= '.?';
    $re = qr/$re/;
    no strict;
    sort grep /$re/, map {
        (defined %{$pack.'::'.$_} ? '%'.$_ : (),
         defined ${$pack.'::'.$_} ? '$'.$_ : (), # ?
         defined @{$pack.'::'.$_} ? '@'.$_ : (),
         defined &{$pack.'::'.$_} ? $_ : (),
     )
    } grep !/::$/ && !/^(?:_<|[^\w])/, keys %{$pack.'::'};
}


sub columnate
{
    my $len = 0;
    my $width = $ENV{COLUMNS} || 80;
    for (@_) {
        $len = length if $len < length;
    }
    my $nc = int($width / ($len+1)) || 1;
    my $nr = int(@_ / $nc) + (@_ % $nc ? 1 : 0);
    my $fmt = ('%-'.($len+1).'s') x ($nc-1) . "%s\n";
    my @incs = map { $_ * $nr } 0..$nc-1;
    my $str = '';
    for my $r (0..$nr-1) {
        $str .= sprintf $fmt, map { $_ || '' } @_[map { $r + $_ } @incs];
    }
    $str =~ s/ +$//m;
    $str
}

sub repl_who
{
    my ($pkg, $re) = split ' ', shift;
    print columnate who($pkg || $PACKAGE, $re);
    0;
}

sub methods
{
    my ($pack, $qualified) = @_;
    no strict;
    my @own = $qualified ? grep {
        defined *{$_}{CODE}
    } map { "$pack\::$_" } keys %{$pack.'::'}
        : grep {
            defined *{"$pack\::$_"}{CODE}
        } keys %{$pack.'::'};
    (@own, defined @{$pack.'::ISA'}
         ? (map methods($_, $qualified), @{$pack.'::ISA'}) : ());
}

sub repl_methods
{
    my ($x, $re) = split ' ', shift;
    $x =~ s/^\s+//;
    $x =~ s/\s+$//;
    if ($x =~ /^\$/) {
        $x = repl_eval("ref $x");
        return 0 if $@;
    }
    $re ||= '.?';
    $re = qr/$re/;
    print columnate sort { $a cmp $b } grep /$re/, methods $x;
    0;
}

sub as_boolean
{
    my ($val, $cur) = @_;
    $val =~ s/\s+//g;
    length($val) ? $val : !$cur;
}

sub repl_wantarray
{
    $WANTARRAY = as_boolean shift, $WANTARRAY;
    0;
}

sub repl_package
{
    chomp(my $p = shift);
    no strict;
    if (defined %{$p.'::'}) {
        $PACKAGE = $p;
#         my $ecmd = '(setq sepia-eval-package "'.$p.'")';
#         print ";;;###".length($ecmd)."\n$ecmd\n";
    } else {
        warn "Can't go to package $p -- doesn't exist!\n";
    }
    0;
}

sub repl_quit
{
    1;
}

sub repl_reload
{
    do $INC{'Sepia.pm'};
    if ($@) {
        print "Reload failed:\n$@\n";
    } else {
        @_ = (select, 0);
        goto &Sepia::repl;
    }
}

sub repl_shell
{
    my $cmd = shift;
    print `$cmd 2>& 1`;
    return 0;
}

sub repl_eval
{
    my ($buf, $wantarray, $pkg) = @_;
    no strict;
    local $PACKAGE = $pkg || $PACKAGE;
    if ($STRICT) {
        if (!$WANTARRAY) {
            $buf = 'scalar($buf)';
        }
        my $ctx = join(',', keys %{$STRICT->get_context('_')});
        $ctx = $ctx ? "my ($ctx);" : '';
        $buf = eval "sub { package $PACKAGE; use strict; $ctx $buf }";
        if ($@) {
            print STDERR "ERROR\n$@\n";
            return;
        }
        $STRICT->call($buf);
    } else {
        $buf = "do { package $PACKAGE; no strict; $buf }";
        if ($WANTARRAY) {
            eval $buf;
        } else {
            scalar eval $buf;
        }
    }
}

## Collects warnings for REPL
my @warn;

sub sig_warn
{
    push @warn, shift
}

sub print_warnings
{
    if (@warn) {
        if ($ISEVAL) {
            my $tmp = "@warn";
            print ';;;'.length($tmp)."\n$tmp\n";
        } else {
            for (@warn) {
                # s/(.*) at .*/$1/;
                print "warning: $_\n";
            }
        }
    }
}

sub repl
{
    if (@_ > 0) {
        $REPL_IN = $_[0];
        $REPL_OUT = $_[1];
    }
    select $REPL_OUT;
    $| = 1;

    local $REPL_LEVEL = $REPL_LEVEL + 1;

    my $in;
    my $buf = '';
    my $sigged = 0;

    my $nextrepl = sub { $sigged = 1; };

    local *__;
    local *CORE::GLOBAL::die = \&Sepia::Debug::die;
    local *CORE::GLOBAL::warn = \&Sepia::Debug::warn;
    Sepia::Debug::add_repl_commands;
    print <<EOS if $REPL_LEVEL == 1;
Sepia version $Sepia::VERSION.
Press ",h" for help, or "^D" or ",q" to exit.
EOS
    print prompt;
    my @sigs = qw(INT TERM PIPE ALRM);
    local @SIG{@sigs};
    $SIG{$_} = $nextrepl for @sigs;
 repl: while (defined(my $in = <$REPL_IN>)) {
            if ($sigged) {
                $buf = '';
                $sigged = 0;
                print "\n", prompt;
                next repl;
            }
            $buf .= $in;
            $buf =~ s/^\s*//;
            local $ISEVAL;
            if ($buf =~ /^<<(\d+)\n(.*)/) {
                $ISEVAL = 1;
                my $len = $1;
                my $tmp;
                $buf = $2;
                while ($len && defined($tmp = read $REPL_IN, $buf, $len, length $buf)) {
                    $len -= $tmp;
                }
            }
            my (@res);
            ## Only install a magic handler if no one else is playing.
            local $SIG{__WARN__} = $SIG{__WARN__};
            @warn = ();
            unless ($SIG{__WARN__}) {
                $SIG{__WARN__} = 'Sepia::sig_warn';
            }
            if ($buf =~ /^,(\S+)\s*(.*)/s) {
                ## Inspector shortcuts
                my $short = $1;
                if (exists $Sepia::RK{$short}) {
                    my $ret;
                    my $arg = $2;
                    chomp $arg;
                    ($ret, @res) = $Sepia::REPL{$Sepia::RK{$short}}->($arg, wantarray);
                    if ($ret) {
                        return wantarray ? @res : $res[0];
                    }
                } else {
                    if (grep /^$short/, keys %Sepia::REPL) {
                        print "Ambiguous shortcut '$short': ",
                            join(', ', sort grep /^$short/, keys %Sepia::REPL),
                                "\n";
                    } else {
                        print "Unrecognized shortcut '$short'\n";
                    }
                    $buf = '';
                    print prompt;
                    next repl;
                }
            } else {
                ## Ordinary eval
                @res = repl_eval $buf, wantarray;
                if ($@) {
                    if ($ISEVAL) {
                        ## Always return results for an eval request
                        Sepia::printer \@res, wantarray;
                        Sepia::printer [$@], wantarray;
                        # print_warnings $ISEVAL;
                        $buf = '';
                        print prompt;
                    } elsif ($@ =~ /at EOF$/m) {
                        ## Possibly-incomplete line
                        if ($in eq "\n") {
                            print "Error:\n$@\n*** cancel ***\n", prompt;
                            $buf = '';
                        } else {
                            print ">> ";
                        }
                    } else {
                        print_warnings;
                        # $@ =~ s/(.*) at eval .*/$1/;
                        print "error: $@\n";
                        print prompt;
                        $buf = '';
                    }
                    next repl;
                }
            }
            if ($buf !~ /;$/ && $buf !~ /^,/) {
                ## Be quiet if it ends with a semicolon, or if we
                ## executed a shortcut.
                Sepia::printer \@res, wantarray;
            }
            $buf = '';
            print_warnings;
            print prompt;
        }
}

sub perl_eval
{
    tolisp(repl_eval(shift));
}

=head2 C<$status = html_module_list($file [, $prefix])>

Generate an HTML list of installed modules, looking inside of
packages.  If C<$prefix> is missing, uses "about://perldoc/".

=head2 C<$status = html_package_list($file [, $prefix])>

Generate an HTML list of installed top-level modules, without looking
inside of packages.  If C<$prefix> is missing, uses
"about://perldoc/".

=cut

sub html_module_list
{
    my ($file, $base) = @_;
    $base ||= 'about://perldoc/';
    my $inst = inst();
    return unless $inst;
    return unless open OUT, ">$file";
    print OUT "<html><body><ul>";
    my $pfx = '';
    my %ns;
    for (package_list) {
        push @{$ns{$1}}, $_ if /^([^:]+)/;
    }
    for (sort keys %ns) {
        print OUT qq{<li><b>$_</b><ul>} if @{$ns{$_}} > 1;
        for (sort @{$ns{$_}}) {
            my @fs = map {
                s/.*man.\///; s|/|::|g; s/\..?pm//; $_
            } grep /\.\dpm$/, sort $inst->files($_);
            if (@fs == 1) {
                print OUT qq{<li><a href="$base$fs[0]">$fs[0]</a>};
            } else {
                print OUT qq{<li>$_<ul>};
                for (@fs) {
                    print OUT qq{<li><a href="$base$_">$_</a>};
                }
                print OUT '</ul>';
            }
        }
        print OUT qq{</ul>} if @{$ns{$_}} > 1;
    }
    print OUT "</ul></body></html>\n";
    close OUT;
    1;
}

sub html_package_list
{
    my ($file, $base) = @_;
    return unless inst();
    $base ||= 'about://perldoc/';
    return unless open OUT, ">$file";
    print OUT "<html><body><ul>";
    my $pfx = '';
    my %ns;
    for (package_list) {
        push @{$ns{$1}}, $_ if /^([^:]+)/;
    }
    for (sort keys %ns) {
        if (@{$ns{$_}} == 1) {
            print OUT
                qq{<li><a href="$base$ns{$_}[0]">$ns{$_}[0]</a>};
        } else {
            print OUT qq{<li><b>$_</b><ul>};
            print OUT qq{<li><a href="$base$_">$_</a>}
                for sort @{$ns{$_}};
            print OUT qq{</ul>};
        }
    }
    print OUT "</ul></body></html>\n";
    close OUT;
    1;
}

1;
__END__

=head1 TODO

See the README file included with the distribution.

=head1 AUTHOR

Sean O'Rourke, E<lt>seano@cpan.orgE<gt>

Bug reports welcome, patches even more welcome.

=head1 COPYRIGHT

Copyright (C) 2005-2007 Sean O'Rourke.  All rights reserved, some
wrongs reversed.  This module is distributed under the same terms as
Perl itself.

=cut
