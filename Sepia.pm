package Sepia;
$VERSION = '0.62';
@ISA = qw(Exporter);

require Exporter;
use strict;
use Cwd 'abs_path';
use Scalar::Util 'looks_like_number';
use Module::Info;
use PadWalker qw(peek_my peek_our peek_sub closed_over);
use Sub::Uplevel;
use Carp;
use B;

=item C<@compls = completions($string [, $type])>

Find a list of completions for C<$string> with glob type $type.
Completion operates on word subparts separated by [:_], so
e.g. "S:m_w" completes to "Sepia::my_walksymtable".

=cut

sub _apropos_re($)
{
    # Do that crazy multi-word identifier completion thing:
    my $re = shift;
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
    if (@_ == 1) {
        map {
            "$stash$_"
        } grep /$_[0]/, keys %$stash;
    } else {
        my $re = shift;
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
    map { s/^:://; $_ } ($type ? do {
        (grep { defined *{$_}{$type} } _completions $str),
            (defined $infunc && defined *{$infunc}{CODE}) ? do {
                my ($apre) = _apropos_re($str);
                my $st = $sigil{$type};
                grep {
                    (my $tmp = $_) =~ s/^\Q$st//;
                    $tmp =~ /$apre/;
                } lexicals($infunc);
            } : ();
    } : do {
        grep {
            defined *{$_}{CODE} || defined *{$_}{IO}
                || (/::$/ && defined *{$_}{HASH});
        } _completions $str;
    })
}

=item C<@locs = location(@names)>

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
                    warn "Bad CV for $name: $cv";
                    [];
                }
            }
        } else {
            []
        }
    } @_;
    return @x;
}

=item C<@matches = apropos($name [, $is_regex])>

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

=item C<@names = mod_subs($pack)>

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

=item C<@decls = mod_decls($pack)>

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
	"sub $sn $proto;\n";
    } mod_subs($pack);
    return wantarray ? @ret : join '', @ret;
}

=item C<$info = module_info($module, $type)>

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

=item C<$file = mod_file($mod)>

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

=item C<lexicals($subname)>

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

=item C<$lisp = tolisp($perl)>

Convert a Perl scalar to some ELisp equivalent.

=cut

sub tolisp($)
{
    my $thing = @_ == 1 ? shift : \@_;
    my $t = ref $thing;
    if (!$t) {
        if (looks_like_number $thing) {
            ''.$thing;
        } else {
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

=item C<printer(\@res [, $iseval])>

Print C<@res> appropriately on the current filehandle.  If C<$iseval>
is true, use terse format.  Otherwise, use human-readable format.

=cut

sub printer
{
    no strict;
    local *res = shift;
    my ($iseval, $wantarray) = @_;
    @__ = @res;
    my $str;
    if ($iseval) {
        $__ = "@res";
    } elsif ($fancy) {
        local $Data::Dumper::Deparse = 1;
        local $Data::Dumper::Indent = 0;
        $__ = Data::Dumper::Dumper(@res > 1 ? \@res : $res[0]);
        $__ =~ s/^\$VAR1 = //;
        $__ =~ s/;$//;
    } else {
        $__ = "@res";
    }
    if ($iseval) {
        print ';;;', length $__, "\n$__\n";
    } else {
        print "=> $__\n";
    }
}

=item C<repl(\*FH)>

Execute a command interpreter on FH.  The prompt has a few bells and
whistles, including:

  * Obviously-incomplete lines are treated as multiline input.

  * C<die> is overridden to enter a recursive interpreter at the point
    C<die> is called.  From within this interpreter, you can examine a
    backtrace by calling "bt", return from C<die> with "r EXPR", or
    go ahead and die by pressing Control-c.

Behavior is controlled in part through the following package-globals:

=over 4

=item C<$PS1> -- the default prompt

=item C<$stopdie> -- true to enter the inspector on C<die()>

=item C<$stopwarn> -- true to enter the inspector on C<warn()>

=item C<$fancy> -- true for pretty-printing via L<Data::Dumper>

=item C<%REPL> -- maps shortcut names to handlers

=cut

use vars qw($PS1 $ps1 $dies $stopdie $stopwarn $fancy %REPL $PACKAGE);
BEGIN {
    no strict;
    $ps1 = $PS1 = "> ";
    $dies = 0;
    $stopdie = 1;
    $stopwarn = 0;
    $fancy = 1;
    $PACKAGE = 'main';
    *REALDIE = *CORE::GLOBAL::die;
    *REALWARN = *CORE::GLOBAL::warn;
    %REPL = (h => \&Sepia::repl_help,
             cd => \&Sepia::repl_chdir);
}

sub Dump {
    Data::Dumper->Dump([$_[0]], [$_[1]]);
}

my $FRAMES = 4;

sub hiding_me
{
    my ($fn, @args) = @_;
    sub {
        uplevel $FRAMES, $fn, @args
    }
}

sub eval_in_env
{
    my ($expr, $env) = @_;
    local $::ENV = $env;
    my $str = '';
    for (keys %$env) {
        next unless /^([\$\@%])(.+)/;
        $str .= "local *$2 = \$::ENV->{'$_'}; ";
    }
    eval "do { no strict; $str $expr }";
}

sub debug_upeval
{
    my ($lev, $exp) = $_[0] =~ /^\s*(\d+)\s+(.*)/;
    print " <= $exp\n";
    (0, eval_in_env($exp, PadWalker::peek_my(0+$lev)));
}

sub debug_inspect
{
    local $_ = shift;
    for my $i (split) {
        my $sub = (caller $i)[3];
        next unless $sub;
        my $h = PadWalker::peek_my($i);
        print "[$i] $sub:\n";
        for (sort keys %$h) {
            print "\t", Sepia::Dump($h->{$_}, $_);
        }
    }
    0;
}

sub repl_help
{
    print <<EOS;
REPL commands (prefixed with ','):
    cd DIR       Change directory to DIR
EOS
    0;
}

sub repl_chdir
{
    chomp(my $dir = shift);
    if (-d $dir) {
        chdir $dir;
        my $ecmd = '(cd "'.Cwd::getcwd().'")';
        print ";;;###".length($ecmd)."\n$ecmd\n";
    } else {
        warn "Can't chdir\n";
    }
    0;
}

sub debug_help
{
    print <<EOS;
Inspector commands (prefixed with ','):
    \\C-c        Pop one debugger level
    b            show backtrace
    i N ...      inspect lexicals in frame(s) N ...
    e N EXPR     evaluate EXPR in lexical environment of frame N
    r EXPR       return EXPR
    d/w          keep on dying/warning
EOS
    0;
}

sub debug_backtrace
{
    Carp::cluck;0
}

sub debug_return
{
    (1, repl_eval(@_));
}

sub repl_eval
{
    my ($buf, $wantarray) = @_;
    no strict;
    $buf = "do { package $PACKAGE; no strict; $buf }";
#     open O, ">>/tmp/blah";
#     print O "##############################\n$buf";
#     close O;
    if ($wantarray || !defined($wantarray)) {
        eval $buf;
    } else {
        scalar eval $buf;
    }
}

sub repl
{
    my ($fh, $level) = @_;
    select((select($fh), $|=1)[0]);
    my $in;
    my $buf = '';

    my $nextrepl = sub { $buf = ""; next repl };

    local *__;
    my $MSG = "('\\C-c' to exit, ',h' for help)";
    my %dhooks = (
                b => \&Sepia::debug_backtrace,
                i => \&Sepia::debug_inspect,
                e => \&Sepia::debug_upeval,
                r => \&Sepia::debug_return,
                h => \&Sepia::debug_help,
            );
    local *CORE::GLOBAL::die = sub {
        my @dieargs = @_;
        if ($stopdie) {
            local $dies = $dies+1;
            local $ps1 = "*$dies*$PS1";
            no strict;
            local %Sepia::REPL = (
                %dhooks, d => sub { local $Sepia::stopdie=0; die @dieargs });
            print "@_\nDied $MSG\n";
            return Sepia::repl($fh, 1);
        }
        CORE::die(@_);
    };

    local *CORE::GLOBAL::warn = sub {
        if ($stopwarn) {
            local $dies = $dies+1;
            local $ps1 = "*$dies*$PS1";
            no strict;
            local %Sepia::REPL = (
                %dhooks, w => sub { local $Sepia::stopwarn=0; warn @dieargs });
            print "@_\nWarned $MSG\n";
            return Sepia::repl($fh, 1);
        }
        CORE::warn(@_);
    };

    print $ps1;
    my @sigs = qw(INT TERM PIPE ALRM);
    local @SIG{@sigs};
    $SIG{$_} = $nextrepl for @sigs;
 repl: while (my $in = <$fh>) {
            $buf .= $in;
            my $iseval;
            if ($buf =~ /^<<(\d+)\n(.*)/) {
                $iseval = 1;
                my $len = $1;
                my $tmp;
                $buf = $2;
                while ($len && defined($tmp = read $fh, $buf, $len, length $buf)) {
                    $len -= $tmp;
                }
            }
            my (@res, @warn);
            local $SIG{__WARN__} = sub {
                push @warn, shift;
            };
            if ($buf =~ /^,(\S+)\s*(.*)/s) {
                ## Inspector shortcuts
                if (exists $Sepia::REPL{$1}) {
                    my $ret;
                    ($ret, @res) = $Sepia::REPL{$1}->($2, wantarray);
                    if ($ret) {
                        return wantarray ? @res : $res[0];
                    }
                } else {
                    print "Unrecignized shortcut '$1'\n";
                    $buf = '';
                    print $ps1;
                    next repl;
                }
            } else {
                ## Ordinary eval
                @res = repl_eval $buf, wantarray;

                if ($@) {
                    if ($@ =~ /at EOF$/m) {
                        ## Possibly-incomplete line
                        if ($in eq "\n") {
                            print "*** cancel ***\n$ps1";
                            $buf = '';
                        } else {
                            print ">> ";
                        }
                        next repl;
                    } else {
                        warn $@;
                        $buf = '';
                        Sepia::printer \@res, $iseval, wantarray if $iseval;
                    }
                }
            }
            if ($buf !~ /;$/) {
                ## Be quiet if it ends with a semicolon.
                Sepia::printer \@res, $iseval, wantarray;
            }
            $buf = '';
            if (@warn) {
                if ($iseval) {
                    my $tmp = "@warn";
                    print ';;;'.length($tmp)."\n$tmp\n";
                } else {
                    print "@warn\n";
                }
            }
            print $ps1;
        }
}

sub perl_eval
{
    tolisp(repl_eval(shift));
}

1;
