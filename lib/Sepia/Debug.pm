package Sepia::Debug;
# use Sepia;
require Carp;
use Text::Abbrev;
use strict;
use vars qw($pack $file $line $sub $level
            $STOPDIE $STOPWARN);

BEGIN {
    ## Just leave it on -- with $DB::trace = 0, there doesn't seem
    ## to be a perforamnce penalty!
    $^P = 0x303;
    $STOPDIE = 1;
    $STOPWARN = 0;

    eval { require PadWalker; import PadWalker qw(peek_my) };
    if ($@) {
        *peek_my = sub { +{ } };
    }
}

# set debugging level
sub repl_debug
{
    debug(@_);
    0;
}

sub repl_backtrace
{
    for (my $i = 0; ; ++$i) {
        my ($pack, $file, $line, $sub) = caller($i);
        last unless $pack;
        print($i == $level+3 ? "*" : ' ', " [$i]\t$sub ($file:$line)\n");
    }
    0
}

# return value from die
sub repl_return
{
    (1, $Sepia::REPL{eval}->(@_));
}

sub repl_lsbreak
{
    no strict 'refs';
    for my $file (sort grep /^_</ && defined %{"::$_"}, keys %::) {
        my ($name) = $file =~ /^_<(.*)/;
        my @pts = keys %{"::$file"};
        next unless @pts;
        print "$name:\n";
        for (sort { $a <=> $b } @pts) {
            print "\t$_\t${$file}{$_}\n"
        }
    }
}

# evaluate EXPR in environment ENV
sub eval_in_env
{
    my ($expr, $env) = @_;
    local $Sepia::ENV = $env;
    my $str = '';
    for (keys %$env) {
        next unless /^([\$\@%])(.+)/;
        $str .= "local *$2 = \$Sepia::ENV->{'$_'}; ";
    }
    eval "do { no strict; $str $expr }";
}

sub tie_class
{
    my $sig = substr shift, 0, 1;
    return $sig eq '$' ? 'Tie::StdScalar'
        : $sig eq '@' ? 'Tie::StdArray'
            : $sig eq '%' ? 'Tie::StdHash'
                : die "Sorry, can't tie $sig\n";
}

# {
#     require Tie::Array;
#     require Tie::Hash;
#     require Tie::Scalar;
#     package Sepia::Array;
#     our @ISA = qw(Tie::StdArray);
#     sub TIEARRAY { bless $_[1], $_[0] }
#     package Sepia::Hash;
#     our @ISA = qw(Tie::StdHash);
#     sub TIEHASH { bless $_[1], $_[0] }
#     package Sepia::Scalar;
#     our @ISA = qw(Tie::StdScalar);
#     sub TIESCALAR { bless $_[1], $_[0] }
# }

# sub eval_in_env3
# {
#     my ($expr, $env) = @_;
#     my @vars = grep /^([\$\@%])(.+)/, keys %$env;
#     my $body = 'sub { my ('.join(',', @vars).');';
#     for my $i (0..$#vars) {
#         $body .= "tie $vars[$i], ".tie_class($vars[$i]).', $_['.$i.'];';
#     }
#     $body .= "$expr }";
#     print STDERR "---\n$body\n---\n";
#     $body = eval $body;
#     $@ || $body->(@{$env}{@vars});
# }

## XXX: this is a better approach (the local/tie business is vile),
## but it segfaults and I'm not sure why.
sub eval_in_env2
{
    my ($expr, $env, $fn) = @_;
    local $Sepia::ENV = $env;
    my @vars = grep /^([\$\@%])(.+)/, keys %$env;
    my $body = 'sub { my ('.join(',', @vars).');';
    for (@vars) {
        $body .= "Devel::LexAlias::lexalias(\$Sepia::ENV, '$_', \\$_);"
    }
    $body .= "$expr }";
    print STDERR "---\n$body\n---\n";
    $body = eval $body;
    $@ || $body->();
}

# evaluate EXP LEV levels up the stack
sub repl_upeval
{
    my $exp = shift;
    # my ($lev, $exp) = $_[0] =~ /^\s*(\d+)\s+(.*)/;
    # print " <= $exp\n";
    # (0, eval_in_env2($exp, $level));
    # (0, eval_in_env3($exp, peek_my(4 + $level)));
    eval_in_env($exp, peek_my(4+$level));
}

# inspect lexicals at level N, or current level
sub repl_inspect
{
    my $i = shift;
    if ($i =~ /\d/) {
        $i = 0+$i;
    } else {
        $i = $level + 3;
    }
    my $sub = (caller $i)[3];
    if ($sub) {
        my $h = peek_my($i+1);
        print "[$i] $sub:\n";
        for (sort keys %$h) {
            local @Sepia::res = $h->{$_};
            print "\t$_ = ", $Sepia::PRINTER->(), "\n";
        }
    }
    0;
}

sub debug
{
    my $new = Sepia::as_boolean(shift, $DB::trace);
    return if $new == $DB::trace;
    if ($new) {
        # $^P = 0x2 | 0x10 | 0x100 | 0x200;
        # *DB::DB = \&repl;
        $DB::trace = 1;
        print "debug ON\n";
    } else {
        $DB::trace = 0;
        print "debug OFF\n";
    }
}

sub breakpoint_file
{
    my ($file) = @_;
    return \%{$main::{"_<$file"}} if exists $main::{"_<$file"};
    if ($file !~ /^\//) {
        ($file) = grep /^_<.*\/\Q$file\E$/, keys %main::;
        return \%{$main::{$file}} if $file;
    }
    return undef;
}

sub breakpoint
{
    my ($file, $line, $cond) = @_;
    my $h = breakpoint_file $file;
    if (defined $h) {
        $h->{$line} = $cond || 1;
        return $cond ? "$file\:$line if $cond" : "$file\:$line";
    }
    return undef;
}

sub repl_break
{
    my $arg = shift;
    $arg =~ s/^\s+//;
    $arg =~ s/\s+$//;
    my ($f, $l, $cond) = $arg =~ /^(.+?):(\d+)\s*(.*)/;
    $cond ||= 1;
    $f ||= $file;
    $l ||= $line;
    print "break ", breakpoint($f, $l, $cond), "\n";
    0;
}

sub update_location
{
    # XXX: magic numberage.
    ($pack, $file, $line, $sub) = caller($level + shift);
}

sub show_location
{
    print "_<$file:$line>\n" if defined $file && defined $line;
}

sub repl_list
{
    my @lines = eval shift;
    @lines = $line - 5 .. $line + 5 unless @lines;
    printf '%-6d%s', $_, ${"::_<$file"}[$_-1] for @lines;
    0
}

sub repl_delete
{
    my ($f, $l) = split /:/, shift;
    $f ||= $file;
    $l ||= $line;
    my $h = breakpoint_file $f;
    delete $h->{$l} if defined $h;
    0
}

my %parent_repl = (
    delete => \&repl_delete,
    debug => \&repl_debug,
    break => \&repl_break,
    lsbreak => \&repl_lsbreak,
);

my %parent_doc = (
    break =>
        'break [F:N [E]]    Set a breakpoint in F at line N (or at current
                       position), enabled if E evalutes to true.',
    delete =>
        'delete             Delete current breakpoint.',
    debug =>
        'debug [0|1]        Enable or disable debugging.',
    lsbreak =>
        'lsbreak            List breakpoints.',
);

sub add_repl_commands
{
    %Sepia::REPL = (%Sepia::REPL, %parent_repl);
    %Sepia::REPL_DOC = (%Sepia::REPL_DOC, %parent_doc);
    %Sepia::RK = abbrev keys %Sepia::REPL;
}

my %REPL = (
    up => sub {
        $level += shift || 1;
        update_location(4);
        show_location;
        0
    },
    down => sub {
        $level -= shift || 1;
        $level = 0 if $level < 0;
        update_location(4);
        show_location;
        0
    },

    continue => sub {
        $level = 0;
        $DB::single = 0; 1
    },

    next => sub {
        my $n = shift || 1;
        $DB::single = 0;
        breakpoint $file, $line + $n, 'next'; 1
    },

    step => sub {
        $DB::single = shift || 1; 1
    },

    break => \&repl_break,

    list => \&repl_list,

    # quit => sub {
    #     debug(0);
    # },
    backtrace => \&repl_backtrace,
    inspect => \&repl_inspect,
    # eval => \&repl_upeval,
    return => \&repl_return,
    lsbreak => \&repl_lsbreak,
    eval => \&repl_upeval,      # DANGER!
);

my %REPL_DOC = (
    continue =>
        'continue        Yep.',
    next =>
        'next [N]        Advance N lines, skipping subroutines.',
    list =>
        'list EXPR       List source lines of current file.',
    step =>
        'step [N]        Step N lines forward, entering subroutines.',
    quit =>
        'quit            Exit the current prompt level.',
    up =>
        'up [N]          Move up N stack frames.',
    down =>
        'down [N]        Move down N stack frames.',
    backtrace =>
        'backtrace       show backtrace',
    inspect =>
        'inspect [N]     inspect lexicals in frame N (or current)',
    eval =>
        'eval EXPR       evaluate EXPR in current frame',
    return =>
        'return EXPR     return EXPR',
    quit =>
        'quit            keep on dying/warning',
 );

sub repl
{
    show_location;

    local %Sepia::REPL = (%Sepia::REPL, %REPL, @_);
    local %Sepia::REPL_DOC = (%Sepia::REPL_DOC, %REPL_DOC);
    local %Sepia::RK = abbrev keys %Sepia::REPL;
    # local $Sepia::REPL_LEVEL = $Sepia::REPL_LEVEL + 1;
    local $Sepia::PS1 = "*$Sepia::REPL_LEVEL*> ";
    Sepia::repl();
}

sub DB::DB
{
    return if $Sepia::ISEVAL;
    local $level = 0;
    local ($pack, $file, $line, $sub) = caller($level);
    ## Don't do anything if we're inside an eval request, even if in
    ## single-step mode.
    return unless $DB::single || exists $main::{"_<$file"}{$line};
    if ($DB::single) {
        return unless --$DB::single == 0;
    } else {
        my $cond = $main::{"_<$file"}{$line};
        if ($cond eq 'next') {
            delete $main::{"_<$file"}{$line};
        } else {
            return unless eval $cond;
        }
    }
    repl();
}

my $MSG = "('\\C-c' to exit, ',h' for help)";

sub die
{
    ## Protect us against people doing weird things.
    if ($STOPDIE && !$SIG{__DIE__}) {
        my @dieargs = @_;
        local $level = 0;
        local ($pack, $file, $line, $sub) = caller($level);
        print "@_\n\tin $sub\nDied $MSG\n";
        my $trace = $DB::trace;
        $DB::trace = 1;
        repl(
            die => sub { local $STOPDIE=0; CORE::die @dieargs },
            quit => sub { local $STOPDIE=0; CORE::die @dieargs });
        $DB::trace = $trace;
    } else {
        CORE::die(Carp::shortmess @_);
    }
}

sub warn
{
    ## Again, this is above our pay grade:
    if ($STOPWARN && $SIG{__WARN__} eq 'Sepia::sig_warn') {
        my @dieargs = @_;
        my $trace = $DB::trace;
        $DB::trace = 1;
        local $level = 0;
        local ($pack, $file, $line, $sub) = caller($level);
        print "@_\n\tin $sub\nWarned $MSG\n";
        repl(
            warn => sub { local $STOPWARN=0; CORE::warn @dieargs },
            quit => sub { local $STOPWARN=0; CORE::warn @dieargs });
        $DB::trace = $trace;
    } else {
        ## Avoid showing up in location information.
        CORE::warn(Carp::shortmess @_);
    }
}

1;
