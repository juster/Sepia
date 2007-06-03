package Sepia::Debug;
# use Sepia;
require Carp;
use Text::Abbrev;
use strict;
use vars qw($pack $file $line $sub $level
            $STOPDIE $STOPWARN);

## Just leave it on -- with $DB::trace = 0, there doesn't seem
## to be a perforamnce penalty!
BEGIN {
    $^P = 0x303;
    $STOPDIE = 1;
    $STOPWARN = 0;

    eval { require PadWalker; import PadWalker qw(peek_my) };
    if ($@) {
        *peek_my = sub { +{ } };
    }
}

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

sub repl_return
{
    (1, Sepia::repl_eval(@_));
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

sub repl_upeval
{
    my ($lev, $exp) = $_[0] =~ /^\s*(\d+)\s+(.*)/;
    print " <= $exp\n";
    (0, eval_in_env($exp, peek_my(0+$lev)));
}

sub repl_inspect
{
    my $i = shift || $level;
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

sub breakpoint
{
    my ($file, $line, $cond) = @_;
    if (!defined $main::{"_<$file"} && $file !~ /^\//) {
        ($file) = grep /^_<.*\/\Q$file\E$/, keys %main::;
        return unless $file;
        $file =~ s/^_<//;
    }
    $main::{"_<$file"}{$line} = $cond || 1;
    $file;
}

sub repl_break
{
    my $arg = shift;
    $arg =~ s/^\s+//;
    $arg =~ s/\s+$//;
    my ($f, $l, $cond) = $arg =~ /^(.+?):(\d+)\s*(.*)/, $arg;
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

my %REPL = (
    delete => sub {
        my ($f, $l) = split /:/, shift;
        $f ||= $file;
        $l ||= $line;
        delete $main::{"_<$f"}{$l}; 0
    },

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

    list => sub {
        my @lines = eval shift;
        print join('', @{$main::{"_<$file"}}[@lines]);
        0
    },

    # quit => sub {
    #     debug(0);
    # },
    backtrace => \&repl_backtrace,
    inspect => \&repl_inspect,
    eval => \&repl_upeval,
    return => \&repl_return,
);

my %REPL_DOC = (
    break => 'break',
    continue => 'continue',
    delete => 'delete',
     next => 'next',
     list => 'list',
     step => 'step',
     quit => 'quit',
    up => 'up',
    down => 'down',
    backtrace =>
        'backtrace       show backtrace',
    inspect =>
        'inspect N ...   inspect lexicals in frame(s) N ...',
    eval =>
        'eval N EXPR     evaluate EXPR in lexical environment of frame N',
    return =>
        'return EXPR     return EXPR',
    quit =>
        'quit            keep on dying/warning',
 );

sub repl
{
    show_location;
    # print STDERR $main::{'_<'.$file}[$line]
    #     if defined $main::{'_<'.$file}[$line];

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
            die => sub { local $STOPDIE=0; die @dieargs },
            quit => sub { local $STOPDIE=0; die @dieargs });
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
            warn => sub { local $STOPWARN=0; warn @dieargs },
            quit => sub { local $STOPWARN=0; warn @dieargs });
        $DB::trace = $trace;
    } else {
        ## Avoid showing up in location information.
        CORE::warn(Carp::shortmess @_);
    }
}

1;
