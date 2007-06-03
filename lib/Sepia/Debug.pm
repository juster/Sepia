package Sepia::Debug;
# use Sepia;
use Text::Abbrev;

## Just leave it on -- with $DB::trace = 0, there doesn't seem
## to be a perforamnce penalty!
BEGIN { $^P = 0x303 }

sub repl_debug
{
    debug(@_);
    0;
}

sub debug
{
    my $new = Sepia::as_boolean(shift, $DB::trace);
    return if $new == $DB::trace;
    $DB::trace = $new;
    if ($DB::trace) {
        # $^P = 0x2 | 0x10 | 0x100 | 0x200;
        # *DB::DB = \&db_db;
        $DB::trace = 1;
        print "debug ON\n";
    } else {
        # *DB::DB = sub {};

        # $^P = 0;
        print "debug OFF\n";
        $DB::trace = 0;
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
    my ($f, $l) = split /:/, $arg;
    $f ||= $file;
    $l ||= $line;
    print "break ", breakpoint($f, $l), "\n";
    0;
}

use vars qw($pack $file $line);

my $level;

my %REPL = (
    delete => sub {
        my ($f, $l) = split /:/, shift;
        $f ||= $file;
        $l ||= $line;
        delete $main::{"_<$f"}{$l}; 0
    },

    up => sub { ++$level; 0 },
    down => sub { --$level if $level > 1; 0 },

    continue => sub {
        $DB::single = 0; 1
    },

    next => sub {
        my $n = shift || 1;
        $DB::single = 0;
        breakpoint $file, $line + $n, 'next'; 1
    },

    step => sub {
        $DB::single = 1; 1
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
    backtrace => \&Sepia::debug_backtrace,
    inspect => \&Sepia::debug_inspect,
    eval => \&Sepia::debug_upeval,
    return => \&Sepia::debug_return,
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
    backtrace => 'backtrace       show backtrace',
    inspect => 'inspect N ...   inspect lexicals in frame(s) N ...',
    eval => 'eval N EXPR     evaluate EXPR in lexical environment of frame N',
    return => 'return EXPR     return EXPR',
    # die => 'die/warn        keep on dying/warning',
 );

sub DB::DB
{
    return if $Sepia::ISEVAL;
    local ($pack, $file, $line) = caller($level);
    ## Don't do anything if we're inside an eval request, even if in
    ## single-step mode.
    return unless $DB::single || exists $main::{"_<$file"}{$line};

    delete $main::{"_<$file"}{$line} if $main::{"_<$file"}{$line} eq 'next';

    print STDERR "_<$file:$line>\n";
    # print STDERR $main::{'_<'.$file}[$line]
    #     if defined $main::{'_<'.$file}[$line];

    local %Sepia::REPL = (%Sepia::REPL, %REPL);
    local %Sepia::REPL_DOC = (%Sepia::REPL_DOC, %REPL_DOC);
    local %Sepia::RK = abbrev keys %Sepia::REPL;
    Sepia::repl(*STDIN, 1);
}

1;
