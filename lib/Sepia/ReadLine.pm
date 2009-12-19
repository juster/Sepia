package Sepia::ReadLine;
use Term::ReadLine;
use Sepia;
require Exporter;
@ISA='Exporter';
@EXPORT='repl';

sub rl_attempted_complete
{
    my ($text, $line, $start, $end) = @_;
    my @xs;
    if (substr($line, 0, $start) =~ /^\s*$/ && $text =~ /^,(\S*)$/) {
        my $x = qr/^\Q$1\E/;
        @xs = map ",$_", grep /$x/, keys %Sepia::REPL;
    } else {
        my ($type, $str) = (substr $line, $start && ($start-1), $end) =~ /^([\$\@\%\&]?)(.*)/;
        my %h = qw(@ ARRAY % HASH & CODE * IO $ VARIABLE);
        @xs = Sepia::completions $h{$type||'&'}, $str;
    }
    $TERM->completion_matches($text,
                              sub { $_[1] < @xs ? $xs[$_[1]] : () });
}

sub repl
{
    { package main; do $_ for @ARGV }
    $TERM = new Term::ReadLine;
    my $attr = $TERM->Attribs;
    # $attr->{completion_entry_function} = \&rl_complete;
    $attr->{attempted_completion_function} = \&rl_attempted_complete;
    $Sepia::READLINE = sub { $TERM->readline(Sepia::prompt()) };
    goto &Sepia::repl;
}

1;
