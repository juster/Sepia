use Term::ReadLine;
use Sepia;
package Sepia::ReadLine;

sub rl_attempted_complete
{
    my ($text, $line, $start, $end) = @_;
    my @xs;
    if (substr($line, 0, $start) =~ /^\s*$/ && $text =~ /^,(\S*)$/) {
        my $x = qr/^\Q$1\E/;
        @xs = map ",$_", grep /$x/, keys %Sepia::REPL;
    } else {
        my ($type, $str) = (substr $line, $start-1, $end) =~ /^([\$\@\%\&]?)(.*)/;
        my %h = qw(@ ARRAY % HASH & CODE * IO $ SCALAR);
        @xs = Sepia::completions $h{$type||'&'}, $str;
        # if (@xs == 1) {
        #     return $xs[0], @xs;
        # }
        @xs = map { s/^[\$\@\%\&]?//; $_ } @xs;
    }
    $TERM->completion_matches($text,
                              sub { $_[1] < @xs ? $xs[$_[1]] : () });
}

sub repl
{
    $TERM = new Term::ReadLine;
    my $attr = $TERM->Attribs;
    # $attr->{completion_entry_function} = \&rl_complete;
    $attr->{attempted_completion_function} = \&rl_attempted_complete;
    $Sepia::READLINE = sub { $TERM->readline(Sepia::prompt()) };
    goto &Sepia::repl;
}

1;
