package Sepia;
our $VERSION = '0.60';

require Exporter;
our @ISA = qw(Exporter);

use strict;
use Cwd 'abs_path';
use Scalar::Util 'looks_like_number';
use Module::Info;
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
                print STDERR "Sorry -- can't lookup variables.";
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
                    print STDERR "Bad CV for $name: $cv";
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
            0+$thing;
        } else {
            '"'.$thing.'"';
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

sub printer
{
    no strict;
    local *res = shift;
    my $marker = shift;
    if ($marker) {
        print "\n$marker\n@res\n$marker\n";
    } else {
        print "=> @res\n";
    }
}

=item C<repl(\*FH)>

Execute a command prompt on FH.

=cut

sub repl
{
    my $fh = shift;
    my $buf = '';
    my $ps1 = "> ";
    print $ps1;
    repl: while (my $in = <$fh>) {
            $buf .= $in;
            my $marker;
            if ($buf =~ /^eval\s+<<(REPL\S*)\s*$/) {
                $marker = $1;
                $buf = '';
                local $/ = "\n$1\n";
                chomp($buf = <$fh>);
            }
            local $SIG{INT} = sub { $buf = ""; next repl };
            my @warn;
            local $SIG{__WARN__} = sub {
                push @warn, shift;
            };
            my @res;
            {
                no strict;
                @res = eval $buf;
            }
            if ($@) {
                if ($@ =~ /at EOF$/m) {
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
                    Sepia::printer \@res, $marker if $marker;
                }
            } else {
                Sepia::printer \@res, $marker unless $buf =~ /;$/;
                $buf = '';
            }
            print "@warn\n" if @warn;
            print $ps1;
        }
}

sub perl_eval
{
    tolisp(eval shift);
}

1;
