package Sepia;
our $VERSION = '0.56';

require Exporter;
our @ISA = qw(Exporter);

use strict;
use Cwd 'abs_path';
use Module::Info;
use B;

sub _apropos_re($)
{
    # Do that crazy multi-word identifier completion thing:
    my $re = shift;
    if ($re !~ /[^\w\d_^:]/) {
	$re =~ s/(?<=[A-Za-z\d])([^A-Za-z\d])/[A-Za-z\\d]*$1+/g;
    }
    qr/$re/;
}

=item C<completions($string)>

=cut

sub completions
{
    no strict;
    my ($str, $pack) = @_;
    if (my ($pfx, $name) = $str =~ /^([\%\$\@]?)(.+)/) {
        my @nameparts = split /:+/, $name;
        if (@nameparts == 1 && $pack) {
            @nameparts = (split(/:+/, $pack), $name);
        }
        local *_completions = sub {
            no strict;
            my ($stash, $part, @rest) = @_;
            $part = join '[^_]*_', split /_/, $part;
            $part = _apropos_re($part);
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

sub location
{
    no strict;
    map {
        my $str = $_;
        if (my ($pfx, $name) = $str =~ /^([\%\$\@]?)(.+)/) {
            if ($pfx) {
                print STDERR "Sorry -- can't lookup variables.";
                [];
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
                    [Cwd::abs_path($file), $line, $shortname || $name]
                } else {
                    [];
                }
            }
        }
    } @_
}

=item C<apropos($name [, $is_regex])>

=cut

sub apropos
{
    no strict;
    my ($it, $re) = @_;
    if ($it =~ /^(.*::)([^:]+)$/) {
        my ($stash, $name) = @_;
        if ($re) {
            my $name = qr/$name/;
            map {
                "$stash$name"
            }
            grep {
                /$name/ && defined &{"$stash$name"}
            } keys %$stash;
        } else {
            defined &$it ? $it : ();
        }
    } else {
        my @ret;
        my $findre = $re ? qr/$it/ : qr/^\Q$it\E$/;
        print STDERR "Searching for $findre...";
        my_walksymtable {
            push @ret, "$stash$_" if /$findre/;
        } '::';
        map { s/^:://;$_ } @ret;
    }
}

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

=item C<mod_subs($pack)>

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

=item C<mod_decls($pack)>

Generate a list of declarations for all subroutines in package
C<$pack>.

=cut

sub mod_decls
{
    my $pack = shift;
    no strict 'refs';
    my @ret = map {
	my $sn = $_->[2];
	my $proto = prototype(\&{"$pack\::$sn"});
	$proto = defined($proto) ? "($proto)" : '';
	"sub $sn $proto;\n";
    } mod_subs($pack);
    return wantarray ? @ret : join '', @ret;
}

=item C<module_info($module, $type)>

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

1;
