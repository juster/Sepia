package Sepia::CPAN;
use CPAN ();
use LWP::Simple;

sub init
{
      CPAN::HandleConfig->load;
      CPAN::Shell::setup_output;
      CPAN::Index->reload;
}

sub interesting_parts
{
    my $mod = shift;
    +{ map { $_ => scalar $mod->$_ } qw(id cpan_version inst_version fullname cpan_file)};
}

sub _list
{
    CPAN::Shell->expand('Module', shift || '/./');
}

sub list
{
    map { interesting_parts $_ } _list @_
}

sub _ls
{
    my $want = shift;
    grep {
        # XXX: key to test in this order, because inst_file is slow.
        $_->userid eq $want
    } CPAN::Shell->expand('Module', '/./')
}

sub ls
{
    map { interesting_parts $_ } _ls @_
}

sub _desc
{
    my $pat = qr/$_[0]/i;
    grep {
        ($_->description =~ /$pat/ || $_->id =~ /$pat/)
    } CPAN::Shell->expand('Module', '/./');
}

sub desc
{
    map { interesting_parts $_ } _desc @_;
}

sub outdated
{
    grep !$_->uptodate, list @_;
}

## stolen from CPAN::Shell...
sub readme
{
    my $dist = CPAN::Shell->expand('Module', shift);
    return unless $dist;
    my $wantfile = shift;
    $dist = $dist->cpan_file;
    # my ($dist) = $self->id;
    my ($sans, $suffix) = $dist =~ /(.+)\.(tgz|tar[\._-]gz|tar\.Z|zip)$/;
    my ($local_file);
    my ($local_wanted) = File::Spec->catfile(
        $CPAN::Config->{keep_source_where}, "authors", "id",
        split(/\//,"$sans.readme"));
    $local_file = CPAN::FTP->localize("authors/id/$sans.readme", $local_wanted);
    ## Return filename rather than contents to avoid Elisp reader issues...
    if ($wantfile) {
        $local_file;
    } else {
        local (*IN, $/);
        open IN, $local_wanted;
        my $ret = <IN>;
        close IN;
        $ret;
    }
}

sub perldoc
{
    get($CPAN::Defaultdocs . shift);
}

sub install
{
    my $dist = CPAN::Shell->expand('Module', shift);
    $dist->install if $dist;
}

# Based on CPAN::Shell::_u_r_common
sub _recommend
{
    my $pat = shift || '/./';
    my (@result, %seen, %need);
    $version_undefs = $version_zeroes = 0;
    for my $module (CPAN::Shell->expand('Module',$pat)) {
        my $file  = $module->cpan_file;
        next unless defined $file && $module->inst_file;
        $file =~ s!^./../!!;
        my $latest = $module->cpan_version;
        my $have = $module->inst_version;
        local ($^W) = 0;
        next unless CPAN::Version->vgt($latest, $have);
        push @result, $module;
        next if $seen{$file}++;
        $need{$module->id}++;
    }
    @result;
}

sub recommend
{
    map { interesting_parts $_ } _recommend @_;
}

1;
