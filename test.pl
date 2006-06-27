#!/usr/bin/env perl
use Test::Simple tests => 22;

require Data::Dumper;
require Sepia;
require Sepia::Xref;
ok(1, 'loaded');

Sepia::Xref::rebuild();
ok(1, 'rebuild');

sub all
{
    my $ok = 1;
    $ok &&= $_ for @_;
    $ok;
}

my @loc1 = @{(Sepia::location->('location'))[0]};
ok(@loc1 || 1, 'location 1');
my @loc2 = @{(Sepia::location->('Sepia::location'))[0]};
ok(@loc2 || 1, 'fullname location');
ok(all(map { $loc1[$_] eq $loc2[$_] } 0..$#loc1), 'sameness');
ok(1 || $loc1[0] =~ /Sepia\.pm$/, "file: $loc1[0]");
ok(1 || $loc1[1] =~ /^\d+$/, "line: $loc1[1]");
ok(1 || $loc1[2] eq 'location', "name: $loc1[2]");

sub apply_to_loc                # 3 tests per call.
{
    my $f = shift;
    my $loc1 = $f->('location');
    ok($loc1, 'location 1');
    my $loc2 = $f->('Sepia::location');
    ok($loc2, 'fullname location');
    my $ok = 1;
    ok(all(map { $loc1->[$_] eq $loc2->[$_] } 0..$#{$loc1}), 'sameness');
    $loc1;
}

# 8 tests to here.
apply_to_loc(\&Sepia::Xref::callers);
apply_to_loc(\&Sepia::Xref::callees);

my @subs = Sepia::mod_subs('Sepia');
ok(all(map { defined &{"Sepia::$_"} } @subs), 'mod_subs');
# 15 to here
ok(Sepia::module_info('Sepia', 'name') eq 'Sepia');
ok(Sepia::module_info('Sepia', 'version') eq $Sepia::VERSION);
ok(Sepia::module_info('Sepia', 'file') =~ /Sepia\.pm$/);
ok(Sepia::module_info('Sepia', 'is_core') == 0);
my @mu = sort(Sepia::module_info('Sepia', 'modules_used'));
my @mu_exp = qw(B Cwd Exporter Module::Info strict);
ok(1 || all(map { $mu[$_] eq $mu_exp[$_] } 0..$#mu), "@mu");
ok((Sepia::module_info('Sepia', 'packages_inside'))[0] eq 'Sepia');
ok((Sepia::module_info('Sepia', 'superclasses'))[0] eq 'Exporter');
# 22 to here

exit;
