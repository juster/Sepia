#!/usr/bin/env perl
use Test::Simple tests => 18;

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

my @loc1 = Sepia::location('Sepia::location');
ok($loc1[0][0] =~ /Sepia\.pm$/, 'location');
ok((grep { $_ eq 'Sepia::location' } Sepia::apropos('location')), 'apropos');
# 4 to here
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

apply_to_loc(\&Sepia::Xref::callers);
apply_to_loc(\&Sepia::Xref::callees);
# 10 tests to here.

my @subs = Sepia::mod_subs('Sepia');
ok(all(map { defined &{"Sepia::$_"} } @subs), 'mod_subs');
ok(Sepia::module_info('Sepia', 'name') eq 'Sepia');
ok(Sepia::module_info('Sepia', 'version') eq $Sepia::VERSION);
ok(Sepia::module_info('Sepia', 'file') =~ /Sepia\.pm$/);
ok(Sepia::module_info('Sepia', 'is_core') == 0);

if (exists $INC{'Module/Info.pm'}) {
    my %mu;
    undef @mu{Sepia::module_info('Sepia', 'modules_used')};

    my @mu_exp = ('B', 'Carp', 'Cwd', 'Exporter', 'Module::Info',
                  'Scalar::Util', 'Text::Abbrev', 'strict', 'vars');

    ok(all(map { exists $mu{$_} } @mu_exp), "uses (@mu_exp)");
    ok((Sepia::module_info('Sepia', 'packages_inside'))[0] eq 'Sepia');
    ok((Sepia::module_info('Sepia', 'superclasses'))[0] eq 'Exporter');
} else {
    ok(1, "no module info");
    ok(1, "no module info");
    ok(1, "no module info");
}
# 18 to here.

exit;
