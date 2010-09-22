#!/usr/bin/env perl
use Test::More;

plan 'skip_all' => 'Test::Without::Module not installed.'
    unless eval { require Test::Without::Module; 1 };

plan 'tests' => 2;

my $str;
$str .= "use Test::Without::Module '$_';" for qw{
Devel::Peek
Devel::Size
IO::Scalar
Lexical::Persistence
LWP::Simple
Module::CoreList
Module::Info
PadWalker
BSD::Resource
Time::HiRes
};
eval $str;
my $res = eval q{use Time::HiRes;1};
ok(!$res, "Test::Without::Module works.");
$res = eval "use Sepia;1";
ok($res && !$@, "loads without optional prereqs? ($res, $@)");
