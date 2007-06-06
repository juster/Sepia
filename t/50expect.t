#!/usr/bin/env perl

BEGIN {
    eval 'use Test::Expect';
    if ($@) {
        print STDERR "All skipped -- requires Test::Expect.\n$@\n";
        print "0..0\n";
        exit 0;
    }
}

use Test::Simple tests => 32;
use FindBin '$Bin';
use Sepia;
use Sepia::Xref;

expect_run
    command => "$^X -Mblib -MSepia -MSepia::Xref -e 'Sepia::repl(\\*STDIN, \\*STDOUT)'",
    prompt => [-re => 'main @[^>]*> '],
    quit => ',quit';
expect_handle()->log_file('/tmp/b');

expect ",help",
q!REPL commands (prefixed with ','):
    break [F:N [E]]    Set a breakpoint in F at line N (or at current
                       position), enabled if E evalutes to true.
    cd DIR             Change directory to DIR
    debug [0|1]        Enable or disable debugging.
    delete             Delete current breakpoint.
    format [dumper|dump|yaml|plain]
                       Set output formatter (default: dumper)
    help               Display this message
    lsbreak            List breakpoints.
    methods X [RE]     List methods for reference or package X,
                       matching optional pattern RE.

    package PACKAGE    Set evaluation package to PACKAGE
    quit               Quit the REPL
    reload             Reload Sepia.pm and relaunch the REPL.
    shell CMD ...      Run CMD in the shell.
    strict [0|1]       Turn 'use strict' mode on or off
    wantarray [0|1]    Set or toggle evaluation context
    who PACKAGE [RE]   List variables and subs in PACKAGE matching optional
                       pattern RE.!;

expect ",wh Sepia::Xref xref",
'xref             xref_definitions xref_main
xref_cv          xref_exclude     xref_object      ';

expect_send '{ package A; sub a {}; package X; @ISA = qw(A); sub x {} };';
expect ",wh X", '@ISA x', 'package list';
expect ",me X", 'a x', 'methods 1';

expect '$x = bless {}, X;', '$x = bless {}, X;'; # XXX: stupid expect.
expect ',me $x', ",me \$x\na x", 'methods 2';    # XXX: stupid expect.

######################################################################
## Debugger
expect ',lsb', '';
expect_send ',debug 1';
expect_send "do '$Bin/testy.pl';", 'get testy';

expect 'fib1 10', '=> 55', 'plain fib';
expect ',br testy.pl:6', "break testy.pl:6 1", 'break?';
expect_send 'fib1 10';
expect_like qr|_<$Bin/testy.pl:6>|, 'break in fib';
expect ',del', '';
expect ',con', '=> 55', 'return from fib';
expect_send 'fib2 10', 'bad fib';
expect_like qr/_<$Bin\/testy.pl:12>/;
expect_send ',q';
expect_like qr/error: asdf/;
