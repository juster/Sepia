#!/bin/sh

makeinfo --no-split --no-headers --html sepia.texi -o Sepia.html
# cat MANIFEST.in > MANIFEST
# ls doc/*.html >> MANIFEST
# perl Makefile.PL && make && make test && make dist
perl Makefile.PL && make && make dist
