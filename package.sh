#!/bin/sh

makeinfo --html sepia.texi -o doc
cat MANIFEST.in > MANIFEST
ls doc/*.html >> MANIFEST
perl Makefile.PL && make && make test && make dist
