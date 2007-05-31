#!/bin/sh

makeinfo --html sepia.texi
cat MANIFEST.in > MANIFEST
ls sepia/*.html >> MANIFEST
perl Makefile.PL && make && make dist
