#!/usr/bin/env perl

use ExtUtils::Installed;

print "<html><body><ul>";
for (sort ExtUtils::Installed->new->modules) {
    print qq{<li><a href="about://perldoc/$_">$_</a>};
}
print "</ul></body></html>\n";
