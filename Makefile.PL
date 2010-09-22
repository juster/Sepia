use warnings;
use strict;

use ExtUtils::MakeMaker qw(WriteMakefile prompt);
use 5.006;                      # for "no warnings" -- sorry!
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

my %prereq = (
    'Data::Dumper'    => 0,
    'Scalar::Util'    => 0,
);

## Poor man's optional deps.
sub test_for
{
    my $mod = shift;
    eval "require $mod";
    if ($@) {
        my $resp = prompt("@_.  Install $mod (y/n)? ", 'n');
        $prereq{$mod} = 0 if $resp =~ /^y/i;
    }
}

test_for 'Devel::Peek', 'Printing internals requires Devel::Peek';
test_for 'Devel::Size', 'Printing variable sizes requires Devel::Size';
test_for 'IO::Scalar', 'Printing internals requires IO::Scalar because Devel::Peek sucks.';
test_for 'Lexical::Persistence', 'Strict mode requires Lexical::Persistence';
test_for 'LWP::Simple', 'CPAN documentation browsing requires LWP::Simple';
test_for 'Module::CoreList', 'sepia-core-version requires Module::CoreList';
test_for 'Module::Info', 'Required for some Emacs functions';
test_for 'PadWalker', 'Stack/lexical inspection requires PadWalker >= 1.0';
test_for 'BSD::Resource', 'Detailed command timing.';
test_for 'Time::HiRes', 'Basic command timing.';
# test_for 'Pod::Webserver', 'Pod::Webserver creates nice documentation.';
# test_for 'Scope::Upper', 'Required for return-from-context';

# try to compile and install Elisp files, but don't die if we can't.
my $install_el = 0;
my ($sysemacs) = grep { -x $_ } $ENV{EMACS}, glob '/usr/bin{/local,}/emacs';
my $emacs = quotemeta prompt("What is your emacs? ", $sysemacs);
my @lp = `$emacs --batch --eval '(dolist (x load-path) (princ x) (terpri))' 2>/dev/null`;
chomp @lp;
my ($site_lisp) = grep { /site-lisp$/ && -d $_ } @lp;
$site_lisp = prompt("Elisp install directory?", $site_lisp);
if (-d $site_lisp) {
    $install_el = 1;
} else {
    warn "$site_lisp is not a directory.\n";
}

$site_lisp = quotemeta $site_lisp;

# Also try to install texinfo documentation.
my $install_info = quotemeta prompt("install-info program?", 'install-info');
my $info_dir = `$emacs --batch --eval '(progn (require (quote info)) (princ (car Info-default-directory-list)))' 2>/dev/null`;
chomp $info_dir;
$info_dir = prompt("Info directory?", $info_dir);
if (!-d $info_dir) {
    warn "Not installing info files.\n";
    undef $install_info;
}

$info_dir = quotemeta $info_dir;

# Append info page and elisp installation to the end of the install
# section in the Makefile...

sub MY::postamble
{
    my $maketxt = <<"END_MAKETXT";
install ::
END_MAKETXT

    if ( $install_info ) { $maketxt .= <<"END_MAKETXT" }
	install -d -m755 \$(DESTDIR)$info_dir
	install -m644 sepia.info \$(DESTDIR)$info_dir
	$install_info "$info_dir/sepia.info" "$info_dir/dir"
	
END_MAKETXT

    if ( $install_el ) {
        $maketxt .= <<"END_MAKETXT"
	install -d -m755 \$(DESTDIR)$site_lisp
	install -m644 *.el *.elc \$(DESTDIR)$site_lisp
END_MAKETXT
    } else {
        print <<'END_MSG'

======================================================================
To actually use this package, you need to move the Emacs Lisp files
somewhere Emacs will find them.

You may also want to install the HTML documentation somewhere
appropriate to your system.
======================================================================

END_MSG
    }

    # Create a new target for compiled elisp (.elc) files...
    # Allow the compilation to fail (it is prefixed with "-")...
    $maketxt .= <<"EOS";

\%.elc : \%.el
	- $emacs -L '$ENV{PWD}' --batch -f batch-byte-compile \$? 2>/dev/null

all :: sepia-snippet.elc sepia.elc sepia-cpan.elc sepia-tree.elc \\
	   sepia-w3m.elc sepia-ido.elc
EOS

    return $maketxt;
}

WriteMakefile(
    NAME		=> 'Sepia',
    VERSION_FROM	=> 'lib/Sepia.pm', # finds $VERSION
    PREREQ_PM		=> \%prereq,
    EXE_FILES		=> ['sepl'],
    AUTHOR		=> "Sean O'Rourke <seano\@cpan.org>",
    ABSTRACT		=> 'Simple Emacs-Perl InterAction',
    clean           => { FILES => '*.elc' },
);
