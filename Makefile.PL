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
$install_el = 0;
$emacs = quotemeta prompt("What is your emacs? ", $ENV{EMACS} || 'emacs');
$lp = `$emacs --batch --eval '(message "\%s" load-path)' 2>&1`;
$lp =~ s/^.*\(//s;
$lp =~ s/\).*$//s;
for (split ' ', $lp) {
    if (/site-lisp$/ && -d $_) {
        $site_lisp = $_;
        last;
    }
}
$site_lisp = prompt("Elisp install directory?", $site_lisp);
if (-d $site_lisp) {
    $install_el = 1;
} else {
    warn "$site_lisp is not a directory.\n";
}

$site_lisp = quotemeta $site_lisp;

# Also try to install texinfo documentation.
$install_info = quotemeta prompt("install-info program?", 'install-info');
$info_dir = `$emacs --batch --eval '(progn (require (quote info)) (message "\%s" (car Info-default-directory-list)))' 2>&1`;
chomp $info_dir;
$info_dir = prompt("Info directory?", $info_dir);
if (!-d $info_dir) {
    warn "Not installing info files.\n";
    undef $install_info;
}

$info_dir = quotemeta $info_dir;

sub MY::postamble
{
    my $ret;
    if ($install_info) {
        $ret = <<EOS;
	\$(CP) sepia.info $info_dir
	$install_info $info_dir/sepia.info $info_dir/dir
EOS
    }
    if ($install_el) {
        $ret .= <<EOS;
install ::
	\@echo Compiling Elisp files.
	$emacs -L '$ENV{PWD}' --batch -f batch-byte-compile *.el 2>/dev/null
	\$(CP) *.el *.elc $site_lisp
EOS
    } else {
        print <<EOS;
NOTE:
    To actually use this package, you need to move the Emacs Lisp files
    somewhere Emacs will find them.

    You may also want to install the HTML documentation somewhere
    appropriate to your system.
EOS
    }
    $ret;
}

WriteMakefile(
    'NAME'		=> 'Sepia',
    'VERSION_FROM'	=> 'lib/Sepia.pm', # finds $VERSION
    'PREREQ_PM'         => \%prereq,
    AUTHOR     => "Sean O'Rourke <seano\@cpan.org>",
    ABSTRACT => 'Simple Emacs-Perl InterAction',
);
