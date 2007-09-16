use ExtUtils::MakeMaker;
use 5.006;                      # for "no warnings" -- sorry!
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile(
    'NAME'		=> 'Sepia',
    'VERSION_FROM'	=> 'lib/Sepia.pm', # finds $VERSION
    'PREREQ_PM'         => { 'Data::Dumper'    => 0,
			     'B::Module::Info' => 0,
                             'Scalar::Util'    => 0,
                         },
    AUTHOR     => "Sean O'Rourke <seano\@cpan.org>",
    ABSTRACT => 'Simple Emacs-Perl InterAction',
    LICENSE => 'perl',
);

print <<EOS;
NOTE: To actually use this package in a useful way, you probably need
to move the Emacs Lisp files somewhere.  Where will depend on your
installation.
EOS

sub test_for
{
    my $mod = shift;
    eval "require $mod";
    if ($@) {
        print "@_\n";
    }
}

test_for 'PadWalker', 'Stack/lexical inspection requires PadWalker >= 1.0.';
test_for 'Lexical::Persistence', 'Strict mode requires Lexical::Persistence.';
test_for 'Module::CoreList',
    'sepia-core-version requires Module::CoreList.';
