use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile(
    'NAME'		=> 'Sepia',
    'VERSION_FROM'	=> 'lib/Sepia.pm', # finds $VERSION
    'PREREQ_PM'         => { 'Data::Dumper'    => 0,
			     'B::Module::Info' => 0,
                             'Scalar::Util'    => 0,
                         },
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
     (AUTHOR     => "Sean O'Rourke <seano\@cpan.org>",
      ABSTRACT => 'Simple Emacs-Perl InterAction')
     : ())
);

print <<EOS;
NOTE: To actually use this package in a useful way, you probably need
to move the Emacs Lisp files somewhere.  Where will depend on your
installation.
EOS

eval { require PadWalker };
if ($@) {
    print <<EOS;

Some features of the interactive debugger require PadWalker >= 1.0.
Please install it to enable them.
EOS
}
