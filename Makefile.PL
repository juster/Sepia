use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'Devel::Xref',
    'VERSION_FROM'	=> 'Xref.pm', # finds $VERSION
    'PREREQ_PM'		=> { 'Emacs::EPL' => 0,
			     'Data::Dumper' => 0,
			     'Module::Info' => 0 },
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
     (AUTHOR     => "Sean O'Rourke <seano\@cpan.org>",
      ABSTRACT => 'Simple Emacs-Perl InterAction: ugly, yet effective.')
     : ())
);

print <<EOS;
*** Remember, to actually use this package in a useful way, you
*** probably need to move the Emacs Lisp files somewhere.  Where will
*** depend on your installation.
EOS
