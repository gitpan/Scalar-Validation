use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'Scalar::Validation',
    VERSION_FROM      => 'lib/Scalar/Validation.pm', # finds $VERSION
    PREREQ_PM         => { 'Test::Exception'	=> '0'}, # or => 1.1
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/Scalar/Validation.pm', # retrieve abstract from module
       AUTHOR         => 'Ralf Peine <ralf.peine@jupiter-programs.de>') : (),
);
