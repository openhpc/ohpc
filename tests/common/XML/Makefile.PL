use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'XML::Generator',
    'PM'	=> { 'Generator.pm' => '$(INST_LIBDIR)/Generator.pm',
		     'DOM.pm'       => '$(INST_LIBDIR)/Generator/DOM.pm' },
    'MAN3PODS'  => { 'Generator.pm' => '$(INST_MAN3DIR)/XML::Generator.3',
		     'DOM.pm'       => '$(INST_MAN3DIR)/XML::Generator::DOM.3' },
    'VERSION_FROM' => 'Generator.pm', # finds $VERSION
);
