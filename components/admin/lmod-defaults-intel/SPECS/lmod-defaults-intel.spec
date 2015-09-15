#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the Performance Peak project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/FSP_macros

%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary:   FSP default login environment
Name:      lmod-defaults-intel%{PROJ_DELIM}
Version:   1.2
Release:   1
License:   BSD
Group:     fsp/admin
BuildArch: noarch
Source0:   FSP_macros
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

# FSP dependencies
requires: lmod%{PROJ_DELIM}

%description

Provides default login environment for Intel toolchain (compiler and MPI families).

%prep

%build

%install

mkdir -p %{buildroot}/%{FSP_MODULES}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/ohpc
#%Module1.0#####################################################################
# Default FSP environment
#############################################################################

proc ModulesHelp { } {
puts stderr "Setup default login environment"
}

#
# Load Desired Modules
#

prepend-path     PATH   %{FSP_PUB}/bin

if { [ expr [module-info mode load] || [module-info mode display] ] } {
        prepend-path MANPATH /usr/local/share/man:/usr/share/man/overrides:/usr/share/man/en:/usr/share/man
	module try-add autotools
	module try-add prun
        module try-add intel
        module try-add impi
}

if [ module-info mode remove ] {
        module del impi
        module del intel
        module del prun
	module del autotools
}
EOF

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%dir %{FSP_HOME}
%dir %{FSP_PUB}
%{FSP_MODULES}

%changelog

