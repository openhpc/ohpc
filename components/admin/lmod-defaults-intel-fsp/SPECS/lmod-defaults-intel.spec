%include %{_sourcedir}/FSP_macros

%define pname lmod
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary:   FSP default login environment
Name:      lmod-defaults-intel
Version:   1.0
Release:   1
License:   Intel
Group:     Development/Tools
BuildArch: noarch
Source0:   FSP_macros
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

# FSP dependencies
requires: lmod%{PROJ_DELIM}

%description

Provides default login environment for compiler and MPI combinations.

%prep

%build

%install

mkdir -p %{buildroot}/%{FSP_MODULES}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/fsp
#%Module1.0#####################################################################
# Default FSP environment
#############################################################################

proc ModulesHelp { } {
puts stderr "Setup default login environment"
}

#
# Load Desired Modules
#

if { [ expr [module-info mode load] || [module-info mode display] ] } {
        prepend-path MANPATH /usr/local/share/man:/usr/share/man/overrides:/usr/share/man/en:/usr/share/man
	module try-add autotools
        module try-add intel
        module try-add impi
}

if [ module-info mode remove ] {
        module del impi
        module del intel
	module del autotools
}
EOF

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{FSP_PUB}

%changelog

