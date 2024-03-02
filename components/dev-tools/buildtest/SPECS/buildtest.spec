#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#-------------------------------------------------------------------------------

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %global PROJ_DELIM -ohpc}

%define pname buildtest

Summary:   HPC Testing Framework
Name:      %{pname}%{PROJ_DELIM}
Version:   0.13.0
Release:   1%{?dist}
License:   MIT
Group:     %{PROJ_NAME}/dev-tools
URL:       https://github.com/buildtesters/buildtest
Source0:   https://github.com/buildtesters/buildtest/archive/v%{version}.tar.gz

BuildRequires: python3
Requires:  bash
Requires: python3

BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root

%define install_path %{OHPC_LIBS}/%{pname}/%{version}


%description
buildtest is a testing framework to help HPC sites write test for their system as
part of their routine acceptance & regression test. buildtest provides a YAML
interface to write tests which buildtest can validate and generate shell scripts
that can run on your HPC system. The test template (YAML) is called buildspec
which can define one or more test instance that is validated by a json schema.

%prep
%setup -n %{pname}-%{version}

%build

%install
mkdir -p %{buildroot}%{install_path}
rsync -av --exclude=.gitignore * %{buildroot}%{install_path}


# OpenHPC module file
%{__mkdir} -p %{buildroot}/%{OHPC_ADMIN}/modulefiles/buildtest
%{__cat} << EOF > %{buildroot}/%{OHPC_ADMIN}/modulefiles/buildtest/%{version}
#%Module1.0#####################################################################

module-whatis "Name: buildtest"
module-whatis "Version: %{version}"
module-whatis "Category: system tool"
module-whatis "Description: %{summary}"
module-whatis "URL: https://github.com/buildtesters/buildtest"

set     version             %{version}
set     BUILDTEST_ROOT      %{install_path}

prepend-path   PATH         %{install_path}/bin

EOF

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_HOME}

%changelog
* Thu Mar 10 2020 <shahzebmsiddiqui@gmail.com>
- Update spec to use version 0.13.0

* Mon Sep 15 2014  <karl.w.schulz@intel.com> - 
- Initial build.

