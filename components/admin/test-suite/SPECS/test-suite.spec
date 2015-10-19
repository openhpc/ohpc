#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/OHPC_macros

Name:           test-suite%{PROJ_DELIM}
Version:        1.0
Release:        1
Summary:        OpenHPC Test Suite
License:        BSD-3-Clause
Group:          ohpc/admin
Source0:        test-suite%{PROJ_DELIM}.tar
BuildRequires:  automake%{PROJ_DELIM}

%define debug_package %{nil}

BuildRoot:     %{_tmppath}/%{name}-%{version}-build
DocDir:        %{OHPC_PUB}/doc/contrib

%define installPath %{OHPC_PUB}/tests

%description 

Standalone test suite for integration testing of OpenHPC components.

%prep
%setup -n test-suite%{PROJ_DELIM}

%build
./bootstrap

%install

rm -rf $RPM_BUILD_ROOT

%{__mkdir_p} %{installPath}
cp -a * %{buildroot}/%{installPath}

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%defattr(-,root,root)
%dir %{OHPC_HOME}
%{OHPC_PUB}
%doc docs/LICENSE

%changelog
