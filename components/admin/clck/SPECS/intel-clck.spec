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

Summary:   Intel(R) Cluster Checker
Name:      intel-clck%{PROJ_DELIM}
Version:   3.0.0
Release:   1
License:   Intel
URL:       http://intel.com/go/cluster
Group:     fsp/admin
BuildArch: x86_64
Source1:   stream.static
Source2:   dgemm_mflops.static
Source3:   FSP_macros
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq:   no
Requires:  time

%define __spec_install_post /usr/lib/rpm/brp-strip-comment-note /bin/true
%define __spec_install_post /usr/lib/rpm/brp-compress /bin/true
%define __spec_install_post /usr/lib/rpm/brp-strip /bin/true

#!BuildIgnore: post-build-checks rpmlint-Factory
%define debug_package %{nil}

%description

Intel cluster checker.

%prep

%build

%install

%{__mkdir} -p %{buildroot}/
cd %{buildroot}
%{__tar} xfz $RPM_SOURCE_DIR/intel-clck%{PROJ_DELIM}-%{version}.tar.gz
# Update key executiables with static versions
#cp %{SOURCE1} %{buildroot}/%{FSP_ADMIN}/clck/%{version}/share/intel64/stream
#cp %{SOURCE2} %{buildroot}/%{FSP_ADMIN}/clck/%{version}/share/intel64/dgemm_mflops
cd -

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog

