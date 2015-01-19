%include %{_sourcedir}/FSP_macros
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary:   FSP packaging for Intel CLCK
Name:      intel-clck%{PROJ_DELIM}
Version:   2.2.1
Release:   1
License:   Intel
Group:     Development/Tools
BuildArch: x86_64
Source1:   stream.static
Source2:   dgemm_mflops.static
Source3:   FSP_macros
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq:   no

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
cp %{SOURCE1} %{buildroot}/%{FSP_ADMIN}/clck/2.2/share/intel64/stream
cp %{SOURCE2} %{buildroot}/%{FSP_ADMIN}/clck/2.2/share/intel64/dgemm_mflops
cd -

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog

