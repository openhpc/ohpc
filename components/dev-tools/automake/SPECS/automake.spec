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

%define pname automake
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary:   A GNU tool for automatically creating Makefiles
Name:      %{pname}%{PROJ_DELIM}
Version:   1.15
Release:   1
License:   GPLv2+ and GFDL
Group:     fsp/dev-tools
URL:       http://www.gnu.org/software/automake/
DocDir:    %{FSP_PUB}/doc/contrib
Source0:   automake-%{version}.tar.gz
Source1:   FSP_macros
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root

%define debug_package %{nil}
%{!?FSP_PUB: %define FSP_PUB /opt/fsp/pub}
%define install_path %{FSP_PUB}/autotools

Requires:      autoconf%{PROJ_DELIM} >= 2.69
BuildRequires: autoconf%{PROJ_DELIM} >= 2.69

%if 0%{?rhel_version} > 600 || 0%{?centos_version} > 600
Requires: perl-Thread-Queue
%endif

%description
Automake is a tool for automatically generating `Makefile.in'
files compliant with the GNU Coding Standards.

You should install Automake if you are developing software and would
like to use its ability to automatically generate GNU standard
Makefiles. If you install Automake, you will also need to install
GNU's Autoconf package.

%prep
%setup -n automake-%{version}

%build
export PATH=%{install_path}/bin:$PATH
./configure --prefix=%{install_path}

%install
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%dir %{FSP_PUB}
%{FSP_PUB}/autotools
%doc THANKS
%doc ChangeLog
%doc NEWS
%doc AUTHORS
%doc README
%doc COPYING

%changelog
* Mon Sep 15 2014  <karl.w.schulz@intel.com> - 
- Initial build.

