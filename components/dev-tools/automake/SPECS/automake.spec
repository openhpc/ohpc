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
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

%define pname automake

Summary:   A GNU tool for automatically creating Makefiles
Name:      %{pname}%{PROJ_DELIM}
Version:   1.15
Release:   1
License:   GNU GPL
Group:     %{PROJ_NAME}/dev-tools
URL:       http://www.gnu.org/software/automake/
DocDir:    %{OHPC_PUB}/doc/contrib
Source0:   https://ftp.gnu.org/gnu/automake/automake-%{version}.tar.gz
Source1:   OHPC_macros
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root

%define install_path %{OHPC_PUB}/autotools

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

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%dir %{OHPC_PUB}
%{OHPC_PUB}
%doc THANKS
%doc ChangeLog
%doc NEWS
%doc AUTHORS
%doc README
%doc COPYING

%changelog
* Mon Sep 15 2014  <karl.w.schulz@intel.com> - 
- Initial build.

