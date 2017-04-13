#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%{!?_rel:%{expand:%%global _rel 0.1}}
%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %global PROJ_DELIM -ohpc}

# Base package name
%define pname singularity
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary: Application and environment virtualization
Name: %{pname}%{PROJ_DELIM}
Version: 2.2.1
Release: %{_rel}%{?dist}
# https://spdx.org/licenses/BSD-3-Clause-LBNL.html
License: BSD-3-Clause-LBNL
Group: System Environment/Base
URL: http://singularity.lbl.gov/
Source: https://github.com/singularityware/singularity/releases/download/%{version}/%{pname}-%{version}.tar.gz
ExclusiveOS: linux
BuildRoot: %{?_tmppath}%{!?_tmppath:/var/tmp}/%{pname}-%{version}-%{release}-root

%description
Singularity provides functionality to build the smallest most minimal
possible containers, and running those containers as single application
environments.

%package devel
Summary: Development libraries for Singularity
Group: System Environment/Development

%description devel
Development files for Singularity

%prep
%setup -q -n %{pname}-%{version}


%build
%configure --disable-static --with-pic
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}
rm -f $RPM_BUILD_ROOT/%{_libdir}/lib*.la


%clean
rm -rf $RPM_BUILD_ROOT

%post
/sbin/ldconfig || exit 1
%postun -p /sbin/ldconfig

%files
%defattr(-, root, root)
%doc examples AUTHORS COPYING ChangeLog INSTALL NEWS README.md
%attr(0755, root, root) %dir %{_sysconfdir}/singularity
%attr(0644, root, root) %config(noreplace) %{_sysconfdir}/singularity/*
%dir %{_libexecdir}/singularity
%attr(4755, root, root) %{_libexecdir}/singularity/sexec-suid
%{_libexecdir}/singularity/bootstrap
%{_libexecdir}/singularity/cli
%{_libexecdir}/singularity/helpers
%{_libexecdir}/singularity/python
%{_libexecdir}/singularity/get-section
%{_libexecdir}/singularity/image-handler.sh
%{_libexecdir}/singularity/sexec
%{_libexecdir}/singularity/functions
%{_libexecdir}/singularity/image-bind
%{_libexecdir}/singularity/image-create
%{_libexecdir}/singularity/image-expand
%{_libexecdir}/singularity/image-mount
%{_bindir}/singularity
%{_bindir}/run-singularity
%{_mandir}/man1/*
%{_libdir}/lib*.so.*
%{_sysconfdir}/bash_completion.d/singularity


%files devel
%defattr(-, root, root)
%{_libdir}/lib*.so
#%{_libdir}/lib*.a
%{_includedir}/*.h



%changelog

