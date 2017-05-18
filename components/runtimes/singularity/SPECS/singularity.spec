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

# Base package name
%define pname singularity
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary: Application and environment virtualization
Name: %{pname}%{PROJ_DELIM}
Version: 2.3pre
Release: %{_rel}%{?dist}
# https://spdx.org/licenses/BSD-3-Clause-LBNL.html
License: BSD-3-Clause-LBNL
Group: %{PROJ_NAME}/runtimes
URL: http://singularity.lbl.gov/
#Source: https://github.com/singularityware/singularity/releases/download/%{version}/%{pname}-%{version}.tar.gz
Source: https://github.com/crbaird/singularity/archive/%{version}.tar.gz
#Source1: build-zypper.sh
#Source2: sles.def
#Patch1:  singularity-makefile.patch
ExclusiveOS: linux
BuildRoot: %{?_tmppath}%{!?_tmppath:/var/tmp}/%{pname}-%{version}-%{release}-root
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: libtool
BuildRequires: python

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
#%patch1 -p1


%build
#cp %SOURCE1 libexec/bootstrap/modules-v2/.
#cp %SOURCE2 examples/.
./autogen.sh
%configure --disable-static --with-pic
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}
rm -f $RPM_BUILD_ROOT/%{_libdir}/lib*.la
export NO_BRP_CHECK_RPATH=true


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
%dir %{_libexecdir}/singularity/defaults
%attr(4755, root, root) %{_libexecdir}/singularity/sexec-suid
%{_libexecdir}/singularity/bootstrap
%{_libexecdir}/singularity/cli
%{_libexecdir}/singularity/helpers
%{_libexecdir}/singularity/python
%{_libexecdir}/singularity/get-section
%{_libexecdir}/singularity/image-handler.sh
%{_libexecdir}/singularity/sexec
%{_libexecdir}/singularity/functions
%{_libexecdir}/singularity/simage
%{_libexecdir}/singularity/defaults/*
%{_bindir}/singularity
%{_bindir}/run-singularity
%{_mandir}/man1/*
%{_libdir}/lib*.so
%{_sysconfdir}/bash_completion.d/singularity


%files devel
%defattr(-, root, root)
%{_libdir}/lib*.so
#%{_libdir}/lib*.a
%{_includedir}/*



%changelog

