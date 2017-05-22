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

# This allows us to pick up the default value from the configure
%define with_slurm @with_slurm@
%if "%{with_slurm}" == "yes"
%global slurm 1
%else
%global slurm 0
%endif

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

%package -n singularity-devel%{PROJ_DELIM}
Summary: Development libraries for Singularity

%description -n singularity-devel%{PROJ_DELIM}
Development files for Singularity

%if %slurm
%package -n singularity-slurm%{PROJ_DELIM}
Summary: Singularity plugin for SLURM
Requires: singularity = %{version}-%{release}
BuildRequires: slurm-devel%{proj_delim}

%description -n singularity-slurm%{PROJ_DELIM}
The Singularity plugin for SLURM allows jobs to be started within
a container.  This provides a simpler interface to the user (they
don't have to be aware of the singularity executable) and doesn't
require a setuid binary.
%endif


%prep
%setup -q -n %{pname}-%{version}


%build
if [ ! -f configure ]; then
  ./autogen.sh
fi

%configure --disable-static --with-pic \
%if %slurm
  --with-slurm
%else
  --without-slurm
%endif


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}
rm $RPM_BUILD_ROOT/%{_libdir}/singularity/*.la
export NO_BRP_CHECK_RPATH=true


%clean
rm -rf $RPM_BUILD_ROOT


%post
/sbin/ldconfig || exit 1
%postun -p /sbin/ldconfig


%files
%defattr(-, root, root)
%doc examples AUTHORS.md CONTRIBUTING.md COPYRIGHT.md INSTALL.md LICENSE-LBNL.md LICENSE.md README.md
%attr(0755, root, root) %dir %{_sysconfdir}/singularity
%attr(0644, root, root) %config(noreplace) %{_sysconfdir}/singularity/*
%dir %{_localstatedir}/singularity
%dir %{_localstatedir}/singularity/mnt
%dir %{_localstatedir}/singularity/mnt/session
%dir %{_localstatedir}/singularity/mnt/container
%dir %{_localstatedir}/singularity/mnt/overlay

%{_bindir}/singularity
%{_bindir}/run-singularity
%{_mandir}/man1/*
%{_libdir}/singularity/lib*.so.*
%{_sysconfdir}/bash_completion.d/singularity

#SUID programs
%attr(4755, root, root) %{_libexecdir}/singularity/bin/action-suid
%attr(4755, root, root) %{_libexecdir}/singularity/bin/create-suid
%attr(4755, root, root) %{_libexecdir}/singularity/bin/copy-suid
%attr(4755, root, root) %{_libexecdir}/singularity/bin/expand-suid
%attr(4755, root, root) %{_libexecdir}/singularity/bin/export-suid
%attr(4755, root, root) %{_libexecdir}/singularity/bin/import-suid
%attr(4755, root, root) %{_libexecdir}/singularity/bin/mount-suid

# Binaries
%{_libexecdir}/singularity/bin/action
%{_libexecdir}/singularity/bin/bootstrap
%{_libexecdir}/singularity/bin/copy
%{_libexecdir}/singularity/bin/cleanupd
%{_libexecdir}/singularity/bin/create
%{_libexecdir}/singularity/bin/expand
%{_libexecdir}/singularity/bin/export
%{_libexecdir}/singularity/bin/get-section
%{_libexecdir}/singularity/bin/import
%{_libexecdir}/singularity/bin/mount

# Scripts
%{_libexecdir}/singularity/functions
%{_libexecdir}/singularity/image-handler.sh

# Directories
%{_libexecdir}/singularity/bootstrap-scripts
%{_libexecdir}/singularity/cli
%{_libexecdir}/singularity/python



%files -n singularity-devel-ohpc
%defattr(-, root, root)
%{_libdir}/singularity/lib*.so
#%{_libdir}/singularity/lib*.a
%{_includedir}/singularity/*.h


%if %slurm
%files -n singularity-slurm-ohpc
%defattr(-, root, root)
%{_libdir}/slurm/singularity.so
%endif

%changelog

