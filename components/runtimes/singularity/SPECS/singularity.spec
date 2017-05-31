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

# Base package name
%define pname singularity

# This allows us to pick up the default value from the configure
%{!?with_slurm: %global with_slurm no}
%if "%{with_slurm}" == "yes"
%global slurm 1
%else
%global slurm 0
%endif

Summary: Application and environment virtualization
Name: %{pname}%{PROJ_DELIM}
Version: 2.3
Release: 1%{?dist}
# https://spdx.org/licenses/BSD-3-Clause-LBNL.html
License: BSD-3-Clause-LBNL
Group: %{PROJ_NAME}/runtimes
URL: http://singularity.lbl.gov/
Source0: https://github.com/singularityware/singularity/releases/download/%{version}/%{pname}-%{version}.tar.gz
Source1: OHPC_macros
ExclusiveOS: linux
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
BuildRequires: slurm-devel%{PROJ_DELIM}

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

%{__make} %{?_smp_mflags}

%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT
rm $RPM_BUILD_ROOT/%{_libdir}/singularity/*.la
# NO_BRP_CHECK_RPATH has no effect on CentOS 7
export NO_BRP_CHECK_RPATH=true


%post
/sbin/ldconfig || exit 1
%postun -p /sbin/ldconfig


%files
%defattr(-, root, root)
%doc examples AUTHORS.md CONTRIBUTING.md COPYRIGHT.md INSTALL.md LICENSE-LBNL.md LICENSE.md README.md
%attr(0755, root, root) %dir %{_sysconfdir}/singularity
%attr(0644, root, root) %config(noreplace) %{_sysconfdir}/singularity/*
%dir %{_libdir}/singularity
%dir %{_libexecdir}/singularity
%dir %{_libexecdir}/singularity/bin
%dir %{_libexecdir}/singularity/helpers
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
%{_libexecdir}/singularity/helpers/inspect.sh

# Directories
%{_libexecdir}/singularity/bootstrap-scripts
%{_libexecdir}/singularity/cli
%{_libexecdir}/singularity/python



%files -n singularity-devel-ohpc
%defattr(-, root, root)
%dir %{_includedir}/singularity
%dir %{_libdir}/singularity
%{_libdir}/singularity/lib*.so
%{_includedir}/singularity/*.h


%if %slurm
%files -n singularity-slurm-ohpc
%defattr(-, root, root)
%{_libdir}/slurm/singularity.so
%endif

%changelog
