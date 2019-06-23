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


%define dname vnfs
%define pname warewulf-%{dname}

Summary: Warewulf VNFS Module
Name:    %{pname}%{PROJ_DELIM}
Version: 3.9.0
Release: .1%{?dist}
License: US Dept. of Energy (BSD-like)
Group:   %{PROJ_NAME}/provisioning
URL:     http://warewulf.lbl.gov/
Source0: https://github.com/warewulf/warewulf3/archive/development.tar.gz
Source3: rhel-7.tmpl
# 03/13/15 karl.w.schulz@intel.com - honor local proxy setting if defined (rhel)
Patch1: warewulf-vnfs.rhel-proxy.patch
# 02/23/17 reese.baird@intel.com - default to pigz for vnfs compression
Patch2: warewulf-vnfs.pigz.patch
# 02/23/17 reese.baird@intel.com - fixes unicode in files inserted to vnfs
Patch3: warewulf-vnfs.utf8.patch
# 10/10/17 reese.baird@intel.com - fixes bootstrap kernel name on sles
Patch4: warewulf-vnfs.bootstrap.kernel.patch
# 10/13/17 karl.w.schulz@intel.com - fixes bootstrap kernel format on aarch64 on sles
Patch5: warewulf-vnfs.bootstrap.aarch64.patch
# 10/23/17 reese.baird@intel.com - allows bootstrap with usb netdev
Patch6: warewulf-vnfs.bootstrap_usb.patch
# 10/31/17 reese.baird@intel.com - allow altarch yum mirror
Patch7: warewulf-vnfs.centos_aarch64.patch
# 03/05/18 reese.baird@intel.com - load msr driver for SLES
Patch8: warewulf-vnfs.bootstrap.msr.patch
# 05/14/18 reese.baird@intel.com - create /dev/urandom in chroot for centos7
Patch9: warewulf-vnfs.urandom-chroot.patch
ExclusiveOS: linux
Requires: warewulf-common%{PROJ_DELIM}
Requires: pigz
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: warewulf-common%{PROJ_DELIM}
Conflicts: warewulf < 3.9
# Previous version had an architecture in its release. This is necessary for
# YUM to properly update a package of a different BuildArch...
Obsoletes: warewulf-vnfs < 3.2-0


%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the VNFS module which supports the creation and management of
Virtual Node FileSystem objects.


%prep
%setup -n warewulf3-development
cd %{dname}
%patch1 -p1
%patch2 -p1
%patch3 -p1
%if 0%{!?sles_version} && 0%{!?suse_version}
%patch4 -p1
%else
%ifarch aarch64
%patch5 -p1
%patch6 -p1
%patch7 -p1
%endif
%endif
%patch8 -p1
%patch9 -p1


%build
cd %{dname}
NO_CONFIGURE=1 ./autogen.sh
%configure
%{__make} %{?mflags}


%install
cd %{dname}
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

install -m 755 %{SOURCE3} $RPM_BUILD_ROOT/%{_libexecdir}/warewulf/*

%files
%{OHPC_PUB}
%doc %{dname}/AUTHORS %{dname}/COPYING %{dname}/ChangeLog %{dname}/INSTALL %{dname}/NEWS %{dname}/README %{dname}/TODO %{dname}/LICENSE
%config(noreplace) %{_sysconfdir}/warewulf/vnfs.conf
%config(noreplace) %{_sysconfdir}/warewulf/bootstrap.conf
%{_libexecdir}/warewulf/*
%{_bindir}/*
%{_mandir}/*
