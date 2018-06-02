#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%{!?_rel:%{expand:%%global _rel 0.r%(test "1686" != "0000" && echo "1686" || svnversion | sed 's/[^0-9].*$//' | grep '^[0-9][0-9]*$' || git svn find-rev `git show -s --pretty=format:%h` || echo 0000)}}

%include %{_sourcedir}/OHPC_macros

%define debug_package %{nil}

%define pname warewulf-vnfs
%define dname vnfs
%define dev_branch_sha 166bcf8938e8e460fc200b0dfe4b61304c7d010a

Summary: Warewulf VNFS Module
Name:    %{pname}%{PROJ_DELIM}
Version: 3.8pre
Release: %{_rel}%{?dist}
License: US Dept. of Energy (BSD-like)
Group:   %{PROJ_NAME}/provisioning
URL:     http://warewulf.lbl.gov/
Source0: https://github.com/crbaird/warewulf3/archive/%{dev_branch_sha}.tar.gz#/warewulf3-%{version}.ohpc1.3.tar.gz
Source2: OHPC_macros
Source3: rhel-7.tmpl

ExclusiveOS: linux
Requires: warewulf-common%{PROJ_DELIM}
Requires: pigz
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: warewulf-common%{PROJ_DELIM}
Conflicts: warewulf < 3
%if 0%{!?sles_version} && 0%{!?suse_version}
BuildArch: noarch
%endif
BuildRoot: %{?_tmppath}%{!?_tmppath:/var/tmp}/%{pname}-%{version}-%{release}-root
DocDir: %{OHPC_PUB}/doc/contrib
# Previous version had an architecture in its release. This is necessary for
# YUM to properly update a package of a different BuildArch...
Obsoletes: warewulf-vnfs < 3.2-0
# 03/13/15 karl.w.schulz@intel.com - honor local proxy setting if defined (rhel)
Patch1: rhel-proxy.patch
# 02/23/17 reese.baird@intel.com - default to pigz for vnfs compression
Patch2: warewulf-vnfs.pigz.patch
# 02/23/17 reese.baird@intel.com - fixes for zypper in wwmkchroot
Patch3: warewulf-vnfs.wwmkchroot.patch
# 02/23/17 reese.baird@intel.com - fixes unicode in files inserted to vnfs
Patch4: warewulf-vnfs.utf8.patch
# 10/10/17 reese.baird@intel.com - fixes bootstrap kernel name on sles
Patch5: warewulf-vnfs.bootstrap.kernel.patch
# 10/13/17 karl.w.schulz@intel.com - fixes bootstrap kernel format on aarch64 on sles
Patch6: warewulf-vnfs.bootstrap.aarch64.patch
# 10/23/17 reese.baird@intel.com - allows bootstrap with usb netdev
Patch7: warewulf-vnfs.bootstrap_usb.patch
# 10/31/17 reese.baird@intel.com - allow altarch yum mirror
Patch8: warewulf-vnfs.centos_aarch64.patch
# 03/05/18 reese.baird@intel.com - load msr driver for SLES
Patch9: warewulf-vnfs.bootstrap.msr.patch
# 05/14/18 reese.baird@intel.com - create /dev/urandom in chroot for centos7
Patch10: warewulf-vnfs.urandom-chroot.patch
# 05/29/18 christopher.m.cantalupo@intel.com - load msr-safe driver
Patch11: warewulf-vnfs.bootstrap.msr-safe.patch


%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the VNFS module which supports the creation and management of
Virtual Node FileSystem objects.


%prep
%setup -n warewulf3-%{dev_branch_sha}

# OpenHPC patches
cd %{dname}
%patch1 -p1
%patch2 -p1
%patch3 -p1
%patch4 -p1
%if 0%{!?sles_version} && 0%{!?suse_version}
%patch5 -p1
%else
%ifarch aarch64
%patch6 -p1
%patch7 -p1
%patch8 -p1
%endif
%endif
%patch9 -p1
%patch10 -p1
%patch11 -p1


%build
cd %{dname}
if [ ! -f configure ]; then
    ./autogen.sh
fi
%configure
%{__make} %{?mflags}


%install
cd %{dname}
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

install -m 755 %{SOURCE3} $RPM_BUILD_ROOT/%{_libexecdir}/warewulf/*

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root)
%{OHPC_PUB}
%doc %{dname}/AUTHORS %{dname}/COPYING %{dname}/ChangeLog %{dname}/INSTALL %{dname}/NEWS %{dname}/README %{dname}/TODO %{dname}/LICENSE
%config(noreplace) %{_sysconfdir}/warewulf/vnfs.conf
%config(noreplace) %{_sysconfdir}/warewulf/bootstrap.conf
%{_bindir}/*
%{_mandir}/*
%{_libexecdir}/warewulf/*


%changelog
