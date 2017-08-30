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

Summary: Warewulf VNFS Module
Name:    %{pname}%{PROJ_DELIM}
Version: 3.7pre
Release: %{_rel}%{?dist}
License: US Dept. of Energy (BSD-like)
Group:   %{PROJ_NAME}/provisioning
URL:     http://warewulf.lbl.gov/
Source0: https://github.com/crbaird/warewulf3/archive/v%{version}.ohpc1.3.tar.gz#/warewulf3-%{version}.ohpc1.3.tar.gz
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


%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the VNFS module which supports the creation and management of
Virtual Node FileSystem objects.


%prep
%setup -n warewulf3-%{version}.ohpc1.3

# OpenHPC patches
cd %{dname}
%patch1 -p1
%patch2 -p1
%patch3 -p1
%patch4 -p1


%build
cd %{dname}
./autogen.sh
%configure
%{__make} %{?mflags}


%install
cd %{dname}
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

install -m 644 %{SOURCE1} $RPM_BUILD_ROOT/%{_libexecdir}/warewulf/*

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root)
%{OHPC_PUB}
%doc %{dname}/AUTHORS %{dname}/COPYING %{dname}/ChangeLog %{dname}/INSTALL %{dname}/NEWS %{dname}/README %{dname}/TODO %{dname}/LICENSE
%config(noreplace) %{_sysconfdir}/warewulf/vnfs.conf
%config(noreplace) %{_sysconfdir}/warewulf/bootstrap.conf
%{_libexecdir}/warewulf/*
%{_bindir}/*


%changelog
