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
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

%define debug_package %{nil}

%define pname warewulf-vnfs

Summary: Warewulf VNFS Module
Name:    %{pname}%{PROJ_DELIM}
Version: 3.6
Release: %{_rel}%{?dist}
License: US Dept. of Energy (BSD-like)
Group:   %{PROJ_NAME}/provisioning
URL:     http://warewulf.lbl.gov/
Source:  http://warewulf.lbl.gov/downloads/releases/warewulf-vnfs/warewulf-vnfs-%{version}.tar.gz
ExclusiveOS: linux
Requires: warewulf-common%{PROJ_DELIM}
Requires: pigz
BuildRequires: warewulf-common%{PROJ_DELIM}
Conflicts: warewulf < 3
%if 0%{?sles_version} || 0%{?suse_version}
BuildArch: x86_64
%else
BuildArch: noarch
%endif
BuildRoot: %{?_tmppath}%{!?_tmppath:/var/tmp}/%{pname}-%{version}-%{release}-root
DocDir: %{OHPC_PUB}/doc/contrib
# Previous version had an architecture in its release. This is necessary for
# YUM to properly update a package of a different BuildArch...
Obsoletes: warewulf-vnfs < 3.2-0
# 06/18/14 charles.r.baird@intel.com - wwmkchroot patch for SLES
Patch1: warewulf-vnfs.wwmkchroot.patch
# 09/10/14 charles.r.baird@intel.com - special chars in vnfs filenames
Patch2: warewulf-vnfs.utf8.patch
# 09/19/14 karl.w.schulz@intel.com - include yum/numactl in centos6 default image
Patch3: centos-add-pkgs.patch
# 02/10/15 charles.r.baird@intel.com - add drivers to bootstrap image
Patch4: warewulf-vnfs.bootstrap.patch
# 03/11/15 karl.w.schulz@intel.com - add in centos7 template (culled from ww trunk)
Source1: centos-7.tmpl
# 03/13/15 karl.w.schulz@intel.com - honor local proxy setting if defined (rhel)
Patch5: rhel-proxy.patch
Patch6: warewulf-vnfs.pigz.patch
# 03/30/16 karl.w.schulz@intel.com - add support for ecdsa host keys
Patch7: warewulf-vnfs.ecdsa.patch
# 04/14/16 karl.w.schulz@intel.com - add init class
Patch8: warewulf-vnfs.init.patch


%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the VNFS module which supports the creation and management of
Virtual Node FileSystem objects.


%prep
%setup -n %{pname}-%{version}

# OpenHPC patches

%patch1 -p1
%patch2 -p1
%patch3 -p1
%patch4 -p1
%patch5 -p1
%patch6 -p1
%patch7 -p3


%build
%configure
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

# 03/11/15 karl.w.schulz@intel.com - add in centos7 template (culled from ww trunk)
install -D -m 0644 %SOURCE1 %{buildroot}/%{_libexecdir}/warewulf/wwmkchroot/centos-7.tmpl

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root)
%{OHPC_HOME}
%{OHPC_PUB}
%doc AUTHORS COPYING ChangeLog INSTALL NEWS README TODO LICENSE
%config(noreplace) %{_sysconfdir}/warewulf/vnfs.conf
%config(noreplace) %{_sysconfdir}/warewulf/bootstrap.conf
%{_libexecdir}/warewulf/*
%{_bindir}/*


%changelog
