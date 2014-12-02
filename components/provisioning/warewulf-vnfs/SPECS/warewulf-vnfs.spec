%{!?_rel:%{expand:%%global _rel 0.r%(test "1686" != "0000" && echo "1686" || svnversion | sed 's/[^0-9].*$//' | grep '^[0-9][0-9]*$' || git svn find-rev `git show -s --pretty=format:%h` || echo 0000)}}
%define debug_package %{nil}

Summary: Warewulf VNFS Module
Name: warewulf-vnfs
Version: 3.6
Release: %{_rel}%{?dist}
License: US Dept. of Energy (BSD-like)
Group: System Environment/Clustering
URL: http://warewulf.lbl.gov/
Source: %{name}-%{version}.tar.gz
ExclusiveOS: linux
Requires: warewulf-common
BuildRequires: warewulf-common
Conflicts: warewulf < 3
%if 0%{?sles_version} || 0%{?suse_version}
BuildArch: x86_64
%else
BuildArch: noarch
%endif
BuildRoot: %{?_tmppath}%{!?_tmppath:/var/tmp}/%{name}-%{version}-%{release}-root
# Previous version had an architecture in its release. This is necessary for
# YUM to properly update a package of a different BuildArch...
Obsoletes: warewulf-vnfs < 3.2-0
# 06/18/14 charles.r.baird@intel.com - wwmkchroot patch for SLES
Patch1: %{name}.wwmkchroot.patch
# 09/10/14 charles.r.baird@intel.com - special chars in vnfs filenames
Patch2: %{name}.utf8.patch
# 09/19/14 karl.w.schulz@intel.com - include yum/numactl in centos6 default image
Patch3: centos-add-pkgs.patch

%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the VNFS module which supports the creation and management of
Virtual Node FileSystem objects.


%prep
%setup

# Intel FSP patches

%patch1 -p1
%patch2 -p1
%patch3 -p1


%build
%configure
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root)
%doc AUTHORS COPYING ChangeLog INSTALL NEWS README TODO LICENSE
%config(noreplace) %{_sysconfdir}/warewulf/vnfs.conf
%config(noreplace) %{_sysconfdir}/warewulf/bootstrap.conf
%{_libexecdir}/warewulf/*
%{_bindir}/*


%changelog
