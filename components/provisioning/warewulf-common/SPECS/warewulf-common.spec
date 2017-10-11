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

%define wwpkgdir /srv/

%include %{_sourcedir}/OHPC_macros

%define pname warewulf-common
%define dname common
%define dev_branch_sha 166bcf8938e8e460fc200b0dfe4b61304c7d010a

Name:    %{pname}%{PROJ_DELIM}
Summary: A suite of tools for clustering
Version: 3.8pre
Release: %{_rel}%{?dist}
License: US Dept. of Energy (BSD-like)
Group:   %{PROJ_NAME}/provisioning
URL:     http://warewulf.lbl.gov/
Source0: https://github.com/crbaird/warewulf3/archive/%{dev_branch_sha}.tar.gz#/warewulf3-%{version}.ohpc1.3.tar.gz
Source1: OHPC_macros
ExclusiveOS: linux
BuildRequires: autoconf
BuildRequires: automake
DocDir: %{OHPC_PUB}/doc/contrib
Conflicts: warewulf <= 2.9
# 06/14/14 karl.w.schulz@intel.com - SUSE does not allow files in /usr/lib64 for noarch package
%if 0%{!?sles_version} && 0%{!?suse_version}
BuildArch: noarch
%endif
BuildRoot: %{?_tmppath}/%{pname}-%{version}-%{release}-root
# 09/10/14 charles.r.baird@intel.com - patch to add SuSE as a system type
Patch1: warewulf-common.system.patch
# 04/01/16 karl.w.schulz@intel.com - patch to enable DB transaction handling from WW trunk
Patch2: mysql.r1978.patch
# 02/22/17 charles.r.baird@intel.com - alternate package names for SuSE
Patch3 : warewulf-common.dbinit.patch
Patch4 : warewulf-common.bin-file.patch
%if 0%{?suse_version}
Requires: mysql perl-DBD-mysql
%else
# 07/23/14 travis.post@intel.com - alternate package names for RHEL7
%if 0%{?rhel_version} > 600 || 0%{?centos_version} > 600
Requires: mariadb-server perl-DBD-MySQL
Requires: perl-Term-ReadLine-Gnu
%else
%if 0%{?rhel_version} < 700 || 0%{?centos_version} < 700
Requires: mysql-server perl-DBD-mysql
BuildRequires: db4-utils
%endif
%endif
%endif

%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the main package which includes the main daemon and
supporting libs.


%prep
%setup -q -n warewulf3-%{dev_branch_sha}
cd %{dname}
%patch1 -p1
%patch2 -p1
%patch3 -p1
%patch4 -p2


%build
cd %{dname}
./autogen.sh
%configure --localstatedir=%{wwpkgdir}
%{__make} %{?mflags}


%install
cd %{dname}
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%pre
groupadd -r warewulf >/dev/null 2>&1 || :


%post
if [ $1 -eq 2 ] ; then
    %{_bindir}/wwsh object canonicalize -t node >/dev/null 2>&1 || :
    %{_bindir}/wwsh object canonicalize -t file >/dev/null 2>&1 || :
fi

systemctl start mariadb >/dev/null 2>&1 || :
systemctl enable mariadb >/dev/null 2>&1 || :


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-, root, root)
%{_sysconfdir}/bash_completion.d/warewulf_completion
%{OHPC_PUB}
%doc %{dname}/AUTHORS %{dname}/COPYING %{dname}/ChangeLog %{dname}/INSTALL %{dname}/NEWS %{dname}/README %{dname}/TODO %{dname}/LICENSE
%attr(0755, root, warewulf) %dir %{_sysconfdir}/warewulf/
%attr(0755, root, warewulf) %dir %{_sysconfdir}/warewulf/defaults/
%attr(0444, root, warewulf) %{_sysconfdir}/warewulf/functions
%attr(0644, root, warewulf) %config(noreplace) %{_sysconfdir}/warewulf/database.conf
%attr(0640, root, warewulf) %config(noreplace) %{_sysconfdir}/warewulf/database-root.conf
%attr(0644, root, warewulf) %config(noreplace) %{_sysconfdir}/warewulf/defaults/node.conf
%{_bindir}/*
%{_datadir}/warewulf/
%{_libexecdir}/warewulf/wwinit
%{_mandir}/*
%{perl_vendorlib}/*

# 06/14/14 karl.w.schulz@intel.com - include required dir for SUSE
%if 0%{?sles_version} || 0%{?suse_version}
%dir %{_libexecdir}/warewulf/
%endif

%changelog
