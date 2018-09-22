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

%define pname warewulf-common
%define dname common
%define wwpkgdir /srv/

Name:    %{pname}%{PROJ_DELIM}
Summary: A suite of tools for clustering
Version: 3.8.1
Release: 1%{?dist}
License: US Dept. of Energy (BSD-like)
Group:   %{PROJ_NAME}/provisioning
URL:     http://warewulf.lbl.gov/
Source0: https://github.com/warewulf/warewulf3/archive/3.8.1.tar.gz#/warewulf3-%{version}.tar.gz
Patch1:  warewulf-common.bin-file.patch
Patch2:  warewulf-common.dbinit.patch
Patch3:  warewulf-common.mysql.r1978.patch
Patch4:  warewulf-common.rhel_service.patch
ExclusiveOS: linux
BuildRequires: autoconf
BuildRequires: automake
Conflicts: warewulf <= 2.9
# 06/14/14 karl.w.schulz@intel.com - SUSE does not allow files in /usr/lib64 for noarch package
%if 0%{!?sles_version} && 0%{!?suse_version}
BuildArch: noarch
%endif
%if 0%{?suse_version}
Requires: mysql perl-DBD-mysql
%else
Requires: mariadb-server perl-DBD-MySQL
Requires: perl-Term-ReadLine-Gnu
%endif

%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the main package which includes the main daemon and
supporting libs.


%prep
%setup -q -n warewulf3-%{version}
cd %{dname}
%patch1 -p1
%patch2 -p1
%patch3 -p1
%patch4 -p1


%build
cd %{dname}
if [ ! -f configure ]; then
    ./autogen.sh
fi
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

%if 0%{?suse_version}
systemctl start mysql >/dev/null 2>&1 || :
systemctl enable mysql >/dev/null 2>&1 || :
%else
systemctl start mariadb >/dev/null 2>&1 || :
systemctl enable mariadb >/dev/null 2>&1 || :
%endif


%files
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
