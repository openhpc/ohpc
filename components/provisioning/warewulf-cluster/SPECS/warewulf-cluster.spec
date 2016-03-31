#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%{!?_rel:%{expand:%%global _rel 0.r%(test "1547" != "0000" && echo "1547" || svnversion | sed 's/[^0-9].*$//' | grep '^[0-9][0-9]*$' || echo 0000)}}

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

%define pname warewulf-cluster

Name:    %{pname}%{PROJ_DELIM}
Summary: Tools used for clustering with Warewulf
Version: 3.6
Release: %{_rel}
License: US Dept. of Energy (BSD-like)
Group:   %{PROJ_NAME}/provisioning
URL:     http://warewulf.lbl.gov/
Source0: http://warewulf.lbl.gov/downloads/releases/warewulf-cluster/warewulf-cluster-%{version}.tar.gz
Source1: OHPC_macros
ExclusiveOS: linux
Requires: warewulf-common%{PROJ_DELIM} warewulf-provision%{PROJ_DELIM} ntp
BuildRequires: warewulf-common%{PROJ_DELIM}
Conflicts: warewulf < 3
BuildRoot: %_tmppath}%{pname}-%{version}-%{release}-root
DocDir: %{OHPC_PUB}/doc/contrib
#%if 0%{?rhel_version} < 700 || 0%{?centos_version} < 700
#%if ! 0%{?suse_version}
#BuildRequires: libdb4-utils
#%endif
#%endif

%define debug_package %{nil}

# 06/13/14 charles.r.baird@intel.com - wwinit patch for SLES
Patch1: warewulf-cluster.wwinit.patch
# 03/30/16 karl.w.schulz@intel.com - add support for ecdsa host keys
Patch2: warewulf-cluster.ecdsa.patch
# 06/14/14 karl.w.schulz@intel.com - OpenHPC flag used to disable inclusion of node package
%if %{OHPC_BUILD}
%define disable_node_package 1
%endif

# 07/21/14 karl.w.schulz@intel.com - explicitly document libcom32 and libutil as being provided
provides: libcom32.c32
provides: libutil.c32



%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This package contains tools to facilitate management of a Cluster
with Warewulf.

# 06/14/14 karl.w.schulz@intel.com - disable warewulf-cluster-node package
%if %{disable_node_package}
%package -n %{pname}-node%{PROJ_DELIM}
Summary: Tools used for clustering with Warewulf
Group: %{PROJ_NAME}/provisioning
Requires: /sbin/sfdisk
%if 0%{?sles_version} || 0%{?suse_version}
PreReq: %{insserv_prereq} %{fillup_prereq}
%endif

%description -n %{pname}-node%{PROJ_DELIM}
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the cluster-node module, that is installed onto the
provisioned nodes.
%endif

%prep
%setup -n %{pname}-%{version}

%patch1 -p1
%patch2 -p0

%build
%configure
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}
cp -r $RPM_BUILD_ROOT/etc/warewulf/vnfs/include/* $RPM_BUILD_ROOT
rm -rf $RPM_BUILD_ROOT/etc/warewulf/vnfs
rmdir $RPM_BUILD_ROOT/etc/warewulf >/dev/null 2>&1 || :

%if 0%{?suse_version}
mv $RPM_BUILD_ROOT/etc/rc.d/init.d  $RPM_BUILD_ROOT/etc/init.d
%endif

# 06/14/14 karl.w.schulz@intel.com - disable warewulf-cluster-node package
%if !%{disable_node_package}
rm -rf $RPM_BUILD_ROOT/etc/sysconfig/wwfirstboot.conf
rm -rf $RPM_BUILD_ROOT/etc/rc.d/init.d/wwfirstboot
rm -rf $RPM_BUILD_ROOT/%{_libexecdir}/warewulf/wwfirstboot/*
%endif

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-, root, root)
%{OHPC_HOME}
%{OHPC_PUB}
%doc AUTHORS COPYING ChangeLog INSTALL LICENSE NEWS README README.node TODO
%{_sysconfdir}/profile.d/*
%{_bindir}/*
%{_libexecdir}/warewulf/wwinit/*
%{perl_vendorlib}/Warewulf/Module/Cli/*

# 06/14/14 karl.w.schulz@intel.com - disable warewulf-cluster-node package
%if %{disable_node_package}

%files -n %{pname}-node%{PROJ_DELIM}
%defattr(-, root, root)
%config(noreplace) %{_sysconfdir}/sysconfig/wwfirstboot.conf
%if 0%{?suse_version}
%dir %{_libexecdir}/warewulf/wwfirstboot
%{_sysconfdir}/init.d/wwfirstboot
%else
%{_sysconfdir}/rc.d/init.d/wwfirstboot
%endif
%defattr(0755, root, root)
%{_libexecdir}/warewulf/wwfirstboot/*


%post -n %{pname}-node%{PROJ_DELIM}
%if 0%{?suse_version}
%{fillup_and_insserv -f}
%else
if [ $1 = 1 ]; then
   [ -x /sbin/chkconfig ] && /sbin/chkconfig --add wwfirstboot
fi
%endif

%endif


%changelog
