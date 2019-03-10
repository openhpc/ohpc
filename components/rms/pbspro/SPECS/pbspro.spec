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

%if !%{defined pbs_name}
%define pbs_name pbspro
%endif

%if !%{defined pbs_version}
%define pbs_version 19.1.1
%endif

%if !%{defined pbs_release}
%define pbs_release 0
%endif

%if !%{defined pbs_prefix}
%define pbs_prefix /opt/pbs
%endif

%if !%{defined pbs_home}
%define pbs_home /var/spool/pbs
%endif

%if !%{defined pbs_dbuser}
%define pbs_dbuser postgres
%endif

%define pbs_client client
%define pbs_execution execution
%define pbs_server server
%define pbs_dist %{pbs_name}-%{pbs_version}.tar.gz

%if !%{defined _unitdir}
%define _unitdir /usr/lib/systemd/system
%endif
%if %{_vendor} == debian && %(test -f /etc/os-release && echo 1 || echo 0)
%define _vendor_ver %(cat /etc/os-release | awk -F[=\\".] '/^VERSION_ID=/ {print \$3}')
%define _vendor_id %(cat /etc/os-release | awk -F= '/^ID=/ {print \$2}')
%endif
%if 0%{?suse_version} >= 1210 || 0%{?rhel} >= 7 || (x%{?_vendor_id} == xdebian && 0%{?_vendor_ver} >= 8) || (x%{?_vendor_id} == xubuntu && 0%{?_vendor_ver} >= 16)
%define have_systemd 1
%endif

Name: %{pbs_name}%{PROJ_DELIM}
Version: %{pbs_version}
Release: %{pbs_release}
Source0: https://github.com/PBSPro/pbspro/archive/v%{version}.tar.gz
Summary: PBS Professional
License: AGPLv3 with exceptions
URL: https://github.com/PBSPro/pbspro
Vendor: Altair Engineering, Inc.
Prefix: %{?pbs_prefix}%{!?pbs_prefix:%{_prefix}}

%bcond_with alps
%bcond_with cpuset
%bcond_with ptl

BuildRequires: gcc
BuildRequires: make
BuildRequires: rpm-build
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: libtool
BuildRequires: libtool-ltdl-devel
BuildRequires: hwloc-devel
BuildRequires: libX11-devel
BuildRequires: libXt-devel
BuildRequires: libedit-devel
BuildRequires: libical-devel
BuildRequires: ncurses-devel
BuildRequires: perl
BuildRequires: postgresql-devel >= 9.1
BuildRequires: postgresql-contrib >= 9.1
BuildRequires: python-devel >= 2.6
BuildRequires: python-devel < 3.0
BuildRequires: tcl-devel
BuildRequires: tk-devel
BuildRequires: swig
BuildRequires: zlib-devel
%if %{defined suse_version}
BuildRequires: libexpat-devel
BuildRequires: libopenssl-devel
BuildRequires: libXext-devel
BuildRequires: libXft-devel
BuildRequires: fontconfig
BuildRequires: timezone
BuildRequires: python-xml
%else
BuildRequires: expat-devel
BuildRequires: openssl-devel
BuildRequires: libXext
BuildRequires: libXft
%endif

# Pure python extensions use the 32 bit library path
%{!?py_site_pkg_32: %global py_site_pkg_32 %(%{__python} -c "from distutils.sysconfig import get_python_lib; print get_python_lib(0)")}
%{!?py_site_pkg_64: %global py_site_pkg_64 %(%{__python} -c "from distutils.sysconfig import get_python_lib; print get_python_lib(1)")}

%description
PBS Professional is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.

%package -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
Summary: PBS Professional for a server host
Group:  %{PROJ_NAME}/rms
Conflicts: pbspro-execution-ohpc
Conflicts: pbspro-client-ohpc
Conflicts: pbspro-server
Conflicts: pbspro-execution
Conflicts: pbspro-client
Conflicts: pbs
Conflicts: pbs-mom
Conflicts: pbs-cmds
Requires: bash
Requires: expat
Requires: libedit
Requires: postgresql-server >= 9.1
Requires: postgresql-contrib >= 9.1
Requires: python >= 2.6
Requires: python < 3.0
Requires: tcl
Requires: tk
%if %{defined suse_version}
Requires: smtp_daemon
Requires: net-tools
%else
Requires: smtpdaemon
Requires: hostname
%endif
Requires: libical
Autoreq: 1

%description -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
PBS Professional is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.

This package is intended for a server host. It includes all
PBS Professional components.

%package -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
Summary: PBS Professional for an execution host
Group:   %{PROJ_NAME}/rms
Conflicts: pbspro-server-ohpc
Conflicts: pbspro-client-ohpc
Conflicts: pbspro-server
Conflicts: pbspro-execution
Conflicts: pbspro-client
Conflicts: pbs
Conflicts: pbs-mom
Conflicts: pbs-cmds
Requires: bash
Requires: expat
Requires: python >= 2.6
Requires: python < 3.0
%if %{defined suse_version}
Requires: libhwloc5
Requires: net-tools
%else
Requires: hostname
%endif
%if 0%{?rhel} >= 7
Requires: hwloc-libs
%endif

Autoreq: 1

%description -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
PBS Professional is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.

This package is intended for an execution host. It does not
include the scheduler, server, or communication agent. It
does include the PBS Professional user commands.

%package -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
Summary: PBS Professional for a client host
Group: %{PROJ_NAME}/rms
Conflicts: pbspro-server-ohpc
Conflicts: pbspro-execution-ohpc
Conflicts: pbspro-server
Conflicts: pbspro-execution
Conflicts: pbspro-client
Conflicts: pbs
Conflicts: pbs-mom
Conflicts: pbs-cmds
Requires: bash
Requires: python >= 2.6
Requires: python < 3.0
Autoreq: 1

%description -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
PBS Professional is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.

This package is intended for a client host and provides
the PBS Professional user commands.

%if %{with ptl}

%define pbs_ptl ptl

%if !%{defined ptl_prefix}
%define ptl_prefix %{pbs_prefix}/../ptl
%endif

%package %{pbs_ptl}
Summary: PBS Test Lab for testing PBS Professional
Group: System Environment/Base
Requires: python-nose
Requires: python-beautifulsoup
%if 0%{?rhel} 
Requires: pexpect
%else
Requires: python-pexpect
%endif
Requires: python-defusedxml
Prefix: %{ptl_prefix}

%description %{pbs_ptl}
PBS Test Lab is a test harness and test suite intended to validate the
functionality of PBS Professional.

%endif

%if 0%{?opensuse_bs}
# Do not specify debug_package for OBS builds.
%else
%if %{defined suse_version}
%debug_package
%endif
%endif

%prep
%setup -n %{pbs_name}-%{pbs_version}
%if 0%{?rhel}
%endif

%build
[ -d build ] && rm -rf build
./autogen.sh
mkdir build
cd build
../configure CFLAGS="-fPIC" \
	PBS_VERSION=%{pbs_version} \
	--prefix=%{pbs_prefix} \
%if %{with ptl}
	--enable-ptl \
%endif
%if %{defined suse_version}
	--libexecdir=%{pbs_prefix}/libexec \
%endif
%if %{with alps}
	--enable-alps \
%endif
%if %{with cpuset}
	--enable-cpuset \
%endif
	--with-pbs-server-home=%{pbs_home} \
	--with-database-user=%{pbs_dbuser}
%{__make} %{?_smp_mflags}

%install
cd build
%make_install

%post -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
# do not run pbs_postinstall when the CLE is greater than or equal to 6
imps=0
cle_release_version=0
cle_release_path=/etc/opt/cray/release/cle-release
if [ -f ${cle_release_path} ]; then
	cle_release_version=`grep RELEASE ${cle_release_path} | cut -f2 -d= | cut -f1 -d.`
fi
[ "${cle_release_version}" -ge 6 ] 2>/dev/null && imps=1
if [ $imps -eq 0 ]; then
${RPM_INSTALL_PREFIX:=%{pbs_prefix}}/libexec/pbs_postinstall server \
	%{version} ${RPM_INSTALL_PREFIX:=%{pbs_prefix}} %{pbs_home} \
	%{pbs_dbuser} >/dev/null 2>&1
else
	install -D %{pbs_prefix}/libexec/pbs_init.d /etc/init.d/pbs
fi

%post -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
# do not run pbs_postinstall when the CLE is greater than or equal to 6
imps=0
cle_release_version=0
cle_release_path=/etc/opt/cray/release/cle-release
if [ -f ${cle_release_path} ]; then
	cle_release_version=`grep RELEASE ${cle_release_path} | cut -f2 -d= | cut -f1 -d.`
fi
[ "${cle_release_version}" -ge 6 ] 2>/dev/null && imps=1
if [ $imps -eq 0 ]; then
${RPM_INSTALL_PREFIX:=%{pbs_prefix}}/libexec/pbs_postinstall execution \
	%{version} ${RPM_INSTALL_PREFIX:=%{pbs_prefix}} %{pbs_home} >/dev/null 2>&1
else
	install -D %{pbs_prefix}/libexec/pbs_init.d /etc/init.d/pbs
fi

%post -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
# do not run pbs_postinstall when the CLE is greater than or equal to 6
imps=0
cle_release_version=0
cle_release_path=/etc/opt/cray/release/cle-release
if [ -f ${cle_release_path} ]; then
	cle_release_version=`grep RELEASE ${cle_release_path} | cut -f2 -d= | cut -f1 -d.`
fi
[ "${cle_release_version}" -ge 6 ] 2>/dev/null && imps=1
if [ $imps -eq 0 ]; then
${RPM_INSTALL_PREFIX:=%{pbs_prefix}}/libexec/pbs_postinstall client \
	%{version} ${RPM_INSTALL_PREFIX:=%{pbs_prefix}} >/dev/null 2>&1
else
	install -D %{pbs_prefix}/libexec/pbs_init.d /etc/init.d/pbs
fi

%preun -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
if [ "$1" != "1" ]; then
	# This is an uninstall, not an upgrade.
	[ -x /etc/init.d/pbs ] && /etc/init.d/pbs stop
	[ -x /sbin/chkconfig ] && /sbin/chkconfig --del pbs >/dev/null 2>&1
	rm -f /etc/rc.d/rc?.d/[KS]??pbs
	if [ `basename ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}` = %{version} ]; then
		top_level=`dirname ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}`
		if [ -h $top_level/default ]; then
			link_target=`readlink $top_level/default`
			[ `basename "$link_target"` = %{version} ] && rm -f $top_level/default
		fi
	fi
	rm -f /etc/init.d/pbs
	rm -f /opt/modulefiles/pbs/%{version}
	%if %{defined have_systemd}
		systemctl disable pbs
		rm -f /usr/lib/systemd/system-preset/95-pbs.preset
	%endif
fi

%preun -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
if [ "$1" != "1" ]; then
	# This is an uninstall, not an upgrade.
	[ -x /etc/init.d/pbs ] && /etc/init.d/pbs stop
	[ -x /sbin/chkconfig ] && /sbin/chkconfig --del pbs >/dev/null 2>&1
	rm -f /etc/rc.d/rc?.d/[KS]??pbs
	if [ `basename ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}` = %{version} ]; then
		top_level=`dirname ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}`
		if [ -h $top_level/default ]; then
			link_target=`readlink $top_level/default`
			[ `basename "$link_target"` = %{version} ] && rm -f $top_level/default
		fi
	fi
	rm -f /etc/init.d/pbs
	rm -f /opt/modulefiles/pbs/%{version}
	%if %{defined have_systemd}
		systemctl disable pbs
		rm -f /usr/lib/systemd/system-preset/95-pbs.preset
	%endif
fi

%preun -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
if [ "$1" != "1" ]; then
	# This is an uninstall, not an upgrade.
	if [ `basename ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}` = %{version} ]; then
		top_level=`dirname ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}`
		if [ -h $top_level/default ]; then
			link_target=`readlink $top_level/default`
			[ `basename "$link_target"` = %{version} ] && rm -f $top_level/default
		fi
	fi
	rm -f /opt/modulefiles/pbs/%{version}
fi

%posttrans -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
# The %preun section of 14.x unconditially removes /etc/init.d/pbs
# because it does not check whether the package is being removed
# or upgraded. Make sure it exists here.
if [ -r %{pbs_prefix}/libexec/pbs_init.d ]; then
	install -D %{pbs_prefix}/libexec/pbs_init.d /etc/init.d/pbs
fi

%posttrans -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
# The %preun section of 14.x unconditially removes /etc/init.d/pbs
# because it does not check whether the package is being removed
# or upgraded. Make sure it exists here.
if [ -r %{pbs_prefix}/libexec/pbs_init.d ]; then
	install -D %{pbs_prefix}/libexec/pbs_init.d /etc/init.d/pbs
fi

%files -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
%dir %{pbs_prefix}
%{pbs_prefix}/*
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_rcp
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_iff
%{_sysconfdir}/profile.d/pbs.csh
%{_sysconfdir}/profile.d/pbs.sh
%config(noreplace) %{_sysconfdir}/profile.d/*
%if %{defined have_systemd}
%attr(644, root, root) %{_unitdir}/pbs.service
%else
%exclude %{_unitdir}/pbs.service
%endif
# %{_sysconfdir}/init.d/pbs
%exclude %{pbs_prefix}/unsupported/*.pyc
%exclude %{pbs_prefix}/unsupported/*.pyo

%files -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
%dir %{pbs_prefix}
%{pbs_prefix}/*
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_rcp
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_iff
%{_sysconfdir}/profile.d/pbs.csh
%{_sysconfdir}/profile.d/pbs.sh
%config(noreplace) %{_sysconfdir}/profile.d/*
%if %{defined have_systemd}
%attr(644, root, root) %{_unitdir}/pbs.service
%else
%exclude %{_unitdir}/pbs.service
%endif
%exclude %{pbs_prefix}/bin/printjob_svr.bin
%exclude %{pbs_prefix}/etc/pbs_db_schema.sql
%exclude %{pbs_prefix}/etc/pbs_dedicated
%exclude %{pbs_prefix}/etc/pbs_holidays*
%exclude %{pbs_prefix}/etc/pbs_resource_group
%exclude %{pbs_prefix}/etc/pbs_sched_config
%exclude %{pbs_prefix}/lib*/init.d/sgiICEplacement.sh
%exclude %{pbs_prefix}/lib*/python/altair/pbs_hooks/*
%exclude %{pbs_prefix}/libexec/install_db
%exclude %{pbs_prefix}/sbin/pbs_comm
%exclude %{pbs_prefix}/sbin/pbs_dataservice
%exclude %{pbs_prefix}/sbin/pbs_ds_monitor
%exclude %{pbs_prefix}/sbin/pbs_ds_password
%exclude %{pbs_prefix}/sbin/pbs_ds_password.bin
%exclude %{pbs_prefix}/sbin/pbs_sched
%exclude %{pbs_prefix}/sbin/pbs_server
%exclude %{pbs_prefix}/sbin/pbs_server.bin
%exclude %{pbs_prefix}/sbin/pbsfs
%exclude %{pbs_prefix}/unsupported/*.pyc
%exclude %{pbs_prefix}/unsupported/*.pyo

%files -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
%dir %{pbs_prefix}
%{pbs_prefix}/*
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_iff
%{_sysconfdir}/profile.d/pbs.csh
%{_sysconfdir}/profile.d/pbs.sh
%config(noreplace) %{_sysconfdir}/profile.d/*
%exclude %{pbs_prefix}/bin/mpiexec
%exclude %{pbs_prefix}/bin/pbs_attach
%exclude %{pbs_prefix}/bin/pbs_tmrsh
%exclude %{pbs_prefix}/bin/printjob_svr.bin
%exclude %{pbs_prefix}/etc/pbs_db_schema.sql
%exclude %{pbs_prefix}/etc/pbs_dedicated
%exclude %{pbs_prefix}/etc/pbs_holidays*
%exclude %{pbs_prefix}/etc/pbs_resource_group
%exclude %{pbs_prefix}/etc/pbs_sched_config
%exclude %{pbs_prefix}/include
%exclude %{pbs_prefix}/lib*/MPI
%exclude %{pbs_prefix}/lib*/init.d
%exclude %{pbs_prefix}/lib*/python/altair/pbs_hooks
%exclude %{pbs_prefix}/lib*/python/pbs_bootcheck*
%exclude %{pbs_prefix}/libexec/install_db
%exclude %{pbs_prefix}/libexec/pbs_habitat
%exclude %{pbs_prefix}/libexec/pbs_schema_upgrade
%exclude %{pbs_prefix}/libexec/pbs_init.d
%exclude %{pbs_prefix}/sbin/pbs_comm
%exclude %{pbs_prefix}/sbin/pbs_demux
%exclude %{pbs_prefix}/sbin/pbs_dataservice
%exclude %{pbs_prefix}/sbin/pbs_ds_monitor
%exclude %{pbs_prefix}/sbin/pbs_ds_password
%exclude %{pbs_prefix}/sbin/pbs_ds_password.bin
%exclude %{pbs_prefix}/sbin/pbs_idled
%exclude %{pbs_prefix}/sbin/pbs_mom
%exclude %{pbs_prefix}/sbin/pbs_rcp
%exclude %{pbs_prefix}/sbin/pbs_sched
%exclude %{pbs_prefix}/sbin/pbs_server
%exclude %{pbs_prefix}/sbin/pbs_server.bin
%exclude %{pbs_prefix}/sbin/pbs_upgrade_job
%exclude %{pbs_prefix}/sbin/pbsfs
%exclude %{pbs_prefix}/unsupported/*.pyc
%exclude %{pbs_prefix}/unsupported/*.pyo
%exclude %{_unitdir}/pbs.service

%if %{with ptl}
%files %{pbs_ptl}
%defattr(-,root,root, -)
%dir %{ptl_prefix}
%{ptl_prefix}/*
%{_sysconfdir}/profile.d/ptl.csh
%{_sysconfdir}/profile.d/ptl.sh
%endif
