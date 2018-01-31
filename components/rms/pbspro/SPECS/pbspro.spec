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

#
#  Copyright (C) 1994-2017 Altair Engineering, Inc.
#  For more information, contact Altair at www.altair.com.
#   
#  This file is part of the PBS Professional ("PBS Pro") software.
#  
#  Open Source License Information:
#   
#  PBS Pro is free software. You can redistribute it and/or modify it under the
#  terms of the GNU Affero General Public License as published by the Free 
#  Software Foundation, either version 3 of the License, or (at your option) any 
#  later version.
#   
#  PBS Pro is distributed in the hope that it will be useful, but WITHOUT ANY 
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
#  PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.
#   
#  You should have received a copy of the GNU Affero General Public License along 
#  with this program.  If not, see <http://www.gnu.org/licenses/>.
#   
#  Commercial License Information: 
#  
#  The PBS Pro software is licensed under the terms of the GNU Affero General 
#  Public License agreement ("AGPL"), except where a separate commercial license 
#  agreement for PBS Pro version 14 or later has been executed in writing with Altair.
#   
#  Altair’s dual-license business model allows companies, individuals, and 
#  organizations to create proprietary derivative works of PBS Pro and distribute 
#  them - whether embedded or bundled with other software - under a commercial 
#  license agreement.
#  
#  Use of Altair’s trademarks, including but not limited to "PBS™", 
#  "PBS Professional®", and "PBS Pro™" and Altair’s logos is subject to Altair's 
#  trademark licensing policies.
#

%define pbs_name pbspro
%define pbs_client client
%define pbs_execution execution
%define pbs_server server
%define pbs_version 14.1.2
%define pbs_release 0
%define pbs_prefix /opt/pbs
%define pbs_home /var/spool/pbs
%define pbs_dbuser postgres
%define pbs_dist %{pbs_name}-%{pbs_version}.tar.gz

# Patch1: logic for systemd.patch
%if 0%{?suse_version} >= 1210 || 0%{?rhel} >= 7
%define have_systemd 1
%endif

Summary:   PBS Professional
Name:      %{pbs_name}%{PROJ_DELIM}
Version:   %{pbs_version}
Release:   %{pbs_release}
Source0: https://github.com/PBSPro/pbspro/releases/download/v%{version}/%{pbs_name}-%{version}.tar.gz
Source1:   OHPC_macros
Patch1:    systemd.patch
License:   AGPLv3 with exceptions
URL:       https://github.com/PBSPro/pbspro
Prefix:    %{pbs_prefix}
BuildRoot: %{_tmppath}/%{pbs_name}-%{pbs_version}-%{release}-root
DocDir:    %{OHPC_PUB}/doc/contrib

%bcond_with alps
%bcond_with cpuset
%bcond_with ibm-ib
%bcond_with ibm-hps

BuildRoot: %{buildroot}
BuildRequires: gcc
BuildRequires: make
BuildRequires: rpm-build
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: libtool
BuildRequires: libtool-ltdl-devel

%if %{defined suse_version}
BuildRequires: hwloc-devel < 2
%else
BuildRequires: hwloc-devel
%endif

BuildRequires: libX11-devel
BuildRequires: libXt-devel
BuildRequires: libedit-devel
BuildRequires: libical-devel
BuildRequires: ncurses-devel
BuildRequires: perl
BuildRequires: postgresql-devel
BuildRequires: python-devel >= 2.6
BuildRequires: python-devel < 3.0
BuildRequires: tcl-devel
BuildRequires: tk-devel
BuildRequires: swig
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

# Patch1: logic for systemd.patch
%if %{defined have_systemd}
BuildRequires: systemd
%endif
#!BuildIgnore: post-build-checks

# Pure python extensions use the 32 bit library path
%{!?py_site_pkg_32: %global py_site_pkg_32 %(%{__python} -c "from distutils.sysconfig import get_python_lib; print get_python_lib(0)")}
%{!?py_site_pkg_64: %global py_site_pkg_64 %(%{__python} -c "from distutils.sysconfig import get_python_lib; print get_python_lib(1)")}

%description
PBS Professional® is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.

%package -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
Summary: PBS Professional for a server host
Group:  %{PROJ_NAME}/rms
Conflicts: pbspro-execution
Conflicts: pbspro-client
Conflicts: pbs
Conflicts: pbs-mom
Conflicts: pbs-cmds
Requires: expat
Requires: libedit
Requires: postgresql-server
Requires: python >= 2.6
Requires: python < 3.0
Requires: tcl
Requires: tk
%if %{defined suse_version}
Requires: smtp_daemon
Requires: libical1
%else
Requires: smtpdaemon
Requires: libical
%endif
Autoreq: 1

%description -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
PBS Professional® is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.

This package is intended for a server host. It includes all
PBS Professional components.

%package -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
Summary: PBS Professional for an execution host
Group:   %{PROJ_NAME}/rms
Conflicts: pbspro-server
Conflicts: pbspro-client
Conflicts: pbs
Conflicts: pbs-mom
Conflicts: pbs-cmds
Requires: expat
Requires: python >= 2.6
Requires: python < 3.0
%if %{defined suse_version}
Requires: libhwloc5
%endif
%if 0%{?rhel} >= 7
Requires: hwloc-libs
%endif
Autoreq: 1

%description -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
PBS Professional® is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.

This package is intended for an execution host. It does not
include the scheduler, server, or communication agent. It
does include the PBS Professional user commands.

%package -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
Summary: PBS Professional for a client host
Group: %{PROJ_NAME}/rms
Conflicts: pbspro-server
Conflicts: pbspro-execution
Conflicts: pbs
Conflicts: pbs-mom
Conflicts: pbs-cmds
Requires: python >= 2.6
Requires: python < 3.0
Autoreq: 1

%description -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
PBS Professional® is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.

This package is intended for a client host and provides
the PBS Professional user commands.

%if 0%{?opensuse_bs}
# Do not specify debug_package for OBS builds.
%else
%if %{defined suse_version}
%debug_package
%endif
%endif

%prep
%setup -n %{pbs_name}-%{pbs_version}

# karl.w.schulz@intel.com (enable systemd startup - patches from pbs master branch)
# Patch1: logic for systemd.patch
%patch1 -p1

%build

[ -d build ] && rm -rf build
mkdir build
cd build
../configure CFLAGS="-fPIC" \
	PBS_VERSION=%{pbs_version} \
	--prefix=%{pbs_prefix} \
%if %{defined suse_version}
	--libexecdir=%{pbs_prefix}/libexec \
%endif
%if %{with alps}
	--enable-alps \
%endif
%if %{with cpuset}
	--enable-cpuset \
%endif
%if %{with ibm-hps}
	--enable-hps \
%endif
%if %{with ibm-ib}
	--enable-aixib \
%endif
	--with-pbs-server-home=%{pbs_home} \
	--with-database-user=%{pbs_dbuser}
%{__make} %{?_smp_mflags}

%install
cd build
%make_install

# scott@altair.com (redirecting pbs_postinstall to /dev/null ; reducing verbosity)
%post -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
${RPM_INSTALL_PREFIX:=%{pbs_prefix}}/libexec/pbs_postinstall server \
	%{pbs_version} ${RPM_INSTALL_PREFIX:=%{pbs_prefix}} %{pbs_home} %{pbs_dbuser} >/dev/null 2>&1

%post -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
${RPM_INSTALL_PREFIX:=%{pbs_prefix}}/libexec/pbs_postinstall execution \
	%{pb_version} ${RPM_INSTALL_PREFIX:=%{pbs_prefix}} %{pbs_home} >/dev/null 2>&1

%post -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
${RPM_INSTALL_PREFIX:=%{pbs_prefix}}/libexec/pbs_postinstall client \
	%{pbs_version} ${RPM_INSTALL_PREFIX:=%{pbs_prefix}} >/dev/null 2>&1

%preun -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
if [ -x /sbin/chkconfig -a -x /sbin/service ]; then
	out=`/sbin/chkconfig --list pbs 2>/dev/null`
	if [ $? -eq 0 ]; then
		if [ -x /etc/init.d/pbs ]; then
			/etc/init.d/pbs stop
		else
			/sbin/service pbs stop
		fi
		/sbin/chkconfig --del pbs
	fi
else
	[ -x /etc/init.d/pbs ] && /etc/init.d/pbs stop
	rm -f /etc/rc.d/rc0.d/K10pbs
	rm -f /etc/rc.d/rc1.d/K10pbs
	rm -f /etc/rc.d/rc2.d/K10pbs
	rm -f /etc/rc.d/rc3.d/S90pbs
	rm -f /etc/rc.d/rc4.d/K10pbs
	rm -f /etc/rc.d/rc5.d/S90pbs
	rm -f /etc/rc.d/rc6.d/K10pbs
fi
rm -f ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}/etc/db_user.new
if [ `basename ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}` = %{pbs_version} ]; then
	top_level=`dirname ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}`
	if [ -h $top_level/default ]; then
		link_target=`readlink $top_level/default`
		[ `basename "$link_target"` = %{pbs_version} ] && rm -f $top_level/default
	fi
fi
rm -f /etc/init.d/pbs

%preun -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
if [ -x /sbin/chkconfig -a -x /sbin/service ]; then
	out=`/sbin/chkconfig --list pbs 2>/dev/null`
	if [ $? -eq 0 ]; then
		if [ -x /etc/init.d/pbs ]; then
			/etc/init.d/pbs stop
		else
			/sbin/service pbs stop
		fi
		/sbin/chkconfig --del pbs
	fi
else
	[ -x /etc/init.d/pbs ] && /etc/init.d/pbs stop
	rm -f /etc/rc.d/rc0.d/K10pbs
	rm -f /etc/rc.d/rc1.d/K10pbs
	rm -f /etc/rc.d/rc2.d/K10pbs
	rm -f /etc/rc.d/rc3.d/S90pbs
	rm -f /etc/rc.d/rc4.d/K10pbs
	rm -f /etc/rc.d/rc5.d/S90pbs
	rm -f /etc/rc.d/rc6.d/K10pbs
fi
if [ `basename ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}` = %{pbs_version} ]; then
	top_level=`dirname ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}`
	if [ -h $top_level/default ]; then
		link_target=`readlink $top_level/default`
		[ `basename "$link_target"` = %{pbs_version} ] && rm -f $top_level/default
	fi
fi
rm -f /etc/init.d/pbs

%preun -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
if [ `basename ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}` = %{pbs_version} ]; then
	top_level=`dirname ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}`
	if [ -h $top_level/default ]; then
		link_target=`readlink $top_level/default`
		[ `basename "$link_target"` = %{pbs_version} ] && rm -f $top_level/default
	fi
fi

# scott@altair.com (commenting out %postun; reducing verbosity)
#%postun -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
#echo
#echo "NOTE: /etc/pbs.conf and the PBS_HOME directory must be deleted manually"
#echo

#%postun -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
#echo
#echo "NOTE: /etc/pbs.conf and the PBS_HOME directory must be deleted manually"
#echo

#%postun -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
#echo
#echo "NOTE: /etc/pbs.conf must be deleted manually"
#echo

%files -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
%defattr(-,root,root, -)
%dir %{pbs_prefix}
%{pbs_prefix}/*
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_rcp
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_iff
%{_sysconfdir}/profile.d/pbs.csh
%{_sysconfdir}/profile.d/pbs.sh

# Patch1: logic for systemd.patch
%if %{defined have_systemd}
%attr(644, root, root) %{_unitdir}/pbs.service
%endif

# %{_sysconfdir}/init.d/pbs
%exclude %{pbs_prefix}/unsupported/*.pyc
%exclude %{pbs_prefix}/unsupported/*.pyo

%files -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
%defattr(-,root,root, -)
%dir %{pbs_prefix}
%{pbs_prefix}/*
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_rcp
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_iff
%{_sysconfdir}/profile.d/pbs.csh
%{_sysconfdir}/profile.d/pbs.sh

# Patch1: logic for systemd.patch
%if %{defined have_systemd}
%attr(644, root, root) %{_unitdir}/pbs.service
%endif

# %{_sysconfdir}/init.d/pbs
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
%defattr(-,root,root, -)
%dir %{pbs_prefix}
%{pbs_prefix}/*
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_iff
%{_sysconfdir}/profile.d/pbs.csh
%{_sysconfdir}/profile.d/pbs.sh
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

# Patch1: logic for systemd.patch
%if %{defined have_systemd}
%exclude %{_unitdir}/pbs.service
%endif
