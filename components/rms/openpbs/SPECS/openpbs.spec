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
%define pbs_name openpbs
%endif

%if !%{defined pbs_version}
%define pbs_version 22.05.11
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
%define pbs_devel devel
%define pbs_dist %{pbs_name}-%{pbs_version}.tar.gz

%if !%{defined _unitdir}
%define _unitdir /usr/lib/systemd/system
%endif
%if "%{_vendor}" == "debian" && %(test -f /etc/os-release && echo 1 || echo 0)
%define _vendor_ver %(cat /etc/os-release | awk -F[=\\".] '/^VERSION_ID=/ {print \$3}')
%define _vendor_id %(cat /etc/os-release | awk -F= '/^ID=/ {print \$2}')
%endif
%if 0%{?suse_version} >= 1210 || 0%{?rhel} >= 7 || ("x%{?_vendor_id}" == "xdebian" && 0%{?_vendor_ver} >= 8) || ("x%{?_vendor_id}" == "xubuntu" && 0%{?_vendor_ver} >= 16)
%define have_systemd 1
%endif

Name: %{pbs_name}%{PROJ_DELIM}
Version: %{pbs_version}
Release: %{pbs_release}
Source0: https://github.com/openpbs/openpbs/archive/v%{version}.tar.gz
Summary: OpenPBS
License: AGPLv3 with exceptions
URL: http://www.openpbs.org
Vendor: Altair Engineering, Inc.
Prefix: %{?pbs_prefix}%{!?pbs_prefix:%{_prefix}}

Patch1: hwloc.patch

%bcond_with alps
%bcond_with ptl

BuildRequires: gcc gcc-c++
BuildRequires: make
BuildRequires: rpm-build
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: libtool
BuildRequires: libtool-ltdl-devel
BuildRequires: hwloc-ohpc
BuildRequires: libX11-devel
BuildRequires: libXt-devel
BuildRequires: libedit-devel
BuildRequires: libical-devel
BuildRequires: ncurses-devel
BuildRequires: perl
BuildRequires: postgresql-devel >= 9.1
BuildRequires: postgresql-contrib >= 9.1
BuildRequires: python3-devel >= 3.5
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
%else
BuildRequires: expat-devel
BuildRequires: openssl-devel
BuildRequires: libXext
BuildRequires: libXft
%endif

%description
OpenPBS is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.

%package -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
Summary: OpenPBS for a server host
Group:  %{PROJ_NAME}/rms
Conflicts: openpbs-execution%{PROJ_DELIM}
Conflicts: openpbs-client%{PROJ_DELIM}
Conflicts: openpbs-server
Conflicts: openpbs-execution
Conflicts: openpbs-client
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
Requires: python3 >= 3.5
Requires: tcl
Requires: tk
%if %{defined suse_version}
Requires: smtp_daemon
Requires: net-tools
# The pbs_postinstall is not 100% systemd and still needs
# sysv files and directories.
Requires: insserv-compat
%else
Requires: smtpdaemon
Requires: hostname
%endif
%if 0%{?rhel} >= 7
Requires: hwloc-ohpc
Requires: chkconfig
%endif
Requires: libical
Autoreq: 1

%description -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
OpenPBS is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.

This package is intended for a server host. It includes all
PBS components.

%package -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
Summary: OpenPBS for an execution host
Group:   %{PROJ_NAME}/rms
Conflicts: openpbs-server%{PROJ_DELIM}
Conflicts: openpbs-client%{PROJ_DELIM}
Conflicts: openpbs-server
Conflicts: openpbs-execution
Conflicts: openpbs-client
Conflicts: pbspro-server
Conflicts: pbspro-execution
Conflicts: pbspro-client
Conflicts: pbs
Conflicts: pbs-mom
Conflicts: pbs-cmds
Requires: bash
Requires: expat
Requires: python3 >= 3.5
%if %{defined suse_version}
Requires: net-tools
# The pbs_postinstall is not 100% systemd and still needs
# sysv files and directories.
Requires: insserv-compat
%else
Requires: hostname
%endif
%if 0%{?rhel} >= 7
Requires: hwloc-ohpc
Requires: chkconfig
%endif
Autoreq: 1
# Open MPI needs pmix installed on the compute nodes, but the
# package is not actually installed on the compute nodes.
# As slurm pulls in pmix for Open MPI in slurm deployments,
# let's have openpbs pull pmix for openpbs deployments.
Requires: pmix%{PROJ_DELIM}

%description -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
OpenPBS is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.

This package is intended for an execution host. It does not
include the scheduler, server, or communication agent. It
does include the PBS user commands.

%package -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
Summary: OpenPBS for a client host
Group: %{PROJ_NAME}/rms
Conflicts: openpbs-server%{PROJ_DELIM}
Conflicts: openpbs-execution%{PROJ_DELIM}
Conflicts: openpbs-server
Conflicts: openpbs-execution
Conflicts: openpbs-client
Conflicts: pbspro-server
Conflicts: pbspro-execution
Conflicts: pbspro-client
Conflicts: pbs
Conflicts: pbs-mom
Conflicts: pbs-cmds
Requires: bash
Requires: python3 >= 3.5
Autoreq: 1

%description -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
OpenPBS is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.

This package is intended for a client host and provides
the PBS user commands.

%package -n %{pbs_name}-%{pbs_devel}%{PROJ_DELIM}
Summary: OpenPBS Development Package
Group: Development/System
Conflicts: openpbs-devel
Conflicts: pbspro-devel
Requires: zlib-devel

%description -n %{pbs_name}-%{pbs_devel}%{PROJ_DELIM}
OpenPBS is a fast, powerful workload manager and
job scheduler designed to improve productivity, optimize
utilization & efficiency, and simplify administration for
HPC clusters, clouds and supercomputers.


%if %{with ptl}

%define pbs_ptl ptl

%if !%{defined ptl_prefix}
%define ptl_prefix %{pbs_prefix}/../ptl
%endif

%package -n %{pbs_name}-%{pbs_ptl}%{PROJ_DELIM}
Summary: Testing framework for PBS
Group: %{PROJ_NAME}/rms
Prefix: %{ptl_prefix}
Conflicts: pbspro-ptl

%description -n %{pbs_name}-%{pbs_ptl}%{PROJ_DELIM}
PBS Test Lab is a testing framework intended to test and validate the
functionality of PBS.

%endif

%if 0%{?opensuse_bs}
# Do not specify debug_package for OBS builds.
%else
%if 0%{?suse_version} || "x%{?_vendor_id}" == "xdebian" || "x%{?_vendor_id}" == "xubuntu"
%debug_package
%endif
%endif

%prep
%autosetup -n %{pbs_name}-%{pbs_version} -p1

%build
[ -f configure ] || ./autogen.sh
[ -d build ] && rm -rf build
mkdir build
cd build
../configure CFLAGS="-fPIC" \
	PBS_VERSION=%{pbs_version} \
	--prefix=%{pbs_prefix} \
        --with-hwloc=%{OHPC_LIBS}/hwloc \
%if %{with ptl}
	--enable-ptl \
%endif
%if %{defined suse_version}
	--libexecdir=%{pbs_prefix}/libexec \
%endif
%if %{with alps}
	--enable-alps \
%endif
	--with-pbs-server-home=%{pbs_home} \
	--with-database-user=%{pbs_dbuser}
%{__make} %{?_smp_mflags}

%install
cd build
%make_install
mandir=$(find %{buildroot} -type d -name man)
[ -d "$mandir" ] && find $mandir -type f -exec gzip -9 -n {} \;
install -D %{buildroot}/%{pbs_prefix}/libexec/pbs_init.d %{buildroot}/etc/init.d/pbs

%post -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
ldconfig %{_libdir}
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
	%{version} ${RPM_INSTALL_PREFIX:=%{pbs_prefix}} %{pbs_home} %{pbs_dbuser}
fi

%post -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
ldconfig %{_libdir}
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
	%{version} ${RPM_INSTALL_PREFIX:=%{pbs_prefix}} %{pbs_home}
fi

%post -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
ldconfig %{_libdir}
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
	%{version} ${RPM_INSTALL_PREFIX:=%{pbs_prefix}}
fi

%post -n %{pbs_name}-%{pbs_devel}%{PROJ_DELIM}
ldconfig %{_libdir}

%preun -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
if [ "$1" != "1" ]; then
	# This is an uninstall, not an upgrade.
	${RPM_INSTALL_PREFIX:=%{pbs_prefix}}/libexec/pbs_preuninstall server \
		%{version} ${RPM_INSTALL_PREFIX:=%{pbs_prefix}} %{defined have_systemd}
fi

%preun -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
if [ "$1" != "1" ]; then
	# This is an uninstall, not an upgrade.
	${RPM_INSTALL_PREFIX:=%{pbs_prefix}}/libexec/pbs_preuninstall execution \
		%{version} ${RPM_INSTALL_PREFIX:=%{pbs_prefix}} %{defined have_systemd}
fi

%preun -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
if [ "$1" != "1" ]; then
	# This is an uninstall, not an upgrade.
	${RPM_INSTALL_PREFIX:=%{pbs_prefix}}/libexec/pbs_preuninstall client \
		%{version} ${RPM_INSTALL_PREFIX:=%{pbs_prefix}} %{defined have_systemd}
fi

%postun -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
if [ "$1" != "1" ]; then
	# This is an uninstall, not an upgrade.
	ldconfig %{_libdir}
	echo
	echo "NOTE: /etc/pbs.conf and the PBS_HOME directory must be deleted manually"
	echo
fi

%postun -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
if [ "$1" != "1" ]; then
	# This is an uninstall, not an upgrade.
	ldconfig %{_libdir}
	echo
	echo "NOTE: /etc/pbs.conf and the PBS_HOME directory must be deleted manually"
	echo
fi

%postun -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
if [ "$1" != "1" ]; then
	# This is an uninstall, not an upgrade.
	ldconfig %{_libdir}
	echo
	echo "NOTE: /etc/pbs.conf must be deleted manually"
	echo
fi

%postun -n %{pbs_name}-%{pbs_devel}%{PROJ_DELIM}
ldconfig %{_libdir}

%posttrans -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
${RPM_INSTALL_PREFIX:=%{pbs_prefix}}/libexec/pbs_posttrans \
	${RPM_INSTALL_PREFIX:=%{pbs_prefix}}

%posttrans -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
${RPM_INSTALL_PREFIX:=%{pbs_prefix}}/libexec/pbs_posttrans \
	${RPM_INSTALL_PREFIX:=%{pbs_prefix}}

%files -n %{pbs_name}-%{pbs_server}%{PROJ_DELIM}
%defattr(-,root,root, -)
%dir %{pbs_prefix}
%{pbs_prefix}/*
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_rcp
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_iff
%attr(644, root, root) %{pbs_prefix}/lib*/libpbs.la
%{_sysconfdir}/profile.d/pbs.csh
%{_sysconfdir}/profile.d/pbs.sh
%config(noreplace) %{_sysconfdir}/profile.d/pbs.*
%exclude %{_sysconfdir}/profile.d/ptl.csh
%exclude %{_sysconfdir}/profile.d/ptl.sh
%if %{defined have_systemd}
%attr(644, root, root) %{_unitdir}/pbs.service
%else
%exclude %{_unitdir}/pbs.service
%endif
%exclude %{pbs_prefix}/unsupported/fw
%exclude %{pbs_prefix}/unsupported/*.pyc
%exclude %{pbs_prefix}/unsupported/*.pyo
%exclude %{pbs_prefix}/lib*/*.a
%exclude %{pbs_prefix}/include/*
%doc README.md
%license LICENSE

%files -n %{pbs_name}-%{pbs_execution}%{PROJ_DELIM}
%defattr(-,root,root, -)
%dir %{pbs_prefix}
%{pbs_prefix}/*
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_rcp
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_iff
%attr(644, root, root) %{pbs_prefix}/lib*/libpbs.la
%{_sysconfdir}/profile.d/pbs.csh
%{_sysconfdir}/profile.d/pbs.sh
%config(noreplace) %{_sysconfdir}/profile.d/pbs.*
%exclude %{_sysconfdir}/profile.d/ptl.csh
%exclude %{_sysconfdir}/profile.d/ptl.sh
%if %{defined have_systemd}
%attr(644, root, root) %{_unitdir}/pbs.service
%else
%exclude %{_unitdir}/pbs.service
%endif
%exclude %{pbs_prefix}/bin/printjob_svr.bin
%exclude %{pbs_prefix}/etc/pbs_db_schema.sql
%exclude %{pbs_prefix}/libexec/pbs_schema_upgrade
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
%exclude %{pbs_prefix}/unsupported/fw
%exclude %{pbs_prefix}/unsupported/*.pyc
%exclude %{pbs_prefix}/unsupported/*.pyo
%exclude %{pbs_prefix}/lib*/*.a
%exclude %{pbs_prefix}/include/*
%doc README.md
%license LICENSE

%files -n %{pbs_name}-%{pbs_client}%{PROJ_DELIM}
%defattr(-,root,root, -)
%dir %{pbs_prefix}
%{pbs_prefix}/*
%attr(4755, root, root) %{pbs_prefix}/sbin/pbs_iff
%attr(644, root, root) %{pbs_prefix}/lib*/libpbs.la
%{_sysconfdir}/profile.d/pbs.csh
%{_sysconfdir}/profile.d/pbs.sh
%config(noreplace) %{_sysconfdir}/profile.d/pbs.*
%exclude %{_sysconfdir}/profile.d/ptl.csh
%exclude %{_sysconfdir}/profile.d/ptl.sh
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
%exclude %{pbs_prefix}/unsupported/fw
%exclude %{pbs_prefix}/unsupported/*.pyc
%exclude %{pbs_prefix}/unsupported/*.pyo
%exclude %{_unitdir}/pbs.service
%exclude %{pbs_prefix}/lib*/*.a
%exclude %{pbs_prefix}/include/*
%exclude /etc/init.d/pbs
%doc README.md
%license LICENSE

%files -n %{pbs_name}-%{pbs_devel}%{PROJ_DELIM}
%defattr(-,root,root, -)
%{pbs_prefix}/lib*/*.a
%{pbs_prefix}/include/*

%if %{with ptl}
%files %{pbs_ptl}
%defattr(-,root,root, -)
%dir %{ptl_prefix}
%{ptl_prefix}/*
%{_sysconfdir}/profile.d/ptl.csh
%{_sysconfdir}/profile.d/ptl.sh
%config(noreplace) %{_sysconfdir}/profile.d/ptl.*

%post %{pbs_ptl}
installed_pkg="$(pip3 list)"
IFS=$'\n' required_pkg=($(cat %{ptl_prefix}/fw/requirements.txt))
for i in "${required_pkg[@]}"; do
	if [[ "$installed_pkg" =~ "$i" ]]; then
		continue
	else
		pip3 install --trusted-host pypi.org --trusted-host files.pythonhosted.org "$i"
		if [ $? -eq 0 ]; then
			echo "$i installed successfully"
		else
			echo "Failed to install third-party package $i required by PTL"
		fi
	fi
done

%preun %{pbs_ptl}
installed_pkg="$(pip3 list)"
IFS=$'\n' required_pkg=($(cat %{ptl_prefix}/fw/requirements.txt))
for i in "${required_pkg[@]}"; do
	if [[ "$installed_pkg" =~ "$i" ]]; then
		pip3 uninstall --yes "$i"
	else
		continue
	fi
done
%endif

