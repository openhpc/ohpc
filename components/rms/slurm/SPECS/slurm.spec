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
%global _with_mysql  1
%global _with_pmix --with-pmix=%{OHPC_ADMIN}/pmix
%global _with_hwloc 1
%global _with_numa 1

%define pname slurm
%if 0%{?rhel} > 7
# this removes '-Wl,-z,now' from ldflags
# slurm plugins are not working without this on RHEL 8
%undefine _hardened_build
%endif

# $Id$
#
Name:		%{pname}%{PROJ_DELIM}
Version:	18.08.8
%global rel	1
Release:	%{rel}%{?dist}
Summary:	Slurm Workload Manager

Group:		%{PROJ_NAME}/rms
License:	GPLv2+
URL:		https://slurm.schedmd.com/

# when the rel number is one, the directory name does not include it
%if "%{rel}" == "1"
%global slurm_source_dir %{pname}-%{version}
%else
%global slurm_source_dir %{pname}-%{version}-%{rel}
%endif

Source0:	https://download.schedmd.com/slurm/%{slurm_source_dir}.tar.bz2
Source1:	slurm.epilog.clean

# build options		.rpmmacros options	change to default action
# ====================  ====================	========================
# --prefix		%_prefix path		install path for commands, libraries, etc.
# --with cray		%_with_cray 1		build for a Native-Slurm Cray system
# --with cray_network	%_with_cray_network 1	build for a non-Cray system with a Cray network
# --without debug	%_without_debug 1	don't compile with debugging symbols
# --with hdf5		%_with_hdf5 path	require hdf5 support
# --with hwloc		%_with_hwloc 1		require hwloc support
# --with lua		%_with_lua path		build Slurm lua bindings
# --with mysql		%_with_mysql 1		require mysql/mariadb support
# --with numa		%_with_numa 1		require NUMA support
# --with openssl	%_with_openssl 1	require openssl RPM to be installed
#						ensures auth/openssl and crypto/openssl are built
# --without pam		%_without_pam 1		don't require pam-devel RPM to be installed
# --without x11         %_without_x11 1         disable internal X11 support

#  Options that are off by default (enable with --with <opt>)
%bcond_with cray
%bcond_with cray_network
%bcond_with multiple_slurmd

# These options are only here to force there to be these on the build.
# If they are not set they will still be compiled if the packages exist.
%bcond_with hwloc
%bcond_with mysql
%bcond_with hdf5
%bcond_with numa
%bcond_with x11

# Build with OpenSSL by default on all platforms (disable using --without openssl)
%bcond_without openssl

# 4/11/18 karl@ices.utexas.edu - enable lua bindings
%bcond_without lua

# Use debug by default on all systems
%bcond_without debug

# Build with PAM by default on linux
%bcond_without pam


%{?systemd_requires}
BuildRequires: systemd
%if 0%{?rhel} <= 7 || 0%{?suse_version}
Requires: munge%{PROJ_DELIM}
BuildRequires: libssh2-devel
BuildRequires: munge-devel%{PROJ_DELIM} munge-libs%{PROJ_DELIM}
%else
Requires: munge
BuildRequires: munge-devel munge-libs
%endif
%if 0%{?rhel} > 7
BuildRequires: python2
%else
BuildRequires: python
%endif
BuildRequires: readline-devel
Obsoletes: slurm-lua%{PROJ_DELIM} slurm-munge%{PROJ_DELIM} slurm-plugins%{PROJ_DELIM}

# 8/15/14 karl.w.schulz@intel.com - include prereq
%if 0%{?suse_version}
PreReq: %{insserv_prereq} %{fillup_prereq}
%endif
BuildRequires: pmix%{PROJ_DELIM}
Requires: pmix%{PROJ_DELIM}
#!BuildIgnore: post-build-checks

# fake systemd support when building rpms on other platforms
%{!?_unitdir: %global _unitdir /lib/systemd/systemd}

%if %{with openssl}
%if 0%{?suse_version} 
BuildRequires: libopenssl-devel openssl
%else
BuildRequires: openssl-devel >= 0.9.6 openssl >= 0.9.6
%endif
%endif

%if %{with mysql}
%if 0%{?suse_version}
BuildRequires: libmysqlclient-devel
%else
BuildRequires: mariadb-devel >= 5.0.0
%endif
%endif

%if %{with cray}
BuildRequires: cray-libalpscomm_cn-devel
BuildRequires: cray-libalpscomm_sn-devel
BuildRequires: libnuma-devel
BuildRequires: libhwloc-devel
BuildRequires: cray-libjob-devel
BuildRequires: gtk2-devel
BuildRequires: glib2-devel
BuildRequires: pkg-config
%endif

%if %{with cray_network}
BuildRequires: mariadb-devel
BuildRequires: cray-libalpscomm_cn-devel
BuildRequires: cray-libalpscomm_sn-devel
BuildRequires: hwloc-devel
BuildRequires: gtk2-devel
BuildRequires: glib2-devel
%if 0%{?suse_version}
BuildRequires:  pkg-config
%else
BuildRequires: pkgconfig
%endif
%endif

BuildRequires: perl(ExtUtils::MakeMaker)

#needed to enable jobcomp_elasticsearch plugin
BuildRequires: libcurl-devel

%if %{with lua}
%if %{defined suse_version}
BuildRequires: lua51-devel
%else
BuildRequires: lua-devel
%endif
%endif

%if %{with hwloc}
BuildRequires: hwloc-devel
%endif

%if %{with numa}
%if %{defined suse_version}
BuildRequires: libnuma-devel
%else
BuildRequires: numactl-devel
%endif
%endif

#  Allow override of sysconfdir via _slurm_sysconfdir.
#  Note 'global' instead of 'define' needed here to work around apparent
#   bug in rpm macro scoping (or something...)
%{!?_slurm_sysconfdir: %global _slurm_sysconfdir /etc/slurm}
%define _sysconfdir %_slurm_sysconfdir

#  Allow override of datadir via _slurm_datadir.
%{!?_slurm_datadir: %global _slurm_datadir %{_prefix}/share}
%define _datadir %{_slurm_datadir}

#  Allow override of mandir via _slurm_mandir.
%{!?_slurm_mandir: %global _slurm_mandir %{_datadir}/man}
%define _mandir %{_slurm_mandir}

#
# Never allow rpm to strip binaries as this will break
#  parallel debugging capability
# Note that brp-compress does not compress man pages installed
#  into non-standard locations (e.g. /usr/local)
#
%define __os_install_post /usr/lib/rpm/brp-compress

#
# Should unpackaged files in a build root terminate a build?
# Uncomment if needed again.
#%define _unpackaged_files_terminate_build      0

# Slurm may intentionally include empty manifest files, which will
# cause errors with rpm 4.13 and on. Turn that check off.
%define _empty_manifest_terminate_build 0

# First we remove $prefix/local and then just prefix to make
# sure we get the correct installdir
%define _perlarch %(perl -e 'use Config; $T=$Config{installsitearch}; $P=$Config{installprefix}; $P1="$P/local"; $T =~ s/$P1//; $T =~ s/$P//; print $T;')

%define _perlman3 %(perl -e 'use Config; $T=$Config{installsiteman3dir}; $P=$Config{siteprefix}; $P1="$P/local"; $T =~ s/$P1//; $T =~ s/$P//; print $T;')

%define _perlarchlib %(perl -e 'use Config; $T=$Config{installarchlib}; $P=$Config{installprefix}; $P1="$P/local"; $T =~ s/$P1//; $T =~ s/$P//; print $T;')

%define _perldir %{_prefix}%{_perlarch}
%define _perlman3dir %{_prefix}%{_perlman3}
%define _perlarchlibdir %{_prefix}%{_perlarchlib}

%description
Slurm is an open source, fault-tolerant, and highly scalable
cluster management and job scheduling system for Linux clusters.
Components include machine status, partition management,
job management, scheduling and accounting modules

%package -n %{pname}-perlapi%{PROJ_DELIM}
Summary: Perl API to Slurm
Group: %{PROJ_NAME}/rms
Requires: %{pname}%{PROJ_DELIM} = %{version}-%{release}
%description -n %{pname}-perlapi%{PROJ_DELIM}
Perl API package for Slurm.  This package includes the perl API to provide a
helpful interface to Slurm through Perl

%package -n %{pname}-devel%{PROJ_DELIM}
Summary: Development package for Slurm
Group: %{PROJ_NAME}/rms
Requires: %{pname}%{PROJ_DELIM} = %{version}-%{release}
%description -n %{pname}-devel%{PROJ_DELIM}
Development package for Slurm.  This package includes the header files
and static libraries for the Slurm API

%package -n %{pname}-example-configs%{PROJ_DELIM}
Summary: Example config files for Slurm
Group: %{PROJ_NAME}/rms
Requires: munge
%description -n %{pname}-example-configs%{PROJ_DELIM}
Example configuration files for Slurm.

%package  -n %{pname}-slurmctld%{PROJ_DELIM}
Summary: Slurm controller daemon
Group: %{PROJ_NAME}/rms
Requires: %{pname}%{PROJ_DELIM} = %{version}-%{release}
%description -n %{pname}-slurmctld%{PROJ_DELIM}
Slurm controller daemon. Used to manage the job queue, schedule jobs,
and dispatch RPC messages to the slurmd processon the compute nodes
to launch jobs.

%package -n %{pname}-slurmd%{PROJ_DELIM}
Summary: Slurm compute node daemon
Group: %{PROJ_NAME}/rms
Requires: %{pname}%{PROJ_DELIM} = %{version}-%{release}
%description -n %{pname}-slurmd%{PROJ_DELIM}
Slurm compute node daemon. Used to launch jobs on compute nodes

%package -n %{pname}-slurmdbd%{PROJ_DELIM}
Summary: Slurm database daemon
Group: %{PROJ_NAME}/rms
Requires: %{pname}%{PROJ_DELIM} = %{version}-%{release}
Obsoletes: slurm-sql%{PROJ_DELIM}
%description -n %{pname}-slurmdbd%{PROJ_DELIM}
Slurm database daemon. Used to accept and process database RPCs and upload
database changes to slurmctld daemons on each cluster

%package -n %{pname}-libpmi%{PROJ_DELIM}
Summary: Slurm\'s implementation of the pmi libraries
Group: %{PROJ_NAME}/rms
Requires: %{pname}%{PROJ_DELIM} = %{version}-%{release}
Conflicts: pmix-libpmi
%description -n %{pname}-libpmi%{PROJ_DELIM}
Slurm\'s version of libpmi. For systems using Slurm, this version
is preferred over the compatibility libraries shipped by the PMIx project.

%package -n %{pname}-torque%{PROJ_DELIM}
Summary: Torque/PBS wrappers for transition from Torque/PBS to Slurm
Group: %{PROJ_NAME}/rms
Requires: slurm-perlapi%{PROJ_DELIM}
%description -n %{pname}-torque%{PROJ_DELIM}
Torque wrapper scripts used for helping migrate from Torque/PBS to Slurm

%package -n %{pname}-openlava%{PROJ_DELIM}
Summary: openlava/LSF wrappers for transition from OpenLava/LSF to Slurm
Group: %{PROJ_NAME}/rms
Requires: slurm-perlapi%{PROJ_DELIM}
%description -n %{pname}-openlava%{PROJ_DELIM}
OpenLava wrapper scripts used for helping migrate from OpenLava/LSF to Slurm

%package -n %{pname}-contribs%{PROJ_DELIM}
Summary: Perl tool to print Slurm job state information
Group: %{PROJ_NAME}/rms
Requires: %{pname}%{PROJ_DELIM} = %{version}-%{release}
Obsoletes: slurm-sjobexit%{PROJ_DELIM} slurm-sjstat%{PROJ_DELIM} slurm-seff%{PROJ_DELIM}
%description -n %{pname}-contribs%{PROJ_DELIM}
seff is a mail program used directly by the Slurm daemons. On completion of a
job, wait for it's accounting information to be available and include that
information in the email body.
sjobexit is a slurm job exit code management tool. It enables users to alter
job exit code information for completed jobs
sjstat is a Perl tool to print Slurm job state information. The output is designed
to give information on the resource usage and availablilty, as well as information
about jobs that are currently active on the machine. This output is built
using the Slurm utilities, sinfo, squeue and scontrol, the man pages for these
utilities will provide more information and greater depth of understanding.

%package -n %{pname}-sview%{PROJ_DELIM}
Summary: Graphical user interface to view and modify Slurm state
Group: %{PROJ_NAME}/rms
Requires: %{pname}%{PROJ_DELIM} = %{version}-%{release}
BuildRequires: gtk2-devel
Requires: gtk2
%description  -n %{pname}-sview%{PROJ_DELIM}
This package provides sview, which can be used to view Slurm configuration, job, 
step, node and partitions state information. Authorized users can also modify 
select information.

%if %{with pam}
%package -n %{pname}-pam_slurm%{PROJ_DELIM}
Summary: PAM module for restricting access to compute nodes via Slurm
Group: %{PROJ_NAME}/rms
Requires: %{pname}%{PROJ_DELIM} = %{version}-%{release}
BuildRequires: pam-devel
Obsoletes: pam_slurm%{PROJ_DELIM}
%description -n %{pname}-pam_slurm%{PROJ_DELIM}
This module restricts access to compute nodes in a cluster where Slurm is in
use.  Access is granted to root, any user with an Slurm-launched job currently
running on the node, or any user who has allocated resources on the node
according to the Slurm
%endif

%if %{with cray}
%package slurmsmwd
Summary: support daemons and software for the Cray SMW
Group: System Environment/Base
Requires: %{name}%{?_isa} = %{version}-%{release}
Obsoletes: craysmw
%description slurmsmwd
support daeamons and software for the Cray SMW.  Includes slurmsmwd which
notifies slurm about failed nodes.
%endif

#############################################################################

%prep
# when the rel number is one, the tarball filename does not include it
%setup -n %{slurm_source_dir}
%if 0%{?rhel} > 7
mkdir bin
ln -s /usr/bin/python2 bin/python
%endif

%build
%if 0%{?rhel} > 7
export PATH="$PWD/bin:$PATH"
%endif
%configure \
	%{?_without_debug:--disable-debug} \
	%{?_with_pam_dir} \
	%{?_with_cpusetdir} \
	%{?_with_mysql_config} \
	%{?_with_ssl} \
	%{?_with_cray:--enable-native-cray}\
	%{?_with_cray_network:--enable-cray-network}\
	%{?_with_multiple_slurmd:--enable-multiple-slurmd} \
	%{?_with_pmix} \
	%{?_with_freeipmi} \
	%{?_with_hdf5} \
	%{?_with_shared_libslurm} \
        %{?_without_x11:--disable-x11} \
	%{?_with_cflags}

make %{?_smp_mflags}

%install
%if 0%{?rhel} > 7
export PATH="$PWD/bin:$PATH"
%endif

# Ignore redundant standard rpaths and insecure relative rpaths,
# for RHEL based distros which use "check-rpaths" tool.
export QA_RPATHS=0x5

# Strip out some dependencies

cat > find-requires.sh <<'EOF'
exec %{__find_requires} "$@" | egrep -v '^libpmix.so|libevent'
EOF
chmod +x find-requires.sh
%global _use_internal_dependency_generator 0
%global __find_requires %{_builddir}/%{buildsubdir}/find-requires.sh

make install DESTDIR=%{buildroot}
make install-contrib DESTDIR=%{buildroot}

install -D -m644 etc/slurmctld.service %{buildroot}/%{_unitdir}/slurmctld.service
install -D -m644 etc/slurmd.service    %{buildroot}/%{_unitdir}/slurmd.service
install -D -m644 etc/slurmdbd.service  %{buildroot}/%{_unitdir}/slurmdbd.service

# Do not package Slurm's version of libpmi on Cray systems in the usual location.
# Cray's version of libpmi should be used. Move it elsewhere if the site still
# wants to use it with other MPI stacks.
%if %{with cray}
   mkdir %{buildroot}/%{_libdir}/slurmpmi
   mv %{buildroot}/%{_libdir}/libpmi* %{buildroot}/%{_libdir}/slurmpmi
   install -D -m644 contribs/cray/plugstack.conf.template %{buildroot}/%{_sysconfdir}/plugstack.conf.template
   install -D -m644 contribs/cray/slurm.conf.template %{buildroot}/%{_sysconfdir}/slurm.conf.template
   mkdir -p %{buildroot}/opt/modulefiles/slurm
   test -f contribs/cray/opt_modulefiles_slurm &&
      install -D -m644 contribs/cray/opt_modulefiles_slurm %{buildroot}/opt/modulefiles/slurm/%{version}-%{rel}
   install -D -m644 contribs/cray/slurmsmwd/slurmsmwd.service %{buildroot}/%{_unitdir}/slurmsmwd.service
   echo -e '#%Module\nset ModulesVersion "%{version}-%{rel}"' > %{buildroot}/opt/modulefiles/slurm/.version
%else
   rm -f contribs/cray/opt_modulefiles_slurm
   rm -f contribs/cray/slurmsmwd/slurmsmwd.service
   rm -f %{buildroot}/%{_sysconfdir}/plugstack.conf.template
   rm -f %{buildroot}/%{_sysconfdir}/slurm.conf.template
   rm -f %{buildroot}/%{_sbindir}/capmc_suspend
   rm -f %{buildroot}/%{_sbindir}/capmc_resume
   rm -f %{buildroot}/%{_sbindir}/slurmconfgen.py
   rm -f %{buildroot}/%{_sbindir}/slurmsmwd
%endif

install -D -m644 etc/cgroup.conf.example %{buildroot}/%{_sysconfdir}/cgroup.conf.example
install -D -m644 etc/layouts.d.power.conf.example %{buildroot}/%{_sysconfdir}/layouts.d/power.conf.example
install -D -m644 etc/layouts.d.power_cpufreq.conf.example %{buildroot}/%{_sysconfdir}/layouts.d/power_cpufreq.conf.example
install -D -m644 etc/layouts.d.unit.conf.example %{buildroot}/%{_sysconfdir}/layouts.d/unit.conf.example
install -D -m644 etc/slurm.conf.example %{buildroot}/%{_sysconfdir}/slurm.conf.example
# 2/11/19 karl@ices.utexas.edu - include epilog cleanup file that shipped with 17.x releases
install -D -m755 %{SOURCE1} %{buildroot}/%{_sysconfdir}/slurm.epilog.clean
#
install -D -m644 etc/slurmdbd.conf.example %{buildroot}/%{_sysconfdir}/slurmdbd.conf.example
install -D -m755 contribs/sjstat %{buildroot}/%{_bindir}/sjstat

# 9/8/14 karl.w.schulz@intel.com - provide starting config file
%if 0%{?OHPC_BUILD}
head -n -2 $RPM_BUILD_ROOT/%{_sysconfdir}/slurm.conf.example | grep -v ReturnToService > $RPM_BUILD_ROOT/%{_sysconfdir}/slurm.conf
echo "# OpenHPC default configuration" >> $RPM_BUILD_ROOT/%{_sysconfdir}/slurm.conf
# 10/2/18 brad.geltz@intel.com - Enabling the task/affinity plugin to add the --cpu-bind option to srun for GEOPM
echo "TaskPlugin=task/affinity" >> $RPM_BUILD_ROOT/%{_sysconfdir}/slurm.conf
echo "PropagateResourceLimitsExcept=MEMLOCK" >> $RPM_BUILD_ROOT/%{_sysconfdir}/slurm.conf
echo "AccountingStorageType=accounting_storage/filetxt" >> $RPM_BUILD_ROOT/%{_sysconfdir}/slurm.conf
echo "Epilog=/etc/slurm/slurm.epilog.clean" >> $RPM_BUILD_ROOT/%{_sysconfdir}/slurm.conf
echo "NodeName=c[1-4] Sockets=2 CoresPerSocket=8 ThreadsPerCore=2 State=UNKNOWN" >> $RPM_BUILD_ROOT/%{_sysconfdir}/slurm.conf
echo "PartitionName=normal Nodes=c[1-4] Default=YES MaxTime=24:00:00 State=UP" >> $RPM_BUILD_ROOT/%{_sysconfdir}/slurm.conf
# 6/3/16 nirmalasrjn@gmail.com - Adding ReturnToService Directive to starting config file (note removal of variable during above creation)
echo "ReturnToService=1" >> $RPM_BUILD_ROOT/%{_sysconfdir}/slurm.conf
# 9/17/14 karl.w.schulz@intel.com - Add option to drop VM cache during epilog
sed -i '/^# No other SLURM jobs,/i \\n# Drop clean caches (OpenHPC)\necho 3 > /proc/sys/vm/drop_caches\n\n#' $RPM_BUILD_ROOT/%{_sysconfdir}/slurm.epilog.clean

%endif

# Correct some file permissions
test -f $RPM_BUILD_ROOT/%{_libdir}/libpmi.la   &&
   chmod 644 $RPM_BUILD_ROOT/%{_libdir}/libpmi.la
test -f $RPM_BUILD_ROOT/%{_libdir}/libslurm.la &&
   chmod 644 $RPM_BUILD_ROOT/%{_libdir}/libslurm.la
test -f $RPM_BUILD_ROOT/%{_libdir}/libslurmdb.la &&
   chmod 644 $RPM_BUILD_ROOT/%{_libdir}/libslurmdb.la

# Delete unpackaged files:
find %{buildroot} -name '*.a' -exec rm {} \;
find %{buildroot} -name '*.la' -exec rm {} \;
rm -f %{buildroot}/%{_libdir}/slurm/job_submit_defaults.so
rm -f %{buildroot}/%{_libdir}/slurm/job_submit_logging.so
rm -f %{buildroot}/%{_libdir}/slurm/job_submit_partition.so
rm -f %{buildroot}/%{_libdir}/slurm/auth_none.so
rm -f %{buildroot}/%{_libdir}/slurm/launch_poe.so
rm -f %{buildroot}/%{_libdir}/slurm/libpermapi.so
rm -f %{buildroot}/%{_libdir}/slurm/libsched_if.so
rm -f %{buildroot}/%{_libdir}/slurm/libsched_if64.so
rm -f %{buildroot}/%{_libdir}/slurm/proctrack_sgi_job.so
rm -f %{buildroot}/%{_libdir}/slurm/runjob_plugin.so
rm -f %{buildroot}/%{_libdir}/slurm/select_bluegene.so
rm -f %{buildroot}/%{_libdir}/slurm/switch_nrt.so
rm -f %{buildroot}/%{_mandir}/man5/bluegene*
rm -f %{buildroot}/%{_sbindir}/sfree
rm -f %{buildroot}/%{_sbindir}/slurm_epilog
rm -f %{buildroot}/%{_sbindir}/slurm_prolog
rm -f %{buildroot}/%{_sysconfdir}/init.d/slurm
rm -f %{buildroot}/%{_sysconfdir}/init.d/slurmdbd
rm -f %{buildroot}/%{_perldir}/auto/Slurm/.packlist
rm -f %{buildroot}/%{_perldir}/auto/Slurm/Slurm.bs
rm -f %{buildroot}/%{_perlarchlibdir}/perllocal.pod
rm -f %{buildroot}/%{_perldir}/perllocal.pod
rm -f %{buildroot}/%{_perldir}/auto/Slurmdb/.packlist
rm -f %{buildroot}/%{_perldir}/auto/Slurmdb/Slurmdb.bs

# Build man pages that are generated directly by the tools
rm -f %{buildroot}/%{_mandir}/man1/sjobexitmod.1
%{buildroot}/%{_bindir}/sjobexitmod --roff > %{buildroot}/%{_mandir}/man1/sjobexitmod.1
rm -f %{buildroot}/%{_mandir}/man1/sjstat.1
%{buildroot}/%{_bindir}/sjstat --roff > %{buildroot}/%{_mandir}/man1/sjstat.1

# Build conditional file list for main package
LIST=./slurm.files
touch $LIST
test -f %{buildroot}/%{_libexecdir}/slurm/cr_checkpoint.sh   &&
  echo %{_libexecdir}/slurm/cr_checkpoint.sh	        >> $LIST
test -f %{buildroot}/%{_libexecdir}/slurm/cr_restart.sh      &&
  echo %{_libexecdir}/slurm/cr_restart.sh	        >> $LIST
test -f %{buildroot}/%{_sbindir}/capmc_suspend		&&
  echo %{_sbindir}/capmc_suspend			>> $LIST
test -f %{buildroot}/%{_sbindir}/capmc_resume		&&
  echo %{_sbindir}/capmc_resume				>> $LIST
test -f %{buildroot}/%{_bindir}/netloc_to_topology		&&
  echo %{_bindir}/netloc_to_topology			>> $LIST

test -f %{buildroot}/opt/modulefiles/slurm/%{version}-%{rel} &&
  echo /opt/modulefiles/slurm/%{version}-%{rel} >> $LIST
test -f %{buildroot}/opt/modulefiles/slurm/.version &&
  echo /opt/modulefiles/slurm/.version >> $LIST

# Make pkg-config file
mkdir -p %{buildroot}/%{_libdir}/pkgconfig
cat >%{buildroot}/%{_libdir}/pkgconfig/slurm.pc <<EOF
includedir=%{_prefix}/include
libdir=%{_libdir}

Cflags: -I\${includedir}
Libs: -L\${libdir} -lslurm
Description: Slurm API
Name: %{pname}
Version: %{version}
EOF

LIST=./pam.files
touch $LIST
%if %{?with_pam_dir}0
    test -f %{buildroot}/%{with_pam_dir}/pam_slurm.so	&&
	echo %{with_pam_dir}/pam_slurm.so	>>$LIST
    test -f %{buildroot}/%{with_pam_dir}/pam_slurm_adopt.so	&&
	echo %{with_pam_dir}/pam_slurm_adopt.so	>>$LIST
%else
    test -f %{buildroot}/lib/security/pam_slurm.so	&&
	echo /lib/security/pam_slurm.so		>>$LIST
    test -f %{buildroot}/lib32/security/pam_slurm.so	&&
	echo /lib32/security/pam_slurm.so	>>$LIST
    test -f %{buildroot}/lib64/security/pam_slurm.so	&&
	echo /lib64/security/pam_slurm.so	>>$LIST
    test -f %{buildroot}/lib/security/pam_slurm_adopt.so		&&
	echo /lib/security/pam_slurm_adopt.so		>>$LIST
    test -f %{buildroot}/lib32/security/pam_slurm_adopt.so		&&
	echo /lib32/security/pam_slurm_adopt.so		>>$LIST
    test -f %{buildroot}/lib64/security/pam_slurm_adopt.so		&&
	echo /lib64/security/pam_slurm_adopt.so		>>$LIST
%endif
mkdir -p $RPM_BUILD_ROOT/%{_docdir}

%post -n %{pname}-example-configs%{PROJ_DELIM}
if [ ! -e %{_sysconfdir}/munge/munge.key -a -c /dev/urandom ]; then
  /bin/dd if=/dev/urandom bs=1 count=1024 \
    >%{_sysconfdir}/munge/munge.key 2>/dev/null
  /bin/chown munge:munge %{_sysconfdir}/munge/munge.key
  /bin/chmod 0400 %{_sysconfdir}/munge/munge.key
fi

%files -f slurm.files
%{_datadir}/doc
%{_bindir}/s*
%exclude %{_bindir}/seff
%exclude %{_bindir}/sjobexitmod
%exclude %{_bindir}/sjstat
%exclude %{_bindir}/smail
%exclude %{_bindir}/sview
%exclude %{_libdir}/libpmi*
%{_libdir}/*.so*
%{_libdir}/slurm/src/*
%{_libdir}/slurm/*.so
%exclude %{_libdir}/slurm/accounting_storage_mysql.so
%exclude %{_libdir}/slurm/job_submit_pbs.so
%exclude %{_libdir}/slurm/spank_pbs.so
%{_mandir}
%exclude %{_mandir}/man1/sjobexit*
%exclude %{_mandir}/man1/sjstat*
%exclude %{_mandir}/man1/sview*
%dir %{_libdir}/slurm/src
#############################################################################

%files -n %{pname}-example-configs%{PROJ_DELIM}
%dir %{_sysconfdir}
%if %{with cray}
%config %{_sysconfdir}/plugstack.conf.template
%config %{_sysconfdir}/slurm.conf.template
%dir /opt/modulefiles/slurm
%{_sbindir}/slurmconfgen.py
%endif
%config %{_sysconfdir}/slurm.conf.example

%{OHPC_PUB}
%doc AUTHORS CONTRIBUTING.md COPYING DISCLAIMER INSTALL LICENSE.OpenSSL NEWS README.rst RELEASE_NOTES

# 9/8/14 karl.w.schulz@intel.com - provide starting config file
%if 0%{?OHPC_BUILD}
%config (noreplace) %{_sysconfdir}/slurm.conf
%endif

# 11/13/14 karl.w.schulz@intel.com - include systemd files 

%if 0%{?suse_version} >= 1200 || 0%{?rhel_version} >= 700 || 0%{?centos_version} >= 700

%config /usr/lib/systemd/system/slurmctld.service
%config /usr/lib/systemd/system/slurmd.service
%config /usr/lib/systemd/system/slurmdbd.service

%endif

%config %{_sysconfdir}/cgroup.conf.example
%config %{_sysconfdir}/layouts.d/power.conf.example
%config %{_sysconfdir}/layouts.d/power_cpufreq.conf.example
%config %{_sysconfdir}/layouts.d/unit.conf.example
%config %{_sysconfdir}/slurm.conf.example
%config %{_sysconfdir}/slurm.epilog.clean
%config %{_sysconfdir}/slurmdbd.conf.example
#############################################################################

%files -n %{pname}-devel%{PROJ_DELIM}
%dir %attr(0755,root,root)
%dir %{_prefix}/include/slurm
%{_prefix}/include/slurm/*
%dir %{_libdir}/pkgconfig
%{_libdir}/pkgconfig/slurm.pc
#############################################################################

%files -n %{pname}-perlapi%{PROJ_DELIM}
%{_perldir}/Slurm.pm
%{_perldir}/Slurm/Bitstr.pm
%{_perldir}/Slurm/Constant.pm
%{_perldir}/Slurm/Hostlist.pm
%{_perldir}/Slurm/Stepctx.pm
%{_perldir}/auto/Slurm/Slurm.so
%{_perldir}/Slurmdb.pm
%{_perldir}/auto/Slurmdb/Slurmdb.so
%{_perldir}/auto/Slurmdb/autosplit.ix
%{_perlman3dir}/Slurm*
#############################################################################

%files -n %{pname}-slurmctld%{PROJ_DELIM}
%{_sbindir}/slurmctld
%{_unitdir}/slurmctld.service
#############################################################################

%files -n %{pname}-slurmd%{PROJ_DELIM}
%{_sbindir}/slurmd
%{_sbindir}/slurmstepd
%{_unitdir}/slurmd.service
#############################################################################

%files -n %{pname}-slurmdbd%{PROJ_DELIM}
%{_sbindir}/slurmdbd
%{_libdir}/slurm/accounting_storage_mysql.so
%{_unitdir}/slurmdbd.service
#############################################################################

%files -n %{pname}-libpmi%{PROJ_DELIM}
%if %{with cray}
%{_libdir}/slurmpmi/*
%else
%{_libdir}/libpmi*
%endif
#############################################################################

%files -n %{pname}-torque%{PROJ_DELIM}
%{_bindir}/pbsnodes
%{_bindir}/qalter
%{_bindir}/qdel
%{_bindir}/qhold
%{_bindir}/qrerun
%{_bindir}/qrls
%{_bindir}/qstat
%{_bindir}/qsub
%{_bindir}/mpiexec
%{_bindir}/generate_pbs_nodefile
%{_libdir}/slurm/job_submit_pbs.so
%{_libdir}/slurm/spank_pbs.so
#############################################################################

%files -n %{pname}-openlava%{PROJ_DELIM}
%{_bindir}/bjobs
%{_bindir}/bkill
%{_bindir}/bsub
%{_bindir}/lsid
#############################################################################

%files -n %{pname}-contribs%{PROJ_DELIM}
%{_bindir}/seff
%{_bindir}/sjobexitmod
%{_bindir}/sjstat
%{_bindir}/smail
%{_mandir}/man1/sjstat*
#############################################################################

%files -n %{pname}-sview%{PROJ_DELIM}
%{_mandir}/man1/sview*
%{_bindir}/sview
#############################################################################

%if %{with pam}
%files -f pam.files -n %{pname}-pam_slurm%{PROJ_DELIM}
%endif
#############################################################################

%if %{with cray}
%files slurmsmwd
%{_sbindir}/slurmsmwd
%{_unitdir}/slurmsmwd.service
%endif
#############################################################################

%pre

# provide specific uid/gid to ensure that it is the same across the cluster
/usr/bin/getent group slurm >/dev/null 2>&1 || \
  /usr/sbin/groupadd -r slurm -g 202
/usr/bin/getent passwd slurm >/dev/null 2>&1 || \
  /usr/sbin/useradd -c "SLURM resource manager" \
  -d %{_sysconfdir} -g slurm -s /sbin/nologin -r slurm -u 202
  
exit 0

%post

# 3/31/17 karl.w.schulz@intel.com - fix perm for txt accounting file possibility
if [ ! -f /var/log/slurm_jobacct.log ];then
    touch /var/log/slurm_jobacct.log
    chown slurm: /var/log/slurm_jobacct.log
fi

# 8/8/17 karl.w.schulz@intel.com - create StateSaveLocation
if [ ! -d /var/spool/slurm/ctld ];then
   mkdir -p /var/spool/slurm/ctld
   chown slurm: /var/spool/slurm
   chown slurm: /var/spool/slurm/ctld
fi

if [ -x /sbin/ldconfig ]; then
    /sbin/ldconfig %{_libdir}
fi

%preun

%postun
/sbin/ldconfig

%post -n %{pname}-slurmctld%{PROJ_DELIM}
%systemd_post slurmctld.service
%preun -n %{pname}-slurmctld%{PROJ_DELIM}
%systemd_preun slurmctld.service
%postun -n %{pname}-slurmctld%{PROJ_DELIM}
%systemd_postun_with_restart slurmctld.service

%post -n %{pname}-slurmd%{PROJ_DELIM}
%systemd_post slurmd.service
%preun -n %{pname}-slurmd%{PROJ_DELIM}
%systemd_preun slurmd.service
%postun -n %{pname}-slurmd%{PROJ_DELIM}
%systemd_postun_with_restart slurmd.service

%post -n %{pname}-slurmdbd%{PROJ_DELIM}
%systemd_post slurmdbd.service
%preun -n %{pname}-slurmdbd%{PROJ_DELIM}
%systemd_preun slurmdbd.service
%postun -n %{pname}-slurmdbd%{PROJ_DELIM}
%systemd_postun_with_restart slurmdbd.service
