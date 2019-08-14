#---------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# lustre.spec

# needssslcertforbuild

%include %{_sourcedir}/OHPC_macros



%if 0%{?OHPC_BUILD}

%if 0%{?suse_version}
BuildRequires: kernel-source
BuildRequires: kernel-default-devel

%define sles_kernel 4.12.14-lp151.28.10-default
%define kdir /lib/modules/%{sles_kernel}/source/
%define kobjdir /lib/modules/%{sles_kernel}/build/
%endif

%if 0%{?centos_version} == 700

# 7.4 kernel version
%ifarch aarch64
%define centos_kernel 4.2.0-0.21.el7
BuildRequires: kernel = %{centos_kernel}
BuildRequires: kernel-devel = %{centos_kernel}
%define kdir /lib/modules/%{centos_kernel}.aarch64/source/
%define kobjdir /lib/modules/%{centos_kernel}.aarch64/build/
%else
%define centos_kernel 3.10.0-957.el7
BuildRequires: kernel = %{centos_kernel}
BuildRequires: kernel-devel = %{centos_kernel}
%define kdir /lib/modules/%{centos_kernel}.x86_64/source/
%define kobjdir /lib/modules/%{centos_kernel}.x86_64/build/
%endif

%endif

#!BuildIgnore: post-build-checks

%endif

# Declare rpmbuild --with/--without parameters
%if 0%{?OHPC_BUILD}
%bcond_with servers
%else
%bcond_without servers
%endif
%bcond_without ldiskfs
%bcond_with zfs
%bcond_without lustre_tests
%bcond_without lustre_utils
%bcond_without lustre_iokit
%bcond_without lustre_modules
%bcond_with snmp
%bcond_with gss
%bcond_with gss_keyring
%bcond_without manpages
%bcond_without shared
%bcond_without static
%bcond_with    systemd

# By default both gss and gss keyring are disabled.
# gss keyring requires the gss core. If the builder
# request gss_keyring we must enable gss core even if
# the builder attempts to disable gss.
%if %{with gss_keyring}
    %define with_gss
%endif

%if %{without servers}
    # --without servers overrides --with {ldiskfs|zfs}
    # so undefine the internal variables set by bcond_*
    %undefine with_ldiskfs
    %undefine with_zfs
%endif

%{!?version: %global version 2.12.2}
%{!?kver:    %global kver    %(uname -r)}
%{!?kdir:    %global kdir    /lib/modules/%{kver}/source}
%{!?kobjdir: %global kobjdir %(if [ "%{kdir}" = "/lib/modules/%{kver}/source" ]; then echo "/lib/modules/%{kver}/build"; else echo "%{kdir}"; fi)}

# as an alternative to this implementation we could simply "make -C $kdir kernelversion"
%{!?kversion: %global kversion %(files="include/generated/utsrelease.h include/linux/utsrelease.h include/linux/version.h"; for f in $files; do if test -r %{kobjdir}/$f && grep UTS_RELEASE %{kobjdir}/$f >/dev/null; then sed -ne '/^#define UTS_RELEASE/s/.*"\\(.*\\)"$/\\1/p' %{kobjdir}/$f; break; fi; done)}

# We set this because kernel_module_package has its own method of identifying
# which kernel to build against, and it is unlikely that its decision will
# match with Lustre's method of selecting which kernel to build against.
# By setting this variable, we override kernel_module_package's kernel
# selection with our selection.
%{!?kernel_version: %global kernel_version %kversion}

# in order to get kernel symset and/or kernel module dependencies into
# the RPM, in order to support weak-modules, the internal dependency gen-
# erator needs to be disabled
# this is done with (reduce the double % down to a single %):
#
# %%global _use_internal_dependency_generator 0
#
# on SLES10, /usr/lib/rpm/macros already sets this, so no harm in also
# defining it here (until Suse changes their mind)
#
# on RHEL5, however, we do need to explicitly disable the internal dep-
# endency generator and allow the external one be used
# but since RedHat's kABI is only a subset of the total kernel ABI, it
# doesn't include all of the symbols we (or OFED for that matter) need
# until RedHat includes all of the symbols we need in their symsets we
# cannot support weak-modules
# we did e-mail the maintainer of all of this stuff @redhat but got no
# response from them
#%%global _use_internal_dependency_generator 0

# Set the package name prefix
%if %{undefined lustre_name}
    %if %{with servers}
        %global lustre_name lustre
    %else
        %global lustre_name lustre-client
    %endif
%endif

%if %{undefined kmoddir}
    %if %{defined kernel_module_package_moddir}
        %global kmoddir %{kernel_module_package_moddir}
    %else
        %if %{defined suse_kernel_module_package}
            %global kmoddir updates
        %else
            %global kmoddir extra
        %endif
    %endif
%endif

# karl.w.schulz@intel.com (3/24/17)
%if 0%{?OHPC_BUILD}
%global lustre_name %{lustre_name}%{PROJ_DELIM}
%endif

%global modules_fs_path /lib/modules/%{kversion}/%{kmoddir}

%if %{_vendor}=="redhat" || %{_vendor}=="fedora"
	%global requires_yaml_name libyaml
	%global requires_kmod_name kmod-%{lustre_name}
	%if %{with lustre_tests}
		%global requires_kmod_tests_name kmod-%{lustre_name}-tests
	%endif
	%global requires_kmod_version %{version}
%else	#for Suse
	%global requires_yaml_name libyaml-0-2
	%global requires_kmod_name %{lustre_name}-kmp
	%if %{with lustre_tests}
		%global requires_kmod_tests_name %{lustre_name}-tests-kmp
	%endif
	%define krequires %(echo %{kversion} | sed -e 's/\.x86_64$//' -e 's/\.i[3456]86$//' -e 's/-smp$//' -e 's/-bigsmp$//' -e 's/[-.]ppc64$//' -e 's/\.aarch64$//' -e 's/-default$//')
	%if 0%{?suse_version} >= 1200
		%global requires_kmod_version %{version}_k%(echo %{krequires} | sed -r 'y/-/_/; s/^(2\.6\.[0-9]+)_/\\1.0_/;')
	%else
		%global requires_kmod_version %{version}_%(echo %{krequires} | sed -r 'y/-/_/; s/^(2\.6\.[0-9]+)_/\\1.0_/;')
	%endif
%endif

# RHEL >= 7 comes with systemd
%if 0%{?rhel} >= 7
%define with_systemd 1
%endif

# Fedora >= 15 comes with systemd, but only >= 18 has
# the proper macros
%if 0%{?fedora} >= 18
%define with_systemd 1
%endif

# opensuse >= 12.1 comes with systemd, but only >= 13.1
# has the proper macros
%if 0%{?suse_version} >= 1310
%define with_systemd 1
%endif

Summary: Lustre File System
Name: %{lustre_name}
Version: %{version}
Release: 1%{?dist}
License: GPL
Group:   %{PROJ_NAME}/lustre
Source: lustre-%{version}.tar.gz
Source1: kmp-lustre.preamble
Source2: kmp-lustre.files
Source3: kmp-lustre-osd-ldiskfs.preamble
Source4: kmp-lustre-osd-ldiskfs.files
Source5: kmp-lustre-osd-zfs.preamble
Source6: kmp-lustre-osd-zfs.files
Source7: kmp-lustre-tests.files
Patch0:  8b77b0b1.diff
URL: https://wiki.whamcloud.com/
Requires: %{requires_kmod_name} = %{requires_kmod_version} zlib
Requires: %{requires_yaml_name}
BuildRequires: libtool libyaml-devel zlib-devel
%if %{with servers}
Requires: lustre-osd
Requires: lustre-osd-mount
Obsoletes: lustre-client < %{version}
Provides: lustre-client = %{version}-%{release}
%endif
# GSS requires this: BuildRequires: pkgconfig, libgssapi-devel >= 0.10
%if %{_vendor}=="redhat" || %{_vendor}=="fedora"
#suse don't support selinux
BuildRequires: libselinux-devel
Requires: libselinux
%endif
%if %{with lustre_modules}
#BuildRequires: %kernel_module_package_buildreqs
%if %{_vendor}=="redhat"
BuildRequires: redhat-rpm-config
%endif
%endif

%if %{with systemd}
Requires(post): systemd
Requires(preun): systemd
Requires(postun): systemd
BuildRequires: systemd
%endif

%description
Userspace tools and files for the Lustre file system.

%if %{with lustre_modules}
%kernel_module_package -n %{name} -p %SOURCE1 -f %SOURCE2 default

%if %{with ldiskfs}
%kernel_module_package -n %{name}-osd-ldiskfs -p %SOURCE3 -f %SOURCE4 default
%if %{with lustre_utils}
%package osd-ldiskfs-mount
Summary: osd-ldiskfs-mount contains mount's ldiskfs specific dso.
Provides: lustre-osd-mount = %{version}-%{fullrelease}
Group: System Environment/Kernel

%description osd-ldiskfs-mount
LDISKFS hooks for mount/mkfs into a dynamic library.

%endif	# with lustre_utils
%endif	# with ldiskfs

%if %{with zfs}
%kernel_module_package -n %{name}-osd-zfs -p %SOURCE5 -f %SOURCE6 default
%if %{with lustre_utils}
%package osd-zfs-mount
Summary: osd-zfs-mount contains mount's zfs specific dso.
Provides: lustre-osd-mount = %{version}-%{fullrelease}
Group: System Environment/Kernel

%description osd-zfs-mount
ZFS hooks for mount/mkfs into a dynamic library.

%endif	# with lustre_utils
%endif	# with zfs

%endif # with lustre_modules

%if %{with servers}
%package resource-agents
Summary: HA Resuable Cluster Resource Scripts for Lustre
Group: System Environment/Base
Requires: lustre
Requires: resource-agents

%description resource-agents
A set of scripts to operate Lustre resources in a High Availablity
environment for both Pacemaker and rgmanager.
%endif

%if %{with lustre_tests}
%package tests
Summary: Lustre testing framework
Group: System Environment/Kernel
Provides: %{name}-tests = %{version}
%if %{with lustre_iokit}
Requires: %{name} = %{version}, lustre-iokit
%else
Requires: %{name} = %{version}
%endif
Requires: %{requires_kmod_name} = %{requires_kmod_version}
Requires: %{requires_kmod_tests_name} = %{requires_kmod_version}
Requires: attr, rsync, perl, lsof, /usr/bin/getconf

%description tests
This package contains a set of test binaries and scripts that are intended
to be used by the Lustre testing framework.

%if %{with lustre_modules}
%kernel_module_package -n %{name}-tests -f %SOURCE7 default
%endif
%endif

%if %{with lustre_iokit}
%package -n lustre-iokit
Summary: The Lustre IO-Kit is a collection of benchmark tools for a cluster with the Lustre file system.
Group: Applications/System
Requires: python > 2.2, sg3_utils

%description -n lustre-iokit
This package includes five tools:
sgpdd-survey:
A test of the 'bare metal' performance, bypassing as much of the kernel as we can. Uses the sgp_dd utility.

obdfilter-survey
This survey can be run in 3 modes to test disk I/O including the filesystem,
network I/O, and disk I/O via the network.  The script does sequential I/O
with varying numbers of threads and objects (files) by using lctl::test_brw
to drive the echo_client connected to local or remote obdfilter instances,
or remote obdecho instances.

ost-survey
This survey tests the client-to-disk performance of individual OSTs, and
ranks then for comparison.

stats-collect
This script will collect IO stats on a defined set of nodes.

ior-survey:
A script to run the IOR benchmark. The latest version can be downloaded from
http://www.llnl.gov/asci/purple/benchmarks/limited/ior/

mds-survey:
This survey tests the local metadata performance using the echo_client to drive
the MDD layer to perform operations. It is run with multiple threads (to
simulate MDT service threads) locally on the MDS node, and does not need Lustre
clients in order to run
%endif

%prep
%setup -qn lustre-%{version}
%patch0 -p1
ln lustre/ChangeLog ChangeLog-lustre
ln lnet/ChangeLog ChangeLog-lnet

%build
# Set an explicit path to our Linux tree, if we can.
cd $RPM_BUILD_DIR/lustre-%{version}
# override %optflags so that the vendor's overzealous flags don't create
# build failures
%define optflags -g -O2 -Werror

CONFIGURE_ARGS="%{?configure_args}"
if [ -n "$CONFIGURE_ARGS" ]; then
	# make sure %%kdir and %%kobjdir are not in the configure arguments
	CONFIGURE_ARGS=$(echo $CONFIGURE_ARGS | sed -e 's/"\?--with-linux=[^ ][^ ]* \?//')
	CONFIGURE_ARGS=$(echo $CONFIGURE_ARGS | sed -e 's/"\?--with-linux-obj=[^ ][^ ]* \?//')
	# remove --with-kmp-moddir from configure arguments,
	# it will be set --with-kmp-moddir=%%kmoddir
	CONFIGURE_ARGS=$(echo $CONFIGURE_ARGS | sed -e 's/"\?--with-kmp-moddir=[^ ][^ ]* \?//')
fi

# we need to eval "configure" because $CONFIGURE_ARGS could have a quoted
# string in it which we don't want word splitted by the shell
# also remove (build|host|target) options because they will be specified
# inside $CONFIGURE_ARGS
# kmod tools/scripts require %{name} directory with kernel modules
%define eval_configure %(echo '%configure' | sed -e 's#\./configure#eval ./configure#' -e 's/--\\(build\\|host\\|target\\)=[^ ][^ ]* //g')

sh ./autogen.sh

%eval_configure $CONFIGURE_ARGS \
	%{?with_lustre_tests:--enable-tests}%{!?with_lustre_tests:--disable-tests} \
	%{?with_lustre_utils:--enable-utils}%{!?with_lustre_utils:--disable-utils} \
	%{?with_lustre_modules:--enable-modules}%{!?with_lustre_modules:--disable-modules} \
	%{!?with_shared:--disable-shared} \
	%{!?with_static:--disable-static} \
	%{!?with_lustre_iokit:--disable-iokit} \
	%{!?with_ldiskfs:--disable-ldiskfs} \
	%{!?with_servers:--disable-server} \
	%{!?with_zfs:--without-zfs} \
	%{!?with_snmp:--disable-snmp} \
	%{!?with_gss:--disable-gss} \
	%{!?with_gss_keyring:--disable-gss-keyring} \
	%{!?with_manpages:--disable-manpages} \
	%{!?with_systemd:--with-systemdsystemunitdir=no} \
	%{?with_systemd:--with-systemdsystemunitdir=%{_unitdir}} \
	--with-linux=%{kdir} \
	--with-linux-obj=%{kobjdir} \
	--with-kmp-moddir=%{kmoddir}/%{name}

make %{?_smp_mflags} -s %{?make_args}

%install
make install DESTDIR=$RPM_BUILD_ROOT

# RHEL's kernel_module_path macro expects that all the modules
# in a kmod package will be in modules_fs_path/<sub packagename>
# but Lustre installs all of the modules in a single pass into
# a shared location.  Since this is a restriction imposed by
# RHEL, we handle this here in the spec file rather than in
# Lustre's build system.  This is not expected to bother SLES's
# kernel_module_path macro.
basemodpath=$RPM_BUILD_ROOT%{modules_fs_path}/%{lustre_name}
%if %{with ldiskfs}
mkdir -p $basemodpath-osd-ldiskfs/fs
mv $basemodpath/fs/osd_ldiskfs.ko $basemodpath-osd-ldiskfs/fs/osd_ldiskfs.ko
mv $basemodpath/fs/ldiskfs.ko $basemodpath-osd-ldiskfs/fs/ldiskfs.ko
%endif
%if %{with zfs}
mkdir -p $basemodpath-osd-zfs/fs
mv $basemodpath/fs/osd_zfs.ko $basemodpath-osd-zfs/fs/osd_zfs.ko
%endif
%if %{with lustre_tests}
mkdir -p $basemodpath-tests/fs
mv $basemodpath/fs/llog_test.ko $basemodpath-tests/fs/llog_test.ko
mkdir -p $RPM_BUILD_ROOT%{_libdir}/lustre/tests/kernel/
mv $basemodpath/fs/kinode.ko $RPM_BUILD_ROOT%{_libdir}/lustre/tests/kernel/
%endif

:> lustre.files

%if %{with servers} && %{with lustre_utils}
# The .ha_v2 extension identifies the heartbeat resource agent as using
# legacy syntax. Install a compatibility symlink to avoid conflicts when
# newer-style agents are added.
ln -s Lustre.ha_v2 $RPM_BUILD_ROOT%{_sysconfdir}/ha.d/resource.d/Lustre
echo '%{_sysconfdir}/ha.d/resource.d/Lustre.ha_v2' >>lustre.files
echo '%{_sysconfdir}/ha.d/resource.d/Lustre' >>lustre.files
%endif

# systemd is on redhat, fedora, and suse
%if %{with systemd}
echo '%{_unitdir}/lnet.service' >>lustre.files
%endif

%if %{_vendor}=="redhat"
# The following scripts are Red Hat specific
%if %{with servers}
echo '%{_sysconfdir}/init.d/lustre' >>lustre.files
echo '%{_sysconfdir}/sysconfig/lustre' >>lustre.files
%if %{with gss_keyring}
echo '%{_sysconfdir}/init.d/lsvcgss' >>lustre.files
echo '%{_sysconfdir}/sysconfig/lsvcgss' >>lustre.files
echo '%config(noreplace) %{_sysconfdir}/request-key.d/lgssc.conf' >>lustre.files
%endif
%endif

%if %{without systemd}
echo '%{_sysconfdir}/init.d/lnet' >>lustre.files
%endif
%endif

%if %{with servers}
mkdir -p $RPM_BUILD_ROOT%{_prefix}/lib/ocf/resource.d/lustre/
install -m 0755 contrib/scripts/pacemaker/* $RPM_BUILD_ROOT%{_prefix}/lib/ocf/resource.d/lustre/
%endif

# fc18 needs 'x' permission for library files
find $RPM_BUILD_ROOT -name \*.so -type f -exec chmod +x {} \;

rm -f $RPM_BUILD_ROOT%{_libdir}/liblnetconfig.la
%if %{with static}
echo '%attr(-, root, root) %{_libdir}/liblnetconfig.a' >>lustre.files
%endif
%if %{with shared}
echo '%attr(-, root, root) %{_libdir}/liblnetconfig.so' >>lustre.files
echo '%attr(-, root, root) %{_libdir}/liblnetconfig.so.*' >>lustre.files
%endif

%if %{with ldiskfs}
echo '%{_libdir}/libiam.a' >>lustre.files
%endif

%if %{with snmp}
mkdir -p $RPM_BUILD_ROOT/%{_libdir}/lustre/snmp
echo '%{_libdir}/lustre/snmp' >>lustre.files
%endif

%if %{with lustre_utils}
mkdir -p $RPM_BUILD_ROOT/%{_datadir}/lustre
if [ -d $RPM_BUILD_ROOT%{_libdir}/lustre ] ; then
	find $RPM_BUILD_ROOT%{_libdir}/lustre -name \*.la -type f -exec rm -f {} \;
fi
%endif

%if %{with lustre_modules}
# mark modules executable for find-debuginfo.sh
find $RPM_BUILD_ROOT/lib/modules -name \*.ko -type f -exec chmod u+x {} \;
%endif

rm -f $RPM_BUILD_ROOT%{_libdir}/liblustreapi.la

%if %{with lustre_tests}
:> lustre-tests.files
echo '%{_libdir}/lustre/tests/*' >>lustre-tests.files
echo '%{_bindir}/mcreate' >>lustre-tests.files
echo '%{_bindir}/munlink' >>lustre-tests.files
echo '%{_sbindir}/wirecheck' >>lustre-tests.files
echo '%{_sbindir}/wiretest' >>lustre-tests.files
%endif

%files -f lustre.files
%{_sbindir}/*
%if %{with lustre_tests}
%exclude %{_sbindir}/wirecheck
%exclude %{_sbindir}/wiretest
%endif
%if %{with zfs}
%exclude %{_sbindir}/zfsobj2fid
%endif
%if %{with lustre_utils}
%if %{with servers}
%{_libexecdir}/lustre/lc_common
%{_libexecdir}/lustre/haconfig
%{_bindir}/lustre_req_history
%endif

%{_bindir}/llobdstat
%{_bindir}/llstat
%{_bindir}/plot-llstat

%{_bindir}/lfs
%{_bindir}/lfs_migrate
/sbin/mount.lustre
%if %{with static}
%{_libdir}/liblustreapi.a
%endif
%if %{with shared}
%{_libdir}/liblustreapi.so*
%endif
%if %{with manpages}
%{_mandir}/man?/*
%endif
%{_datadir}/lustre
%{_includedir}/lustre
%{_includedir}/linux/lnet
%{_includedir}/linux/lustre
%endif
%{_sysconfdir}/udev/rules.d/99-lustre.rules
%if %{with zfs}
%config(noreplace) %{_sysconfdir}/ldev.conf
%endif
%config(noreplace) %{_sysconfdir}/lnet.conf
%config(noreplace) %{_sysconfdir}/modprobe.d/ko2iblnd.conf
%if %{with lustre_utils}
%config(noreplace) %{_sysconfdir}/lnet_routes.conf
%endif
%if %{with lustre_modules}

%if %{with shared}
%if %{with ldiskfs}
%if %{with lustre_utils}
%files osd-ldiskfs-mount
%{_libdir}/lustre/mount_osd_ldiskfs.so
%endif
%endif
%endif

%if %{with shared}
%if %{with zfs}
%if %{with lustre_utils}
%files osd-zfs-mount
%{_libdir}/lustre/mount_osd_zfs.so
%{_sysconfdir}/zfs/zed.d/*
%endif
%endif
%endif

%endif # with lustre_modules

%if %{with servers}
%files resource-agents
%defattr(0755,root,root)
%{_prefix}/lib/ocf/resource.d/lustre/
%endif

%if %{with lustre_tests}
%files tests -f lustre-tests.files
%endif

%if %{with lustre_iokit}
%files -n lustre-iokit
%{_bindir}/iokit-config
%{_bindir}/iokit-gather-stats
%{_bindir}/iokit-libecho
%{_bindir}/iokit-lstats
%{_bindir}/iokit-parse-ior
%{_bindir}/iokit-plot-obdfilter
%{_bindir}/iokit-plot-ost
%{_bindir}/iokit-plot-sgpdd
%{_bindir}/ior-survey
%{_bindir}/mds-survey
%{_bindir}/obdfilter-survey
%{_bindir}/ost-survey
%{_bindir}/sgpdd-survey
%doc lustre-iokit/ior-survey/README.ior-survey
%doc lustre-iokit/mds-survey/README.mds-survey
%doc lustre-iokit/obdfilter-survey/README.obdfilter-survey
%doc lustre-iokit/ost-survey/README.ost-survey
%doc lustre-iokit/sgpdd-survey/README.sgpdd-survey
%doc lustre-iokit/stats-collect/README.iokit-lstats
%endif

%post
%if %{with systemd}
%systemd_post lnet.service
%endif

%preun
%if %{with systemd}
%systemd_preun lnet.service
%endif

%postun
%if %{with systemd}
%systemd_postun_with_restart lnet.service
%endif
