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

%if 0%{?OHPC_BUILD}

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

%define debug_package %{nil}

%if 0%{?suse_version} == 1110
BuildRequires: kernel-source = 3.0.76-0.11.1
BuildRequires: kernel-default-devel = 3.0.76-0.11.1
%define kdir /lib/modules/3.0.76-0.11-default/source/
%define kobjdir /lib/modules/3.0.76-0.11-default/build/
%endif

%if 0%{?suse_version} == 1315
BuildRequires: kernel-source = 3.12.49-11.1
BuildRequires: kernel-default-devel = 3.12.49-11.1
%define kdir /lib/modules/3.12.49-11-default/source/
%define kobjdir /lib/modules/3.12.49-11-default/build/
%endif

%if 0%{?centos_version} == 600
BuildRequires: kernel = 2.6.32-431.el6
BuildRequires: kernel-devel = 2.6.32-431.el6
%define kdir /lib/modules/2.6.32-431.el6.x86_64/source/
%define kobjdir /lib/modules/2.6.32-431.el6.x86_64/build/
%endif

%if 0%{?centos_version} == 700
# 7.1 kernel version
# %define centos_kernel 3.10.0-229.el7

# 7.2 kernel version
%define centos_kernel 3.10.0-327.el7
BuildRequires: kernel = %{centos_kernel}
BuildRequires: kernel-devel = %{centos_kernel}
%define kdir /lib/modules/%{centos_kernel}.x86_64/source/
%define kobjdir /lib/modules/%{centos_kernel}.x86_64/build/

%endif

BuildRequires: python-docutils

%endif

BuildRequires:	-post-build-checks

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
%bcond_with lnet_dlc

%if %{without servers}
    # --without servers overrides --with {ldiskfs|zfs}
    # so undefine the internal variables set by bcond_*
    %undefine with_ldiskfs
    %undefine with_zfs
%endif

%define sha_full  8eb26594344887fabc4b81245abd12b1aeefbccb
%define sha_short 8eb2659
%{!?version: %global version 2.8.0}

%{!?kver: %global kver ""}
%{!?kdir: %global kdir %(dir=$(echo "%configure_args" | sed -ne 's/.*--with-linux=\\([^ ][^ ]*\\).*$/\\1/p'); if [ -n "$dir" ]; then echo "$dir"; else if [ -n "%kver" ]; then kversion="%kver"; else kversion="$(uname -r)"; fi; echo "/lib/modules/$kversion/source"; fi)}

%{!?kobjdir: %global kobjdir %(dir=$(echo "%configure_args" | sed -ne 's/.*--with-linux-obj=\\([^ ][^ ]*\\).*$/\\1/p'); if [ -n "$dir" ]; then echo "$dir"; else if [ -n "%kver" ]; then kversion="%kver"; else kversion="$(uname -r)"; fi; if [ "%kdir" = "/lib/modules/$kversion/source" ]; then echo "/lib/modules/$kversion/build"; else echo "%kdir"; fi; fi)}

# as an alternative to this implementation we could simply "make -C $kdir kernelversion"
%{!?kversion: %global kversion %(if test -s %kobjdir/include/generated/utsrelease.h ; then LINUXRELEASEHEADER=%kobjdir/include/generated/utsrelease.h ; elif test -s %kobjdir/include/linux/utsrelease.h ; then LINUXRELEASEHEADER=%kobjdir/include/linux/utsrelease.h ; else LINUXRELEASEHEADER=%kobjdir/include/linux/version.h; fi; sed -ne '/^#define UTS_RELEASE/s/.*"\\(.*\\)"$/\\1/p' $LINUXRELEASEHEADER)}

%{!?downstream_release: %global downstream_release ""}

%define buildid %(if [ -n "" ]; then echo "_"; fi)

%{!?myrelease: %global myrelease %(if [ -n "%downstream_release" ]; then echo -n "%{downstream_release}_"; fi; echo %kversion | tr '-' '_')}

# always append the buildid, even when the caller defines %release
%define fullrelease %{myrelease}%{buildid}

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

# for those uses that don't want the -smp/-bigsmp (or the .arch) on the end
# of %kversion
%define krequires %(bash -c "echo %{kversion} | sed -e 's/\.x86_64$//' -e 's/\.i[3456]86$//' -e 's/-smp$//' -e 's/-bigsmp$//' -e 's/-ppc64$//' -e 's/-default$//'")

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

%if %{defined cross_path} && %{defined post_script}
%define rpm_post_base %(echo $(dirname %{cross_path})/%{lustre_name})
%endif

# SUSE don't support .debug_info section from cross compiler:
# /usr/lib/rpm/debugedit: Unhandled relocation 10 in .debug_info section
%if %{defined cross_path} && 0%{?suse_version}
%global __debug_install_post %{nil}
%global __debug_package %{nil}
%global debug_package %{nil}
%endif

Summary: Lustre File System
Name: %{lustre_name}%{PROJ_DELIM}
Version: %{version}
Release: %{fullrelease}
License: GPL
Group:   %{PROJ_NAME}/lustre
Source: lustre-%{version}.tar.gz
Source1: OHPC_macros
URL: https://wiki.hpdd.intel.com/
DocDir: %{OHPC_PUB}/doc/contrib
BuildRoot: %{_tmppath}/lustre-%{version}-root
Obsoletes: lustre-lite, lustre-lite-utils, lustre-ldap nfs-utils-lustre
Provides: lustre-lite = %{version}, lustre-lite-utils = %{version}
Requires: %{name}-modules = %{version}
BuildRequires: libtool
%if %{with servers}
Requires: lustre-osd
Requires: lustre-osd-mount
%endif
%if %{defined cross_requires}
Requires: %{cross_requires}
AutoReqProv: no
%else
# GSS requires this: BuildRequires: pkgconfig, libgssapi-devel >= 0.10
%if %{_vendor}=="redhat" || %{_vendor}=="fedora"
#suse don't support selinux
BuildRequires: libselinux-devel
Requires: libselinux
%endif
%endif

%description
Userspace tools and files for the Lustre file system.

%if %{with lustre_modules}
%package modules
Summary: Kernel Lustre modules for Linux %{kversion}
%if %{defined cross_requires}
Requires: %{cross_requires}
AutoReqProv: no
%else
# for SLES11, we need nothing here
%if %{_vendor}=="redhat" || %{_vendor}=="fedora"
# for RHEL we need to require the specific kernel still since weak-modules
# support on RH is, well, weak, to be punny about it
Requires: kernel = %{krequires}
%if %{with lnet_dlc}
Requires: libyaml
BuildRequires: libyaml-devel
%endif
%endif
%endif
Group: Development/Kernel

%description modules
Lustre file system, server and network drivers for Linux %{kversion}.

%if %{with ldiskfs}
%package osd-ldiskfs
Summary: osd-ldiskfs contains both ldiskfs and its osd interface in Lustre.
Requires: lustre-modules = %{version}
Requires: module-init-tools >= 3.9
Requires: ldiskfsprogs >= 1.42.7.wc1
Requires: lustre-osd-ldiskfs-mount
Provides: lustre-osd
Obsoletes: lustre-ldiskfs
Group: Development/Kernel

%description osd-ldiskfs
The Lustre Object Storage Device (OSD) API is the interface to access and
modify data that is supposed to be stored persistently. This API is the interface
to code that bridges individual file systems. This specific package provides an
implementation of the OSD API for using the Ldiskfs filesystem as the underlying
backing store of a Lustre server.

%if %{with lustre_utils}
%package osd-ldiskfs-mount
Summary: osd-ldiskfs-mount contains mount's ldiskfs specific dso.
Provides: lustre-osd-mount
Group: Development/Kernel

%description osd-ldiskfs-mount
LDISKFS hooks for mount/mkfs into a dynamic library.

%endif
%endif

%if %{with zfs}
%package osd-zfs
Summary: osd-zfs is the mandatory glue for ZFS support in Lustre.
Requires: lustre-modules = %{version}, zfs-kmod
Requires: lustre-osd-zfs-mount
Provides: lustre-osd
Group: Development/Kernel

%description osd-zfs
The Lustre Object Storage Device (OSD) API is the interface to access and
modify data that is supposed to be stored persistently. This API is the interface
to code that bridges individual file systems. This specific package provides an
implementation of the OSD API for using the ZFS filesystem as the underlying
backing store of a Lustre server.

%if %{with lustre_utils}
%package osd-zfs-mount
Summary: osd-zfs-mount contains mount's ldiskfs specific dso.
Provides: lustre-osd-mount
Group: Development/Kernel

%description osd-zfs-mount
ZFS hooks for mount/mkfs into a dynamic library.

%endif
%endif
%endif # with lustre_modules

%package source
Summary: Object-Based Disk storage driver source
Group: Development/Kernel

%description source
Lustre sources for further development

# Since the RPMs we ship are to be used on both SLES and RHEL, we
# can't include any dependency information (since the package names
# are different on the two platforms).
#
# Instead, we can build these empty meta-packages that only include
# dependency information.  These let people get the correct
# dependencies for their platform and lets them use tools like yum and
# red carpet to install the correct files.
#
# Unfortunately I have not seen this come up on the lists much, so I
# have disabled them (by commenting out their empty files section
# below) until it's clear that they resolve more confusion than they
# add.

%package deps-sles
Summary: Lustre dependencies meta-package for SLES
Group: Utilities/System
Provides: lustre-deps = %{version}
Requires: %{name} = %{version}, sles-release
Conflicts: %{name}-deps-rhel

%description deps-sles
This package has RPM dependencies appropriate for SLES systems.

%package deps-rhel
Summary: Lustre dependencies meta-package for RHEL
Group: Utilities/System
Provides: lustre-deps = %{version}
Requires: %{name} = %{version}, redhat-release
Conflicts: %{name}-deps-sles

%description deps-rhel
This package has RPM dependencies appropriate for RHEL, RHL, and FC
systems.

%package tests
Summary: Lustre testing framework
Group: Development/Kernel
Provides: %{name}-tests = %{version}
Requires: %{name} = %{version}, %{name}-modules = %{version}, lustre-iokit
Requires: attr, rsync, perl, lsof, /usr/bin/getconf

%description tests
This package contains a set of test binaries and scripts that are intended
to be used by the Lustre testing framework.

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

%if 0%{?suse_version}
%debug_package
%endif
%prep

%setup -qn lustre-%{version}
#patch1 -p1

ln lustre/ChangeLog ChangeLog-lustre
ln lnet/ChangeLog ChangeLog-lnet

%build

# Set an explicit path to our Linux tree, if we can.

cd $RPM_BUILD_DIR/lustre-%{version}

# override %optflags so that the vendor's overzealous flags don't create
# build failures
%define optflags -g -O2 -Werror

%if 0%{?OHPC_BUILD}
#CONFIGURE_ARGS="%{?configure_args} --with-release=3.0.76-0.11-default --disable-ldiskfs --disable-server"
CONFIGURE_ARGS="%{?configure_args} --with-release=%release --disable-ldiskfs --disable-server"
%else
CONFIGURE_ARGS="%{?configure_args} --with-release=%release"
%endif

%if %{with lustre_tests}
CONFIGURE_ARGS="$CONFIGURE_ARGS --enable-tests"
%else
CONFIGURE_ARGS="$CONFIGURE_ARGS --disable-tests"
%endif

%if %{with lustre_utils}
CONFIGURE_ARGS="$CONFIGURE_ARGS --enable-utils"
%else
CONFIGURE_ARGS="$CONFIGURE_ARGS --disable-utils"
%endif

%if %{without lustre_iokit}
CONFIGURE_ARGS="$CONFIGURE_ARGS --disable-iokit"
%endif

%if %{with lustre_modules}
CONFIGURE_ARGS="$CONFIGURE_ARGS --enable-modules"
%else
CONFIGURE_ARGS="$CONFIGURE_ARGS --disable-modules"
%endif

%if %{without servers}
CONFIGURE_ARGS="$CONFIGURE_ARGS --disable-server"
%endif

%if %{without ldiskfs}
CONFIGURE_ARGS="$CONFIGURE_ARGS --disable-ldiskfs"
%endif

%if %{without zfs}
CONFIGURE_ARGS="$CONFIGURE_ARGS --without-zfs"
%endif

# if %%kdir was given, make sure it's not in the configure arguments
if [ -n "%kdir" ]; then
	CONFIGURE_ARGS=$(echo $CONFIGURE_ARGS | sed -e 's/"\?--with-linux=[^ ][^ ]* \?//')
fi
# ditto for %%kobjdir
if [ -n "%kobjdir" ]; then
	CONFIGURE_ARGS=$(echo $CONFIGURE_ARGS | sed -e 's/"\?--with-linux-obj=[^ ][^ ]* \?//')
fi
# remove --with-kmp-moddir from configure arguments,
# it will be set --with-kmp-moddir=%%kmoddir
CONFIGURE_ARGS=$(echo $CONFIGURE_ARGS | sed -e 's/"\?--with-kmp-moddir=[^ ][^ ]* \?//')

# we need to eval "configure" because $CONFIGURE_ARGS could have a quoted
# string in it which we don't want word splitted by the shell
# also remove (build|host|target) options because they will be specified
# inside $CONFIGURE_ARGS
%define eval_configure %(echo '%configure' | sed -e 's#\./configure#eval ./configure#' -e 's/--\\(build\\|host\\|target\\)=[^ ][^ ]* //g')

%eval_configure \
	%{?kdir: --with-linux=%kdir} %{?kobjdir: --with-linux-obj=%kobjdir} \
	$CONFIGURE_ARGS --with-kmp-moddir=%{kmoddir}
make %{?_smp_mflags} -s %{?make_args}

%install
make install DESTDIR=$RPM_BUILD_ROOT

:> lustre.files

%if %{with servers}
# The .ha_v2 extension identifies the heartbeat resource agent as using
# legacy syntax. Install a compatibility symlink to avoid conflicts when
# newer-style agents are added.
%if %{with lustre_utils}
ln -s Lustre.ha_v2 $RPM_BUILD_ROOT%{_sysconfdir}/ha.d/resource.d/Lustre
echo '%{_sysconfdir}/ha.d/resource.d/Lustre.ha_v2' >>lustre.files
echo '%{_sysconfdir}/ha.d/resource.d/Lustre' >>lustre.files
%endif

if [ -f $RPM_BUILD_ROOT%{_sysconfdir}/init.d/lustre ]; then
	echo '%{_sysconfdir}/sysconfig/lustre' >>lustre.files
	echo '%{_sysconfdir}/init.d/lustre' >>lustre.files
fi
%endif

if [ -f $RPM_BUILD_ROOT%{_sysconfdir}/init.d/lnet ]; then
	echo '%{_sysconfdir}/init.d/lnet' >>lustre.files
fi

# Create the pristine source directory.
cd $RPM_BUILD_DIR/lustre-%{version}
mkdir -p $RPM_BUILD_ROOT%{_prefix}/src
rm -f lustre-source
ln -s $RPM_BUILD_ROOT%{_prefix}/src lustre-source
make distdir distdir=lustre-source/lustre-%{version}
chmod -R go-w lustre-source/lustre-%{version}
# fc18 needs 'x' permission for library files
find $RPM_BUILD_ROOT -name \*.so -type f -exec chmod +x {} \;

if [ -f $RPM_BUILD_ROOT%{_libdir}/liblnetconfig.a ] ; then
  echo '%attr(-, root, root) %{_libdir}/liblnetconfig.a' >>lustre.files
  echo '%attr(-, root, root) %{_libdir}/liblnetconfig.so' >>lustre.files
fi

if [ -f $RPM_BUILD_ROOT%{_libdir}/liblustre.so ] ; then
  echo '%{_libdir}/liblustre.a' >>lustre.files
  echo '%{_libdir}/liblustre.so' >>lustre.files
fi

if [ -f $RPM_BUILD_ROOT%{_libdir}/libiam.a ] ; then
  echo '%{_libdir}/libiam.a' >>lustre.files
fi

if [ -d $RPM_BUILD_ROOT%{_libdir}/lustre/snmp ] ; then
  echo '%{_libdir}/lustre/snmp' >>lustre.files
  echo '%{_datadir}/lustre/snmp/mibs' >>lustre.files
fi

find $RPM_BUILD_ROOT%{_libdir}/lustre/ -name \*.la -type f -delete

%if %{with lustre_tests}
echo '%{_libdir}/lustre/tests/*' >>lustre-tests.files
echo '%{_bindir}/mcreate' >>lustre-tests.files
echo '%{_bindir}/munlink' >>lustre-tests.files
echo '%{_bindir}/req_layout' >>lustre-tests.files
echo '%{_sbindir}/wirecheck' >>lustre-tests.files
echo '%{_sbindir}/wiretest' >>lustre-tests.files
%if %{with lustre_modules}
echo '%{?rootdir}/lib/modules/%{kversion}/%{kmoddir}/kernel/fs/lustre/llog_test.ko' >>lustre-tests.files
%endif
%endif

%if %{defined cross_path}
%if %{defined rpm_post_base}
POST_SCRIPT=$RPM_BUILD_DIR/lustre-%{version}/%{post_script}
if [ -f $POST_SCRIPT ]; then
	cp -f $POST_SCRIPT $RPM_BUILD_ROOT/%{rpm_post_base}.sh
	echo '%attr(0555, root, root) %{rpm_post_base}.sh' >>lustre.files
	cp -f $POST_SCRIPT $RPM_BUILD_ROOT/%{rpm_post_base}-modules.sh
%if %{with ldiskfs}
	cp -f $POST_SCRIPT $RPM_BUILD_ROOT/%{rpm_post_base}-osd-ldiskfs.sh
%if %{with lustre_utils}
	cp -f $POST_SCRIPT $RPM_BUILD_ROOT/%{rpm_post_base}-mount-osd-ldiskfs.sh
%endif
%endif
%if %{with zfs}
	cp -f $POST_SCRIPT $RPM_BUILD_ROOT/%{rpm_post_base}-osd-zfs.sh
%if %{with lustre_utils}
	cp -f $POST_SCRIPT $RPM_BUILD_ROOT/%{rpm_post_base}-mount-osd-zfs.sh
%endif
%endif
%if %{with lustre_tests}
	cp -f $POST_SCRIPT $RPM_BUILD_ROOT/%{rpm_post_base}-tests.sh
	echo '%attr(0555, root, root) %{rpm_post_base}-tests.sh' >>lustre-tests.files
%endif
fi
%endif
%else
%if %{with lustre_modules}
# mark modules executable for find-debuginfo.sh
find $RPM_BUILD_ROOT%{?rootdir}/lib/modules/%{kversion}/%{kmoddir} \
    -name \*.ko -type f -exec chmod u+x {} \;
%endif
%endif

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files -f lustre.files
%defattr(-,root,root)
%{_sbindir}/*
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
%{?rootdir}/sbin/mount.lustre
%{_libdir}/libptlctl.a
%{_libdir}/libcfsutil.a
%{_libdir}/liblustreapi.a
%{_libdir}/liblustreapi.so
%{_mandir}/man?/*
%{_includedir}/lustre
%{_includedir}/libcfs
%endif
%{_datadir}/lustre
%{_sysconfdir}/udev/rules.d/99-lustre.rules
%config(noreplace) %{_sysconfdir}/ldev.conf
%if 0%{?centos_version}
/etc/init.d/lsvcgss
%endif

%if %{with lustre_modules}
%files modules
%defattr(-,root,root)
%{?rootdir}/lib/modules/%{kversion}/%{kmoddir}/*
%config /etc/modprobe.d/ko2iblnd.conf
%if %{with lustre_tests}
%exclude %{?rootdir}/lib/modules/%{kversion}/%{kmoddir}/kernel/fs/lustre/llog_test.ko
%endif
%if %{with ldiskfs}
%exclude %{?rootdir}/lib/modules/%{kversion}/%{kmoddir}/kernel/fs/lustre/ldiskfs.ko
%exclude %{?rootdir}/lib/modules/%{kversion}/%{kmoddir}/kernel/fs/lustre/osd_ldiskfs.ko
%endif
%if %{with zfs}
%exclude %{?rootdir}/lib/modules/%{kversion}/%{kmoddir}/kernel/fs/lustre/osd_zfs.ko
%endif
%if %{defined rpm_post_base}
%attr(0555, root, root) %{rpm_post_base}-modules.sh
%endif
%doc COPYING
%doc ChangeLog-lustre
%doc ChangeLog-lnet
%{OHPC_PUB}

%if %{with ldiskfs}
%files osd-ldiskfs
%defattr(-,root,root)
%{?rootdir}/lib/modules/%{kversion}/%{kmoddir}/kernel/fs/lustre/ldiskfs.ko
%{?rootdir}/lib/modules/%{kversion}/%{kmoddir}/kernel/fs/lustre/osd_ldiskfs.ko
%if %{defined rpm_post_base}
%attr(0555, root, root) %{rpm_post_base}-osd-ldiskfs.sh
%endif
%if %{with lustre_utils}
%files osd-ldiskfs-mount
%defattr(-,root,root)
%{_libdir}/lustre/mount_osd_ldiskfs.so
%if %{defined rpm_post_base}
%attr(0555, root, root) %{rpm_post_base}-mount-osd-ldiskfs.sh
%endif
%endif
%endif

%if %{with zfs}
%files osd-zfs
%defattr(-,root,root)
%{?rootdir}/lib/modules/%{kversion}/%{kmoddir}/kernel/fs/lustre/osd_zfs.ko
%if %{defined rpm_post_base}
%attr(0555, root, root) %{rpm_post_base}-osd-zfs.sh
%endif
%if %{with lustre_utils}
%files osd-zfs-mount
%defattr(-,root,root)
%{_libdir}/lustre/mount_osd_zfs.so
%if %{defined rpm_post_base}
%attr(0555, root, root) %{rpm_post_base}-mount-osd-zfs.sh
%endif
%endif
%endif
%endif # with lustre_modules

%files source
%defattr(-,root,root)
%{_prefix}/src/lustre-%{version}

# uncomment these lines to enable deps packages
# %files deps-sles
# %files deps-rhel

%if %{with lustre_tests}
%files tests -f lustre-tests.files
%defattr(-,root,root)
%endif

%if %{with lustre_iokit}
%files -n lustre-iokit
%defattr(-, root, root)
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

%if %{defined rpm_post_base}
%post
if [ -x %{rpm_post_base}.sh ]; then
	%{rpm_post_base}.sh %{cross_path} create
fi

%preun
if [ -x %{rpm_post_base}.sh ]; then
	%{rpm_post_base}.sh %{cross_path} remove
fi
%endif

%if %{with lustre_modules}
%post modules
%if %{defined rpm_post_base}
if [ -x %{rpm_post_base}-modules.sh ]; then
	%{rpm_post_base}-modules.sh %{cross_path} create
fi
%else
if [ -f /boot/System.map-%{kversion} ]; then
	depmod -ae -F /boot/System.map-%{kversion} %{kversion} || exit 0
else
	depmod -ae %{kversion} || exit 0
fi

MODULES_RPM_NAME=$(rpm -q %{name}-modules | grep "%{version}-%{release}")
# "weak modules" support
# Suse
if [ -x /usr/lib/module-init-tools/weak-modules ]; then
    rpm -ql $MODULES_RPM_NAME | grep '\.ko$' |
        /usr/lib/module-init-tools/weak-modules --add-modules
fi
# RedHat
if [ -x /sbin/weak-modules ]; then
    rpm -ql $MODULES_RPM_NAME | grep '\.ko$' |
        /sbin/weak-modules --add-modules
fi

# If the kernel was built to reject unsupported modules (e.g. not a Lustre
# kernel), and this hasn't been overridden in /etc/modprobe.d yet, then
# print a warning so that users are aware of this issue.
if sysctl kernel.unsupported >/dev/null 2>&1 &&
   [ "$(sysctl -n kernel.unsupported 2>/dev/null)" = "0" ] &&
    ! modprobe -c | grep -q "^allow_unsupported_modules[ \t]1" ; then
     echo "
     warning: the Lustre modules are not supported by Novell. To use Lustre
              on this system, you should put

     allow_unsupported_modules 1

     into /etc/modprobe.d/unsupported_modules"
fi
%endif

%if %{with ldiskfs}
%post osd-ldiskfs
%if %{defined rpm_post_base}
if [ -x %{rpm_post_base}-osd-ldiskfs.sh ]; then
	%{rpm_post_base}-osd-ldiskfs.sh %{cross_path} create
fi
%else
if [ -f /boot/System.map-%{kversion} ]; then
       depmod -ae -F /boot/System.map-%{kversion} %{kversion} || exit 0
else
       depmod -ae %{kversion} || exit 0
fi

OSD_LDISKFS_RPM_NAME=$(rpm -q %{name}-osd-ldiskfs | grep "%{version}-%{release}")
# "weak modules" support
# Suse
if [ -x /usr/lib/module-init-tools/weak-modules ]; then
    rpm -ql $OSD_LDISKFS_RPM_NAME | grep '\.ko$' |
        /usr/lib/module-init-tools/weak-modules --add-modules
fi
# RedHat
if [ -x /sbin/weak-modules ]; then
    rpm -ql $OSD_LDISKFS_RPM_NAME | grep '\.ko$' |
        /sbin/weak-modules --add-modules
fi

# If the kernel was built to reject unsupported modules (e.g. not a Lustre
# kernel), and this hasn't been overridden in /etc/modprobe.d yet, then
# print a warning so that users are aware of this issue.
if sysctl kernel.unsupported >/dev/null 2>&1 &&
   [ "$(sysctl -n kernel.unsupported 2>/dev/null)" = "0" ] &&
    ! modprobe -c | grep -q "^allow_unsupported_modules[ \t]1" ; then
     echo "
     warning: the Lustre modules are not supported by Novell. To use Lustre
              on this system, you should put

     allow_unsupported_modules 1

     into /etc/modprobe.d/unsupported_modules"
fi
%endif
%if %{with lustre_utils} && %{defined rpm_post_base}
%post osd-ldiskfs-mount
if [ -x %{rpm_post_base}-mount-osd-ldiskfs.sh ]; then
	%{rpm_post_base}-mount-osd-ldiskfs.sh %{cross_path} create
fi
%endif
%endif

%if %{with zfs}
%post osd-zfs
%if %{defined rpm_post_base}
if [ -x %{rpm_post_base}-osd-zfs.sh ]; then
	%{rpm_post_base}-osd-zfs.sh %{cross_path} create
fi
%else
if [ -f /boot/System.map-%{kversion} ]; then
       depmod -ae -F /boot/System.map-%{kversion} %{kversion} || exit 0
else
       depmod -ae %{kversion} || exit 0
fi

OSD_ZFS_RPM_NAME=$(rpm -q %{name}-osd-zfs | grep "%{version}-%{release}")
# "weak modules" support
# Suse
if [ -x /usr/lib/module-init-tools/weak-modules ]; then
    rpm -ql $OSD_ZFS_RPM_NAME | grep '\.ko$' |
        /usr/lib/module-init-tools/weak-modules --add-modules
fi
# RedHat
if [ -x /sbin/weak-modules ]; then
    rpm -ql $OSD_ZFS_RPM_NAME | grep '\.ko$' |
        /sbin/weak-modules --add-modules
fi

# If the kernel was built to reject unsupported modules (e.g. not a Lustre
# kernel), and this hasn't been overridden in /etc/modprobe.d yet, then
# print a warning so that users are aware of this issue.
if sysctl kernel.unsupported >/dev/null 2>&1 &&
   [ "$(sysctl -n kernel.unsupported 2>/dev/null)" = "0" ] &&
    ! modprobe -c | grep -q "^allow_unsupported_modules[ \t]1" ; then
     echo "
     warning: the Lustre modules are not supported by Novell. To use Lustre
              on this system, you should put

     allow_unsupported_modules 1

     into /etc/modprobe.d/unsupported_modules"
fi
%endif
%if %{with lustre_utils} && %{defined rpm_post_base}
%post osd-zfs-mount
if [ -x %{rpm_post_base}-mount-osd-zfs.sh ]; then
	%{rpm_post_base}-mount-osd-zfs.sh %{cross_path} create
fi
%endif
%endif

%preun modules
%if %{defined rpm_post_base}
if [ -x %{rpm_post_base}-modules.sh ]; then
	%{rpm_post_base}-modules.sh %{cross_path} remove
fi
%else
MODULES_RPM_NAME=$(rpm -q %{name}-modules | grep "%{version}-%{release}")
rpm -ql $MODULES_RPM_NAME | grep '\.ko$' > /var/run/%{name}-modules || true
%endif

%if %{with ldiskfs}
%preun osd-ldiskfs
%if %{defined rpm_post_base}
if [ -x %{rpm_post_base}-osd-ldiskfs.sh ]; then
	%{rpm_post_base}-osd-ldiskfs.sh %{cross_path} remove
fi
%else
OSD_LDISKFS_RPM_NAME=$(rpm -q %{name}-osd-ldiskfs | grep "%{version}-%{release}")
rpm -ql $OSD_LDISKFS_RPM_NAME | grep '\.ko$' > /var/run/%{name}-osd-ldiskfs || true
%endif
%if %{with lustre_utils} && %{defined rpm_post_base}
%preun osd-ldiskfs-mount
if [ -x %{rpm_post_base}-mount-osd-ldiskfs.sh ]; then
	%{rpm_post_base}-mount-osd-ldiskfs.sh %{cross_path} remove
fi
%endif
%endif

%if %{with zfs}
%preun osd-zfs
%if %{defined rpm_post_base}
if [ -x %{rpm_post_base}-osd-zfs.sh ]; then
	%{rpm_post_base}-osd-zfs.sh %{cross_path} remove
fi
%else
OSD_ZFS_RPM_NAME=$(rpm -q %{name}-osd-zfs | grep "%{version}-%{release}")
rpm -ql $OSD_ZFS_RPM_NAME | grep '\.ko$' > /var/run/%{name}-osd-zfs || true
%endif
%if %{with lustre_utils} && %{defined rpm_post_base}
%preun osd-zfs-mount
if [ -x %{rpm_post_base}-mount-osd-zfs.sh ]; then
	%{rpm_post_base}-mount-osd-zfs.sh %{cross_path} remove
fi
%endif
%endif

%if %{undefined cross_path}
%postun modules
if [ -f /boot/System.map-%{kversion} ]; then
	depmod -ae -F /boot/System.map-%{kversion} %{kversion} || exit 0
else
	depmod -ae %{kversion} || exit 0
fi

# "weak modules" support
# Suse
if [ -x /usr/lib/module-init-tools/weak-modules ]; then
    cat /var/run/%{name}-modules |
        /usr/lib/module-init-tools/weak-modules --remove-modules
fi
# RedHat
if [ -x /sbin/weak-modules ]; then
    cat /var/run/%{name}-modules |
        /sbin/weak-modules --remove-modules
fi
rm /var/run/%{name}-modules

%if %{with ldiskfs}
%postun osd-ldiskfs
if [ -f /boot/System.map-%{kversion} ]; then
       depmod -ae -F /boot/System.map-%{kversion} %{kversion} || exit 0
else
       depmod -ae %{kversion} || exit 0
fi

# "weak modules" support
# Suse
if [ -x /usr/lib/module-init-tools/weak-modules ]; then
    cat /var/run/%{name}-osd-ldiskfs |
        /usr/lib/module-init-tools/weak-modules --remove-modules
fi
# RedHat
if [ -x /sbin/weak-modules ]; then
    cat /var/run/%{name}-osd-ldiskfs |
        /sbin/weak-modules --remove-modules
fi
rm /var/run/%{name}-osd-ldiskfs
%endif

%if %{with zfs}
%postun osd-zfs
if [ -f /boot/System.map-%{kversion} ]; then
       depmod -ae -F /boot/System.map-%{kversion} %{kversion} || exit 0
else
       depmod -ae %{kversion} || exit 0
fi

# "weak modules" support
# Suse
if [ -x /usr/lib/module-init-tools/weak-modules ]; then
    cat /var/run/%{name}-osd-zfs |
        /usr/lib/module-init-tools/weak-modules --remove-modules
fi
# RedHat
if [ -x /sbin/weak-modules ]; then
    cat /var/run/%{name}-osd-zfs |
        /sbin/weak-modules --remove-modules
fi
rm /var/run/%{name}-osd-zfs
%endif
%endif
%endif # with lustre_modules

%if %{with lustre_tests}
%if %{defined rpm_post_base}
%post tests
if [ -x %{rpm_post_base}-tests.sh ]; then
	%{rpm_post_base}-tests.sh %{cross_path} create
fi

%preun tests
if [ -x %{rpm_post_base}-tests.sh ]; then
	%{rpm_post_base}-tests.sh %{cross_path} remove
fi
%else
%if %{with lustre_modules}
%post tests
if [ -f /boot/System.map-%{kversion} ]; then
	depmod -ae -F /boot/System.map-%{kversion} %{kversion} || exit 0
else
	depmod -ae %{kversion} || exit 0
fi

%postun tests
if [ -f /boot/System.map-%{kversion} ]; then
	depmod -ae -F /boot/System.map-%{kversion} %{kversion} || exit 0
else
	depmod -ae %{kversion} || exit 0
fi
%endif
%endif
%endif

%clean
rm -rf $RPM_BUILD_ROOT

