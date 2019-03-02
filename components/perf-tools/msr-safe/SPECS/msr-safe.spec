#---------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# msr-safe.spec

%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname msr-safe


Name:           %{pname}%{PROJ_DELIM}
Version:        1.2.0
Release:        1
License:        GPLv3+
Summary:        Allows safer access to model specific registers (MSRs)
Url:            https://github.com/LLNL/msr-safe
Group:          %{PROJ_NAME}/perf-tools
Source0:        https://github.com/LLNL/%{pname}/archive/v%{version}.tar.gz#/%{pname}-%{version}.tar.gz
Source1:        msr-safe.service
Source2:        msr-safe.sysconfig
Source3:        10-msr-safe.rules
Source4:        msr-safe.sh
Patch1:         0001-Correcting-hash_for_each_possible-function.-Fixes-41.patch
Patch2:         0002-Adding-slurm-spank-plugin-to-enable-MSR-save-restore.patch
#BuildRequires:  %kernel_module_package_buildreqs
BuildRequires:  systemd

%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  udev
BuildRequires:  kernel-default-devel
#!BuildIgnore: post-build-checks
%endif

%if 0%{?centos_version} == 700
%define centos_kernel 3.10.0-957.el7
BuildRequires: kernel = %{centos_kernel}
BuildRequires: kernel-devel = %{centos_kernel}
%endif

%kernel_module_package default

%description
Allows safer access to model specific registers (MSRs)

%package -n %{pname}-slurm%{PROJ_DELIM}
Summary: SLURM spank plugin for msr-safe
Group: Development/Libraries
Requires:       %{pname}%{PROJ_DELIM}
%if 0%{?sles_version} || 0%{?suse_version}
Requires:       %{pname}%{PROJ_DELIM}-kmp-default
%else
Requires:       kmod-%{pname}%{PROJ_DELIM}
%endif
BuildRequires:  slurm%{PROJ_DELIM}
BuildRequires:  slurm-devel%{PROJ_DELIM}

%description -n %{pname}-slurm%{PROJ_DELIM}
The purpose of this slurm plugin is to ensure that MSRs modified
within a user slurm job allocation are reset to their original state
before the compute node is returned to the pool available to other
users of the system.  The msr-safe kernel module is targeting HPC
systems that enforce single user occupancy per compute node, and is
not appropriate for systems where compute nodes are shared between
users.  The modifications that one user makes to whitelisted registers
may impact subsequent users of the processor if not restored.


%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1
%patch2 -p1

%build
for flavor in %flavors_to_build; do
    rm -rf obj/$flavor
    mkdir -p obj/$flavor
    cp -r msr* Makefile obj/$flavor
    %{__make} -C %{kernel_source $flavor} M=$PWD/obj/$flavor
done
%{__make} CPPFLAGS="-DVERSION=\\\"%{version}-%{release}\\\"" msrsave/msrsave
%{__make} CPPFLAGS="-DVERSION=\\\"%{version}-%{release}\\\"" spank

%install
%{__make} install DESTDIR=%{buildroot} prefix=%{_prefix} sbindir=%{_sbindir} mandir=%{_mandir}
%{__make} install-spank DESTDIR=%{buildroot} prefix=%{_prefix} libdir=%{_libdir}

install -d %{buildroot}/%{_datadir}/msr-safe/whitelists
install -m 0644 whitelists/* %{buildroot}/%{_datadir}/msr-safe/whitelists/
install -d %{buildroot}%{_unitdir}
install -m 0644 %{SOURCE1} %{buildroot}%{_unitdir}/msr-safe.service
install -d %{buildroot}/%{_sysconfdir}/sysconfig
install -m 0644 %{SOURCE2} %{buildroot}/%{_sysconfdir}/sysconfig/msr-safe
install -d %{buildroot}/%{_udevrulesdir}
install -m 0644 %{SOURCE3} %{buildroot}/%{_udevrulesdir}/10-msr-safe.rules
install -d %{buildroot}%{_sbindir}
install -m 0755 %{SOURCE4} %{buildroot}%{_sbindir}/msr-safe

export INSTALL_MOD_PATH=$RPM_BUILD_ROOT
export INSTALL_MOD_DIR=extra/%{name}
for flavor in %flavors_to_build ; do
        make -C %{kernel_source $flavor} modules_install \
                M=$PWD/obj/$flavor
done

%pre
getent group msr >/dev/null || groupadd -r msr
exit 0

%post
/usr/bin/udevadm control --reload-rules
echo /lib/modules/%{latest_kernel}/extra/msr-safe/msr-safe.ko | weak-modules --add-modules
/usr/bin/systemctl daemon-reload >/dev/null 2>&1
/usr/bin/systemctl enable msr-safe >/dev/null 2>&1 || :

%preun
if [ $1 -eq 0 ] ; then
    /usr/bin/systemctl stop msr-safe >/dev/null 2>&1
    /usr/bin/systemctl disable msr-safe >/dev/null 2>&1
fi
echo /lib/modules/%{latest_kernel}/extra/msr-safe/msr-safe.ko | weak-modules --remove-modules

%postun
if [ "$1" -ge "1" ] ; then
   /usr/bin/systemctl try-restart msr-safe >/dev/null 2>&1 || :
fi

%files
%dir %{_datadir}/msr-safe
%dir %{_datadir}/msr-safe/whitelists
%{_datadir}/msr-safe/whitelists/*
%{_unitdir}/msr-safe.service
%{_udevrulesdir}/10-msr-safe.rules
%config %{_sysconfdir}/sysconfig/msr-safe
%doc README
%{_sbindir}/msrsave
%{_sbindir}/msr-safe
%dir %{_mandir}/man1
%doc %{_mandir}/man1/msrsave.1.gz

%files -n %{pname}-slurm%{PROJ_DELIM}
%{_libdir}/slurm/libspank_msrsafe.so
