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
Version:        1.6.0
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
BuildRequires:  systemd

%if 0%{?suse_version}
BuildRequires:  udev
BuildRequires:  kernel-default-devel
#!BuildIgnore: post-build-checks
%endif

%if 0%{?rhel}
BuildRequires: kernel
BuildRequires: kernel-devel
BuildRequires: kernel-abi-whitelists kernel-rpm-macros elfutils-libelf-devel
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
users.  The modifications that one user makes to allowlisted registers
may impact subsequent users of the processor if not restored.


%prep
%setup -q -n %{pname}-%{version}

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

install -d %{buildroot}/%{_datadir}/msr-safe/allowlists
install -m 0644 allowlists/* %{buildroot}/%{_datadir}/msr-safe/allowlists/
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
if which weak-modules >/dev/null 2>&1; then
    echo /lib/modules/%{latest_kernel}/extra/msr-safe/msr-safe.ko | weak-modules --add-modules
fi
/usr/bin/systemctl daemon-reload >/dev/null 2>&1
/usr/bin/systemctl enable msr-safe >/dev/null 2>&1 || :

%preun
if [ $1 -eq 0 ] ; then
    /usr/bin/systemctl stop msr-safe >/dev/null 2>&1
    /usr/bin/systemctl disable msr-safe >/dev/null 2>&1
fi
if which weak-modules >/dev/null 2>&1; then
    echo /lib/modules/%{latest_kernel}/extra/msr-safe/msr-safe.ko | weak-modules --remove-modules
fi

%postun
if [ "$1" -ge "1" ] ; then
   /usr/bin/systemctl try-restart msr-safe >/dev/null 2>&1 || :
fi

%files
%dir %{_datadir}/msr-safe
%dir %{_datadir}/msr-safe/allowlists
%{_datadir}/msr-safe/allowlists/*
%{_unitdir}/msr-safe.service
%{_udevrulesdir}/10-msr-safe.rules
%config %{_sysconfdir}/sysconfig/msr-safe
%doc README.md
%doc LICENSE
%doc THANKS
%{_sbindir}/msrsave
%{_sbindir}/msr-safe
%dir %{_mandir}/man1
%doc %{_mandir}/man1/msrsave.1.gz

%files -n %{pname}-slurm%{PROJ_DELIM}
%{_libdir}/slurm/libspank_msrsafe.so
