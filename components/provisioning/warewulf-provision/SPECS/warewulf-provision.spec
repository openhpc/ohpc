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

%define dname provision
%define pname warewulf-%{dname}
%define wwsrvdir /srv
%define develSHA c6de604fc76eabfaef2cb99f4c6ae5ed44eff1e0
%define wwextract warewulf3-%{develSHA}

Name:    %{pname}%{PROJ_DELIM}
Version: 3.10.0
Release: 1%{?dist}
Summary: Warewulf - System provisioning core
License: US Dept. of Energy (BSD-like) and BSD-3 Clause
URL:     http://warewulf.lbl.gov/
Source0: https://github.com/warewulf/warewulf3/archive/%{develSHA}.tar.gz
Source1: ipxe-09e8a15.tar.xz
Source2: https://github.com/ipxe/ipxe/pull/1036.patch
Patch0:  warewulf-provision.bin-file.patch
Patch1:  warewulf-provision.ipxe-kargs.patch
Patch2:  warewulf-provision.parted_libdir.patch
Patch3:  warewulf-provision.ppc64le.patch
Patch4:  warewulf-provision.sle_tftpboot.patch
Patch5:  warewulf-provision.wwgetfiles.patch
Patch6:  warewulf-provision.update_ipxe_to_09e8a15.patch
Patch7:  warewulf-provision.zstd.patch
Patch8:  https://github.com/warewulf/warewulf3/commit/ecb8ece02892caf2cbd3b8d74a1f3e01052b9844.patch
Patch9:  apply-ipxe-pr-1036.patch
Group:   %{PROJ_NAME}/provisioning
Requires: warewulf-common%{PROJ_DELIM}
Requires: perl-CGI
Requires: %{name}-initramfs-%{_arch} = %{version}-%{release}
Conflicts: warewulf < 3
BuildRequires: autoconf
BuildRequires: automake, make
BuildRequires: which
BuildRequires: gcc
BuildRequires: git
BuildRequires: warewulf-common%{PROJ_DELIM}
BuildRequires: libselinux-devel, libacl-devel, libattr-devel
BuildRequires: libuuid-devel, device-mapper-devel, xz-devel
BuildRequires: libtirpc-devel
BuildRequires: kmod

%if 0%{?rhel} || 0%{?openEuler}
BuildRequires: systemd
BuildRequires: perl-generators
%global httpsvc httpd
%global httpgrp apache
%global tftpsvc tftp
%global tftpsrv tftp-server
%global dhcpsrv dhcp-server
%else
BuildRequires: systemd-rpm-macros
%global httpsvc apache2
%global httpgrp www
%global tftpsvc tftp
%global tftpsrv tftp
%global dhcpsrv dhcp-server
%endif

# New RHEL and SLE include the required FS tools
%if 0%{?rhel} || 0%{?sle_version}
%global localtools 1
BuildRequires: parted, e2fsprogs, bsdtar, mdadm, xfsprogs, kmod
Requires: parted, autofs, e2fsprogs
BuildRequires: libarchive.so.13()(64bit)
Requires: libarchive.so.13()(64bit)
%global CONF_FLAGS --with-local-e2fsprogs --with-local-libarchive --with-local-parted --with-local-partprobe --with-local-mdam --with-local-xfsprogs --with-local-kmod
%else
%global localtools 0
Requires: %{name}-gpl_sources = %{version}-%{release}
Provides: parted = 3.2
Provides: e2fsprogs = 1.42.12
Provides: libarchive = 3.3.1
Provides: libarchive.so.13()(64bit)
%endif

%description
Warewulf is an operating system management toolkit designed to facilitate
large scale deployments of systems on physical, virtual and cloud-based
infrastructures. It facilitates elastic and large deployments consisting
of groups of homogeneous systems.

Warewulf Provision contains the core components, extensions, and tools to
administrate system provisioning.  To perform provisioning, the
%{name}-server package is also required.


%prep
cd %{_builddir}
%{__rm} -rf %{name}-%{version} %{wwextract}
%{__ln_s} %{wwextract}/%{dname} %{name}-%{version}
%setup -q -D
%patch -P0 -p1
%patch -P1 -p1
%patch -P2 -p1
%patch -P3 -p1
%patch -P4 -p1
%patch -P5 -p1
%patch -P6 -p1
%define _default_patch_fuzz 3
%patch -P7 -p2
%patch -P8 -p2
%patch -P9 -p1
%{__rm} -f 3rd_party/GPL/ipxe-2265a65.tar.xz
%{__cp} %SOURCE1 %SOURCE2 3rd_party/GPL/


%build
./autogen.sh

# Configure needs to locate mkfs.ext4
export PATH=/usr/sbin:/sbin:$PATH

%configure --sharedstatedir=%{wwsrvdir} %{?CONF_FLAGS}
%{__make} %{?_smp_mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT


%post
if [ $1 -eq 2 ] ; then
  echo "To update software within existing bootstraps run: wwsh bootstrap rebuild"
fi


%files
%doc AUTHORS ChangeLog INSTALL NEWS README TODO COPYING LICENSE
%config(noreplace) %{_sysconfdir}/warewulf/provision.conf
%config(noreplace) %{_sysconfdir}/warewulf/livesync.conf
%config(noreplace) %{_sysconfdir}/warewulf/defaults/provision.conf
%dir %{_sysconfdir}/warewulf/filesystem
%dir %{_sysconfdir}/warewulf/filesystem/examples
%{_sysconfdir}/warewulf/filesystem/examples/*.cmds
%{_mandir}/*
%{perl_vendorlib}/Warewulf/Bootstrap.pm
%{perl_vendorlib}/Warewulf/Provision.pm
%{perl_vendorlib}/Warewulf/Vnfs.pm
%{perl_vendorlib}/Warewulf/DSO/*
%{perl_vendorlib}/Warewulf/Provision
%{perl_vendorlib}/Warewulf/Event/DynamicHosts.pm
%{perl_vendorlib}/Warewulf/Event/DefaultProvisionNode.pm
%{perl_vendorlib}/Warewulf/Event/Genders.pm
%{perl_vendorlib}/Warewulf/Event/Http.pm
%{perl_vendorlib}/Warewulf/Event/ProvisionFileDelete.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Bootstrap.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Provision.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Ucode.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Vnfs.pm

%package initramfs-%{_arch}
Summary: Warewulf - initramfs base for %{_arch}
BuildArch: noarch
Requires: warewulf-common%{PROJ_DELIM}

%description initramfs-%{_arch}
Warewulf is an operating system management toolkit designed to facilitate
large scale deployments of systems on physical, virtual and cloud-based
infrastructures. It facilitates elastic and large deployments consisting
of groups of homogeneous systems.

This package includes tools and files to create an initramfs
image and to provide boot capability for %{_arch} architecture.

%files initramfs-%{_arch}
%dir %{wwsrvdir}/warewulf
%dir %{wwsrvdir}/warewulf/initramfs
%{wwsrvdir}/warewulf/initramfs/%{_arch}


%package server
Summary: Warewulf - System provisioning server
Requires: %{name} = %{version}-%{release}
Requires: %{name}-server-ipxe-%{_arch} = %{version}-%{release}
Requires: %{httpsvc}, perl(Apache), %{tftpsrv}, %{dhcpsrv}

%if 0%{?rhel} || 0%{?openEuler}
Requires(post): policycoreutils-python-utils
%else
Requires(pre): shadow
Requires(post): policycoreutils
%endif

%description server
Warewulf is an operating system management toolkit designed to facilitate
large scale deployments of systems on physical, virtual and cloud-based
infrastructures. It facilitates elastic and large deployments consisting
of groups of homogeneous systems.

This package contains the CGI scripts and event components to
provision systems.  Systems used solely for administration of Warewulf
do not require this package.

%pre server
/usr/bin/getent group %{httpgrp} 1>/dev/null 2>&1 || \
  /usr/sbin/groupadd -r %{httpgrp}

%post server
# Update users and services on first time installation
if [ $1 -eq 1 ] ; then
usermod -a -G warewulf %{httpgrp} >/dev/null 2>&1 || :
%{__mkdir_p} %{wwsrvdir}/warewulf/ipxe %{wwsrvdir}/warewulf/bootstrap 2>/dev/null || :
%if 0%{?sle_version} || 0%{?rhel}
%systemd_post %{httpsvc}.service
%systemd_post %{tftpsvc}.socket
%else
/usr/bin/systemctl --system enable %{httpsvc}.service >/dev/null 2>&1 || :
/usr/bin/systemctl --system restart %{httpsvc}.service >/dev/null 2>&1 || :
/usr/bin/systemctl --system enable %{tftpsvc}.socket >/dev/null 2>&1 || :
/usr/bin/systemctl --system restart %{tftpsvc}.socket >/dev/null 2>&1 || :
%endif
fi

# Reset selinux context on any installation or update
/usr/sbin/semanage fcontext -a -t httpd_sys_content_t '%{wwsrvdir}/warewulf/ipxe(/.*)?' 2>/dev/null || :
/usr/sbin/semanage fcontext -a -t httpd_sys_content_t '%{wwsrvdir}/warewulf/bootstrap(/.*)?' 2>/dev/null || :
/sbin/restorecon -R %{wwsrvdir}/warewulf || :


%postun server
# Remove selinux context on package removal. Don't disable web or tftp services.
if [ $1 -eq 0 ] ; then
semanage fcontext -d -t httpd_sys_content_t '%{wwsrvdir}/warewulf/ipxe(/.*)?' 2>/dev/null || :
semanage fcontext -d -t httpd_sys_content_t '%{wwsrvdir}/warewulf/bootstrap(/.*)?' 2>/dev/null || :
/sbin/restorecon -R %{wwsrvdir}/warewulf || :
fi
%if 0%{?sle_version} || 0%{?rhel}
%systemd_postun_with_restart %{httpsvc}.service
%systemd_postun_with_restart %{tftpsvc}.socket
%else
/usr/bin/systemctl --system restart %{httpsvc}.service >/dev/null 2>&1 || :
/usr/bin/systemctl --system restart %{tftpsvc}.socket >/dev/null 2>&1 || :
%endif

%files server
%config(noreplace) %{_sysconfdir}/warewulf/dhcpd-template.conf
%config(noreplace) %{_sysconfdir}/warewulf/dnsmasq-template.conf
%dir %{_sysconfdir}/%{httpsvc}
%dir %{_sysconfdir}/%{httpsvc}/conf.d
%config(noreplace) %{_sysconfdir}/%{httpsvc}/conf.d/warewulf-httpd.conf
%{_bindir}/*
%attr(0750, root, %{httpgrp}) %{_libexecdir}/warewulf/cgi-bin/
%{perl_vendorlib}/Warewulf/Event/Bootstrap.pm
%{perl_vendorlib}/Warewulf/Event/Dhcp.pm
%{perl_vendorlib}/Warewulf/Event/Pxe.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Pxe.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Dhcp.pm


%if "%{_arch}" == "x86_64"
%package server-ipxe-x86_64
Summary: Warewulf - iPXE Bootloader for x86_64
BuildArch: noarch

%description server-ipxe-x86_64
Warewulf is an operating system management toolkit designed to facilitate
large scale deployments of systems on physical, virtual and cloud-based
infrastructures. It facilitates elastic and large deployments consisting
of groups of homogeneous systems.

This package provides bundled iPXE binaries for x86_64.

%files server-ipxe-x86_64
%dir %{_datadir}/warewulf/ipxe
%{_datadir}/warewulf/ipxe/bin-i386-efi
%{_datadir}/warewulf/ipxe/bin-i386-pcbios
%{_datadir}/warewulf/ipxe/bin-x86_64-efi
%endif


%if "%{_arch}" == "aarch64"
%package server-ipxe-aarch64
Summary: Warewulf - iPXE Bootloader for aarch64
BuildArch: noarch

%description server-ipxe-aarch64
Warewulf is an operating system management toolkit designed to facilitate
large scale deployments of systems on physical, virtual and cloud-based
infrastructures. It facilitates elastic and large deployments consisting
of groups of homogeneous systems.

This package provides bundled iPXE binaries for aarch64.

%files server-ipxe-aarch64
%dir %{_datadir}/warewulf/ipxe
%{_datadir}/warewulf/ipxe/bin-arm64-efi
%endif


%package gpl_sources
Summary: Warewulf - GPL sources used in Warewulf provisioning
License: GPL+
Requires: %{name} = %{version}-%{release}
BuildArch: noarch

%description gpl_sources
Warewulf is an operating system management toolkit designed to facilitate
large scale deployments of systems on physical, virtual and cloud-based
infrastructures. It facilitates elastic and large deployments consisting
of groups of homogeneous systems.

For user convenience, Warewulf is distributed with some third-party
software.  While Warewulf itself is licensed under a DOE license
(a derivative of the BSD license), the third-party software may have
different licensing terms. To be fully compliant to the GPL open source
license, GPL source files are included in this package.

%files gpl_sources
%dir %{_prefix}/src/warewulf
%dir %{_prefix}/src/warewulf/3rd_party
%{_prefix}/src/warewulf/3rd_party/GPL/
