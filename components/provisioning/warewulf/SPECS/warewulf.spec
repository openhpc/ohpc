#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

## Inserted by OHPC
%include %{_sourcedir}/OHPC_macros

%global debug_package %{nil}

%global api 0

# Base package name
%global pname warewulf

# Group for warewulfd and other WW operations
%global wwgroup warewulf

%if 0%{?fedora}
%define _build_id_links none
%endif

# Service directories (change /var/lib/* default to match WW3 build)
%global tftpdir /srv/tftpboot
%global srvdir /srv
%global statedir /srv

Name:    %{pname}%{PROJ_DELIM}
Summary: A provisioning system for large clusters of bare metal and/or virtual systems
Version: 4.6.0
Release: 1%{?dist}
License: BSD-3-Clause
Group:   %{PROJ_NAME}/provisioning
URL:     https://github.com/warewulf/warewulf
Source0: https://github.com/warewulf/warewulf/releases/download/v%{version}/warewulf-%{version}.tar.gz

ExclusiveOS: linux

Conflicts: warewulf < 4
Conflicts: warewulf-common
Conflicts: warewulf-cluster
Conflicts: warewulf-vnfs
Conflicts: warewulf-provision
Conflicts: warewulf-ipmi

%if 0%{?suse_version} || 0%{?sle_version}
BuildRequires: distribution-release
BuildRequires: systemd-rpm-macros
BuildRequires: go > 1.20
BuildRequires: firewall-macros
BuildRequires: firewalld
BuildRequires: tftp
BuildRequires: yq
Requires: tftp
Requires: nfs-kernel-server
Requires: firewalld
Requires: ipxe-bootimgs
%else
# Assume Red Hat/Fedora build
BuildRequires: system-release
BuildRequires: systemd
BuildRequires: golang > 1.20
BuildRequires: firewalld-filesystem
Requires: tftp-server
Requires: nfs-utils
%if 0%{?rhel} < 8
Requires: ipxe-bootimgs
%else
Requires: ipxe-bootimgs-x86
Requires: ipxe-bootimgs-aarch64
%endif
%endif

%if 0%{?rhel} >= 8 || 0%{?suse_version} || 0%{?fedora}
Requires: dhcp-server
%else
# rhel < 8
Requires: dhcp
%endif

BuildRequires: git
BuildRequires: make
BuildRequires: gpgme-devel
%if %{api}
BuildRequires: libassuan-devel
%endif

Recommends: logrotate
Recommends: ipmitool

%description
Warewulf is a stateless and diskless provisioning
system for large clusters of bare metal and/or virtual systems.

%package dracut
Summary: dracut module for loading a Warewulf image
BuildArch: noarch

Requires: dracut
%if 0%{?suse_version}
%else
Requires: dracut-network
%endif
Requires: curl
Requires: cpio
Requires: dmidecode

%description dracut
Warewulf is a stateless and diskless provisioning
system for large clusters of bare metal and/or virtual systems.

This subpackage contains a dracut module that can be used to generate
an initramfs that can fetch and boot a Warewulf node image from a
Warewulf server.

%prep
%setup -q -n %{pname}-%{version} -b0 %if %{?with_offline:-a2}

%build
export OFFLINE_BUILD=1
make defaults \
    PREFIX=%{_prefix} \
    BINDIR=%{_bindir} \
    SYSCONFDIR=%{_sysconfdir} \
    DATADIR=%{_datadir} \
    LOCALSTATEDIR=%{statedir} \
    SHAREDSTATEDIR=%{statedir} \
    MANDIR=%{_mandir} \
    INFODIR=%{_infodir} \
    DOCDIR=%{_docdir} \
    SRVDIR=%{srvdir} \
    TFTPDIR=%{tftpdir} \
    SYSTEMDDIR=%{_unitdir} \
    BASHCOMPDIR=/etc/bash_completion.d/ \
    FIREWALLDDIR=/usr/lib/firewalld/services \
    WWCLIENTDIR=/warewulf \
    IPXESOURCE=/usr/share/ipxe \
    DRACUTMODDIR=/usr/lib/dracut/modules.d \
    CACHEDIR=%{_localstatedir}/cache
make build
%if %{api}
make api
%endif


%install
export OFFLINE_BUILD=1
export NO_BRP_STALE_LINK_ERROR=yes
make install \
    DESTDIR=%{buildroot}
%if %{api}
make installapi \
    DESTDIR=%{buildroot}
%endif

## Inserted by OHPC
# For RH, tftpboot directory is hardcoded
%if 0%{?rhel}
ln -s %{_sharedstatedir}/tftpboot %{buildroot}%{tftpdir}
%endif

%if 0%{?suse_version} || 0%{?sle_version}
yq e '
  .tftp.ipxe."00:00" = "undionly.kpxe" |
  .tftp.ipxe."00:07" = "ipxe-x86_64.efi" |
  .tftp.ipxe."00:09" = "ipxe-x86_64.efi" |
  .tftp.ipxe."00:0B" = "snp-arm64.efi" ' \
  -i %{buildroot}%{_sysconfdir}/warewulf/warewulf.conf
%endif


%pre
getent group %{wwgroup} >/dev/null || groupadd -r %{wwgroup}
# use ipxe images from the distribution


%post
%systemd_post warewulfd.service
%firewalld_reload


%preun
%systemd_preun warewulfd.service


%postun
%systemd_postun_with_restart warewulfd.service
%firewalld_reload


%files
%defattr(-, root, root)

%dir %{_sysconfdir}/warewulf
%config(noreplace) %{_sysconfdir}/warewulf/warewulf.conf
%config(noreplace) %attr(0640,-,%{wwgroup}) %{_sysconfdir}/warewulf/nodes.conf
%config(noreplace) %{_sysconfdir}/warewulf/examples
%config(noreplace) %{_sysconfdir}/warewulf/ipxe
%config(noreplace) %{_sysconfdir}/warewulf/grub
%{_sysconfdir}/bash_completion.d
%config(noreplace) %{_sysconfdir}/logrotate.d

%dir %{statedir}/warewulf
%dir %{statedir}/warewulf/chroots
%dir %{statedir}/warewulf/overlays

%dir %{_datadir}/warewulf
%{_datadir}/warewulf/bmc
%dir %{_datadir}/warewulf/overlays
%dir %{_datadir}/warewulf/overlays/*
%dir %{_datadir}/warewulf/overlays/*/rootfs
%{_datadir}/warewulf/overlays/NetworkManager/rootfs/*
%{_datadir}/warewulf/overlays/debian.interfaces/rootfs/*
%{_datadir}/warewulf/overlays/debug/rootfs/*
%{_datadir}/warewulf/overlays/fstab/rootfs/*
%{_datadir}/warewulf/overlays/host/rootfs/*
%{_datadir}/warewulf/overlays/hostname/rootfs/*
%{_datadir}/warewulf/overlays/hosts/rootfs/*
%{_datadir}/warewulf/overlays/ifcfg/rootfs/*
%{_datadir}/warewulf/overlays/ignition/rootfs/*
%{_datadir}/warewulf/overlays/issue/rootfs/*
%{_datadir}/warewulf/overlays/netplan/rootfs/*
%{_datadir}/warewulf/overlays/resolv/rootfs/*
%attr(700, -, -) %{_datadir}/warewulf/overlays/ssh.authorized_keys/rootfs/*
%{_datadir}/warewulf/overlays/ssh.host_keys/rootfs/*
%{_datadir}/warewulf/overlays/syncuser/rootfs/*
%{_datadir}/warewulf/overlays/systemd.netname/rootfs/*
%{_datadir}/warewulf/overlays/udev.netname/rootfs/*
%{_datadir}/warewulf/overlays/wicked/rootfs/*
%{_datadir}/warewulf/overlays/wwclient/rootfs/*
%{_datadir}/warewulf/overlays/wwinit/rootfs/*
%{_datadir}/warewulf/overlays/localtime/rootfs/*

%{_bindir}/wwctl
%{_prefix}/lib/firewalld/services/warewulf.xml
%{_unitdir}/warewulfd.service
%{_mandir}/man1/wwctl*
%{_mandir}/man5/*.5*

%dir %{_docdir}/warewulf
%license %{_docdir}/warewulf/LICENSE.md

%if %{api}
%{_bindir}/wwapi*
%config(noreplace) %{_sysconfdir}/warewulf/wwapi*.conf
%endif

## Inserted by OHPC
%if 0%{?rhel}
%{tftpdir}
%endif

%files dracut
%defattr(-, root, root)
%{_prefix}/lib/dracut/modules.d/90wwinit
