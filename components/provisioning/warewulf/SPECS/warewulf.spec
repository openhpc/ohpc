#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

## OHPC: add; macros
%include %{_sourcedir}/OHPC_macros
%global pname warewulf
## OHPC: end

%global debug_package %{nil}

# feature macros
%if 0%{?rhel} >= 10 || 0%{?openEuler}
%global use_dnsmasq 1
%else
%global use_dnsmasq 0
%endif

%if 0%{?suse_version} || 0%{?sle_version}
%global is_suse 1
%else
%global is_suse 0
%endif

# Set tftpdir based on distribution
# NOTE: 4.6.x OHPC unconditionally used /srv/tftpboot on all distros including openEuler.
# 4.7.0 aligns with upstream: RHEL gets /var/lib/tftpboot (real dir) + /srv/tftpboot compat symlink;
# openEuler moves from /srv/tftpboot (real dir) to /var/lib/tftpboot — upgrade requires moving directories
%if 0%{?is_suse}
%global tftpdir /srv/tftpboot
%else
%global tftpdir /var/lib/tftpboot
%endif

%global srvdir %{_sharedstatedir}
## OHPC: add; state dirs use /srv (OHPC convention) rather than upstream /var/lib
%global statedir /srv

%global wwgroup warewulf

%if 0%{?fedora}
%define _build_id_links none
%endif

%define _overlaydir %{_datadir}/warewulf/overlays
%global __brp_mangle_shebangs_exclude_from ^%{_overlaydir}/.*$


## OHPC: edit-block; Name uses pname+delimiter; Group added
Name:    %{pname}%{PROJ_DELIM}
Summary: A provisioning system for large clusters of bare metal and/or virtual systems
Version: 4.7.0
Release: 1%{?dist}
License: BSD-3-Clause
Group:   %{PROJ_NAME}/provisioning
URL:     https://github.com/warewulf/warewulf
Source0: https://github.com/warewulf/warewulf/releases/download/v%{version}/warewulf-%{version}.tar.gz
# OpenHPC modification: add .localdomain suffix to hosts.ww template
Patch0:  hosts.ww.patch
## OHPC: end

ExclusiveOS: linux

Conflicts: warewulf < 4
Conflicts: warewulf-common
Conflicts: warewulf-cluster
Conflicts: warewulf-vnfs
Conflicts: warewulf-provision
Conflicts: warewulf-ipmi

%if 0%{?is_suse}
## OHPC: removed; not available in OHPC build infrastructure
#BuildRequires: distribution-release
BuildRequires: systemd-rpm-macros
BuildRequires: go >= 1.22
BuildRequires: firewall-macros
BuildRequires: firewalld
Requires: nfs-kernel-server
Requires: firewalld
Requires: ipxe-bootimgs
%else
# Assume Red Hat/Fedora
## OHPC: removed; not available in OHPC build infrastructure
#BuildRequires: system-release
BuildRequires: systemd
BuildRequires: golang >= 1.22
BuildRequires: firewalld-filesystem
Requires: nfs-utils
## OHPC: edit; openEuler ships a single ipxe-bootimgs package like pre-RHEL8
%if 0%{?rhel} < 8 || 0%{?openEuler}
Requires: ipxe-bootimgs
%else
Requires: ipxe-bootimgs-x86
Requires: ipxe-bootimgs-aarch64
%endif
%endif

# dhcp/tftp requirements
%if 0%{?is_suse}
BuildRequires: tftp
Requires: tftp
%else
%if 0%{?use_dnsmasq}
Requires: dnsmasq
%else
# Assume Red Hat/Fedora
Requires: tftp-server
%if 0%{?rhel} >= 8 || 0%{?fedora}
Requires: dhcp-server
%else
Requires: dhcp
%endif
%endif
%endif

BuildRequires: git
BuildRequires: make
BuildRequires: gpgme-devel
BuildRequires: python3-devel

Recommends: logrotate
Recommends: ipmitool


%description
Warewulf is a stateless and diskless provisioning
system for large clusters of bare metal and/or virtual systems.


%prep
## OHPC: edit; pname used in setup because %%{name} expands to warewulf-ohpc
%setup -q -n %{pname}-%{version} -b0
%patch -P 0 -p1


%build
export OFFLINE_BUILD=1
## OHPC: edit; statedir is /srv rather than upstream %%{_sharedstatedir} (/var/lib)
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
    SOSPLUGINS=%{python3_sitelib}/sos/report/plugins \
    CACHEDIR=%{_localstatedir}/cache
make build


%install
export OFFLINE_BUILD=1
export NO_BRP_STALE_LINK_ERROR=yes
make install \
    DESTDIR=%{buildroot}

## OHPC: add-block; /srv/tftpboot compat symlink pointing to /var/lib/tftpboot on RHEL
%if 0%{?rhel}
ln -s %{_sharedstatedir}/tftpboot %{buildroot}/srv/tftpboot
%endif
## OHPC: end

%if 0%{?rhel} >= 10 || 0%{?openEuler}
cp -f etc/warewulf.conf-el10 %{buildroot}%{_sysconfdir}/warewulf/warewulf.conf
%else
%if 0%{?is_suse}
cp -f etc/warewulf.conf-suse %{buildroot}%{_sysconfdir}/warewulf/warewulf.conf
%endif
%endif

%if ! 0%{?is_suse}
make install-sos \
    DESTDIR=%{buildroot}
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
%config(noreplace) %attr(0600,-,-) %{_sysconfdir}/warewulf/auth.conf
%config(noreplace) %{_sysconfdir}/warewulf/examples
%config(noreplace) %{_sysconfdir}/warewulf/ipxe
%config(noreplace) %{_sysconfdir}/warewulf/grub
%{_sysconfdir}/bash_completion.d
%config(noreplace) %{_sysconfdir}/logrotate.d

## OHPC: edit-block; s/_sharedstatedir/statedir/; statedir is /srv
%dir %{statedir}/warewulf
%dir %{statedir}/warewulf/chroots
%dir %{statedir}/warewulf/overlays
## OHPC: end

%dir %{_datadir}/warewulf
%{_datadir}/warewulf/bmc
%dir %{_overlaydir}
%dir %{_overlaydir}/*
%dir %{_overlaydir}/*/rootfs
%{_overlaydir}/NetworkManager/rootfs/*
%{_overlaydir}/chrony/rootfs/*
%{_overlaydir}/debian.interfaces/rootfs/*
%{_overlaydir}/debug/rootfs/*
%{_overlaydir}/fstab/rootfs/*
%{_overlaydir}/host/rootfs/*
%{_overlaydir}/hostname/rootfs/*
%{_overlaydir}/hosts/rootfs/*
%{_overlaydir}/ifcfg/rootfs/*
%{_overlaydir}/ifupdown/rootfs/*
%{_overlaydir}/ignition/rootfs/*
%{_overlaydir}/issue/rootfs/*
%{_overlaydir}/netplan/rootfs/*
%{_overlaydir}/resolv/rootfs/*
%attr(700, -, -) %{_overlaydir}/ssh.authorized_keys/rootfs/*
%{_overlaydir}/ssh.host_keys/rootfs/*
%{_overlaydir}/syncuser/rootfs/*
%{_overlaydir}/systemd.netname/rootfs/*
%{_overlaydir}/systemd.networkd/rootfs/*
%{_overlaydir}/udev.netname/rootfs/*
%{_overlaydir}/wicked/rootfs/*
%{_overlaydir}/wwclient/rootfs/*
%{_overlaydir}/wwclient.x86_64/rootfs/*
%{_overlaydir}/wwclient.aarch64/rootfs/*
%{_overlaydir}/wwinit/rootfs/*
%{_overlaydir}/localtime/rootfs/*
%{_overlaydir}/sfdisk/rootfs/*
%{_overlaydir}/mkfs/rootfs/*
%{_overlaydir}/mkswap/rootfs/*
%{_overlaydir}/systemd.mount/rootfs/*
%{_overlaydir}/systemd.swap/rootfs/*
%{_overlaydir}/mig/rootfs/*

%{_bindir}/wwctl
%{_prefix}/lib/firewalld/services/warewulf.xml
%{_unitdir}/warewulfd.service
%{_mandir}/man1/wwctl*
%{_mandir}/man5/*.5*

%dir %{_docdir}/warewulf
%license %{_docdir}/warewulf/LICENSE.md

## OHPC: /srv/tftpboot compat symlink owned by package on RHEL
%if 0%{?rhel}
/srv/tftpboot
%endif


%package dracut
Summary: dracut module for loading a Warewulf image
BuildArch: noarch

Requires: dracut
%if ! 0%{?is_suse}
Requires: dracut-network
%endif
Requires: curl
Requires: cpio
Requires: dmidecode

%description dracut
Warewulf is a stateless and diskless provisioning system for large clusters of
bare metal and/or virtual systems.

This subpackage contains a dracut module that can be used to generate an
initramfs that can fetch and boot a Warewulf OS image from a Warewulf server.

%files dracut
%defattr(-, root, root)
%{_prefix}/lib/dracut/modules.d/90wwinit


%if ! 0%{?is_suse}
%package sos
Summary: sos plugin for Warewulf
BuildArch: noarch
Requires: sos

%description sos
Warewulf is a stateless and diskless provisioning system for large clusters of
bare metal and/or virtual systems.

This subpackage contains an sos module that can be used to include information
about Warewulf in an sos report.

%files sos
%{python3_sitelib}/sos/report/plugins/warewulf.py
%{python3_sitelib}/sos/report/plugins/__pycache__/warewulf.*.pyc
%endif

## OHPC: delete; upstream %%changelog removed
