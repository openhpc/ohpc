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

%global debug_package %{nil}

# Base package name
%global pname warewulf

# Group for warewulfd and other WW operations
%global wwgroup warewulf

# Service directories (change /var/lib/* default to match WW3 build)
%global tftpdir /srv/tftpboot
%global srvdir /srv
%global statedir /srv

Name:    %{pname}%{PROJ_DELIM}
Summary: A provisioning system for large clusters of bare metal and/or virtual systems
Version: 4.5.5
Release: 1%{?dist}
License: BSD-3-Clause
Group:   %{PROJ_NAME}/provisioning
URL:     https://github.com/hpcng/warewulf
Source0: https://github.com/hpcng/warewulf/releases/download/v%{version}/warewulf-%{version}.tar.gz
Patch0:  warewulf-4.5.x-sle_ipxe.patch

ExclusiveOS: linux

Conflicts: warewulf < 4
Conflicts: warewulf-common
Conflicts: warewulf-cluster
Conflicts: warewulf-vnfs
Conflicts: warewulf-provision
Conflicts: warewulf-ipmi

BuildRequires: make
BuildRequires: git
BuildRequires: libassuan-devel
BuildRequires: gpgme-devel
BuildRequires: unzip
Requires: dhcp-server

%if 0%{?suse_version} || 0%{?sle_version}
BuildRequires: systemd-rpm-macros
BuildRequires: go > 1.20
BuildRequires: firewall-macros
BuildRequires: firewalld
BuildRequires: tftp
Requires: tftp
Requires: nfs-kernel-server
Requires: firewalld
Requires: ipxe-bootimgs
%else
# Assume Fedora-based OS (>= RHEL 9) if not SUSE-based
BuildRequires: systemd
BuildRequires: gcc
BuildRequires: golang > 1.20
BuildRequires: firewalld-filesystem
Requires: tftp-server
Requires: nfs-utils
Requires: ipxe-bootimgs-x86
Requires: ipxe-bootimgs-aarch64
%endif

%description
Warewulf is a stateless and diskless container operating system provisioning
system for large clusters of bare metal and/or virtual systems.


%prep
%setup -q -n %{pname}-%{version}
%if 0%{?suse_version} || 0%{?sle_version}
%patch0 -p1
%endif


%build
export OFFLINE_BUILD=1
# Install to sharedstatedir by redirecting LOCALSTATEDIR
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
    WWCLIENTDIR=/warewulf
    IPXESOURCE=/usr/share/ipxe
make
make api


%install
export NO_BRP_STALE_LINK_ERROR=yes
export OFFLINE_BUILD=1
make install DESTDIR=%{buildroot}
make installapi DESTDIR=%{buildroot}

# For RH, tftpboot directory is hardcoded
%if 0%{?rhel}
ln -s %{_sharedstatedir}/tftpboot %{buildroot}%{tftpdir}
%endif


%pre
getent group %{wwgroup} >/dev/null || groupadd -r %{wwgroup}


%post
%systemd_post warewulfd.service
%firewalld_reload


%preun
%systemd_preun warewulfd.service


%postun
%systemd_postun_with_restart warewulfd.service
%firewalld_reload


%files
%defattr(-, root, %{wwgroup})
%dir %{_sysconfdir}/warewulf
%config(noreplace) %{_sysconfdir}/warewulf/warewulf.conf
%dir %{_sysconfdir}/warewulf/examples
%config(noreplace) %{_sysconfdir}/warewulf/examples/*.ww
%dir %{_sysconfdir}/warewulf/ipxe
%config(noreplace) %{_sysconfdir}/warewulf/ipxe/*.ipxe
%dir %{_sysconfdir}/warewulf/grub
%config(noreplace) %{_sysconfdir}/warewulf/grub/*.ww
%config(noreplace) %attr(0640,-,-) %{_sysconfdir}/warewulf/nodes.conf
%{_sysconfdir}/bash_completion.d/wwctl

%dir %{statedir}/warewulf
%dir %{srvdir}/warewulf
%{statedir}/warewulf/chroots
%dir %{statedir}/warewulf/overlays
%dir %{statedir}/warewulf/overlays/*
%attr(-, root, root) %{statedir}/warewulf/overlays/*/rootfs

%if 0%{?rhel}
%{tftpdir}
%endif

%attr(-, root, root) %{_bindir}/wwctl
%attr(-, root, root) %{_prefix}/lib/firewalld/services/warewulf.xml
%attr(-, root, root) %{_unitdir}/warewulfd.service
%attr(-, root, root) %{_mandir}/man1/wwctl*
%attr(-, root, root) %{_mandir}/man5/*.5*
%attr(-, root, root) %{_datadir}/warewulf

%dir %{_docdir}/warewulf
%license %{_docdir}/warewulf/LICENSE.md

%attr(-, root, root) %{_bindir}/wwapi*
%config(noreplace) %{_sysconfdir}/warewulf/wwapi*.conf

# ===========================================================
%define dracut_package %{pname}-dracut%{PROJ_DELIM}

%package -n %{dracut_package}
Summary: A dracut module for loading a Warewulf container image
BuildArch: noarch

Requires: dracut
%if 0%{?suse_version} || 0%{?sle_version}
%else
Requires: dracut-network
%endif

%description -n %{dracut_package}
Warewulf is a stateless and diskless container operating system provisioning
system for large clusters of bare metal and/or virtual systems.

This subpackage contains a dracut module that can be used to generate
an initramfs that can fetch and boot a Warewulf container image from a
Warewulf server.


%files -n %{dracut_package}
%defattr(-, root, root)
%dir %{_prefix}/lib/dracut/modules.d/90wwinit
%{_prefix}/lib/dracut/modules.d/90wwinit/*.sh
