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

# Service directories (normally defaults to /var/lib/*)
%global tftpdir /srv/tftpboot
%global srvdir /srv
%global statedir /srv

Name:    %{pname}%{PROJ_DELIM}
Summary: A provisioning system for large clusters of bare metal and/or virtual systems
Version: 4.4.0
Release: 1%{?dist}
License: BSD-3-Clause
Group:   %{PROJ_NAME}/provisioning
URL:     https://github.com/hpcng/warewulf
Source0: https://github.com/hpcng/warewulf/releases/download/v%{version}/warewulf-%{version}.tar.gz

ExclusiveOS: linux

Conflicts: warewulf < 4
Conflicts: warewulf-common
Conflicts: warewulf-cluster
Conflicts: warewulf-vnfs
Conflicts: warewulf-provision
Conflicts: warewulf-ipmi

BuildRequires: make
BuildRequires: libassuan-devel
BuildRequires: gpgme-devel
Requires: dhcp-server

%if 0%{?suse_version} || 0%{?sle_version}
BuildRequires: distribution-release
BuildRequires: systemd-rpm-macros
BuildRequires: go > 1.16
BuildRequires: firewall-macros
BuildRequires: firewalld
BuildRequires: tftp
Requires: tftp
Requires: nfs-kernel-server
Requires: firewalld
%else
# Assume Fedora-based OS if not SUSE-based
BuildRequires: system-release
BuildRequires: systemd
BuildRequires: golang > 1.16
BuildRequires: firewalld-filesystem
Requires: tftp-server
Requires: nfs-utils
%endif

%description
Warewulf is a stateless and diskless container operating system provisioning
system for large clusters of bare metal and/or virtual systems.


%prep
%setup -q -n %{pname}-%{version}

# No network access in OBS, so module downloads will break builds
%if !0%{?update_mods}
sed -i "s/go mod tidy .*$//" Makefile
sed -i "s/go mod vendor .*$//" Makefile
%endif


%build
# Install to sharedstatedir by redirecting LOCALSTATEDIR
make genconfig \
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
make


%install
export NO_BRP_STALE_LINK_ERROR=yes
make install DESTDIR=%{buildroot}

# For SUSE, move dhcp.conf.ww to replace symlink
%if 0%{?suse_version} || 0%{?sle_version}
rm %{buildroot}%{statedir}/warewulf/overlays/host/etc/dhcpd.conf
mv %{buildroot}%{statedir}/warewulf/overlays/host/etc/dhcp/dhcpd.conf.ww \
    %{buildroot}%{statedir}/warewulf/overlays/host/etc/
%endif

# For RH, tftpboot directory is hardcoded
%if 0%{?rhel}
ln -s %{_sharedstatedir}/tftpboot %{buildroot}%{tftpdir}
%endif

# Remove wwapi until the next release
rm -f %{buildroot}%{_bindir}/wwapi*
rm -f %{buildroot}%{_sysconfdir}/warewulf/wwapi*


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
%config(noreplace) %{_sysconfdir}/warewulf/*
%config(noreplace) %attr(0640,-,-) %{_sysconfdir}/warewulf/nodes.conf
%{_sysconfdir}/bash_completion.d/warewulf

%dir %{statedir}/warewulf
%{statedir}/warewulf/chroots
%{statedir}/warewulf/overlays
%{srvdir}/warewulf

%if 0%{?rhel}
%{tftpdir}
%endif

%attr(-, root, root) %{_bindir}/wwctl
%attr(-, root, root) %{_prefix}/lib/firewalld/services/warewulf.xml
%attr(-, root, root) %{_unitdir}/warewulfd.service
%attr(-, root, root) %{_mandir}/man1/wwctl*
%attr(-, root, root) %{_mandir}/man5/defaults.conf*
%attr(-, root, root) %{_mandir}/man5/nodes.conf*
%attr(-, root, root) %{_mandir}/man5/warewulf.conf*
%attr(-, root, root) %{_datadir}/warewulf

%dir %{_docdir}/warewulf
%license %{_docdir}/warewulf/LICENSE.md
