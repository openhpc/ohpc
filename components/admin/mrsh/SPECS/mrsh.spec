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

%define pname mrsh


Name:    %{pname}%{PROJ_DELIM}
Version: 2.12
Release: 1%{?dist}
Epoch: 3
Summary: Remote shell program that uses munge authentication
License: none
Group: %{PROJ_NAME}/admin
URL: https://github.com/chaos/mrsh
Source:    https://github.com/chaos/mrsh/archive/%{version}.tar.gz#/mrsh-%{version}.tar.gz
Patch1: mrsh-pam-suse.patch
Patch2: mrlogin-Don-t-use-union-wait.patch
Patch3: Add-force-to-libtoolize.patch
BuildRequires: ncurses-devel pam-devel munge-devel
Requires: munge
Provides: mrsh
BuildRequires:  systemd-devel

# support re-run of autogen
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: libtool

%define _prefix %{OHPC_HOME}/admin/%{pname}

%description
Remote shell programs that use munge authentication rather than
reserved ports for security.

%package -n %{pname}-server%{PROJ_DELIM}
Summary: Servers for remote access commands (mrsh, mrlogin, mrcp)
Group: System Environment/Daemons
Requires(post): systemd
%description -n %{pname}-server%{PROJ_DELIM}
Server daemons for remote access commands (mrsh, mrlogin, mrcp)

%package -n %{pname}-rsh-compat%{PROJ_DELIM}
Summary: rsh compatability package for mrcp/mrlogin/mrsh
Group: System Environment/Base
Requires: mrsh
Provides: rsh
%description -n %{pname}-rsh-compat%{PROJ_DELIM}
rsh compatability package for mrcp/mrlogin/mrsh

%prep
%setup -q -n %{pname}-%{version}
%if 0%{?suse_version}
%patch1 -p1
%endif
%patch2 -p1
%patch3 -p1
./autogen.sh

%build
%configure %{?_without_pam}
make

%install
DESTDIR="%{buildroot}" make install

%{__mkdir_p} %{buildroot}/usr/bin
ln -sf %{_prefix}/bin/mrcp %{buildroot}/usr/bin/
ln -sf %{_prefix}/bin/mrsh %{buildroot}/usr/bin/
ln -sf %{_prefix}/bin/mrlogin %{buildroot}/usr/bin/

ln -sf %{_prefix}/bin/rcp %{buildroot}/usr/bin/
ln -sf %{_prefix}/bin/rsh %{buildroot}/usr/bin/
ln -sf %{_prefix}/bin/rlogin %{buildroot}/usr/bin/

ln -sf in.mrlogind %{buildroot}%{_sbindir}/in.rlogind
ln -sf in.mrshd %{buildroot}%{_sbindir}/in.rshd

for i in mrsh mrlogin
do
    sed -i 's#\(account\s\+include\s\+\)system-auth#\1common-account#' %{buildroot}/%{_sysconfdir}/pam.d/$i
    sed -i 's#\(session\s\+include\s\+\)system-auth#\1common-session#' %{buildroot}/%{_sysconfdir}/pam.d/$i
done


%files
%doc NEWS README ChangeLog COPYING DISCLAIMER DISCLAIMER.UC
%{_mandir}/man1/mrcp.1*
%{_mandir}/man1/mrsh.1*
%{_mandir}/man1/mrlogin.1*
%{_bindir}/mrcp
%{_bindir}/mrsh
%{_bindir}/mrlogin
/usr/bin/mr*
%dir /opt/ohpc/admin/mrsh
%dir /opt/ohpc/admin/mrsh/bin
%dir /opt/ohpc/admin/mrsh/share
%dir /opt/ohpc/admin/mrsh/share/man
%dir /opt/ohpc/admin/mrsh/share/man/man1

%files -n %{pname}-server%{PROJ_DELIM}
%if %{?_without_pam:0}%{!?_without_pam:1}
%config(noreplace) /etc/pam.d/mrsh
%config(noreplace) /etc/pam.d/mrlogin
%endif
%{_unitdir}/*
%{_mandir}/man8/in.mrlogind.8*
%{_mandir}/man8/in.mrshd.8*
%{_mandir}/man8/mrlogind.8*
%{_mandir}/man8/mrshd.8*
%{_sbindir}/*
%dir /opt/ohpc/admin/mrsh
%dir /opt/ohpc/admin/mrsh/sbin
%dir /opt/ohpc/admin/mrsh/share
%dir /opt/ohpc/admin/mrsh/share/man
%dir /opt/ohpc/admin/mrsh/share/man/man8

%files -n %{pname}-rsh-compat%{PROJ_DELIM}
%{_mandir}/man1/rcp.1*
%{_mandir}/man1/rsh.1*
%{_mandir}/man1/rlogin.1*
%{_mandir}/man8/in.rlogind.8*
%{_mandir}/man8/in.rshd.8*
%{_mandir}/man8/rlogind.8*
%{_mandir}/man8/rshd.8*
%{_bindir}/rcp
%{_bindir}/rsh
%{_bindir}/rlogin
/usr/bin/r*
%dir /opt/ohpc/admin/mrsh/share/man/man8

%post -n %{pname}-server%{PROJ_DELIM}
%service_add_post mrshd.socket mrlogind.socket
