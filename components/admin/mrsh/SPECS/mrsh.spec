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
Source:    https://github.com/chaos/mrsh/archive/mrsh-2-7-1.tar.gz
Patch0: null-terminate.patch
Patch1: mrsh-pam-suse.patch
BuildRequires: ncurses-devel pam-devel munge-devel%{PROJ_DELIM}
Requires: munge%{PROJ_DELIM} >= 0.1-0
Provides: mrsh

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
Requires: xinetd
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
%setup -q -n mrsh-mrsh-2-7-1
./autogen.sh
%patch0 -p1
%if 0%{?suse_version}
%patch1 -p1
%endif

%build
%configure %{?_without_pam}
make

%install
DESTDIR="$RPM_BUILD_ROOT" make install

%{__mkdir_p} ${RPM_BUILD_ROOT}/usr/bin
ln -sf %{_prefix}/bin/mrcp ${RPM_BUILD_ROOT}/usr/bin/
ln -sf %{_prefix}/bin/mrsh ${RPM_BUILD_ROOT}/usr/bin/
ln -sf %{_prefix}/bin/mrlogin ${RPM_BUILD_ROOT}/usr/bin/

ln -sf %{_prefix}/bin/rcp ${RPM_BUILD_ROOT}/usr/bin/
ln -sf %{_prefix}/bin/rsh ${RPM_BUILD_ROOT}/usr/bin/
ln -sf %{_prefix}/bin/rlogin ${RPM_BUILD_ROOT}/usr/bin/

sed -i 's#/usr/sbin/in.mrshd#/opt/ohpc/admin/mrsh/sbin/in.mrshd#' ${RPM_BUILD_ROOT}/etc/xinetd.d/mrshd
sed -i 's#/usr/sbin/in.mrlogind#/opt/ohpc/admin/mrsh/sbin/in.mrlogind#' ${RPM_BUILD_ROOT}/etc/xinetd.d/mrlogind
sed -i 's#disable\s*= yes#disable			= no#' ${RPM_BUILD_ROOT}/etc/xinetd.d/mrlogind
sed -i 's#disable\s*= yes#disable			= no#' ${RPM_BUILD_ROOT}/etc/xinetd.d/mrshd

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
%config(noreplace) /etc/xinetd.d/mrshd
%config(noreplace) /etc/xinetd.d/mrlogind
%if %{?_without_pam:0}%{!?_without_pam:1}
%config(noreplace) /etc/pam.d/mrsh
%config(noreplace) /etc/pam.d/mrlogin
%endif
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
if ! grep "^mshell" /etc/services > /dev/null; then
        echo "mshell          21212/tcp                  # mrshd" >> /etc/services
fi
if ! grep "^mlogin" /etc/services > /dev/null; then
        echo "mlogin            541/tcp                  # mrlogind" >> /etc/services
fi
if ! grep "^mrsh" /etc/securetty > /dev/null; then
        echo "mrsh" >> /etc/securetty
fi
if ! grep "^mrlogin" /etc/securetty > /dev/null; then
        echo "mrlogin" >> /etc/securetty
fi
# 'condrestart' is not portable
if [ -x /etc/init.d/xinetd ]; then
    if /etc/init.d/xinetd status | grep -q running; then
       /etc/init.d/xinetd restart
    fi
elif [ -x /bin/systemctl ]; then
    if /bin/systemctl status xinetd | grep -q running; then
       /bin/systemctl restart xinetd
    fi
fi
