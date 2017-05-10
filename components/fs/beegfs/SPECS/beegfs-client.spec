%define buildarch noarch
%define KDIR /lib/modules/3.10.0-514.el7.x86_64/build /usr/src/linux-headers-3.10.0-514.el7.x86_64
%define MAKE_CONCURRENCY 4
%define BEEGFS_VERSION 17.5-git10
%define BEEGFS_MAJOR_VERSION %(echo '%{BEEGFS_VERSION}' | cut -d . -f 1)
%define VER %(echo '%{BEEGFS_VERSION}' | cut -d - -f 1)
%define BEEGFS_RELEASE_STR %(echo '%{BEEGFS_VERSION}-' | cut -d - -f 2)
%define EPOCH %(echo '%{VER}' | cut -d . -f 1)

%define CLIENT_DIR /opt/beegfs/src/client/beegfs_client_module_%{BEEGFS_MAJOR_VERSION}
%define CLIENT_COMPAT_DIR /opt/beegfs/src/client/beegfs_client_compat_module_%{BEEGFS_MAJOR_VERSION}
%define COMPAT compat

%define is_fedora %(test -e /etc/fedora-release && echo 1 || echo 0)
%define is_redhat %(test -e /etc/redhat-release && echo 1 || echo 0)
%define is_suse %(test -e /etc/SuSE-release && echo 1 || echo 0)
%define is_mandrake %(test -e /etc/mandrake-release && echo 1 || echo 0)

%if %is_mandrake
%define disttag mdk
%endif

%if %is_suse
%define disttag suse
%define distver %(relpackage="`rpm -qf /etc/SuSE-release`"; release="`rpm -q --queryformat='%{VERSION}' $relpackage 2> /dev/null | tr . : | sed s/:.*$//g`" ; if test $? != 0 ; then release="" ; fi ; echo "$release")
%endif

%if %is_fedora
%define disttag fc
%endif

%if %is_redhat
%define disttag el
%define distver %(relpackage="`rpm -qf /etc/redhat-release`"; release="`rpm -q --queryformat='%{VERSION}' $relpackage 2> /dev/null | tr . : | sed s/:.*$//g`" ; if test $? != 0 ; then release="" ; fi ; echo "$release")
%endif

%if %{defined disttag}
%define RELEASE %{BEEGFS_RELEASE_STR}%{disttag}%{distver}
%else
%define RELEASE %{BEEGFS_RELEASE_STR}generic
%endif


#
# beegfs-client configuration RPM creator
#
Name: beegfs-client
Version: %{VER}
Release: %{RELEASE}
License: GPL v2
Group: Software/Other
Source: beegfs-client-%{BEEGFS_VERSION}.tgz
URL: http://www.beegfs.com
Vendor: Fraunhofer ITWM
BuildRoot: %{_tmppath}/beegfs-client-root
BuildArch: %{buildarch}
Summary: BeeGFS client kernel module
Epoch: %{EPOCH}

%description
This package contains binary objects of the closed source part of BeeGFS and 
open source code to allow to build the client kernel module.

# results in beegfs-client-source ('Name:' above defines the main package name)
# and '%package' is a sub-package name
%package %{COMPAT}
Group: Software/Other
Summary: BeeGFS client compat module, allows to run two different client versions.
requires: make, gcc, gcc-c++

%description %{COMPAT}
This package allows to build and to run a compatbility beegfs-client kernel module 
on a system that has a newer beegfs-client version installed.

%clean
rm -rf %{buildroot}

%prep
%setup -c

%build

%define debug_package %{nil}
%debug_package %{nil}

%install
cd build
echo "mkdir RPM_BUILD_ROOT (${RPM_BUILD_ROOT})"
mkdir -p ${RPM_BUILD_ROOT}
make    RELEASE_PATH=${RPM_BUILD_ROOT}/opt/beegfs/src/client		\
	BEEGFS_VERSION=%{BEEGFS_VERSION}					\
	KDIR="%{KDIR}"							\
	V=1 -j%{MAKE_CONCURRENCY} prepare_release 
pwd
mkdir -p ${RPM_BUILD_ROOT}/etc/beegfs
cp dist/etc/*.conf ${RPM_BUILD_ROOT}/etc/beegfs/
mkdir -p ${RPM_BUILD_ROOT}/etc/init.d/

# compat files
cp -a ${RPM_BUILD_ROOT}/%{CLIENT_DIR} ${RPM_BUILD_ROOT}/%{CLIENT_COMPAT_DIR}

compat_fstype="beegfs-%{BEEGFS_MAJOR_VERSION}"
compat_fstype=`echo ${compat_fstype} | sed -e 's/\.//'`
echo ${compat_fstype} > ${RPM_BUILD_ROOT}/%{CLIENT_COMPAT_DIR}/build/beegfs.fstype

# we use the redhat script for all rpm distros, as we now provide our own
# daemon() and killproc() function library (derived from redhat)
INITSCRIPT="dist/etc/init.d/beegfs-client.init"
install -D $INITSCRIPT ${RPM_BUILD_ROOT}/etc/init.d/beegfs-client

#install systemd unit description
install -D -m644 dist/usr/lib/systemd/system/beegfs-client.service \
	${RPM_BUILD_ROOT}/usr/lib/systemd/system/beegfs-client.service

install -D  dist/etc/default/beegfs-client ${RPM_BUILD_ROOT}/etc/default/beegfs-client

install -D ../scripts/etc/beegfs/lib/init-multi-mode.beegfs-client \
	${RPM_BUILD_ROOT}/etc/beegfs/lib/init-multi-mode.beegfs-client

install -D dist/sbin/beegfs-setup-client \
	${RPM_BUILD_ROOT}/opt/beegfs/sbin/beegfs-setup-client

%post
if [ "$1" = 1 ]
then
	output=$(systemctl is-system-running 2> /dev/null)
	if [ "$?" == 127 ]
	then
		chkconfig beegfs-client on
	elif [ "$?" == 0 ] || ( [ "$output" != "offline" ] && [ "$output" != "unknown" ] )
	then
		systemctl enable beegfs-client.service
	else
		chkconfig beegfs-client on
	fi
fi


# make the script to run autobuild
mkdir -p /var/lib/beegfs/client
touch /var/lib/beegfs/client/force-auto-build

%preun
make -C %{CLIENT_DIR}/build/ clean --silent || true

# only disable the service if this is the last installation
# (package removal and not upgrade)
if [ "$1" = 0 ]
then
	output=$(systemctl is-system-running 2> /dev/null)
	if [ "$?" == 127 ]
	then
		chkconfig beegfs-client off
	elif [ "$?" == 0 ] || ( [ "$output" != "offline" ] && [ "$output" != "unknown" ] )
	then
		systemctl disable beegfs-client.service
	else
		chkconfig beegfs-client off
	fi
fi

%preun %{COMPAT}
make -C %{CLIENT_COMPAT_DIR}/build/ clean --silent || true


%files
%defattr(-,root,root)
%config(noreplace) /etc/beegfs/*
%config(noreplace) /etc/default/beegfs-client
/etc/init.d/beegfs-client
/opt/beegfs/sbin/beegfs-setup-client
/usr/lib/systemd/system/beegfs-client.service
%{CLIENT_DIR}

%files %{COMPAT}
%defattr(-,root,root)
%{CLIENT_COMPAT_DIR}

