#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%global __brp_mangle_shebangs_exclude bats
%undefine __brp_mangle_shebangs

%include %{_sourcedir}/OHPC_macros

%global testuser ohpc-test
%global homedir %(sed -n "s/^\s*HOME=\s*//p" /etc/default/useradd || echo "/home" )

Summary:   Integration test suite for OpenHPC
Name:      test-suite%{PROJ_DELIM}
Version:   2.7.0
Release:   1
License:   Apache-2.0
Group:     %{PROJ_NAME}/admin
BuildArch: noarch
URL:       https://github.com/openhpc/ohpc/tests
Source0:   tests-ohpc.tar
# In the OHPC source root dir, create with "tar -cf tests-ohpc.tar tests/"

BuildRequires:  autoconf%{PROJ_DELIM}
BuildRequires:  automake%{PROJ_DELIM}
BuildRequires:  sed, findutils

%if 0%{?suse_version} || 0%{?sle_version}
Requires(pre):  shadow
Requires: python3-base
%endif

%if 0%{?rhel}
Requires(pre):  shadow-utils
Requires: python3
%endif

%description
This package provides a suite of integration tests used by the OpenHPC project
during continuous integration. Most components can be tested individually, but
a default configuration is setup to enable collective testing. The test suite
is made available under an '%{testuser}' user account.


%prep
%setup -n tests-ohpc
find . -name .gitignore  -exec rm {} \;


%build
export PATH=/opt/ohpc/pub/utils/autotools/bin:$PATH
cd tests
./bootstrap


%install
mkdir -p %{buildroot}%{homedir}/%{testuser}
cp -a tests %{buildroot}%{homedir}/%{testuser}/


%pre
getent passwd %{testuser} >/dev/null || \
    /usr/sbin/useradd -U -c "OpenHPC integration test account" \
    -s /bin/bash -m -b %{homedir} %{testuser}
if [ $(getent group singularity) ]; then
    usermod -a -G singularity %{testuser}
fi
if [ $(getent group geopm) ]; then
    usermod -a -G geopm %{testuser}
fi


%files
%defattr(-,%{testuser},%{testuser},-)
%{homedir}/%{testuser}/tests
