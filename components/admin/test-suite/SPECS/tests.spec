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

Summary:   Integration test suite for OpenHPC
Name:      test-suite%{PROJ_DELIM}
Version:   3.2.0
Release:   1
License:   Apache-2.0
Group:     %{PROJ_NAME}/admin
BuildArch: noarch
URL:       https://github.com/openhpc/ohpc
Source0:   tests-ohpc.tar

BuildRequires:  perl(File::Copy)
BuildRequires:  perl(File::Compare)
BuildRequires:  autoconf%{PROJ_DELIM}
BuildRequires:  automake%{PROJ_DELIM}

Requires: which bats

%global __brp_mangle_shebangs_exclude bats
%undefine __brp_mangle_shebangs

%if 0%{?suse_version}
Requires(pre):  shadow
Requires: python3-base
%endif

%if 0%{?rhel_version}
Requires(pre):  shadow-utils
Requires: python3
%endif

%define testuser ohpc-test

%description

This package provides a suite of integration tests used by the OpenHPC project
during continuous integration. Most components can be tested individually, but
a default configuration is setup to enable collective testing. The test suite
is made available under an '%{testuser}' user account.

%prep
%setup -n tests-ohpc

%build

export PATH=/opt/ohpc/pub/utils/autotools/bin:$PATH
cd tests
./bootstrap


%install

cd tests
%{__mkdir_p} %{buildroot}/home/%{testuser}/tests
cp -a * %{buildroot}/home/%{testuser}/tests
find %{buildroot}/home/%{testuser}/tests -name .gitignore  -exec rm {} \;

%pre
getent passwd %{testuser} >/dev/null || \
    /usr/sbin/useradd -U -c "OpenHPC integration test account" \
    -s /bin/bash -m -b /home %{testuser}
%if 0%{?suse_version}
if [ $(getent group singularity) ]; then
    usermod -a -G singularity ohpc-test
fi
%endif
exit 0

%files
%defattr(-,%{testuser},%{testuser},-)
%dir /home/%{testuser}
/home/%{testuser}/tests
