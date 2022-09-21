#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Don't try to compile python files with /usr/bin/python
%{?el7:%global __python %__python3}

%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname charliecloud

# Specify python version of a given file
%define versionize_script() (sed -i 's,/env python,/env %1,g' %2)

%{!?build_ldflags:%global build_ldflags %nil}

Summary:   Lightweight user-defined software stacks for high-performance computing
Name:      %{pname}%{PROJ_DELIM}
Version:   0.15
Release:   1%{?dist}
License:   Apache-2.0
Group:     %{PROJ_NAME}/runtimes
URL:       https://hpc.github.io/%{pname}/
Source0:   https://github.com/hpc/charliecloud/releases/download/v%{version}/charliecloud-%{version}.tar.gz
Source1:   Build

BuildRequires: gcc
%if 0%{?centos_version} || 0%{?rhel_version}
BuildRequires: python36
%endif
%if 0%{?sles_version} || 0%{?suse_version} || 0%{?openEuler}
BuildRequires: python3
%endif

Requires:  %{name}%{?_isa} = %{version}-%{release}
Requires:  bash
Requires:  wget
%if 0%{?centos_version} || 0%{?rhel_version}
Requires:  python36
%endif
%if 0%{?sles_version} || 0%{?suse_version} || 0%{?openEuler}
Requires: python3
%endif

# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

# libexec path
%define _libexecdir %{install_path}/libexec

%description
Charliecloud uses Linux user namespaces to run containers with no privileged
operations or daemons and minimal configuration changes on center resources.
This simple approach avoids most security risks while maintaining access to
the performance and functionality already on offer.

Container images can be built using Docker or anything else that can generate
a standard Linux filesystem tree.

For more information: https://hpc.github.io/charliecloud/

%prep
%setup -q -n %{pname}-%{version}
%{versionize_script python3 test/docs-sane}
%{versionize_script python3 test/make-perms-test}

%build
./configure --prefix=%{install_path}
CFLAGS="-std=c11 -fPIC -pthread" LDFLAGS="%build_ldflags" %{__make} %{?mflags}


%install
PREFIX=%{install_path} DESTDIR=$RPM_BUILD_ROOT %{__make} install %{?mflags_install}

%{__mkdir_p} %{buildroot}/%{install_path}/share/doc/charliecloud/test/chtest/
%{__cp} %{SOURCE1} %{buildroot}/%{install_path}/share/doc/charliecloud/test/chtest/Build

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

        puts stderr " "
        puts stderr "This module loads the %{pname} utility"
        puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: runtime"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

cat > README.EL7 <<EOF
For RHEL7 you must increase the number of available user namespaces to a non-
zero number (note the number below is taken from the default for RHEL8):

  echo user.max_user_namespaces=3171 >/etc/sysctl.d/51-userns.conf
  reboot

Note for versions below RHEL7.6, you will also need to enable user namespaces:

  grubby --args=namespace.unpriv_enable=1 --update-kernel=ALL
  reboot
EOF

cat > README.TEST <<EOF
Charliecloud comes with a fairly comprehensive Bats test suite. For testing
instructions visit: https://hpc.github.io/charliecloud/test.html
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%doc LICENSE README.rst README.TEST %{?el7:README.EL7}
%{OHPC_PUB}
