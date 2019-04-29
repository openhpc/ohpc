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

# Base package name
%define pname charliecloud

# libexec path
%define _libexecdir %{_prefix}/libexec

# Specify python version of a given file
%define versionize_script() (sed -i 's,/env python,/env% %1,g' %2)

Summary:   Lightweight user-defined software stacks for high-performance computing
Name:      %{pname}%{PROJ_DELIM}
Version:   0.9.9
Release:   1%{?dist}
License:   Apache-2.0
Group:     %{PROJ_NAME}/runtimes
URL:       https://hpc.github.io/%{pname}/
Source0:   https://github.com/hpc/%{pname}/releases/v%{version}.tar.gz#/%{pname}-%{version}.tar.gz

BuildRequires: gcc
BuildRequires: /usr/bin/python3

%package test
Summary:   Charliecloud examples and test suite
Requires:  %{name}%{?_isa} = %{version}-%{release}
Requires:  bats
Requires:  bash
Requires:  wget
Requires:  /usr/bin/python3

# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

%description
Charliecloud uses Linux user namespaces to run containers with no privileged
operations or daemons and minimal configuration changes on center resources.
This simple approach avoids most security risks while maintaining access to
the performance and functionality already on offer.

Container images can be built using Docker or anything else that can generate
a standard Linux filesystem tree.

For more information: https://hpc.github.io/charliecloud/

%description test
Charliecloud test suite and examples. The test suite takes advantage of
container image builders such as Docker, Skopeo, and Buildah.

%prep
%setup -q -n %{pname}-%{version}
%{versionize_script python3 test/make-auto}
%{versionize_script python3 test/make-perms-test}

%build
%{__make} %{?mflags}

%install
PREFIX=%{install_path} DESTDIR=$RPM_BUILD_ROOT %{__make} install %{?mflags_install}

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

%check
# Don't try to compile python files with /usr/bin/python
%{?el7:%global __python %__python3}

%files
%license LICENSE
%doc README.rst %{?el7:README.EL7}
%{_mandir}/man1/ch-*
%exclude %{_datadir}/doc/%{pname}

# Helper scripts and binaries
%{_libexecdir}/%{pname}/base.sh
%{_libexecdir}/%{pname}/version.sh
%{_bindir}/ch-*

%{OHPC_PUB}

%files test
%doc README.TEST
%{_libexecdir}/%{name}/examples
%{_libexecdir}/%{name}/test
%exclude %{_datadir}/doc/%{name}
