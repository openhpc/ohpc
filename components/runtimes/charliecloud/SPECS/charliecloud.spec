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
Version:   0.40
Release:   1%{?dist}
License:   Apache-2.0
Group:     %{PROJ_NAME}/runtimes
URL:       https://charliecloud.io/
Source0:   https://gitlab.com/charliecloud/charliecloud/-/package_files/204172900/download#/charliecloud-%{version}.tar.gz
Source1:   Build

BuildRequires: make
BuildRequires: gcc
BuildRequires: python3

Requires: %{name}%{?_isa} = %{version}-%{release}
Requires: bash
Requires: wget
Requires: python3

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

For more information: %{URL}

%prep
%setup -q -n %{pname}-%{version}

%build
export CFLAGS="$CFLAGS -Wno-error=format-security"
./configure --prefix=%{install_path} --enable-buggy-build
make %{?mflags}


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

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%doc LICENSE README.rst
%{OHPC_PUB}
