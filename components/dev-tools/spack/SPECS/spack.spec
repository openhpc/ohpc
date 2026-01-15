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

%define pname spack
%define spack_packages_version 2025.07.0

Name:		%{pname}%{PROJ_DELIM}
Version:	1.0.1
Release:	%{?dist}.1
Summary:	HPC software package management

Group:		%{PROJ_NAME}/dev-tools
License:	LGPL
URL:		https://github.com/spack/spack
Source0:	https://github.com/spack/%{pname}/archive/v%{version}.tar.gz
Source1:	https://github.com/spack/spack-packages/archive/refs/tags/v%{spack_packages_version}.tar.gz
Patch0:		modules.yaml.patch
Patch1:		config.yaml.patch
Patch2:		repos.yaml.patch

BuildRequires: rsync
BuildRequires: python3
Requires: bash
Requires: coreutils
Requires: subversion
Requires: hg
Requires: patch
Requires: gcc
Requires: gcc-c++
Requires: make
Requires: tar
Requires: gzip
Requires: unzip
Requires: bzip2
Requires: xz
Requires: zstd
Requires: file
Requires: git
Requires: curl
%if 0%{?rhel} || 0%{?openEuler}
Requires: gnupg2
%endif
%if 0%{?suse_version}
Requires: gpg2
%endif

%global install_path %{OHPC_APPS}/%{pname}/%version
%global spack_packages_path %{OHPC_APPS}/%{pname}/spack-packages-%{spack_packages_version}
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')

%description
Spack is a package management tool designed to support multiple versions and
configurations of software on a wide variety of platforms and environments. It
was designed for large supercomputing centers, where many users and application
teams share common installations of software on clusters with exotic
architectures, using libraries that do not have a standard ABI. Spack is
non-destructive: installing a new version does not break existing
installations, so many configurations can coexist on the same system.

Most importantly, Spack is simple. It offers a simple spec syntax so that users
can specify versions and configuration options concisely. Spack is also simple
for package authors: package files are written in pure Python, and specs allow
package authors to write a single build script for many different builds of the
same package.


%prep
%setup -q -n %{pname}-%{version} -a1
%patch -P 0 -p 1
%patch -P 1 -p 1
%patch -P 2 -p 1

# cleanup any recipes that have hard-coded /bin/env in them as this will
# prevent installation on Leap
grep -rl '#!/bin/env ' . | xargs -i@ sed -i 's|#!/bin/env|#!/usr/bin/env|g' @

# do not distribute qa scripts
rm -rf share/spack/qa

# This binary has GLIBC_2.2.5 which does not exist on EL9 aarch64
rm -f spack-packages-%{spack_packages_version}/repos/spack_repo/builtin/packages/patchelf/test/hello

sed -e "s,@@OHPC_APPS@@,%{OHPC_APPS},g" -i etc/spack/defaults/config.yaml
sed -e "s,@@OHPC_MODULEDEPS@@,%{OHPC_MODULEDEPS},g" -i etc/spack/defaults/modules.yaml
sed -e "s,@@SPACK_PACKAGES_PATH@@,%{spack_packages_path}/spack_repo/builtin,g" -i etc/spack/defaults/repos.yaml

%install
mkdir -p %{buildroot}%{install_path}
rsync -a --exclude=.gitignore {etc,bin,lib,var,share} %{buildroot}%{install_path}

mkdir -p %{buildroot}%{spack_packages_path}
rsync -a spack-packages-%{spack_packages_version}/repos/ %{buildroot}%{spack_packages_path}/

# remove embedded binary with /usr/tce rpaths that breaks Leap 15.3 builds
rm -f %{buildroot}/%{install_path}/var/spack/repos/builtin/packages/patchelf/test/hello

# OpenHPC module file
%{__mkdir} -p %{buildroot}/%{OHPC_MODULES}/spack
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/spack/%{version}
#%Module1.0#####################################################################

module-whatis "Name: Spack"
module-whatis "Version: %{version}"
module-whatis "Packages Version: %{spack_packages_version}"
module-whatis "Category: System/Configuration"
module-whatis "Description: Spack package management"
module-whatis "URL: %{url}"

set     version             %{version}
set     SPACK_ROOT          %{install_path}

prepend-path   PATH         %{install_path}/bin
prepend-path   MODULEPATH   %{OHPC_MODULEDEPS}/%{pname}/

EOF

%{__mkdir} -p %{buildroot}/%{_docdir}
%{__mkdir} -p %{buildroot}/%{OHPC_MODULEDEPS}/%{pname}/
%{__mkdir} -p %{buildroot}/%{OHPC_APPS}/%{pname}/local

%files
%{OHPC_HOME}
%doc LICENSE* README.md
