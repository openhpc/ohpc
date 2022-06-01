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

Name:		%{pname}%{PROJ_DELIM}
Version:	0.17.0
Release:	1%{?dist}
Summary:	HPC software package management

Group:		%{PROJ_NAME}/dev-tools
License:	LGPL
URL:		https://github.com/LLNL/spack
Source0:	https://github.com/LLNL/%{pname}/archive/v%{version}.tar.gz

BuildRequires: rsync
BuildRequires: python3
Requires: bash
Requires: coreutils
Requires: subversion
Requires: hg
Requires: patch
Requires: python3-mock
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
%if 0%{?rhel}
Requires: gnupg2
%endif
%if 0%{?suse_version}
Requires: gpg2
%endif

%global install_path %{OHPC_ADMIN}/%{pname}/%version
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
%setup -q -n %{pname}-%{version}

# cleanup any recipes that have hard-coded /bin/env in them as this will
# prevent installation on Leap
grep -rl '#!/bin/env ' . | xargs -i@ sed -i 's|#!/bin/env|#!/usr/bin/env|g' @

%install
mkdir -p %{buildroot}%{install_path}
rsync -av --exclude=.gitignore {etc,bin,lib,var,share} %{buildroot}%{install_path}

# remove embedded binary with /usr/tce rpaths that breaks Leap 15.3 builds
rm -f %{buildroot}/%{install_path}/var/spack/repos/builtin/packages/patchelf/test/hello 

# OpenHPC module file
%{__mkdir} -p %{buildroot}/%{OHPC_ADMIN}/modulefiles/spack
%{__cat} << EOF > %{buildroot}/%{OHPC_ADMIN}/modulefiles/spack/%{version}
#%Module1.0#####################################################################

module-whatis "Name: Spack"
module-whatis "Version: %{version}"
module-whatis "Category: System/Configuration"
module-whatis "Description: Spack package management"
module-whatis "URL: https://github.com/LLNL/spack/"

set     version             %{version}
set     SPACK_ROOT          %{install_path}

prepend-path   PATH         %{install_path}/bin
prepend-path   MODULEPATH   %{install_path}/modules

EOF

%{__mkdir} -p %{buildroot}/%{_docdir}

%files
%{OHPC_HOME}
%doc LICENSE* README.md
