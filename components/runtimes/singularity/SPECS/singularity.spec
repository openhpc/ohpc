#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-
#
# Copyright (c) 2017-2018, SyLabs, Inc. All rights reserved.
# Copyright (c) 2017, SingularityWare, LLC. All rights reserved.
#
# Copyright (c) 2015-2017, Gregory M. Kurtzer. All rights reserved.
# 
# Copyright (c) 2016, The Regents of the University of California, through
# Lawrence Berkeley National Laboratory (subject to receipt of any required
# approvals from the U.S. Dept. of Energy).  All rights reserved.
# 
# This software is licensed under a customized 3-clause BSD license.  Please
# consult LICENSE file distributed with the sources of this project regarding
# your rights to use or distribute this software.
# 
# NOTICE.  This Software was developed under funding from the U.S. Department of
# Energy and the U.S. Government consequently retains certain rights. As such,
# the U.S. Government has been granted for itself and others acting on its
# behalf a paid-up, nonexclusive, irrevocable, worldwide license in the Software
# to reproduce, distribute copies to the public, prepare derivative works, and
# perform publicly and display publicly, and to permit other to do so. 
# 
# 

%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname singularity

%define singgopath src/github.com/sylabs/singularity

Summary: Application and environment virtualization
Name: %{pname}%{PROJ_DELIM}
Version: 3.1.0
Release: 1%{?dist}
# https://spdx.org/licenses/BSD-3-Clause-LBNL.html
License: BSD-3-Clause-LBNL
Group: %{PROJ_NAME}/runtimes
URL: http://singularity.lbl.gov/
Source0: https://github.com/sylabs/singularity/releases/download/v%{version}/%{pname}-%{version}.tar.gz
Patch1: singularity-suse-timezone.patch
ExclusiveOS: linux
BuildRequires: gcc
BuildRequires: git
BuildRequires: openssl-devel
BuildRequires: libuuid-devel
BuildRequires: libseccomp-devel
Requires: file
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires: go
Requires: squashfs
%else
BuildRequires: golang > 1.6
Requires: squashfs-tools
%endif
#!BuildIgnore: post-build-checks rpmlint-Factory

# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

%description
Singularity provides functionality to make portable
containers that can be used across host environments.

%prep
# Create our build root
rm -rf %{name}-%{version}
mkdir %{name}-%{version}

%build
cd %{name}-%{version}

mkdir -p gopath/%{singgopath}
tar -C "gopath/src/github.com/sylabs/" -xf "%SOURCE0"

export GOPATH=$PWD/gopath
export PATH=$GOPATH/bin:$PATH
cd $GOPATH/%{singgopath}

./mconfig -V %{version}-%{release} \
    --prefix=%{install_path}

cd builddir
make old_config=

%install
cd %{name}-%{version}

export GOPATH=$PWD/gopath
export PATH=$GOPATH/bin:$PATH
cd $GOPATH/%{singgopath}/builddir

mkdir -p $RPM_BUILD_ROOT%{install_path}/share/man/man1
make DESTDIR=$RPM_BUILD_ROOT install man
chmod 644 $RPM_BUILD_ROOT%{install_path}/etc/singularity/actions/*

# NO_BRP_CHECK_RPATH has no effect on CentOS 7
export NO_BRP_CHECK_RPATH=true


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
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib
prepend-path    MANPATH             %{install_path}/man

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
%doc singularity-ohpc-3.1.0/gopath/%{singgopath}/examples singularity-ohpc-3.1.0/gopath/%{singgopath}/*.md
%dir %{install_path}/etc/singularity
%config(noreplace) %{install_path}/etc/singularity/*
%attr(755, root, root) %{install_path}/etc/singularity/actions/exec
%attr(755, root, root) %{install_path}/etc/singularity/actions/run
%attr(755, root, root) %{install_path}/etc/singularity/actions/shell
%attr(755, root, root) %{install_path}/etc/singularity/actions/start
%attr(755, root, root) %{install_path}/etc/singularity/actions/test
%{OHPC_PUB}
#SUID programs
%attr(4755, root, root) %{install_path}/libexec/singularity/bin/starter-suid
