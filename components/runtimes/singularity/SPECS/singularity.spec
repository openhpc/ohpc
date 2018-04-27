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
%define PNAME SINGULARITY

# This allows us to pick up the default value from the configure
%{!?with_slurm: %global with_slurm no}
%if "%{with_slurm}" == "yes"
%global slurm 1
%else
%global slurm 0
%endif

Summary: Application and environment virtualization
Name: %{pname}%{PROJ_DELIM}
Version: 2.5.0
Release: 1%{?dist}
# https://spdx.org/licenses/BSD-3-Clause-LBNL.html
License: BSD-3-Clause-LBNL
Group: %{PROJ_NAME}/runtimes
URL: http://singularity.lbl.gov/
Source0: https://github.com/singularityware/singularity/releases/download/%{version}/%{pname}-%{version}.tar.gz
Source1: OHPC_macros
Patch1: singularity-suse-timezone.patch
ExclusiveOS: linux
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: libarchive-devel
BuildRequires: libtool
BuildRequires: python
Requires: file
%if 0%{?sles_version} || 0%{?suse_version}
Requires: squashfs
%else
Requires: squashfs-tools
%endif

# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

%description
Singularity provides functionality to build the smallest most minimal
possible containers, and running those containers as single application
environments.

%if %slurm
%package -n singularity-slurm%{PROJ_DELIM}
Summary: Singularity plugin for SLURM
Requires: singularity = %{version}-%{release}
BuildRequires: slurm-devel%{PROJ_DELIM}

%description -n singularity-slurm%{PROJ_DELIM}
The Singularity plugin for SLURM allows jobs to be started within
a container.  This provides a simpler interface to the user (they
don't have to be aware of the singularity executable) and doesn't
require a setuid binary.
%endif


%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1

%build
if [ ! -f configure ]; then
  ./autogen.sh
fi

./configure --prefix=%{install_path}  \
  --disable-static --with-pic \
%if %slurm
  --with-slurm
%else
  --without-slurm
%endif

%{__make} %{?_smp_mflags}

%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT
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
%defattr(-, root, root)
%doc examples CONTRIBUTORS.md CONTRIBUTING.md COPYRIGHT.md INSTALL.md LICENSE-LBNL.md LICENSE.md README.md
%attr(0644, root, root) %config(noreplace) %{install_path}/etc/singularity/*
%{OHPC_PUB}
#SUID programs
%attr(4755, root, root) %{install_path}/libexec/singularity/bin/action-suid
%attr(4755, root, root) %{install_path}/libexec/singularity/bin/mount-suid
%attr(4755, root, root) %{install_path}/libexec/singularity/bin/start-suid

%if %slurm
%files -n singularity-slurm%{PROJ_DELIM}
%defattr(-, root, root)
%{install_path}/lib/slurm/singularity.so
%endif

%changelog
