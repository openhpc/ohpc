#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# OpenMPI stack that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name/config
%define pname openmpi3
%define with_openib 1

%ifarch aarch64
%define with_psm 0
%define with_psm2 0
%else
%define with_psm 1
%define with_psm2 1
%endif

%define with_lustre 0
%define with_slurm 1

%{!?with_tm: %global with_tm 1}

Summary:   A powerful implementation of MPI

Name:      %{pname}-%{compiler_family}%{PROJ_DELIM}

Version:   3.0.0
Release:   1%{?dist}
License:   BSD-3-Clause
Group:     %{PROJ_NAME}/mpi-families
URL:       http://www.open-mpi.org
Source0:   http://www.open-mpi.org/software/ompi/v3.0/downloads/%{pname}-%{version}.tar.bz2
Source1:   OHPC_macros
Source3:   pbs-config
Patch0:    openmpi-3.0-pbs-config.patch

BuildRequires:  autoconf
BuildRequires:  automake
BuildRequires:  libtool
BuildRequires:  postfix
BuildRequires:  opensm
BuildRequires:  opensm-devel
BuildRequires:  numactl
BuildRequires:  libevent-devel
BuildRequires:  pmix%{PROJ_DELIM}
%if 0%{with_slurm}
BuildRequires:  slurm-devel%{PROJ_DELIM}
#!BuildIgnore:  slurm%{PROJ_DELIM}
%endif

%if 0%{?suse_version}
BuildRequires:  libnuma1
BuildRequires:  sysfsutils
%else
BuildRequires:  libsysfs-devel
BuildRequires:  numactl-devel
%endif

%if %{with_lustre}
BuildRequires:  lustre-client%{PROJ_DELIM}
%endif

%if %{with_openib}
%if 0%{?suse_version}
BuildRequires:  libibumad-devel
BuildRequires:  libibverbs-devel
%else
BuildRequires:  rdma-core-devel
%endif
%endif

%if %{with_psm}
BuildRequires:  infinipath-psm infinipath-psm-devel
%endif

%if %{with_tm}
BuildRequires:  pbspro-server%{PROJ_DELIM}
BuildRequires:  openssl-devel
%endif
%global __requires_exclude ^libpbs.so.*$

%if %{with_psm2}
BuildRequires:  libpsm2-devel >= 10.2.0
%endif

Requires: prun%{PROJ_DELIM}
#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_MPI_STACKS}/%{pname}-%{compiler_family}/%version

%description

Open MPI is a project combining technologies and resources from several
other projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in order to
build the best MPI library available.

This RPM contains all the tools necessary to compile, link, and run
Open MPI jobs.

%prep

%setup -q -n %{pname}-%{version}
%patch0 -p1

%build
# OpenHPC compiler designation
%ohpc_setup_compiler


BASEFLAGS="--prefix=%{install_path} --disable-static --enable-builtin-atomics --with-sge --enable-mpi-cxx"

# build against external pmix and libevent
BASEFLAGS="$BASEFLAGS --with-pmix=/opt/ohpc/pub/libs/pmix"
BASEFLAGS="$BASEFLAGS --with-libevent=external"

%if %{with_psm}
  BASEFLAGS="$BASEFLAGS --with-psm"
%endif
%if %{with_psm2}
  BASEFLAGS="$BASEFLAGS --with-psm2"
%endif
%if %{with_tm}
  BASEFLAGS="$BASEFLAGS --with-tm=/opt/pbs/"
%endif
%if %{with_openib}
  BASEFLAGS="$BASEFLAGS --with-verbs"
%endif
%if %{with_lustre}
  BASEFLAGS="$BASEFLAGS --with-io-romio-flags=--with-file-system=testfs+ufs+nfs+lustre"
%endif

export BASEFLAGS

%if %{with_tm}
cp %{SOURCE3} .
%{__chmod} 700 pbs-config
export PATH="./:$PATH"
%endif

./configure ${BASEFLAGS}

make %{?_smp_mflags}

%install
# OpenHPC compiler designation
%ohpc_setup_compiler
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install
# Remove .la files detected by rpm

rm $RPM_BUILD_ROOT/%{install_path}/lib/*.la


# OpenHPC module file
%{__mkdir_p} %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL: %{url}"

set     version			    %{version}

setenv          MPI_DIR             %{install_path}
prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/man
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/%{compiler_family}-%{pname}
prepend-path    PKG_CONFIG_PATH     %{install_path}/lib/pkgconfig

family "MPI"
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%{install_path}
%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%doc NEWS
%doc README
%doc LICENSE
%doc AUTHORS
%doc README.JAVA.txt

%changelog
* Thu Sep 21 2017 Adrian Reber <areber@redhat.com> - 3.0.0-1
- update to 3.0.0
- use the OpenHPC pmix package
- use the same libevent as pmix (external)
- small cleanups

* Thu Sep 21 2017 Adrian Reber <areber@redhat.com> - 1.10.7-1
- default to building with PSM and PSM2 at the same time

* Fri May 12 2017 Karl W Schulz <karl.w.schulz@intel.com> - 1.10.4-1
- switch to ohpc_compiler_dependent flag

* Fri Feb 17 2017 Adrian Reber <areber@redhat.com> - 1.10.6-1
- Switching to %%ohpc_compiler macro

* Tue Aug  5 2014  <karl.w.schulz@intel.com>
- Initial build.
