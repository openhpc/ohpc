#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# MVAPICH2 MPI stack that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Multiple permutations for this MPI stack are possible depending
# on the desired underlying resource manager and comm library support.

%{!?with_slurm: %global with_slurm 0}
%{!?with_pbs: %global with_pbs 0}
%{!?RMS_DELIM: %global RMS_DELIM %{nil}}
%{!?COMM_DELIM: %global COMM_DELIM %{nil}}

# Base package name/config
%define pname mvapich2
%define sname mvapich

Summary:   OSU MVAPICH2 MPI implementation
Name:      %{pname}%{COMM_DELIM}-%{compiler_family}%{RMS_DELIM}%{PROJ_DELIM}
Version:   4.1
Release:   1%{?dist}
License:   BSD
Group:     %{PROJ_NAME}/mpi-families
URL:       http://mvapich.cse.ohio-state.edu
Source0:   http://mvapich.cse.ohio-state.edu/download/mvapich/mv2/%{sname}-%{version}.tar.gz
Source1:   http://wgropp.cs.illinois.edu/projects/software/sowing/sowing.tar.gz

%if 0%{with_slurm}
BuildRequires: slurm-devel%{PROJ_DELIM} slurm%{PROJ_DELIM}
Provides:      %{pname}-%{compiler_family}%{PROJ_DELIM}
%endif

%if 0%{?sles_version} || 0%{?suse_version}
Buildrequires: ofed
BuildRequires: rdma-core-devel infiniband-diags-devel
%endif
%if 0%{?rhel} || 0%{?openEuler}
Buildrequires: rdma-core-devel libibmad-devel
%endif

Requires: prun%{PROJ_DELIM}
BuildRequires: bison make m4
BuildRequires: zlib-devel

# Default library install path
%define install_path %{OHPC_MPI_STACKS}/%{pname}-%{compiler_family}/%version

%description

MVAPICH is a high-performance MPI implementation based on MPICH's
ch4 device layer with UCX support. It targets InfiniBand and RoCE
networks and provides optimized collective operations and
point-to-point communication for HPC clusters.

%prep

%setup -q -n %{sname}-%{version} -a 1

%build
%ohpc_setup_compiler
%if "%{compiler_family}" == "gnu12" || "%{compiler_family}" == "gnu13" || "%{compiler_family}" == "gnu14" || "%{compiler_family}" == "gnu15"
# configure fails with:
#   The Fortran compiler gfortran does not accept programs that
#   call the same routine with arguments of different types without
#   the option -fallow-argument-mismatch.
#   Rerun configure with FFLAGS=-fallow-argument-mismatch
# This seems to fix the build.
export FFLAGS=-fallow-argument-mismatch
%if "%{compiler_family}" == "gnu14" || "%{compiler_family}" == "gnu15"
export CFLAGS="${CFLAGS} -Wno-incompatible-pointer-types"
%endif
%if "%{compiler_family}" == "gnu15"
export CFLAGS="${CFLAGS} -std=gnu17"
%endif
%endif
%if "%{compiler_family}" == "intel"
export CFLAGS="${CFLAGS} -Wno-incompatible-function-pointer-types"
export FFLAGS="-DFLANG"
%endif

# Build sowing/doctext to generate man pages
cd sowing-1.1.26
./configure && make
export PATH=$(pwd)/src/doctext:${PATH}
export DOCTEXT_PATH=$(pwd)/share/doctext
export TEXTFILTER_PATH=$(pwd)/share
cd ..

./configure --prefix=%{install_path} \
	    --enable-cxx \
	    --enable-g=dbg \
            --with-device=ch4:ucx \
%if 0%{with_slurm}
            --with-pm=no --with-pmi=slurm \
%endif
	    --enable-fast=O3 || { cat config.log && exit 1; }

make %{?_smp_mflags}
make mandoc

%install
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

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/%{compiler_family}-%{pname}
prepend-path    MPI_DIR             %{install_path}
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
%{OHPC_HOME}
%doc README.envvar
%doc COPYRIGHT
%doc CHANGELOG
%doc CHANGES
%doc README
