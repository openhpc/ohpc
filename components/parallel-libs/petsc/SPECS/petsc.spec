#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# PETSc library that is is dependent on compiler toolchain and MPI
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

%if "%{compiler_family}" != "intel"
BuildRequires: scalapack-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:      scalapack-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
%endif

# Base package name
%define pname petsc

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:        Portable Extensible Toolkit for Scientific Computation
License:        2-clause BSD
Group:          %{PROJ_NAME}/parallel-libs
Version:        3.16.1
Release:        1%{?dist}
Source0:        http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/petsc-%{version}.tar.gz
Patch1:         petsc.rpath.patch
Patch2:         py3_tests.patch
Url:            http://www.mcs.anl.gov/petsc/
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires:  phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  python2-devel
BuildRequires:  valgrind-devel
BuildRequires:  xz
BuildRequires:  zlib-devel
%if 0%{?rhel}
BuildRequires:  openssh-clients
BuildRequires:  glibc-langpack-en
%else
BuildRequires:  openssh
%endif

#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%version

%description
PETSc is a suite of data structures and routines for the scalable
(parallel) solution of scientific applications modeled by partial
differential equations.

%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1
%patch2 -p1


%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load phdf5

%if "%{compiler_family}" == "arm"
module load scalapack
%endif

# Enable scalapack and openblas linkage for blas/lapack with gnu and other (e.g. llvm) builds
%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
module load scalapack openblas
%endif

# PETSc build system doesn't honor canonical CFLAGS,CXXFLAGS, etc
# directly. Place them in OPTFLAG variants instead.
COPTFLAGS="${CFLAGS}"
CXXOPTFLAGS="${CXXFLAGS}"
FOPTFLAGS="${FCFLAGS}"
unset CFLAGS
unset CXXFLAGS
unset FCFLAGS

# icc-impi requires mpiicc wrappers, otherwise dynamic libs are not generated.
# gnu-impi finds include/4.8.0/mpi.mod first, unless told not to.
%{__python2} ./config/configure.py \
        --prefix=%{install_path} \
        --FFLAGS="-fPIC" \
%if %{compiler_family} == intel
        --with-blas-lapack-dir=$MKLROOT/lib/intel64 \
%else
        --CFLAGS="-fPIC -DPIC" \
        --CXXFLAGS="-fPIC -DPIC" \
        --with-scalapack-dir=$SCALAPACK_DIR \
%if %{compiler_family} == arm
        --with-blas-lapack-lib=$ARMPL_LIBRARIES/libarmpl.so \
%else
        --with-blas-lapack-lib=$OPENBLAS_LIB/libopenblas.so \
%endif
%endif
%if %{mpi_family} == impi
%if %{compiler_family} == intel
        --with-cc=mpiicc    \
        --with-cxx=mpiicpc  \
        --with-fc=mpiifort  \
        --with-f77=mpiifort \
%else
%if "%{compiler_family}" == "gnu"
        --FFLAGS=-I$I_MPI_ROOT/include64/gfortran/4.9.0/ \
%endif
%endif
%endif
%if 0%{?OHPC_BUILD}
        --with-make-np=3 \
%endif
        --with-c-support \
        --with-fortran-interfaces=1 \
        --with-debugging=no \
        --with-shared-libraries \
        --with-mpi=1 \
        --with-batch=0 \
        --with-hdf5=1 \
        --with-hdf5-lib=$HDF5_LIB/libhdf5.so \
        --with-hdf5-include=$HDF5_INC || cat configure.log

make

%install

make install DESTDIR=$RPM_BUILD_ROOT

cd %{buildroot}%{install_path}
for file in \
	lib/petsc/bin/petsc_gen_xdmf.py \
	lib/petsc/bin/PetscBinaryIOTrajectory.py \
	lib/petsc/bin/petsc-performance-view \
	lib/petsc/bin/petscnagfor \
	lib/petsc/bin/taucc.py \
	lib/petsc/bin/petscnagupgrade.py \
	lib/petsc/bin/saws/SAWs.py \
	lib/petsc/bin/petsclogformat.py \
	share/petsc/examples/config/testparse.py \
	share/petsc/examples/config/gmakegen.py \
	share/petsc/examples/config/gmakegentest.py \
	share/petsc/examples/config/report_tests.py; do
		sed -e "s,/env python,/python2,g" -i $file
done

# remove stock module file
rm -rf %{buildroot}%{install_path}/lib/modules

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the PETSc library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr " "

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version                     %{version}

# Require phdf5 (and scalapack for compiler families other than intel)
depends-on phdf5
%if "%{compiler_family}" != "intel"
depends-on scalapack
%endif

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_LIB        %{install_path}/lib

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}%{OHPC_CUSTOM_PKG_DELIM}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%{OHPC_PUB}
%doc CONTRIBUTING LICENSE

