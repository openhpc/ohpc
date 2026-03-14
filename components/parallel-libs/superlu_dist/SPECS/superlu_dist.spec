#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# superlu_dist build that is is dependent on compiler toolchain and MPI
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname superlu_dist

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        9.2.1
Release:        1%{?dist}
Summary:        A general purpose library for the direct solution of linear equations
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/parallel-libs
URL:            https://portal.nersc.gov/project/sparse/superlu/
Source0:        https://github.com/xiaoyeli/superlu_dist/archive/v%{version}.tar.gz#/%{pname}-%{version}.tar
Patch1:         superlu_dist-parmetis.patch
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires:  cmake
BuildRequires:  ptscotch-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       ptscotch-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  metis-%{compiler_family}%{PROJ_DELIM}
Requires:       metis-%{compiler_family}%{PROJ_DELIM}
%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
BuildRequires:  openblas-%{compiler_family}%{PROJ_DELIM}
Requires:       openblas-%{compiler_family}%{PROJ_DELIM}
%endif
%if 0%{?rhel} || 0%{?openEuler}
BuildRequires:  bzip2-devel
Requires:       bzip2
%else
BuildRequires:  libbz2-devel
Requires:       libbz2-1
%endif
BuildRequires:  zlib-devel make
Requires:       zlib

#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%version

%description
SuperLU is a general purpose library for the direct solution of large, sparse,
nonsymmetric systems of linear equations on high performance machines. The
library is written in C and is callable from either C or Fortran. The library
routines will perform an LU decomposition with partial pivoting and triangular
system solves through forward and back substitution. The LU factorization routines
can handle non-square matrices but the triangular solves are performed only for
square matrices. The matrix columns may be preordered (before factorization)
either through library or user supplied routines. This preordering for sparsity
is completely separate from the factorization. Working precision iterative
refinement subroutines are provided for improved backward stability. Routines
are also provided to equilibrate the system, estimate the condition number,
calculate the relative backward error, and estimate error bounds for the refined
solutions.

%prep
%setup -q -n superlu_dist-%{version}
%patch -P 1 -p1

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load metis ptscotch

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
module load openblas
%define blas_lib "-L${OPENBLAS_LIB} -lopenblas"
%endif

%if "%{compiler_family}" == "arm1"
%define blas_lib "-larmpl"
%endif

%if "%{compiler_family}" == "intel"
%define blas_lib "-L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl"
%undefine _hardened_build
%endif

export CFLAGS="${CFLAGS} -Wno-implicit-function-declaration"
export CFLAGS="${CFLAGS} -Wno-implicit-int"
export CFLAGS="${CFLAGS} -Wno-unused-but-set-variable"
export CFLAGS="${CFLAGS} -Wno-unused-variable"
export CFLAGS="${CFLAGS} -Wno-maybe-uninitialized"


mkdir build && cd build
cmake .. \
	-DCMAKE_INSTALL_PREFIX=%{install_path} \
	-DCMAKE_C_COMPILER=mpicc \
	-DCMAKE_CXX_COMPILER=mpicxx \
	-DCMAKE_Fortran_COMPILER=mpif90 \
	-DCMAKE_C_FLAGS="${CFLAGS} -fPIC" \
	-DCMAKE_CXX_FLAGS="${CXXFLAGS} -fPIC" \
	-DBUILD_SHARED_LIBS=ON \
	-DBUILD_STATIC_LIBS=OFF \
	-DTPL_ENABLE_PARMETISLIB=ON \
	-DTPL_PARMETIS_INCLUDE_DIRS="${PTSCOTCH_INC}" \
	-DTPL_PARMETIS_LIBRARIES="${PTSCOTCH_LIB}/libptscotchparmetisv3.so;${METIS_LIB}/libmetis.so" \
	-DTPL_BLAS_LIBRARIES=%{blas_lib} \
	-DTPL_ENABLE_INTERNAL_BLASLIB=OFF \
	-DTPL_ENABLE_LAPACKLIB=OFF \
	-Denable_examples=OFF \
	-Denable_tests=OFF
make %{?_smp_mflags}


%install

cd build
%make_install

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the SuperLU_dist library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr " "
puts stderr "Note that this build of SuperLU_dist leverages the metis and MKL libraries."
puts stderr "Consequently, these packages are loaded automatically with this module."

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version                     %{version}

depends-on metis
depends-on ptscotch

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib64

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_LIB        %{install_path}/lib64

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}%{OHPC_CUSTOM_PKG_DELIM}"
EOF

%{__mkdir_p} %{buildroot}/%_docdir

%files
%{OHPC_PUB}
%doc README.md
