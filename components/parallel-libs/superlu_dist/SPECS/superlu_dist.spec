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

%define major   5
%define libname libsuperlu_dist

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        6.4.0
Release:        1%{?dist}
Summary:        A general purpose library for the direct solution of linear equations
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/parallel-libs
URL:            https://portal.nersc.gov/project/sparse/superlu/
Source0:        https://github.com/xiaoyeli/superlu_dist/archive/v%{version}.tar.gz#/%{pname}-%{version}.tar
Source2:        superlu_dist-make.inc
Source3:        superlu_dist-intel-make.inc
Source4:        superlu_dist-arm1-make.inc
Patch1:         superlu_dist-parmetis.patch
Patch2:         noexamples.patch
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
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
%patch1 -p1
# disable build of examples which don't get installed (karl@ices.utexas.edu - 3/6/19)
%patch2 -p0

%if "%{compiler_family}" == "intel"
cp %SOURCE3 make.inc
%else
%if "%{compiler_family}" == "arm1"
cp %SOURCE4 make.inc
%else
cp %SOURCE2 make.inc
%endif
%endif

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load metis ptscotch

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
module load openblas
%define blas_lib -L$OPENBLAS_LIB -lopenblas
%endif

export CFLAGS="${CFLAGS} -Wno-implicit-function-declaration"
export CFLAGS="${CFLAGS} -Wno-implicit-int"

%if "%{compiler_family}" == "arm1"
%define blas_lib -armpl
%endif

%if "%{compiler_family}" == "intel"
%define blas_lib  -L$MKLROOT/lib/intel64 -lmkl_intel_ilp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl
%undefine _hardened_build
%endif

make SuperLUroot=$(pwd)

mkdir tmp
(cd tmp; ar x ../SRC/libsuperlu_dist.a)
mpif90 -z muldefs -shared -Wl,-soname=%{libname}.so.%{major} \
    -o ./%{libname}.so.%{version} tmp/*.o -L$METIS_LIB \
%if "%{compiler_family}" == "intel"
    -qopenmp \
%else
    -fopenmp \
%endif
    -L$PTSCOTCH_LIB \
    -lptscotch -lptscotcherr -lscotch -lmetis %{blas_lib} \
    -lbz2 -lz %{?__global_ldflags}


%install

%{__mkdir_p} %{buildroot}%{install_path}/etc
install -m644 make.inc %{buildroot}%{install_path}/etc

%{__mkdir_p} %{buildroot}%{install_path}/include
install -m644 SRC/*.h %{buildroot}%{install_path}/include/

%{__mkdir_p} %{buildroot}%{install_path}/lib
install -m 755 libsuperlu_dist.so.%{version} %{buildroot}%{install_path}/lib
pushd %{buildroot}%{install_path}/lib
ln -s libsuperlu_dist.so.%{version} libsuperlu_dist.so.%{major}
ln -s libsuperlu_dist.so.%{version} libsuperlu_dist.so
popd

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
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
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

%{__mkdir_p} %{buildroot}/%_docdir

%files
%{OHPC_PUB}
%doc README.md
