#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is is dependent on compiler toolchain and MPI
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires:      openblas-%{compiler_family}%{PROJ_DELIM}
%endif

# Base package name
%define pname scalapack

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:        A subset of LAPACK routines redesigned for heterogeneous computing
License:        netlib ScaLAPACK License
Group:          %{PROJ_NAME}/parallel-libs
Version:        2.2.3
Release:        1%{?dist}
# This is freely distributable without any restrictions.
Url:            https://netlib.org/scalapack/
Source0:        https://github.com/Reference-ScaLAPACK/scalapack/archive/refs/tags/v%{version}.tar.gz
Source1:        baselibs.conf
BuildRequires:  cmake make

%description
The ScaLAPACK (or Scalable LAPACK) library includes a subset
of LAPACK routines redesigned for distributed memory MIMD
parallel computers. It is currently written in a
Single-Program-Multiple-Data style using explicit message
passing for interprocessor communication. It assumes
matrices are laid out in a two-dimensional block cyclic
decomposition.

ScaLAPACK is designed for heterogeneous computing and is
portable on any computer that supports MPI or PVM.

Like LAPACK, the ScaLAPACK routines are based on
block-partitioned algorithms in order to minimize the frequency
of data movement between different levels of the memory hierarchy.
(For such machines, the memory hierarchy includes the off-processor
memory of other processors, in addition to the hierarchy of registers,
cache, and local memory on each processor.) The fundamental building
blocks of the ScaLAPACK library are distributed memory versions (PBLAS)
of the Level 1, 2 and 3 BLAS, and a set of Basic Linear Algebra
Communication Subprograms (BLACS) for communication tasks that arise
frequently in parallel linear algebra computations. In the ScaLAPACK
routines, all interprocessor communication occurs within the PBLAS and the
BLACS. One of the design goals of ScaLAPACK was to have the ScaLAPACK
routines resemble their LAPACK equivalents as much as possible.

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%version

%prep
%setup -q -n %{pname}-%{version}

%build
%ohpc_setup_compiler
export CFLAGS="${CFLAGS} -Wno-implicit-function-declaration"
export CFLAGS="${CFLAGS} -Wno-implicit-int"
export CFLAGS="${CFLAGS} -Wno-maybe-uninitialized"
export CFLAGS="${CFLAGS} -Wno-uninitialized"
export CFLAGS="${CFLAGS} -Wno-unused-variable"
export CFLAGS="${CFLAGS} -Wno-strict-aliasing"
export CFLAGS="${CFLAGS} -Wno-unused-but-set-variable"
export FFLAGS="${FCFLAGS}"
export FFLAGS="${FFLAGS} -Wno-maybe-uninitialized"
export FFLAGS="${FFLAGS} -Wno-unused-label"
export FFLAGS="${FFLAGS} -Wno-unused-dummy-argument"
export FFLAGS="${FFLAGS} -Wno-unused-variable"
export FFLAGS="${FFLAGS} -Wno-tabs"

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
module load openblas
%define blas_lib "-L${OPENBLAS_LIB} -lopenblas"
%endif

%if "%{compiler_family}" == "arm1"
export CFLAGS="${CFLAGS} -fsimdmath"
%define blas_lib "-L${ARMPL_LIBRARIES} -larmpl"
%endif

%if "%{compiler_family}" == "intel"
%define blas_lib "-L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl"
%endif

%if "%{compiler_family}" == "gnu14" || "%{compiler_family}" == "gnu15"
export FFLAGS="${FFLAGS} -fallow-argument-mismatch"
%endif
%if "%{compiler_family}" == "gnu15"
export CFLAGS="${CFLAGS} -std=gnu89"
%endif

mkdir build && cd build
cmake .. \
	-DCMAKE_INSTALL_PREFIX=%{install_path} \
	-DCMAKE_INSTALL_LIBDIR=lib \
	-DCMAKE_C_COMPILER=mpicc \
	-DCMAKE_Fortran_COMPILER=mpif90 \
	-DCMAKE_C_FLAGS="${CFLAGS} -fPIC" \
	-DCMAKE_Fortran_FLAGS="${FFLAGS} -fPIC" \
	-DBUILD_SHARED_LIBS=ON \
	-DBUILD_STATIC_LIBS=OFF \
	-DSCALAPACK_BUILD_TESTS=OFF \
%if "%{?OHPC_USE_CCACHE}" == "yes"
	-DCMAKE_C_COMPILER_LAUNCHER=ccache \
	-DCMAKE_Fortran_COMPILER_LAUNCHER=ccache \
%endif
	-DLAPACK_LIBRARIES=%{blas_lib} \
	-DBLAS_LIBRARIES=%{blas_lib}
make %{?_smp_mflags}

%install
%{__mkdir} -p %{buildroot}/%{_docdir}

cd build
%make_install

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the ScaLAPACK library built with the %{compiler_family} compiler"
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

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
depends-on openblas
%endif

prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}%{OHPC_CUSTOM_PKG_DELIM}"
EOF

%files
%{OHPC_PUB}
%doc README LICENSE
