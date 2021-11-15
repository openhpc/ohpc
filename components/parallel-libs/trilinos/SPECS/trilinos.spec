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


# Base package name
%define pname trilinos
%define ver_exp 13-2-0

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        13.2.0
Release:        1%{?dist}
Summary:        A collection of libraries of numerical algorithms
# Trilinos is licensed on a per-package basis. Refer to https://trilinos.github.io/license.html
License:        LGPL-2.1 and BSD-3-Clause
Group:          %{PROJ_NAME}/parallel-libs
Url:            https://trilinos.org/
Source0:        https://github.com/trilinos/Trilinos/archive/trilinos-release-%{ver_exp}.tar.gz
Patch0:         trilinos-13_0_0-destdir_fix.patch
Patch1:         trilinos-13_2_0-lapack_nothrow.patch

Requires:       lmod%{PROJ_DELIM} >= 7.6.1
Requires:       python3

BuildRequires:  cmake
BuildRequires:  doxygen
BuildRequires:  expat
BuildRequires:  graphviz
BuildRequires:  libxml2-devel
BuildRequires:  swig > 3.0.0
BuildRequires:  xz
BuildRequires:  zlib-devel
BuildRequires:  boost-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  netcdf-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  python3

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
BuildRequires:  openblas-%{compiler_family}%{PROJ_DELIM}
Requires:       openblas-%{compiler_family}%{PROJ_DELIM}
%endif

#!BuildIgnore: post-build-checks
#!BuildIgnore: brp-check-suse

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%version

%description
Trilinos is a collection of reusable scientific software libraries, known
in particular for linear solvers, non-linear solvers, transient solvers,
optimization solvers, and uncertainty quantification (UQ) solvers.

The Trilinos Project is an effort to develop algorithms and enabling
technologies within an object-oriented software framework for the solution of
large-scale, complex multi-physics engineering and scientific problems

Trilinos is organized around fundamental software elements called packages.
For a summary of included packages see https://trilinos.github.io/packages.html

%prep
%setup -q -n  Trilinos-trilinos-release-%{ver_exp}
%patch0 -p1
%patch1 -p1

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load boost
module load netcdf
module load phdf5

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
module load openblas
%endif

mkdir build
cd build
cmake   -DCMAKE_INSTALL_PREFIX=%{install_path}                          \
        -DCMAKE_EXE_LINKER_FLAGS:STRING="-fPIC"                         \
        -DCMAKE_VERBOSE_MAKEFILE:BOOL=TRUE                              \
        -DCMAKE_BUILD_TYPE:STRING=RELEASE                               \
        -DBUILD_SHARED_LIBS:BOOL=ON                                     \
        -DCMAKE_SKIP_INSTALL_RPATH:BOOL=ON                              \
        -DCMAKE_SKIP_RPATH:BOOL=ON                                      \
        -DTrilinos_VERBOSE_CONFIGURE:BOOL=ON                            \
        -DTrilinos_ENABLE_ALL_PACKAGES:BOOL=OFF                         \
        -DTrilinos_INSTALL_LIB_DIR="%{install_path}/lib"                                \
%if "%{compiler_family}" == "intel"
        -DTPL_ENABLE_MKL:BOOL=ON                                        \
        -DMKL_INCLUDE_DIRS:FILEPATH="${MKLROOT}/include"                \
        -DMKL_LIBRARY_DIRS:FILEPATH="${MKLROOT}/lib/intel64"            \
        -DMKL_LIBRARY_NAMES:STRING="mkl_rt"                             \
        -DTPL_ENABLE_BLAS:BOOL=ON                                       \
        -DBLAS_LIBRARY_DIRS:PATH="${MKLROOT}/lib/intel64"               \
        -DBLAS_LIBRARY_NAMES:STRING="mkl_rt"                            \
        -DTPL_ENABLE_LAPACK:BOOL=ON                                     \
        -DLAPACK_LIBRARY_DIRS:PATH="${MKLROOT}/lib/intel64"             \
        -DLAPACK_LIBRARY_NAMES:STRING="mkl_rt"                          \
%else
%if "%{compiler_family}" == "arm"
        -DTPL_ENABLE_BLAS:BOOL=ON                                       \
        -DBLAS_LIBRARY_DIRS:PATH="${ARMPL_LIBRARIES}"                   \
        -DBLAS_LIBRARY_NAMES:STRING="armpl_mp"                          \
        -DTPL_ENABLE_LAPACK:BOOL=ON                                     \
        -DLAPACK_LIBRARY_DIRS:PATH="${ARMPL_LIBRARIES}"                 \
        -DLAPACK_LIBRARY_NAMES:STRING="armpl_mp"                        \
%else
        -DTPL_ENABLE_BLAS:BOOL=ON                                       \
        -DBLAS_LIBRARY_DIRS:PATH="${OPENBLAS_LIB}"                      \
        -DBLAS_LIBRARY_NAMES:STRING="openblas"                          \
        -DTPL_ENABLE_LAPACK:BOOL=ON                                     \
        -DLAPACK_LIBRARY_DIRS:PATH="${OPENBLAS_LIB}"                    \
        -DLAPACK_LIBRARY_NAMES:STRING="openblas"                        \
%endif
%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm"
        -DTrilinos_EXTRA_LINK_FLAGS:STRING="-lflang"                    \
%else
        -DTrilinos_EXTRA_LINK_FLAGS:STRING="-lgfortran"                 \
%endif
%endif
        -DTrilinos_ENABLE_MueLu:BOOL=ON                                 \
        -DTrilinos_ENABLE_Phalanx:BOOL=ON                               \
        -DTrilinos_ENABLE_Stokhos:BOOL=ON                               \
        -DTrilinos_ENABLE_Didasko:BOOL=ON                               \
        -DTrilinos_ENABLE_TrilinosCouplings:BOOL=ON                     \
        -DTrilinos_ENABLE_PyTrilinos:BOOL=OFF                           \
        -DTrilinos_ENABLE_CTrilinos:BOOL=ON                             \
%if 0%{?suse_version} >= 1210
        -DTrilinos_ENABLE_ForTrilinos:BOOL=ON                           \
%endif
        -DTrilinos_ENABLE_EXAMPLES:BOOL=OFF                             \
        -DTrilinos_ENABLE_STK:BOOL=OFF                                  \
        -DTrilinos_ENABLE_TESTS:BOOL=OFF                                \
        -DTrilinos_ENABLE_OpenMP:BOOL=ON                                \
        -DTrilinos_ENABLE_EXPLICIT_INSTANTIATION:BOOL=ON                \
        -DTEUCHOS_ENABLE_expat:BOOL=ON                                  \
        -DTEUCHOS_ENABLE_expat:BOOL=ON                                  \
        -DTEUCHOS_ENABLE_libxml2:BOOL=ON                                \
        -DTEUCHOS_ENABLE_gmp:BOOL=ON                                    \
        -DTPL_ENABLE_MPI:BOOL=ON                                        \
        -DMPI_C_COMPILER:FILEPATH=mpicc                                 \
        -DMPI_CXX_COMPILER:FILEPATH=mpicxx                              \
        -DMPI_FORTRAN_COMPILER:FILEPATH=mpif90                          \
        -DTPL_ENABLE_Netcdf:BOOL=ON                                     \
        -DNetcdf_INCLUDE_DIRS:PATH="${NETCDF_INC}"                      \
        -DNetcdf_LIBRARY_DIRS:PATH="${NETCDF_LIB}"                      \
        -DTPL_ENABLE_HDF5:BOOL=ON                                       \
        -DHDF5_INCLUDE_DIRS:PATH="${HDF5_INC}"                          \
        -DHDF5_LIBRARY_DIRS:PATH="${HDF5_LIB}"                          \
        -DHDF5_LIBRARY_NAMES:STRING="hdf5"                              \
        -DTPL_ENABLE_Boost:BOOL=ON                                      \
        -DBOOST_INCLUDE_DIRS:PATH="${BOOST_INC}"                        \
        -DBOOST_LIBRARY_DIRS:PATH="${BOOST_LIB}"                        \
        -DBOOST_LIBRARY_NAMES:STRING="boost"                            \
        -DTPL_ENABLE_Pthread:BOOL=ON                                    \
        -DTPL_ENABLE_CppUnit:BOOL=OFF                                   \
        -DTPL_ENABLE_Zlib:BOOL=ON                                       \
        -DTPL_ENABLE_QT:BOOL=OFF                                        \
        -DTPL_ENABLE_Matio=OFF                                          \
        -DTPL_ENABLE_GLM=OFF                                            \
        ..

make %{?_smp_mflags} VERBOSE=1
cd ..

%install
%ohpc_setup_compiler
cd build
make %{?_smp_mflags} DESTDIR=%{buildroot} install INSTALL='install -p'
cd ..

# fix unversioned python interpreter
sed -e "s,/env python,/python3,g" -i %{buildroot}%{install_path}/bin/phalanx_create_evaluator.py

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version                     %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_LIB        %{install_path}/lib

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
# Autoload openblas for gnu and llvm builds
depends-on openblas
%endif

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
%doc INSTALL README RELEASE_NOTES
%license Copyright.txt LICENSE

