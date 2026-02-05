#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# hdf5/phdf5 - A general purpose library and file format for storing scientific data
# Build that is dependent on compiler and conditionally the mpi toolchains
%define ohpc_compiler_dependent 1
%{!?ohpc_mpi_dependent:%define ohpc_mpi_dependent 1}
%include %{_sourcedir}/OHPC_macros

# Base package name
%define base_pname hdf5
%define BASE_PNAME %(echo %{base_pname} | tr [a-z] [A-Z] | tr - _)

%if 0%{?ohpc_mpi_dependent}
%define pname p%{base_pname}
Name:      %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:   A general purpose library and file format for storing scientific data (parallel version)
%else
%define pname %{base_pname}
Name:      %{pname}-%{compiler_family}%{PROJ_DELIM}
Summary:   A general purpose library and file format for storing scientific data
%endif
Version:   2.0.0
Release:   1%{?dist}
License:   Hierarchical Data Format (HDF) Software Library and Utilities License
Group:     %{PROJ_NAME}/io-libs
URL:       http://www.hdfgroup.org/HDF5

Source0:   https://github.com/HDFGroup/%{base_pname}/releases/download/%{version}/%{base_pname}-%{version}.tar.gz

BuildRequires: zlib-devel make cmake

#!BuildIgnore: post-build-checks rpmlint-Factory

# Default library install path
%if 0%{?ohpc_mpi_dependent}
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%{version}
%else
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%{version}
%endif

%description
HDF5 is a general purpose library and file format for storing scientific data.
HDF5 can store two primary objects: datasets and groups. A dataset is
essentially a multidimensional array of data elements, and a group is a
structure for organizing objects in an HDF5 file. Using these two basic
objects, one can create and store almost any kind of scientific data
structure, such as images, arrays of vectors, and structured and unstructured
grids. You can also mix and match them in HDF5 files according to your needs.

%prep
%setup -q -n %{base_pname}-%{version}

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

# Clean ccache prefix from compiler variables if present
export CC=$(echo $CC | sed 's/^ccache //')
export CXX=$(echo $CXX | sed 's/^ccache //')

%if 0%{?ohpc_mpi_dependent}
export CC=mpicc
export CXX=mpicxx
export FC=mpif90
%endif

mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=%{install_path} \
      -DCMAKE_BUILD_TYPE=Release             \
      -DBUILD_SHARED_LIBS=ON                 \
      -DBUILD_STATIC_LIBS=OFF                \
      -DHDF5_BUILD_FORTRAN=ON                \
%if "%{?OHPC_USE_CCACHE}" == "yes"
      -DCMAKE_C_COMPILER_LAUNCHER=ccache     \
      -DCMAKE_CXX_COMPILER_LAUNCHER=ccache   \
%endif
%if 0%{?ohpc_mpi_dependent}
      -DHDF5_ENABLE_PARALLEL=ON              \
%else
      -DHDF5_BUILD_CPP_LIB=ON                \
%endif
      ..

make %{?_smp_mflags}

%install

# OpenHPC compiler designation
%ohpc_setup_compiler

export NO_BRP_CHECK_RPATH=true

cd build
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# Remove static libraries
find "%buildroot" -type f -name "*.la" | xargs rm -f

# OpenHPC module file
%if 0%{?ohpc_mpi_dependent}
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
%else
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
%endif
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
%if 0%{?ohpc_mpi_dependent}
puts stderr "This module loads the parallel %{BASE_PNAME} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
%else
puts stderr "This module loads the %{BASE_PNAME} library built with the %{compiler_family} compiler toolchain."
%endif
puts stderr "\nVersion %{version}\n"

}
%if 0%{?ohpc_mpi_dependent}
module-whatis "Name: %{BASE_PNAME} built with %{compiler_family} compiler and %{mpi_family} MPI"
%else
module-whatis "Name: %{BASE_PNAME} built with %{compiler_family} toolchain"
%endif
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{BASE_PNAME}_DIR        %{install_path}
setenv          %{BASE_PNAME}_BIN        %{install_path}/bin
setenv          %{BASE_PNAME}_LIB        %{install_path}/lib
setenv          %{BASE_PNAME}_INC        %{install_path}/include

family "hdf5"

EOF

%if 0%{?ohpc_mpi_dependent}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
%else
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
%endif
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%{OHPC_PUB}
%doc README.md
