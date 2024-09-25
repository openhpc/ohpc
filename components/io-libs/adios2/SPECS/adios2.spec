#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is dependent on compiler/mpi toolchains
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%define ohpc_python_dependent 1
%global python_family python3
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname adios2

Summary: The Adaptable IO System v2 (ADIOS2)
Name:    %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version: 2.8.3
Release: 1%{?dist}
License: Apache License 2.0
Group:   %{PROJ_NAME}/io-libs
Url:     https://adios2.readthedocs.io/en/latest/index.html
Source0: https://github.com/ornladios/ADIOS2/archive/refs/tags/v%{version}.tar.gz
AutoReq: no

%if 0%{?rhel} || 0%{?openEuler}
BuildRequires:  bzip2-devel
%endif
%if 0%{?suse_version}
BuildRequires:  libbz2-devel
%endif

BuildRequires: libtool cmake make
Requires:      lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires: phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:      phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

BuildRequires: %{python_prefix}-devel %{python_prefix}-setuptools
BuildRequires: %{python_prefix}-numpy-%{compiler_family}%{PROJ_DELIM}
BuildRequires: %{python_prefix}-mpi4py-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

Requires: %{python_prefix}-numpy-%{compiler_family}%{PROJ_DELIM}
Requires: %{python_prefix}-mpi4py-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%{version}

%description
The Adaptable IO System (ADIOS2) provides a simple, flexible way for
scientists to describe the data in their code that may need to be
written, read, or processed outside of the running simulation. By
providing an external to the code XML file describing the various
elements, their types, and how you wish to process them this run, the
routines in the host code (either Fortran or C) can transparently change
how they process the data.

%prep
%setup -q -n %{PNAME}-%{version}

%build
mkdir adios2-build
cd adios2-build

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load phdf5
%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
module load openblas
%endif
module load %{python_module_prefix}numpy
module load %{python_module_prefix}mpi4py

export CFLAGS="${CFLAGS} -Wno-implicit-int"
export CFLAGS="${CFLAGS} -Wno-implicit-function-declaration"
%if "%{compiler_family}" == "arm1" || "%{compiler_family}" == "intel"
export CFLAGS="${CFLAGS} -Wno-incompatible-function-pointer-types"
export CXXFLAGS="${CXXFLAGS} -Wno-implicit-int"
export CXXFLAGS="${CXXFLAGS} -Wno-implicit-function-declaration"
%else
export CFLAGS="${CFLAGS} -Wno-incompatible-pointer-types"
%endif
%if "%{compiler_family}" == "arm1"
export CXXFLAGS="${CXXFLAGS} -fsimdmath"
export CFLAGS="${CFLAGS} -fsimdmath"
%endif

export CC=mpicc
export CXX=mpicxx
export F77=mpif77
export FC=mpif90
export MPICC=mpicc
export MPIFC=mpif90
export MPICXX=mpicxx

cmake \
    -DCMAKE_INSTALL_PREFIX=%{install_path} \
    -DCMAKE_BUILD_TYPE=Release \
    -DBUILD_SHARED_LIBS=ON \
    -DCMAKE_SKIP_INSTALL_RPATH:BOOL=ON \
    -DCMAKE_SKIP_RPATH:BOOL=ON \
    -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
    -DADIOS2_USE_MPI=ON \
    -DADIOS2_USE_HDF5=ON -DHDF5_ROOT="${HDF5_DIR}" \
    -DADIOS2_USE_ZeroMQ=OFF \
    -DADIOS2_USE_Fortran=ON \
    -DADIOS2_USE_Python=ON \
    -DADIOS2_USE_SST=ON \
    -DADIOS2_USE_BZip2=ON \
    -DBUILD_TESTING=OFF \
    -DADIOS2_BUILD_EXAMPLES=OFF \
    -DPYTHON_EXECUTABLE=%{__python} \
    -DPython_FIND_STRATEGY=LOCATION \
    ..
make -j$(nproc)
# make test


%install
# OpenHPC compiler designation
%ohpc_setup_compiler
cd adios2-build
make DESTDIR=$RPM_BUILD_ROOT install

# this is clearly generated someway and shouldn't be static
export PPATH="/lib64/%{python_lib_dir}/site-packages"
export PATH=$(pwd):$PATH


# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set             version             %{version}

depends-on phdf5
depends-on %{python_module_prefix}numpy
depends-on %{python_module_prefix}mpi4py

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib64

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib64
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%{OHPC_HOME}
%doc Copyright.txt
%doc Contributing.md
%doc LICENSE
%doc ReadMe.md
