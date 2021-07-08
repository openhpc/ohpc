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
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname netcdf-fortran

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:        Fortran Libraries for the Unidata network Common Data Form
License:        NetCDF
Group:          %{PROJ_NAME}/io-libs
Version:        4.5.3
Release:        1%{?dist}
Url:            http://www.unidata.ucar.edu/software/netcdf/
Source0:	https://github.com/Unidata/netcdf-fortran/archive/v%{version}.tar.gz

BuildRequires:  zlib-devel >= 1.2.5
BuildRequires:  libcurl-devel m4
BuildRequires:  phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM} >= 1.8.8
BuildRequires:  netcdf-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       netcdf-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       lmod%{PROJ_DELIM} >= 7.6.1

#!BuildIgnore: post-build-checks rpmlint-Factory

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
The Unidata network Common Data Form (netCDF) is an interface for scientific
data access and a freely-distributed software library that provides an
implementation of the interface. The netCDF library also defines a
machine-independent format for representing scientific data. Together, the
interface,library, and format support the creation, access, and sharing of
scientific data.

NetCDF files are self-describing, network-transparent, directly accessible, and
extendible. Self-describing means that a netCDF file includes information about
the data it contains. Network-transparent means that a netCDF file is
represented in a form that can be accessed by computers with different ways of
storing integers, characters, and floating-point numbers. Direct-access means
that a small subset of a large dataset may be accessed efficiently, without
first reading through all the preceding data. Extendible means that data can be
appended to a netCDF dataset without copying it or redefining its structure.

This software package provides Fortran application interfaces for accessing
netCDF data. It depends on the netCDF C library, which must be installed first.


%prep
%setup -q -n %{pname}-%{version}


%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load phdf5
module load netcdf

mkdir -p build
cd build

export CFLAGS="-L$HDF5_LIB -I$HDF5_INC -L$NETCDF_LIB -I$NETCDF_INC"
export CXXFLAGS="-L$HDF5_LIB -I$HDF5_INC -L$NETCDF_LIB -I$NETCDF_INC"
export FCFLAGS="-L$HDF5_LIB -I$HDF5_INC -L$NETCDF_LIB -I$NETCDF_INC"
export CPPFLAGS="-I$HDF5_INC -I$NETCDF_INC"
export LDFLAGS="-L$HDF5_LIB -L$NETCDF_LIB"

cmake -DCMAKE_PREFIX_PATH="%{install_path}" \
      -DCMAKE_INSTALL_PREFIX="%{buildroot}%{install_path}" \
      -DCMAKE_INSTALL_LIBDIR:PATH=lib \
      -DENABLE_DOXYGEN=OFF \
      -DCMAKE_EXE_LINKER_FLAGS:STRING="-fPIC" \
      -DCMAKE_VERBOSE_MAKEFILE:BOOL=TRUE \
      -DCMAKE_BUILD_TYPE:STRING=RELEASE \
      -DCMAKE_SKIP_INSTALL_RPATH:BOOL=YES \
      -DCMAKE_SKIP_RPATH:BOOL=YES \
      -DBUILD_SHARED_LIBS=ON ..

%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm"
sed -i -e 's#wl=""#wl="-Wl,"#g' libtool
sed -i -e 's#pic_flag=""#pic_flag=" -fPIC -DPIC"#g' libtool
%endif

make %{?_smp_mflags}


%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

cd build
make install

# OpenHPC module file
mkdir -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
cat << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the NetCDF Fortran API built with the %{compiler_family} compiler toolchain."
puts stderr " "
puts stderr "Note that this build of NetCDF leverages the HDF I/O library and requires linkage"
puts stderr "against hdf5 and the native C NetCDF library. Consequently, phdf5 and the standard C"
puts stderr "version of NetCDF are loaded automatically via this module. A typical compilation"
puts stderr "example for Fortran applications requiring NetCDF is as follows:"
puts stderr " "
puts stderr "\\\$FC  -I\\\$NETCDF_FORTRAN_INC app.f90 -L\\\$NETCDF_FORTRAN_LIB -lnetcdff -L\\\$NETCDF_LIB -lnetcdf -L\$HDF5_LIB -lhdf5"

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

depends-on netcdf

set             version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include
EOF

cat << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

mkdir -p ${RPM_BUILD_ROOT}/%{_docdir}


%files
%{install_path}
%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%license COPYRIGHT F03Interfaces_LICENSE
%doc README.md
