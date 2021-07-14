#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build depends on compiler/mpi toolchains
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname netcdf

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:        C Libraries for the Unidata network Common Data Form
License:        NetCDF
Group:          %{PROJ_NAME}/io-libs
Version:        4.8.0
Release:        1%{?dist}
Url:            http://www.unidata.ucar.edu/software/netcdf/
Source0:	https://github.com/Unidata/netcdf-c/archive/v%{version}.tar.gz

BuildRequires:  curl-devel m4 make
BuildRequires:  zlib-devel >= 1.2.5
BuildRequires:  cmake%{PROJ_DELIM}
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires:  phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

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

This software package provides C interfaces for applications and data.


%prep
%setup -q -n %{pname}-c-%{version}


%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load cmake
module load phdf5

mkdir -p ./build
cd build

export CPPFLAGS="-I$HDF5_INC -I$MPI_DIR/include"
export LDFLAGS="-L$HDF5_LIB"
export CFLAGS="$CPPFLAGS $LDFLAGS"
export CC=mpicc

cmake -DCMAKE_PREFIX_PATH="%{install_path}" \
      -DCMAKE_INSTALL_PREFIX="%{buildroot}%{install_path}" \
      -DCMAKE_INSTALL_LIBDIR:PATH=lib \
      -DENABLE_NETCDF_4=ON \
      -DENABLE_HDF5=ON \
      -DENABLE_DAP=ON \
      -DENABLE_DOXYGEN=OFF \
      -DCMAKE_EXE_LINKER_FLAGS:STRING="-fPIC" \
      -DCMAKE_VERBOSE_MAKEFILE:BOOL=TRUE \
      -DCMAKE_BUILD_TYPE:STRING=RELEASE \
      -DCMAKE_SKIP_RPATH:BOOL=YES \
      -DBUILD_UTILITIES=ON \
      -DBUILD_SHARED_LIBS=ON ..

# Reported problems with parallel build on older versions
# Tested this at -j16 with no errors (JCS-7/8/21)
make %{?_smp_mflags}


%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

cd build
make install

# Correct absolute paths added during make install
for f in $(grep -Ilr "BUILDROOT" %{buildroot}%{install_path}); do
   sed -i "s,%{buildroot},," $f
done

# OpenHPC module file
mkdir -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
cat << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the NetCDF C API built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr " "
puts stderr "Note that this build of NetCDF leverages the HDF I/O library and requires linkage"
puts stderr "against hdf5. Consequently, the phdf5 package is loaded automatically with this module."
puts stderr "A typical compilation step for C applications requiring NetCDF is as follows:"
puts stderr " "
puts stderr "\\\$CC -I\\\$NETCDF_INC app.c -L\\\$NETCDF_LIB -lnetcdf -L\\\$HDF5_LIB -lhdf5"

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set             version             %{version}

depends-on phdf5

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

mkdir -p ${buildroot}/%{_docdir}


%files
%dir %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}
%{install_path}
%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%license COPYRIGHT
%doc README.md
