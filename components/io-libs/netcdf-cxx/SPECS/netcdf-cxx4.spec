#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is dependent on compiler and conditionally the mpi toolchains
%define ohpc_compiler_dependent 1
%{!?ohpc_mpi_dependent:%define ohpc_mpi_dependent 1}
%include %{_sourcedir}/OHPC_macros
# %{_build_id_links}
# Base package name

%define pname netcdf-cxx

Name:           %{ohpc_name}
Summary:        C++ Libraries for the Unidata network Common Data Form
License:        NetCDF
Group:          %{PROJ_NAME}/io-libs
Version:        4.3.1
Release:        1%{?dist}
Url:            http://www.unidata.ucar.edu/software/netcdf/
Source0:        https://github.com/Unidata/netcdf-cxx4/archive/v%{version}.tar.gz

BuildRequires:  zlib-devel >= 1.2.5
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
%if 0%{?ohpc_mpi_dependent}
BuildRequires:  phdf5%{ohpc_suffix}
%else
BuildRequires:  hdf5%{ohpc_suffix}
%endif
BuildRequires:  netcdf%{ohpc_suffix}
Requires:       netcdf%{ohpc_suffix}


#!BuildIgnore: post-build-checks rpmlint-Factory

%description
NetCDF (network Common Data Form) is an interface for array-oriented
data access and a freely-distributed collection of software libraries
for C, Fortran, C++, and perl that provides an implementation of the
interface.  The NetCDF library also defines a machine-independent
format for representing scientific data.  Together, the interface,
library, and format support the creation, access, and sharing of
scientific data. The NetCDF software was developed at the Unidata
Program Center in Boulder, Colorado.

NetCDF data is:

   o Self-Describing: A NetCDF file includes information about the
     data it contains.

   o Network-transparent:  A NetCDF file is represented in a form that
     can be accessed by computers with different ways of storing
     integers, characters, and floating-point numbers.

   o Direct-access:  A small subset of a large dataset may be accessed
     efficiently, without first reading through all the preceding
     data.

   o Appendable:  Data can be appended to a NetCDF dataset along one
     dimension without copying the dataset or redefining its
     structure. The structure of a NetCDF dataset can be changed,
     though this sometimes causes the dataset to be copied.

   o Shareable:  One writer and multiple readers may simultaneously
     access the same NetCDF file.


%prep
%setup -q -n %{pname}4-%{version}

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if 0%{?ohpc_mpi_dependent}
module load phdf5
%else
module load hdf5
%endif
module load netcdf

export CPPFLAGS="-I$HDF5_INC -I$NETCDF_INC"
export LDFLAGS="-L$HDF5_LIB -L$NETCDF_LIB"

./configure --prefix=%{ohpc_install_path} \
    --enable-shared \
    --enable-netcdf-4 \
    --enable-dap \
    --enable-ncgen4 \
    --with-pic \
    --disable-doxygen \
    --disable-filter-testing \
    --disable-static || { cat config.log && exit 1; }

make %{?_smp_mflags}

%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if 0%{?ohpc_mpi_dependent}
module load phdf5
%else
module load hdf5
%endif
module load netcdf

make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{ohpc_modulepath}/%{pname}
%{__cat} << EOF > %{buildroot}/%{ohpc_modulepath}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the NetCDF C++ API built with the %{compiler_family} compiler toolchain."
puts stderr " "
puts stderr "Note that this build of NetCDF leverages the HDF I/O library and requires linkage"
%if 0%{?ohpc_mpi_dependent}
puts stderr "against hdf5 and the native C NetCDF library. Consequently, phdf5 and the standard C"
%else
puts stderr "against hdf5 and the native C NetCDF library. Consequently, hdf5 and the standard C"
%endif
puts stderr "version of NetCDF are loaded automatically via this module. A typical compilation"
puts stderr "example for C++ applications requiring NetCDF is as follows:"
puts stderr " "
puts stderr "\\\$CXX -I\\\$NETCDF_CXX_INC app.cpp -L\\\$NETCDF_CXX_LIB -lnetcdf_c++4 -L\\\$NETCDF_LIB -lnetcdf -L\\\$HDF5_LIB -lhdf5"

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

depends-on netcdf

set             version             %{version}

prepend-path    PATH                %{ohpc_install_path}/bin
prepend-path    MANPATH             %{ohpc_install_path}/share/man
prepend-path    INCLUDE             %{ohpc_install_path}/include
prepend-path    LD_LIBRARY_PATH     %{ohpc_install_path}/lib

setenv          %{PNAME}_DIR        %{ohpc_install_path}
setenv          %{PNAME}_BIN        %{ohpc_install_path}/bin
setenv          %{PNAME}_LIB        %{ohpc_install_path}/lib
setenv          %{PNAME}_INC        %{ohpc_install_path}/include
EOF

%{__cat} << EOF > %{buildroot}/%{ohpc_modulepath}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%{OHPC_PUB}
%doc COPYRIGHT

