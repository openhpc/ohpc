##
# netcdf spec file 
# netcdf library build that is dependent on compiler toolchain
#
# Copyright (c) 2015
#
#-fsp-header-comp-begin----------------------------------------------

%include %{_sourcedir}/FSP_macros

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?PROJ_DELIM:      %define PROJ_DELIM %{nil}}
  

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM}
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers%{PROJ_DELIM}
%if 0%{?FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif

#-fsp-header-comp-end------------------------------------------------


# Base package name

%define pname netcdf
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

%define ncdf_so_major 7

Name:           %{pname}-%{compiler_family}%{PROJ_DELIM}
Summary:        Libraries for the Unidata network Common Data Form
License:        NetCDF
Group:          System/Libraries
Version:        4.3.2
Release:        1
Url:            http://www.unidata.ucar.edu/software/netcdf/
Source0:	%{pname}-%{version}.tar.gz
Source1:        nc-config.1.gz
Source101:	FSP_macros
Source102:	FSP_setup_compiler

BuildRoot:	%{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires:  gawk
BuildRequires:  valgrind%{PROJ_DELIM}
BuildRequires:  hdf5-%{compiler_family}%{PROJ_DELIM}
BuildRequires:  libcurl-devel >= 7.18.0
BuildRequires:  pkg-config
BuildRequires:  zlib-devel >= 1.2.5
Requires:       hdf5-%{compiler_family}%{PROJ_DELIM}

#!BuildIgnore: post-build-checks rpmlint-Factory

%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%{pname}/%version

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

   o Sharable:  One writer and multiple readers may simultaneously
     access the same NetCDF file.


%prep
%setup -q -n %{pname}-%{version}

%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family} 
. %{_sourcedir}/FSP_setup_compiler

module load hdf5
export CFLAGS="-L$HDF5_LIB -I$HDF5_INC"
 

./configure --prefix=%{install_path} \
    --enable-shared \
    --enable-netcdf-4 \
    --enable-dap \
    --enable-ncgen4 \
    --enable-extra-example-tests \
    --disable-dap-remote-tests \
    --with-pic \
    --disable-doxygen \
    --disable-static || cat config.log

%install
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family} 
. %{_sourcedir}/FSP_setup_compiler


module load hdf5
export CFLAGS="-L$HDF5_LIB -I$HDF5_INC"

make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# Remove static libraries
find "%buildroot" -type f -name "*.la" | xargs rm -f

# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the NetCDF library built with the %{compiler_family} compiler toolchain."
puts stderr " "
puts stderr "Note that this build of NetCDF is built on top of the HDF library and requires linkage"
puts stderr "against hdf5. Consequently, the hdf5 package is loaded automatically as part of this module."
puts stderr "In addition, the C++ and Fortran interface versions of NetCDF are also loaded with this"
puts stderr "module. Typical compilation steps for C, C++, and Fortran applications are exemplified below:"
puts stderr " "
puts stderr "  C:   \$CC  -I\$NETCDF_INC app.c -L\$NETCDF_LIB -lnetcdf -L\$HDF5_LIB -lhdf5"
puts stderr "  C++: \$CXX -I\$NETCDF_CXX_INC app.cpp -L\$NETCDF_CXX_LIB -lnetcdf_c++ -L\$NETCDF_LIB -lnetcdf -L\$HDF5_LIB -lhdf5"
puts stderr "  F90: \$FC  -I\$NETCDF_FORTRAN_INC app.f90 -L\$NETCDF_FORTRAN_LIB -lnetcdff -L\$NETCDF_LIB -lnetcdf -L\$HDF5_LIB -lhdf5"
puts stderr " "
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set             version             %{version}

# Require hdf5, netcdf-cxx, and netcdf-fortran

if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if {  ![is-loaded hdf5]  } {
        module load hdf5
    }
    if {  ![is-loaded netcdf-cxx]  } {
        module load netcdf-cxx
    }
    if {  ![is-loaded netcdf-fortran]  } {
        module load netcdf-fortran
    }
}


prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog
