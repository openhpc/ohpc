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

%{!?compiler_family: %define compiler_family gnu   }
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}
 
%define PROJ_DELIM -fsp

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
Patch0:         %{pname}-correct_casting.patch
Patch1:         %{pname}-codecleanup.patch
Patch2:         %{pname}-no_date_time.patch
#Strip FFLAGS from nc-config
#Use pkgconfig in nc-config to avoid multi-lib issues
Patch3:         netcdf-pkgconfig.patch
#Strip FFLAGS from nc-config
BuildRoot:	%{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires:  gawk
#BuildRequires:  gcc-c++
#BuildRequires:  gcc-fortran
BuildRequires:  hdf5-%{compiler_family}%{PROJ_DELIM} 
BuildRequires:  libcurl-devel >= 7.18.0
BuildRequires:  pkg-config
BuildRequires:  zlib-devel >= 1.2.5
BuildRequires:  valgrind%{PROJ_DELIM}
Requires:       hdf5-%{compiler_family}%{PROJ_DELIM} 

#!BuildIgnore: post-build-checks rpmlint-Factory

%include %{_sourcedir}/FSP_macros

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
%patch0 -p1 -b .correct_casting
%patch1 -p1 -b .codecleanup
%patch2 -p0 -b .no_date_time
%patch3 -p1 -b .pkgconfig

%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family} 
. %{_sourcedir}/FSP_setup_compiler

module load hdf5-%{compiler_family}%{PROJ_DELIM}
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
    --enable-static || cat config.log
# %%ifnarch s390 s390x
#            --enable-valgrind-tests \
# %%endif


%install
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family} 
. %{_sourcedir}/FSP_setup_compiler


module load hdf5-%{compiler_family}%{PROJ_DELIM}
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
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set             version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

family "netcdf"
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

#**%check  ... Disabling make check during OBS build
#**make check

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%{FSP_HOME}
#** %doc COPYRIGHT README.md RELEASE_NOTES.md
#**  %{_bindir}/*
#** %{_bindir}/nc-config
#** %exclude %{_bindir}/nc-config
#** %{_mandir}/man1/*
#** %{_mandir}/man3/*
#** %exclude %{_mandir}/man1/*.1.gz
#** %{_includedir}/*
#** %{_libdir}/*.so
#** %{_libdir}/pkgconfig/netcdf.pc
#** %{_libdir}/libnetcdf.so.*
#** %{_libdir}/libnetcdf.a



%changelog
