#
# spec file for package parallel-netcdf
#
# Copyright (c) 2014 
#

#-fsp-header-comp-begin----------------------------------------------

%include %{_sourcedir}/FSP_macros

# FSP convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family and mpi_family variables via rpmbuild or other
# mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family: %define mpi_family openmpi}
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM} coreutils
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

# MPI dependencies
%if %{mpi_family} == impi
BuildRequires: intel-mpi%{PROJ_DELIM}
Requires:      intel-mpi%{PROJ_DELIM}
%endif
%if %{mpi_family} == mvapich2
BuildRequires: mvapich2-%{compiler_family}%{PROJ_DELIM}
Requires:      mvapich2-%{compiler_family}%{PROJ_DELIM}
%endif
%if %{mpi_family} == openmpi
BuildRequires: openmpi-%{compiler_family}%{PROJ_DELIM}
Requires:      openmpi-%{compiler_family}%{PROJ_DELIM}
%endif

#-fsp-header-comp-end------------------------------------------------


# Base package name

%define pname parallel-netcdf
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        1.5.0
Release:        1
Summary:        A library providing high-performance I/O with Unidata's NetCDF
License:        GPL-2.0
Group:          Development/Libraries/Parallel
Url:            http://trac.mcs.anl.gov/projects/parallel-netcdf/
Source0:        %{pname}-%{version}.tar.gz
BuildRequires:  bison
BuildRequires:  flex
#BuildRequires:  gcc-c++
#BuildRequires:  gcc-fortran
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root

#!BuildIgnore: post-build-checks rpmlint-Factory

 
%define debug_package %{nil}
 
# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version


%description
Parallel netCDF (PnetCDF) is a library providing high-performance I/O while
still maintaining file-format compatibility with Unidata's NetCDF.


%prep
%setup -q -n %{pname}-%{version}

%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi


#** export CXXFLAGS="-L$HDF5_LIB -I$HDF5_INC -L$NETCDF_LIB -I$NETCDF_INC"
export MPICC=mpicc MPICXX=mpicxx MPIF77=mpif77 MPIF90=mpif90 
export CFLAGS="-O2" FFLAGS="-O2" FCFLAGS="-O2"

./configure --prefix=%{buildroot}/%{install_path} --disable-static --enable-shared || cat config.log

make

%install
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi


#** export CXXFLAGS="-L$HDF5_LIB -I$HDF5_INC -L$NETCDF_LIB -I$NETCDF_INC"
export MPICC=mpicc MPICXX=mpicxx MPIF77=mpif77 MPIF90=mpif90 
export CFLAGS="-O2" FFLAGS="-O2" FCFLAGS="-O2"
 
export NO_BRP_CHECK_RPATH=true

#cd %buildroot
make DESTDIR=$RPM_BUILD_ROOT install

# Remove static libraries
find "%buildroot" -type f -name "*.la" | xargs rm -f

 
# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
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


set             version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include


EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig
 
%postun -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog
