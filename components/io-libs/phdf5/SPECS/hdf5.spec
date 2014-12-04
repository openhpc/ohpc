# Parallel HDF5 library build that is dependent on compiler
# toolchain and MPI

#-fsp-header-comp-begin----------------------------------------------

# FSP convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family and mpi_family variables via rpmbuild or other
# mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family: %define mpi_family openmpi}

# Compiler dependencies
BuildRequires: lmod coreutils
%if %{compiler_family} == gnu
BuildRequires: FSP-gnu-compilers 
Requires:      FSP-gnu-compilers 
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ FSP-intel-compilers 
Requires:      gcc-c++ FSP-intel-compilers 
%if 0%{?FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif

# MPI dependencies
%if %{mpi_family} == impi
BuildRequires: FSP-intel-mpi
Requires:      FSP-intel-mpi
%endif
%if %{mpi_family} == mvapich2
BuildRequires: FSP-mvapich2-%{compiler_family}
Requires:      FSP-mvapich2-%{compiler_family}
%endif
%if %{mpi_family} == openmpi
BuildRequires: FSP-openmpi-%{compiler_family}
Requires:      FSP-openmpi-%{compiler_family}
%endif

#-fsp-header-comp-end------------------------------------------------

%define _unpackaged_files_terminate_build 0

# Base package name
%define pname hdf5
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary:   A general purpose library and file format for storing scientific data
Name:      p%{pname}-%{compiler_family}-%{mpi_family}
Version:   1.8.13
Release:   0.1
License:   BSD-3-Clause
Group:     Development/Libraries/Other
URL:       http://www.hdfgroup.org/HDF5
Source0:   %{pname}-%{version}.tar.gz
Source1:   FSP_macros
Source2:   FSP_setup_compiler
Source3:   FSP_setup_mpi
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires: zlib-devel

#!BuildIgnore: post-build-checks rpmlint-Factory

%include %{_sourcedir}/FSP_macros

%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
HDF5 is a general purpose library and file format for storing scientific data.
HDF5 can store two primary objects: datasets and groups. A dataset is
essentially a multidimensional array of data elements, and a group is a
structure for organizing objects in an HDF5 file. Using these two basic
objects, one can create and store almost any kind of scientific data
structure, such as images, arrays of vectors, and structured and unstructured
grids. You can also mix and match them in HDF5 files according to your needs.

%prep

%setup -q -n %{pname}-%{version}

%build

# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi

export CC=mpicc 
export CXX=mpicxx 
export F77=mpif77 
export FC=mpif90 
export MPICC=mpicc 
export MPIFC=mpifc 
export MPICXX=mpicxx 

./configure --prefix=%{install_path} \
	    --enable-fortran         \
            --enable-static=no       \
            --enable-parallel        \
	    --enable-shared          \
	    --enable-fortran2003    || cat config.log

%install

# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi

export NO_BRP_CHECK_RPATH=true

make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# Remove static libraries
find "%buildroot" -type f -name "*.la" | xargs rm -f

# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/p%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/p%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the parallel %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

family "hdf5"
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/p%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog


