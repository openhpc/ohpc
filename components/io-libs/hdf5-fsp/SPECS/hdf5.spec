# Serial HDF5 library build that is dependent on compiler toolchain

#-fsp-header-comp-begin----------------------------------------------

%include %{_sourcedir}/FSP_macros

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}

# Compiler dependencies
BuildRequires: lmod coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers-fsp 
Requires:      gnu-compilers-fsp 
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-fsp
Requires:      gcc-c++ intel-compilers-fsp
%if 0%{?FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif

#-fsp-header-comp-end------------------------------------------------

# Base package name
%define pname hdf5
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

# RPM name
%if 0%{?PROJ_NAME:1}
%define rpmname %{pname}-%{compiler_family}-%{PROJ_NAME}
%else
%define rpmname %{pname}-%{compiler_family}
%endif

Summary:   A general purpose library and file format for storing scientific data
Name:      %{rpmname}
Version:   1.8.13
Release:   0.1
License:   BSD-3-Clause
Group:     Development/Libraries/Other
URL:       http://www.hdfgroup.org/HDF5
Source0:   %{pname}-%{version}.tar.gz
Source1:   FSP_macros
Source2:   FSP_setup_compiler
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root

BuildRequires: zlib-devel
Requires:      zlib

#!BuildIgnore: post-build-checks rpmlint-Factory

%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%{pname}/%version

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
. %{_sourcedir}/FSP_setup_compiler

./configure --prefix=%{install_path} \
	    --enable-fortran         \
            --enable-static=no       \
	    --enable-shared          \
	    --enable-cxx             \
	    --enable-fortran2003    || cat config.log

%install

# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

export NO_BRP_CHECK_RPATH=true

make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# Remove static libraries
find "%buildroot" -type f -name "*.la" | xargs rm -f
find "%buildroot"

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

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
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

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog


