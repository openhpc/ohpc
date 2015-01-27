# petsc library that is is depdendent on compiler toolchain and MPI

#-fsp-header-comp-begin-----------------------------

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

#-fsp-header-comp-end-------------------------------

# Base package name
%define pname petsc
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:        Portable Extensible Toolkit for Scientific Computation
License:        MIT
Group:          Development/Libraries/C and C++
Version:        3.5.2
Release:        0

Source0:        %{pname}-%{version}.tar.gz
Source1:        FSP_macros
Source2:        FSP_setup_compiler
Source3:        FSP_setup_mpi
#Patch0:         petsc-3.3-p2-fix-shared-libs-sonames.patch
Patch1:         petsc.rpath.patch
Patch2:         petsc.usrlocal.patch
#Patch2:         petsc-3.3-p2-dont-check-for-option-mistakes.patch
#Patch3:         petsc-3.3-fix-error-detection-in-makefile.patch 
Url:            http://www-unix.mcs.anl.gov/petsc/petsc-as/
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
BuildRequires:  blas-devel
BuildRequires:  lapack-devel
BuildRequires:  phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  python
BuildRequires:  valgrind%{PROJ_DELIM}
BuildRequires:  xz
BuildRequires:  zlib-devel

%include %{_sourcedir}/FSP_macros
#!BuildIgnore: post-build-checks

%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
PETSc is a suite of data structures and routines for the scalable
(parallel) solution of scientific applications modeled by partial 
differential equations.

%prep
%setup -q -n %{pname}-%{version}
#%patch0 -p1 -b .soname
%patch1 -p1
%patch2 -p1
#%patch2 -p1 -b .option-mistakes
#%patch3 -p1 -b .error-detect


%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi
module load phdf5

./config/configure.py \
	--prefix=%{install_path} \
%if %{compiler_family} == intel
    --FFLAGS="-fPIC" \
    --with-blas-lapack-dir=$MKLROOT/lib/intel64 \
%endif
%if %{mpi_family} == impi
%if %{compiler_family} == intel
    --with-cc=mpiicc \
    --with-cxx=mpiicpc \
    --with-fc=mpiifort \
    --with-f77=mpiifort \
%else
    --with-mpiuni-fortran-binding=0
%endif
%endif
    --with-clanguage=C++ \
    --with-c-support \
	--with-fortran-interfaces=1 \
	--with-debugging=no \
 	--with-shared-libraries \
	--with-mpi=1 \
	--with-batch=0 \
    --with-hdf5=1 \
    --with-hdf5-lib=$HDF5_LIB/libhdf5.so \
    --with-hdf5-include=$HDF5_INC \
	#--CFLAGS="$RPM_OPT_FLAGS" \
	#--FFLAGS="$RPM_OPT_FLAGS" \
	#--FFLAGS="-fPIC $RPM_OPT_FLAGS" \
	#--CXXFLAGS="$RPM_OPT_FLAGS" \
make

%install

make install DESTDIR=$RPM_BUILD_ROOT/%{install_path}

# remove buildroot
for f in $RPM_BUILD_ROOT%{install_path}/conf/*; do
    sed -i -e 's!%{buildroot}!!g' $f
done

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

set     version                     %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

family "petsc"
EOF

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig


%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog
