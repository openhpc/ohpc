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
# hack to install MKL for the moment
BuildRequires: intel-compilers%{PROJ_DELIM}
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
Group:          fsp/parallel-libs
Version:        3.5.3
Release:        0

Source0:        %{pname}-%{version}.tar.gz
Source1:        FSP_macros
Source2:        FSP_setup_compiler
Source3:        FSP_setup_mpi
Patch1:         petsc.rpath.patch
Patch2:         petsc.usrlocal.patch
Url:            http://www-unix.mcs.anl.gov/petsc/petsc-as/
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
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
%patch1 -p1
%patch2 -p1


%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi

module load phdf5

# Enable MKL linkage for blas/lapack with gnu builds
%if %{compiler_family} == gnu
module load mkl
%endif

# icc-impi requires mpiicc wrappers, otherwise dynamic libs are not genereated
# gnu-impi finds include/4.8.0/mpi.mod first, unless told not to
./config/configure.py \
        --prefix=%{install_path} \
%if %{compiler_family} == intel
        --FFLAGS="-fPIC" \
%endif
        --with-blas-lapack-dir=$MKLROOT/lib/intel64 \
%if %{mpi_family} == impi
%if %{compiler_family} == intel
        --with-cc=mpiicc    \
        --with-cxx=mpiicpc  \
        --with-fc=mpiifort  \
        --with-f77=mpiifort \
%else
        --FFLAGS=-I$MPI_DIR/include/gfortran/4.8.0/ \
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
        --with-hdf5-include=$HDF5_INC || cat configure.log

make

%install

make install DESTDIR=$RPM_BUILD_ROOT/%{install_path}

# remove buildroot
for f in $RPM_BUILD_ROOT%{install_path}/conf/*; do
    sed -i -e 's!%{buildroot}!!g' $f
done

# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the PETSc library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr " "
puts stderr "Note that this build of PETSc leverages the FFTW and parallel HDF libraries."
puts stderr "Consequently, these packages are loaded automatically with this module."

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version                     %{version}

# Require phdf5 and fftw (and mkl for gnu compiler families)

if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if {  ![is-loaded phdf5]  } {
        module load phdf5
    }
    if {  ![is-loaded fftw]  } {
        module load fftw
    }
    if { [is-loaded gnu] } {
        if { ![is-loaded mkl]  } {
          module load mkl
        }
    }
}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib
prepend-path    LD_LIBRARY_PATH     %{MKLROOT}/lib/intel64

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_LIB        %{install_path}/lib

EOF

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig


%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog
