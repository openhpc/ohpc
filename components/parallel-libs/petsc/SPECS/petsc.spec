#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# PETSc library that is is dependent on compiler toolchain and MPI
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

%if "%{compiler_family}" != "intel"
BuildRequires: scalapack-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:      scalapack-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
%endif

# Base package name
%define pname petsc
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:        Portable Extensible Toolkit for Scientific Computation
License:        2-clause BSD
Group:          %{PROJ_NAME}/parallel-libs
Version:        3.7.6
Release:        1%{?dist}
Source0:        http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/petsc-%{version}.tar.gz
Source1:        OHPC_macros
Patch1:         petsc.rpath.patch
Url:            http://www.mcs.anl.gov/petsc/
BuildRequires:  phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  python
BuildRequires:  valgrind%{PROJ_DELIM}
BuildRequires:  xz
BuildRequires:  zlib-devel

#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
PETSc is a suite of data structures and routines for the scalable
(parallel) solution of scientific applications modeled by partial
differential equations.

%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1


%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load phdf5

# Enable scalapack linkage for blas/lapack with gnu builds
%if "%{compiler_family}" != "intel"
module load scalapack openblas
%endif

# icc-impi requires mpiicc wrappers, otherwise dynamic libs are not generated.
# gnu-impi finds include/4.8.0/mpi.mod first, unless told not to.
./config/configure.py \
        --prefix=%{install_path} \
%if %{compiler_family} == intel
        --FFLAGS="-fPIC" \
        --with-blas-lapack-dir=$MKLROOT/lib/intel64 \
%else
        --CFLAGS="-fPIC -DPIC" \
        --CXXFLAGS="-fPIC -DPIC" \
        --FFLAGS="-fPIC" \
        --with-blas-lapack-lib=$OPENBLAS_LIB/libopenblas.so \
        --with-scalapack-dir=$SCALAPACK_DIR \
%endif
%if "%{mpi_family}" == "impi"
%if "%{compiler_family}" == "intel"
        --with-cc=mpiicc    \
        --with-cxx=mpiicpc  \
        --with-fc=mpiifort  \
        --with-f77=mpiifort \
%else
        --FFLAGS=-I$I_MPI_ROOT/include64/gfortran/4.9.0/ \
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

rm %{buildroot}%{install_path}/lib/petsc/conf/configure.log

# remove stock module file
rm -rf %{buildroot}%{install_path}/lib/modules

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the PETSc library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr " "

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version                     %{version}

# Require phdf5 (and scalapack for gnu compiler families)

if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if {  ![is-loaded phdf5]  } {
        module load phdf5
    }
    if { ![is-loaded intel] } {
        if { ![is-loaded openblas]  } {
          module load openblas
        }
        if { ![is-loaded scalapack]  } {
          module load scalapack
        }
    }
}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_LIB        %{install_path}/lib

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc CONTRIBUTING LICENSE

%changelog
* Tue May 23 2017 Adrian Reber <areber@redhat.com> - 3.7.6-1
- Remove separate mpi setup; it is part of the %%ohpc_compiler macro

* Fri May 12 2017 Karl W Schulz <karl.w.schulz@intel.com> - 3.7.6-0
- switch to use of ohpc_compiler_dependent and ohpc_mpi_dependent flags

* Wed Feb 22 2017 Adrian Reber <areber@redhat.com> - 3.7.5-0
- Switching to %%ohpc_compiler macro
