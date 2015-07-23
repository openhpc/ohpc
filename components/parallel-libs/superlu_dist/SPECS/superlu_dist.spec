#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the Performance Peak project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#
# spec file for package superlu_dist
#
# Copyright (c) 2012 SUSE LINUX Products GmbH, Nuernberg, Germany.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

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
# require Intel runtime for MKL
BuildRequires: intel-compilers%{PROJ_DELIM}
Requires:      intel-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%if 0%{?FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif

# MPI dependencies
%if %{mpi_family} == impi
BuildRequires: intel-mpi-devel%{PROJ_DELIM}
Requires:      intel-mpi-devel%{PROJ_DELIM}
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
%define pname superlu_dist
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

%define major   4
%define libname libsuperlu_dist

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        4.0
Release:        0
Summary:        A general purpose library for the direct solution of linear equations
License:        LGPL-2.1
Group:          fsp/parallel-libs
Url:            http://crd-legacy.lbl.gov/~xiaoye/SuperLU/
Source0:        %{pname}-%{version}.tar.gz
# PATCH-FIX-UPSTREAM superlu_dist-3.1-sequence-point.patch
Patch0:         superlu_dist-3.1-sequence-point.patch
# PATCH-FIX-OPENSUSE superlu_dist-4.0-make.patch
Patch1:         superlu_dist-4.0-make.patch
# PATCH-FIX-UPSTREAM superlu_dist-3.2-example-no-return-in-non-void.patch
Patch2:         superlu_dist-3.2-example-no-return-in-non-void.patch
#BuildRequires:  blas-devel
#BuildRequires:  gcc-fortran
#BuildRequires:  scotch-devel
#%if 0%{?_openmpi}
#BuildRequires:  openmpi-devel
#BuildRequires:  ptscotch-openmpi-devel
#%endif
#%if 0%{?_mvapich2}
#BuildRequires:  mvapich2-devel
#BuildRequires:  ptscotch-mvapich2-devel
#%endif
BuildRequires: parmetis-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires: parmetis-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires: metis-%{compiler_family}%{PROJ_DELIM}
Requires: metis-%{compiler_family}%{PROJ_DELIM}
BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%include %{_sourcedir}/FSP_macros
#!BuildIgnore: post-build-checks
%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
SuperLU is a general purpose library for the direct solution of large, sparse,
nonsymmetric systems of linear equations on high performance machines. The 
library is written in C and is callable from either C or Fortran. The library
routines will perform an LU decomposition with partial pivoting and triangular
system solves through forward and back substitution. The LU factorization routines
can handle non-square matrices but the triangular solves are performed only for
square matrices. The matrix columns may be preordered (before factorization)
either through library or user supplied routines. This preordering for sparsity 
is completely separate from the factorization. Working precision iterative
refinement subroutines are provided for improved backward stability. Routines
are also provided to equilibrate the system, estimate the condition number,
calculate the relative backward error, and estimate error bounds for the refined
solutions. 

%prep
%setup -q -n %{pname}-%{version}
%patch0 -p1
%patch1 -p1
%patch2 -p1

%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi

module load metis
module load parmetis

# Enable MKL linkage for blas/lapack with gnu builds
%if %{compiler_family} == gnu
module load mkl
%endif

make superlulib DSuperLUroot=$PWD 

mkdir tmp
(cd tmp; ar x ../lib/libsuperlu_dist_%{version}.a)
mpif90 -z muldefs -shared -Wl,-soname=%{libname}.so.%{major} -o lib/%{libname}.so.%{version} tmp/*.o
pushd lib
ln -s %{libname}.so.%{version} %{libname}.so
popd


%install

%{__mkdir} -p %{buildroot}%{install_path}/etc
install -m644 make.inc %{buildroot}%{install_path}/etc

%{__mkdir} -p %{buildroot}%{install_path}/include
install -m644 SRC/Cnames.h SRC/dcomplex.h SRC/machines.h SRC/psymbfact.h \
              SRC/superlu_ddefs.h SRC/superlu_defs.h SRC/superlu_enum_consts.h \
              SRC/superlu_zdefs.h SRC/supermatrix.h SRC/util_dist.h \
              %{buildroot}%{install_path}/include/

%{__mkdir} -p %{buildroot}%{install_path}/lib
install -m 755 lib/libsuperlu_dist.so.%{version} %{buildroot}%{install_path}/lib
pushd %{buildroot}%{install_path}/lib
ln -s libsuperlu_dist.so.%{version} libsuperlu_dist.so.4
ln -s libsuperlu_dist.so.%{version} libsuperlu_dist.so
popd

# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the SuperLU_dist library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr " "
puts stderr "Note that this build of SuperLU_dist leverages the parmetis and MKL libraries."
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
    if {  ![is-loaded metis]  } {
        module load metis
    }
    if {  ![is-loaded parmetis]  } {
        module load parmetis
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
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_LIB        %{install_path}/lib

EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF


%clean
rm -rf %{buildroot}

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig


%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog
