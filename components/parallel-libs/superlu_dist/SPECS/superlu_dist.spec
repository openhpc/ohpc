#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# superlu_dist build that is is dependent on compiler toolchain and MPI
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname superlu_dist
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

%define major   5
%define libname libsuperlu_dist

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        5.2.2
Release:        1%{?dist}
Summary:        A general purpose library for the direct solution of linear equations
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/parallel-libs
URL:            http://crd-legacy.lbl.gov/~xiaoye/SuperLU/
Source0:        http://crd-legacy.lbl.gov/~xiaoye/SuperLU/superlu_dist_%{version}.tar.gz
Source1:        OHPC_macros
Source2:        superlu_dist-make.inc
Patch1:         superlu_dist-parmetis.patch
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires:  ptscotch-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  metis-%{compiler_family}%{PROJ_DELIM}
Requires:       metis-%{compiler_family}%{PROJ_DELIM}
%if "%{compiler_family}" != "intel"
BuildRequires:  openblas-%{compiler_family}%{PROJ_DELIM}
Requires:       openblas-%{compiler_family}%{PROJ_DELIM}
%endif

#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

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
%setup -q -n SuperLU_DIST_%{version}
%patch1 -p1
cp %SOURCE2 make.inc

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load metis ptscotch

%if "%{compiler_family}" != "intel"
module load openblas
%endif

make SuperLUroot=$(pwd)

mkdir tmp
(cd tmp; ar x ../SRC/libsuperlu_dist.a)
mpif90 -z muldefs -shared -Wl,-soname=%{libname}.so.%{major} \
    -o ../%{libname}.so.%{version} tmp/*.o -fopenmp -L$METIS_LIB \
    -L$OPENBLAS_LIB -L$PTSCOTCH_LIB -lptscotchmarmetis \
    -lptscotch -lptscotcherr -lscotch -lmetis -lopenblas \
    -lbz2 %{?__global_ldflags}
pushd lib
ln -s %{libname}.so.%{version} %{libname}.so
popd


%install

%{__mkdir_p} %{buildroot}%{install_path}/etc
install -m644 make.inc %{buildroot}%{install_path}/etc

%{__mkdir_p} %{buildroot}%{install_path}/include
install -m644 SRC/*.h %{buildroot}%{install_path}/include/

%{__mkdir_p} %{buildroot}%{install_path}/lib
install -m 755 libsuperlu_dist.so.%{version} %{buildroot}%{install_path}/lib
pushd %{buildroot}%{install_path}/lib
ln -s libsuperlu_dist.so.%{version} libsuperlu_dist.so.4
ln -s libsuperlu_dist.so.%{version} libsuperlu_dist.so
popd

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the SuperLU_dist library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr " "
puts stderr "Note that this build of SuperLU_dist leverages the metis and MKL libraries."
puts stderr "Consequently, these packages are loaded automatically with this module."

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version                     %{version}

depends-on metis

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
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

%{__mkdir_p} %{buildroot}/%_docdir

%files
%defattr(-,root,root,-)
%{OHPC_PUB}
%doc README

%changelog
* Tue May 23 2017 Adrian Reber <areber@redhat.com> - 4.2-1
- Remove separate mpi setup; it is part of the %%ohpc_compiler macro

* Fri May 12 2017 Karl W Schulz <karl.w.schulz@intel.com> - 4.2-0
- switch to use of ohpc_compiler_dependent and ohpc_mpi_dependent flags

* Wed Feb 22 2017 Adrian Reber <areber@redhat.com> - 4.2-0
- Switching to %%ohpc_compiler macro
