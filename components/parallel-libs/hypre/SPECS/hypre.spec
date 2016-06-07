#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#
# spec file for package hypre
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

#-ohpc-header-comp-begin-----------------------------

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

# OpenHPC convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family and mpi_family variables via rpmbuild or other
# mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family:      %define mpi_family openmpi}

# Lmod dependency (note that lmod is pre-populated in the OpenHPC OBS build
# environment; if building outside, lmod remains a formal build dependency).
%if !0%{?OHPC_BUILD}
BuildRequires: lmod%{PROJ_DELIM}
%endif
# Compiler dependencies
BuildRequires: coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
BuildRequires: openblas-gnu%{PROJ_DELIM}
Requires:      openblas-gnu%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%if 0%{?OHPC_BUILD}
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

#-ohpc-header-comp-end-------------------------------

# Base package name
%define pname hypre
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        2.10.1
Release:        0
Summary:        Scalable algorithms for solving linear systems of equations
License:        LGPL-2.1
Group:          %{PROJ_NAME}/parallel-libs
Url:            http://www.llnl.gov/casc/hypre/
Source:         https://computation.llnl.gov/project/linear_solvers/download/hypre-%{version}.tar.gz
%if 0%{?suse_version} <= 1110
%{!?python_sitearch: %global python_sitearch %(python -c "from distutils.sysconfig import get_python_lib; print(get_python_lib(1))")}
%endif
# TODO : add babel
#BuildRequires:  babel-devel
#BuildRequires:  libltdl-devel
BuildRequires:  superlu-%{compiler_family}%{PROJ_DELIM}
Requires:  superlu-%{compiler_family}%{PROJ_DELIM}
BuildRequires:  superlu_dist-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:  superlu_dist-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  libxml2-devel
BuildRequires:  python-devel
BuildRequires:  python-numpy-%{compiler_family}%{PROJ_DELIM}
%if 0%{?suse_version}
BuildRequires:  python-xml
%else
BuildRequires:  libxml2-python
%endif
BuildRequires:  xz
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
DocDir:         %{OHPC_PUB}/doc/contrib

%define debug_package %{nil}

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
The goal of the Scalable Linear Solvers project is to develop scalable
algorithms and software for solving large, sparse linear systems of equations on
parallel computers. The primary software product is Hypre, a library of high
performance preconditioners that features parallel multigrid methods for both
structured and unstructured grid problems. The problems of interest arise in the
simulation codes being developed at LLNL and elsewhere to study physical
phenomena in the defense, environmental, energy, and biological sciences.

%prep
%setup -q -n %{pname}-%{version}

%build

export OHPC_COMPILER_FAMILY=%{compiler_family}
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_compiler
. %{_sourcedir}/OHPC_setup_mpi

module load superlu
module load superlu_dist

%if %{compiler_family} == gnu
module load scalapack
%endif


FLAGS="%optflags -fPIC"
cd src
./configure \
    --prefix=%{install_path} \
    --without-examples \
    --with-MPI \
    --with-MPI-include=$MPI_DIR/include \
    --with-MPI-lib-dirs="$MPI_DIR/lib" \
    --with-timing \
    --without-openmp \
%if %{compiler_family} == intel
    --with-blas-libs="mkl_core mkl_intel_lp64 mkl_sequential" \
    --with-blas-lib-dirs=$MKLROOT/intel64/lib \
    --with-lapack-libs="mkl_core mkl_intel_lp64 mkl_sequential" \
    --with-lapack-lib-dirs=$MKLROOT/intel64/lib \
%else
    --with-blas-libs=openblas \
    --with-blas-lib-dirs=$OPENBLAS_LIB \
    --with-lapack-libs=openblas \
    --with-lapack-lib-dirs=$OPENBLAS_LIB \
%endif
    --with-mli \
    --with-fei \
    --with-superlu \
    --with-superlu_dist \
    CC="mpicc $FLAGS" \
    CXX="mpicxx $FLAGS" \
    F77="mpif77 $FLAGS"

mkdir -p hypre/lib
pushd FEI_mv/femli
make %{?_smp_mflags} all CC="mpicc $FLAGS" \
                         CXX="mpicxx $FLAGS" \
                         F77="mpif77 $FLAGS"
popd
make %{?_smp_mflags} all CC="mpicc $FLAGS" \
                         CXX="mpicxx $FLAGS" \
                         F77="mpif77 $FLAGS"
cd ..

%install

export OHPC_COMPILER_FAMILY=%{compiler_family}
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_compiler
. %{_sourcedir}/OHPC_setup_mpi

module load superlu
module load superlu_dist

%if %{compiler_family} == gnu
module load scalapack
%endif

# %%makeinstall macro does not work with hypre
cd src
make install HYPRE_INSTALL_DIR=%{buildroot}%{install_path} \
             HYPRE_LIB_INSTALL=%{buildroot}%{install_path}/lib \
             HYPRE_INC_INSTALL=%{buildroot}%{install_path}/include
install -m644 hypre/lib/* %{buildroot}%{install_path}/lib

# install LLNL FEI headers
mkdir %{buildroot}%{install_path}/include/FEI_mv
cp -r FEI_mv/fei-base %{buildroot}%{install_path}/include/FEI_mv/.
cd ..

# Fix wrong permissions
chmod 644 %{buildroot}%{install_path}/include/LLNL_FEI_*.h

# This files are provided with babel
#rm -f %{buildroot}%{_libdir}/mpi/gcc/$mpi/%_lib/libsidl*
#popd

# shared libraries

pushd %{buildroot}%{install_path}/lib
LIBS="$(ls *.a|sed 's|\.a||'|sort)"
mkdir tmp
pushd tmp
for i in $LIBS; do
    if [ "$i" != "libbHYPREClient-F" -a "$i" != "libbHYPREClient-CX" ]
    then
        ar x ../$i.a
        mpicxx -shared * -L.. $ADDLIB \
                       -Wl,-soname,$i.so -o ../$i.so 
        ADDLIB="-lHYPRE"
    fi
done
popd
rm -rf tmp
rm libHYPRE.a

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the hypre library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr " "
puts stderr "Note that this build of hypre leverages the superlu and MKL libraries."
puts stderr "Consequently, these packages are loaded automatically with this module."

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version                     %{version}

# Require superlu (and scalapack for gnu compiler families)

if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if {  ![is-loaded superlu]  } {
        module load superlu
    }
    if {  ![is-loaded superlu_dist]  } {
        module load superlu_dist
    }
    if { [is-loaded gnu] } {
        if { ![is-loaded scalapack]  } {
          module load scalapack
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

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p %{buildroot}/%{_docdir}

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc CHANGELOG COPYING.LESSER COPYRIGHT INSTALL README

%changelog

