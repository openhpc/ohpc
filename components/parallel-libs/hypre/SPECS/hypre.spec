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

# Build that is is dependent on compiler toolchain and MPI
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

%if "%{compiler_family}" != "intel"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires:      openblas-%{compiler_family}%{PROJ_DELIM}
%endif

# Base package name
%define pname hypre
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        2.14.0
Release:        1%{?dist}
Summary:        Scalable algorithms for solving linear systems of equations
License:        LGPL-2.1
Group:          ohpc/parallel-libs
Url:            http://www.llnl.gov/casc/hypre/
Source:         https://github.com/LLNL/hypre/archive/v%{version}.tar.gz#/hypre-%{version}.tar.gz
Source1:        OHPC_macros
%if 0%{?suse_version} <= 1110
%{!?python_sitearch: %global python_sitearch %(python -c "from distutils.sysconfig import get_python_lib; print(get_python_lib(1))")}
%endif
# TODO : add babel
#BuildRequires:  babel-devel
#BuildRequires:  libltdl-devel
BuildRequires:  superlu-%{compiler_family}%{PROJ_DELIM}
Requires:       superlu-%{compiler_family}%{PROJ_DELIM}
BuildRequires:  libxml2-devel
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires:  python-devel
%if 0%{?suse_version}
BuildRequires:  python-xml
%else
BuildRequires:  libxml2-python
%endif
BuildRequires:  xz

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

%ifarch aarch64 || ppc64le
cp /usr/lib/rpm/config.guess src/config
%endif

%ohpc_setup_compiler

module load superlu

%if "%{compiler_family}" != "intel"
module load openblas
%endif


FLAGS="%optflags -fPIC -Dhypre_dgesvd=dgesvd_ -Dhypre_dlamch=dlamch_  -Dhypre_blas_lsame=hypre_lapack_lsame -Dhypre_blas_xerbla=hypre_lapack_xerbla "
cd src
./configure \
    --prefix=%{install_path} \
    --with-MPI \
    --with-MPI-include=$MPI_DIR/include \
    --with-MPI-lib-dirs="$MPI_DIR/lib" \
    --with-timing \
    --without-openmp \
%if "%{compiler_family}" == "intel"
    --with-blas-libs="mkl_core mkl_intel_lp64 mkl_sequential" \
    --with-blas-lib-dirs=$MKLROOT/intel64/lib \
    --with-lapack-libs="mkl_core mkl_intel_lp64 mkl_sequential" \
    --with-lapack-lib-dirs=$MKLROOT/intel64/lib \
%else
    --with-blas-lib="-L$OPENBLAS_LIB -lopenblas" \
    --with-lapack-lib="-L$OPENBLAS_LIB -lopenblas" \
%endif
    --with-mli \
    --with-fei \
    --with-superlu-include=$SUPERLU_INC \
    --with-superlu-lib=$SUPERLU_LIB \
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
%ohpc_setup_compiler

module load superlu

%if "%{compiler_family}" != "intel"
module load openblas
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

# Require superlu (and openblas for gnu compiler families)
depends-on superlu
if { ![is-loaded intel] } {
    depends-on openblas
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

%files
%defattr(-,root,root,-)
%{OHPC_PUB}
%doc CHANGELOG COPYING.LESSER COPYRIGHT INSTALL README
