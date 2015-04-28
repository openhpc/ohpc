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
%define pname hypre
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        2.10.0b
Release:        0
Summary:        Scalable algorithms for solving linear systems of equations
License:        LGPL-2.1
Group:          Development/Libraries/Parallel
Url:            http://www.llnl.gov/casc/hypre/
Source:         %{pname}-%{version}.tar.gz
#Patch0:         hypre-2.8.0b-no-date-and-time-fix.patch
%if 0%{?suse_version} <= 1110
%{!?python_sitearch: %global python_sitearch %(python -c "from distutils.sysconfig import get_python_lib; print(get_python_lib(1))")}
%endif
# TODO : add babel
#BuildRequires:  babel-devel
#BuildRequires:  libltdl-devel
BuildRequires:  superlu-%{compiler_family}%{PROJ_DELIM}
BuildRequires:  superlu_dist-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
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

%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

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
#%patch0 -p1

%build

export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi

module load superlu
module load superlu_dist

# Enable MKL linkage for blas/lapack with gnu builds
%if %{compiler_family} == gnu
module load mkl
%endif

#export includedir=%{_includedir}

#FLAGS="%optflags -fPIC -I%{_includedir}/numpy"
FLAGS="%optflags -fPIC"
%configure \
        --prefix=%{install_path} \
    --without-examples \
    --with-MPI \
    --with-MPI-include=$MPI_DIR/include \
    --with-MPI-lib-dirs="$MPI_DIR/lib" \
    --with-timing \
    --without-openmp \
    --with-blas-libs="mkl_core mkl_intel_lp64 mkl_sequential" \
    --with-blas-lib-dirs=$MKLROOT/intel64/lib \
        --with-lapack-libs="mkl_core mkl_intel_lp64 mkl_sequential" \
        --with-lapack-lib-dirs=$MKLROOT/intel64/lib \
    --with-lapack \
    --with-mli \
    --with-fei \
        --with-superlu \
        --with-superlu_dist \
    CC="mpicc $FLAGS" \
    CXX="mpicxx $FLAGS" \
    F77="mpif77 $FLAGS"
#    MPI_PREFIX=%{_libdir}/mpi/gcc/$mpi
mkdir -p hypre/lib
pushd FEI_mv/femli
make %{?_smp_mflags} all CC="mpicc $FLAGS" \
                         CXX="mpicxx $FLAGS" \
                         F77="mpif77 $FLAGS"
popd
make %{?_smp_mflags} all CC="mpicc $FLAGS" \
                         CXX="mpicxx $FLAGS" \
                         F77="mpif77 $FLAGS"

%install

# %%makeinstall macro does not work with hypre
make install HYPRE_INSTALL_DIR=%{buildroot}%{_libdir}/mpi/gcc/$mpi \
             HYPRE_LIB_INSTALL=%{buildroot}%{_libdir}/mpi/gcc/$mpi/%_lib \
             HYPRE_INC_INSTALL=%{buildroot}%{_libdir}/mpi/gcc/$mpi/include/%{name}
install -m644 hypre/lib/* %{buildroot}%{_libdir}/mpi/gcc/$mpi/%_lib

# Fix wrong permissions
chmod 644 %{buildroot}%{_libdir}/mpi/gcc/$mpi/include/%{name}/LLNL_FEI_*.h

# This files are provided with babel
rm -f %{buildroot}%{_libdir}/mpi/gcc/$mpi/%_lib/libsidl*
popd

# shared libraries

pushd %{buildroot}%{_libdir}/mpi/gcc/$mpi/%_lib
LIBS="$(ls *.a|sed 's|\.a||'|sort)"
mkdir tmp
pushd tmp
for i in $LIBS; do
    if [ "$i" != "libbHYPREClient-F" -a "$i" != "libbHYPREClient-CX" ]
    then
        ar x ../$i.a
        %{_libdir}/mpi/gcc/$mpi/bin/mpicxx -shared * -L.. $ADDLIB \
                       -llapack -lblas \
                       -Wl,-soname,$i.so.%{somver} -o ../$i.so.%sover
        ln -s $i.so.%sover ../$i.so.%{somver}
        ln -s $i.so.%{somver} ../$i.so
        rm -f *
        ADDLIB="-lHYPRE"
    fi
done
popd
rmdir tmp

# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
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

# Require phdf5 and fftw (and mkl for gnu compiler families)

if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if {  ![is-loaded superlu]  } {
        module load superlu
    }
    if {  ![is-loaded superlu_dist]  } {
        module load superlu_dist
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

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%files devel
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog

