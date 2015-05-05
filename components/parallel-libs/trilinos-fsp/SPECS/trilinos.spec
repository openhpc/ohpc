#
# spec file for package trilinos
#
# Copyright (c) 2013 SUSE LINUX Products GmbH, Nuernberg, Germany.
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
%{!?mpi_family:      %define mpi_family      openmpi}
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM} coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
# hack to install MKL for the moment
BuildRequires: intel-compilers%{PROJ_DELIM}
Requires:      intel-compilers%{PROJ_DELIM}
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
%define pname trilinos
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        11.14.3
Release:        0
Summary:        A collection of libraries of numerical algorithms
License:        LGPL-2.0
Group:          System/Libraries
Url:            http://trilinos.sandia.gov/index.html
Source0:        %{pname}-%{version}.tar.gz
Patch0:         trilinos-11.14.3-no-return-in-non-void.patch
Patch1:         trilinos-11.14.3-no_rpath.patch
BuildRequires:  boost-devel
BuildRequires:  cmake >= 2.8
BuildRequires:  cppunit-devel
BuildRequires:  doxygen
BuildRequires:  expat
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  fdupes
%endif
#BuildRequires:  gcc-c++
#BuildRequires:  gcc-fortran
#BuildRequires:  glpk-devel
BuildRequires:  graphviz
#BuildRequires:  hdf5-devel
BuildRequires:  phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  lapack-devel
#BuildRequires:  libscotch-devel
BuildRequires:  libxml2-devel
#BuildRequires:  mumps-devel
BuildRequires:  perl
BuildRequires:  libqt4-devel
#BuildRequires:  suitesparse-common-devel
BuildRequires:  swig > 2.0.0
#BuildRequires:  tbb-devel
#BuildRequires:  superlu-devel
#BuildRequires:  superlu-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
#%if 0%{?suse_version} == 1140 || 0%{?suse_version} == 1210
#BuildRequires:  libnetcdf-devel
#%else
#BuildRequires:  netcdf-devel
#%endif
BuildRequires:  netcdf-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
#BuildRequires:  umfpack-devel
BuildRequires:  xz
BuildRequires:  zlib-devel
%if 0%{?suse_version} <= 1110
%{!?python_sitearch: %global python_sitearch %(python -c "from distutils.sysconfig import get_python_lib; print(get_python_lib(1))")}
%endif
BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%include %{_sourcedir}/FSP_macros
#!BuildIgnore: post-build-checks
%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
Trilinos is a collection of compatible software packages that support parallel 
linear algebra computations, solution of linear, non-linear and eigen systems 
of equations and related capabilities. The majority of packages are written in 
C++ using object-oriented techniques. All packages are self-contained, with the 
Trilinos top layer providing a common look-and-feel and infrastructure.

%prep
%setup -q -n %{pname}-%{version}
%patch0 -p1
%patch1 -p1

%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi

module load phdf5
module load netcdf

%if %{compiler_family} == gnu
module load mkl
%endif

mkdir tmp
cd tmp
cmake	-DCMAKE_INSTALL_PREFIX=%{install_path}		                \
	-DTrilinos_INSTALL_INCLUDE_DIR:PATH=%{install_path}/include	\
	-DTrilinos_INSTALL_LIB_DIR:PATH=%{install_path}/lib             \
	-DCMAKE_EXE_LINKER_FLAGS:STRING="-fPIC"				\
	-DCMAKE_VERBOSE_MAKEFILE:BOOL=TRUE				\
	-DCMAKE_BUILD_TYPE:STRING=RELEASE				\
	-DBUILD_SHARED_LIBS:BOOL=ON					\
	-DCMAKE_SKIP_INSTALL_RPATH:BOOL=ON				\
	-DCMAKE_SKIP_RPATH:BOOL=ON					\
	-DTrilinos_VERBOSE_CONFIGURE:BOOL=ON				\
	-DTrilinos_ENABLE_ALL_PACKAGES:BOOL=ON 				\
	-DTrilinos_ENABLE_Didasko:BOOL=ON				\
	-DTrilinos_ENABLE_Stokhos:BOOL=ON				\
	-DTrilinos_ENABLE_Phalanx:BOOL=ON				\
	-DTrilinos_ENABLE_TrilinosCouplings:BOOL=ON			\
	-DTrilinos_ENABLE_PyTrilinos:BOOL=OFF				\
	-DTrilinos_ENABLE_CTrilinos:BOOL=ON				\
%if 0%{?suse_version} >= 1210
	-DTrilinos_ENABLE_ForTrilinos:BOOL=ON				\
%endif
	-DTrilinos_ENABLE_TESTS:BOOL=OFF				\
	-DTEUCHOS_ENABLE_expat:BOOL=ON					\
	-DTEUCHOS_ENABLE_libxml2:BOOL=ON				\
	-DTEUCHOS_ENABLE_gmp:BOOL=ON					\
	-DBLAS_LIBRARY_DIRS:PATH=%{_libdir}				\
	-DBLAS_LIBRARY_NAMES:STRING="blas"				\
	-DTPL_ENABLE_BLAS:BOOL=ON					\
	-DLAPACK_LIBRARY_DIRS:PATH=%{_libdir}				\
	-DLAPACK_LIBRARY_NAMES:STRING="lapack"				\
	-DTPL_ENABLE_LAPACK:BOOL=ON					\
	-DNOX_ENABLE_lapack:BOOL=ON					\
	-DTPL_ENABLE_MPI:BOOL=ON					\
        -DMPI_C_COMPILER:FILEPATH=mpicc                                 \
        -DMPI_CXX_COMPILER:FILEPATH=mpicxx                              \
        -DMPI_FORTRAN_COMPILER:FILEPATH=mpif90                          \
	-DTPL_ENABLE_Pthread:BOOL=ON					\
	-DTPL_ENABLE_Boost:BOOL=ON					\
	-DTPL_ENABLE_CppUnit:BOOL=ON					\
	-DTPL_ENABLE_Zlib:BOOL=ON					\
	-DTPL_ENABLE_Netcdf:BOOL=ON					\
        -DNetcdf_INCLUDE_DIRS:PATH=$NETCDF_INC                          \
	-DNetcdf_LIBRARY_DIRS:PATH=$NETCDF_LIB                          \
	-DTPL_ENABLE_QT:BOOL=ON						\
	-DTPL_ENABLE_HDF5:BOOL=ON					\
	-DHDF5_INCLUDE_DIRS:PATH=$HDF5_INC	        		\
	-DHDF5_LIBRARY_DIRS:PATH=$HDF5_LIB				\
        -DHDF5_LIBRARY_NAMES:STRING="hdf5"                              \
	-DTPL_ENABLE_SCALAPACK:BOOL=ON					\
	-DSCALAPACK_LIBRARY_DIRS:PATH=$MKLROOT/lib/intel64		\
	-DSCALAPACK_LIBRARY_NAMES:STRING="mkl_intel_lp64;mkl_core;mkl_sequential"\
	-DTPL_ENABLE_Matio=OFF						\
	-DTPL_ENABLE_GLM=OFF						\
        ..			
#       -DTPL_ENABLE_BLACS:BOOL=ON                                      \
#       -DBLACS_LIBRARY_DIRS:PATH=$MKLROOT/lib/intel64                  \
#       -DBLACS_INCLUDE_DIRS:PATH=$MKLROOT/include                      \
#       -DBLACS_LIBRARY_NAMES:STRING="mkl_intel_lp64;mkl_core;mkl_sequential"\
make VERBOSE=1 
make %{?_smp_mflags}
cd ..

# Build the doc
echo "HTML_TIMESTAMP=NO" >> packages/common/Doxyfile
find packages/ -name 'footer.html' -print0 | xargs -0 sed -i 's/ on \$datetime//'
find packages/ -name 'Doxyfile*' -print0 | xargs -0 sed -i 's/HTML_TIMESTAMP         = YES/HTML_TIMESTAMP         = NO/'

cd doc
perl ./build_docs.pl
cd ..
# move html files in a single directory for doc package
find ./ -type d -name html -print0 | xargs -0 -I '{}' cp --parent -r '{}' doc/
test -d doc/doc && rm -rf doc/doc
test -f doc/build_docs.pl && rm -f doc/build_docs.pl 
sed -i s/href=\"\.\./href=\"\./ doc/index.html

%if 0%{?sles_version} || 0%{?suse_version}
%fdupes -s doc/
%endif

%install
# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi

make DESTDIR=$RPM_BUILD_ROOT install

# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} <<EOF
 EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version                     %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} <<EOF
 EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

#cd serial
#make DESTDIR=%{buildroot} install INSTALL='install -p'
#cd ..
#find %{buildroot}%{_libdir} -name '*.la' -exec rm {} \;

# ld.so.conf.d file
mkdir -p %{buildroot}%{_sysconfdir}/ld.so.conf.d
echo "%{_libdir}/%{name}" > %{buildroot}%{_sysconfdir}/ld.so.conf.d/%{name}.conf

%if 0%{?sles_version} || 0%{?suse_version}
%fdupes -s %{buildroot}%{_includedir}
%endif

cd tmp
make DESTDIR=%{buildroot} install INSTALL='install -p'
%if 0%{?sles_version} || 0%{?suse_version}
%fdupes -s %{buildroot}%{_libdir}/mpi/gcc/openmpi/include
%endif
cd ..

%clean
rm -rf %{buildroot}

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

#%post openmpi -p /sbin/ldconfig
#%postun openmpi -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%{FSP_HOME}

#%files
#%defattr(-, root, root, -)
#%doc CHANGELOG LICENSE README RELEASE_NOTES
#%dir %{_libdir}/%{name} 
#%{_bindir}/*
#%{_libdir}/%{name}/*.so
#%{_libdir}/%{name}/cmake
#%config %{_sysconfdir}/ld.so.conf.d/%{name}.conf

#%files devel
#%defattr(-, root, root, -)
#%{_includedir}/%{name}

#%files doc
#%defattr(-, root, root, -)
#%doc doc/*

#%files -n python-PyTrilinos
#%defattr(-, root, root, -)
#%{python_sitearch}/PyTrilinos

#%files openmpi
#%defattr(-, root, root, -)
#%doc CHANGELOG LICENSE README RELEASE_NOTES
#%{_libdir}/mpi/gcc/openmpi/bin/*
#%{_libdir}/mpi/gcc/openmpi/%{_lib}/*.so
#%{_libdir}/mpi/gcc/openmpi/%{_lib}/cmake

#%files openmpi-devel
#%defattr(-, root, root, -)
#%{_libdir}/mpi/gcc/openmpi/include/%{name}

%changelog
