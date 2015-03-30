#-fsp-header-comp-begin----------------------------------------------

%include %{_sourcedir}/FSP_macros

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family: %define mpi_family openmpi}
%{!?PROJ_DELIM:      %define PROJ_DELIM   %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM}
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers%{PROJ_DELIM}
%if 0%{FSP_BUILD}
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

#-fsp-header-comp-end------------------------------------------------

#%define mpiimpl %{mpi_family}
#%define mpidir %_libdir/%mpiimpl

%define somver 0
%define sover %somver.0.0

# Base package name
%define pname adios
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary: The Adaptable IO System (ADIOS)
Name:    %{pname}-%{compiler_family}%{PROJ_DELIM}
Version: 1.8.0
Release: 1
License: BSD
Group:   fsp/io-libs
Url:     http://www.olcf.ornl.gov/center-projects/adios/
Source0: %{pname}-%{version}.tar.gz
Source1: FSP_macros
Source2: FSP_setup_compiler

# Minimum Build Requires
BuildRequires: mxml-devel cmake zlib-devel glib2-devel
BuildRequires: %{mpi_family}-devel

# libm.a from CMakeLists
BuildRequires: glibc-static

BuildRequires: phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:      phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

# Additional Build Requires
#BuildRequires: libhdf5-mpi-devel
#BuildRequires: libnetcdf-mpi-devel
#BuildRequires: libmpe2-devel
#BuildRequires: python-modules-xml
BuildRequires: python-devel
#BuildRequires: bzlib-devel
#BuildRequires: libsz2-devel
# This is the legacy name for lustre-lite
# BuildRequires: liblustre-devel
BuildRequires: lustre-lite
#BuildRequires: libnumpy-devel
BuildRequires: python-numpy
#!BuildIgnore: post-build-checks

Requires: python-module-%pname = %version-%release

%description
The Adaptable IO System (ADIOS) provides a simple, flexible way for
scientists to desribe the data in their code that may need to be
written, read, or processed outside of the running simulation. By
providing an external to the code XML file describing the various
elements, their types, and how you wish to process them this run, the
routines in the host code (either Fortran or C) can transparently change
how they process the data.

%package -n python-module-%pname
Summary: Python module of the Adaptable IO System (ADIOS)
Group: Development/Python

%description -n python-module-%pname
The Adaptable IO System (ADIOS) provides a simple, flexible way for
scientists to desribe the data in their code that may need to be
written, read, or processed outside of the running simulation. By
providing an external to the code XML file describing the various
elements, their types, and how you wish to process them this run, the
routines in the host code (either Fortran or C) can transparently change
how they process the data.

This package contains python module of ADIOS.

%package -n lib%pname
Summary: Shared libraries of the Adaptable IO System (ADIOS)
Group: System/Libraries

%description -n lib%pname
The Adaptable IO System (ADIOS) provides a simple, flexible way for
scientists to desribe the data in their code that may need to be
written, read, or processed outside of the running simulation. By
providing an external to the code XML file describing the various
elements, their types, and how you wish to process them this run, the
routines in the host code (either Fortran or C) can transparently change
how they process the data.

This package contains shared libraries of ADIOS.

%package -n lib%pname-devel
Summary: Development files of the Adaptable IO System (ADIOS)
Group: Development/C
Requires: lib%pname = %version-%release

%description -n lib%pname-devel
The Adaptable IO System (ADIOS) provides a simple, flexible way for
scientists to desribe the data in their code that may need to be
written, read, or processed outside of the running simulation. By
providing an external to the code XML file describing the various
elements, their types, and how you wish to process them this run, the
routines in the host code (either Fortran or C) can transparently change
how they process the data.

This package contains development files of ADIOS.

%package examples
Summary: Examples for the Adaptable IO System (ADIOS)
Group: Sciences/Mathematics

%description examples
The Adaptable IO System (ADIOS) provides a simple, flexible way for
scientists to desribe the data in their code that may need to be
written, read, or processed outside of the running simulation. By
providing an external to the code XML file describing the various
elements, their types, and how you wish to process them this run, the
routines in the host code (either Fortran or C) can transparently change
how they process the data.

This package contains examples for ADIOS.

%prep
%setup -q -n %{pname}-%{version}

sed -i 's|@BUILDROOT@|%buildroot|' wrappers/numpy/setup*
%ifarch x86_64
LIBSUFF=64
%endif
sed -i "s|@64@|$LIBSUFF|" wrappers/numpy/setup*

%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi
%if %{compiler_family} == intel
export CFLAGS="-fp-model strict $CFLAGS"
%endif

module load phdf5

# Attempt serial build
# mpi-selector --set %mpiimpl
# source %mpidir/bin/mpivars.sh
export OMPI_LDFLAGS="-Wl,--as-needed,-rpath,$MPI_DIR/lib -L$MPI_DIR/lib"
export MPIDIR="$MPI_DIR"
TOPDIR=$PWD

#%add_optflags -I$TOPDIR/src/public
# Attempt to build serial
# %add_optflags -I%mpidir/include -I%mpidir/include/netcdf %optflags_shared

export optflags="-I$TOPDIR/src/public -I$MPI_DIR/include -I$MPI_DIR/include/netcdf -I$HDF5_INC -lpthread -L$HDF5_LIB" 
export CFLAGS="$optflags"
#export CFLAGS="-I$TOPDIR/src/public -I$MPI_DIR/include -I$MPI_DIR/include/netcdf"

export CC=mpicc
export CXX=mpicxx
export F77=mpif77
export FC=mpif90
export MPICC=mpicc
export MPIFC=mpifc
export MPICXX=mpicxx

mkdir BUILD
pushd BUILD
cmake \
%if %_lib == lib64
	-DLIB_SUFFIX=64 \
%endif
	-DCMAKE_INSTALL_PREFIX:PATH=%prefix \
	-DCMAKE_C_FLAGS:STRING="$optflags" \
	-DCMAKE_CXX_FLAGS:STRING="$optflags" \
	-DCMAKE_Fortran_FLAGS:STRING="$optflags" \
	-DCXXFLAGS:STRING="$optflags" \
	-DFCFLAGS:STRING="$optflags" \
	-DNC4PAR:BOOL=ON \
	-DPHDF5:BOOL=ON \
	-DPHDF5_CFLAGS:STRING="$optflags" \
	-DPHDF5_LIBS:STRING="-lhdf5 -lhdf5_hl -lz" \
	-DMPIDIR:PATH="$MPI_DIR" \
	-DMPILIBS:STRING="-L$MPI_DIR/lib -lmpi_f90 -lmpi_f77 -lmpi_cxx -lmpi" \
	-DCMAKE_INSTALL_RPATH:STRING="$MPI_DIR/lib" \
	-DCMAKE_SKIP_RPATH:BOOL=ON \
	-DSOMVER:STRING=%somver \
	-DSOVER:STRING=%sover \
	..

#%make VERBOSE=1
make VERBOSE=1
popd

%install
# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi

# Attempt to build serial
#source %mpidir/bin/mpivars.sh
export OMPI_LDFLAGS="-Wl,--as-needed,-rpath,$MPI_DIR/lib -L$MPI_DIR/lib"
export MPIDIR=$MPI_DIR

pushd BUILD
#%makeinstall_std
make install
#cp -P src/libadios_internal_nompi.so* %buildroot%_libdir/
popd

install -d %buildroot%_datadir/%pname
install -d %buildroot%_bindir
#mv %buildroot%_bindir/adios_config.flags %buildroot%_datadir/%pname/
mv BUILD/adios_config.flags %buildroot%_datadir/%pname/


####################################################################
# adios_config and adios_config.flags have bogus info in them
# required for make MPI=y python
# sed -i 's|%prefix/etc|%prefix'%_datadir/%pname/'|' BUILD/adios_config
# sed -i 's|%prefix|'%buildroot'|' BUILD/adios_config
# sed -i 's|^\.|. "$FLAGSFILE"|' BUILD/adios_config
# mv BUILD/adios_config %buildroot%_bindir
#mv BUILD/%prefix/%prefix/etc/adios_config.flags %buildroot%_datadir/%pname/

pushd wrappers/numpy
export PATH=$PATH:%buildroot%_bindir
export CFLAGS=-I%buildroot%_includedir
#%make MPI=y python
# Need numpy/arrayobject.h -- assume the header from python-numpy
# make MPI=y python
#%python_install
popd

find %buildroot -type d

#####################################################################
# %python_sitelibdir is undefined. not sure if this will be FSP
# dependant or there is a global place for these.
# install -m644 utils/skel/lib/skel_suite.py \
# 	utils/skel/lib/skel_template.py \
# 	utils/skel/lib/skel_test_plan.py \
# 	%buildroot%python_sitelibdir/

rm -f $(find examples -name '*.o') \
	examples/staging/stage_write/writer_adios

install -d %buildroot%_libdir/%pname
cp -fR examples %buildroot%_libdir/%pname/

# See above regarding %python_sutelibdir
# install -d %buildroot%python_sitelibdir
# mv %buildroot%_libdir/python/*.py %buildroot%python_sitelibdir/

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

set             version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

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

%files
%doc AUTHORS COPYING ChangeLog KNOWN_BUGS NEWS README TODO
%_sysconfdir/*
%_bindir/*
%exclude %_bindir/adios_config

%files -n lib%pname
%_libdir/*.so.*

%files -n python-module-%pname
%python_sitelibdir/*
%exclude %python_sitelibdir/argparse.py*

%files -n lib%pname-devel
%_bindir/adios_config
%_libdir/*.so
%_includedir/*
%_datadir/%pname
%_libdir/FindADIOS.cmake

%files examples
%_libdir/%pname

%changelog
