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

%define somver 0
%define sover %somver.0.0

# Base package name
%define pname adios
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary: The Adaptable IO System (ADIOS)
Name:    %{pname}-%{compiler_family}%{PROJ_DELIM}
Version: 1.8.0
Release: 1
License: BSD-3-Clause
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

BuildRequires: netcdf-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
#BuildRequires: libmpe2-devel
#BuildRequires: python-modules-xml
BuildRequires: python-devel
#BuildRequires: bzlib-devel
#BuildRequires: libsz2-devel
# This is the legacy name for lustre-lite
# BuildRequires: liblustre-devel
BuildRequires: lustre-lite
BuildRequires: python-numpy

# define fdupes, clean up rpmlint errors
BuildRequires: fdupes

###############################################################
# Disable until we get the numpy headers
#Requires: python-module-%pname = %version-%release

# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

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
module load netcdf

TOPDIR=$PWD

export CFLAGS="-I$TOPDIR/src/public -I$MPI_DIR/include -I$NETCDF_INC -I$HDF5_INC -lpthread -L$NETCDF_LIB -lnetcdf -L$HDF5_LIB" 

export CC=mpicc
export CXX=mpicxx
export F77=mpif77
export FC=mpif90
export MPICC=mpicc
export MPIFC=mpif90
export MPICXX=mpicxx

%if %{compiler_family} == intel
export CFLAGS="-fp-model strict $CFLAGS"
%endif
./configure --prefix=%{install_path} \
	--with-mxml=/usr/include \
	--with-lustre=/usr/include/lustre \
	--with-phdf5="$HDF5_DIR" \
	--with-zlib=/usr/include \
	--with-netcdf="$NETCDF_DIR"
make VERBOSE=1

%install
# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi

make DESTDIR=$RPM_BUILD_ROOT install

####################################################################
# adios_config and adios_config.flags have bogus info in them
# required for make MPI=y python
# sed -i 's|%prefix/etc|%prefix'%_datadir/%pname/'|' BUILD/adios_config
# sed -i 's|%prefix|'%buildroot'|' BUILD/adios_config
# sed -i 's|^\.|. "$FLAGSFILE"|' BUILD/adios_config
#cp adios_config %buildroot%_bindir
#mv BUILD/%prefix/%prefix/etc/adios_config.flags %buildroot%_datadir/%pname/

pushd wrappers/numpy
export PATH=$PATH:%buildroot%_bindir
export CFLAGS=-I%buildroot%_includedir
#%make MPI=y python
# Need numpy/arrayobject.h -- assume the header from python-numpy
# make MPI=y python
#%python_install
popd

#####################################################################
# %python_sitelibdir is undefined. not sure if this will be FSP
# dependant or there is a global place for these.
# install -m644 utils/skel/lib/skel_suite.py \
# 	utils/skel/lib/skel_template.py \
# 	utils/skel/lib/skel_test_plan.py \
# 	%buildroot%python_sitelibdir/

rm -f $(find examples -name '*.o') \
	examples/staging/stage_write/writer_adios

find examples -type f -name .gitignore -exec rm {} \;
find examples -type f -name "*.xml" -exec chmod 644 {} \;
find examples -type f -name ".lib" -exec rm -rf {} \;
install -d %buildroot%{install_path}/lib
cp -fR examples %buildroot%{install_path}/lib
%fdupes -s %buildroot%{install_path}/lib/examples

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

pushd /home/abuild/rpmbuild/BUILDROOT/adios-gnu-fsp-1.8.0-*.1.x86_64
find
echo -e '\n\n'
cat ./opt/fsp/pub/libs/gnu/openmpi/adios/1.8.0/etc/adios_config.flags
echo -e '\n\n'
cat ./opt/fsp/pub/libs/gnu/openmpi/adios/1.8.0/bin/adios_config
echo -e '\n\n'
ls -l ./opt/fsp/pub/libs/gnu/openmpi/adios/1.8.0/bin/*.py
popd

%files
%defattr(-,root,root,-)
%doc AUTHORS COPYING ChangeLog KNOWN_BUGS NEWS README TODO
#%exclude %{install_path}/lib/examples
%{FSP_HOME}

#%files -n python-module-%pname
#%python_sitelibdir/*
#%exclude %python_sitelibdir/argparse.py*

#%files examples
#%{install_path}/lib/examples


%changelog
