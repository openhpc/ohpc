#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#-ohpc-header-comp-begin----------------------------------------------

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

# OpenHPC convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family:      %define mpi_family openmpi}

# Lmod dependency (note that lmod is pre-populated in the OpenHPC OBS build
# environment; if building outside, lmod remains a formal build dependency).
%if !0%{?OHPC_BUILD}
BuildRequires: lmod%{PROJ_DELIM}
%endif
# Compiler dependencies
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%if 0%{OHPC_BUILD}
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

#-ohpc-header-comp-end------------------------------------------------

# not generating a debug package, CentOS build breaks without this if no debug package defined
%define debug_package %{nil}

%define somver 0
%define sover %somver.0.0

# Base package name
%define pname adios
%define PNAME %(tr [a-z] [A-Z] <<< %{pname})

Summary: The Adaptable IO System (ADIOS)
Name:    %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version: 1.9.0
Release: 1
License: BSD-3-Clause
Group:   %{PROJ_NAME}/io-libs
DocDir:  %{OHPC_PUB}/doc/contrib
Url:     http://www.olcf.ornl.gov/center-projects/adios/
Source0: http://users.nccs.gov/~pnorbert/adios-%{version}.tar.gz 
Source1: OHPC_macros
Source2: OHPC_setup_compiler
AutoReq: no

# Minimum Build Requires - our mxml build included devel headers in libmxml1
BuildRequires: libmxml1 cmake zlib-devel glib2-devel
Requires:      libmxml1 zlib
#bzip support confuses the CMtests
#Requires:      bzip2
#BuildRequires: bzip2-devel

# libm.a from CMakeLists
BuildRequires: glibc-static

BuildRequires: phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:      phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

BuildRequires: netcdf-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:      netcdf-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
#BuildRequires: libmpe2-devel
#BuildRequires: python-modules-xml
BuildRequires: python-devel
#BuildRequires: bzlib-devel
#BuildRequires: libsz2-devel
# This is the legacy name for lustre-lite
# BuildRequires: liblustre-devel
BuildRequires: lustre-lite
BuildRequires: python-numpy-%{compiler_family}%{PROJ_DELIM}
Requires: lustre-client%{PROJ_DELIM}

%if 0%{?sles_version} || 0%{?suse_version}
# define fdupes, clean up rpmlint errors
BuildRequires: fdupes
%endif

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
The Adaptable IO System (ADIOS) provides a simple, flexible way for
scientists to desribe the data in their code that may need to be
written, read, or processed outside of the running simulation. By
providing an external to the code XML file describing the various
elements, their types, and how you wish to process them this run, the
routines in the host code (either Fortran or C) can transparently change
how they process the data.

%prep
%setup -q -n %{pname}-%{version}

%build
sed -i 's|@BUILDROOT@|%buildroot|' wrappers/numpy/setup*
%ifarch x86_64
LIBSUFF=64
%endif
sed -i "s|@64@|$LIBSUFF|" wrappers/numpy/setup*

pushd %{_sourcedir}
cp -p adios.spec %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}.spec
popd

# OpenHPC compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_compiler
. %{_sourcedir}/OHPC_setup_mpi
%if %{compiler_family} == intel
export CFLAGS="-fp-model strict $CFLAGS"
%endif

module load phdf5
module load netcdf

TOPDIR=$PWD

export CFLAGS="-fPIC -I$TOPDIR/src/public -I$MPI_DIR/include -I$NETCDF_INC -I$HDF5_INC -pthread -lpthread -L$NETCDF_LIB -lnetcdf -L$HDF5_LIB"

# These lines break intel builds, but are required for gnu mvapich2
%if %{compiler_family} == gnu
export LDFLAGS="-L$NETCDF_LIB -L$HDF5_LIB"
export LIBS="-pthread -lpthread -lnetcdf"
%endif

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
	--with-mxml=/usr \
	--with-lustre=/usr/include/lustre \
	--with-phdf5="$HDF5_DIR" \
	--with-zlib=/usr \
        --without-atl \
        --without-cercs_env \
        --without-dill \
        --without-evpath \
        --without-fastbit \
        --without-ffs \
	--with-netcdf="$NETCDF_DIR" || { cat config.log && exit 1; }
# bzip2 support is confusing CMtests
#	--with-bzip2=/usr \

# modify libtool script to not hardcode library paths
sed -i -r -e 's/(hardcode_into_libs)=.*$/\1=no/' \
	-e 's/^hardcode_direct.*$/hardcode_direct=yes/g' \
	-e 's/^hardcode_minus_L.*$/hardcode_minus_L=yes/g' \
	-e 's/^hardcode_shlibpath_var.*$/hardcode_shlibpath_var=no/g' \
	-e 's/^hardcode_libdir_flag_spec.*$/hardcode_libdir_flag_spec=" -D__LIBTOOL_IS_A_FOOL__ "/' \
	libtool

make VERBOSE=1

chmod +x adios_config

%install
# OpenHPC compiler designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_compiler
. %{_sourcedir}/OHPC_setup_mpi
export NO_BRP_CHECK_RPATH=true

make DESTDIR=$RPM_BUILD_ROOT install

# this is clearly generated someway and shouldn't be static
export PPATH="/lib64/python2.7/site-packages"
export PATH=$(pwd):$PATH

%if %{compiler_family} == gnu
module load openblas
%endif
module load numpy
export CFLAGS="-I%buildroot%{install_path}/include -I$NUMPY_DIR$PPATH/numpy/core/include -I$(pwd)/src/public -L$(pwd)/src"
pushd wrappers/numpy
make MPI=y python
python setup.py install --prefix="%buildroot%{install_path}/python"
popd

%if 0%{?rhel_version} || 0%{?centos_version}
	find $RPM_BUILD_ROOT -type f -exec sed -i "s|$RPM_BUILD_ROOT||g" {} \;
%endif

install -m644 utils/skel/lib/skel_suite.py \
	utils/skel/lib/skel_template.py \
	utils/skel/lib/skel_test_plan.py \
	%buildroot%{install_path}/python

rm -f $(find examples -name '*.o') \
	examples/staging/stage_write/writer_adios
rm -f $(find examples -type f -name .gitignore)
rm -rf $(find examples -type d -name ".libs")
chmod 644 $(find examples -type f -name "*.xml")

install -d %buildroot%{install_path}/lib
cp -fR examples %buildroot%{install_path}/lib

mv %buildroot%{install_path}/lib/python/*.py %buildroot%{install_path}/python

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set             version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib
prepend-path	PYTHONPATH          %{install_path}/python/lib64/python2.7/site-packages

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_DOC        %{install_path}/docs
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_ETC        %{install_path}/etc
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%if 0%{?sles_version} || 0%{?suse_version}
# This happens last -- compiler and mpi _family are unset after
%fdupes -s %buildroot%{install_path}/lib/examples
%endif

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%doc AUTHORS
%doc COPYING
%doc ChangeLog
%doc KNOWN_BUGS
%doc NEWS
%doc README
%doc TODO

%changelog
