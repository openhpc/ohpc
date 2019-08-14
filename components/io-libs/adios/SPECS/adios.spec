#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is dependent on compiler/mpi toolchains
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%define ohpc_python_dependent 1
%global python_family python2
%include %{_sourcedir}/OHPC_macros

%{!?with_lustre: %global with_lustre 0}

# Base package name
%define pname adios

Summary: The Adaptable IO System (ADIOS)
Name:    %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version: 1.13.1
Release: 1%{?dist}
License: BSD-3-Clause
Group:   %{PROJ_NAME}/io-libs
Url:     http://www.olcf.ornl.gov/center-projects/adios/
Source0: http://users.nccs.gov/~pnorbert/adios-%{version}.tar.gz
Patch1:  adios-return-value.patch
AutoReq: no

BuildRequires: zlib-devel glib2-devel
Requires:      zlib zlib-devel

# libm.a from CMakeLists
BuildRequires: glibc-static

BuildRequires: libtool%{PROJ_DELIM}
Requires:      lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires: phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:      phdf5-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

BuildRequires: netcdf-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:      netcdf-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

%if 0%{with_lustre}
# This is the legacy name for lustre-lite
# BuildRequires: liblustre-devel
BuildRequires: lustre-lite
Requires: lustre-client%{PROJ_DELIM}
%endif
BuildRequires: %{python_prefix}-numpy-%{compiler_family}%{PROJ_DELIM}


%if 0%{?sle_version} || 0%{?suse_version}
# define fdupes, clean up rpmlint errors
BuildRequires: fdupes libcurl4 libcurl-devel
%else
BuildRequires: libcurl libcurl-devel
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
%patch1 -p1

%build
sed -i 's|@BUILDROOT@|%buildroot|' wrappers/numpy/setup*
%ifarch x86_64
LIBSUFF=64
%endif
sed -i "s|@64@|$LIBSUFF|" wrappers/numpy/setup*

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load autotools
module load phdf5
module load netcdf
module load %{python_module_prefix}numpy
module list

TOPDIR=$PWD

export CFLAGS="-fPIC -I$TOPDIR/src/public -I$MPI_DIR/include -I$NETCDF_INC -I$HDF5_INC -pthread -lpthread -L$NETCDF_LIB -lnetcdf -L$HDF5_LIB"

# These lines break intel builds, but are required for gnu mvapich2
%if "%{compiler_family}" != "intel"
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

%if "%{compiler_family}" == "intel"
export CFLAGS="-fp-model strict $CFLAGS"
%endif

# work around old config.guess on aarch64 systems
%ifarch aarch64
cp /usr/lib/rpm/config.guess config
%endif

./configure --prefix=%{install_path} \
    --libdir=%{install_path}/lib \
    --enable-shared=yes \
    --enable-static=no \
%if 0%{with_lustre}
    --with-lustre=/usr/include/lustre \
%endif
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
    #    --with-mxml=/usr \

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
%ohpc_setup_compiler
export NO_BRP_CHECK_RPATH=true

make DESTDIR=$RPM_BUILD_ROOT install

# this is clearly generated someway and shouldn't be static
export PPATH="/lib64/%{python_lib_dir}/site-packages"
export PATH=$(pwd):$PATH

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
module load openblas
%endif
module load %{python_module_prefix}numpy
export CFLAGS="-I$NUMPY_DIR$PPATH/numpy/core/include -I$(pwd)/src/public -L$(pwd)/src"
pushd wrappers/numpy
make MPI=y python

#%{python_prefix} setup.py install --prefix="%buildroot%{install_path}/python"
python setup.py install --prefix="%buildroot%{install_path}/python"
popd

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
find %buildroot%{install_path}/python -name \*pyc -exec sed -i "s|$RPM_BUILD_ROOT||g" {} \;

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

depends-on phdf5

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib
prepend-path	PYTHONPATH          %{install_path}/python/lib64/%{python_lib_dir}/site-packages

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
%{OHPC_HOME}
%doc AUTHORS
%doc COPYING
%doc ChangeLog
%doc KNOWN_BUGS
%doc NEWS
%doc README.md
%doc TODO
