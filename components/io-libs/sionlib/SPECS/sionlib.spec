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
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname sionlib

Summary:   Scalable I/O Library for Parallel Access to Task-Local Files
Name:      %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   1.7.7
Release:   1%{?dist}
License:   BSD
Group:     %{PROJ_NAME}/io-libs
URL:       https://apps.fz-juelich.de/jsc/sionlib/docu/index.html
Source0:   http://apps.fz-juelich.de/jsc/sionlib/download.php?version=%{version}#/%{pname}-%{version}.tar.gz
Patch0:    sionlib-llvm-arm.patch

# For pre-processor only:
%if 0%{?rhel} || 0%{?openEuler}
BuildRequires: gcc-gfortran
%else
BuildRequires: gcc-fortran
%endif

BuildRequires: make gcc-c++

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
SIONlib is a library for writing and reading binary data to/from several
thousands of processors into one or a small number of physical files.

This is the %{compiler_family}-%{mpi_family} version.


%prep

%setup -q -n %{pname}
%patch0 -p1

%build

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "intel"
CONFIGURE_OPTIONS="--compiler=intel --disable-parutils "
%endif
%if "%{compiler_family}" == "arm1"
CONFIGURE_OPTIONS="--compiler=arm "
%endif
%if "%{compiler_family}" == "llvm"
CONFIGURE_OPTIONS="--compiler=llvm "
%endif
%if "%{compiler_family}" == "gnu14"
CONFIGURE_OPTIONS="--compiler=gnu "
%endif
%if "%{compiler_family}" == "gnu14"
CONFIGURE_OPTIONS="--compiler=gnu "
%endif

%if "%{mpi_family}" == "impi"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --mpi=intel2 "
%endif

%if "%{mpi_family}" == "mpich"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --mpi=mpich3 "
%endif

%if "%{mpi_family}" == "mvapich2"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --mpi=mpich3 "
%endif

%if "%{mpi_family}" == "openmpi"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --mpi=openmpi "
%endif

%if "%{mpi_family}" == "openmpi4"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --mpi=openmpi "
%endif

%if "%{mpi_family}" == "openmpi5"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --mpi=openmpi "
%endif

./configure --prefix=%{buildroot}%{install_path} $CONFIGURE_OPTIONS

# remove ARM incompatible cflag
%ifnarch x86_64
sed -i 's|-m$(PREC)||g' build-*/Makefile.defs
%endif

%if "%{compiler_family}" == "intel"
sed -i 's|-g|-g -fpic|g' build-*/Makefile.defs
sed -i "s/^CC.*/CC=$CC/g" build-*/Makefile.defs
sed -i "s/^CXX.*icpc/CXX=$CXX/g" build-*/Makefile.defs
sed -i "s/^F77.*/F77=$F77/g" build-*/Makefile.defs
sed -i "s/-mieee-fp//g;s/-wd161//g;" build-*/Makefile.defs
sed -i 's/$(CPP)/ifx/g' src/fortraninterface/Makefile
%endif

%if "%{compiler_family}" == "gnu14"
sed -i 's/FFLAGS.*/& -fallow-argument-mismatch/g' build-*/Makefile.defs
sed -i 's/F90FLAGS.*/& -fallow-argument-mismatch/g' build-*/Makefile.defs
sed -i 's/F90 .*/& -fallow-argument-mismatch/g' build-*/Makefile.defs
%endif

sed -i "s/CFLAGS.*/& $CFLAGS/g" build-*/Makefile.defs
sed -i "s/CXXFLAGS.*/& $CXXFLAGS/g" build-*/Makefile.defs
sed -i "s/FFLAGS.*/& $FCFLAGS/g" build-*/Makefile.defs
sed -i "s/F90FLAGS.*/& $FCFLAGS/g" build-*/Makefile.defs

make %{?_smp_mflags}

%install

# OpenHPC compiler designation
%ohpc_setup_compiler

make DESTDIR=$RPM_BUILD_ROOT install

# don't package static libs
rm -f $RPM_BUILD_ROOT%{install_path}/lib/*la

# clean buildroot
sed -i 's|%{buildroot}||g' %{buildroot}%{install_path}/bin/sionconfig
sed -i 's|%{buildroot}||g' %{buildroot}%{install_path}/examples/simple/Makefile.defs
sed -i 's|%{buildroot}||g' %{buildroot}%{install_path}/examples/mp2c/Makefile.defs

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: IO Library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
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

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%{OHPC_PUB}
%doc COPYRIGHT LICENSE README INSTALL RELEASE
