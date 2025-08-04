#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is is dependent on compiler toolchain and MPI
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

%global gnu_family gnu15

# Base package name
%define pname mumps

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        5.8.1
Release:        1%{?dist}
Summary:        A MUltifrontal Massively Parallel Sparse direct Solver
License:        CeCILL-C
Group:          %{PROJ_NAME}/parallel-libs
Url:            https://mumps-solver.org/
Source0:        http://mumps-solver.org/MUMPS_%{version}.tar.gz
Source1:        Makefile.gnu.inc
Source2:        Makefile.mkl.intel.inc
Source3:        Makefile.arm.inc
Requires:       lmod%{PROJ_DELIM} >= 7.6.1

%if 0%{?rhel} || 0%{?openEuler}
BuildRequires: libgomp
%else
BuildRequires: libgomp1
%endif

BuildRequires: make

# Every other family needs scalapack
%if "%{compiler_family}" != "intel"
BuildRequires: scalapack-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:      scalapack-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
%endif

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%version

%description
MUMPS implements a direct solver for large sparse linear systems, with a
particular focus on symmetric positive definite matrices.  It can
operate on distributed matrices e.g. over a cluster.  It has Fortran and
C interfaces, and can interface with ordering tools such as Scotch.

%prep
%setup -q -n MUMPS_%{version}

%build
%ohpc_setup_compiler

%if "%{compiler_family}" == "arm1"
module load scalapack
%endif

# Enable scalapack and openblas linkage for blas/lapack with gnu and other (e.g. llvm) builds
%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
module load scalapack openblas
%endif

export LIBS="-L$MPI_DIR/lib -lmpi"
%if "%{compiler_family}" == "%{gnu_family}"
cp -f %{S:1} Makefile.inc
%endif
%if "%{compiler_family}" == "intel"
cp -f %{S:2} Makefile.inc
%endif
%if "%{compiler_family}" == "arm1"
cp -f %{S:3} Makefile.inc
%endif

%if "%{mpi_family}" == "openmpi5"
%global MUMPS_MPI openmpi
export LIBS="-L$MPI_DIR/lib -lmpi_mpifh -lmpi"
%else
%global MUMPS_MPI $OHPC_MPI_FAMILY
export LIBS="-L$MPI_DIR/lib -lmpi"
%endif

%if "%{compiler_family}" == "%{gnu_family}"
export FCFLAGS="$FCFLAGS -fallow-argument-mismatch"
export FCFLAGS="$FCFLAGS -Wno-unused-dummy-argument"
export FCFLAGS="$FCFLAGS -Wno-misleading-indentation"
export FCFLAGS="$FCFLAGS -Wno-unused-function"
export FCFLAGS="$FCFLAGS -Wno-unused-variable"
export FCFLAGS="$FCFLAGS -Wno-maybe-uninitialized"
export FCFLAGS="$FCFLAGS -Wno-conversion"
export FCFLAGS="$FCFLAGS -Wno-target-lifetime"
export FCFLAGS="$FCFLAGS -Wno-argument-mismatch"

export CFLAGS="$CFLAGS -Wno-unused-function"
export CFLAGS="$CFLAGS -Wno-strict-aliasing"
export CFLAGS="$CFLAGS -Wno-misleading-indentation"
%endif
%if "%{compiler_family}" == "arm1"
export CFLAGS="$CFLAGS -fsimdmath"
%endif

make MUMPS_MPI=%{MUMPS_MPI} \
     FC=mpif77 \
     MUMPS_LIBF77="$LIBS" \
     OPTC="$CFLAGS" OPTF="$FCFLAGS" allshared %{?_smp_mflags}


%install

%{__mkdir} -p %{buildroot}%{install_path}/lib
%{__mkdir} -p %{buildroot}%{install_path}/include
%{__mkdir} -p %{buildroot}%{install_path}/PORD/lib
%{__mkdir} -p %{buildroot}%{install_path}/PORD/include
%{__mkdir} -p %{buildroot}%{install_path}/etc

rm -v PORD/lib/sort*
mv -v PORD/lib/*so* lib/.
mv -v PORD/include/* include/.

install -m 755 lib/*so* %{buildroot}%{install_path}/lib
install -m 644 include/* %{buildroot}%{install_path}/include
install -m 644 Makefile.inc %{buildroot}%{install_path}/etc


# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the mumps library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr " "

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version                     %{version}

%if "%{compiler_family}" != "intel"
depends-on scalapack
%endif

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_LIB        %{install_path}/lib

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}%{OHPC_CUSTOM_PKG_DELIM}"
EOF

%{__mkdir} -p %{buildroot}/%{_docdir}

%files
%{OHPC_PUB}
%doc ChangeLog CREDITS INSTALL LICENSE README VERSION
