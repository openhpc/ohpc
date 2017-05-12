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
# spec file for package mumps
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

%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname mumps
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        5.1.1
Release:        0%{?dist}
Summary:        A MUltifrontal Massively Parallel Sparse direct Solver
License:        CeCILL-C
Group:          %{PROJ_NAME}/parallel-libs
Url:            http://mumps.enseeiht.fr/
Source0:        http://mumps.enseeiht.fr/MUMPS_%{version}.tar.gz
Source1:        Makefile.gnu.openmpi.inc
Source2:        Makefile.gnu.impi.inc
Source3:        Makefile.mkl.intel.impi.inc
Source4:        Makefile.mkl.intel.openmpi.inc
Source5:        OHPC_macros
Source7:        OHPC_setup_mpi
Patch0:         mumps-5.0.1-shared-mumps.patch
Patch1:         mumps-5.0.0-shared-pord.patch
Patch2:         mumps-5.0.2-psxe2017.patch

%if 0%{?suse_version}
BuildRequires: libgomp1
%else
BuildRequires: libgomp
%endif

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
MUMPS implements a direct solver for large sparse linear systems, with a
particular focus on symmetric positive definite matrices.  It can
operate on distributed matrices e.g. over a cluster.  It has Fortran and
C interfaces, and can interface with ordering tools such as Scotch.

%prep
%setup -q -n MUMPS_%{version}
%patch0 -p1
%patch1 -p1
<<<<<<< HEAD
%if %{compiler_family} == intel
#%patch2 -p1
=======
%if "%{compiler_family}" == "intel"
%patch2 -p2
>>>>>>> adrianreber-gcc7
%endif

%build
%ohpc_setup_compiler
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_mpi

# Enable scalapack linkage for blas/lapack with gnu builds
%if "%{compiler_family}" != "intel"
module load scalapack openblas
%endif

# Select appropriate Makefile.inc with MKL
%if "%{mpi_family}" == "impi"
export LIBS="-L$MPI_DIR/lib -lmpi"
%if "%{compiler_family}" == "gnu"
cp -f %{S:2} Makefile.inc
%endif
%if "%{compiler_family}" == "intel"
cp -f %{S:3} Makefile.inc
%endif
%endif

%if "%{mpi_family}" == "mpich"
export LIBS="-L$MPI_DIR/lib -lmpi"
%if "%{compiler_family}" == "intel"
cp -f %{S:3} Makefile.inc
%else
cp -f %{S:2} Makefile.inc
%endif
%endif
%if "%{mpi_family}" == "mvapich2"
export LIBS="-L$MPI_DIR/lib -lmpi"
%if "%{compiler_family}" == "intel"
cp -f %{S:3} Makefile.inc
%else
cp -f %{S:2} Makefile.inc
%endif
%endif

%if "%{mpi_family}" == "openmpi"
export LIBS="-L$MPI_DIR/lib -lmpi_mpifh -lmpi"
%if "%{compiler_family}" == "intel"
cp -f %{S:4} Makefile.inc
%else
cp -f %{S:1} Makefile.inc
%endif
%endif

make MUMPS_MPI=$OHPC_MPI_FAMILY \
     FC=mpif77 \
     MUMPS_LIBF77="$LIBS" \
     OPTC="$RPM_OPT_FLAGS" all


%install

%{__mkdir} -p %{buildroot}%{install_path}/lib
%{__mkdir} -p %{buildroot}%{install_path}/include
%{__mkdir} -p %{buildroot}%{install_path}/PORD/lib
%{__mkdir} -p %{buildroot}%{install_path}/PORD/include
%{__mkdir} -p %{buildroot}%{install_path}/etc

rm PORD/lib/sort*
mv PORD/lib/*so* lib/.
mv PORD/include/* include/.

install -m 755 lib/*so* %{buildroot}%{install_path}/lib
install -m 644 include/* %{buildroot}%{install_path}/include
install -m 644 Makefile.inc %{buildroot}%{install_path}/etc


# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
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

if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if { [is-loaded gnu] } {
        if { ![is-loaded scalapack]  } {
          module load scalapack
        }
    }
}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

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
%{OHPC_HOME}
%{OHPC_PUB}
%doc ChangeLog CREDITS INSTALL LICENSE README VERSION

%changelog
* Wed Feb 22 2017 Adrian Reber <areber@redhat.com> - 5.0.2-0
- Switching to %%ohpc_compiler macro
