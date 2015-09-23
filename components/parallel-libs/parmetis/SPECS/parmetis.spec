#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# parmetis library that is is dependent on compiler toolchain and MPI

#-fsp-header-comp-begin----------------------------------------------

%include %{_sourcedir}/OHPC_macros

# OpenHPC convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family and mpi_family variables via rpmbuild or other
# mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family: %define mpi_family openmpi}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM} coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%if 0%{?OHPC_build}
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

#-fsp-header-comp-end------------------------------------------------


# Base package name
%define pname metis
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:    par%{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary: Serial Graph Partitioning and Fill-reducing Matrix Ordering
Version: 4.0.3
Release: 1
License: BSD-like
Group:   fsp/parallel-libs
URL:     http://glaros.dtc.umn.edu/gkhome/metis/metis/overview
DocDir:  %{OHPC_PUB}/doc/contrib
Source0: par%{pname}-%{version}.tar.gz
Source1: OHPC_macros
Source2: OHPC_setup_compiler
Source3: OHPC_setup_mpi
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root

BuildRequires: make
BuildRequires: pkgconfig
BuildRequires: cmake
Requires:      libmetis0 = %{version}
Provides:      libmetis0 = %{version}

%define debug_package %{nil}

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/par%{pname}/%version

%description
METIS is a family of programs for partitioning unstructured graphs and hypergraph
and computing fill-reducing orderings of sparse matrices. The underlying algorithms
used by METIS are based on the state-of-the-art multilevel paradigm that has been
shown to produce high quality results and scale to very large problems.

%package -n libmetis0
Summary:        Serial Graph Partitioning and Fill-reducing Matrix Ordering
Group:          System/Libraries

%package devel
License:         Free for non-commercial use
Requires:        %name = %version
Provides:        %name = %version
Requires:	 pkgconfig
Summary:         Metis development files
Group:           Development/Libraries/C and C++

%description -n libmetis0
METIS is a family of programs for partitioning unstructured graphs and hypergraph
and computing fill-reducing orderings of sparse matrices. The underlying algorithms
used by METIS are based on the state-of-the-art multilevel paradigm that has been
shown to produce high quality results and scale to very large problems.

%description devel
METIS is a family of programs for partitioning unstructured graphs and hypergraph
and computing fill-reducing orderings of sparse matrices. The underlying algorithms
used by METIS are based on the state-of-the-art multilevel paradigm that has been
shown to produce high quality results and scale to very large problems.

%prep
%setup -q -n par%{pname}-%{version}
%build

# FSP compiler/mpi designation

%if %{compiler_family} == intel
export CFLAGS="-fPIC $CFLAGS"
%endif
export OHPC_COMPILER_FAMILY=%{compiler_family}
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_compiler
. %{_sourcedir}/OHPC_setup_mpi

make config shared=1 prefix=%{install_path}
make

%install

# FSP compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_compiler
. %{_sourcedir}/OHPC_setup_mpi

make install DESTDIR=${RPM_BUILD_ROOT}

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/par%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/par%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the parallel %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          PAR%{PNAME}_DIR        %{install_path}
setenv          PAR%{PNAME}_LIB        %{install_path}/lib
setenv          PAR%{PNAME}_INC        %{install_path}/include

family "parmetis"
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/par%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for par%{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%post -n libmetis0
/sbin/ldconfig

%postun -n libmetis0
/sbin/ldconfig

%clean
rm -fr ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)
%{OHPC_HOME}
%{OHPC_PUB}
%doc BUILD.txt Changelog Install.txt LICENSE.txt

%changelog
