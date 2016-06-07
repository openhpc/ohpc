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

# OpenHPC convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family and mpi_family variables via rpmbuild or other
# mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family:      %define mpi_family openmpi}

# Lmod dependency (note that lmod is pre-populated in the OpenHPC OBS build
# environment; if building outside, lmod remains a formal build dependency).
%if !0%{?OHPC_BUILD}
BuildRequires: lmod%{PROJ_DELIM}
%endif

# Compiler dependencies 

# Note: this package is slightly non-standard in that we always use
# gnu compilers undernead in order to support call-site demangling

BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}

# MPI dependencies
%if %{mpi_family} == impi
BuildRequires: intel-mpi-devel%{PROJ_DELIM}
Requires:      intel-mpi-devel%{PROJ_DELIM}
%endif
%if %{mpi_family} == mvapich2
BuildRequires: mvapich2-gnu%{PROJ_DELIM}
Requires:      mvapich2-gnu%{PROJ_DELIM}
%endif
%if %{mpi_family} == openmpi
BuildRequires: openmpi-gnu%{PROJ_DELIM}
Requires:      openmpi-gnu%{PROJ_DELIM}
%endif

#-ohpc-header-comp-end------------------------------------------------

# Base package name
%define pname mpiP
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary:   mpiP: a lightweight profiling library for MPI applications.
Name:      %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   3.4.1
Release:   1
License:   BSD-3
Group:     %{PROJ_NAME}/perf-tools
URL:       http://mpip.sourceforge.net/
Source0:   http://sourceforge.net/projects/mpip/files/mpiP/mpiP-3.4.1/mpiP-%{version}.tar.gz
Source1:   OHPC_macros
Source2:   OHPC_setup_compiler
Source3:   OHPC_setup_mpi
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root
DocDir:    %{OHPC_PUB}/doc/contrib

BuildRequires: binutils-devel
BuildRequires: python

%define debug_package %{nil}

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description 

mpiP is a lightweight profiling library for MPI applications. Because
it only collects statistical information about MPI functions, mpiP
generates considerably less overhead and much less data than tracing
tools. All the information captured by mpiP is task-local. It only
uses communication during report generation, typically at the end of
the experiment, to merge results from all of the tasks into one output
file.

%prep

%setup -q -n %{pname}-%{version}

%build

# OpenHPC compiler/mpi designation

# note: in order to support call-site demangling, we compile mpiP with gnu
export OHPC_COMPILER_FAMILY=gnu
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_compiler
. %{_sourcedir}/OHPC_setup_mpi

CC=mpicc
CXX=mpicxx
FC=mpif90



./configure --prefix=%{install_path} --enable-demangling --disable-libunwind || { cat config.log && exit 1; }

%install

# OpenHPC compiler designation

# note: in order to support call-site demangling, we compile mpiP with gnu
export OHPC_COMPILER_FAMILY=gnu
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_compiler
. %{_sourcedir}/OHPC_setup_mpi

make %{?_smp_mflags} 
make DESTDIR=$RPM_BUILD_ROOT install

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
module-whatis "Category: Profiling library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc ChangeLog doc/PORTING.txt doc/README doc/UserGuide.txt
