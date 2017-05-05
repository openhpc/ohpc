#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Scalasca library that is is dependent on compiler toolchain and MPI

%include %{_sourcedir}/OHPC_macros
%ohpc_compiler

%{!?mpi_family:      %global mpi_family openmpi}

# MPI dependencies
%if %{mpi_family} == impi
BuildRequires: intel-mpi-devel%{PROJ_DELIM}
Requires:      intel-mpi-devel%{PROJ_DELIM}
%endif
%if %{mpi_family} == mpich
BuildRequires: mpich-%{compiler_family}%{PROJ_DELIM}
Requires:      mpich-%{compiler_family}%{PROJ_DELIM}
%endif
%if %{mpi_family} == mvapich2
BuildRequires: mvapich2-%{compiler_family}%{PROJ_DELIM}
Requires:      mvapich2-%{compiler_family}%{PROJ_DELIM}
%endif
%if %{mpi_family} == openmpi
BuildRequires: openmpi-%{compiler_family}%{PROJ_DELIM}
Requires:      openmpi-%{compiler_family}%{PROJ_DELIM}
%endif

# Base package name
%define pname scalasca
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary:   Toolset for performance analysis of large-scale parallel applications
Name:      %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   2.3.1
Release:   1%{?dist}
License:   BSD
Group:     %{PROJ_NAME}/perf-tools
URL:       http://www.scalasca.org
Source0:   http://apps.fz-juelich.de/scalasca/releases/scalasca/2.3/dist/scalasca-%{version}.tar.gz
Source1:   OHPC_macros
Source3:   OHPC_setup_mpi
BuildRequires: scorep-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires: scorep-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:  scorep-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires: zlib-devel

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
Scalasca is a software tool that supports the performance
optimization of parallel programs by measuring and analyzing their
runtime behavior. The analysis identifies potential performance
bottlenecks – in particular those concerning communication and
synchronization – and offers guidance in exploring their causes.

Scalasca targets mainly scientific and engineering applications
based on the programming interfaces MPI and OpenMP, including
hybrid applications based on a combination of the two. The tool
has been specifically designed for use on large-scale systems, but
is also well suited for small- and medium-scale HPC platforms.

This is the %{compiler_family}-%{mpi_family} version.


%prep

%setup -q -n %{pname}-%{version}

%build

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_mpi

module load scorep

%if %{compiler_family} == intel
CONFIGURE_OPTIONS="--with-nocross-compiler-suite=intel "
%endif

%if %{mpi_family} == impi
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=intel3 "
%endif

%if %{mpi_family} == mpich
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=mpich3 "
%endif

%if %{mpi_family} == mvapich2
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=mpich3 "
%endif

%if %{mpi_family} == openmpi
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=openmpi "
%endif

./configure --prefix=%{install_path} $CONFIGURE_OPTIONS

%install

# OpenHPC compiler designation
%ohpc_setup_compiler
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_mpi

module load scorep

make DESTDIR=$RPM_BUILD_ROOT install

# don't package static libs
rm -rf $RPM_BUILD_ROOT%{install_path}/lib


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
module-whatis "Category: performance tool"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if { ![is-loaded scorep]  } {
      module load scorep
    }
}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin

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
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc ChangeLog COPYING INSTALL OPEN_ISSUES README


%changelog
* Wed Feb 22 2017 Adrian Reber <areber@redhat.com> - 2.3.1-1
- Switching to %%ohpc_compiler macro
