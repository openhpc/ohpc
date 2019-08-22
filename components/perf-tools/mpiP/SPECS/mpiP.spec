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

# Note: this package is slightly non-standard in that we always use
# gnu compilers underneath in order to support call-site demangling
%if "%{compiler_family}" == "intel"
Requires:      intel-compilers-devel%{PROJ_DELIM}
BuildRequires: gnu8-compilers%{PROJ_DELIM}
Requires:      gnu8-compilers%{PROJ_DELIM}
%if "%{mpi_family}" != "impi"
BuildRequires: %{mpi_family}-gnu8%{PROJ_DELIM}
Requires:      %{mpi_family}-gnu8%{PROJ_DELIM}
%endif
%endif

# Base package name
%define pname mpiP

Summary:   mpiP: a lightweight profiling library for MPI applications.
Name:      %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   3.4.1
Release:   1%{?dist}
License:   BSD-3
Group:     %{PROJ_NAME}/perf-tools
URL:       http://mpip.sourceforge.net/
Source0:   http://sourceforge.net/projects/mpip/files/mpiP/mpiP-3.4.1/mpiP-%{version}.tar.gz
Patch1:    mpip.unwinder.patch

BuildRequires: binutils-devel
BuildRequires: python

# Default library install path
%global install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version
%global module_dir %{compiler_family}-%{mpi_family}

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
%patch1 -p1

%build

# override with newer config.guess for aarch64
%ifarch aarch64 || ppc64le
cp /usr/lib/rpm/config.guess bin
%endif

# OpenHPC compiler/mpi designation

# note: in order to support call-site demangling, we compile mpiP with gnu
. %{OHPC_ADMIN}/ohpc/OHPC_setup_compiler gnu8
module load %{mpi_family}

CC=mpicc
CXX=mpicxx
FC=mpif90

./configure --prefix=%{install_path} \
            --libdir=%{install_path}/lib \
            --enable-demangling \
            --disable-libunwind \
%ifarch aarch64
            --enable-setjmp \
%endif
            || { cat config.log && exit 1; }

%install

# OpenHPC compiler designation

# note: in order to support call-site demangling, we compile mpiP with gnu
. %{OHPC_ADMIN}/ohpc/OHPC_setup_compiler gnu8
module load %{mpi_family}

make %{?_smp_mflags} shared
make DESTDIR=$RPM_BUILD_ROOT install

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{module_dir}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{module_dir}/%{pname}/%{version}
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

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{module_dir}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

# Remove static libs
rm -rf $RPM_BUILD_ROOT/%{install_path}/lib/*.a

%files
%{OHPC_PUB}
%doc ChangeLog doc/PORTING.txt doc/README doc/UserGuide.txt
