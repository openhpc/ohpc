#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#-fsp-header-comp-begin-----------------------------

%include %{_sourcedir}/OHPC_macros

# OpenHPC convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family and mpi_family variables via rpmbuild or other
# mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family: %define mpi_family openmpi}
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

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

#-fsp-header-comp-end-------------------------------

# Base package name
%define pname blacs
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        1.1
Release:        0
Summary:        A MUltifrontal Massively Parallel Sparse direct Solver
License:        BSD
Group:          fsp/parallel-libs
Url:            http://www.netlib.org/blacs
Source0:        BLACS.tar.gz
Source1:        Bmake.inc
Patch0:         BLACS-1.1.shared.patch
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
DocDir:         %{OHPC_PUB}/doc/contrib

%define debug_package %{nil}

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
The BLACS (Basic Linear Algebra Communication Subprograms) project is an ongoing 
investigation whose purpose is to create a linear algebra oriented message passing 
interface that may be implemented efficiently and uniformly across a large range 
of distributed memory platforms.

%prep
%setup -q -n BLACS
%patch0 -p1

%build

export OHPC_COMPILER_FAMILY=%{compiler_family}
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_compiler
. %{_sourcedir}/OHPC_setup_mpi

cp -f %{S:1} .
make mpi

%install

%{__mkdir} -p %{buildroot}%{install_path}/include
%{__mkdir} -p %{buildroot}%{install_path}/lib


install -m 644 SRC/MPI/*so* %{buildroot}%{install_path}/lib
install -m 644 LIB/*a %{buildroot}%{install_path}/lib
install -m 644 SRC/MPI/*h %{buildroot}%{install_path}/include

# FSP module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the BLACS library built with the %{compiler_family} compiler"
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

prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
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

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc README

%changelog

