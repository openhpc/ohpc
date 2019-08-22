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
%define pname extrae

Summary:	Extrae tool
Name:		%{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:	3.7.0
Release:	1%{?dist}
License:	LGPLv2+
Group:		%{PROJ_NAME}/perf-tools
URL:		https://tools.bsc.es
Source0:	https://ftp.tools.bsc.es/extrae/extrae-%{version}-src.tar.bz2


BuildRequires:	autoconf%{PROJ_DELIM}
BuildRequires:	automake%{PROJ_DELIM}
BuildRequires:	libtool%{PROJ_DELIM}
BuildRequires:	binutils-devel
BuildRequires:	libxml2-devel
BuildRequires:	papi%{PROJ_DELIM}
Requires:	papi%{PROJ_DELIM}
#!BuildIgnore:  post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
Extrae is the package devoted to generate Paraver trace-files for a post-mortem analysis. Extrae is a tool that uses different interposition mechanisms to inject probes into the target application so as to gather information regarding the application performance.

This is the %{compiler_family}-%{mpi_family} version.

%prep
%setup -q -n %{pname}-%{version}

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load autotools
module load papi

%if  "%{compiler_family}" == "intel"
%if  "%{mpi_family}" == "impi"
export compiler_vars="CC=icc CXX=icpc MPICC=mpicc MPIF90=mpiifort"
%endif
%endif

./bootstrap
./configure $compiler_vars \
            --with-xml-prefix=/usr \
            --with-papi=$PAPI_DIR  \
            --without-unwind \
            --without-dyninst \
            --disable-openmp-intel \
            --prefix=%{install_path} \
            --libdir=%{install_path}/lib \
            --with-mpi=$MPI_DIR \
%if  "%{mpi_family}" == "impi"
            --with-mpi-libs=$MPI_DIR/lib/release \
%endif
    || { cat config.log && exit 1; }

make %{?_smp_mflags}

%install
export NO_BRP_CHECK_RPATH=true

# OpenHPC compiler designation
%ohpc_setup_compiler

module load papi

make DESTDIR=$RPM_BUILD_ROOT install

# don't package static libs
rm -f $RPM_BUILD_ROOT%{install_path}/lib/*.la
rm -f $RPM_BUILD_ROOT%{install_path}/lib/*.a

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

depends-on papi

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
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

