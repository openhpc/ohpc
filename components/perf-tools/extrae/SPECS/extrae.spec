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
Version:	3.8.3
Release:	1%{?dist}
License:	LGPLv2+
Group:		%{PROJ_NAME}/perf-tools
URL:		https://tools.bsc.es
Source0:	https://ftp.tools.bsc.es/extrae/extrae-%{version}-src.tar.bz2
Patch0:		arm.function.definition.patch


BuildRequires:	autoconf%{PROJ_DELIM}
BuildRequires:	automake%{PROJ_DELIM}
BuildRequires:	libtool%{PROJ_DELIM} make which
BuildRequires:	binutils-devel
BuildRequires:	libxml2-devel
BuildRequires:	papi%{PROJ_DELIM}
Requires:	papi%{PROJ_DELIM}
#!BuildIgnore:  post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
Extrae is the package devoted to generate Paraver trace-files for a post-mortem
analysis. Extrae is a tool that uses different interposition mechanisms to
inject probes into the target application so as to gather information regarding
the application performance.

This is the %{compiler_family}-%{mpi_family} version.

%prep
%setup -q -n %{pname}-%{version}
%patch -P0 -p0

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load papi

export compiler_vars="MPICC=$(which mpicc)"

%if "%{compiler_family}" == "intel"
%if "%{mpi_family}" == "impi"
export compiler_vars="CC=${CC} CXX=${CXX} MPIF90=mpiifort $compiler_vars"
%endif
%endif

export PATH=%{OHPC_UTILS}/autotools/bin:${PATH}
./bootstrap
export LDFLAGS="$LDFLAGS -lz"
%if 0%{?sle_version}
export LDFLAGS="$LDFLAGS -lsframe"
%endif
export CFLAGS="${CFLAGS} -Wno-implicit-function-declaration"
export CFLAGS="${CFLAGS} -Wno-incompatible-pointer-types"
%if "%{compiler_family}" == "arm1"
export CFLAGS="${CFLAGS} -fsimdmath -fPIC"
export CXXFLAGS="${CXXFLAGS} -fsimdmath -fPIC"
export FCFLAGS="${FCFLAGS} -fsimdmath -fPIC"
%endif
./configure $compiler_vars --with-xml-prefix=/usr --with-papi=$PAPI_DIR  --without-unwind \
    --without-dyninst --disable-openmp-intel --prefix=%{install_path} --with-mpi=$MPI_DIR \
%if "%{mpi_family}" == "impi"
    --with-mpi-libs=$MPI_DIR/lib/release \
%endif
    || { cat config.log && exit 1; }

make %{?_smp_mflags} V=1

%install
export NO_BRP_CHECK_RPATH=true

# OpenHPC compiler designation
%ohpc_setup_compiler

module load papi

make DESTDIR=$RPM_BUILD_ROOT install

# fix a path in one of the scripts
sed -e "s,export EXTRAE_HOME=.*,export EXTRAE_HOME=%{install_path},g" -i $RPM_BUILD_ROOT/%{install_path}/share/tests/overhead/run_overhead_tests.sh

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
