#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# scotch/ptscotch - Graph, mesh and hypergraph partitioning library
# Build that is dependent on compiler and conditionally the mpi toolchains
%define ohpc_compiler_dependent 1
%{!?ohpc_mpi_dependent:%define ohpc_mpi_dependent 1}
%include %{_sourcedir}/OHPC_macros

# Base package name
%define base_pname scotch

%if 0%{?ohpc_mpi_dependent}
%define pname pt%{base_pname}
Name:		%{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:	Graph, mesh and hypergraph partitioning library using MPI
Group:		%{PROJ_NAME}/parallel-libs
%else
%define pname %{base_pname}
Name:		%{pname}-%{compiler_family}%{PROJ_DELIM}
Summary:	Graph, mesh and hypergraph partitioning library
Group:		%{PROJ_NAME}/serial-libs
%endif
Version:	7.0.11
Release:	1%{?dist}
License:	CeCILL-C
URL:		https://gitlab.inria.fr/scotch/scotch
Source0:	https://gitlab.inria.fr/scotch/scotch/-/archive/v%{version}/%{base_pname}-v%{version}.tar.bz2
Source1:	%{base_pname}-rpmlintrc

BuildRequires:	flex bison make cmake%{PROJ_DELIM}
BuildRequires:	zlib-devel
%if 0%{?rhel} || 0%{?openEuler}
BuildRequires:	bzip2-devel
Requires:	bzip2-devel
%else
BuildRequires:	libbz2-devel
Requires:	libbz2-devel
%endif

# Default library install path
%if 0%{?ohpc_mpi_dependent}
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%{version}
%else
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%{version}
%endif

%description
Scotch is a software package for graph and mesh/hypergraph partitioning and
sparse matrix ordering.

%prep
%setup -q -n %{base_pname}-v%{version}

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler
module load cmake

cmake -S . -B build \
    -DCMAKE_INSTALL_PREFIX=%{install_path} \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_FLAGS="${CFLAGS} -fPIC" \
    -DBUILD_SHARED_LIBS=ON \
%if 0%{?ohpc_mpi_dependent}
    -DBUILD_PTSCOTCH=ON \
%else
    -DBUILD_PTSCOTCH=OFF \
%endif
    -DBUILD_LIBSCOTCHMETIS=ON \
    -DBUILD_LIBESMUMPS=ON \
    -DTHREADS=ON \
    -DIDXSIZE=64 \
    -DSCOTCH_DETERMINISTIC=FIXED_SEED

cmake --build build %{?_smp_mflags}

%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler
module load cmake

DESTDIR=%{buildroot} cmake --install build

# Convert the license files to utf8
pushd doc
iconv -f iso8859-1 -t utf-8 < CeCILL-C_V1-en.txt > CeCILL-C_V1-en.txt.conv
iconv -f iso8859-1 -t utf-8 < CeCILL-C_V1-fr.txt > CeCILL-C_V1-fr.txt.conv
mv -f CeCILL-C_V1-en.txt.conv CeCILL-C_V1-en.txt
mv -f CeCILL-C_V1-fr.txt.conv CeCILL-C_V1-fr.txt
popd

# OpenHPC module file
%if 0%{?ohpc_mpi_dependent}
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}%{OHPC_CUSTOM_PKG_DELIM}
%else
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}%{OHPC_CUSTOM_PKG_DELIM}
%endif
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
%if 0%{?ohpc_mpi_dependent}
puts stderr "toolchain and the %{mpi_family} MPI stack."
%else
puts stderr "toolchain."
%endif
puts stderr "\nVersion %{version}\n"

}
%if 0%{?ohpc_mpi_dependent}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
%else
module-whatis "Name: %{pname} built with %{compiler_family} compiler"
%endif
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib64

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib64
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%if 0%{?ohpc_mpi_dependent}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}%{OHPC_CUSTOM_PKG_DELIM}
%else
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}%{OHPC_CUSTOM_PKG_DELIM}
%endif
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}%{OHPC_CUSTOM_PKG_DELIM}"
EOF

%files
%doc README.txt ./doc/*
%{OHPC_PUB}
