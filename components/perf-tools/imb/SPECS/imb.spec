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
%define pname imb

Summary:   Intel MPI Benchmarks (IMB)
Name:      %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   2021.3
Release:   1%{?dist}
License:   CPL
Group:     %{PROJ_NAME}/perf-tools
URL:       https://software.intel.com/en-us/articles/intel-mpi-benchmarks
Source0:   https://github.com/intel/mpi-benchmarks/archive/IMB-v%{version}.tar.gz

BuildRequires: make

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
The Intel MPI Benchmarks perform a set of MPI performance measurements for
point-to-point and global communication operations for a range of message sizes.

The generated benchmark data fully characterizes:
   - Performance of a cluster system, including node performance, network latency,
     and throughput
   - Efficiency of the MPI implementation used

%prep
%setup -n mpi-benchmarks-IMB-v%{version}


%build

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

export CC=mpicc
export CXX=mpicxx

export CFLAGS="$CFLAGS -I$MPI_DIR/include"
export CPPFLAGS="$CPPFLAGS -I$MPI_DIR/include"
export CXXFLAGS="$CXXFLAGS -I$MPI_DIR/include"

%if "%{compiler_family}" == "arm1"
export CFLAGS="${CFLAGS} -fsimdmath"
export CFLAGS="${CFLAGS} -Wno-register"
export CXXFLAGS="${CXXFLAGS} -fsimdmath"
export CXXFLAGS="${CXXFLAGS} -Wno-register"
%endif

make all


%install

# OpenHPC compiler designation
%ohpc_setup_compiler

%{__mkdir} -p %{buildroot}%{install_path}/bin
%{__cp} IMB-EXT  %{buildroot}%{install_path}/bin/.
%{__cp} IMB-IO   %{buildroot}%{install_path}/bin/.
%{__cp} IMB-MPI1 %{buildroot}%{install_path}/bin/.
%{__cp} IMB-NBC  %{buildroot}%{install_path}/bin/.
%{__cp} IMB-RMA  %{buildroot}%{install_path}/bin/.

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
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin

setenv          %{PNAME}_DIR        %{install_path}

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
%{OHPC_PUB}
%doc ReadMe_IMB.txt license/license.txt license/use-of-trademark-license.txt
