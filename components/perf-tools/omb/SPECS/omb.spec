#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# MVAPICH2 MPI stack that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name/config
%define pname omb

Summary:   OSU Micro-benchmarks
Name:      %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   5.8
Release:   1%{?dist}
License:   BSD
Group:     %{PROJ_NAME}/perf-tools
URL:       http://mvapich.cse.ohio-state.edu/benchmarks/
Source0:   http://mvapich.cse.ohio-state.edu/download/mvapich/osu-micro-benchmarks-%{version}.tgz

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description

The OSU micro-benchmarks feature a series of MPI benchmarks that
measure the performances of various MPI operations including: 

* Point-to-Point MPI Benchmarks: Latency, multi-threaded latency,
  multi-pair latency, multiple bandwidth / message rate test
  bandwidth, bidirectional bandwidth

* Collective MPI Benchmarks: Collective latency tests for various MPI
  collective operations such as MPI_Allgather, MPI_Alltoall,
  MPI_Allreduce, MPI_Barrier, MPI_Bcast, MPI_Gather, MPI_Reduce,
  MPI_Reduce_Scatter, MPI_Scatter and vector collectives.

* One-sided MPI Benchmarks: one-sided put latency (active/passive),
  one-sided put bandwidth (active/passive), one-sided put
  bidirectional bandwidth, one-sided get latency (active/passive),
  one-sided get bandwidth (active/passive), one-sided accumulate
  latency (active/passive), compare and swap latency (passive), and
  fetch and operate (passive).

%prep

%setup -q -n osu-micro-benchmarks-%{version}

%build
%ohpc_setup_compiler
./configure CC=mpicc CXX=mpicxx \
    --prefix=%{install_path} \
    --libexec=%{install_path}/bin/ || { cat config.log && exit 1; }

make %{?_smp_mflags}

%install
%ohpc_setup_compiler

make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the OSU Micro-benchmarks built with the %{compiler_family} toolchain"
puts stderr "and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}

module-whatis "Name: OSU Micro-benchmarks built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: performance tool"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin/osu-micro-benchmarks/mpi/startup
prepend-path    PATH                %{install_path}/bin/osu-micro-benchmarks/mpi/pt2pt
prepend-path    PATH                %{install_path}/bin/osu-micro-benchmarks/mpi/one-sided
prepend-path    PATH                %{install_path}/bin/osu-micro-benchmarks/mpi/collective

setenv          %{PNAME}_DIR        %{install_path}

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%{OHPC_HOME}
%doc COPYRIGHT
%doc CHANGES
%doc README
