#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %global PROJ_DELIM -ohpc}

Summary: Meta-packages to ease installation.
Name:    %{PROJ_NAME}
Version: 1.3.1
Release: 1
License: Apache-2.0
Group:   %{PROJ_NAME}/meta-package
URL:     https://github.com/openhpc/ohpc
Source0: LICENSE


BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
DocDir:    %{OHPC_PUB}/doc/contrib

%description

This is an internal package that is used to create groups as part
of the installation source setup.  Installation of this package does
not make sense.

%package autotools
Summary:   OpenHPC autotools
Requires:  autoconf%{PROJ_DELIM}
Requires:  automake%{PROJ_DELIM}
Requires:  libtool%{PROJ_DELIM}
%description autotools
OpenHPC collection of GNU autotools packages

%package base
Summary:   OpenHPC base
Requires:  bc
Requires:  conman%{PROJ_DELIM}
Requires:  emacs-nox
Requires:  examples%{PROJ_DELIM}
Requires:  gdb
Requires:  ipmitool
Requires:  libstdc++-devel
Requires:  lmod%{PROJ_DELIM}
Requires:  losf%{PROJ_DELIM}
Requires:  make
Requires:  man
Requires:  net-tools
Requires:  nfs-utils
Requires:  ntp
Requires:  OpenIPMI
Requires:  pdsh%{PROJ_DELIM}
Requires:  screen
Requires:  sudo
%if 0%{?centos_version} || 0%{?rhel_version}
Requires:  binutils
Requires:  binutils-devel
Requires:  man-db
%endif
%if 0%{?sles_version}
Requires:  glibc-locale
Requires:  libmlx4-rdmav2
Requires:  nfs-kernel-server
%endif
%description base
OpenHPC base packages

%package base-compute
Summary:   OpenHPC compute node base
Requires:  libicu
Requires:  libunwind
Requires:  numactl
%description base-compute
OpenHPC compute node base packages

%package ganglia
Summary:   OpenHPC Ganglia monitoring
Requires:  ganglia%{PROJ_DELIM}
Requires:  ganglia-gmetad%{PROJ_DELIM}
Requires:  ganglia-gmond%{PROJ_DELIM}
Requires:  ganglia-gmond-python%{PROJ_DELIM}
Requires:  ganglia-web%{PROJ_DELIM}
%description ganglia
OpenHPC collection of Ganglia monitoring and metrics packages

%package io-libs-gnu
Summary:   OpenHPC IO libraries for GNU
Requires:  adios-gnu-mpich%{PROJ_DELIM}
Requires:  adios-gnu-openmpi%{PROJ_DELIM}
Requires:  hdf5-gnu%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu-mpich%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu-openmpi%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu-mpich%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu-openmpi%{PROJ_DELIM}
Requires:  netcdf-gnu-mpich%{PROJ_DELIM}
Requires:  netcdf-gnu-openmpi%{PROJ_DELIM}
Requires:  phdf5-gnu-mpich%{PROJ_DELIM}
Requires:  phdf5-gnu-openmpi%{PROJ_DELIM}
%ifnarch aarch64 && !0%{?centos_version}
Requires:  adios-gnu-mvapich2%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu-mvapich2%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu-mvapich2%{PROJ_DELIM}
Requires:  netcdf-gnu-mvapich2%{PROJ_DELIM}
Requires:  phdf5-gnu-mvapich2%{PROJ_DELIM}
%endif
%description io-libs-gnu
OpenHPC IO library builds for use with GNU compiler toolchain

%package io-libs-intel
Summary:   OpenHPC IO libraries for Intel(R) Parallel Studio XE
Requires:  adios-gnu-impi%{PROJ_DELIM}
Requires:  adios-intel-impi%{PROJ_DELIM}
Requires:  adios-intel-mpich%{PROJ_DELIM}
Requires:  adios-intel-mvapich2%{PROJ_DELIM}
Requires:  adios-intel-openmpi%{PROJ_DELIM}
Requires:  hdf5-intel%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu-impi%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-impi%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-mpich%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-mvapich2%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-openmpi%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu-impi%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-impi%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-mpich%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-mvapich2%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-openmpi%{PROJ_DELIM}
Requires:  netcdf-gnu-impi%{PROJ_DELIM}
Requires:  netcdf-intel-impi%{PROJ_DELIM}
Requires:  netcdf-intel-mpich%{PROJ_DELIM}
Requires:  netcdf-intel-mvapich2%{PROJ_DELIM}
Requires:  netcdf-intel-openmpi%{PROJ_DELIM}
Requires:  phdf5-gnu-impi%{PROJ_DELIM}
Requires:  phdf5-intel-impi%{PROJ_DELIM}
Requires:  phdf5-intel-mpich%{PROJ_DELIM}
Requires:  phdf5-intel-mvapich2%{PROJ_DELIM}
Requires:  phdf5-intel-openmpi%{PROJ_DELIM}
%description io-libs-intel
OpenHPC IO library builds for use with Intel(R) Parallel Studio XE software suite

%package nagios
Summary:   OpenHPC Nagios monitoring
Requires:  nagios%{PROJ_DELIM}
Requires:  nagios-plugins-all%{PROJ_DELIM}
Requires:  nrpe%{PROJ_DELIM}
%description nagios
OpenHPC collection of Nagios monitoring and metrics packages

%package parallel-libs-gnu
Summary:   OpenHPC parallel libraries for GNU
Requires:  boost-gnu-mpich%{PROJ_DELIM}
Requires:  boost-gnu-openmpi%{PROJ_DELIM}
Requires:  fftw-gnu-mpich%{PROJ_DELIM}
Requires:  fftw-gnu-openmpi%{PROJ_DELIM}
Requires:  hypre-gnu-mpich%{PROJ_DELIM}
Requires:  hypre-gnu-openmpi%{PROJ_DELIM}
Requires:  mumps-gnu-mpich%{PROJ_DELIM}
Requires:  mumps-gnu-openmpi%{PROJ_DELIM}
Requires:  petsc-gnu-mpich%{PROJ_DELIM}
Requires:  petsc-gnu-openmpi%{PROJ_DELIM}
Requires:  scalapack-gnu-mpich%{PROJ_DELIM}
Requires:  scalapack-gnu-openmpi%{PROJ_DELIM}
Requires:  superlu_dist-gnu-mpich%{PROJ_DELIM}
Requires:  superlu_dist-gnu-openmpi%{PROJ_DELIM}
Requires:  trilinos-gnu-mpich%{PROJ_DELIM}
Requires:  trilinos-gnu-openmpi%{PROJ_DELIM}
%ifnarch aarch64 && !0%{?centos_version}
Requires:  boost-gnu-mvapich2%{PROJ_DELIM}
Requires:  fftw-gnu-mvapich2%{PROJ_DELIM}
Requires:  hypre-gnu-mvapich2%{PROJ_DELIM}
Requires:  mumps-gnu-mvapich2%{PROJ_DELIM}
Requires:  petsc-gnu-mvapich2%{PROJ_DELIM}
Requires:  scalapack-gnu-mvapich2%{PROJ_DELIM}
Requires:  superlu_dist-gnu-mvapich2%{PROJ_DELIM}
Requires:  trilinos-gnu-mvapich2%{PROJ_DELIM}
%endif
%description parallel-libs-gnu
OpenHPC parallel library builds for use with GNU compiler toolchain

%package parallel-libs-gnu-mpich
Summary:   OpenHPC parallel libraries for GNU and MPICH
Requires:  boost-gnu-mpich%{PROJ_DELIM}
Requires:  fftw-gnu-mpich%{PROJ_DELIM}
Requires:  hypre-gnu-mpich%{PROJ_DELIM}
Requires:  mumps-gnu-mpich%{PROJ_DELIM}
Requires:  petsc-gnu-mpich%{PROJ_DELIM}
Requires:  scalapack-gnu-mpich%{PROJ_DELIM}
Requires:  superlu_dist-gnu-mpich%{PROJ_DELIM}
Requires:  trilinos-gnu-mpich%{PROJ_DELIM}
%description parallel-libs-gnu-mpich
OpenHPC parallel library builds for use with GNU compiler toolchain and the MPICH runtime

%package parallel-libs-gnu-mvapich2
Summary:   OpenHPC parallel libraries for GNU and MVAPICH2
Requires:  boost-gnu-mvapich2%{PROJ_DELIM}
Requires:  fftw-gnu-mvapich2%{PROJ_DELIM}
Requires:  hypre-gnu-mvapich2%{PROJ_DELIM}
Requires:  mumps-gnu-mvapich2%{PROJ_DELIM}
Requires:  petsc-gnu-mvapich2%{PROJ_DELIM}
Requires:  scalapack-gnu-mvapich2%{PROJ_DELIM}
Requires:  superlu_dist-gnu-mvapich2%{PROJ_DELIM}
Requires:  trilinos-gnu-mvapich2%{PROJ_DELIM}
%description parallel-libs-gnu-mvapich2
OpenHPC parallel library builds for use with GNU compiler toolchain and the MVAPICH2 runtime

%package parallel-libs-gnu-openmpi
Summary:   OpenHPC parallel libraries for GNU and OpenMPI
Requires:  boost-gnu-openmpi%{PROJ_DELIM}
Requires:  fftw-gnu-openmpi%{PROJ_DELIM}
Requires:  hypre-gnu-openmpi%{PROJ_DELIM}
Requires:  mumps-gnu-openmpi%{PROJ_DELIM}
Requires:  petsc-gnu-openmpi%{PROJ_DELIM}
Requires:  scalapack-gnu-openmpi%{PROJ_DELIM}
Requires:  superlu_dist-gnu-openmpi%{PROJ_DELIM}
Requires:  trilinos-gnu-openmpi%{PROJ_DELIM}
%description parallel-libs-gnu-openmpi
OpenHPC parallel library builds for use with GNU compiler toolchain and the OpenMPI runtime

%package parallel-libs-intel-impi
Summary:   OpenHPC parallel libraries for Intel(R) Parallel Studio XE and Intel(R) MPI Library
Requires:  boost-gnu-impi%{PROJ_DELIM}
Requires:  boost-intel-impi%{PROJ_DELIM}
Requires:  hypre-gnu-impi%{PROJ_DELIM}
Requires:  hypre-intel-impi%{PROJ_DELIM}
Requires:  mumps-gnu-impi%{PROJ_DELIM}
Requires:  mumps-intel-impi%{PROJ_DELIM}
Requires:  petsc-gnu-impi%{PROJ_DELIM}
Requires:  petsc-intel-impi%{PROJ_DELIM}
Requires:  scalapack-gnu-impi%{PROJ_DELIM}
Requires:  scalapack-intel-impi%{PROJ_DELIM}
Requires:  superlu_dist-gnu-impi%{PROJ_DELIM}
Requires:  superlu_dist-intel-impi%{PROJ_DELIM}
Requires:  trilinos-gnu-impi%{PROJ_DELIM}
Requires:  trilinos-intel-impi%{PROJ_DELIM}
%description parallel-libs-intel-impi
OpenHPC parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the Intel(R) MPI Library

%package parallel-libs-intel-mpich
Summary:   OpenHPC parallel libraries for Intel(R) Parallel Studio XE and MPICH
Requires:  boost-intel-mpich%{PROJ_DELIM}
Requires:  hypre-intel-mpich%{PROJ_DELIM}
Requires:  mumps-intel-mpich%{PROJ_DELIM}
Requires:  petsc-intel-mpich%{PROJ_DELIM}
Requires:  scalapack-intel-mpich%{PROJ_DELIM}
Requires:  superlu_dist-intel-mpich%{PROJ_DELIM}
Requires:  trilinos-intel-mpich%{PROJ_DELIM}
%description parallel-libs-intel-mpich
OpenHPC parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the MPICH runtime

%package parallel-libs-intel-mvapich2
Summary:   OpenHPC parallel libraries for Intel(R) Parallel Studio XE and MVAPICH2
Requires:  boost-intel-mvapich2%{PROJ_DELIM}
Requires:  hypre-intel-mvapich2%{PROJ_DELIM}
Requires:  mumps-intel-mvapich2%{PROJ_DELIM}
Requires:  petsc-intel-mvapich2%{PROJ_DELIM}
Requires:  scalapack-intel-mvapich2%{PROJ_DELIM}
Requires:  superlu_dist-intel-mvapich2%{PROJ_DELIM}
Requires:  trilinos-intel-mvapich2%{PROJ_DELIM}
%description parallel-libs-intel-mvapich2
OpenHPC parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the MVAPICH2 runtime

%package parallel-libs-intel-openmpi
Summary:   OpenHPC parallel libraries for Intel(R) Parallel Studio XE and OpenMPI
Requires:  boost-intel-openmpi%{PROJ_DELIM}
Requires:  hypre-intel-openmpi%{PROJ_DELIM}
Requires:  mumps-intel-openmpi%{PROJ_DELIM}
Requires:  petsc-intel-openmpi%{PROJ_DELIM}
Requires:  scalapack-intel-openmpi%{PROJ_DELIM}
Requires:  superlu_dist-intel-openmpi%{PROJ_DELIM}
Requires:  trilinos-intel-openmpi%{PROJ_DELIM}
%description parallel-libs-intel-openmpi
OpenHPC parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the OpenMPI runtime

%package perf-tools-gnu
Summary:   OpenHPC performance tools for GNU
Requires:  imb-gnu-mpich%{PROJ_DELIM}
Requires:  imb-gnu-openmpi%{PROJ_DELIM}
Requires:  mpiP-gnu-mpich%{PROJ_DELIM}
Requires:  mpiP-gnu-openmpi%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
Requires:  tau-gnu-mpich%{PROJ_DELIM}
Requires:  tau-gnu-openmpi%{PROJ_DELIM}
Requires:  scalasca-gnu-mpich%{PROJ_DELIM}
Requires:  scalasca-gnu-openmpi%{PROJ_DELIM}
%ifnarch aarch64 && !0%{?centos_version}
Requires:  imb-gnu-mvapich2%{PROJ_DELIM}
Requires:  mpiP-gnu-mvapich2%{PROJ_DELIM}
Requires:  tau-gnu-mvapich2%{PROJ_DELIM}
Requires:  scalasca-gnu-mvapich2%{PROJ_DELIM}
%endif
%description perf-tools-gnu
OpenHPC performance tool builds for use with GNU compiler toolchain

%package perf-tools-intel
Summary:   OpenHPC performance tools for Intel(R) Parallel Studio XE
Requires:  imb-gnu-impi%{PROJ_DELIM}
Requires:  imb-intel-impi%{PROJ_DELIM}
Requires:  imb-intel-mpich%{PROJ_DELIM}
Requires:  imb-intel-mvapich2%{PROJ_DELIM}
Requires:  imb-intel-openmpi%{PROJ_DELIM}
Requires:  mpiP-gnu-impi%{PROJ_DELIM}
Requires:  mpiP-intel-impi%{PROJ_DELIM}
Requires:  mpiP-intel-mpich%{PROJ_DELIM}
Requires:  mpiP-intel-mvapich2%{PROJ_DELIM}
Requires:  mpiP-intel-openmpi%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
Requires:  tau-gnu-impi%{PROJ_DELIM}
Requires:  tau-intel-impi%{PROJ_DELIM}
Requires:  tau-intel-mpich%{PROJ_DELIM}
Requires:  tau-intel-mvapich2%{PROJ_DELIM}
Requires:  tau-intel-openmpi%{PROJ_DELIM}
Requires:  scalasca-gnu-impi%{PROJ_DELIM}
Requires:  scalasca-intel-impi%{PROJ_DELIM}
Requires:  scalasca-intel-mpich%{PROJ_DELIM}
Requires:  scalasca-intel-mvapich2%{PROJ_DELIM}
Requires:  scalasca-intel-openmpi%{PROJ_DELIM}
%description perf-tools-intel
OpenHPC performance tool builds for use with Intel(R) Parallel Studio XE toolchain

%package python-libs-gnu
Summary:   OpenHPC python libraries for GNU
Requires:  python-numpy-gnu%{PROJ_DELIM}
Requires:  python-scipy-gnu-mpich%{PROJ_DELIM}
Requires:  python-scipy-gnu-openmpi%{PROJ_DELIM}
%ifnarch aarch64 && !0%{?centos_version}
Requires:  python-scipy-gnu-mvapich2%{PROJ_DELIM}
%endif
%description python-libs-gnu
OpenHPC python related library builds for use with GNU compiler toolchain

%package python-libs-intel
Summary:   OpenHPC python libraries for Intel(R) Parallel Studio XE
Requires:  python-numpy-intel%{PROJ_DELIM}
%description python-libs-intel
OpenHPC python related library builds for use with Intel(R) Parallel Studio XE toolchain

%package runtimes-gnu
Summary:   OpenHPC runtimes for GNU
Requires:  ocr-gnu%{PROJ_DELIM}
Requires:  singularity%{PROJ_DELIM}
%description runtimes-gnu
OpenHPC runtimes for use with GNU compiler toolchain

%package runtimes-intel
Summary:   OpenHPC runtimes for Intel(R) Parallel Studio XE toolchain
Requires:  ocr-gnu%{PROJ_DELIM}
Requires:  singularity%{PROJ_DELIM}
%description runtimes-intel
OpenHPC runtimes for use with Intel(R) Parallel Studio XE toolchain

%package serial-libs-gnu
Summary:   OpenHPC serial libraries for GNU
Requires:  gsl-gnu%{PROJ_DELIM}
Requires:  metis-gnu%{PROJ_DELIM}
Requires:  openblas-gnu%{PROJ_DELIM}
Requires:  superlu-gnu%{PROJ_DELIM}
%description serial-libs-gnu
OpenHPC serial library builds for use with GNU compiler toolchain

%package serial-libs-intel
Summary:   OpenHPC serial libraries for Intel(R) Parallel Studio XE
Requires:  metis-intel%{PROJ_DELIM}
Requires:  superlu-intel%{PROJ_DELIM}
%description serial-libs-intel
OpenHPC serial library builds for use with Intel(R) Parallel Studio XE toolchain

%package slurm-client
Summary:   OpenHPC client packages for SLURM
Requires:  slurm%{PROJ_DELIM}
Requires:  slurm-munge%{PROJ_DELIM}
Requires:  slurm-plugins%{PROJ_DELIM}
Requires:  slurm-sjobexit%{PROJ_DELIM}
Requires:  slurm-pam_slurm%{PROJ_DELIM}
Requires:  munge%{PROJ_DELIM}
%description slurm-client
OpenHPC client packages for SLURM

%package slurm-server
Summary:   OpenHPC server packages for SLURM
Requires:  slurm%{PROJ_DELIM}
Requires:  slurm-munge%{PROJ_DELIM}
Requires:  slurm-plugins%{PROJ_DELIM}
Requires:  slurm-perlapi%{PROJ_DELIM}
Requires:  slurm-devel%{PROJ_DELIM}
Requires:  slurm-slurmdbd%{PROJ_DELIM}
Requires:  slurm-sql%{PROJ_DELIM}
Requires:  slurm-slurmdb-direct%{PROJ_DELIM}
Requires:  munge%{PROJ_DELIM}
Requires:  munge-libs%{PROJ_DELIM}
Requires:  munge-devel%{PROJ_DELIM}
%description slurm-server
OpenHPC server packages for SLURM

%package warewulf
Summary:   OpenHPC base packages for Warewulf
Requires:  warewulf-cluster%{PROJ_DELIM}
Requires:  warewulf-common%{PROJ_DELIM}
Requires:  warewulf-provision%{PROJ_DELIM}
Requires:  warewulf-provision-server%{PROJ_DELIM}
Requires:  warewulf-vnfs%{PROJ_DELIM}
%description warewulf
OpenHPC collection of base packages for Warewulf provisioning


%prep
%{__cp} %SOURCE0 .

%build

%install

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%clean
rm -rf $RPM_BUILD_ROOT

%files

%files autotools
%files base
%files base-compute
%files ganglia
%files io-libs-gnu
%files io-libs-intel
%files nagios
%files parallel-libs-gnu
%files parallel-libs-gnu-mpich
%files parallel-libs-gnu-mvapich2
%files parallel-libs-gnu-openmpi
%files parallel-libs-intel-impi
%files parallel-libs-intel-mpich
%files parallel-libs-intel-mvapich2
%files parallel-libs-intel-openmpi
%files perf-tools-gnu
%files perf-tools-intel
%files python-libs-gnu
%files python-libs-intel
%files runtimes-gnu
%files runtimes-intel
%files serial-libs-gnu
%files serial-libs-intel
%files slurm-client
%files slurm-server
%files warewulf

%changelog

