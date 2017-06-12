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
Version: 1.4
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
Collection of GNU autotools packages

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
Requires:  yum-utils
%endif
%if 0%{?sles_version}
Requires:  glibc-locale
Requires:  libmlx4-rdmav2
Requires:  nfs-kernel-server
%endif
%description base
Collection of base packages

%package base-compute
Summary:   OpenHPC compute node base
Requires:  libicu
Requires:  libunwind
Requires:  numactl
%description base-compute
Collection of compute node base packages

%package ganglia
Summary:   OpenHPC Ganglia monitoring
Requires:  ganglia%{PROJ_DELIM}
Requires:  ganglia-devel%{PROJ_DELIM}
Requires:  ganglia-gmetad%{PROJ_DELIM}
Requires:  ganglia-gmond%{PROJ_DELIM}
Requires:  ganglia-gmond-python%{PROJ_DELIM}
Requires:  ganglia-web%{PROJ_DELIM}
%description ganglia
Collection of Ganglia monitoring and metrics packages

%package io-libs-gnu7
Summary:   OpenHPC IO libraries for GNU
Requires:  adios-gnu7-mpich%{PROJ_DELIM}
Requires:  adios-gnu7-openmpi%{PROJ_DELIM}
Requires:  hdf5-gnu7%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu7-mpich%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu7-openmpi%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu7-mpich%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu7-openmpi%{PROJ_DELIM}
Requires:  netcdf-gnu7-mpich%{PROJ_DELIM}
Requires:  netcdf-gnu7-openmpi%{PROJ_DELIM}
Requires:  phdf5-gnu7-mpich%{PROJ_DELIM}
Requires:  phdf5-gnu7-openmpi%{PROJ_DELIM}
%ifnarch aarch64 && !0%{?centos_version}
Requires:  adios-gnu7-mvapich2%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu7-mvapich2%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu7-mvapich2%{PROJ_DELIM}
Requires:  netcdf-gnu7-mvapich2%{PROJ_DELIM}
Requires:  phdf5-gnu7-mvapich2%{PROJ_DELIM}
%endif
%description io-libs-gnu7
Collection of IO library builds for use with GNU compiler toolchain

%package io-libs-intel
Summary:   OpenHPC IO libraries for Intel(R) Parallel Studio XE
Requires:  adios-gnu7-impi%{PROJ_DELIM}
Requires:  adios-intel-impi%{PROJ_DELIM}
Requires:  adios-intel-mpich%{PROJ_DELIM}
Requires:  adios-intel-mvapich2%{PROJ_DELIM}
Requires:  adios-intel-openmpi%{PROJ_DELIM}
Requires:  hdf5-intel%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu7-impi%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-impi%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-mpich%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-mvapich2%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-openmpi%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu7-impi%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-impi%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-mpich%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-mvapich2%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-openmpi%{PROJ_DELIM}
Requires:  netcdf-gnu7-impi%{PROJ_DELIM}
Requires:  netcdf-intel-impi%{PROJ_DELIM}
Requires:  netcdf-intel-mpich%{PROJ_DELIM}
Requires:  netcdf-intel-mvapich2%{PROJ_DELIM}
Requires:  netcdf-intel-openmpi%{PROJ_DELIM}
Requires:  phdf5-gnu7-impi%{PROJ_DELIM}
Requires:  phdf5-intel-impi%{PROJ_DELIM}
Requires:  phdf5-intel-mpich%{PROJ_DELIM}
Requires:  phdf5-intel-mvapich2%{PROJ_DELIM}
Requires:  phdf5-intel-openmpi%{PROJ_DELIM}
%description io-libs-intel
Collection of IO library builds for use with Intel(R) Parallel Studio XE software suite

%package nagios
Summary:   OpenHPC Nagios monitoring
Requires:  nagios%{PROJ_DELIM}
Requires:  nagios-plugins-all%{PROJ_DELIM}
Requires:  nrpe%{PROJ_DELIM}
%description nagios
Collection of Nagios monitoring and metrics packages

%package parallel-libs-gnu7
Summary:   OpenHPC parallel libraries for GNU
Requires:  boost-gnu7-mpich%{PROJ_DELIM}
Requires:  boost-gnu7-openmpi%{PROJ_DELIM}
Requires:  fftw-gnu7-mpich%{PROJ_DELIM}
Requires:  fftw-gnu7-openmpi%{PROJ_DELIM}
Requires:  hypre-gnu7-mpich%{PROJ_DELIM}
Requires:  hypre-gnu7-openmpi%{PROJ_DELIM}
Requires:  mumps-gnu7-mpich%{PROJ_DELIM}
Requires:  mumps-gnu7-openmpi%{PROJ_DELIM}
Requires:  petsc-gnu7-mpich%{PROJ_DELIM}
Requires:  petsc-gnu7-openmpi%{PROJ_DELIM}
Requires:  scalapack-gnu7-mpich%{PROJ_DELIM}
Requires:  scalapack-gnu7-openmpi%{PROJ_DELIM}
Requires:  superlu_dist-gnu7-mpich%{PROJ_DELIM}
Requires:  superlu_dist-gnu7-openmpi%{PROJ_DELIM}
Requires:  trilinos-gnu7-mpich%{PROJ_DELIM}
Requires:  trilinos-gnu7-openmpi%{PROJ_DELIM}
%ifnarch aarch64 && !0%{?centos_version}
Requires:  boost-gnu7-mvapich2%{PROJ_DELIM}
Requires:  fftw-gnu7-mvapich2%{PROJ_DELIM}
Requires:  hypre-gnu7-mvapich2%{PROJ_DELIM}
Requires:  mumps-gnu7-mvapich2%{PROJ_DELIM}
Requires:  petsc-gnu7-mvapich2%{PROJ_DELIM}
Requires:  scalapack-gnu7-mvapich2%{PROJ_DELIM}
Requires:  superlu_dist-gnu7-mvapich2%{PROJ_DELIM}
Requires:  trilinos-gnu7-mvapich2%{PROJ_DELIM}
%endif
%description parallel-libs-gnu7
Collection of parallel library builds for use with GNU compiler toolchain

%package parallel-libs-gnu7-mpich
Summary:   OpenHPC parallel libraries for GNU and MPICH
Requires:  boost-gnu7-mpich%{PROJ_DELIM}
Requires:  fftw-gnu7-mpich%{PROJ_DELIM}
Requires:  hypre-gnu7-mpich%{PROJ_DELIM}
Requires:  mumps-gnu7-mpich%{PROJ_DELIM}
Requires:  petsc-gnu7-mpich%{PROJ_DELIM}
Requires:  scalapack-gnu7-mpich%{PROJ_DELIM}
Requires:  superlu_dist-gnu7-mpich%{PROJ_DELIM}
Requires:  trilinos-gnu7-mpich%{PROJ_DELIM}
%description parallel-libs-gnu7-mpich
Collection of parallel library builds for use with GNU compiler toolchain and the MPICH runtime

%package parallel-libs-gnu7-mvapich2
Summary:   OpenHPC parallel libraries for GNU and MVAPICH2
Requires:  boost-gnu7-mvapich2%{PROJ_DELIM}
Requires:  fftw-gnu7-mvapich2%{PROJ_DELIM}
Requires:  hypre-gnu7-mvapich2%{PROJ_DELIM}
Requires:  mumps-gnu7-mvapich2%{PROJ_DELIM}
Requires:  petsc-gnu7-mvapich2%{PROJ_DELIM}
Requires:  scalapack-gnu7-mvapich2%{PROJ_DELIM}
Requires:  superlu_dist-gnu7-mvapich2%{PROJ_DELIM}
Requires:  trilinos-gnu7-mvapich2%{PROJ_DELIM}
%description parallel-libs-gnu7-mvapich2
Collection of parallel library builds for use with GNU compiler toolchain and the MVAPICH2 runtime

%package parallel-libs-gnu7-openmpi
Summary:   OpenHPC parallel libraries for GNU and OpenMPI
Requires:  boost-gnu7-openmpi%{PROJ_DELIM}
Requires:  fftw-gnu7-openmpi%{PROJ_DELIM}
Requires:  hypre-gnu7-openmpi%{PROJ_DELIM}
Requires:  mumps-gnu7-openmpi%{PROJ_DELIM}
Requires:  petsc-gnu7-openmpi%{PROJ_DELIM}
Requires:  scalapack-gnu7-openmpi%{PROJ_DELIM}
Requires:  superlu_dist-gnu7-openmpi%{PROJ_DELIM}
Requires:  trilinos-gnu7-openmpi%{PROJ_DELIM}
%description parallel-libs-gnu7-openmpi
Collection of parallel library builds for use with GNU compiler toolchain and the OpenMPI runtime

%package parallel-libs-intel-impi
Summary:   OpenHPC parallel libraries for Intel(R) Parallel Studio XE and Intel(R) MPI Library
Requires:  boost-gnu7-impi%{PROJ_DELIM}
Requires:  boost-intel-impi%{PROJ_DELIM}
Requires:  hypre-gnu7-impi%{PROJ_DELIM}
Requires:  hypre-intel-impi%{PROJ_DELIM}
Requires:  mumps-gnu7-impi%{PROJ_DELIM}
Requires:  mumps-intel-impi%{PROJ_DELIM}
Requires:  petsc-gnu7-impi%{PROJ_DELIM}
Requires:  petsc-intel-impi%{PROJ_DELIM}
Requires:  scalapack-gnu7-impi%{PROJ_DELIM}
Requires:  scalapack-intel-impi%{PROJ_DELIM}
Requires:  superlu_dist-gnu7-impi%{PROJ_DELIM}
Requires:  superlu_dist-intel-impi%{PROJ_DELIM}
Requires:  trilinos-gnu7-impi%{PROJ_DELIM}
Requires:  trilinos-intel-impi%{PROJ_DELIM}
%description parallel-libs-intel-impi
Collection of parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the Intel(R) MPI Library

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
Collection of parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the MPICH runtime

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
Collection of parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the MVAPICH2 runtime

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
Collection of parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the OpenMPI runtime

%package perf-tools-gnu7
Summary:   OpenHPC performance tools for GNU
Requires:  imb-gnu7-mpich%{PROJ_DELIM}
Requires:  imb-gnu7-openmpi%{PROJ_DELIM}
Requires:  mpiP-gnu7-mpich%{PROJ_DELIM}
Requires:  mpiP-gnu7-openmpi%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
Requires:  tau-gnu7-mpich%{PROJ_DELIM}
Requires:  tau-gnu7-openmpi%{PROJ_DELIM}
Requires:  scalasca-gnu7-mpich%{PROJ_DELIM}
Requires:  scalasca-gnu7-openmpi%{PROJ_DELIM}
%ifnarch aarch64 && !0%{?centos_version}
Requires:  imb-gnu7-mvapich2%{PROJ_DELIM}
Requires:  mpiP-gnu7-mvapich2%{PROJ_DELIM}
Requires:  tau-gnu7-mvapich2%{PROJ_DELIM}
Requires:  scalasca-gnu7-mvapich2%{PROJ_DELIM}
%endif
%description perf-tools-gnu7
Collection of performance tool builds for use with GNU compiler toolchain

%package perf-tools-intel
Summary:   OpenHPC performance tools for Intel(R) Parallel Studio XE
Requires:  imb-gnu7-impi%{PROJ_DELIM}
Requires:  imb-intel-impi%{PROJ_DELIM}
Requires:  imb-intel-mpich%{PROJ_DELIM}
Requires:  imb-intel-mvapich2%{PROJ_DELIM}
Requires:  imb-intel-openmpi%{PROJ_DELIM}
Requires:  mpiP-gnu7-impi%{PROJ_DELIM}
Requires:  mpiP-intel-impi%{PROJ_DELIM}
Requires:  mpiP-intel-mpich%{PROJ_DELIM}
Requires:  mpiP-intel-mvapich2%{PROJ_DELIM}
Requires:  mpiP-intel-openmpi%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
Requires:  tau-gnu7-impi%{PROJ_DELIM}
Requires:  tau-intel-impi%{PROJ_DELIM}
Requires:  tau-intel-mpich%{PROJ_DELIM}
Requires:  tau-intel-mvapich2%{PROJ_DELIM}
Requires:  tau-intel-openmpi%{PROJ_DELIM}
Requires:  scalasca-gnu7-impi%{PROJ_DELIM}
Requires:  scalasca-intel-impi%{PROJ_DELIM}
Requires:  scalasca-intel-mpich%{PROJ_DELIM}
Requires:  scalasca-intel-mvapich2%{PROJ_DELIM}
Requires:  scalasca-intel-openmpi%{PROJ_DELIM}
%description perf-tools-intel
Collection of performance tool builds for use with Intel(R) Parallel Studio XE toolchain

%package python-libs-gnu7
Summary:   OpenHPC python libraries for GNU
Requires:  python-numpy-gnu7%{PROJ_DELIM}
Requires:  python-scipy-gnu7-mpich%{PROJ_DELIM}
Requires:  python-scipy-gnu7-openmpi%{PROJ_DELIM}
%ifnarch aarch64 && !0%{?centos_version}
Requires:  python-scipy-gnu7-mvapich2%{PROJ_DELIM}
%endif
%description python-libs-gnu7
Collection of python related library builds for use with GNU compiler toolchain

%package python-libs-intel
Summary:   OpenHPC python libraries for Intel(R) Parallel Studio XE
Requires:  python-numpy-intel%{PROJ_DELIM}
%description python-libs-intel
Collection of python related library builds for use with Intel(R) Parallel Studio XE toolchain

%package runtimes-gnu7
Summary:   OpenHPC runtimes for GNU
Requires:  ocr-gnu7%{PROJ_DELIM}
Requires:  singularity%{PROJ_DELIM}
%description runtimes-gnu7
Collection of runtimes for use with GNU compiler toolchain

%package runtimes-intel
Summary:   OpenHPC runtimes for Intel(R) Parallel Studio XE toolchain
Requires:  ocr-intel%{PROJ_DELIM}
Requires:  singularity%{PROJ_DELIM}
%description runtimes-intel
Collection of runtimes for use with Intel(R) Parallel Studio XE toolchain

%package serial-libs-t gnu7
Summary:   OpenHPC serial libraries for GNU
Requires:  gsl-gnu7%{PROJ_DELIM}
Requires:  metis-gnu7%{PROJ_DELIM}
Requires:  openblas-gnu7%{PROJ_DELIM}
Requires:  superlu-gnu7%{PROJ_DELIM}
%description serial-libs-gnu7
Collection of serial library builds for use with GNU compiler toolchain

%package serial-libs-intel
Summary:   OpenHPC serial libraries for Intel(R) Parallel Studio XE
Requires:  metis-intel%{PROJ_DELIM}
Requires:  superlu-intel%{PROJ_DELIM}
%description serial-libs-intel
Collection of serial library builds for use with Intel(R) Parallel Studio XE toolchain

%package slurm-client
Summary:   OpenHPC client packages for SLURM
Requires:  slurm%{PROJ_DELIM}
Requires:  slurm-munge%{PROJ_DELIM}
Requires:  slurm-plugins%{PROJ_DELIM}
Requires:  slurm-sjobexit%{PROJ_DELIM}
Requires:  slurm-pam_slurm%{PROJ_DELIM}
Requires:  munge%{PROJ_DELIM}
%description slurm-client
Collection of client packages for SLURM

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
Requires:  pdsh-mod-slurm%{PROJ_DELIM}
%description slurm-server
Collection of server packages for SLURM

%package warewulf
Summary:   OpenHPC base packages for Warewulf
Requires:  warewulf-cluster%{PROJ_DELIM}
Requires:  warewulf-common%{PROJ_DELIM}
Requires:  warewulf-provision%{PROJ_DELIM}
Requires:  warewulf-provision-server%{PROJ_DELIM}
Requires:  warewulf-vnfs%{PROJ_DELIM}
%description warewulf
Collection of base packages for Warewulf provisioning


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
%files io-libs-gnu7
%files io-libs-intel
%files nagios
%files parallel-libs-gnu7
%files parallel-libs-gnu7-mpich
%files parallel-libs-gnu7-mvapich2
%files parallel-libs-gnu7-openmpi
%files parallel-libs-intel-impi
%files parallel-libs-intel-mpich
%files parallel-libs-intel-mvapich2
%files parallel-libs-intel-openmpi
%files perf-tools-gnu7
%files perf-tools-intel
%files python-libs-gnu7
%files python-libs-intel
%files runtimes-gnu7
%files runtimes-intel
%files serial-libs-gnu7
%files serial-libs-intel
%files slurm-client
%files slurm-server
%files warewulf

%changelog

