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
Name:    meta-packages%{PROJ_DELIM}
Version: 1.3.1
Release: 1
License: Apache-2.0
Group:   %{PROJ_NAME}/admin
URL:     https://github.com/openhpc/ohpc
Source0: LICENSE


BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
DocDir:    %{OHPC_PUB}/doc/contrib

%description

This is an internal package that is used to create groups as part
of the installation source setup.  Installation of this package does
not make sense.

%package ohpc-autotools
Summary:   OpenHPC collection of GNU autotools packages
Requires:  autoconf-ohpc
Requires:  automake-ohpc
Requires:  libtool-ohpc
%description ohpc-autotools
OpenHPC collection of GNU autotools packages

%package ohpc-base
Summary:   OpenHPC base packages
Requires:  bc
Requires:  conman-ohpc
Requires:  emacs-nox
Requires:  examples-ohpc
Requires:  gdb
Requires:  glibc-locale
Requires:  ipmitool
Requires:  libstdc++-devel
Requires:  libxml4-rdmav2
Requires:  lmod-ohpc
Requires:  losf-ohpc
Requires:  make
Requires:  man
Requires:  net-tools
Requires:  nfs-kernel-server
Requires:  nfs-utils
Requires:  ntp
Requires:  OpenIPMI
Requires:  pdsh-ohpc
Requires:  screen
Requires:  sudo
%if 0%{?centos_version} || 0%{?rhel_version}
Requires:  binutils
Requires:  binutils-devel
Requires:  man-db
%endif
%description ohpc-base
OpenHPC base packages

%package ohpc-base-compute
Summary:   OpenHPC compute node base packages
Requires:  libicu
Requires:  libunwind
Requires:  numactl
%description ohpc-base-compute
OpenHPC compute node base packages

%package ohpc-ganglia
Summary:   OpenHPC collection of Ganglia monitoring and metrics packages
Requires:  ganglia-ohpc
Requires:  ganglia-gmetad-ohpc
Requires:  ganglia-gmond-ohpc
Requires:  ganglia-gmond-python-ohpc
Requires:  ganglia-web-ohpc
%description ohpc-ganglia
OpenHPC collection of Ganglia monitoring and metrics packages

%package ohpc-io-libs-gnu
Summary:   OpenHPC IO library builds for use with GNU compiler toolchain
Requires:  adios-gnu-mpich-ohpc
Requires:  adios-gnu-openmpi-ohpc
Requires:  hdf5-gnu-ohpc
Requires:  netcdf-cxx-gnu-mpich-ohpc
Requires:  netcdf-cxx-gnu-openmpi-ohpc
Requires:  netcdf-fortran-gnu-mpich-ohpc
Requires:  netcdf-fortran-gnu-openmpi-ohpc
Requires:  netcdf-gnu-mpich-ohpc
Requires:  netcdf-gnu-openmpi-ohpc
Requires:  phdf5-gnu-mpich-ohpc
Requires:  phdf5-gnu-openmpi-ohpc
%ifnarch aarch64 && !0%{?centos_version}
Requires:  adios-gnu-mvapich2-ohpc
Requires:  netcdf-cxx-gnu-mvapich2-ohpc
Requires:  netcdf-fortran-gnu-mvapich2-ohpc
Requires:  netcdf-gnu-mvapich2-ohpc
Requires:  phdf5-gnu-mvapich2-ohpc
%endif
%description ohpc-io-libs-gnu
OpenHPC IO library builds for use with GNU compiler toolchain

%package ohpc-io-libs-intel
Summary:   OpenHPC IO library builds for use with Intel(R) Parallel Studio XE software suite
Requires:  adios-gnu-impi-ohpc
Requires:  adios-intel-impi-ohpc
Requires:  adios-intel-mpich-ohpc
Requires:  adios-intel-mvapich2-ohpc
Requires:  adios-intel-openmpi-ohpc
Requires:  hdf5-intel-ohpc
Requires:  netcdf-cxx-gnu-impi-ohpc
Requires:  netcdf-cxx-intel-impi-ohpc
Requires:  netcdf-cxx-intel-mpich-ohpc
Requires:  netcdf-cxx-intel-mvapich2-ohpc
Requires:  netcdf-cxx-intel-openmpi-ohpc
Requires:  netcdf-fortran-gnu-impi-ohpc
Requires:  netcdf-fortran-intel-impi-ohpc
Requires:  netcdf-fortran-intel-mpich-ohpc
Requires:  netcdf-fortran-intel-mvapich2-ohpc
Requires:  netcdf-fortran-intel-openmpi-ohpc
Requires:  netcdf-gnu-impi-ohpc
Requires:  netcdf-intel-impi-ohpc
Requires:  netcdf-intel-mpich-ohpc
Requires:  netcdf-intel-mvapich2-ohpc
Requires:  netcdf-intel-openmpi-ohpc
Requires:  phdf5-gnu-impi-ohpc
Requires:  phdf5-intel-impi-ohpc
Requires:  phdf5-intel-mpich-ohpc
Requires:  phdf5-intel-mvapich2-ohpc
Requires:  phdf5-intel-openmpi-ohpc
%description ohpc-io-libs-intel
OpenHPC IO library builds for use with Intel(R) Parallel Studio XE software suite

%package ohpc-nagios
Summary:   OpenHPC collection of Nagios monitoring and metrics packages
Requires:  nagios-ohpc
Requires:  nagios-plugins-allohpc
Requires:  nrpe-ohpc
%description ohpc-nagios
OpenHPC collection of Nagios monitoring and metrics packages

%package ohpc-parallel-libs-gnu
Summary:   OpenHPC parallel library builds for use with GNU compiler toolchain
Requires:  boost-gnu-mpich-ohpc
Requires:  boost-gnu-openmpi-ohpc
Requires:  fftw-gnu-mpich-ohpc
Requires:  fftw-gnu-openmpi-ohpc
Requires:  hypre-gnu-mpich-ohpc
Requires:  hypre-gnu-openmpi-ohpc
Requires:  mumps-gnu-mpich-ohpc
Requires:  mumps-gnu-openmpi-ohpc
Requires:  petsc-gnu-mpich-ohpc
Requires:  petsc-gnu-openmpi-ohpc
Requires:  scalapack-gnu-mpich-ohpc
Requires:  scalapack-gnu-openmpi-ohpc
Requires:  superlu_dist-gnu-mpich-ohpc
Requires:  superlu_dist-gnu-openmpi-ohpc
Requires:  trilinos-gnu-mpich-ohpc
Requires:  trilinos-gnu-openmpi-ohpc
%ifnarch aarch64 && !0%{?centos_version}
Requires:  boost-gnu-mvapich2-ohpc
Requires:  fftw-gnu-mvapich2-ohpc
Requires:  hypre-gnu-mvapich2-ohpc
Requires:  mumps-gnu-mvapich2-ohpc
Requires:  petsc-gnu-mvapich2-ohpc
Requires:  scalapack-gnu-mvapich2-ohpc
Requires:  superlu_dist-gnu-mvapich2-ohpc
Requires:  trilinos-gnu-mvapich2-ohpc
%endif
%description ohpc-parallel-libs-gnu
OpenHPC parallel library builds for use with GNU compiler toolchain

%package ohpc-parallel-libs-gnu-mpich
Summary:   OpenHPC parallel library builds for use with GNU compiler toolchain and the MPICH runtime
Requires:  boost-gnu-mpich-ohpc
Requires:  fftw-gnu-mpich-ohpc
Requires:  hypre-gnu-mpich-ohpc
Requires:  mumps-gnu-mpich-ohpc
Requires:  petsc-gnu-mpich-ohpc
Requires:  scalapack-gnu-mpich-ohpc
Requires:  superlu_dist-gnu-mpich-ohpc
Requires:  trilinos-gnu-mpich-ohpc
%description ohpc-parallel-libs-gnu-mpich
OpenHPC parallel library builds for use with GNU compiler toolchain and the MPICH runtime

%package ohpc-parallel-libs-gnu-mvapich2
Summary:   OpenHPC parallel library builds for use with GNU compiler toolchain and the MVAPICH2 runtime
Requires:  boost-gnu-mvapich2-ohpc
Requires:  fftw-gnu-mvapich2-ohpc
Requires:  hypre-gnu-mvapich2-ohpc
Requires:  mumps-gnu-mvapich2-ohpc
Requires:  petsc-gnu-mvapich2-ohpc
Requires:  scalapack-gnu-mvapich2-ohpc
Requires:  superlu_dist-gnu-mvapich2-ohpc
Requires:  trilinos-gnu-mvapich2-ohpc
%description ohpc-parallel-libs-gnu-mvapich2
OpenHPC parallel library builds for use with GNU compiler toolchain and the MVAPICH2 runtime

%package ohpc-parallel-libs-gnu-openmpi
Summary:   OpenHPC parallel library builds for use with GNU compiler toolchain and the OpenMPI runtime
Requires:  boost-gnu-openmpi-ohpc
Requires:  fftw-gnu-openmpi-ohpc
Requires:  hypre-gnu-openmpi-ohpc
Requires:  mumps-gnu-openmpi-ohpc
Requires:  petsc-gnu-openmpi-ohpc
Requires:  scalapack-gnu-openmpi-ohpc
Requires:  superlu_dist-gnu-openmpi-ohpc
Requires:  trilinos-gnu-openmpi-ohpc
%description ohpc-parallel-libs-gnu-openmpi
OpenHPC parallel library builds for use with GNU compiler toolchain and the OpenMPI runtime

%package ohpc-parallel-libs-intel-impi
Summary:   OpenHPC parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the Intel(R) MPI Library
Requires:  boost-gnu-impi-ohpc
Requires:  boost-intel-impi-ohpc
Requires:  hypre-gnu-impi-ohpc
Requires:  hypre-intel-impi-ohpc
Requires:  mumps-gnu-impi-ohpc
Requires:  mumps-intel-impi-ohpc
Requires:  petsc-gnu-impi-ohpc
Requires:  petsc-intel-impi-ohpc
Requires:  scalapack-gnu-impi-ohpc
Requires:  scalapack-intel-impi-ohpc
Requires:  superlu_dist-gnu-impi-ohpc
Requires:  superlu_dist-intel-impi-ohpc
Requires:  trilinos-gnu-impi-ohpc
Requires:  trilinos-intel-impi-ohpc
%description ohpc-parallel-libs-intel-impi
OpenHPC parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the Intel(R) MPI Library

%package ohpc-parallel-libs-intel-mpich
Summary:   OpenHPC parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the MPICH runtime
Requires:  boost-intel-mpich-ohpc
Requires:  hypre-intel-mpich-ohpc
Requires:  mumps-intel-mpich-ohpc
Requires:  petsc-intel-mpich-ohpc
Requires:  scalapack-intel-mpich-ohpc
Requires:  superlu_dist-intel-mpich-ohpc
Requires:  trilinos-intel-mpich-ohpc
%description ohpc-parallel-libs-intel-mpich
OpenHPC parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the MPICH runtime

%package ohpc-parallel-libs-intel-mvapich2
Summary:   OpenHPC parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the MVAPICH2 runtime
Requires:  boost-intel-mvapich2-ohpc
Requires:  hypre-intel-mvapich2-ohpc
Requires:  mumps-intel-mvapich2-ohpc
Requires:  petsc-intel-mvapich2-ohpc
Requires:  scalapack-intel-mvapich2-ohpc
Requires:  superlu_dist-intel-mvapich2-ohpc
Requires:  trilinos-intel-mvapich2-ohpc
%description ohpc-parallel-libs-intel-mvapich2
OpenHPC parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the MVAPICH2 runtime

%package ohpc-parallel-libs-intel-openmpi
Summary:   OpenHPC parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the OpenMPI runtime
Requires:  boost-intel-openmpi-ohpc
Requires:  hypre-intel-openmpi-ohpc
Requires:  mumps-intel-openmpi-ohpc
Requires:  petsc-intel-openmpi-ohpc
Requires:  scalapack-intel-openmpi-ohpc
Requires:  superlu_dist-intel-openmpi-ohpc
Requires:  trilinos-intel-openmpi-ohpc
%description ohpc-parallel-libs-intel-openmpi
OpenHPC parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the OpenMPI runtime

%package ohpc-perf-tools-gnu
Summary:   OpenHPC performance tool builds for use with GNU compiler toolchain
Requires:  imb-gnu-mpich-ohpc
Requires:  imb-gnu-openmpi-ohpc
Requires:  mpiP-gnu-mpich-ohpc
Requires:  mpiP-gnu-openmpi-ohpc
Requires:  papi-ohpc
Requires:  tau-gnu-mpich-ohpc
Requires:  tau-gnu-openmpi-ohpc
Requires:  scalasca-gnu-mpich-ohpc
Requires:  scalasca-gnu-openmpi-ohpc
%ifnarch aarch64 && !0%{?centos_version}
Requires:  imb-gnu-mvapich2-ohpc
Requires:  mpiP-gnu-mvapich2-ohpc
Requires:  tau-gnu-mvapich2-ohpc
Requires:  scalasca-gnu-mvapich2-ohpc
%endif
%description ohpc-perf-tools-gnu
OpenHPC performance tool builds for use with GNU compiler toolchain

%package ohpc-perf-tools-intel
Summary:   OpenHPC performance tool builds for use with Intel(R) Parallel Studio XE toolchain
Requires:  imb-gnu-impi-ohpc
Requires:  imb-intel-impi-ohpc
Requires:  imb-intel-mpich-ohpc
Requires:  imb-intel-mvapich2-ohpc
Requires:  imb-intel-openmpi-ohpc
Requires:  mpiP-gnu-impi-ohpc
Requires:  mpiP-intel-impi-ohpc
Requires:  mpiP-intel-mpich-ohpc
Requires:  mpiP-intel-mvapich2-ohpc
Requires:  mpiP-intel-openmpi-ohpc
Requires:  papi-ohpc
Requires:  tau-gnu-impi-ohpc
Requires:  tau-intel-impi-ohpc
Requires:  tau-intel-mpich-ohpc
Requires:  tau-intel-mvapich2-ohpc
Requires:  tau-intel-openmpi-ohpc
Requires:  scalasca-gnu-impi-ohpc
Requires:  scalasca-intel-impi-ohpc
Requires:  scalasca-intel-mpich-ohpc
Requires:  scalasca-intel-mvapich2-ohpc
Requires:  scalasca-intel-openmpi-ohpc
%description ohpc-perf-tools-intel
OpenHPC performance tool builds for use with Intel(R) Parallel Studio XE toolchain

%package ohpc-python-libs-gnu
Summary:   OpenHPC python related library builds for use with GNU compiler toolchain
Requires:  python-numpy-gnu-ohpc
Requires:  python-scipy-gnu-mpich-ohpc
Requires:  python-scipy-gnu-openmpi-ohpc
%ifnarch aarch64 && !0%{?centos_version}
Requires:  python-scipy-gnu-mvapich2-ohpc
%endif
%description ohpc-python-libs-gnu
OpenHPC python related library builds for use with GNU compiler toolchain

%package ohpc-python-libs-intel
Summary:   OpenHPC python related library builds for use with Intel(R) Parallel Studio XE toolchain
Requires:  python-numpy-intel-ohpc
%description ohpc-python-libs-intel
OpenHPC python related library builds for use with Intel(R) Parallel Studio XE toolchain

%package ohpc-runtimes-gnu
Summary:   OpenHPC runtimes for use with GNU compiler toolchain
Requires:  ocr-gnu-ohpc
Requires:  singularity-ohpc
%description ohpc-runtimes-gnu
OpenHPC runtimes for use with GNU compiler toolchain

%package ohpc-runtimes-intel
Summary:   OpenHPC runtimes for use with Intel(R) Parallel Studio XE toolchain
Requires:  ocr-gnu-ohpc
Requires:  singularity-ohpc
%description ohpc-runtimes-intel
OpenHPC runtimes for use with Intel(R) Parallel Studio XE toolchain

%package ohpc-serial-libs-gnu
Summary:   OpenHPC serial library builds for use with GNU compiler toolchain
Requires:  gsl-gnu-ohpc
Requires:  metis-gnu-ohpc
Requires:  openblas-gnu-ohpc
Requires:  superlu-gnu-ohpc
%description ohpc-serial-libs-gnu
OpenHPC serial library builds for use with GNU compiler toolchain

%package ohpc-serial-libs-intel
Summary:   OpenHPC serial library builds for use with Intel(R) Parallel Studio XE toolchain
Requires:  metis-intel-ohpc
Requires:  superlu-intel-ohpc
%description ohpc-serial-libs-intel
OpenHPC serial library builds for use with Intel(R) Parallel Studio XE toolchain

%package ohpc-slurm-client
Summary:   OpenHPC client packages for SLURM
Requires:  slurm-ohpc
Requires:  slurm-munge-ohpc
Requires:  slurm-plugins-ohpc
Requires:  slurm-sjobexit-ohpc
Requires:  slurm-pam_slurm-ohpc
Requires:  munge-ohpc
%description ohpc-slurm-client
OpenHPC client packages for SLURM

%package ohpc-slurm-server
Summary:   OpenHPC server packages for SLURM
Requires:  slurm-ohpc
Requires:  slurm-munge-ohpc
Requires:  slurm-plugins-ohpc
Requires:  slurm-perlapi-ohpc
Requires:  slurm-devel-ohpc
Requires:  slurm-slurmdbd-ohpc
Requires:  slurm-sql-ohpc
Requires:  slurm-slurmdb-direct-ohpc
Requires:  munge-ohpc
Requires:  munge-libs-ohpc
Requires:  munge-devel-ohpc
%description ohpc-slurm-server
OpenHPC server packages for SLURM

%package ohpc-warewulf
Summary:   OpenHPC collection of base packages for Warewulf provisioning
Requires:  warewulf-cluster-ohpc
Requires:  warewulf-common-ohpc
Requires:  warewulf-provision-ohpc
Requires:  warewulf-provision-server-ohpc
Requires:  warewulf-vnfs-ohpc
%description ohpc-warewulf
OpenHPC collection of base packages for Warewulf provisioning


%prep

%build

%install

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%dir %{OHPC_HOME}
%doc LICENSE
%{OHPC_PUB}

%changelog

