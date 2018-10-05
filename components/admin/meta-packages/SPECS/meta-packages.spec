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
%global gnu_major_ver 8
%global openmpi_major_ver 3

%if 0%{?rhel_version}
%global python3_prefix python34
%else
%global python3_prefix python3
%endif

Summary: Meta-packages to ease installation
Name:    meta-packages
Version: 1.3.6
Release: 1
License: Apache-2.0
Group:   %{PROJ_NAME}/meta-package
URL:     https://github.com/openhpc/ohpc
Source0: LICENSE



%description

This is an internal package that is used to create groups as part
of the installation source setup.  Installation of this package does
not make sense.

%package -n %{PROJ_NAME}-autotools
Summary:   OpenHPC autotools
Requires:  autoconf%{PROJ_DELIM}
Requires:  automake%{PROJ_DELIM}
Requires:  libtool%{PROJ_DELIM}
%description -n %{PROJ_NAME}-autotools
Collection of GNU autotools packages

%package -n %{PROJ_NAME}-base
Summary:   OpenHPC base
Requires:  bc
Requires:  conman%{PROJ_DELIM}
Requires:  cmake%{PROJ_DELIM}
Requires:  emacs-nox
Requires:  examples%{PROJ_DELIM}
Requires:  gdb
Requires:  ipmitool
Requires:  libstdc++-devel
Requires:  libunwind
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
%if 0%{?sles_version} || 0%{?suse_version}
Requires:  glibc-locale
Requires:  nfs-kernel-server
%endif
%description -n %{PROJ_NAME}-base
Collection of base packages

%package -n %{PROJ_NAME}-base-compute
Summary:   OpenHPC compute node base
Requires:  binutils
Requires:  libicu
Requires:  libunwind
Requires:  numactl
%if 0%{?centos_version} || 0%{?rhel_version}
Requires:  cairo-devel
Requires:  libpciaccess
Requires:  python34
%endif
%if 0%{?sles_version} || 0%{?suse_version}
Requires:  libcairo2
Requires:  libpciaccess0
Requires:  python3
%endif
%description -n %{PROJ_NAME}-base-compute
Collection of compute node base packages

%package -n %{PROJ_NAME}-ganglia
Summary:   OpenHPC Ganglia monitoring
Requires:  ganglia%{PROJ_DELIM}
Requires:  ganglia-devel%{PROJ_DELIM}
Requires:  ganglia-gmetad%{PROJ_DELIM}
Requires:  ganglia-gmond%{PROJ_DELIM}
Requires:  ganglia-gmond-python%{PROJ_DELIM}
Requires:  ganglia-web%{PROJ_DELIM}
%description -n %{PROJ_NAME}-ganglia
Collection of Ganglia monitoring and metrics packages

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-io-libs
Summary:   OpenHPC IO libraries for GNU
Requires:  adios-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  adios-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  hdf5-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  netcdf-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  netcdf-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  pnetcdf-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  pnetcdf-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  phdf5-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  phdf5-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%ifnarch aarch64
Requires:  adios-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  netcdf-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  pnetcdf-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  phdf5-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
%endif
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-io-libs
Collection of IO library builds for use with GNU compiler toolchain

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mpich-io-libs
Summary:   OpenHPC IO libraries for GNU and MPICH
Requires:  adios-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  netcdf-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  pnetcdf-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  phdf5-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  hdf5-gnu%{gnu_major_ver}%{PROJ_DELIM}
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mpich-io-libs
Collection of IO library builds for use with GNU compiler toolchain and the MPICH runtime

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mvapich2-io-libs
Summary:   OpenHPC IO libraries for GNU and MVAPICH2
Requires:  adios-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  netcdf-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  pnetcdf-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  phdf5-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  hdf5-gnu%{gnu_major_ver}%{PROJ_DELIM}
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mvapich2-io-libs
Collection of IO library builds for use with GNU compiler toolchain and the MVAPICH2 runtime

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}-io-libs
Summary:   OpenHPC IO libraries for GNU and OpenMPI
Requires:  adios-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  netcdf-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  pnetcdf-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  phdf5-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  hdf5-gnu%{gnu_major_ver}%{PROJ_DELIM}
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}-io-libs
Collection of IO library builds for use with GNU compiler toolchain and the OpenMPI runtime

%package -n %{PROJ_NAME}-nagios
Summary:   OpenHPC Nagios monitoring
Requires:  nagios%{PROJ_DELIM}
Requires:  nagios-plugins-all%{PROJ_DELIM}
Requires:  nrpe%{PROJ_DELIM}
%description -n %{PROJ_NAME}-nagios
Collection of Nagios monitoring and metrics packages

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-parallel-libs
Summary:   OpenHPC parallel libraries for GNU
Requires:  boost-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  boost-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  fftw-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  fftw-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  hypre-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  hypre-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  mfem-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  mfem-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  mumps-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  mumps-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  petsc-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  petsc-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  scalapack-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  scalapack-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  slepc-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  slepc-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  ptscotch-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  ptscotch-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  superlu_dist-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  superlu_dist-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  trilinos-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  trilinos-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%ifnarch aarch64
Requires:  boost-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  fftw-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  hypre-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  mfem-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  mumps-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  petsc-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  scalapack-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  slepc-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  ptscotch-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  superlu_dist-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  trilinos-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
%endif
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-parallel-libs
Collection of parallel library builds for use with GNU compiler toolchain

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mpich-parallel-libs
Summary:   OpenHPC parallel libraries for GNU and MPICH
Requires:  boost-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  fftw-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  hypre-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  mfem-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  mumps-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  petsc-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  scalapack-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  slepc-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  ptscotch-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  superlu_dist-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  trilinos-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mpich-parallel-libs
Collection of parallel library builds for use with GNU compiler toolchain and the MPICH runtime

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}-parallel-libs
Summary:   OpenHPC parallel libraries for GNU and OpenMPI
Requires:  boost-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  fftw-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  hypre-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  mfem-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  mumps-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  petsc-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  scalapack-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  slepc-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  ptscotch-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  superlu_dist-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  trilinos-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}-parallel-libs
Collection of parallel library builds for use with GNU compiler toolchain and the OpenMPI runtime

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-perf-tools
Summary:   OpenHPC performance tools for GNU
Requires:  imb-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  imb-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  mpiP-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  mpiP-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  tau-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  tau-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  scalasca-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  scalasca-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  scorep-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  scorep-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%ifnarch aarch64
Requires:  geopm-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  geopm-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  geopm-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  imb-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  likwid-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  mpiP-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
Requires:  tau-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  scalasca-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  scorep-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
%endif
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-perf-tools
Collection of performance tool builds for use with GNU compiler toolchain

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mpich-perf-tools
Summary:   OpenHPC performance tools for GNU and MPICH
Requires:  imb-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  mpiP-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  tau-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  scalasca-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  scorep-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
%ifnarch aarch64
Requires:  geopm-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
%endif
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mpich-perf-tools
Collection of performance tool builds for use with GNU compiler toolchain and the MPICH runtime

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mvapich2-perf-tools
Summary:   OpenHPC performance tools for GNU and MVAPICH2
Requires:  imb-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  likwid-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  mpiP-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  tau-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  scalasca-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  scorep-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
%ifnarch aarch64
Requires:  geopm-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
%endif
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mvapich2-perf-tools
Collection of performance tool builds for use with GNU compiler toolchain and the MVAPICH2 runtime

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}-perf-tools
Summary:   OpenHPC performance tools for GNU and OpenMPI
Requires:  imb-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  likwid-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  mpiP-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  tau-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  scalasca-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  scorep-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
%ifnarch aarch64
Requires:  geopm-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%endif
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}-perf-tools
Collection of performance tool builds for use with GNU compiler toolchain and the OpenMPI runtime

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-python-libs
Summary:   OpenHPC python libraries for GNU
Requires:  %{PROJ_NAME}-gnu%{gnu_major_ver}-python2-libs
Requires:  %{PROJ_NAME}-gnu%{gnu_major_ver}-python3-libs
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-python-libs
Collection of python related library builds for use with GNU compiler toolchain

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-python2-libs
Summary:   OpenHPC python2 libraries for GNU
Requires:  python-mpi4py-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  python-mpi4py-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  python-numpy-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  python-scipy-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  python-scipy-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%ifnarch aarch64
Requires:  python-mpi4py-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  python-scipy-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
%endif
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-python2-libs
Collection of python2 related library builds for use with GNU compiler toolchain

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-python3-libs
Summary:   OpenHPC python3 libraries for GNU
Requires:  %{python3_prefix}-mpi4py-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  %{python3_prefix}-mpi4py-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  %{python3_prefix}-numpy-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  %{python3_prefix}-scipy-gnu%{gnu_major_ver}-mpich%{PROJ_DELIM}
Requires:  %{python3_prefix}-scipy-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%ifnarch aarch64
Requires:  %{python3_prefix}-mpi4py-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  %{python3_prefix}-scipy-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
%endif
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-python3-libs
Collection of python3 related library builds for use with GNU compiler toolchain

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-runtimes
Summary:   OpenHPC runtimes for GNU
Requires:  ocr-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  charliecloud%{PROJ_DELIM}
Requires:  singularity%{PROJ_DELIM}
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-runtimes
Collection of runtimes for use with GNU compiler toolchain

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-serial-libs
Summary:   OpenHPC serial libraries for GNU
Requires:  gsl-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  metis-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  openblas-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  plasma-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  R-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  scotch-gnu%{gnu_major_ver}%{PROJ_DELIM}
Requires:  superlu-gnu%{gnu_major_ver}%{PROJ_DELIM}
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-serial-libs
Collection of serial library builds for use with GNU compiler toolchain

%package -n %{PROJ_NAME}-slurm-client
Summary:   OpenHPC client packages for SLURM
Requires:  msr-safe-slurm%{PROJ_DELIM}
Requires:  munge%{PROJ_DELIM}
Requires:  slurm%{PROJ_DELIM}
Requires:  slurm-slurmd%{PROJ_DELIM}
Requires:  slurm-contribs%{PROJ_DELIM}
Requires:  slurm-example-configs%{PROJ_DELIM}
Requires:  slurm-pam_slurm%{PROJ_DELIM}
%if 0%{?centos_version} || 0%{?rhel_version}
Requires:  hwloc-libs
%endif
%if 0%{?sles_version} || 0%{?suse_version}
Requires:  libhwloc5
%endif
%description -n %{PROJ_NAME}-slurm-client
Collection of client packages for SLURM

%package -n %{PROJ_NAME}-slurm-server
Summary:   OpenHPC server packages for SLURM
Requires:  slurm%{PROJ_DELIM}
Requires:  slurm-devel%{PROJ_DELIM}
Requires:  slurm-example-configs%{PROJ_DELIM}
Requires:  slurm-perlapi%{PROJ_DELIM}
Requires:  slurm-slurmctld%{PROJ_DELIM}
Requires:  slurm-slurmdbd%{PROJ_DELIM}
Requires:  munge%{PROJ_DELIM}
Requires:  munge-devel%{PROJ_DELIM}
Requires:  munge-libs%{PROJ_DELIM}
Requires:  pdsh-mod-slurm%{PROJ_DELIM}
%description -n %{PROJ_NAME}-slurm-server
Collection of server packages for SLURM

%package -n %{PROJ_NAME}-warewulf
Summary:   OpenHPC base packages for Warewulf
Requires:  warewulf-cluster%{PROJ_DELIM}
Requires:  warewulf-common%{PROJ_DELIM}
Requires:  warewulf-ipmi%{PROJ_DELIM}
Requires:  warewulf-provision-initramfs-%{_arch}%{PROJ_DELIM}
Requires:  warewulf-provision%{PROJ_DELIM}
Requires:  warewulf-provision-server%{PROJ_DELIM}
Requires:  warewulf-provision-server-ipxe-%{_arch}%{PROJ_DELIM}
Requires:  warewulf-vnfs%{PROJ_DELIM}
%description -n %{PROJ_NAME}-warewulf
Collection of base packages for Warewulf provisioning

# x86_64 specific groups
%ifnarch aarch64
%package -n %{PROJ_NAME}-intel-io-libs
Summary:   OpenHPC IO libraries for Intel(R) Parallel Studio XE
Requires:  adios-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  adios-intel-impi%{PROJ_DELIM}
Requires:  adios-intel-mpich%{PROJ_DELIM}
Requires:  adios-intel-mvapich2%{PROJ_DELIM}
Requires:  adios-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  hdf5-intel%{PROJ_DELIM}
Requires:  netcdf-cxx-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-impi%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-mpich%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-mvapich2%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  netcdf-fortran-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-impi%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-mpich%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-mvapich2%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  netcdf-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  netcdf-intel-impi%{PROJ_DELIM}
Requires:  netcdf-intel-mpich%{PROJ_DELIM}
Requires:  netcdf-intel-mvapich2%{PROJ_DELIM}
Requires:  netcdf-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  pnetcdf-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  pnetcdf-intel-impi%{PROJ_DELIM}
Requires:  pnetcdf-intel-mpich%{PROJ_DELIM}
Requires:  pnetcdf-intel-mvapich2%{PROJ_DELIM}
Requires:  pnetcdf-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  phdf5-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  phdf5-intel-impi%{PROJ_DELIM}
Requires:  phdf5-intel-mpich%{PROJ_DELIM}
Requires:  phdf5-intel-mvapich2%{PROJ_DELIM}
Requires:  phdf5-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-io-libs
Collection of IO library builds for use with Intel(R) Parallel Studio XE software suite

%package -n %{PROJ_NAME}-intel-impi-io-libs
Summary:   OpenHPC IO libraries for Intel(R) Parallel Studio XE and Intel(R) MPI runtime
Requires:  adios-intel-impi%{PROJ_DELIM}
Requires:  hdf5-intel%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-impi%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-impi%{PROJ_DELIM}
Requires:  netcdf-intel-impi%{PROJ_DELIM}
Requires:  phdf5-intel-impi%{PROJ_DELIM}
Requires:  pnetcdf-intel-impi%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-impi-io-libs
Collection of IO library builds for use with Intel(R) Parallel Studio XE software suite and Intel(R) MPI runtime

%package -n %{PROJ_NAME}-intel-mpich-io-libs
Summary:   OpenHPC IO libraries for Intel(R) Parallel Studio XE and MPICH
Requires:  adios-intel-mpich%{PROJ_DELIM}
Requires:  hdf5-intel%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-mpich%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-mpich%{PROJ_DELIM}
Requires:  netcdf-intel-mpich%{PROJ_DELIM}
Requires:  phdf5-intel-mpich%{PROJ_DELIM}
Requires:  pnetcdf-intel-mpich%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-mpich-io-libs
Collection of IO library builds for use with Intel(R) Parallel Studio XE software suite and MPICH runtime

%package -n %{PROJ_NAME}-intel-mvapich2-io-libs
Summary:   OpenHPC IO libraries for Intel(R) Parallel Studio XE and MVAPICH2
Requires:  adios-intel-mvapich2%{PROJ_DELIM}
Requires:  hdf5-intel%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-mvapich2%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-mvapich2%{PROJ_DELIM}
Requires:  netcdf-intel-mvapich2%{PROJ_DELIM}
Requires:  phdf5-intel-mvapich2%{PROJ_DELIM}
Requires:  pnetcdf-intel-mvapich2%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-mvapich2-io-libs
Collection of IO library builds for use with Intel(R) Parallel Studio XE software suite and MVAPICH2 runtime

%package -n %{PROJ_NAME}-intel-openmpi%{openmpi_major_ver}-io-libs
Summary:   OpenHPC IO libraries for Intel(R) Parallel Studio XE and OpenMPI
Requires:  adios-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  hdf5-intel%{PROJ_DELIM}
Requires:  netcdf-cxx-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  netcdf-fortran-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  netcdf-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  phdf5-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  pnetcdf-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-openmpi%{openmpi_major_ver}-io-libs
Collection of IO library builds for use with Intel(R) Parallel Studio XE software suite and OpenMPI runtime

%package -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mvapich2-parallel-libs
Summary:   OpenHPC parallel libraries for GNU and MVAPICH2
Requires:  boost-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  fftw-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  hypre-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  mfem-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  mumps-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  petsc-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  scalapack-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  slepc-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  ptscotch-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  superlu_dist-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
Requires:  trilinos-gnu%{gnu_major_ver}-mvapich2%{PROJ_DELIM}
%description -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mvapich2-parallel-libs
Collection of parallel library builds for use with GNU compiler toolchain and the MVAPICH2 runtime

%package -n %{PROJ_NAME}-intel-impi-parallel-libs
Summary:   OpenHPC parallel libraries for Intel(R) Parallel Studio XE and Intel(R) MPI Library
Requires:  boost-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  boost-intel-impi%{PROJ_DELIM}
Requires:  hypre-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  hypre-intel-impi%{PROJ_DELIM}
Requires:  mfem-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  mfem-intel-impi%{PROJ_DELIM}
Requires:  mumps-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  mumps-intel-impi%{PROJ_DELIM}
Requires:  petsc-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  petsc-intel-impi%{PROJ_DELIM}
Requires:  scalapack-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  scalapack-intel-impi%{PROJ_DELIM}
Requires:  slepc-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  slepc-intel-impi%{PROJ_DELIM}
Requires:  ptscotch-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  ptscotch-intel-impi%{PROJ_DELIM}
Requires:  superlu_dist-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  superlu_dist-intel-impi%{PROJ_DELIM}
Requires:  trilinos-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  trilinos-intel-impi%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-impi-parallel-libs
Collection of parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the Intel(R) MPI Library

%package -n %{PROJ_NAME}-intel-mpich-parallel-libs
Summary:   OpenHPC parallel libraries for Intel(R) Parallel Studio XE and MPICH
Requires:  boost-intel-mpich%{PROJ_DELIM}
Requires:  hypre-intel-mpich%{PROJ_DELIM}
Requires:  mfem-intel-mpich%{PROJ_DELIM}
Requires:  mumps-intel-mpich%{PROJ_DELIM}
Requires:  petsc-intel-mpich%{PROJ_DELIM}
Requires:  scalapack-intel-mpich%{PROJ_DELIM}
Requires:  slepc-intel-mpich%{PROJ_DELIM}
Requires:  ptscotch-intel-mpich%{PROJ_DELIM}
Requires:  superlu_dist-intel-mpich%{PROJ_DELIM}
Requires:  trilinos-intel-mpich%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-mpich-parallel-libs
Collection of parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the MPICH runtime

%package -n %{PROJ_NAME}-intel-mvapich2-parallel-libs
Summary:   OpenHPC parallel libraries for Intel(R) Parallel Studio XE and MVAPICH2
Requires:  boost-intel-mvapich2%{PROJ_DELIM}
Requires:  hypre-intel-mvapich2%{PROJ_DELIM}
Requires:  mfem-intel-mvapich2%{PROJ_DELIM}
Requires:  mumps-intel-mvapich2%{PROJ_DELIM}
Requires:  petsc-intel-mvapich2%{PROJ_DELIM}
Requires:  scalapack-intel-mvapich2%{PROJ_DELIM}
Requires:  slepc-intel-mvapich2%{PROJ_DELIM}
Requires:  ptscotch-intel-mvapich2%{PROJ_DELIM}
Requires:  superlu_dist-intel-mvapich2%{PROJ_DELIM}
Requires:  trilinos-intel-mvapich2%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-mvapich2-parallel-libs
Collection of parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the MVAPICH2 runtime

%package -n %{PROJ_NAME}-intel-openmpi%{openmpi_major_ver}-parallel-libs
Summary:   OpenHPC parallel libraries for Intel(R) Parallel Studio XE and OpenMPI
Requires:  boost-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  hypre-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  mfem-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  mumps-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  petsc-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  scalapack-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  slepc-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  ptscotch-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  superlu_dist-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  trilinos-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-openmpi%{openmpi_major_ver}-parallel-libs
Collection of parallel library builds for use with Intel(R) Parallel Studio XE toolchain and the OpenMPI runtime

%package -n %{PROJ_NAME}-intel-perf-tools
Summary:   OpenHPC performance tools for Intel(R) Parallel Studio XE
Requires:  geopm-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  geopm-intel-impi%{PROJ_DELIM}
Requires:  geopm-intel-mpich%{PROJ_DELIM}
Requires:  geopm-intel-mvapich2%{PROJ_DELIM}
Requires:  geopm-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  imb-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  imb-intel-impi%{PROJ_DELIM}
Requires:  imb-intel-mpich%{PROJ_DELIM}
Requires:  imb-intel-mvapich2%{PROJ_DELIM}
Requires:  imb-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  likwid-intel%{PROJ_DELIM}
Requires:  mpiP-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
Requires:  tau-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  tau-intel-impi%{PROJ_DELIM}
Requires:  tau-intel-mpich%{PROJ_DELIM}
Requires:  tau-intel-mvapich2%{PROJ_DELIM}
Requires:  tau-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  scalasca-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  scalasca-intel-impi%{PROJ_DELIM}
Requires:  scalasca-intel-mpich%{PROJ_DELIM}
Requires:  scalasca-intel-mvapich2%{PROJ_DELIM}
Requires:  scalasca-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  scorep-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  scorep-intel-impi%{PROJ_DELIM}
Requires:  scorep-intel-mpich%{PROJ_DELIM}
Requires:  scorep-intel-mvapich2%{PROJ_DELIM}
Requires:  scorep-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-perf-tools
Collection of performance tool builds for use with Intel(R) Parallel Studio XE toolchain

%package -n %{PROJ_NAME}-intel-impi-perf-tools
Summary:   OpenHPC performance tools for Intel(R) Parallel Studio XE and Intel(R) MPI
Requires:  geopm-intel-impi%{PROJ_DELIM}
Requires:  imb-intel-impi%{PROJ_DELIM}
Requires:  likwid-intel%{PROJ_DELIM}
Requires:  mpiP-intel-impi%{PROJ_DELIM}
Requires:  tau-intel-impi%{PROJ_DELIM}
Requires:  scalasca-intel-impi%{PROJ_DELIM}
Requires:  scorep-intel-impi%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-impi-perf-tools
Collection of performance tool builds for use with Intel(R) Parallel Studio XE compiler toolchain and the Intel(R) MPI runtime

%package -n %{PROJ_NAME}-intel-mpich-perf-tools
Summary:   OpenHPC performance tools for Intel(R) Parallel Studio XE and MPICH
Requires:  geopm-intel-mpich%{PROJ_DELIM}
Requires:  imb-intel-mpich%{PROJ_DELIM}
Requires:  likwid-intel%{PROJ_DELIM}
Requires:  mpiP-intel-mpich%{PROJ_DELIM}
Requires:  tau-intel-mpich%{PROJ_DELIM}
Requires:  scalasca-intel-mpich%{PROJ_DELIM}
Requires:  scorep-intel-mpich%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-mpich-perf-tools
Collection of performance tool builds for use with Intel(R) Parallel Studio XE compiler toolchain and the MPICH runtime

%package -n %{PROJ_NAME}-intel-mvapich2-perf-tools
Summary:   OpenHPC performance tools for Intel(R) Parallel Studio XE and MVAPICH2
Requires:  geopm-intel-mvapich2%{PROJ_DELIM}
Requires:  imb-intel-mvapich2%{PROJ_DELIM}
Requires:  likwid-intel%{PROJ_DELIM}
Requires:  mpiP-intel-mvapich2%{PROJ_DELIM}
Requires:  tau-intel-mvapich2%{PROJ_DELIM}
Requires:  scalasca-intel-mvapich2%{PROJ_DELIM}
Requires:  scorep-intel-mvapich2%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-mvapich2-perf-tools
Collection of performance tool builds for use with Intel(R) Parallel Studio XE compiler toolchain and the MVAPICH2 runtime

%package -n %{PROJ_NAME}-intel-openmpi%{openmpi_major_ver}-perf-tools
Summary:   OpenHPC performance tools for Intel(R) Parallel Studio XE and OpenMPI
Requires:  geopm-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  imb-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  likwid-intel%{PROJ_DELIM}
Requires:  mpiP-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  tau-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  scalasca-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  scorep-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
Requires:  papi%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-openmpi%{openmpi_major_ver}-perf-tools
Collection of performance tool builds for use with Intel(R) Parallel Studio XE compiler toolchain and the OpenMPI runtime

%package -n %{PROJ_NAME}-intel-python-libs
Summary:   OpenHPC python libraries for Intel(R) Parallel Studio XE
Requires:  %{PROJ_NAME}-intel-python2-libs
Requires:  %{PROJ_NAME}-intel-python3-libs
%description -n %{PROJ_NAME}-intel-python-libs
Collection of python related library builds for use with Intel(R) Parallel Studio XE toolchain

%package -n %{PROJ_NAME}-intel-python2-libs
Summary:   OpenHPC python2 libraries for Intel(R) Parallel Studio XE
Requires:  python-numpy-intel%{PROJ_DELIM}
Requires:  python-mpi4py-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  python-mpi4py-intel-impi%{PROJ_DELIM}
Requires:  python-mpi4py-intel-mpich%{PROJ_DELIM}
Requires:  python-mpi4py-intel-mvapich2%{PROJ_DELIM}
Requires:  python-mpi4py-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-python2-libs
Collection of python2 related library builds for use with Intel(R) Parallel Studio XE toolchain

%package -n %{PROJ_NAME}-intel-python3-libs
Summary:   OpenHPC python3 libraries for Intel(R) Parallel Studio XE
Requires:  %{python3_prefix}-numpy-intel%{PROJ_DELIM}
Requires:  %{python3_prefix}-mpi4py-gnu%{gnu_major_ver}-impi%{PROJ_DELIM}
Requires:  %{python3_prefix}-mpi4py-intel-impi%{PROJ_DELIM}
Requires:  %{python3_prefix}-mpi4py-intel-mpich%{PROJ_DELIM}
Requires:  %{python3_prefix}-mpi4py-intel-mvapich2%{PROJ_DELIM}
Requires:  %{python3_prefix}-mpi4py-intel-openmpi%{openmpi_major_ver}%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-python3-libs
Collection of python3 related library builds for use with Intel(R) Parallel Studio XE toolchain

%package -n %{PROJ_NAME}-intel-runtimes
Summary:   OpenHPC runtimes for Intel(R) Parallel Studio XE toolchain
Requires:  ocr-intel%{PROJ_DELIM}
Requires:  charliecloud%{PROJ_DELIM}
Requires:  singularity%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-runtimes
Collection of runtimes for use with Intel(R) Parallel Studio XE toolchain

%package -n %{PROJ_NAME}-intel-serial-libs
Summary:   OpenHPC serial libraries for Intel(R) Parallel Studio XE
Requires:  metis-intel%{PROJ_DELIM}
Requires:  plasma-intel%{PROJ_DELIM}
Requires:  scotch-intel%{PROJ_DELIM}
Requires:  superlu-intel%{PROJ_DELIM}
%description -n %{PROJ_NAME}-intel-serial-libs
Collection of serial library builds for use with Intel(R) Parallel Studio XE toolchain

%endif


%prep
%{__cp} %SOURCE0 .

%build

%install

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files

%files -n %{PROJ_NAME}-autotools
%files -n %{PROJ_NAME}-base
%files -n %{PROJ_NAME}-base-compute
%files -n %{PROJ_NAME}-ganglia
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-io-libs
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mpich-io-libs
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}-io-libs
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-parallel-libs
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mpich-parallel-libs
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}-parallel-libs
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-perf-tools
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mpich-perf-tools
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-openmpi%{openmpi_major_ver}-perf-tools
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-python-libs
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-python2-libs
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-python3-libs
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-runtimes
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-serial-libs
%files -n %{PROJ_NAME}-nagios
%files -n %{PROJ_NAME}-slurm-client
%files -n %{PROJ_NAME}-slurm-server
%files -n %{PROJ_NAME}-warewulf
# x86_64 specific groups
%ifnarch aarch64
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mvapich2-io-libs
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mvapich2-perf-tools
%files -n %{PROJ_NAME}-gnu%{gnu_major_ver}-mvapich2-parallel-libs
%files -n %{PROJ_NAME}-intel-io-libs
%files -n %{PROJ_NAME}-intel-impi-io-libs
%files -n %{PROJ_NAME}-intel-mpich-io-libs
%files -n %{PROJ_NAME}-intel-mvapich2-io-libs
%files -n %{PROJ_NAME}-intel-openmpi%{openmpi_major_ver}-io-libs
%files -n %{PROJ_NAME}-intel-impi-parallel-libs
%files -n %{PROJ_NAME}-intel-mpich-parallel-libs
%files -n %{PROJ_NAME}-intel-mvapich2-parallel-libs
%files -n %{PROJ_NAME}-intel-openmpi%{openmpi_major_ver}-parallel-libs
%files -n %{PROJ_NAME}-intel-perf-tools
%files -n %{PROJ_NAME}-intel-impi-perf-tools
%files -n %{PROJ_NAME}-intel-mpich-perf-tools
%files -n %{PROJ_NAME}-intel-mvapich2-perf-tools
%files -n %{PROJ_NAME}-intel-openmpi%{openmpi_major_ver}-perf-tools
%files -n %{PROJ_NAME}-intel-python-libs
%files -n %{PROJ_NAME}-intel-python2-libs
%files -n %{PROJ_NAME}-intel-python3-libs
%files -n %{PROJ_NAME}-intel-runtimes
%files -n %{PROJ_NAME}-intel-serial-libs
%endif
