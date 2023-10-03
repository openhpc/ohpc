# -*-sh-*-
# vim: ft=config

[global]
github_url = "git@github.com:openhpc/ohpc.git"
service_template=service.file
link_compiler_template=link_compiler
link_mpi_template=link_mpi

override_templates=templates
dry_run=True  # do not make changes to OBS, just do a dry-run

[groups]
# used to identify where components reside in ohpc Github repo
admin         = ["conman","docs","examples","ganglia","genders","lmod-defaults","lmod",
	         "losf","meta-packages","mrsh","nagios","nagios-plugins","ndoutils","nhc","nrpe",
		 "ohpc-filesystem","ohpc-release","hpc-workspace",
		 "pdsh","prun","test-suite"]
dev-tools     = ["autoconf","automake","cmake","easybuild","hwloc","libtool","mpi4py","numpy",
	         "scipy","spack","valgrind"]
distro-packages = ["python-Cython","flex"]
fs            = ["lustre-client"]
io-libs       = ["adios","hdf5","netcdf","netcdf-cxx","netcdf-fortran","phdf5","pnetcdf","sionlib",
                 "adios2"]
runtimes      = ["singularity","ocr","charliecloud"]
rms           = ["slurm","openpbs","pmix","munge","magpie"]
serial-libs   = ["R","gsl","metis","openblas","plasma","scotch","superlu"]
parallel-libs = ["boost","fftw","hypre","mfem","mumps","opencoarrays","petsc","scalapack","ptscotch",
                 "slepc","superlu_dist","trilinos"]
provisioning  = ["warewulf-vnfs","warewulf","warewulf-common","warewulf-cluster","warewulf-ipmi",
                 "warewulf-provision"]
perf-tools    = ["dimemas","extrae","geopm","imb","likwid","msr-safe","omb","papi","paraver","pdtoolkit",
                 "scalasca","scorep","tau","papi57"]

compiler-families=["gnu-compilers","intel-compilers-devel","arm-compilers-devel","llvm-compilers"]
mpi-families=["impi-devel","mpich","mvapich2","openmpi","libfabric","ucx"]

[3.0.0]
skip_aarch=["-intel\\b","lustre-client","-impi\\b","-mvapich2\\b","likwid-arm1","geopm",
            "intel-compilers-devel","impi-devel","mvapich2","openblas-arm1","tau-arm1","plasma-arm1"]
skip_x86  = ["-arm1","arm-compilers-devel"]
compiler_families=["gnu12","intel","arm1"]
mpi_families=["openmpi4","mpich","mvapich2","impi"]

standalone = ["ohpc-filesystem","slurm","hwloc","lmod","genders","magpie","easybuild","prun","gnu-compilers","ucx",
              "libfabric","openpbs","conman","autoconf","automake","spack","cmake","libtool","python-Cython",
              "hpc-workspace","intel-compilers-devel","impi-devel","mrsh","losf","paraver","papi","charliecloud",
              "pmix","pdsh","flex","warewulf-common","warewulf-vnfs","warewulf-cluster","warewulf-ipmi","docs",
              "warewulf-provision","examples","meta-packages","valgrind","warewulf","arm-compilers-devel",
              "nhc","test-suite"]

compiler_dependent = ["openmpi","mpich","mvapich2","openblas","R","likwid",
                      "pdtoolkit","gsl","metis","superlu","scotch",
                      "numpy","plasma","hdf5"]
mpi_dependent = ["ptscotch","boost","sionlib","pnetcdf","phdf5","netcdf","omb",
                 "tau","extrae","imb","fftw","scalapack","opencoarrays",
                 "hypre","mpi4py","dimemas","scorep","scalasca",
                 "scipy","adios2","netcdf-fortran","netcdf-cxx","trilinos",
                 "petsc","slepc","superlu_dist","mumps","mfem","lmod-defaults",
                 "geopm"]
openblas_compiler=["gnu12"]
R_compiler=["gnu12"]
trilinos_compiler=["gnu12"]
lmod-defaults_mpi=["openmpi4","mvapich2","impi"]
opencoarrays_compiler=["gnu12"]
scipy_compiler=["gnu12"]
openmpi_compiler=["gnu12","arm1","intel"]
# The parser is looking for entries starting with 'skip_on_distro_' and will
# disable all packages on the distro after 'skip_on_distro_'
# This is mainy used to automatically disable '-intel' and '-arm1' packages
# on distros without support (like openEuler).
skip_on_distro_openEuler_22.03 = ["-arm1","-intel","-impi","impi-devel","intel-compilers-devel","arm-compilers-devel"]


[2.7.0]
skip_aarch=["-intel\\b","lustre-client","-impi\\b","-mvapich2\\b","likwid-gnu","likwid-arm1","geopm",
            "intel-compilers-devel","impi-devel","mvapich2","openblas-arm1"]
skip_x86  = ["-arm1"]
compiler_families=["intel","gnu12","arm1"]
mpi_families=["openmpi4","mpich","mvapich2","impi"]

standalone = ["ohpc-filesystem","slurm","hwloc","lmod","easybuild","papi57","magpie","ucx",
              "spack","intel-compilers-devel","impi-devel"]
compiler_dependent = ["openmpi","mvapich2","mpich","likwid","pdtoolkit","gsl",
		      "superlu","scotch","numpy","plasma","hdf5"]
mpi_dependent = ["phdf5","netcdf","boost","sionlib","ptscotch","pnetcdf","omb",
                 "tau","extrae","imb","fftw","scalapack",
                 "hypre","mpi4py","dimemas","scorep","scalasca",
                 "adios","netcdf-fortran","netcdf-cxx","!trilinos",
                 "petsc","slepc","superlu_dist","mumps","mfem",
                 "geopm"]

scipy_mpi=["openmpi4","mpich","mvapich2"]
# Only rebuilding everything with the Intel compiler for this release.
# The Intel compiler dependency has been switched from a minimal version
# to an exact version that is required to ensure the same version is
# used during building the packages as well as using the packages.

# For this to work correctly the first compiler in the 'compiler_families'
# needs to be 'intel'. This breaks the usual setup where the *gnu12* package
# is the main package and all other compiler packages are links to the
# *gnu12* package.

mpich_compiler = ["intel"]
mvapich2_compiler = ["intel"]
phdf5_compiler = ["intel"]
netcdf_compiler = ["intel"]
boost_compiler = ["intel"]
likwid_compiler = ["intel"]
pdtoolkit_compiler = ["intel"]
gsl_compiler = ["intel"]
superlu_compiler = ["intel"]
scotch_compiler = ["intel"]
numpy_compiler = ["intel"]
plasma_compiler = ["intel"]
hdf5_compiler = ["intel"]
sionlib_compiler = ["intel"]
ptscotch_compiler = ["intel"]
pnetcdf_compiler = ["intel"]
omb_compiler = ["intel"]
tau_compiler = ["intel"]
extrae_compiler = ["intel"]
imb_compiler = ["intel"]
fftw_compiler = ["intel"]
scalapack_compiler = ["intel"]
hypre_compiler = ["intel"]
mpi4py_compiler = ["intel"]
dimemas_compiler = ["intel"]
scorep_compiler = ["intel"]
scalasca_compiler = ["intel"]
adios_compiler = ["intel"]
netcdf-fortran_compiler = ["intel"]
netcdf-cxx_compiler = ["intel"]
petsc_compiler = ["intel"]
slepc_compiler = ["intel"]
superlu_dist_compiler = ["intel"]
mumps_compiler = ["intel"]
mfem_compiler = ["intel"]
geopm_compiler = ["intel"]

[2.6.1]

# define patterns for a given arch in which to disable builds
skip_aarch=["-intel\\b","lustre-client","-impi\\b","-mvapich2\\b","likwid-gnu","likwid-arm1","geopm",
            "intel-compilers-devel","impi-devel","mvapich2","openblas-arm1"]
skip_x86  = ["-arm1"]

# define compiler/MPI families: first entry in list is defined to be parent in OBS
compiler_families=["gnu12","intel","arm1"]
mpi_families=["openmpi4","mpich","mvapich2","impi"]

standalone = ["meta-packages"]


[2.6.0]

# define patterns for a given arch in which to disable builds
skip_aarch=["-intel\\b","lustre-client","-impi\\b","-mvapich2\\b","likwid-gnu","likwid-arm1","geopm",
            "intel-compilers-devel","impi-devel","mvapich2","openblas-arm1"]
skip_x86  = ["-arm1"]

# define compiler/MPI families: first entry in list is defined to be parent in OBS
compiler_families=["gnu12","intel","arm1"]
mpi_families=["openmpi4","mpich","mvapich2","impi"]

standalone = ["docs","cmake","slurm","!docs","gnu-compilers","hwloc","lmod","lustre-client","pmix","valgrind",
              "easybuild","spack","ohpc-filesystem","libfabric","ucx","openpbs","python-Cython","papi",
              "impi-devel","meta-packages","paraver","test-suite","warewulf","intel-compilers-devel","arm-compilers-devel"]

compiler_dependent = ["gsl","hdf5","metis","mpich","mvapich2","numpy","likwid",
                      "openblas","openmpi","pdtoolkit","plasma","R","scotch","superlu"]
mpi_dependent = ["adios","boost","dimemas","extrae","fftw","geopm","hypre","imb","lmod-defaults","mfem",
                 "mpi4py","mumps","netcdf","netcdf-cxx","netcdf-fortran","omb","opencoarrays",
                 "petsc","phdf5","pnetcdf","ptscotch","scalapack","scalasca","scipy","scorep",
                 "sionlib","slepc","superlu_dist","tau","trilinos"]

openblas_compiler=["gnu12"]
R_compiler=["gnu12"]
opencoarrays_compiler=["gnu12"]
scipy_compiler=["gnu12","arm1"]
scipy_mpi=["openmpi4","mpich","mvapich2"]
lmod-defaults_mpi=["openmpi4","mvapich2","impi"]

[2.5.0]

# define patterns for a given arch in which to disable builds
skip_aarch=["-intel\\b","lustre-client","-impi\\b","-mvapich2\\b","likwid-gnu","likwid-arm1","geopm",
            "intel-compilers-devel","impi-devel","mvapich2","openblas-arm1"]
skip_x86  = ["-arm1"]

# define compiler/MPI families: first entry in list is defined to be parent in OBS
compiler_families=["gnu9","intel","arm1"]
mpi_families=["openmpi4","mpich","mvapich2","impi"]

standalone = ["slurm","docs","lustre-client"]
compiler_dependent = ["!gsl","!hdf5","!metis","!mpich","!mvapich2","!numpy","!likwid",
                      "!openblas","!openmpi","!pdtoolkit","!plasma","!R","!scotch","!superlu"]

[2.4.0]

# define patterns for a given arch in which to disable builds
skip_aarch=["-intel\\b","lustre-client","-impi\\b","-mvapich2\\b","likwid-gnu","likwid-arm1","geopm",
            "intel-compilers-devel","impi-devel","mvapich2","openblas-arm1"]
skip_x86  = ["-arm1"]

# define compiler/MPI families: first entry in list is defined to be parent in OBS
compiler_families=["gnu9","intel","arm1"]
mpi_families=["openmpi4","mpich","mvapich2","impi"]

standalone = ["arm-compilers-devel","cmake","docs","easybuild","gnu-compilers","hwloc",
              "intel-compilers-devel","impi-devel","msr-safe","libfabric","lmod","!llvm-compilers",
	      "lustre-client","!magpie","meta-packages","ohpc-filesystem","prun","slurm",
	      "spack","test-suite","ucx","valgrind"]
compiler_dependent = ["gsl","hdf5","!metis","mpich","mvapich2","!numpy","!likwid",
                      "!openblas","openmpi","!pdtoolkit","!plasma","R","!scotch","!superlu"]

# overdefault compiler families for any desired components
R_compiler=["gnu9"]
#gsl_compiler=["gnu9"]
openblas_compiler=["gnu9"]

mpi_dependent = ["boost","!mfem","netcdf","netcdf-fortran","omb","!opencoarrays",
                 "petsc","phdf5","pnetcdf","slepc","superlu_dist"]

opencoarrays_compiler=["gnu9"]

[2.2.0]

# define patterns for a given arch in which to disable builds
skip_aarch=["-intel\\b","lustre-client","-impi\\b","-mvapich2\\b","likwid-gnu","likwid-arm1","geopm",
            "intel-compilers-devel","impi-devel","mvapich2","openblas-arm1"]
skip_x86  = ["-arm1"]

# define compiler/MPI families: first entry in list is defined to be parent in OBS
compiler_families=["gnu9","intel","arm1"]
mpi_families=["openmpi4","mpich","mvapich2","impi"]

standalone = ["cmake","docs","easybuild","libfabric","llvm-compilers","lustre-client","magpie",
              "meta-packages","prun","slurm","test-suite","warewulf-vnfs","ucx","valgrind"]
compiler_dependent = ["!gsl","!hdf5","!metis","!mpich","mvapich2","!numpy","!likwid",
                      "!openblas","openmpi","!pdtoolkit","!plasma","!R","!scotch","!superlu"]

mpi_dependent = ["boost","mfem","opencoarrays","petsc","slepc"]

opencoarrays_compiler=["gnu9"]

[2.1.0]

# define patterns for a given arch in which to disable builds
skip_aarch=["-intel\\b","lustre-client","-impi\\b","-mvapich2\\b","likwid-gnu","likwid-arm1","geopm",
            "intel-compilers-devel","impi-devel","mvapich2","openblas-arm1"]
skip_x86  = ["-arm1"]

# define compiler/MPI families: first entry in list is defined to be parent in OBS
compiler_families=["gnu9","intel","arm1"]
mpi_families=["openmpi4","mpich","mvapich2","impi"]

standalone = ["cmake","docs","easybuild","libfabric","llvm-compilers","lustre-client","magpie",
              "meta-packages","prun","singularity","slurm","test-suite","warewulf-vnfs","ucx","valgrind"]
compiler_dependent = ["!gsl","!hdf5","!metis","!mpich","mvapich2","!numpy","!likwid",
                      "!openblas","openmpi","!pdtoolkit","!plasma","!R","!scotch","!superlu"]

mpi_dependent = ["boost","mfem","opencoarrays","petsc","slepc"]

opencoarrays_compiler=["gnu9"]

[2.0.0]

# define patterns for a given arch in which to disable builds
skip_aarch=["-intel\\b","lustre-client","-impi\\b","-mvapich2\\b","likwid-gnu","likwid-arm1","geopm",
            "intel-compilers-devel","impi-devel","mvapich2","openblas-arm1"]
skip_x86  = ["-arm1"]

# define compiler/MPI families: first entry in list is defined to be parent in OBS
compiler_families=["gnu9","intel","arm1"]
mpi_families=["openmpi4","mpich","mvapich2","impi"]

standalone = ["arm-compilers-devel","autoconf","automake","charliecloud","cmake","conman",
              "docs","examples","flex","genders","gnu-compilers","hwloc","intel-compilers-devel","libfabric","libtool",
	      "lmod","losf","lustre-client","meta-packages","mrsh","msr-safe","nhc",
	      "ohpc-filesystem","openpbs","papi","paraver","pdsh","pmix","prun","python-Cython",
	      "singularity","slurm","test-suite","ucx","valgrind"]
              

# define (compiler dependent) packages
compiler_dependent = ["gsl","hdf5","metis","mpich","mvapich2","numpy","likwid",
                      "openblas","openmpi","pdtoolkit","plasma","R","scotch","superlu"]

# overdefault compiler families for any desired components
R_compiler=["gnu9"]
#gsl_compiler=["gnu9"]
openblas_compiler=["gnu9"]
#numpy_compiler=["gnu9","arm1"]

opencoarrays_compiler=["gnu9"]

mpi_dependent = ["adios","boost","dimemas","extrae","fftw","geopm","hypre","imb","lmod-defaults",
                 "mpi4py","mumps","netcdf","netcdf-cxx","netcdf-fortran","omb","opencoarrays",
                 "petsc","phdf5","pnetcdf","ptscotch","scalapack","scalasca","scipy","scorep",
		 "sionlib","slepc","superlu_dist","tau","trilinos"]

fftw_compiler=["gnu9","arm1"]
#scalapack_compiler=["gnu9","arm1"]
scipy_compiler=["gnu9","arm1"]
scipy_mpi=["openmpi4","mpich","mvapich2"]
lmod-defaults_mpi=["openmpi4","mvapich2","impi"]
