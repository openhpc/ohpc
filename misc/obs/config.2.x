# -*-sh-*-

[global]
github_url = "git@github.com:openhpc/ohpc.git"
service_template=service.file
link_compiler_template=link_compiler
link_mpi_template=link_mpi

override_templates=templates
dry_run=True  # do not make changes to OBS, just do a dry-run

[groups]
# used to identify where components reside in ohpc Github repo
admin         = ["clustershell","conman","docs","examples","ganglia","genders","lmod-defaults","lmod",
	         "losf","meta-packages","mrsh","nagios","nagios-plugins","ndoutils","nhc","nrpe",
		 "ohpc-filesystem","ohpc-release",
		 "pdsh","prun","test-suite"]
dev-tools     = ["autoconf","automake","cmake","easybuild","hwloc","libtool","python-mpi4py","python-numpy",
	         "python-scipy","spack","valgrind"]
io-libs       = ["hdf5","netcdf","netcdf-fortran","phdf5","pnetcdf","sionlib"]
runtimes      = ["singularity","ocr","charliecloud"]
rms           = ["slurm","pbspro","pmix","munge"]
serial-libs   = ["R","gsl","metis","openblas","plasma","superlu"]
parallel-libs = ["boost","fftw","hypre","mumps","opencoarrays","petsc","scalapack","scotch","slepc","superlu_dist"]
perf-tools    = ["dimemas","extrae","geopm","likwid","omb","papi","scorep","tau"]

compiler-families=["gnu-compilers","intel-compilers-devel","arm-compilers-devel"]
mpi-families=["mpich","mvapich2","openmpi"]


[2.0.0]

# define patterns for a given arch in which to disable builds
skip_aarch=["-intel\\b","lustre-client","-impi\\b","-mvapich2\\b","likwid-gnu","geopm",
            "intel-compilers-devel","impi-devel","mvapich2","openblas-arm1"]
skip_x86  = ["-arm1"]

# define compiler/MPI families: first entry in list is defined to be parent in OBS
compiler_families=["gnu9","intel","arm1"]
mpi_families=["openmpi4","mpich","mvapich2","impi"]


standalone = ["arm-compilers-devel","autoconf","automake","clustershell","cmake","conman",
              "docs","examples","genders","gnu-compilers","hwloc","intel-compilers-devel","libtool",
	      "lmod","losf","mrsh","ohpc-filesystem","papi","pbspro","pdsh","pmix",
	      "prun","slurm","test-suite","valgrind"]
              

# define (compiler dependent) packages
compiler_dependent = ["gsl","hdf5","metis","mpich","mvapich2","!likwid",
                      "openblas","openmpi","plasma","R","scotch","superlu"]

# overdefault compiler families for any desired components
R_compiler=["gnu9"]
#gsl_compiler=["gnu9"]
openblas_compiler=["gnu9"]

#opencoarrays_compiler=["gnu9"]

mpi_dependent = ["boost","dimemas","extrae","fftw","!geopm","hypre","mumps","!opencoarrays",
                 "omb","petsc","!phdf5","scalapack","!scorep","sionlib","!slepc"]

fftw_compiler=["gnu9","arm1"]
scalapack_compiler=["gnu9","arm1"]
