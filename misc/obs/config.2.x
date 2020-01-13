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
io-libs       = ["hdf5","netcdf","netcdf-fortran","phdf5","pnetcdf"]
runtimes      = ["singularity","ocr","charliecloud"]
rms           = ["slurm","pbspro","pmix"]
serial-libs   = ["R","openblas","plasma"]
parallel-libs = ["boost","hypre","mumps","opencoarrays","petsc","slepc","superlu_dist"]
perf-tools    = ["dimemas","extrae","geopm","likwid","omb","papi","scorep","tau"]

compiler-families=["gnu-compilers","intel-compilers-devel"]
mpi-families=["mpich","mvapich2","openmpi"]


[2.0.0]

# define patterns for a given arch in which to disable builds
skip_aarch=["-intel\\b","lustre-client","-impi\\b","-mvapich2\\b","likwid-gnu","geopm"]
# define compiler/MPI families: first entry in list is defined to be parent in OBS
compiler_families=["gnu8","intel"]
mpi_families=["openmpi3","mpich","mvapich2","impi"]


standalone = ["ohpc-filesystem","gnu-compilers"]

# define (compiler dependent) packages
#compiler_dependent = ["!hdf5","!openmpi","!mvapich2","!likwid","!plasma","!R"]

# overdefault compiler families for any desired components
#R_compiler=["gnu8"]
#opencoarrays_compiler=["gnu8"]

#mpi_dependent = ["!boost","!dimemas","!extrae","!geopm","!mumps","!opencoarrays","!omb","!petsc","!phdf5","!scorep","!slepc"]
