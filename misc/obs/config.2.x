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
dev-tools     = ["autoconf","automake","cmake","easybuild","hwloc","libtool","mpi4py","numpy",
	         "scipy","spack","valgrind"]
distro-packages = ["python-Cython","flex"]
fs            = ["lustre-client"]
io-libs       = ["hdf5","netcdf","netcdf-cxx","netcdf-fortran","phdf5","pnetcdf","sionlib"]
runtimes      = ["singularity","ocr","charliecloud"]
rms           = ["slurm","openpbs","pmix","munge"]
serial-libs   = ["R","gsl","metis","openblas","plasma","scotch","superlu"]
parallel-libs = ["boost","fftw","hypre","mumps","opencoarrays","petsc","scalapack","ptscotch",
                 "slepc","superlu_dist","trilinos"]
provisioning  = ["warewulf-vnfs"]
perf-tools    = ["dimemas","extrae","geopm","imb","likwid","msr-safe","omb","papi","paraver","pdtoolkit",
                 "scalasca","scorep","tau"]

compiler-families=["gnu-compilers","intel-compilers-devel","arm-compilers-devel","llvm-compilers"]
mpi-families=["mpich","mvapich2","openmpi","libfabric","ucx"]

[2.1.0]

# define patterns for a given arch in which to disable builds
skip_aarch=["-intel\\b","lustre-client","-impi\\b","-mvapich2\\b","likwid-gnu","likwid-arm1","geopm",
            "intel-compilers-devel","impi-devel","mvapich2","openblas-arm1"]
skip_x86  = ["-arm1"]

# define compiler/MPI families: first entry in list is defined to be parent in OBS
compiler_families=["gnu9","intel","arm1"]
mpi_families=["openmpi4","mpich","mvapich2","impi"]

standalone = ["llvm-compilers","easybuild","warewulf-vnfs"]



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
