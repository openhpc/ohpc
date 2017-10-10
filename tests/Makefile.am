AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS  = -I m4
AM_MAKEFLAGS     = --no-print-directory
SUBDIRS          = 
TESTS            =

# Tests are broken up into those which require elevetated credentials
# versus those that run in normal userspace

if ROOT_ENABLED

if ADMIN_ENABLED
   SUBDIRS += admin
endif

if BEEGFS_ENABLED
   SUBDIRS += fs/beegfs
endif

if BOS_ENABLED
   SUBDIRS += bos
endif

if OPA_FABRIC_ENABLED
   SUBDIRS += fabric
endif

if IB_FABRIC_ENABLED
   SUBDIRS += fabric
endif


if OOB_ENABLED
   SUBDIRS += oob
endif

if SINGULARITY_ENABLED
   SUBDIRS += runtimes/singularity
endif

if SPACK_ENABLED
   SUBDIRS += admin/spack
endif


else # non root tests

if EASYBUILD_ENABLED
   SUBDIRS += dev-tools/easybuild
endif

if LUSTRE_ENABLED
   SUBDIRS += fs/lustre
endif

if MODULES_ENABLED
   SUBDIRS += modules 
endif

if USERENV_ENABLED
   SUBDIRS += user-env
endif

#----------------------------------------------------------------------
# run OHPC wrapper tests for embeded autotools project (e.g. those that
# have compiler and/or mpi dependencies)
#----------------------------------------------------------------------

if HPCG_ENABLED
   TESTS += apps/hpcg/run
endif

if MINIDFT_ENABLED
   TESTS += apps/miniDFT/ohpc-tests/test_mpi_families
endif

if MINIFE_ENABLED
   TESTS += apps/miniFE/ohpc-tests/test_mpi_families
endif

if PRK_ENABLED
   TESTS += apps/prk/ohpc-tests/test_mpi_families
endif

if COMPILERS_ENABLED
   TESTS += compilers/ohpc-tests/test_compiler_families
endif

if AUTOTOOLS_ENABLED
   TESTS += dev-tools/autotools/run
endif

if CMAKE_ENABLED
   TESTS += dev-tools/cmake/run
endif

if CILK_ENABLED
   TESTS += dev-tools/cilk/ohpc-tests/test_compiler_families
endif

if HWLOC_ENABLED
   TESTS += dev-tools/hwloc/ohpc-tests/test_compiler_families
endif

if MPI4PY_ENABLED
   TESTS += dev-tools/mpi4py/ohpc-tests/test_mpi_families
endif

if NUMPY_ENABLED
   TESTS += dev-tools/numpy/ohpc-tests/test_mpi_families
endif

if SCIPY_ENABLED
   TESTS += dev-tools/scipy/ohpc-tests/test_mpi_families
endif

if TBB_ENABLED
   TESTS += dev-tools/tbb/ohpc-tests/test_compiler_families
endif

if VALGRIND_ENABLED
   TESTS += dev-tools/valgrind/ohpc-tests/test_compiler_families
endif

if ADIOS_ENABLED
   TESTS += libs/adios/ohpc-tests/test_mpi_families
endif

if BOOST_ENABLED
   TESTS += libs/boost/ohpc-tests/test_mpi_families
endif

if BOOST_MPI_ENABLED
   TESTS += libs/boost-mpi/ohpc-tests/test_mpi_families
endif

if FFTW_ENABLED
   TESTS += libs/fftw/ohpc-tests/test_mpi_families
endif

if GSL_ENABLED
   TESTS += libs/gsl/ohpc-tests/test_compiler_families
endif

if HDF5_ENABLED
   TESTS += libs/hdf5/ohpc-tests/test_compiler_families
endif

if HYPRE_ENABLED
   TESTS += libs/hypre/ohpc-tests/test_mpi_families
endif

if MFEM_ENABLED
   TESTS += libs/mfem/ohpc-tests/test_mpi_families
endif

if METIS_ENABLED
   TESTS += libs/metis/ohpc-tests/test_compiler_families
endif

if MUMPS_ENABLED
   TESTS += libs/mumps/ohpc-tests/test_mpi_families
endif

if NETCDF_ENABLED
   TESTS += libs/netcdf/ohpc-tests/test_mpi_families
endif

if OPENBLAS_ENABLED
   TESTS += libs/openblas/ohpc-tests/test_compiler_families
endif

if PETSC_ENABLED
   TESTS += libs/petsc/ohpc-tests/test_mpi_families
endif

if PHDF5_ENABLED
   TESTS += libs/phdf5/ohpc-tests/test_mpi_families
endif

if PLASMA_ENABLED
   TESTS += libs/plasma/ohpc-tests/test_compiler_families
endif

if PNETCDF_ENABLED
   TESTS += libs/pnetcdf/ohpc-tests/test_mpi_families
endif

if PTSCOTCH_ENABLED
   TESTS += libs/ptscotch/ohpc-tests/test_mpi_families
endif

if R_ENABLED
   TESTS += libs/R/ohpc-tests/test_compiler_families
endif

if SCALAPACK_ENABLED
   TESTS += libs/scalapack/ohpc-tests/test_mpi_families
endif

if SCOTCH_ENABLED
   TESTS += libs/scotch/ohpc-tests/test_compiler_families
endif

if SLEPC_ENABLED
   TESTS += libs/slepc/ohpc-tests/test_mpi_families
endif

if SUPERLU_ENABLED
   TESTS += libs/superlu/ohpc-tests/test_compiler_families
endif

if SUPERLU_DIST_ENABLED
   TESTS += libs/superlu_dist/ohpc-tests/test_mpi_families
endif

if TRILINOS_ENABLED
   TESTS += libs/trilinos/ohpc-tests/test_mpi_families
endif

if MPI_ENABLED
   TESTS += mpi/ohpc-tests/test_mpi_families
endif

if IMB_ENABLED
   TESTS += perf-tools/imb/ohpc-tests/test_mpi_families
endif

if GEOPM_ENABLED
   TESTS += perf-tools/geopm/ohpc-tests/test_mpi_families
endif

if LIKWID_ENABLED
   TESTS += perf-tools/likwid/ohpc-tests/test_compiler_families
endif

if MPIP_ENABLED
   TESTS += perf-tools/mpiP/ohpc-tests/test_mpi_families
endif

if PAPI_ENABLED
   TESTS += perf-tools/papi/ohpc-tests/test_compiler_families
endif

if SCALASCA_ENABLED
   TESTS += perf-tools/scalasca/ohpc-tests/test_mpi_families
endif

if TAU_ENABLED
   TESTS += perf-tools/tau/ohpc-tests/test_mpi_families
endif

if OCR_ENABLED
   TESTS += runtimes/ocr/ohpc-tests/test_compiler_families
endif

if RMS_HARNESS_ENABLED
   TESTS += rms-harness/ohpc-tests/test_mpi_families
endif

if OOM_ENABLED
   TESTS += user-env-oom/ohpc-tests/test_compiler_families
endif

endif
