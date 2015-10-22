AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS  = -I m4
AM_MAKEFLAGS     = --no-print-directory
SUBDIRS          = 
TESTS            =

# Tests are broken up into those which require elevetated credentials
# versus those that run in normal userspace

if ROOT_ENABLED

if BOS_ENABLED
   SUBDIRS += bos
endif
if OOB_ENABLED
   SUBDIRS += oob
endif
if CLCK_ENABLED
   SUBDIRS += clck
endif

if LUSTRE_ENABLED
   SUBDIRS += lustre
endif

else # non root tests

if EASYBUILD_ENABLED
   SUBDIRS += dev-tools/easybuild
endif

if MODULES_ENABLED
   SUBDIRS += modules 
endif

if USERENV_ENABLED
   SUBDIRS += user-env
endif



if ITAC_ENABLED
   SUBDIRS += perf-tools/intel-itac
endif

if ADVISOR_ENABLED
   SUBDIRS += perf-tools/intel-advisor
endif

#----------------------------------------------------------------------
# run OHPC wrapper tests for embeded autotools project (e.g. those that
# have compiler and/or mpi dependencies)
#----------------------------------------------------------------------

if RMS_HARNESS_ENABLED
   TESTS += rms-harness/ohpc-tests/test_mpi_families
endif

if APPS_ENABLED
   TESTS += apps/miniFE/ohpc-tests/test_miniFE
   TESTS += apps/miniDFT/ohpc-tests/test_miniDFT
   TESTS += apps/hpcg/run
   TESTS += apps/prk/ohpc-tests/test_PRK
endif

if COMPILERS_ENABLED
   TESTS += compilers/ohpc-tests/test_compiler_families
endif

if VALGRIND_ENABLED
   TESTS += dev-tools/valgrind/ohpc-tests/test_compiler_families
endif

if R_ENABLED
   TESTS += dev-tools/R-base/ohpc-tests/test_compiler_families
endif

if MPIP_ENABLED
   TESTS += perf-tools/mpiP/ohpc-tests/test_mpiP
endif

if VTUNE_ENABLED
   TESTS += perf-tools/intel-vtune/ohpc-tests/test_mpi_families
endif

if FFTW_ENABLED
   TESTS += libs/fftw/ohpc-tests/test_mpi_families
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

if GSL_ENABLED
   TESTS += libs/gsl/ohpc-tests/test_gsl
endif

if IMB_ENABLED
   TESTS += libs/imb/ohpc-tests/test_imb
endif

if HDF5_ENABLED
   TESTS += libs/hdf5/ohpc-tests/test_hdf5
endif

if PHDF5_ENABLED
   TESTS += libs/phdf5/ohpc-tests/test_mpi_families
endif

if HYPRE_ENABLED
   TESTS += libs/hypre/ohpc-tests/test_hypre
endif

if INSPECTOR_ENABLED
   TESTS += dev-tools/intel-inspector/ohpc-tests/test_inspector
endif

if METIS_ENABLED
   TESTS += libs/metis/ohpc-tests/test_compiler_families
endif

if MUMPS_ENABLED
   TESTS += libs/mumps/ohpc-tests/test_mumps
endif

if NETCDF_ENABLED
   TESTS += libs/netcdf/ohpc-tests/test_mpi_families
endif

if OPENBLAS_ENABLED
   TESTS += libs/openblas/ohpc-tests/test_openblas
endif

if OOM_ENABLED
   TESTS += user-env-oom/ohpc-tests/test_compiler_families
endif

if PAPI_ENABLED
   TESTS += perf-tools/papi/ohpc-tests/test_compiler_families
endif

if PETSC_ENABLED
   TESTS += libs/petsc/ohpc-tests/test_petsc
endif

if MPI_ENABLED
   TESTS += mpi/ohpc-tests/test_mpi_families
endif

if NUMPY_ENABLED
   TESTS += dev-tools/numpy/ohpc-tests/test_numpy
endif

if SCALAPACK_ENABLED
   TESTS += libs/scalapack/ohpc-tests/test_scalapack
endif

if SCIPY_ENABLED
   TESTS += dev-tools/scipy/ohpc-tests/test_scipy
endif

if SUPERLU_ENABLED
   TESTS += libs/superlu/ohpc-tests/test_superlu
endif

if SUPERLU_DIST_ENABLED
   TESTS += libs/superlu_dist/ohpc-tests/test_superlu_dist
endif

if TAU_ENABLED
   TESTS += perf-tools/tau/ohpc-tests/test_tau
endif

if AUTOTOOLS_ENABLED
   TESTS += dev-tools/autotools/run
endif

if TBB_ENABLED
   TESTS += dev-tools/tbb/ohpc-tests/test_tbb
endif

if CILK_ENABLED
   TESTS += dev-tools/cilk/ohpc-tests/test_cilk
endif

if TRILINOS_ENABLED
   TESTS += libs/trilinos/ohpc-tests/test_trilinos
endif

endif
