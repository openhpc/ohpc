# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([test-suite], [1.0.0], [karl.w.schulz@intel.com])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([1.14])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AC_CANONICAL_HOST

AM_CONDITIONAL(HAVE_SHARED_CONDITIONAL,test 0)
AM_CONDITIONAL(DIRECT_VFD_CONDITIONAL,test 0)
AM_CONDITIONAL(BUILD_ALL_CONDITIONAL,test 0)

# Test user
AX_CHECK_USER([root])

AC_ARG_ENABLE([all],
	 [AS_HELP_STRING([--disable-all],[Disable all tests by default (default=no]))],
	 [],[enable_all=yes])

# Test harness includes support for performing a set of quick-running
# tests or a longer suite of tests. Default is to run the quick
# configuration, user can opt-in for longer tests.

AC_MSG_CHECKING([if requesting longer-running tests])
AC_ARG_ENABLE([long],
	 [AS_HELP_STRING([--enable-long],[Enable longer-running test content (default=no]))],
	 [],[enable_long=no])

if test x$enable_long = xyes ;then
   AC_MSG_RESULT([yes])
else
   AC_MSG_RESULT([no])
fi

# Use customizable test driver
#AC_SUBST([LOG_DRIVER],['$(SHELL) $(top_srcdir)/test-driver-cmt'])

#-----------------
# ROOT level tests
#-----------------

#------------------------------------------------------------------------------------------
# Administrator Environment
AC_ARG_ENABLE([admin],
   [AS_HELP_STRING([--enable-admin],[Enable administrator environment tests (default=yes, root only)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_admin,yes)])
AM_CONDITIONAL(ADMIN_ENABLED,test "x$enable_admin" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Base operating system 
AC_ARG_ENABLE([bos],
	 [AS_HELP_STRING([--enable-bos],[Enable bos tests (default=yes, root only)])],
	 [],[enable_bos=yes])
AM_CONDITIONAL(BOS_ENABLED,test "x$enable_bos" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Out-of-band
AC_ARG_ENABLE([oob],
	 [AS_HELP_STRING([--enable-oob],[Enable oob tests (default=yes, root only)])],
	 [],[enable_oob=yes])
AM_CONDITIONAL(OOB_ENABLED,test "x$enable_oob" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Cluster checker
AC_ARG_ENABLE([clck],
	 [AS_HELP_STRING([--enable-clck],[Enable clck tests (default=yes, root only)])],
	 [],[enable_clck=yes])
AM_CONDITIONAL(CLCK_ENABLED,test "x$enable_clck" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Lustre
AC_ARG_ENABLE([lustre],
	 [AS_HELP_STRING([--enable-lustre],[Enable Lustre utility tests (default=no, root only)])],
	 [],[enable_lustre=no])
AM_CONDITIONAL(LUSTRE_ENABLED,test "x$enable_lustre" = "xyes" ) 
#------------------------------------------------------------------------------------------
# spack
AC_ARG_ENABLE([spack],
	 [AS_HELP_STRING([--enable-spack],[Enable spack tests (default=yes, root only)])],
	 [],[enable_spack=yes])
AM_CONDITIONAL(SPACK_ENABLED,test "x$enable_spack" = "xyes" ) 

#-----------------
# User level tests
#-----------------



#------------------------------------------------------------------------------------------
# Compilers
AC_ARG_ENABLE([compilers],
   [AS_HELP_STRING([--enable-compilers],[Enable compiler toolchain tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_compilers,yes)])
AM_CONDITIONAL(COMPILERS_ENABLED,test "x$enable_compilers" = "xyes" ) 
#------------------------------------------------------------------------------------------
# R
AC_ARG_ENABLE([R],
   [AS_HELP_STRING([--enable-R],[Enable R tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_R,yes)])
AM_CONDITIONAL(R_ENABLED,test "x$enable_R" = "xyes" ) 
#------------------------------------------------------------------------------------------
# mpiP
AC_ARG_ENABLE([mpiP],
   [AS_HELP_STRING([--enable-mpiP],[Enable mpiP profiler tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_mpiP,yes)])
AM_CONDITIONAL(MPIP_ENABLED,test "x$enable_mpiP" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Valgrind
AC_ARG_ENABLE([valgrind],
   [AS_HELP_STRING([--enable-valgrind],[Enable valgrind tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_valgrind,yes)])
AM_CONDITIONAL(VALGRIND_ENABLED,test "x$enable_valgrind" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Intel-inspector
AC_ARG_ENABLE([inspector],
   [AS_HELP_STRING([--enable-inspector],[Enable Intel Inspector tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_inspector,yes)])
AM_CONDITIONAL(INSPECTOR_ENABLED,test "x$enable_inspector" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Intel-ITAC
AC_ARG_ENABLE([itac],
   [AS_HELP_STRING([--enable-itac],[Enable Intel Trace Analyzer/Collector tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_itac,yes)])
AM_CONDITIONAL(ITAC_ENABLED,test "x$enable_itac" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Intel-Vtune
AC_ARG_ENABLE([vtune],
   [AS_HELP_STRING([--enable-vtune],[Enable Intel Vtune Amplifier XE tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_vtune,yes)])
AM_CONDITIONAL(VTUNE_ENABLED,test "x$enable_vtune" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Intel-Advisor
AC_ARG_ENABLE([advisor],
   [AS_HELP_STRING([--enable-advisor],[Enable Intel Advisor XE tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_advisor,yes)])
AM_CONDITIONAL(ADVISOR_ENABLED,test "x$enable_advisor" = "xyes" ) 
#------------------------------------------------------------------------------------------
# MPI
AC_ARG_ENABLE([mpi],
   [AS_HELP_STRING([--enable-mpi],[Enable MPI compilation/execution tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_mpi,yes)])
AM_CONDITIONAL(MPI_ENABLED,test "x$enable_mpi" = "xyes" ) 
#------------------------------------------------------------------------------------------
# NetCDF
AC_ARG_ENABLE([netcdf],
   [AS_HELP_STRING([--enable-netcdf],[Enable NetCDF tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_netcdf,yes)])
AM_CONDITIONAL(NETCDF_ENABLED,test "x$enable_netcdf" = "xyes" ) 
#------------------------------------------------------------------------------------------
# HDF5
AC_ARG_ENABLE([hdf5],
   [AS_HELP_STRING([--enable-hdf5],[Enable HDF5 tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_hdf5,yes)])
AM_CONDITIONAL(HDF5_ENABLED,test "x$enable_hdf5" = "xyes" ) 
#------------------------------------------------------------------------------------------
# PHDF5
AC_ARG_ENABLE([phdf5],
   [AS_HELP_STRING([--enable-phdf5],[Enable PHDF5 tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_phdf5,yes)])
AM_CONDITIONAL(PHDF5_ENABLED,test "x$enable_phdf5" = "xyes" ) 
#------------------------------------------------------------------------------------------
# IMB
AC_ARG_ENABLE([imb],
   [AS_HELP_STRING([--enable-imb],[Enable Intel MPI Benchmarks tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_imb,yes)])
AM_CONDITIONAL(IMB_ENABLED,test "x$enable_imb" = "xyes" )
#------------------------------------------------------------------------------------------
# GSL
AC_ARG_ENABLE([gsl],
   [AS_HELP_STRING([--enable-gsl],[Enable GSL library tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_gsl,yes)])
AM_CONDITIONAL(GSL_ENABLED,test "x$enable_gsl" = "xyes" )
#------------------------------------------------------------------------------------------
# FFTW
AC_ARG_ENABLE([fftw],
   [AS_HELP_STRING([--enable-fftw],[Enable FFTW library tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_fftw,yes)])
AM_CONDITIONAL(FFTW_ENABLED,test "x$enable_fftw" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Adios 
AC_ARG_ENABLE([adios],
   [AS_HELP_STRING([--enable-adios],[Enable Adios tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_adios,yes)])
AM_CONDITIONAL(ADIOS_ENABLED,test "x$enable_adios" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Boost
AC_ARG_ENABLE([boost],
   [AS_HELP_STRING([--enable-boost],[Enable BOOST tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_boost,yes)])
AM_CONDITIONAL(BOOST_ENABLED,test "x$enable_boost" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Boost (MPI)
AC_ARG_ENABLE([boost-mpi],
   [AS_HELP_STRING([--enable-boost-mpi],[Enable BOOST library MPI tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_boost_mpi,yes)])
AM_CONDITIONAL(BOOST_MPI_ENABLED,test "x$enable_boost_mpi" = "xyes" ) 
#------------------------------------------------------------------------------------------
# EasyBuild
AC_ARG_ENABLE([easybuild],
   [AS_HELP_STRING([--enable-easybuild],[Enable EasyBuild tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_easybuild,yes)])
AM_CONDITIONAL(EASYBUILD_ENABLED,test "x$enable_easybuild" = "xyes" ) 
#------------------------------------------------------------------------------------------
# HYPRE
AC_ARG_ENABLE([hypre],
   [AS_HELP_STRING([--enable-hypre],[Enable HYPRE tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_hypre,yes)])
AM_CONDITIONAL(HYPRE_ENABLED,test "x$enable_hypre" = "xyes" ) 
#------------------------------------------------------------------------------------------
# MODULES
AC_ARG_ENABLE([modules],
   [AS_HELP_STRING([--enable-modules],[Enable Module tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_modules,yes)])
   AM_CONDITIONAL(MODULES_ENABLED,test "x$enable_modules" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Numpy
AC_ARG_ENABLE([numpy],
   [AS_HELP_STRING([--enable-numpy],[Enable numpy tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_numpy,yes)])
AM_CONDITIONAL(NUMPY_ENABLED,test "x$enable_numpy" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Metis
AC_ARG_ENABLE([metis],
   [AS_HELP_STRING([--enable-metis],[Enable metis tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_metis,yes)])
AM_CONDITIONAL(METIS_ENABLED,test "x$enable_metis" = "xyes" ) 
#------------------------------------------------------------------------------------------
# MUMPS
AC_ARG_ENABLE([mumps],
   [AS_HELP_STRING([--enable-mumps],[Enable MUMPS tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_mumps,yes)])
AM_CONDITIONAL(MUMPS_ENABLED,test "x$enable_mumps" = "xyes" )
#------------------------------------------------------------------------------------------
# OOM
AC_ARG_ENABLE([oom],
   [AS_HELP_STRING([--enable-oom],[Enable out-of-memory tests under resource manager (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_oom,yes)])
AM_CONDITIONAL(OOM_ENABLED,test "x$enable_oom" = "xyes" )
#------------------------------------------------------------------------------------------
# OCR
AC_ARG_ENABLE([ocr],
   [AS_HELP_STRING([--enable-ocr],[Enable OCR tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_ocr,yes)])
AM_CONDITIONAL(OCR_ENABLED,test "x$enable_ocr" = "xyes" )
#------------------------------------------------------------------------------------------
# OPENBLAS
AC_ARG_ENABLE([openblas],
   [AS_HELP_STRING([--enable-openblas],[Enable OPENBLAS tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_openblas,yes)])
AM_CONDITIONAL(OPENBLAS_ENABLED,test "x$enable_openblas" = "xyes" )
#------------------------------------------------------------------------------------------
# Papi
AC_ARG_ENABLE([papi],
   [AS_HELP_STRING([--enable-papi],[Enable papi tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_papi,yes)])
AM_CONDITIONAL(PAPI_ENABLED,test "x$enable_papi" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Petsc
AC_ARG_ENABLE([petsc],
   [AS_HELP_STRING([--enable-petsc],[Enable petsc tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_petsc,yes)])
AM_CONDITIONAL(PETSC_ENABLED,test "x$enable_petsc" = "xyes" ) 
#------------------------------------------------------------------------------------------
# RMS harness
AC_ARG_ENABLE([rms-harness],
   [AS_HELP_STRING([--enable-rms-harness],[Enable RMS harness tests(default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_rms_harness,yes)])
AM_CONDITIONAL(RMS_HARNESS_ENABLED,test "x$enable_rms_harness" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Scalapack
AC_ARG_ENABLE([scalapack],
   [AS_HELP_STRING([--enable-scalapack],[Enable scalapack tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_scalapack,yes)])
AM_CONDITIONAL(SCALAPACK_ENABLED,test "x$enable_scalapack" = "xyes" ) 
#------------------------------------------------------------------------------------------
# Scipy
AC_ARG_ENABLE([scipy],
   [AS_HELP_STRING([--enable-scipy],[Enable scipy tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_scipy,yes)])
AM_CONDITIONAL(SCIPY_ENABLED,test "x$enable_scipy" = "xyes" ) 
#------------------------------------------------------------------------------------------
# SuperLU
AC_ARG_ENABLE([superlu],
   [AS_HELP_STRING([--enable-superlu],[Enable superlu tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_superlu,yes)])
AM_CONDITIONAL(SUPERLU_ENABLED,test "x$enable_superlu" = "xyes" ) 
#------------------------------------------------------------------------------------------
# SuperLU_dist
AC_ARG_ENABLE([superlu_dist],
   [AS_HELP_STRING([--enable-superlu_dist],[Enable superlu_dist tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_superlu_dist,yes)])
AM_CONDITIONAL(SUPERLU_DIST_ENABLED,test "x$enable_superlu_dist" = "xyes" ) 
#------------------------------------------------------------------------------------------
# TAU
AC_ARG_ENABLE([tau],
   [AS_HELP_STRING([--enable-tau],[Enable tau tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_tau,yes)])
AM_CONDITIONAL(TAU_ENABLED,test "x$enable_tau" = "xyes" )
#------------------------------------------------------------------------------------------
# Autotools
AC_ARG_ENABLE([autotools],
   [AS_HELP_STRING([--enable-autotools],[Enable autotools tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_autotools,yes)])
AM_CONDITIONAL(AUTOTOOLS_ENABLED,test "x$enable_autotools" = "xyes" )
#------------------------------------------------------------------------------------------
# TBB
AC_ARG_ENABLE([tbb],
   [AS_HELP_STRING([--enable-tbb],[Enable tbb tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_tbb,yes)])
AM_CONDITIONAL(TBB_ENABLED,test "x$enable_tbb" = "xyes" )
#------------------------------------------------------------------------------------------
# CILK
AC_ARG_ENABLE([cilk],
   [AS_HELP_STRING([--enable-cilk],[Enable cilk tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_cilk,yes)])
AM_CONDITIONAL(CILK_ENABLED,test "x$enable_cilk" = "xyes" )
#------------------------------------------------------------------------------------------
# Trilinos
AC_ARG_ENABLE([trilinos],
   [AS_HELP_STRING([--enable-trilinos],[Enable trilinos tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_trilinos,yes)])
AM_CONDITIONAL(TRILINOS_ENABLED,test "x$enable_trilinos" = "xyes" )
#------------------------------------------------------------------------------------------
# User Environment
AC_ARG_ENABLE([munge],
   [AS_HELP_STRING([--enable-munge],[Enable munge authentication tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_munge,yes)])
AM_CONDITIONAL(USERENV_ENABLED,test "x$enable_munge" = "xyes" ) 
#------------------------------------------------------------------------------------------
# MiniFE
AC_ARG_ENABLE([minife],
   [AS_HELP_STRING([--enable-minife],[Enable MiniFE tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_minife,yes)])
AM_CONDITIONAL(MINIFE_ENABLED,test "x$enable_minife" = "xyes" )
#------------------------------------------------------------------------------------------
# MiniDFT
AC_ARG_ENABLE([minidft],
   [AS_HELP_STRING([--enable-minidft],[Enable MiniDFT tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_minidft,yes)])
AM_CONDITIONAL(MINIDFT_ENABLED,test "x$enable_minidft" = "xyes" )
#------------------------------------------------------------------------------------------
# HPCG
AC_ARG_ENABLE([hpcg],
   [AS_HELP_STRING([--enable-hpcg],[Enable HPCG tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_hpcg,yes)])
AM_CONDITIONAL(HPCG_ENABLED,test "x$enable_hpcg" = "xyes" )
#------------------------------------------------------------------------------------------
# PRK
AC_ARG_ENABLE([prk],
   [AS_HELP_STRING([--enable-prk],[Enable PRK tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=long,enable_prk,yes)])
AM_CONDITIONAL(PRK_ENABLED,test "x$enable_prk" = "xyes" )
#------------------------------------------------------------------------------------------
# PACKAGING
AC_ARG_ENABLE([packaging],
   [AS_HELP_STRING([--enable-packaging],[Enable Packaging tests (default=yes)])],[],
   [AX_OPTION_DEFAULT(type=short,enable_packaging,yes)])
AM_CONDITIONAL(PACKAGING_ENABLED,test "x$enable_packaging" = "xyes" )
#------------------------------------------------------------------------------------------

AC_OUTPUT( Makefile 
	   admin/Makefile 
	   user-env/Makefile 
	   bos/Makefile
	   perf-tools/intel-itac/Makefile
	   perf-tools/intel-itac/tests/Makefile
	   perf-tools/intel-advisor/Makefile
	   perf-tools/intel-advisor/tests/Makefile
       perf-tools/tau/Makefile
	   oob/Makefile
	   clck/Makefile
	   lustre/Makefile
       dev-tools/easybuild/Makefile
       admin/spack/Makefile
       dev-tools/numpy/Makefile
       dev-tools/numpy/tests/Makefile
       dev-tools/scipy/Makefile
       dev-tools/scipy/tests/Makefile
       dev-tools/tbb/Makefile
       dev-tools/tbb/tests/Makefile
       dev-tools/cilk/Makefile
       dev-tools/R-base/Makefile
       modules/Makefile)

echo
echo '--------------------------------------------- SUMMARY ---------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo 
echo Build user.................... : $USER
echo Build host.................... : ${ac_hostname}
echo Configure date................ : `date +'%F %H:%M'`
echo Build architecture............ : ${host}
if test "x$enable_long" = "xyes" ; then
   echo Test suite configuration...... : long
else
   echo Test suite configuration...... : short
fi
echo 
echo Submodule Configuration:
echo 
if test "x$ROOT_ENABLED" = "x1" ; then
    if test "x$enable_bos" = "xyes"; then
       echo '    'Base operating system..... : enabled
    else 
       echo '    'Base operating system..... : disabled
    fi
    if test "x$enable_oob" = "xyes"; then
       echo '    'Out of band tools......... : enabled
    else 
       echo '    'Out of band tools......... : disabled
    fi
    if test "x$enable_hardware" = "xyes"; then
       echo '    'Hardware benchmarks....... : enabled	
    else 
       echo '    'Hardware benchmarks....... : disabled
    fi
    if test "x$enable_clck" = "xyes"; then
       echo '    'Cluster checker........... : enabled
    else 
       echo '    'Cluster checker........... : disabled
    fi
    if test "x$enable_lustre" = "xyes"; then
       echo '    'Lustre client............. : enabled
    else
       echo '    'Lustre client............. : disabled
    fi
    if test "x$enable_spack" = "xyes"; then
       echo '    'Spack..................... : enabled
    else
       echo '    'Spack..................... : disabled
    fi

else

echo    User Environment:
    if test "x$enable_packaging" = "xyes"; then
       echo '    'Packaging tests........... : enabled 
    else   
       echo '    'Packaging tests........... : disabled 
    fi

    if test "x$enable_rms_harness" = "xyes"; then
       echo '    'RMS test harness.......... : enabled 
    else   
       echo '    'RMS test harness.......... : disabled 
    fi

    if test "x$enable_munge" = "xyes"; then
       echo '    'Munge..................... : enabled
    else
       echo '    'Munge..................... : disabled
    fi

    if test "x$enable_compilers" = "xyes"; then
       echo '    'Compilers................. : enabled
    else
       echo '    'Compilers................. : disabled
    fi

    if test "x$enable_mpi" = "xyes"; then
       echo '    'MPI....................... : enabled
    else
       echo '    'MPI....................... : disabled
    fi

    if test "x$enable_modules" = "xyes"; then
       echo '    'Modules................... : enabled
    else
       echo '    'Modules................... : disabled
    fi

    if test "x$enable_oom" = "xyes"; then
       echo '    'OOM....................... : enabled
    else
       echo '    'OOM....................... : disabled
    fi

echo    Dev Tools:

    if test "x$enable_autotools" = "xyes"; then
       echo '    'Autotools................. : enabled
    else
       echo '    'Autotools................. : disabled
    fi

    if test "x$enable_easybuild" = "xyes"; then
       echo '    'EasyBuild................. : enabled
    else
       echo '    'EasyBuild................. : disabled
    fi

    if test "x$enable_inspector" = "xyes"; then
       echo '    'Intel Inspector........... : enabled
    else
       echo '    'Intel Inspector........... : disabled
    fi

    if test "x$enable_valgrind" = "xyes"; then
       echo '    'Valgrind.................. : enabled
    else
       echo '    'Valgrind.................. : disabled
    fi

    if test "x$enable_R" = "xyes"; then
       echo '    'R base package............ : enabled
    else
       echo '    'R base package............ : disabled
    fi

    if test "x$enable_tbb" = "xyes"; then
       echo '    'TBB....................... : enabled
    else
       echo '    'TBB....................... : disabled
    fi

    if test "x$enable_cilk" = "xyes"; then
       echo '    'CILK...................... : enabled
    else
       echo '    'CILK...................... : disabled
    fi

echo    Performance Tools:

    if test "x$enable_mpiP" = "xyes"; then
       echo '    'mpiP Profiler........ .... : enabled
    else
       echo '    'mpiP Profiler............. : disabled
    fi

    if test "x$enable_advisor" = "xyes"; then
       echo '    'Intel Advisor XE.......... : enabled
    else
       echo '    'Intel Advisor XE.......... : disabled
    fi

    if test "x$enable_itac" = "xyes"; then
       echo '    'Intel Trace Analyzer...... : enabled
    else
       echo '    'Intel Trace Analyzer...... : disabled
    fi

    if test "x$enable_vtune" = "xyes"; then
       echo '    'Intel Vtune Amplifier..... : enabled
    else
       echo '    'Intel Vtune Amplifier..... : disabled
    fi

    if test "x$enable_papi" = "xyes"; then
       echo '    'Papi...................... : enabled
    else
       echo '    'Papi...................... : disabled
    fi

    if test "x$enable_tau" = "xyes"; then
       echo '    'TAU....................... : enabled
    else
       echo '    'TAU....................... : disabled
    fi

echo    Libraries:

    if test "x$enable_adios" = "xyes"; then
       echo '    'Adios .................... : enabled
    else
       echo '    'Adios .................... : disabled
    fi

    if test "x$enable_boost" = "xyes"; then
       echo '    'Boost .................... : enabled
    else
       echo '    'Boost .................... : disabled
    fi

    if test "x$enable_boost_mpi" = "xyes"; then
       echo '    'Boost MPI................. : enabled
    else
       echo '    'Boost MPI................. : disabled
    fi

    if test "x$enable_fftw" = "xyes"; then
       echo '    'FFTW...................... : enabled
    else
       echo '    'FFTW...................... : disabled
    fi

    if test "x$enable_gsl" = "xyes"; then
       echo '    'GSL....................... : enabled
    else
       echo '    'GSL....................... : disabled
    fi

    if test "x$enable_hdf5" = "xyes"; then
       echo '    'HDF5...................... : enabled
    else
       echo '    'HDF5...................... : disabled
    fi

    if test "x$enable_hypre" = "xyes"; then
       echo '    'HYPRE..................... : enabled
    else
       echo '    'HYPRE..................... : disabled
    fi

    if test "x$enable_imb" = "xyes"; then
       echo '    'IMB....................... : enabled
    else
       echo '    'IMB....................... : disabled
    fi

    if test "x$enable_metis" = "xyes"; then
       echo '    'Metis..................... : enabled
    else
       echo '    'Metis..................... : disabled
    fi

    if test "x$enable_mumps" = "xyes"; then
       echo '    'MUMPS..................... : enabled
    else
       echo '    'MUMPS..................... : disabled
    fi

    if test "x$enable_netcdf" = "xyes"; then
       echo '    'NetCDF.................... : enabled
    else
       echo '    'NetCDF.................... : disabled
    fi

    if test "x$enable_numpy" = "xyes"; then
       echo '    'Numpy..................... : enabled
    else
       echo '    'Numpy..................... : disabled
    fi

    if test "x$enable_ocr" = "xyes"; then
       echo '    'OCR....................... : enabled
    else
       echo '    'OCR....................... : disabled
    fi

    if test "x$enable_openblas" = "xyes"; then
       echo '    'OPENBLAS.................. : enabled
    else
       echo '    'OPENBLAS.................. : disabled
    fi

    if test "x$enable_petsc" = "xyes"; then
       echo '    'PETSc..................... : enabled
    else
       echo '    'PETSc..................... : disabled
    fi

    if test "x$enable_phdf5" = "xyes"; then
       echo '    'PHDF5..................... : enabled
    else
       echo '    'PHDF5..................... : disabled
    fi

    if test "x$enable_scalapack" = "xyes"; then
       echo '    'ScaLAPACK................. : enabled
    else
       echo '    'ScaLAPACK................. : disabled
    fi

    if test "x$enable_scipy" = "xyes"; then
       echo '    'Scipy..................... : enabled
    else
       echo '    'Scipy..................... : disabled
    fi

    if test "x$enable_superlu" = "xyes"; then
       echo '    'Superlu................... : enabled
    else
       echo '    'Superlu................... : disabled
    fi

    if test "x$enable_superlu_dist" = "xyes"; then
       echo '    'Superlu_dist.............. : enabled
    else
       echo '    'Superlu_dist.............. : disabled
    fi

    if test "x$enable_trilinos" = "xyes"; then
       echo '    'Trilinos ................. : enabled
    else
       echo '    'Trilinos.................. : disabled
    fi

echo    Apps:

    if test "x$enable_minife" = "xyes"; then
       echo '    'MiniFE.................... : enabled
    else
       echo '    'MiniFE.................... : disabled
    fi

    if test "x$enable_minidft" = "xyes"; then
       echo '    'MiniDFT................... : enabled
    else
       echo '    'MiniDFT................... : disabled
    fi

    if test "x$enable_hpcg" = "xyes"; then
       echo '    'HPCG...................... : enabled
    else
       echo '    'HPCG...................... : disabled
    fi

    if test "x$enable_prk" = "xyes"; then
       echo '    'PRK....................... : enabled
    else
       echo '    'PRK....................... : disabled
    fi

fi
echo '---------------------------------------------------------------------------------------------------'
