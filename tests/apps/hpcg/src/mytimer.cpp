
//@HEADER
// ***************************************************
//
// HPCG: High Performance Conjugate Gradient Benchmark
//
// Contact:
// Michael A. Heroux ( maherou@sandia.gov)
// Jack Dongarra     (dongarra@eecs.utk.edu)
// Piotr Luszczek    (luszczek@eecs.utk.edu)
//
// ***************************************************
//@HEADER

/////////////////////////////////////////////////////////////////////////

// Function to return time in seconds.
// If compiled with no flags, return CPU time (user and system).
// If compiled with -DWALL, returns elapsed time.

/////////////////////////////////////////////////////////////////////////

#ifndef HPCG_NO_MPI
#include <mpi.h>

double mytimer(void) {
  return MPI_Wtime();
}

#elif !defined(HPCG_NO_OPENMP)

// If this routine is compiled with HPCG_NO_MPI defined and not compiled with HPCG_NO_OPENMP then use the OpenMP timer
#include <omp.h>
double mytimer(void) {
  return omp_get_wtime();
}
#else

#include <cstdlib>
#include <sys/time.h>
#include <sys/resource.h>
double mytimer(void) {
  struct timeval tp;
  static long start=0, startu;
  if (!start) {
    gettimeofday(&tp, NULL);
    start = tp.tv_sec;
    startu = tp.tv_usec;
    return 0.0;
  }
  gettimeofday(&tp, NULL);
  return ((double) (tp.tv_sec - start)) + (tp.tv_usec-startu)/1000000.0 ;
}

#endif
