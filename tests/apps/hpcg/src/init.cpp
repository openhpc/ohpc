
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

#ifndef HPCG_NO_MPI
#include <mpi.h>
#endif

#ifndef HPCG_NO_OPENMP
#include <omp.h>
#endif

#ifdef _WIN32
const char* NULLDEVICE="nul";
#else
const char* NULLDEVICE="/dev/null";
#endif

#include <ctime>
#include <cstdio>
#include <cstring>

#include <fstream>
#include <iostream>

#include "hpcg.hpp"

#include "ReadHpcgDat.hpp"

std::ofstream HPCG_fout; //!< output file stream for logging activities during HPCG run

static int
startswith(const char * s, const char * prefix) {
  size_t n = strlen( prefix );
  if (strncmp( s, prefix, n ))
    return 0;
  return 1;
}

/*!
  Initializes an HPCG run by obtaining problem parameters (from a file or
  command line) and then broadcasts them to all nodes. It also initializes
  login I/O streams that are used throughout the HPCG run. Only MPI rank 0
  performs I/O operations.

  The function assumes that MPI has already been initialized for MPI runs.

  @param[in] argc_p the pointer to the "argc" parameter passed to the main() function
  @param[in] argv_p the pointer to the "argv" parameter passed to the main() function
  @param[out] params the reference to the data structures that is filled the basic parameters of the run

  @return returns 0 upon success and non-zero otherwise

  @see HPCG_Finalize
*/
int
HPCG_Init(int * argc_p, char ** *argv_p, HPCG_Params & params) {
  int argc = *argc_p;
  char ** argv = *argv_p;
  char fname[80];
  int i, j, iparams[4];
  char cparams[4][6] = {"--nx=", "--ny=", "--nz=", "--rt="};
  time_t rawtime;
  tm * ptm;

  // Initialize iparams
  for (i = 0; i < 4; ++i) iparams[i] = 0;

  /* for sequential and some MPI implementations it's OK to read first three args */
  for (i = 0; i < 4; ++i)
    if (argc <= i+1 || sscanf(argv[i+1], "%d", iparams+i) != 1 || iparams[i] < 10) iparams[i] = 0;

  /* for some MPI environments, command line arguments may get complicated so we need a prefix */
  for (i = 1; i <= argc && argv[i]; ++i)
    for (j = 0; j < 4; ++j)
      if (startswith(argv[i], cparams[j]))
        if (sscanf(argv[i]+strlen(cparams[j]), "%d", iparams+j) != 1 || iparams[j] < 10) iparams[j] = 0;

  // Check if --rt was specified on the command line
  int * rt  = iparams+3;  // Assume runtime was not specified and will be read from the hpcg.dat file
  if (! iparams[3]) rt = 0; // If --rt was specified, we already have the runtime, so don't read it from file
  if (! iparams[0] && ! iparams[1] && ! iparams[2]) { /* no geometry arguments on the command line */
    ReadHpcgDat(iparams, rt);
  }

  // Check for small or unspecified nx, ny, nz values
  // If any dimension is less than 16, make it the max over the other two dimensions, or 16, whichever is largest
  for (i = 0; i < 3; ++i) {
    if (iparams[i] < 16)
      for (j = 1; j <= 2; ++j)
        if (iparams[(i+j)%3] > iparams[i])
          iparams[i] = iparams[(i+j)%3];
    if (iparams[i] < 16)
      iparams[i] = 16;
  }

// Broadcast values of iparams to all MPI processes
#ifndef HPCG_NO_MPI
  MPI_Bcast( iparams, 4, MPI_INT, 0, MPI_COMM_WORLD );
#endif

  params.nx = iparams[0];
  params.ny = iparams[1];
  params.nz = iparams[2];

  params.runningTime = iparams[3];

#ifndef HPCG_NO_MPI
  MPI_Comm_rank( MPI_COMM_WORLD, &params.comm_rank );
  MPI_Comm_size( MPI_COMM_WORLD, &params.comm_size );
#else
  params.comm_rank = 0;
  params.comm_size = 1;
#endif

#ifdef HPCG_NO_OPENMP
  params.numThreads = 1;
#else
  #pragma omp parallel
  params.numThreads = omp_get_num_threads();
#endif

  time ( &rawtime );
  ptm = localtime(&rawtime);
  sprintf( fname, "hpcg_log_%04d.%02d.%02d.%02d.%02d.%02d.txt",
      1900 + ptm->tm_year, ptm->tm_mon+1, ptm->tm_mday, ptm->tm_hour, ptm->tm_min, ptm->tm_sec );

  if (0 == params.comm_rank)
    HPCG_fout.open(fname);
  else {
#if defined(HPCG_DEBUG) || defined(HPCG_DETAILED_DEBUG)
    char local[15];
    sprintf( local, "%d_", params.comm_rank );
    sprintf( fname, "hpcg_log_%s%04.d%02d.%02d.%02d.%02d.%02d.txt", local,
        1900 + ptm->tm_year, ptm->tm_mon+1, ptm->tm_mday, ptm->tm_hour, ptm->tm_min, ptm->tm_sec );
    HPCG_fout.open(fname);
#else
    HPCG_fout.open(NULLDEVICE);
#endif
  }

  return 0;
}
