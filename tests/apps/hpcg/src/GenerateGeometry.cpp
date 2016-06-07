
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

/*!
 @file GenerateGeometry.cpp

 HPCG routine
 */

#include <cmath>
#include <cstdlib>

#include "ComputeOptimalShapeXYZ.hpp"
#include "GenerateGeometry.hpp"

#ifdef HPCG_DEBUG
#include <fstream>
#include "hpcg.hpp"
using std::endl;

#include <cassert>
#endif

/*!
  Computes the factorization of the total number of processes into a
  3-dimensional process grid that is as close as possible to a cube. The
  quality of the factorization depends on the prime number structure of the
  total number of processes. It then stores this decompostion together with the
  parallel parameters of the run in the geometry data structure.

  @param[in]  size total number of MPI processes
  @param[in]  rank this process' rank among other MPI processes
  @param[in]  numThreads number of OpenMP threads in this process
  @param[in]  nx, ny, nz number of grid points for each local block in the x, y, and z dimensions, respectively
  @param[out] geom data structure that will store the above parameters and the factoring of total number of processes into three dimensions
*/
void GenerateGeometry(int size, int rank, int numThreads, int nx, int ny, int nz, Geometry * geom) {

  int npx, npy, npz;

  ComputeOptimalShapeXYZ( size, npx, npy, npz );

  // Now compute this process's indices in the 3D cube
  int ipz = rank/(npx*npy);
  int ipy = (rank-ipz*npx*npy)/npx;
  int ipx = rank%npx;

#ifdef HPCG_DEBUG
  if (rank==0)
    HPCG_fout   << "size = "<< size << endl
        << "nx  = " << nx << endl
        << "ny  = " << ny << endl
        << "nz  = " << nz << endl
        << "npx = " << npx << endl
        << "npy = " << npy << endl
        << "npz = " << npz << endl;

  HPCG_fout    << "For rank = " << rank << endl
      << "ipx = " << ipx << endl
      << "ipy = " << ipy << endl
      << "ipz = " << ipz << endl;

  assert(size==npx*npy*npz);
#endif
  geom->size = size;
  geom->rank = rank;
  geom->numThreads = numThreads;
  geom->nx = nx;
  geom->ny = ny;
  geom->nz = nz;
  geom->npx = npx;
  geom->npy = npy;
  geom->npz = npz;
  geom->ipx = ipx;
  geom->ipy = ipy;
  geom->ipz = ipz;
  return;
}
