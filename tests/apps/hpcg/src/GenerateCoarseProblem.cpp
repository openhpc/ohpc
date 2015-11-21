
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
 @file GenerateProblem.cpp

 HPCG routine
 */

#ifndef HPCG_NOOPENMP
#include <omp.h>
#endif

#include <cassert>
#include "GenerateCoarseProblem.hpp"
#include "GenerateGeometry.hpp"
#include "GenerateProblem.hpp"
#include "SetupHalo.hpp"

/*!
  Routine to construct a prolongation/restriction operator for a given fine grid matrix
  solution (as computed by a direct solver).

  @param[inout]  Af - The known system matrix, on output its coarse operator, fine-to-coarse operator and auxiliary vectors will be defined.

  Note that the matrix Af is considered const because the attributes we are modifying are declared as mutable.

*/

void GenerateCoarseProblem(const SparseMatrix & Af) {

  // Make local copies of geometry information.  Use global_int_t since the RHS products in the calculations
  // below may result in global range values.
  global_int_t nxf = Af.geom->nx;
  global_int_t nyf = Af.geom->ny;
  global_int_t nzf = Af.geom->nz;

  local_int_t nxc, nyc, nzc; //Coarse nx, ny, nz
  assert(nxf%2==0); assert(nyf%2==0); assert(nzf%2==0); // Need fine grid dimensions to be divisible by 2
  nxc = nxf/2; nyc = nyf/2; nzc = nzf/2;
  local_int_t * f2cOperator = new local_int_t[Af.localNumberOfRows];
  local_int_t localNumberOfRows = nxc*nyc*nzc; // This is the size of our subblock
  // If this assert fails, it most likely means that the local_int_t is set to int and should be set to long long
  assert(localNumberOfRows>0); // Throw an exception of the number of rows is less than zero (can happen if int overflow)

  // Use a parallel loop to do initial assignment:
  // distributes the physical placement of arrays of pointers across the memory system
#ifndef HPCG_NOOPENMP
  #pragma omp parallel for
#endif
  for (local_int_t i=0; i< localNumberOfRows; ++i) {
    f2cOperator[i] = 0;
  }


  // TODO:  This triply nested loop could be flattened or use nested parallelism
#ifndef HPCG_NOOPENMP
  #pragma omp parallel for
#endif
  for (local_int_t izc=0; izc<nzc; ++izc) {
	  local_int_t izf = 2*izc;
	  for (local_int_t iyc=0; iyc<nyc; ++iyc) {
		  local_int_t iyf = 2*iyc;
		  for (local_int_t ixc=0; ixc<nxc; ++ixc) {
			  local_int_t ixf = 2*ixc;
			  local_int_t currentCoarseRow = izc*nxc*nyc+iyc*nxc+ixc;
			  local_int_t currentFineRow = izf*nxf*nyf+iyf*nxf+ixf;
			  f2cOperator[currentCoarseRow] = currentFineRow;
		  } // end iy loop
	  } // end even iz if statement
  } // end iz loop

  // Construct the geometry and linear system
  Geometry * geomc = new Geometry;
  GenerateGeometry(Af.geom->size, Af.geom->rank, Af.geom->numThreads, nxc, nyc, nzc, geomc);

  SparseMatrix * Ac = new SparseMatrix;
  InitializeSparseMatrix(*Ac, geomc);
  GenerateProblem(*Ac, 0, 0, 0);
  SetupHalo(*Ac);
  Vector *rc = new Vector;
  Vector *xc = new Vector;
  Vector * Axf = new Vector;
  InitializeVector(*rc, Ac->localNumberOfRows);
  InitializeVector(*xc, Ac->localNumberOfColumns);
  InitializeVector(*Axf, Af.localNumberOfColumns);
  Af.Ac = Ac;
  MGData * mgData = new MGData;
  InitializeMGData(f2cOperator, rc, xc, Axf, *mgData);
  Af.mgData = mgData;

  return;
}
