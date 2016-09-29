
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
 @file SetupHalo.cpp

 HPCG routine
 */

#ifndef HPCG_NO_MPI
#include <mpi.h>
#include <map>
#include <set>
#endif

#ifndef HPCG_NO_OPENMP
#include <omp.h>
#endif

#include "SetupHalo.hpp"
#include "SetupHalo_ref.hpp"

/*!
  Prepares system matrix data structure and creates data necessary necessary
  for communication of boundary values of this process.

  @param[inout] A    The known system matrix

  @see ExchangeHalo
*/
void SetupHalo(SparseMatrix & A) {

  // The call to this reference version of SetupHalo can be replaced with custom code.
  // However, any code must work for general unstructured sparse matrices.  Special knowledge about the
  // specific nature of the sparsity pattern may not be explicitly used.

  return(SetupHalo_ref(A));
}
