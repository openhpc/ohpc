#ifndef _time_kernels_hpp_
#define _time_kernels_hpp_

//@HEADER
// ************************************************************************
// 
//               miniFE: simple finite-element assembly and linear-solve
//                 Copyright (2006) Sandia Corporation
// 
// Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
// license for use of this work by or on behalf of the U.S. Government.
// 
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//  
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//  
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// Questions? Contact Michael A. Heroux (maherou@sandia.gov) 
// 
// ************************************************************************
//@HEADER

#include <cmath>

#include <Vector_functions.hpp>
#include <mytimer.hpp>

#ifdef MINIFE_HAVE_CUDA
#include <cuda.h>
#endif

namespace miniFE {

template<typename OperatorType,
         typename VectorType,
         typename Matvec>
void
time_kernels(OperatorType& A,
             const VectorType& b,
             VectorType& x,
             Matvec matvec,
             typename OperatorType::LocalOrdinalType max_iter,
             typename OperatorType::ScalarType& xdotp,
             timer_type* my_kern_times)
{
  typedef typename OperatorType::ScalarType ScalarType;
  typedef typename OperatorType::LocalOrdinalType OrdinalType;
  typedef typename TypeTraits<ScalarType>::magnitude_type magnitude_type;

  timer_type t0 = 0, tWAXPY = 0, tDOT = 0, tMATVEC = 0;

  int myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  if (!A.has_local_indices) {
    std::cerr << "miniFE::time_kernels ERROR, A.has_local_indices is false, needs to be true. This probably means "
       << "miniFE::make_local_matrix(A) was not called prior to calling miniFE::time_kernels."
       << std::endl;
    return;
  }

  OrdinalType nrows = A.rows.size();
  OrdinalType ncols = A.num_cols;

  VectorType p(0, ncols, b.compute_node);

  ScalarType one = 1.0;
  ScalarType zero = 0.0;

  typedef typename VectorType::ComputeNodeType ComputeNodeType;
  ComputeNodeType& compute_node = x.compute_node;

  //The following lines that create and initialize buffers are no-ops in many
  //cases, but perform actual allocations and copies if a off-cpu device such as
  //a GPU is being used by compute_node.

  //Do any required allocations for buffers that will be needed during CG:
  ScalarType* d_x = compute_node.get_buffer(&x.coefs[0], x.coefs.size());
  ScalarType* d_p = compute_node.get_buffer(&p.coefs[0], p.coefs.size());
  ScalarType* d_b = compute_node.get_buffer(&b.coefs[0], b.coefs.size());
  OrdinalType* d_Arowoff = compute_node.get_buffer(&A.row_offsets[0], A.row_offsets.size());
  OrdinalType* d_Acols   = compute_node.get_buffer(&A.packed_cols[0], A.packed_cols.size());
  ScalarType* d_Acoefs  = compute_node.get_buffer(&A.packed_coefs[0], A.packed_coefs.size());

  //Copy data to buffers that need to be initialized from input data:
  compute_node.copy_to_buffer(&x.coefs[0], x.coefs.size(), d_x);
  compute_node.copy_to_buffer(&b.coefs[0], b.coefs.size(), d_b);
  compute_node.copy_to_buffer(&A.row_offsets[0], A.row_offsets.size(), d_Arowoff);
  compute_node.copy_to_buffer(&A.packed_cols[0], A.packed_cols.size(), d_Acols);
  compute_node.copy_to_buffer(&A.packed_coefs[0], A.packed_coefs.size(), d_Acoefs);

  TICK();
  for(OrdinalType i=0; i<max_iter; ++i) {
    waxpby(one, x, zero, x, p);
  }
#ifdef MINIFE_HAVE_CUDA
  cudaThreadSynchronize();
#endif
  TOCK(tWAXPY);

  TICK();
  for(OrdinalType i=0; i<max_iter; ++i) {
    matvec(A, p, x);
  }
#ifdef MINIFE_HAVE_CUDA
  cudaThreadSynchronize();
#endif
  TOCK(tMATVEC);

  TICK();
  xdotp = 0;
  for(OrdinalType i=0; i<max_iter; ++i) {
    xdotp += dot(x, p);
  }
#ifdef MINIFE_HAVE_CUDA
  cudaThreadSynchronize();
#endif
  TOCK(tDOT);

  my_kern_times[WAXPY] = tWAXPY;
  my_kern_times[DOT] = tDOT;
  my_kern_times[MATVEC] = tMATVEC;
  my_kern_times[TOTAL] = 0;
}

}//namespace miniFE

#endif

