#ifndef _cg_solve_hpp_
#define _cg_solve_hpp_

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
#include <limits>

#include <Vector_functions.hpp>
#include <mytimer.hpp>

#include <outstream.hpp>

namespace miniFE {

template<typename Scalar>
void print_vec(const std::vector<Scalar>& vec, const std::string& name)
{
  for(size_t i=0; i<vec.size(); ++i) {
    std::cout << name << "["<<i<<"]: " << vec[i] << std::endl;
  }
}

template<typename VectorType>
bool breakdown(typename VectorType::ScalarType inner,
               const VectorType& v,
               const VectorType& w)
{
  typedef typename VectorType::ScalarType Scalar;
  typedef typename TypeTraits<Scalar>::magnitude_type magnitude;

//This is code that was copied from Aztec, and originally written
//by my hero, Ray Tuminaro.
//
//Assuming that inner = <v,w> (inner product of v and w),
//v and w are considered orthogonal if
//  |inner| < 100 * ||v||_2 * ||w||_2 * epsilon

  magnitude vnorm = std::sqrt(dot(v,v));
  magnitude wnorm = std::sqrt(dot(w,w));
  return std::abs(inner) <= 100*vnorm*wnorm*std::numeric_limits<magnitude>::epsilon();
}

template<typename OperatorType,
         typename VectorType,
         typename Matvec>
void
cg_solve(OperatorType& A,
         const VectorType& b,
         VectorType& x,
         Matvec matvec,
         typename OperatorType::LocalOrdinalType max_iter,
         typename TypeTraits<typename OperatorType::ScalarType>::magnitude_type& tolerance,
         typename OperatorType::LocalOrdinalType& num_iters,
         typename TypeTraits<typename OperatorType::ScalarType>::magnitude_type& normr,
         timer_type* my_cg_times)
{
  typedef typename OperatorType::ScalarType ScalarType;
  typedef typename OperatorType::GlobalOrdinalType GlobalOrdinalType;
  typedef typename OperatorType::LocalOrdinalType LocalOrdinalType;
  typedef typename TypeTraits<ScalarType>::magnitude_type magnitude_type;

  timer_type t0 = 0, tWAXPY = 0, tDOT = 0, tMATVEC = 0, tMATVECDOT = 0;
  timer_type total_time = mytimer();

  int myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  if (!A.has_local_indices) {
    std::cerr << "miniFE::cg_solve ERROR, A.has_local_indices is false, needs to be true. This probably means "
       << "miniFE::make_local_matrix(A) was not called prior to calling miniFE::cg_solve."
       << std::endl;
    return;
  }

  size_t nrows = A.rows.size();
  LocalOrdinalType ncols = A.num_cols;

  VectorType r(b.startIndex, nrows, b.compute_node);
  VectorType p(0, ncols, b.compute_node);
  VectorType Ap(b.startIndex, nrows, b.compute_node);

  normr = 0;
  magnitude_type rtrans = 0;
  magnitude_type oldrtrans = 0;

  LocalOrdinalType print_freq = max_iter/10;
  if (print_freq>50) print_freq = 50;
  if (print_freq<1)  print_freq = 1;

  ScalarType one = 1.0;
  ScalarType zero = 0.0;

  typedef typename VectorType::ComputeNodeType ComputeNodeType;
  ComputeNodeType& compute_node = x.compute_node;

  //The following lines that create and initialize buffers are no-ops in many
  //cases, but perform actual allocations and copies if an off-cpu device such
  //as a GPU is being used by compute_node.

  //Do any required allocations for buffers that will be needed during CG:
  ScalarType* d_x = compute_node.get_buffer(&x.coefs[0], x.coefs.size());
  ScalarType* d_p = compute_node.get_buffer(&p.coefs[0], p.coefs.size());
  ScalarType* d_b = compute_node.get_buffer(&b.coefs[0], b.coefs.size());
  ScalarType* d_Ap = compute_node.get_buffer(&Ap.coefs[0], Ap.coefs.size());
  ScalarType* d_r  = compute_node.get_buffer(&r.coefs[0], r.coefs.size());
#ifdef MINIFE_CSR_MATRIX
  LocalOrdinalType* d_Arowoff = compute_node.get_buffer(&A.row_offsets[0], A.row_offsets.size());
  GlobalOrdinalType* d_Acols   = compute_node.get_buffer(&A.packed_cols[0], A.packed_cols.size());
  ScalarType* d_Acoefs  = compute_node.get_buffer(&A.packed_coefs[0], A.packed_coefs.size());
#endif
#ifdef MINIFE_ELL_MATRIX
  GlobalOrdinalType* d_Acols   = compute_node.get_buffer(&A.cols[0], A.cols.size());
  ScalarType* d_Acoefs  = compute_node.get_buffer(&A.coefs[0], A.coefs.size());
#endif

  //Copy data to buffers that need to be initialized from input data:
  compute_node.copy_to_buffer(&x.coefs[0], x.coefs.size(), d_x);
  compute_node.copy_to_buffer(&b.coefs[0], b.coefs.size(), d_b);
#ifdef MINIFE_CSR_MATRIX
  compute_node.copy_to_buffer(&A.row_offsets[0], A.row_offsets.size(), d_Arowoff);
  compute_node.copy_to_buffer(&A.packed_cols[0], A.packed_cols.size(), d_Acols);
  compute_node.copy_to_buffer(&A.packed_coefs[0], A.packed_coefs.size(), d_Acoefs);
#endif
#ifdef MINIFE_ELL_MATRIX
  compute_node.copy_to_buffer(&A.cols[0], A.cols.size(), d_Acols);
  compute_node.copy_to_buffer(&A.coefs[0], A.coefs.size(), d_Acoefs);
#endif

  TICK(); waxpby(one, x, zero, x, p); TOCK(tWAXPY);

  compute_node.copy_from_buffer(&p.coefs[0], p.coefs.size(), d_p);
//  print_vec(p.coefs, "p");

  TICK();
  matvec(A, p, Ap);
  TOCK(tMATVEC);

  TICK(); waxpby(one, b, -one, Ap, r); TOCK(tWAXPY);

//  if (b.coefs.size() == r.coefs.size()) std::cout << "b.size == r.size" << std::endl;
//  else std::cout << "b.size != r.size" << std::endl;
//  if (b.coefs == r.coefs) std::cout << "b == r" << std::endl;
//  else std::cout << "b != r" << std::endl;
//  compute_node.copy_from_buffer(&r.coefs[0], r.coefs.size(), d_r);
//  print_vec(b.coefs, "b");
//  print_vec(r.coefs, "r");

  TICK(); rtrans = dot(r, r); TOCK(tDOT);

//std::cout << "rtrans="<<rtrans<<std::endl;

  normr = std::sqrt(rtrans);

  if (myproc == 0) {
    std::cout << "Initial Residual = "<< normr << std::endl;
  }

  magnitude_type brkdown_tol = std::numeric_limits<magnitude_type>::epsilon();

#ifdef MINIFE_DEBUG
  std::ostream& os = outstream();
  os << "brkdown_tol = " << brkdown_tol << std::endl;
#endif


  for(LocalOrdinalType k=1; k <= max_iter && normr > tolerance; ++k) {
    if (k == 1) {
      TICK(); waxpby(one, r, zero, r, p); TOCK(tWAXPY);
    }
    else {
      oldrtrans = rtrans;
      TICK(); rtrans = dot(r, r); TOCK(tDOT);
      magnitude_type beta = rtrans/oldrtrans;
      TICK(); waxpby(one, r, beta, p, p); TOCK(tWAXPY);
    }

    normr = std::sqrt(rtrans);

    if (myproc == 0 && (k%print_freq==0 || k==max_iter)) {
      std::cout << "Iteration = "<<k<<"   Residual = "<<normr<<std::endl;
    }

    magnitude_type alpha = 0;
    magnitude_type p_ap_dot = 0;

#ifdef MINIFE_FUSED
    TICK();
    p_ap_dot = matvec_and_dot(A, p, Ap);
    TOCK(tMATVECDOT);
#else
    TICK(); matvec(A, p, Ap); TOCK(tMATVEC);

    TICK(); p_ap_dot = dot(Ap, p); TOCK(tDOT);
#endif

#ifdef MINIFE_DEBUG
    os << "iter " << k << ", p_ap_dot = " << p_ap_dot;
    os.flush();
#endif
    if (p_ap_dot < brkdown_tol) {
      if (p_ap_dot < 0 || breakdown(p_ap_dot, Ap, p)) {
        std::cerr << "miniFE::cg_solve ERROR, numerical breakdown!"<<std::endl;
#ifdef MINIFE_DEBUG
        os << "ERROR, numerical breakdown!"<<std::endl;
#endif
        //update the timers before jumping out.
        my_cg_times[WAXPY] = tWAXPY;
        my_cg_times[DOT] = tDOT;
        my_cg_times[MATVEC] = tMATVEC;
        my_cg_times[TOTAL] = mytimer() - total_time;
        return;
      }
      else brkdown_tol = 0.1 * p_ap_dot;
    }
    alpha = rtrans/p_ap_dot;
#ifdef MINIFE_DEBUG
    os << ", rtrans = " << rtrans << ", alpha = " << alpha << std::endl;
#endif

#ifdef MINIFE_FUSED
    TICK();
    fused_waxpby(one, x, alpha, p, x, one, r, -alpha, Ap, r);
    TOCK(tWAXPY);
#else
    TICK(); waxpby(one, x, alpha, p, x);
            waxpby(one, r, -alpha, Ap, r); TOCK(tWAXPY);
#endif

    num_iters = k;
  }

  compute_node.copy_from_buffer(&x.coefs[0], x.coefs.size(), d_x);

  my_cg_times[WAXPY] = tWAXPY;
  my_cg_times[DOT] = tDOT;
  my_cg_times[MATVEC] = tMATVEC;
  my_cg_times[MATVECDOT] = tMATVECDOT;
  my_cg_times[TOTAL] = mytimer() - total_time;
}

}//namespace miniFE

#endif

