
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
 @file CG.cpp

 HPCG routine
 */

#include <fstream>

#include <cmath>

#include "hpcg.hpp"

#include "CG.hpp"
#include "mytimer.hpp"
#include "ComputeSPMV.hpp"
#include "ComputeMG.hpp"
#include "ComputeDotProduct.hpp"
#include "ComputeWAXPBY.hpp"


// Use TICK and TOCK to time a code section in MATLAB-like fashion
#define TICK()  t0 = mytimer() //!< record current time in 't0'
#define TOCK(t) t += mytimer() - t0 //!< store time difference in 't' using time in 't0'

/*!
  Routine to compute an approximate solution to Ax = b

  @param[in]    geom The description of the problem's geometry.
  @param[inout] A    The known system matrix
  @param[inout] data The data structure with all necessary CG vectors preallocated
  @param[in]    b    The known right hand side vector
  @param[inout] x    On entry: the initial guess; on exit: the new approximate solution
  @param[in]    max_iter  The maximum number of iterations to perform, even if tolerance is not met.
  @param[in]    tolerance The stopping criterion to assert convergence: if norm of residual is <= to tolerance.
  @param[out]   niters    The number of iterations actually performed.
  @param[out]   normr     The 2-norm of the residual vector after the last iteration.
  @param[out]   normr0    The 2-norm of the residual vector before the first iteration.
  @param[out]   times     The 7-element vector of the timing information accumulated during all of the iterations.
  @param[in]    doPreconditioning The flag to indicate whether the preconditioner should be invoked at each iteration.

  @return Returns zero on success and a non-zero value otherwise.

  @see CG_ref()
*/
int CG(const SparseMatrix & A, CGData & data, const Vector & b, Vector & x,
    const int max_iter, const double tolerance, int & niters, double & normr, double & normr0,
    double * times, bool doPreconditioning) {

  double t_begin = mytimer();  // Start timing right away
  normr = 0.0;
  double rtz = 0.0, oldrtz = 0.0, alpha = 0.0, beta = 0.0, pAp = 0.0;


  double t0 = 0.0, t1 = 0.0, t2 = 0.0, t3 = 0.0, t4 = 0.0, t5 = 0.0;
//#ifndef HPCG_NO_MPI
//  double t6 = 0.0;
//#endif
  local_int_t nrow = A.localNumberOfRows;
  Vector & r = data.r; // Residual vector
  Vector & z = data.z; // Preconditioned residual vector
  Vector & p = data.p; // Direction vector (in MPI mode ncol>=nrow)
  Vector & Ap = data.Ap;

  if (!doPreconditioning && A.geom->rank==0) HPCG_fout << "WARNING: PERFORMING UNPRECONDITIONED ITERATIONS" << std::endl;

#ifdef HPCG_DEBUG
  int print_freq = 1;
  if (print_freq>50) print_freq=50;
  if (print_freq<1)  print_freq=1;
#endif
  // p is of length ncols, copy x to p for sparse MV operation
  CopyVector(x, p);
  TICK(); ComputeSPMV(A, p, Ap); TOCK(t3); // Ap = A*p
  TICK(); ComputeWAXPBY(nrow, 1.0, b, -1.0, Ap, r, A.isWaxpbyOptimized);  TOCK(t2); // r = b - Ax (x stored in p)
  TICK(); ComputeDotProduct(nrow, r, r, normr, t4, A.isDotProductOptimized); TOCK(t1);
  normr = sqrt(normr);
#ifdef HPCG_DEBUG
  if (A.geom->rank==0) HPCG_fout << "Initial Residual = "<< normr << std::endl;
#endif

  // Record initial residual for convergence testing
  normr0 = normr;

  // Start iterations

  for (int k=1; k<=max_iter && normr/normr0 > tolerance; k++ ) {
    TICK();
    if (doPreconditioning)
      ComputeMG(A, r, z); // Apply preconditioner
    else
      CopyVector (r, z); // copy r to z (no preconditioning)
    TOCK(t5); // Preconditioner apply time

    if (k == 1) {
      TICK(); ComputeWAXPBY(nrow, 1.0, z, 0.0, z, p, A.isWaxpbyOptimized); TOCK(t2); // Copy Mr to p
      TICK(); ComputeDotProduct (nrow, r, z, rtz, t4, A.isDotProductOptimized); TOCK(t1); // rtz = r'*z
    } else {
      oldrtz = rtz;
      TICK(); ComputeDotProduct (nrow, r, z, rtz, t4, A.isDotProductOptimized); TOCK(t1); // rtz = r'*z
      beta = rtz/oldrtz;
      TICK(); ComputeWAXPBY (nrow, 1.0, z, beta, p, p, A.isWaxpbyOptimized);  TOCK(t2); // p = beta*p + z
    }

    TICK(); ComputeSPMV(A, p, Ap); TOCK(t3); // Ap = A*p
    TICK(); ComputeDotProduct(nrow, p, Ap, pAp, t4, A.isDotProductOptimized); TOCK(t1); // alpha = p'*Ap
    alpha = rtz/pAp;
    TICK(); ComputeWAXPBY(nrow, 1.0, x, alpha, p, x, A.isWaxpbyOptimized);// x = x + alpha*p
            ComputeWAXPBY(nrow, 1.0, r, -alpha, Ap, r, A.isWaxpbyOptimized);  TOCK(t2);// r = r - alpha*Ap
    TICK(); ComputeDotProduct(nrow, r, r, normr, t4, A.isDotProductOptimized); TOCK(t1);
    normr = sqrt(normr);
#ifdef HPCG_DEBUG
    if (A.geom->rank==0 && (k%print_freq == 0 || k == max_iter))
      HPCG_fout << "Iteration = "<< k << "   Scaled Residual = "<< normr/normr0 << std::endl;
#endif
    niters = k;
  }

  // Store times
  times[1] += t1; // dot-product time
  times[2] += t2; // WAXPBY time
  times[3] += t3; // SPMV time
  times[4] += t4; // AllReduce time
  times[5] += t5; // preconditioner apply time
//#ifndef HPCG_NO_MPI
//  times[6] += t6; // exchange halo time
//#endif
  times[0] += mytimer() - t_begin;  // Total time. All done...
  return 0;
}
