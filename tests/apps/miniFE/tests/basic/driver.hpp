#ifndef _driver_hpp_
#define _driver_hpp_

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

#include <cstddef>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <iomanip>

#include <box_utils.hpp>
#include <Vector.hpp>

#ifdef MINIFE_CSR_MATRIX
#include <CSRMatrix.hpp>
#elif defined(MINIFE_ELL_MATRIX)
#include <ELLMatrix.hpp>
#else
#include <CSRMatrix.hpp>
#endif

#include <simple_mesh_description.hpp>

#include <SparseMatrix_functions.hpp>

#include <generate_matrix_structure.hpp>
#include <assemble_FE_data.hpp>

#include <verify_solution.hpp>

#include <compute_matrix_stats.hpp>
#include <make_local_matrix.hpp>
#include <imbalance.hpp>
#include <cg_solve.hpp>
#if MINIFE_KERNELS != 0
#include <time_kernels.hpp>
#endif
#include <outstream.hpp>
#include <utils.hpp>
#include <mytimer.hpp>
#include <YAML_Doc.hpp>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#define RUN_TIMED_FUNCTION(msg, fn, time_inc, time_total) \
{                                   \
  if (myproc==0) {                  \
    std::cout.width(30);            \
    std::cout << msg;               \
    std::cout.flush();              \
  }                                 \
  timer_type rtf_t0 = mytimer();    \
  fn;                               \
  time_inc = mytimer() - rtf_t0;    \
  time_total += time_inc;           \
  if (myproc==0) {                  \
    std::cout << time_inc << "s, total time: " << time_total << std::endl; \
  }                                 \
}

//This program assembles finite-element matrices into a global matrix and
//vector, then solves the linear-system using Conjugate Gradients.
//Each finite-element is a hexahedron with 8 vertex-nodes.
//
//Notes:
//- In finite-element terms, the box dimensions are in elements, not nodes.
//  In other words, a 2x2x2 box describes 8 elements, each of which has 8 nodes,
//  so it is a 3x3x3 node domain (27 nodes).
//  The assembled linear system will have 1 equation for each finite element node.
//
//- The coordinate origin is at the corner of the global box where x=0,
//  y=0, z=0, and the box extends along the positive x-axis, positive y-axis,
//  and the positive z-axis.
//
//- Some aspects of matrix-structure generation and finite-element assembly
//  are convenient to do using global node identifiers.
//  A global identifier for each node is obtained from coordinates plus
//  global box dimensions. See the function 'get_id' in box_utils.hpp.
//
//- Each node corresponds to a row in the matrix. The RCB partitioning method
//  we use to split the global box among processors results in some
//  processors owning non-contiguous blocks of global node identifiers.
//  Since it is convenient for matrices and vectors to store contiguously-
//  numbered blocks of rows, we map global node identifiers to a separate
//  space of row numbers such that each processor's nodes correspond to a
//  contiguous block of row numbers.
//

namespace miniFE {

template<typename Scalar,
         typename LocalOrdinal,
         typename GlobalOrdinal,
         typename ComputeNodeType>
void
driver(const Box& global_box, Box& my_box, ComputeNodeType& compute_node,
       Parameters& params, YAML_Doc& ydoc)
{
  int global_nx = global_box[0][1];
  int global_ny = global_box[1][1];
  int global_nz = global_box[2][1];

  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  if (params.load_imbalance > 0) {
    add_imbalance<GlobalOrdinal>(global_box, my_box, params.load_imbalance, ydoc);
  }

  float largest_imbalance = 0, std_dev = 0;
  compute_imbalance<GlobalOrdinal>(global_box, my_box, largest_imbalance,
                                   std_dev, ydoc, true);


  //Create a representation of the mesh:
  //Note that 'simple_mesh_description' is a virtual or conceptual
  //mesh that doesn't actually store mesh data.

  if (myproc==0) {
    std::cout.width(30);
    std::cout << "creating/filling mesh...";
    std::cout.flush();
  }

  timer_type t_start = mytimer();
  timer_type t0 = mytimer();

  simple_mesh_description<GlobalOrdinal> mesh(global_box, my_box);

  timer_type mesh_fill = mytimer() - t0;
  timer_type t_total = mytimer() - t_start;

  if (myproc==0) {
    std::cout << mesh_fill << "s, total time: " << t_total << std::endl;
  }

  //next we will generate the matrix structure.

  //Declare matrix object:

#ifdef MINIFE_CSR_MATRIX
  typedef CSRMatrix<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> MatrixType;
#elif defined(MINIFE_ELL_MATRIX)
  typedef ELLMatrix<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> MatrixType;
#else
  typedef CSRMatrix<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> MatrixType;
#endif

  MatrixType A(compute_node);

  timer_type gen_structure;
  RUN_TIMED_FUNCTION("generating matrix structure...",
                     generate_matrix_structure(mesh, A),
                     gen_structure, t_total);

  GlobalOrdinal local_nrows = A.rows.size();
  GlobalOrdinal my_first_row = local_nrows > 0 ? A.rows[0] : -1;

  Vector<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> b(my_first_row, local_nrows,compute_node);
  Vector<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> x(my_first_row, local_nrows,compute_node);

  //Assemble finite-element sub-matrices and sub-vectors into the global
  //linear system:

  timer_type fe_assembly;
  RUN_TIMED_FUNCTION("assembling FE data...",
                     assemble_FE_data(mesh, A, b, params),
                     fe_assembly, t_total);

  if (myproc == 0) {
    ydoc.add("Matrix structure generation","");
    ydoc.get("Matrix structure generation")->add("Mat-struc-gen Time",gen_structure);
    ydoc.add("FE assembly","");
    ydoc.get("FE assembly")->add("FE assembly Time",fe_assembly);
  }

#ifdef MINIFE_DEBUG
  write_matrix("A_prebc.mtx", A);
  write_vector("b_prebc.vec", b);
#endif

  //Now apply dirichlet boundary-conditions
  //(Apply the 0-valued surfaces first, then the 1-valued surface last.)

  timer_type dirbc_time;
  RUN_TIMED_FUNCTION("imposing Dirichlet BC...",
            impose_dirichlet(0.0, A, b, global_nx+1, global_ny+1, global_nz+1, mesh.bc_rows_0), dirbc_time, t_total);
  RUN_TIMED_FUNCTION("imposing Dirichlet BC...",
            impose_dirichlet(1.0, A, b, global_nx+1, global_ny+1, global_nz+1, mesh.bc_rows_1), dirbc_time, t_total);

#ifdef MINIFE_DEBUG
  write_matrix("A.mtx", A);
  write_vector("b.vec", b);
#endif

  //Transform global indices to local, set up communication information:

  timer_type make_local_time;
  RUN_TIMED_FUNCTION("making matrix indices local...",
                     make_local_matrix(A),
                     make_local_time, t_total);

#ifdef MINIFE_DEBUG
  write_matrix("A_local.mtx", A);
  write_vector("b_local.vec", b);
#endif

  size_t global_nnz = compute_matrix_stats(A, myproc, numprocs, ydoc);

  //Prepare to perform conjugate gradient solve:

  LocalOrdinal max_iters = 50;
  LocalOrdinal num_iters = 0;
  typedef typename TypeTraits<Scalar>::magnitude_type magnitude;
  magnitude rnorm = 0;
  magnitude tol = std::numeric_limits<magnitude>::epsilon();

  timer_type cg_times[NUM_TIMERS];

  typedef Vector<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> VectorType;

  t_total = mytimer() - t_start;

  bool matvec_with_comm_overlap = params.mv_overlap_comm_comp==1;

#if MINIFE_KERNELS != 0
  if (myproc==0) {
    std::cout.width(30);
    std::cout << "Starting kernel timing loops ..." << std::endl;
  }

  max_iters = 500;
  x.coefs[0] = 0.9;
  if (matvec_with_comm_overlap) {
    time_kernels(A, b, x, matvec_overlap<MatrixType,VectorType>(), max_iters, rnorm, cg_times);
  }
  else {
    time_kernels(A, b, x, matvec_std<MatrixType,VectorType>(), max_iters, rnorm, cg_times);
  }
  num_iters = max_iters;
  std::string title("Kernel timings");
#else
  if (myproc==0) {
    std::cout << "Starting CG solver ... " << std::endl;
  }

  if (matvec_with_comm_overlap) {
#ifdef MINIFE_CSR_MATRIX
    rearrange_matrix_local_external(A);
    cg_solve(A, b, x, matvec_overlap<MatrixType,VectorType>(), max_iters, tol,
           num_iters, rnorm, cg_times);
#else
    std::cout << "ERROR, matvec with overlapping comm/comp only works with CSR matrix."<<std::endl;
#endif
  }
  else {
    cg_solve(A, b, x, matvec_std<MatrixType,VectorType>(), max_iters, tol,
           num_iters, rnorm, cg_times);
    if (myproc == 0) {
      std::cout << "Final Resid Norm: " << rnorm << std::endl;
    }

#ifdef MINIFE_DEBUG
    if (myproc == 0) {
      std::cout << "verifying solution..." << std::endl;
    }
    verify_solution(mesh, x);
#endif
  }

#ifdef MINIFE_DEBUG
  write_vector("x.vec", x);
#endif
  std::string title("CG solve");
#endif

  if (myproc == 0) {
    ydoc.get("Global Run Parameters")->add("ScalarType",TypeTraits<Scalar>::name());
    ydoc.get("Global Run Parameters")->add("GlobalOrdinalType",TypeTraits<GlobalOrdinal>::name());
    ydoc.get("Global Run Parameters")->add("LocalOrdinalType",TypeTraits<LocalOrdinal>::name());
    ydoc.add(title,"");
    ydoc.get(title)->add("Iterations",num_iters);
    ydoc.get(title)->add("Final Resid Norm",rnorm);

    GlobalOrdinal global_nrows = global_nx;
    global_nrows *= global_ny*global_nz;

    //flops-per-mv, flops-per-dot, flops-per-waxpy:
    double mv_flops = global_nnz*2.0;
    double dot_flops = global_nrows*2.0;
    double waxpy_flops = global_nrows*3.0;

#if MINIFE_KERNELS == 0
//if MINIFE_KERNELS == 0 then we did a CG solve, and in that case
//there were num_iters+1 matvecs, num_iters*2 dots, and num_iters*3+2 waxpys.
    mv_flops *= (num_iters+1);
    dot_flops *= (2*num_iters);
    waxpy_flops *= (3*num_iters+2);
#else
//if MINIFE_KERNELS then we did one of each operation per iteration.
    mv_flops *= num_iters;
    dot_flops *= num_iters;
    waxpy_flops *= num_iters;
#endif

    double total_flops = mv_flops + dot_flops + waxpy_flops;

    double mv_mflops = -1;
    if (cg_times[MATVEC] > 1.e-4)
      mv_mflops = 1.e-6 * (mv_flops/cg_times[MATVEC]);

    double dot_mflops = -1;
    if (cg_times[DOT] > 1.e-4)
      dot_mflops = 1.e-6 * (dot_flops/cg_times[DOT]);

    double waxpy_mflops = -1;
    if (cg_times[WAXPY] > 1.e-4)
      waxpy_mflops = 1.e-6 *  (waxpy_flops/cg_times[WAXPY]);

    double total_mflops = -1;
    if (cg_times[TOTAL] > 1.e-4)
      total_mflops = 1.e-6 * (total_flops/cg_times[TOTAL]);

    ydoc.get(title)->add("WAXPY Time",cg_times[WAXPY]);
    ydoc.get(title)->add("WAXPY Flops",waxpy_flops);
    if (waxpy_mflops >= 0)
      ydoc.get(title)->add("WAXPY Mflops",waxpy_mflops);
    else
      ydoc.get(title)->add("WAXPY Mflops","inf");

    ydoc.get(title)->add("DOT Time",cg_times[DOT]);
    ydoc.get(title)->add("DOT Flops",dot_flops);
    if (dot_mflops >= 0)
      ydoc.get(title)->add("DOT Mflops",dot_mflops);
    else
      ydoc.get(title)->add("DOT Mflops","inf");

    ydoc.get(title)->add("MATVEC Time",cg_times[MATVEC]);
    ydoc.get(title)->add("MATVEC Flops",mv_flops);
    if (mv_mflops >= 0)
      ydoc.get(title)->add("MATVEC Mflops",mv_mflops);
    else
      ydoc.get(title)->add("MATVEC Mflops","inf");

#ifdef MINIFE_FUSED
    ydoc.get(title)->add("MATVECDOT Time",cg_times[MATVECDOT]);
    ydoc.get(title)->add("MATVECDOT Flops",mv_flops);
    if (mv_mflops >= 0)
      ydoc.get(title)->add("MATVECDOT Mflops",mv_mflops);
    else
      ydoc.get(title)->add("MATVECDOT Mflops","inf");
#endif

#if MINIFE_KERNELS == 0
    ydoc.get(title)->add("Total","");
    ydoc.get(title)->get("Total")->add("Total CG Time",cg_times[TOTAL]);
    ydoc.get(title)->get("Total")->add("Total CG Flops",total_flops);
    if (total_mflops >= 0)
      ydoc.get(title)->get("Total")->add("Total CG Mflops",total_mflops);
    else
      ydoc.get(title)->get("Total")->add("Total CG Mflops","inf");
    ydoc.get(title)->add("Time per iteration",cg_times[TOTAL]/num_iters);
#endif
  }
}

}//namespace miniFE

#endif

