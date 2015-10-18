#ifndef _utest_cases_hpp_
#define _utest_cases_hpp_

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

#include <iostream>
#include <cmath>

#include <BoxPartition.hpp>
#include <box_utils.hpp>
#include <simple_mesh_description.hpp>
#include <generate_matrix_structure.hpp>
#include <Hex8.hpp>
#include <Hex8_box_utils.hpp>
#include <assemble_FE_data.hpp>
#include <Parameters.hpp>
#include <make_local_matrix.hpp>
#include <exchange_externals.hpp>
#include <Vector_functions.hpp>
#include <BoxIterator.hpp>
#include <mytimer.hpp>

#include <SerialComputeNode.hpp>

#ifdef MINIFE_HAVE_TPI
#include <TPI.h>
#include <TPINode.hpp>
#endif

#ifdef MINIFE_HAVE_TBB
#include <tbb/task_scheduler_init.h>
#include <TBBNode.hpp>
#endif

#ifdef MINIFE_HAVE_CUDA
#include <CudaNode.hpp>
#endif

#include <utest_case.hpp>

typedef MINIFE_SCALAR Scalar;
typedef MINIFE_LOCAL_ORDINAL LocalOrdinal;
typedef MINIFE_GLOBAL_ORDINAL GlobalOrdinal;

template<typename T>
inline
int check_get_id(int nx, int ny, int nz, int x, int y, int z, T expected, const char* testname)
{
  T val = miniFE::get_id<T>(nx,ny,nz,x,y,z);
  if (val != expected) {
    std::cout << testname << " failed. val=" << val<<", expected " << expected << std::endl;
    return -1;
  }
  return 0;
}

UTEST_CASE(box_partition)
{
  int global_box[3][2] = { { 0, 2000 }, { 0, 2000}, { 0, 2000} };
  int numprocs = 4, myproc = 0;

  int (*local_boxes0)[3][2] = (int(*)[3][2])std::malloc(sizeof(int)*numprocs*3*2);
  int (*local_boxes1)[3][2] = (int(*)[3][2])std::malloc(sizeof(int)*numprocs*3*2);
  int (*local_boxes2)[3][2] = (int(*)[3][2])std::malloc(sizeof(int)*numprocs*3*2);
  int (*local_boxes3)[3][2] = (int(*)[3][2])std::malloc(sizeof(int)*numprocs*3*2);

  box_partition(0, numprocs, 2, global_box, local_boxes0);
  box_partition(0, numprocs, 2, global_box, local_boxes1);
  box_partition(0, numprocs, 2, global_box, local_boxes2);
  box_partition(0, numprocs, 2, global_box, local_boxes3);

  for(int i=1; i<numprocs; ++i) {
    if (miniFE::get_num_ids<int>(local_boxes0[i]) !=
        miniFE::get_num_ids<int>(local_boxes0[0])) {
      return false;
    }
    if (miniFE::get_num_ids<int>(local_boxes1[i]) !=
        miniFE::get_num_ids<int>(local_boxes1[0])) {
      return false;
    }
    if (miniFE::get_num_ids<int>(local_boxes2[i]) !=
        miniFE::get_num_ids<int>(local_boxes2[0])) {
      return false;
    }
    if (miniFE::get_num_ids<int>(local_boxes3[i]) !=
        miniFE::get_num_ids<int>(local_boxes3[0])) {
      return false;
    }

    if (miniFE::get_num_ids<int>(local_boxes0[i]) < 0 ||
        miniFE::get_num_ids<int>(local_boxes0[i]) > 2000000000) {
      return false;
    }
  }

  std::free(local_boxes0);
  std::free(local_boxes1);
  std::free(local_boxes2);
  std::free(local_boxes3);

  return true;
}

UTEST_CASE(generate_matrix_structure1)
{
  int global_box[3][2] = {{ 0, 1 }, { 0, 1 }, { 0, 1 } };
  int box[3][2] = {{ 0, 1 }, { 0, 1 }, { 0, 1 } };

  miniFE::simple_mesh_description<int> mesh(global_box, box);

  SerialComputeNode compute_node;
  miniFE::CSRMatrix<Scalar, int, int, SerialComputeNode> A(compute_node);

  miniFE::generate_matrix_structure(mesh, A);

  int nodes_x = global_box[0][1]+1;
  int nodes_y = global_box[1][1]+1;
  int nodes_z = global_box[2][1]+1;
  int nrows = nodes_x*nodes_y*nodes_z;
  
  if (A.rows.size() != nrows) {
    return false;
  }

  if (A.row_offsets[nrows] != 64) {
    return false;
  }

  return true;
}

UTEST_CASE(generate_matrix_structure2)
{
  int global_box[3][2] = {{ 0, 2 }, { 0, 2 }, { 0, 2 } };
  int box[3][2] = {{ 0, 2 }, { 0, 2 }, { 0, 2 } };

  miniFE::simple_mesh_description<int> mesh(global_box, box);

  SerialComputeNode compute_node;
  miniFE::CSRMatrix<Scalar, int, int,SerialComputeNode> A(compute_node);

  int nodes_x = global_box[0][1]+1;
  int nodes_y = global_box[1][1]+1;
  int nodes_z = global_box[2][1]+1;
  int nrows = nodes_x*nodes_y*nodes_z;
  
  if (nrows != 27) {
    return false;
  }

  miniFE::generate_matrix_structure(mesh, A);

  if (A.row_offsets.size() != nrows+1) {
    return false;
  }

  if (A.row_offsets[nrows] != 343) {
    return false;
  }

  if (A.row_offsets[14]-A.row_offsets[13] != 27) {
    return false;
  }

  return true;
}

UTEST_CASE(get_hex8_node_coords_3d)
{
  std::vector<Scalar> coords(24);
  coords[0] = 0;
  coords[1] = 0;
  coords[2] = 0;
  coords[3] = 1;
  coords[4] = 0;
  coords[5] = 0;
  coords[6] = 1;
  coords[7] = 0;
  coords[8] = -1;
  coords[9] = 0;
  coords[10] = 0;
  coords[11] = -1;
  coords[12] = 0;
  coords[13] = 1;
  coords[14] = 0;
  coords[15] = 1;
  coords[16] = 1;
  coords[17] = 0;
  coords[18] = 1;
  coords[19] = 1;
  coords[20] = -1;
  coords[21] = 0;
  coords[22] = 1;
  coords[23] = -1;

  std::vector<Scalar> testcoords(24);

  miniFE::get_hex8_node_coords_3d(0, 0, 0, 1.0, &testcoords[0]);

  if (coords != testcoords) {
    return false;
  }

  return true;
}

inline
void get_test_elem_mat(std::vector<Scalar>& elem_mat)
{
//after much careful debugging, I'm convinced that the following is a
//correct element-diffusion matrix for the element with local-node-0 at
//coordinates 0,0,0. So pasting this into a unit-test will guard against
//unintended changes as I continue working on the code for various reasons.

  elem_mat.resize(36);
elem_mat[0] = 0.6666666664477059;
elem_mat[1] = 1.094804871759614e-10;
elem_mat[2] = -0.1666666666666667;
elem_mat[3] = 1.094805019211109e-10;
elem_mat[4] = 1.094804871759614e-10;
elem_mat[5] = -0.1666666666666667;
elem_mat[6] = -0.1666666667761472;
elem_mat[7] = -0.1666666666666667;
elem_mat[8] = 0.666666666447706;
elem_mat[9] = 1.094804941148553e-10;
elem_mat[10] = -0.1666666666666667;
elem_mat[11] = -0.1666666666666667;
elem_mat[12] = 1.094804732981736e-10;
elem_mat[13] = -0.1666666666666667;
elem_mat[14] = -0.1666666667761472;
elem_mat[15] = 0.666666666447706;
elem_mat[16] = 1.094804841401953e-10;
elem_mat[17] = -0.1666666667761472;
elem_mat[18] = -0.1666666666666667;
elem_mat[19] = 1.094804871759614e-10;
elem_mat[20] = -0.1666666666666667;
elem_mat[21] = 0.6666666664477059;
elem_mat[22] = -0.1666666666666668;
elem_mat[23] = -0.1666666667761472;
elem_mat[24] = -0.1666666666666667;
elem_mat[25] = 1.094804702624075e-10;
elem_mat[26] = 0.666666666447706;
elem_mat[27] = 1.094804802370675e-10;
elem_mat[28] = -0.1666666666666667;
elem_mat[29] = 1.094804698287266e-10;
elem_mat[30] = 0.666666666447706;
elem_mat[31] = 1.094805079926431e-10;
elem_mat[32] = -0.1666666666666667;
elem_mat[33] = 0.666666666447706;
elem_mat[34] = 1.094804663592797e-10;
elem_mat[35] = 0.666666666447706;
}

UTEST_CASE(diffusionMatrix)
{
  std::vector<Scalar> elem_mat_correct(64);
  get_test_elem_mat(elem_mat_correct);

  const size_t len = miniFE::Hex8::numNodesPerElem*miniFE::Hex8::numNodesPerElem;
  Scalar elem_mat[len];
  Scalar testcoords[miniFE::Hex8::numNodesPerElem*miniFE::Hex8::spatialDim];

  miniFE::get_hex8_node_coords_3d(0, 0, 0, 1.0, &testcoords[0]);

  miniFE::Hex8::diffusionMatrix_symm(testcoords, elem_mat);

  for(size_t i=0; i<len; ++i) {
    if (std::abs(elem_mat[i] - elem_mat_correct[i]) > 1.e-6) {
      return false;
    }
  }

  Scalar elem_vec_correct[miniFE::Hex8::numNodesPerElem];
  elem_vec_correct[0] = 0.125;
  elem_vec_correct[1] = 0.125;
  elem_vec_correct[2] = 0.125;
  elem_vec_correct[3] = 0.125;
  elem_vec_correct[4] = 0.125;
  elem_vec_correct[5] = 0.125;
  elem_vec_correct[6] = 0.125;
  elem_vec_correct[7] = 0.125;

  Scalar elem_vec[miniFE::Hex8::numNodesPerElem];
  miniFE::Hex8::sourceVector(testcoords, elem_vec);

  const size_t nn = miniFE::Hex8::numNodesPerElem;
  for(size_t i=0; i<nn; ++i) {
    if (std::abs(elem_vec[i] - elem_vec_correct[i]) > 1.e-13) {
      return false;
    }
  }

  return true;
}

UTEST_CASE(sum_into_row)
{
  SerialComputeNode compute_node;
  miniFE::CSRMatrix<Scalar,LocalOrdinal,GlobalOrdinal,SerialComputeNode> A(compute_node);
  A.rows.resize(1,0);
  A.row_offsets.resize(2,0);
  A.row_offsets[1] = 4;
  A.packed_cols.resize(4);
  A.packed_cols[0] = 0;
  A.packed_cols[1] = 1;
  A.packed_cols[2] = 2;
  A.packed_cols[3] = 3;
  A.packed_coefs.resize(4,0);

  std::vector<int> indices(4);
  indices[0] = 2;
  indices[1] = 0;
  indices[2] = 1;
  indices[3] = 3;
  std::vector<Scalar> coefs(4);
  coefs[0] = 2.0;
  coefs[1] = 0.0;
  coefs[2] = 1.0;
  coefs[3] = 3.0;

  miniFE::sum_into_row(0, 4, &indices[0], &coefs[0], A);

  coefs[0] = 0.0;
  coefs[1] = 1.0;
  coefs[2] = 2.0;
  coefs[3] = 3.0;

  if (coefs != A.packed_coefs) {
    return false;
  }

  return true;
}

UTEST_CASE(sum_in_elem_matrix)
{
  SerialComputeNode compute_node;
  miniFE::CSRMatrix<Scalar,LocalOrdinal,GlobalOrdinal,SerialComputeNode> A(compute_node);
  A.rows.resize(4,0);
  A.rows[0] = 0;
  A.rows[1] = 1;
  A.rows[2] = 2;
  A.rows[3] = 3;
  A.row_offsets.resize(5,0);
  A.row_offsets[1] = 4;
  A.row_offsets[2] = 8;
  A.row_offsets[3] = 12;
  A.row_offsets[4] = 16;
  A.packed_cols.resize(16);
  A.packed_cols[0] = 0;
  A.packed_cols[1] = 1;
  A.packed_cols[2] = 2;
  A.packed_cols[3] = 3;
  A.packed_cols[4] = 0;
  A.packed_cols[5] = 1;
  A.packed_cols[6] = 2;
  A.packed_cols[7] = 3;
  A.packed_cols[8] = 0;
  A.packed_cols[9] = 1;
  A.packed_cols[10] = 2;
  A.packed_cols[11] = 3;
  A.packed_cols[12] = 0;
  A.packed_cols[13] = 1;
  A.packed_cols[14] = 2;
  A.packed_cols[15] = 3;

  A.packed_coefs.resize(16,0);

  std::vector<int> indices(4);
  indices[0] = 2;
  indices[1] = 0;
  indices[2] = 1;
  indices[3] = 3;
  std::vector<Scalar> coefs(16);
  coefs[0] = 2.0;
  coefs[1] = 0.0;
  coefs[2] = 1.0;
  coefs[3] = 3.0;
  coefs[4] = 2.0;
  coefs[5] = 0.0;
  coefs[6] = 1.0;
  coefs[7] = 3.0;
  coefs[8] = 2.0;
  coefs[9] = 0.0;
  coefs[10] = 1.0;
  coefs[11] = 3.0;
  coefs[12] = 2.0;
  coefs[13] = 0.0;
  coefs[14] = 1.0;
  coefs[15] = 3.0;

  miniFE::sum_in_elem_matrix(4, &indices[0], &coefs[0], A);

  coefs[0] = 0.0;
  coefs[1] = 1.0;
  coefs[2] = 2.0;
  coefs[3] = 3.0;
  coefs[4] = 0.0;
  coefs[5] = 1.0;
  coefs[6] = 2.0;
  coefs[7] = 3.0;
  coefs[8] = 0.0;
  coefs[9] = 1.0;
  coefs[10] = 2.0;
  coefs[11] = 3.0;
  coefs[12] = 0.0;
  coefs[13] = 1.0;
  coefs[14] = 2.0;
  coefs[15] = 3.0;

  if (coefs != A.packed_coefs) {
    return false;
  }

  return true;
}

UTEST_CASE(assemble_FE_data)
{
  int global_box[3][2] = {{ 0, 1 }, { 0, 1 }, { 0, 1 } };
  int box[3][2] = {{ 0, 1 }, { 0, 1 }, { 0, 1 } };

  miniFE::simple_mesh_description<int> mesh(global_box, box);

  SerialComputeNode compute_node;
  miniFE::CSRMatrix<Scalar, int, int, SerialComputeNode> A(compute_node);

  miniFE::generate_matrix_structure(mesh, A);

  miniFE::Vector<Scalar,LocalOrdinal,GlobalOrdinal,SerialComputeNode> b(0, 8, compute_node);

  const int num_nodes = 8;

  std::vector<Scalar> symm_elem_mat_correct;
  get_test_elem_mat(symm_elem_mat_correct);
  std::vector<Scalar> full_elem_mat_correct(num_nodes*num_nodes);

  int offset = 0;
  for(int i=0; i<num_nodes; ++i) {
    for(int j=0; j<num_nodes; ++j) {
      if (j>=i) {
        Scalar coef = symm_elem_mat_correct[offset++];
        full_elem_mat_correct[i*num_nodes+j] = coef;
        full_elem_mat_correct[j*num_nodes+i] = coef;
      }
    }
  }

  std::vector<int> elem_node_ids(num_nodes);
  elem_node_ids[0] = 0;
  elem_node_ids[1] = 1;
  elem_node_ids[2] = 5;
  elem_node_ids[3] = 4;
  elem_node_ids[4] = 2;
  elem_node_ids[5] = 3;
  elem_node_ids[6] = 7;
  elem_node_ids[7] = 6;

  //now for each row of of the 8x8 elem_mat_correct, reorder that
  //row according to the order of elem_node_ids, rows and columns.
  std::vector<Scalar> elem_mat_reordered(num_nodes*num_nodes);
  offset = 0;
  int row = 0;
  for(int i=0; i<num_nodes; ++i) {
    row = num_nodes*elem_node_ids[i];
    for(int j=0; j<num_nodes; ++j) {
      elem_mat_reordered[row+elem_node_ids[j]] = full_elem_mat_correct[offset+j];
    }
    offset += num_nodes;
  }

  //now elem_mat_reordered should contain the same coefficients,
  //in the same order, as the assembled-matrix coefficients that will be
  //produced in A by assemble_FE_data:

  miniFE::Parameters params;
  params.use_locking = 1;

  miniFE::assemble_FE_data(mesh, A, b, params);

  std::vector<Scalar>& assembled_mat = A.packed_coefs;

  for(size_t i=0; i<elem_mat_reordered.size(); ++i) {
    if (std::abs(elem_mat_reordered[i] - assembled_mat[i]) > 1.e-13) {
      return false;
    }
  }

  return true;
}

UTEST_CASE(pll_matvec2)
{
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  if (numprocs != 2) {
    if (myproc == 0) std::cout <<"pll_matvec2_utest only runs when numprocs=2."<<std::endl;
    return true;
  }

  //create the following matrix and vector:
  //
  // A = | 1 1      |  x = | 1 |
  //     | 2 1 -1 1 |      | 2 |
  //     |  -2  1   |      | 3 |
  //     |   2    1 |      | 4 |
  //
  // with the first 2 rows on proc 0 and the other rows on proc 1.
  //
  //So a matvec should produce y = | 3 |
  //                               | 5 |
  //                               |-1 |
  //                               | 8 |

  SerialComputeNode compute_node;

  miniFE::CSRMatrix<Scalar,LocalOrdinal,GlobalOrdinal,SerialComputeNode> A(compute_node);
  miniFE::Vector<Scalar,LocalOrdinal,GlobalOrdinal,SerialComputeNode> x(myproc, 4,compute_node) ,y(myproc, 4,compute_node);

  A.rows.resize(2, 0);
  if (myproc == 0) {
    A.rows[0] = 0; A.rows[1] = 1;
  }
  else {
    A.rows[0] = 2; A.rows[1] = 3;
  }

  A.row_offsets.resize(3, 0);
  if (myproc == 0) {
    A.row_offsets[1] = 2; A.row_offsets[2] = 6;
  }
  else {
    A.row_offsets[1] = 2; A.row_offsets[2] = 4;
  }

  if (myproc == 0) {
    A.packed_cols.resize(6, 0);
    A.packed_cols[1] = 1;
    A.packed_cols[2] = 0;
    A.packed_cols[3] = 1;
    A.packed_cols[4] = 2;
    A.packed_cols[5] = 3;
  }
  else {
    A.packed_cols.resize(4, 0);
    A.packed_cols[0] = 1;
    A.packed_cols[1] = 2;
    A.packed_cols[2] = 1;
    A.packed_cols[3] = 3;
  }
  if (myproc == 0) {
    A.packed_coefs.resize(6, 1);
    A.packed_coefs[2] = 2;
    A.packed_coefs[4] = -1;
  }
  else {
    A.packed_coefs.resize(4, 1);
    A.packed_coefs[0] = -2;
    A.packed_coefs[2] = 2;
  }

  if (myproc == 0) {
    x.coefs[0] = 1; x.coefs[1] = 2;
  }
  else {
    x.coefs[0] = 3; x.coefs[1] = 4;
  }

  miniFE::make_local_matrix(A);
  miniFE::exchange_externals(A, x);
  miniFE::matvec(A, x, y);

  if (myproc == 0) {
    if (y.coefs[0] != 3.0 || y.coefs[1] != 5.0) {
      std::cout << "proc 0: pll_matvec2_utest failed" << std::endl;
      return false;
    }
  }
  else {
    if (y.coefs[0] != -1.0 || y.coefs[1] != 8.0) {
      std::cout << "proc 1: pll_matvec2_utest failed" << std::endl;
      return false;
    }
  }

  return true;
}

UTEST_CASE(pll_matvec3)
{
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  if (numprocs != 3) {
    if (myproc == 0) std::cout <<"pll_matvec3_utest only runs when numprocs=3."<<std::endl;
    return true;
  }

  //create the following matrix and vector:
  //
  // cols: 0  1  2  3  4  5 
  // A = | 1       -1        |  x = | 1 |
  //     |    1          -1  |      | 2 |
  //     | 2     1    -1     |      | 3 |
  //     |          1        |      | 4 |
  //     |    2        1     |      | 5 |
  //     |          2     1  |      | 6 |
  //
  // with the first 2 rows on proc 0, next 2 on proc 1, last 2 on proc 2.
  //
  //So a matvec should produce y = |-3 |
  //                               |-4 |
  //                               | 0 |
  //                               | 4 |
  //                               | 9 |
  //                               |14 |

  SerialComputeNode compute_node;

  miniFE::CSRMatrix<Scalar,LocalOrdinal,GlobalOrdinal,SerialComputeNode> A(compute_node);
  miniFE::Vector<Scalar,LocalOrdinal,GlobalOrdinal,SerialComputeNode> x(myproc, 6, compute_node) ,y(myproc, 6, compute_node);

  A.rows.resize(2, 0);
  A.rows[0] = myproc*2; A.rows[1] = myproc*2+1;

  A.row_offsets.resize(3, 0);
  if (myproc == 0) {
    A.row_offsets[1] = 2; A.row_offsets[2] = 4;
  }
  else if (myproc == 1) {
    A.row_offsets[1] = 3; A.row_offsets[2] = 4;
  }
  else {
    A.row_offsets[1] = 2; A.row_offsets[2] = 4;
  }

  A.packed_cols.resize(4, 0);
  if (myproc == 0) {
    A.packed_cols[1] = 3;
    A.packed_cols[2] = 1;
    A.packed_cols[3] = 5;
  }
  else if (myproc == 1) {
    A.packed_cols[1] = 2;
    A.packed_cols[2] = 4;
    A.packed_cols[3] = 3;
  }
  else {
    A.packed_cols[0] = 1;
    A.packed_cols[1] = 4;
    A.packed_cols[2] = 3;
    A.packed_cols[3] = 5;
  }

  A.packed_coefs.resize(4, 1);
  if (myproc == 0) {
    A.packed_coefs[1] = -1;
    A.packed_coefs[3] = -1;
  }
  else if (myproc == 1) {
    A.packed_coefs[0] = 2;
    A.packed_coefs[2] = -1;
  }
  else {
    A.packed_coefs[0] = 2;
    A.packed_coefs[2] = 2;
  }

  if (myproc == 0) {
    x.coefs[0] = 1; x.coefs[1] = 2;
  }
  else if (myproc == 1) {
    x.coefs[0] = 3; x.coefs[1] = 4;
  }
  else {
    x.coefs[0] = 5; x.coefs[1] = 6;
  }

  miniFE::make_local_matrix(A);
  miniFE::exchange_externals(A, x);
  miniFE::matvec(A, x, y);

  if (myproc == 0) {
    if (y.coefs[0] != -3.0 || y.coefs[1] != -4.0) {
      std::cout << "proc 0: pll_matvec3 failed" << std::endl;
      return false;
    }
  }
  else if (myproc == 1) {
    if (y.coefs[0] != 0.0 || y.coefs[1] != 4.0) {
      std::cout << "proc 1: pll_matvec3 failed" << std::endl;
      return false;
    }
  }
  else {
    if (y.coefs[0] != 9.0 || y.coefs[1] != 14.0) {
      std::cout << "proc 2: pll_matvec3 failed" << std::endl;
      return false;
    }
  }

  return true;
}

UTEST_CASE(ComputeNode_waxpy1)
{
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  if (numprocs != 1) {
    if (myproc == 0) std::cout <<"ComputeNode_waxpy1 only runs when numprocs=1."<<std::endl;
    return true;
  }

#ifdef MINIFE_HAVE_CUDA
  CUDANode compute_node(0,16,64);
  typedef CUDANode ComputeNodeType;
#else
  SerialComputeNode compute_node;
  typedef SerialComputeNode ComputeNodeType;
#endif

  size_t len = 10;

  miniFE::Vector<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> x(0, len, compute_node), y(0, len, compute_node), w(0, len, compute_node);

  std::vector<GlobalOrdinal> inds(len, 0);
  for(size_t i=0; i<len; ++i) inds[i] = i;

  std::vector<Scalar> coefs(len, 1);

  miniFE::sum_into_vector(len, &inds[0], &coefs[0], x);
  miniFE::sum_into_vector(len, &inds[0], &coefs[0], y);
  miniFE::sum_into_vector(len, &inds[0], &coefs[0], w);

  Scalar* d_x = compute_node.get_buffer(&x.coefs[0], x.coefs.size());
  Scalar* d_y = compute_node.get_buffer(&y.coefs[0], y.coefs.size());

  compute_node.copy_to_buffer(&x.coefs[0], x.coefs.size(), d_x);
  compute_node.copy_to_buffer(&y.coefs[0], y.coefs.size(), d_y);

  miniFE::waxpby(1.0, x, 1.0, y, w);

  Scalar* d_w = compute_node.get_buffer(&w.coefs[0], w.coefs.size());
  compute_node.copy_from_buffer(&w.coefs[0], w.coefs.size(), d_w);

  Scalar expected = 2;
  Scalar tol = 1.e-7;

  for(size_t i=0; i<len; ++i) { 
    if (std::abs(w.coefs[i]-expected) > tol) {
      return false;
    }
  }
  return true;
}

UTEST_CASE(ComputeNode_dot1)
{
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  if (numprocs != 1) {
    if (myproc == 0) std::cout <<"ComputeNode_dot1 only runs when numprocs=1."<<std::endl;
    return true;
  }

#ifdef MINIFE_HAVE_CUDA
  CUDANode compute_node(0,1,64);
  typedef CUDANode ComputeNodeType;
#else
  SerialComputeNode compute_node;
  typedef SerialComputeNode ComputeNodeType;
#endif

  size_t N = 100;
  miniFE::Vector<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> x(0, N, compute_node), y(0, N, compute_node);

  std::vector<int> inds(N, 0);
  for(size_t i=0; i<N; ++i) inds[i] = i;

  std::vector<Scalar> coefs(N, 1);

  miniFE::sum_into_vector(N, &inds[0], &coefs[0], x);
  miniFE::sum_into_vector(N, &inds[0], &coefs[0], y);

  Scalar* d_x = compute_node.get_buffer(&x.coefs[0], x.coefs.size());
  Scalar* d_y = compute_node.get_buffer(&y.coefs[0], y.coefs.size());

  compute_node.copy_to_buffer(&x.coefs[0], x.coefs.size(), d_x);
  compute_node.copy_to_buffer(&y.coefs[0], y.coefs.size(), d_y);

  Scalar dot_prod = miniFE::dot(x,y);

  if (dot_prod != N) {
    return false;
  }

  return true;
}

UTEST_CASE(ComputeNode_TBB_dot1)
{
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  if (numprocs != 1) {
    if (myproc == 0) std::cout <<"ComputeNode_TBB_dot1_utest only runs when numprocs=1."<<std::endl;
    return true;
  }

#ifdef MINIFE_HAVE_TBB
  TBBNode compute_node(2);
  typedef TBBNode ComputeNodeType;

  size_t N = 10;
  miniFE::Vector<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> x(0, N, compute_node), y(0, N, compute_node);

  std::vector<GlobalOrdinal> inds(N, 0);
  for(size_t i=0; i<N; ++i) inds[i] = i;

  std::vector<Scalar> coefs(N, 1);

  miniFE::sum_into_vector(inds.size(), &inds[0], &coefs[0], x);
  miniFE::sum_into_vector(inds.size(), &inds[0], &coefs[0], y);

  Scalar dot_prod = miniFE::dot(x,y);

  if (dot_prod != N) {
    return false;
  }

#else
  std::cout << "ComputeNode_TBB_dot1_utest only runs when MINIFE_HAVE_TBB is defined."<<std::endl;
#endif
  return true;
}

UTEST_CASE(ComputeNode_dot2)
{
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  if (numprocs != 1) {
    if (myproc == 0) std::cout <<"ComputeNode_dot2_utest only runs when numprocs=1."<<std::endl;
    return true;
  }

#ifdef MINIFE_HAVE_CUDA
  CUDANode compute_node(0,64,128);
  typedef CUDANode ComputeNodeType;
#else
  SerialComputeNode compute_node;
  typedef SerialComputeNode ComputeNodeType;
#endif

  miniFE::Vector<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> x(0, 10, compute_node), y(0, 10, compute_node);

  size_t len = 10;
  std::vector<int> inds(len, 0);
  for(size_t i=0; i<len; ++i) inds[i] = i;

  std::vector<Scalar> coefs(len, 1);

  miniFE::sum_into_vector(len, &inds[0], &coefs[0], x);
  miniFE::sum_into_vector(len, &inds[0], &coefs[0], y);

  Scalar* d_x = compute_node.get_buffer(&x.coefs[0], x.coefs.size());
  Scalar* d_y = compute_node.get_buffer(&y.coefs[0], y.coefs.size());

  compute_node.copy_to_buffer(&x.coefs[0], x.coefs.size(), d_x);
  compute_node.copy_to_buffer(&y.coefs[0], y.coefs.size(), d_y);

  Scalar dot_prod = miniFE::dot(x, y);

  if (std::abs(dot_prod-10.0) > 1.e-12) {
    return false;
  }
  return true;
}

UTEST_CASE(ser_matvec1)
{
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  if (numprocs != 1) {
    if (myproc == 0) std::cout <<"ser_matvec1_utest only runs when numprocs=1."<<std::endl;
    return 0;
  }

  //create the following matrix and vector:
  //
  // A = | 1 1      |  x = | 1 |
  //     | 2 1 -1 1 |      | 2 |
  //     |  -2  1   |      | 3 |
  //     |   2    1 |      | 4 |
  //
  // with the first 2 rows on proc 0 and the other rows on proc 1.
  //
  //So a matvec should produce y = | 3 |
  //                               | 5 |
  //                               |-1 |
  //                               | 8 |

#ifdef MINIFE_HAVE_CUDA
  CUDANode compute_node(0,64,128);
  typedef CUDANode ComputeNodeType;
#else
  SerialComputeNode compute_node;
  typedef SerialComputeNode ComputeNodeType;
#endif

  miniFE::CSRMatrix<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> A(compute_node);
  miniFE::Vector<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> x(0, 4,compute_node) ,y(0, 4,compute_node);

  A.rows.resize(4, 0);
  A.rows[0] = 0; A.rows[1] = 1;
  A.rows[2] = 2; A.rows[3] = 3;

  A.row_offsets.resize(5, 0);
  A.row_offsets[1] = 2; A.row_offsets[2] = 6;
  A.row_offsets[3] = 8; A.row_offsets[4] = 10;

  A.packed_cols.resize(10, 0);
  A.packed_cols[1] = 1;
  A.packed_cols[2] = 0;
  A.packed_cols[3] = 1;
  A.packed_cols[4] = 2;
  A.packed_cols[5] = 3;
  A.packed_cols[6] = 1;
  A.packed_cols[7] = 2;
  A.packed_cols[8] = 1;
  A.packed_cols[9] = 3;

  A.packed_coefs.resize(10, 1);
  A.packed_coefs[2] = 2;
  A.packed_coefs[4] = -1;
  A.packed_coefs[6] = -2;
  A.packed_coefs[8] = 2;

  x.coefs[0] = 1; x.coefs[1] = 2; x.coefs[2] = 3; x.coefs[3] = 4;

  for(size_t i=0; i<y.coefs.size(); ++i) y.coefs[i] = 0;

  miniFE::make_local_matrix(A);

  Scalar* d_x = compute_node.get_buffer(&x.coefs[0], x.coefs.size());
  compute_node.copy_to_buffer(&x.coefs[0], x.coefs.size(), d_x);

  LocalOrdinal* d_Arowoff = compute_node.get_buffer(&A.row_offsets[0], A.row_offsets.size());
  GlobalOrdinal* d_Acols   = compute_node.get_buffer(&A.packed_cols[0], A.packed_cols.size());
  Scalar* d_Acoefs  = compute_node.get_buffer(&A.packed_coefs[0], A.packed_coefs.size());

  compute_node.copy_to_buffer(&A.row_offsets[0], A.row_offsets.size(), d_Arowoff);
  compute_node.copy_to_buffer(&A.packed_cols[0], A.packed_cols.size(), d_Acols);
  compute_node.copy_to_buffer(&A.packed_coefs[0], A.packed_coefs.size(), d_Acoefs);

  miniFE::matvec(A, x, y);

  Scalar* ybuf = compute_node.get_buffer(&y.coefs[0], y.coefs.size());
  compute_node.copy_from_buffer(&y.coefs[0], y.coefs.size(), ybuf);

  if (std::abs(y.coefs[0] - 3.0) > 1.e-12) {
    std::cout << "failed 0. y.coefs[0]=" <<y.coefs[0]<<", expected 3.0" << std::endl;
    return false;
  }

  if (std::abs(y.coefs[1] - 5.0) > 1.e-12) {
    std::cout << "failed 1. y.coefs[1]=" <<y.coefs[1]<<", expected 5.0" << std::endl;
    return false;
  }

  if (std::abs(y.coefs[2] - -1.0) > 1.e-12) {
    std::cout << "failed 2. y.coefs[2]=" <<y.coefs[2]<<", expected -1.0" << std::endl;
    return false;
  }

  if (std::abs(y.coefs[3] - 8.0) > 1.e-12) {
    std::cout << "failed 3. y.coefs[3]=" <<y.coefs[3]<<", expected 8.0" << std::endl;
    return false;
  }

  return true;
}

using miniFE::mytimer;

UTEST_CASE(waxpby_perf)
{
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  if (numprocs != 1) {
    if (myproc == 0) std::cout <<"waxpby_perf_utest only runs when numprocs=1."<<std::endl;
    return true;
  }

  size_t num_iters = 10;
  size_t len = 8193;

#ifdef MINIFE_HAVE_CUDA
  CUDANode compute_node(0,16,64);
  typedef CUDANode ComputeNodeType;
#else
  SerialComputeNode compute_node;
  typedef SerialComputeNode ComputeNodeType;
#endif

  miniFE::timer_type t0 = 0, tWAXPY = 0;

  while(tWAXPY < 1.e-2) {

    tWAXPY = 0;
    len *= 2;

    miniFE::Vector<Scalar,LocalOrdinal,GlobalOrdinal,ComputeNodeType> x(0, len,compute_node) ,y(0, len,compute_node), w(0, len,compute_node);
  
    Scalar one = 1, zero = 0;
  
    for(size_t i=0; i<len; ++i) {
      x.coefs[i] = one;
      y.coefs[i] = one;
      w.coefs[i] = zero;
    }

    Scalar* d_x = compute_node.get_buffer(&x.coefs[0], x.coefs.size());
    Scalar* d_y = compute_node.get_buffer(&y.coefs[0], y.coefs.size());

    compute_node.copy_to_buffer(&x.coefs[0], x.coefs.size(), d_x);
    compute_node.copy_to_buffer(&y.coefs[0], y.coefs.size(), d_y);

    TICK();
    for(size_t i=0; i<num_iters; ++i) {
      miniFE::waxpby(one, x, one, y, w);
    }
    TOCK(tWAXPY);

#ifdef MINIFE_HAVE_CUDA
//on cuda this time tends to stay very small because (I think) the
//waxpby function returns before the cuda calculation finishes. So if we
//don't artificially make this time large, this loop will go on forever.
    if (tWAXPY < 1.e-2) tWAXPY = 1.e-2;
#endif
  }

  Scalar waxpy_flops = len*3.0*num_iters;
  Scalar waxpy_mflops = tWAXPY>1.e-2 ? 1.e-6 * (waxpy_flops/tWAXPY) : 0;

  std::cout << "waxpby_perf_utest: WAXPBY time: " << tWAXPY << ", len: " << len << ", num_iters: " << num_iters
      << ", MFLOPS: " << waxpy_mflops << std::endl;
  return true;
}

UTEST_CASE(matmat3x3_1)
{
  Scalar A[] = {1, 4, 7, 2, 5, 8, 3, 6, 9};
  Scalar B[] = {1, 1, 1, 2, 2, 2, 3, 3, 3};
  Scalar C[9];

  miniFE::matmat3x3<Scalar>(A, B, C);

  TEST_EQUAL(C[0], 6.0);
  TEST_EQUAL(C[1], 15.0);
  TEST_EQUAL(C[2], 24.0);
  TEST_EQUAL(C[3], 12.0);
  TEST_EQUAL(C[4], 30.0);
  TEST_EQUAL(C[5], 48.0);
  TEST_EQUAL(C[6], 18.0);
  TEST_EQUAL(C[7], 45.0);
  TEST_EQUAL(C[8], 72.0);

  return true;
}

UTEST_CASE(matmat3x3_X_3xn_1)
{
  Scalar A[] = {1, 4, 7, 2, 5, 8, 3, 6, 9};
  Scalar B[] = {1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6};
  Scalar C[18];

  miniFE::matmat3x3_X_3xn<Scalar>(A, 6, B, C);

  TEST_EQUAL(C[0], 6.0);
  TEST_EQUAL(C[1], 15.0);
  TEST_EQUAL(C[2], 24.0);
  TEST_EQUAL(C[3], 12.0);
  TEST_EQUAL(C[4], 30.0);
  TEST_EQUAL(C[5], 48.0);
  TEST_EQUAL(C[6], 18.0);
  TEST_EQUAL(C[7], 45.0);
  TEST_EQUAL(C[8], 72.0);
  TEST_EQUAL(C[9], 24.0);
  TEST_EQUAL(C[10], 60.0);
  TEST_EQUAL(C[11], 96.0);
  TEST_EQUAL(C[12], 30.0);
  TEST_EQUAL(C[13], 75.0);
  TEST_EQUAL(C[14], 120.0);
  TEST_EQUAL(C[15], 36.0);
  TEST_EQUAL(C[16], 90.0);
  TEST_EQUAL(C[17], 144.0);

  return true;
}

UTEST_CASE(matTransMat3x3_X_3xn_1)
{
  Scalar A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  Scalar B[] = {1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6};
  Scalar C[18];

  miniFE::matTransMat3x3_X_3xn<Scalar>(A, 6, B, C);

  TEST_EQUAL(C[0], 6.0);
  TEST_EQUAL(C[1], 15.0);
  TEST_EQUAL(C[2], 24.0);
  TEST_EQUAL(C[3], 12.0);
  TEST_EQUAL(C[4], 30.0);
  TEST_EQUAL(C[5], 48.0);
  TEST_EQUAL(C[6], 18.0);
  TEST_EQUAL(C[7], 45.0);
  TEST_EQUAL(C[8], 72.0);
  TEST_EQUAL(C[9], 24.0);
  TEST_EQUAL(C[10], 60.0);
  TEST_EQUAL(C[11], 96.0);
  TEST_EQUAL(C[12], 30.0);
  TEST_EQUAL(C[13], 75.0);
  TEST_EQUAL(C[14], 120.0);
  TEST_EQUAL(C[15], 36.0);
  TEST_EQUAL(C[16], 90.0);
  TEST_EQUAL(C[17], 144.0);

  return true;
}

UTEST_CASE(BoxIterator1)
{
  int box1[3][2] = {{ 0, 2 }, { 0, 2 }, { 0, 2 } };
  miniFE::BoxIterator iter = miniFE::BoxIterator::begin(box1);
  miniFE::BoxIterator end = miniFE::BoxIterator::end(box1);

  for(int iz=box1[2][0]; iz<box1[2][1]; ++iz) {
   for(int iy=box1[1][0]; iy<box1[1][1]; ++iy) {
    for(int ix=box1[0][0]; ix<box1[0][1]; ++ix) {
      TEST_EQUAL((iter == end), false);
      TEST_EQUAL(ix, iter.x);
      TEST_EQUAL(iy, iter.y);
      TEST_EQUAL(iz, iter.z);
      ++iter;
    }
   }
  }

  TEST_EQUAL((iter == end), true);

  return true;
}

UTEST_CASE(BoxIterator_get_coords)
{
  const int nx=2;
  const int ny=3;
  const int nz=4;
  int box1[3][2] = {{ 0, nx }, { 0, ny }, { 0, nz } };
  miniFE::BoxIterator iter = miniFE::BoxIterator::begin(box1);
  miniFE::BoxIterator end = miniFE::BoxIterator::end(box1);

  for(; iter!=end; ++iter) {
    int elemID = miniFE::get_id<int>(nx,ny,nz,iter.x,iter.y,-iter.z);
    int x, y, z;
    miniFE::get_coords<int>(elemID, nx,ny,nz, x,y,z);
    TEST_EQUAL(x,iter.x);
    TEST_EQUAL(y,iter.y);
    TEST_EQUAL(z,-iter.z);
  }

  return true;
}

#endif

