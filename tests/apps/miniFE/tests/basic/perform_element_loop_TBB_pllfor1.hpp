#ifndef _perform_element_loop_TBB_pllfor1_hpp_
#define _perform_element_loop_TBB_pllfor1_hpp_

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

#ifdef MINIFE_HAVE_TBB

#include <LockingMatrix.hpp>
#include <LockingVector.hpp>
#include <BoxIterator.hpp>
#include <simple_mesh_description.hpp>
#include <SparseMatrix_functions.hpp>
#include <Hex8_box_utils.hpp>
#include <Hex8_ElemData.hpp>
#include <mytimer.hpp>

namespace miniFE {

//---------------------------------------------------------------------

template<typename GlobalOrdinal,typename Scalar,
         typename MatrixType, typename VectorType>
struct FEAssembleSumInto {
  const simple_mesh_description<GlobalOrdinal>* mesh;
  GlobalOrdinal* elemIDs;
  LockingMatrix<MatrixType>* A;
  LockingVector<VectorType>* b;

inline void operator()(int i)
{
  ElemData<GlobalOrdinal,Scalar> elem_data;
  GlobalOrdinal elemID = elemIDs[i];
  get_elem_nodes_and_coords(*mesh, elemID, elem_data.elem_node_ids,
                            elem_data.elem_node_coords);
  compute_element_matrix_and_vector(elem_data);
  sum_into_global_linear_system(elem_data, *A, *b);
}
};

template<typename GlobalOrdinal,
         typename MatrixType, typename VectorType>
void
perform_element_loop(const simple_mesh_description<GlobalOrdinal>& mesh,
                     const Box& local_elem_box,
                     MatrixType& A, VectorType& b,
                     Parameters& params)
{
  typedef typename MatrixType::ScalarType Scalar;

  if (A.rows.size() == 0) return;

  int num_threads = params.numthreads;

  timer_type t0 = mytimer();

  //We will iterate the local-element-box (local portion of the mesh), and
  //assemble the FE operators into the global sparse linear-system.
  
  int global_elems_x = mesh.global_box[0][1];
  int global_elems_y = mesh.global_box[1][1];
  int global_elems_z = mesh.global_box[2][1];

  GlobalOrdinal num_elems = get_num_ids<GlobalOrdinal>(local_elem_box);
  std::vector<GlobalOrdinal> elemIDs(num_elems);

  BoxIterator iter = BoxIterator::begin(local_elem_box);
  BoxIterator end  = BoxIterator::end(local_elem_box);

  for(size_t i=0; iter != end; ++iter, ++i) {
    elemIDs[i] = get_id<GlobalOrdinal>(global_elems_x, global_elems_y, global_elems_z,
                                       iter.x, iter.y, iter.z);
  }

  LockingMatrix<MatrixType> lockingA(A);
  LockingVector<VectorType> lockingb(b);

  FEAssembleSumInto<GlobalOrdinal,Scalar,MatrixType,VectorType> fe_op;
  fe_op.mesh = &mesh;
  fe_op.elemIDs = &elemIDs[0];
  fe_op.A = &lockingA;
  fe_op.b = &lockingb;
  
  typedef typename VectorType::ComputeNodeType ComputeNodeType;

  ComputeNodeType& compute_node = b.compute_node;

  compute_node.parallel_for(elemIDs.size(), fe_op);

  std::cout << "\n{number of matrix conflicts: " << miniFE_num_matrix_conflicts << "}"<<std::endl;
  std::cout << "{number of vector conflicts: " << miniFE_num_vector_conflicts << "}"<<std::endl;
}

}//namespace miniFE

#else
#error "ERROR, this file shouldn't be compiled if MINIFE_HAVE_TBB is not defined."
#endif

#endif

