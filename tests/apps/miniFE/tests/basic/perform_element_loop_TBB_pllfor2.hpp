#ifndef _perform_element_loop_TBB_pllfor2_hpp_
#define _perform_element_loop_TBB_pllfor2_hpp_

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

#ifdef MINIFE_HAVE_CUDA
#include <CudaNode.hpp>
#endif

#include <LockingMatrix.hpp>
#include <LockingVector.hpp>
#include <ElemData.hpp>
#include <BoxIterator.hpp>
#include <simple_mesh_description.hpp>
#include <SparseMatrix_functions.hpp>
#include <Hex8_box_utils.hpp>
#include <GetNodesCoords.hpp>
#include <FEComputeElem.hpp>
#include <SumInLinSys.hpp>
#include <mytimer.hpp>

namespace miniFE {

//---------------------------------------------------------------------

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

  std::vector<GlobalOrdinal> node_ordinals(num_elems*Hex8::numNodesPerElem);
  std::vector<Scalar> node_coords(num_elems*Hex8::numNodesPerElem*Hex8::spatialDim);
  std::vector<Scalar> elem_matrices(num_elems*Hex8::numNodesPerElem*Hex8::numNodesPerElem);
  std::vector<Scalar> elem_vectors(num_elems*Hex8::numNodesPerElem);

  LockingMatrix<MatrixType> lockingA(A);
  LockingVector<VectorType> lockingb(b);

  GetNodesCoords<GlobalOrdinal,Scalar> get_nodes_coords;
  get_nodes_coords.elemIDs = &elemIDs[0];
  get_nodes_coords.mesh = &mesh;
  get_nodes_coords.node_ordinals = &node_ordinals[0];
  get_nodes_coords.elem_node_coords = &node_coords[0];
  
  typedef typename VectorType::ComputeNodeType ComputeNodeType;

  ComputeNodeType& compute_node = b.compute_node;

  compute_node.parallel_for(elemIDs.size(), get_nodes_coords);

  timer_type t_gn = mytimer() - t0;
  t0 = mytimer();

#ifdef MINIFE_HAVE_CUDA
  CUDANode& elem_compute_node = CUDANode::singleton();
#else
  ComputeNodeType& elem_compute_node = compute_node;
#endif
  timer_type t_ccn = mytimer() - t0;
  t0 = mytimer();

  Scalar* d_node_coords = elem_compute_node.get_buffer(&node_coords[0], node_coords.size());
  Scalar* d_elem_matrices = elem_compute_node.get_buffer(&elem_matrices[0], elem_matrices.size());
  Scalar* d_elem_vectors  = elem_compute_node.get_buffer(&elem_vectors[0], elem_vectors.size());

  elem_compute_node.copy_to_buffer(&node_coords[0], node_coords.size(), d_node_coords);

  FEComputeElem<GlobalOrdinal,Scalar> fe_compute_elem;
  fe_compute_elem.elem_node_coords = &d_node_coords[0];
  fe_compute_elem.elem_diffusion_matrix = &d_elem_matrices[0];
  fe_compute_elem.elem_source_vector = &d_elem_vectors[0];

  elem_compute_node.parallel_for(elemIDs.size(), fe_compute_elem);

  elem_compute_node.copy_from_buffer(&elem_matrices[0], elem_matrices.size(), d_elem_matrices);
  elem_compute_node.copy_from_buffer(&elem_vectors[0], elem_vectors.size(), d_elem_vectors);

 timer_type t_ce = mytimer() - t0;

  t0 = mytimer();
  SumInLinSys<GlobalOrdinal,Scalar,MatrixType,VectorType> sum_in;
  sum_in.node_ordinals = &node_ordinals[0];
  sum_in.elem_diffusion_matrix = &elem_matrices[0];
  sum_in.elem_source_vector = &elem_vectors[0];
  sum_in.A = &lockingA;
  sum_in.b = &lockingb;

  compute_node.parallel_for(elemIDs.size(), sum_in);

  timer_type t_si = mytimer() - t0;
  std::cout << "time to get nodes/coords: " << t_gn << std::endl;
  std::cout << "time to create compute-node: " << t_ccn << ", time to compute elements: " << t_ce << std::endl;
  std::cout << "time to sum into linsys: " << t_si << std::endl;
  std::cout << "\n{number of matrix conflicts: " << miniFE_num_matrix_conflicts << "}"<<std::endl;
  std::cout << "{number of vector conflicts: " << miniFE_num_vector_conflicts << "}"<<std::endl;
}

}//namespace miniFE

#else
#error "ERROR, this file shouldn't be compiled if MINIFE_HAVE_TBB is not defined."
#endif

#endif

