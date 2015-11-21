#ifndef _assemble_FE_data_hpp_
#define _assemble_FE_data_hpp_

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

#include <box_utils.hpp>
#include <simple_mesh_description.hpp>

#ifdef MINIFE_HAVE_TBB
//#include <perform_element_loop_TBB_pipe.hpp>
#include <perform_element_loop_TBB_pllfor1.hpp>
//#include <perform_element_loop_TBB_pllfor2.hpp>
#else
#include <perform_element_loop.hpp>
#endif

namespace miniFE {

template<typename MatrixType,
         typename VectorType>
void
assemble_FE_data(const simple_mesh_description<typename MatrixType::GlobalOrdinalType>& mesh,
                 MatrixType& A,
                 VectorType& b,
                 Parameters& params)
{
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;

  int global_elems_x = mesh.global_box[0][1];
  int global_elems_y = mesh.global_box[1][1];
  int global_elems_z = mesh.global_box[2][1];

  Box local_elem_box;
  copy_box(mesh.local_box, local_elem_box);

  if (get_num_ids<GlobalOrdinal>(local_elem_box) < 1) {
    return;
  }

  //
  //We want the element-loop to loop over our (processor-local) domain plus a
  //ghost layer, so we can assemble the complete linear-system without doing
  //any communication.
  //
  int ghost = 1;
  if (local_elem_box[0][0] > 0) local_elem_box[0][0] -= ghost;
  if (local_elem_box[1][0] > 0) local_elem_box[1][0] -= ghost;
  if (local_elem_box[2][0] > 0) local_elem_box[2][0] -= ghost;
  if (local_elem_box[0][1] < global_elems_x) local_elem_box[0][1] += ghost;
  if (local_elem_box[1][1] < global_elems_y) local_elem_box[1][1] += ghost;
  if (local_elem_box[2][1] < global_elems_z) local_elem_box[2][1] += ghost;

  perform_element_loop(mesh, local_elem_box, A, b, params);
}
                      
}//namespace miniFE

#endif

