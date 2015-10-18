#ifndef _ElemData_hpp_
#define _ElemData_hpp_

//@HEADER
// ************************************************************************
//
// MiniFE: Simple Finite Element Assembly and Solve
// Copyright (2006-2013) Sandia Corporation
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
//
// ************************************************************************
//@HEADER

#include <Hex8_enums.hpp>

namespace miniFE {

template<typename GlobalOrdinal, typename Scalar>
struct ElemData {
  ElemData() : nodes_per_elem(Hex8::numNodesPerElem) {}
  ~ElemData(){}

  const size_t nodes_per_elem;
  GlobalOrdinal elem_node_ids[Hex8::numNodesPerElem];
  Scalar grad_vals[Hex8::numGaussPointsPerDim * Hex8::numGaussPointsPerDim * Hex8::numGaussPointsPerDim * Hex8::numNodesPerElem * Hex8::spatialDim];
  Scalar elem_node_coords[Hex8::numNodesPerElem*Hex8::spatialDim];
  Scalar elem_diffusion_matrix[(Hex8::numNodesPerElem*(Hex8::numNodesPerElem+1))/2];
  Scalar elem_source_vector[Hex8::numNodesPerElem];
};

template<typename GlobalOrdinal, typename Scalar>
struct ElemDataPtr {
  ElemDataPtr() : nodes_per_elem(Hex8::numNodesPerElem) {}
  ~ElemDataPtr(){}

  const size_t nodes_per_elem;
  GlobalOrdinal elem_node_ids[Hex8::numNodesPerElem];
  Scalar grad_vals[Hex8::numGaussPointsPerDim * Hex8::numGaussPointsPerDim * Hex8::numGaussPointsPerDim * Hex8::numNodesPerElem * Hex8::spatialDim];
  Scalar elem_node_coords[(Hex8::numNodesPerElem*(Hex8::spatialDim+1))/2];
  Scalar* elem_diffusion_matrix;
  Scalar* elem_source_vector;
};

}//namespace miniFE

#endif

