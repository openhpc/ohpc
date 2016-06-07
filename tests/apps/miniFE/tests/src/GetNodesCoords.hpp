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

#ifndef _GETNODESCOORDS_HPP_
#define _GETNODESCOORDS_HPP_

#include <Hex8_enums.hpp>
#include <simple_mesh_description.hpp>

template<typename GlobalOrdinal,typename Scalar>
struct GetNodesCoords {
  const miniFE::simple_mesh_description<GlobalOrdinal>* mesh;
  GlobalOrdinal* elemIDs;
  GlobalOrdinal* node_ordinals;
  Scalar* elem_node_coords;

inline void operator()(int i)
{
  unsigned nnodes = miniFE::Hex8::numNodesPerElem;
  GlobalOrdinal elemID = elemIDs[i];
  GlobalOrdinal* node_ords = node_ordinals+i*nnodes;
  Scalar* node_coords = elem_node_coords+i*nnodes*miniFE::Hex8::spatialDim;
  get_elem_nodes_and_coords(*mesh, elemID, node_ords, node_coords);
}
};

#endif
