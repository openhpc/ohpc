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
