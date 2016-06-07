#ifndef FECOMPUTEELEM_HPP_
#define FECOMPUTEELEM_HPP_

#include <Hex8.hpp>

#ifndef KERNEL_PREFIX 
#define KERNEL_PREFIX
#endif

template<typename GlobalOrdinal,typename Scalar>
struct FEComputeElem {
  Scalar* elem_node_coords;
  Scalar* elem_diffusion_matrix;
  Scalar* elem_source_vector;

inline KERNEL_PREFIX void operator()(int i)
{
  unsigned nnodes = miniFE::Hex8::numNodesPerElem;
  unsigned dim = miniFE::Hex8::spatialDim;
  Scalar* coords = elem_node_coords+i*nnodes*dim;
  Scalar* diffusionMat = elem_diffusion_matrix+i*nnodes*nnodes;
  Scalar* sourceVec = elem_source_vector+i*nnodes;

  miniFE::Hex8::diffusionMatrix(coords, diffusionMat);
  miniFE::Hex8::sourceVector(coords, sourceVec);
}
};

#endif
