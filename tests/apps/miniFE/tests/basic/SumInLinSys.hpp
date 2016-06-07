#ifndef _SUMINLINSYS_HPP_
#define _SUMINLINSYS_HPP_

#include <Hex8_enums.hpp>
#include <LockingMatrix.hpp>
#include <LockingVector.hpp>

template<typename GlobalOrdinal,typename Scalar,
         typename MatrixType, typename VectorType>
struct SumInLinSys {
  GlobalOrdinal* node_ordinals;
  Scalar* elem_diffusion_matrix;
  Scalar* elem_source_vector;
  miniFE::LockingMatrix<MatrixType>* A;
  miniFE::LockingVector<VectorType>* b;

inline void operator()(int i)
{
  size_t nnodes = miniFE::Hex8::numNodesPerElem;
  GlobalOrdinal* node_ords = node_ordinals+i*nnodes;
  Scalar* diffusionMat = elem_diffusion_matrix+i*nnodes*nnodes;
  Scalar* sourceVec = elem_source_vector+i*nnodes;
  for(size_t ii=0; ii<nnodes; ++ii) {
    GlobalOrdinal row = node_ords[ii];
    A->sum_in(row, nnodes, node_ords,
              &(diffusionMat[ii*nnodes]));
    b->sum_in(1, &row, &(sourceVec[ii]));
  }
}

};

#endif
