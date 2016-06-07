#define CUDANODE_INCLUDE_PARALLEL_REDUCE
#define CUDANODE_INCLUDE_PARALLEL_FOR

// include for CudaNode method implementations
#include <CudaNode.cuh>

// includes for all operators for which Vector needs support
#include <WaxpbyOp.hpp>
#include <DotOp.hpp>
#include <MemInitOp.hpp>
#include <FEComputeElem.hpp>

// explicit instantiations for Vectors
#define EXPLICIT_VECTOR_SUPPORT(GLOBALORDINAL, SCALAR) \
template void CUDANode::parallel_for<WaxpbyOp< SCALAR > >(int , WaxpbyOp< SCALAR >); \
template void CUDANode::parallel_reduce< DotOp< SCALAR > >(int ,  DotOp< SCALAR >& ); \
template void CUDANode::parallel_for<FEComputeElem< GLOBALORDINAL, SCALAR > >(int , FEComputeElem< GLOBALORDINAL, SCALAR > );

EXPLICIT_VECTOR_SUPPORT(MINIFE_GLOBAL_ORDINAL, MINIFE_SCALAR)
