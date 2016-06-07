#define CUDANODE_INCLUDE_PARALLEL_FOR

// include for CudaNode method implementations
#include <CudaNode.cuh>

// includes for all operators for which Matrix needs support
#include <MatvecOp.hpp>
#include <MatrixInitOp.hpp>
#include <MatrixCopyOp.hpp>

#include <Vector.hpp>
#include <SparseMatrix.hpp>

// explicit instantiations for Matrix class
#define EXPLICIT_MATRIX_SUPPORT(MATRIX,VECTOR) \
template void CUDANode::parallel_for<MatvecOp< MATRIX, VECTOR > >(int , MatvecOp< MATRIX, VECTOR >);

typedef miniFE::SparseMatrix<MINIFE_SCALAR,MINIFE_LOCAL_ORDINAL,MINIFE_GLOBAL_ORDINAL,CUDANode> Matrix_type;
typedef miniFE::Vector<MINIFE_SCALAR,MINIFE_LOCAL_ORDINAL,MINIFE_GLOBAL_ORDINAL,CUDANode> Vector_type;

EXPLICIT_MATRIX_SUPPORT(Matrix_type,Vector_type)

