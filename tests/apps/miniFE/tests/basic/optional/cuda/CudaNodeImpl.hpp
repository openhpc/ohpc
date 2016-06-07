#ifndef CUDANODE_IMPL_HPP_
#define CUDANODE_IMPL_HPP_

#include <CudaNode.hpp>
#include <cuda.h>
#include <cuda_runtime.h>
#include <cutil_inline_runtime.h>
#include <stdlib.h>
#include <stdexcept>

// TODO: consider using cudaMallocHost to allocate page-locked host memory
//       this speeds up transfer between device and host, and could be very 
//       useful in the case of Import/Export multivector operations

#endif
