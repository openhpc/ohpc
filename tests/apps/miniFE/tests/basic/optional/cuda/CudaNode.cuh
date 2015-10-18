#ifndef CUDANODE_CUH_
#define CUDANODE_CUH_

#include <stdio.h>
#include <cuda.h>
#include <sharedmem.cuh>
#include <cutil_inline_runtime.h>
#include <cublas.h>

// must define this before including any kernels
#define KERNEL_PREFIX __device__ __host__

#include <CudaNode.hpp>

#include <DotOp.hpp>

#ifdef CUDANODE_INCLUDE_PARALLEL_FOR
template <class WDP>
__global__ void
Tkern1D(int length, WDP wd, int stride)
{
  unsigned int i = blockIdx.x*blockDim.x + threadIdx.x;
  while(i < length) {
    wd(i);
    i += stride;
  }
}

template <class WDP>
void CUDANode::parallel_for(int length, WDP wd) {
  if (length == 0) return;
  unsigned int stride = numThreads_ * numBlocks_;
  Tkern1D<WDP> <<< numBlocks_, numThreads_ >>>(length,wd,stride);
}
#endif // parallel_for

#ifdef CUDANODE_INCLUDE_PARALLEL_REDUCE
template<typename SCALAR>
void call_dot(DotOp<SCALAR>& wd)
{
  printf("ERROR, unknown scalar-type, skipping cuda dot-product.\n");
}
template<>
void call_dot(DotOp<double>& wd)
{
  wd.result = cublasDdot(wd.n, wd.x, 1, wd.y, 1);
}
template<>
void call_dot(DotOp<float>& wd)
{
  wd.result = cublasSdot(wd.n, wd.x, 1, wd.y, 1);
}

template <class WDP>
void CUDANode::parallel_reduce(int length, WDP& wd) 
{
  if (length == 1) {
    wd.result = wd.generate(0);
    return;
  }

  call_dot(wd);
}
#endif // parallel_reduce

#endif
