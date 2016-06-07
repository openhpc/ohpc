#ifndef stk_algsup_CudaCall_hpp
#define stk_algsup_CudaCall_hpp

#include <cuda.h>
#include <cuda_runtime.h>

//----------------------------------------------------------------
inline
void stk_cuda_call(cudaError err , const char* name )
{
  if ( err != cudaSuccess ) {
    fprintf(stderr, "%s error: %s\n",name, cudaGetErrorString(err) );
    exit(-1);
  }
}

#define CUDA_CALL( cuda_fn ) stk_cuda_call( cuda_fn , #cuda_fn )


#endif

