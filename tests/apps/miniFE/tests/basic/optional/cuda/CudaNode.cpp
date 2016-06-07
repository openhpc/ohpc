#include <CudaNode.hpp>
#include <stdexcept>
#include <iostream>
#include <cutil_inline_runtime.h>

// some CUDA rules of thumb employed here (stolen from slides by Mike Bailey, Oregon State)
// -The number of Blocks should be at least twice the number of MPs 
// -The number of Threads per Block should be a multiple of 64 
// -  192 or 256 are good numbers for Threads/Block 
// We will enforce that numThreads is a power of two (to ease the reduction kernel)
// greater than 64

CUDANode::CUDANode(int device, int numBlocks, int numThreads, int verbose)
: numBlocks_(numBlocks)
, numThreads_(numThreads)
, h_blk_mem_(NULL)
, d_blk_mem_(NULL)
, blk_mem_size_(0)
{
  using std::cout;
  using std::endl;
  using std::runtime_error;
  // enforce that numThreads_ is a multiple of 64
  if (numThreads_ != 64 && numThreads_ != 128 && numThreads_ != 256 && numThreads_ != 512
      && numThreads_ != 1 && numThreads_ != 2 && numThreads_ != 4 && numThreads_ != 8 && numThreads_ != 16
      && numThreads_ != 32) {
//    throw runtime_error("CUDANode::CUDANode(): number of threads per block must be a power of two in [1,512].");
  }
  int deviceCount; cudaGetDeviceCount(&deviceCount); 
  if (device >= deviceCount) {
    if (deviceCount == 0) {
//      throw runtime_error("CUDANode::CUDANode(): system has no CUDA devices.");
    }
    if (verbose) {
      cout << "CUDANode::CUDANode(): specified device number not valid. Using device 0." << endl;
    }
    device = 0;
  }
  cudaDeviceProp deviceProp; 
  int deviceAlreadyBeingUsed = -1;
  cudaGetDevice(&deviceAlreadyBeingUsed);
  if (deviceAlreadyBeingUsed >= 0 && deviceAlreadyBeingUsed < deviceCount) {
    device = deviceAlreadyBeingUsed;
  }
  else {
    cudaSetDevice(device);
  }
  cudaGetDeviceProperties(&deviceProp, device); 
  // as of CUDA 2.1, device prop contains the following fields
  // char name[256]; 
  // size_t totalGlobalMem, sharedMemPerBlock; 
  // int regsPerBlock, warpSize; 
  // size_t memPitch; 
  // int maxThreadsPerBlock, maxThreadsDim[3], maxGridSize[3]; 
  // size_t totalConstMem; 
  // int major, minor;
  // int clockRate; 
  // size_t textureAlignment; 
  // int deviceOverlap; 
  // int multiProcessorCount; 
  // int kernelExecTimeoutEnabled; 
  if (verbose) {
    cout << "CUDANode attached to device #" << device << " \"" << deviceProp.name 
         << "\", of compute capability " << deviceProp.major << "." << deviceProp.minor
         << endl;
  }
  totalMem_ = deviceProp.totalGlobalMem;

  expand_blk_mem(numBlocks_*8);
} 

void CUDANode::expand_blk_mem(size_t size_in_bytes)
{
  if (blk_mem_size_ >= size_in_bytes) return;

  if (d_blk_mem_ != NULL) {
    cutilSafeCallNoSync( cudaFree(d_blk_mem_) );
    delete [] h_blk_mem_;
  }

  cutilSafeCallNoSync( cudaMalloc(&d_blk_mem_, size_in_bytes) );
  h_blk_mem_ = new char[size_in_bytes];
  blk_mem_size_ = size_in_bytes;
}

CUDANode::~CUDANode()
{
  if (d_blk_mem_ != NULL) {
    cutilSafeCallNoSync( cudaFree(d_blk_mem_) );
    d_blk_mem_ = NULL; 
    delete [] h_blk_mem_;
    h_blk_mem_ = NULL;
  }
  blk_mem_size_ = 0;
}

