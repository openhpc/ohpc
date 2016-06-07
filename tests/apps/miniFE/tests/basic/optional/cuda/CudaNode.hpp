#ifndef CUDANODE_HPP_
#define CUDANODE_HPP_

#include <CudaMemoryModel.hpp>

// forward declaration
class CUDANode;

class CUDANode : public CudaMemoryModel {
  public:

    CUDANode(int device = 0, int numBlocks = -1, int numThreads = 256, int verbose = 1);

    ~CUDANode();

    //@{ Computational methods

    template <class WDP>
    void parallel_for(int length, WDP wdp);

    template <class WDP>
    void parallel_reduce(int length, WDP& wd);

    //@} 

    static CUDANode& singleton(int device=0, int numBlocks=-1, int numThreads=256)
    {
      static CUDANode* cuda_node = NULL;
      if (cuda_node == NULL) {
        cuda_node = new CUDANode(device, numBlocks, numThreads);
      }
      return *cuda_node;
    }
      
  private:
    //template <class WDP, int FirstLevel>
    //void call_reduce(int length, WDP wd, int threads, int blocks, void * d_blkpart);
    // numBlocks_ is 
    // - the number of blocks launched in a call to parallel_for()
    // - not used by parallel_reduce()
    int numBlocks_;
    // numThreads_ is required to be a power-of-two (our requirement) between 1 and 512 (CUDA's requirement). It is:
    // - the maximum number of threads used by parallel_reduce()
    // - the number of threads per block in a call to parallel_for()
    int numThreads_;
    // total global device memory, in bytes
    int totalMem_;

    void expand_blk_mem(size_t size_in_bytes);

    char* h_blk_mem_;
    void* d_blk_mem_;
    size_t blk_mem_size_;

};

#endif
