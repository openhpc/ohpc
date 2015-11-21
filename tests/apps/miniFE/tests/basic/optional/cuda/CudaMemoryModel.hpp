#ifndef _CudaMemoryModel_hpp_
#define _CudaMemoryModel_hpp_

#include <iostream>
#ifdef MINIFE_HAVE_CUDA

#include <stdio.h>
#include <stdexcept>
#include <map>

#include <cuda.h>
#include <cuda_runtime.h>
#include <CudaCall.hpp>

class CudaMemoryModel {
  public:
    CudaMemoryModel()
     : host_to_device_map(),
       device_to_host_map()
    {}

    /** Destructor
     * Upon destruction this class de-allocates all device-buffers that
     * it was tracking.
     */
    virtual ~CudaMemoryModel();

    /** Return a device-pointer corresponding to the given host-ptr and size.
     * The returned device-pointer points to a buffer which has been allocated
     * on the CUDA device with length buf_size*sizeof(T), but not initialized.
     *
     * If a device-pointer has already been allocated for the given host-pointer
     * (by a previous call to this method) then that (previously-allocated) device-pointer
     * is returned.
     */
    template<class T>
    T* get_buffer(const T* host_ptr, size_t buf_size);

    /** Destroy (free) the specified device-pointer.
     *
     * De-allocates the cuda-device buffer.
     */
    template<class T>
    void destroy_buffer(T*& device_ptr);

    /** Copy the contents of the given host-ptr to the given device-ptr.
     * If the given device-ptr is not known (was not created by a previous
     * call to get_buffer), an exception is thrown.
     */
    template<class T>
    void copy_to_buffer(const T* host_ptr, size_t buf_size, T* device_ptr);

    /** Copy the contents of the given device-ptr to the given host-ptr.
     * If the given device-ptr is not known (was not created by a previous
     * call to get_buffer), an exception is thrown.
     */
    template<class T>
    void copy_from_buffer(T* host_ptr, size_t buf_size, const T* device_ptr);

 private:
  std::map<const void*,void*> host_to_device_map;
  std::map<const void*,const void*> device_to_host_map;
};

//------------------------------------------------------------------------------
template<class T>
inline
T* CudaMemoryModel::get_buffer(const T* host_ptr, size_t buf_size)
{
  T* device_ptr = NULL;

  std::map<const void*,void*>::iterator iter = host_to_device_map.find(host_ptr);

  if (iter == host_to_device_map.end()) {
    CUDA_CALL( cudaMalloc( (void**)&device_ptr, sizeof(T)*buf_size) );

    host_to_device_map.insert( std::make_pair(host_ptr, device_ptr) );
    device_to_host_map.insert( std::make_pair(device_ptr, host_ptr) );
  }
  else {
    device_ptr = reinterpret_cast<T*>(iter->second);
  }

  return device_ptr;
}

//------------------------------------------------------------------------------
template<class T>
inline
void CudaMemoryModel::destroy_buffer(T*& device_ptr)
{
  std::map<const void*,const void*>::iterator iter = device_to_host_map.find(device_ptr);
  if (iter != device_to_host_map.end()) {
    const void* host_ptr = iter->second;
    if (host_ptr != NULL) {
      std::map<const void*,void*>::iterator iter2 = host_to_device_map.find(host_ptr);
      if (iter2 != host_to_device_map.end()) {
        host_to_device_map.erase(iter2);
      }
    }
    CUDA_CALL( cudaFree(device_ptr) );
    device_ptr = NULL;
    device_to_host_map.erase(iter);
  }
}

//------------------------------------------------------------------------------
template<class T>
inline
void CudaMemoryModel::copy_to_buffer(const T* host_ptr, size_t buf_size, T* device_ptr)
{
  std::map<const void*,const void*>::iterator iter = device_to_host_map.find(device_ptr);
  if (iter == device_to_host_map.end()) {
    //failed to find device_ptr in device_to_host_map
    throw std::runtime_error("CudaMemoryModel::copy_to_buffer ERROR, device_ptr not known.");
  }

  CUDA_CALL( cudaMemcpy( device_ptr, host_ptr, sizeof(T)*buf_size, cudaMemcpyHostToDevice) );
}

//------------------------------------------------------------------------------
template<class T>
inline
void CudaMemoryModel::copy_from_buffer(T* host_ptr, size_t buf_size, const T* device_ptr)
{
  std::map<const void*,const void*>::iterator iter = device_to_host_map.find(device_ptr);
  if (iter == device_to_host_map.end()) {
    //failed to find device_ptr in device_to_host_map
    throw std::runtime_error("CudaMemoryModel::copy_from_buffer ERROR, device_ptr not known.");
  }

  CUDA_CALL( cudaMemcpy( host_ptr, device_ptr, sizeof(T)*buf_size, cudaMemcpyDeviceToHost) );
}

inline
CudaMemoryModel::~CudaMemoryModel()
{
  std::map<const void*,const void*>::iterator
    iter = device_to_host_map.begin(),
    iter_end = device_to_host_map.end();

  for(; iter!=iter_end; ++iter) {
    //cast away const so we can free the pointer:
    void* dev_ptr = const_cast<void*>(iter->first);
    CUDA_CALL( cudaFree(dev_ptr) );
  }
}

#endif

#endif

