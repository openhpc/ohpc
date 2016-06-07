#ifndef _NoOpMemoryModel_hpp_
#define _NoOpMemoryModel_hpp_

class NoOpMemoryModel {
  public:
    NoOpMemoryModel(){}
    virtual ~NoOpMemoryModel(){}

    template<class T>
    T* get_buffer(const T* host_ptr, size_t buf_size)
    { return const_cast<T*>(host_ptr); }

    template<class T>
    void destroy_buffer(T*& device_ptr)
    { }

    template<class T>
    void copy_to_buffer(const T* host_ptr, size_t buf_size, T* device_ptr)
    { }

    template<class T>
    void copy_from_buffer(T* host_ptr, size_t buf_size, const T* device_ptr)
    { }
};

#endif

