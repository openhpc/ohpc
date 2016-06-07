#ifndef MEMINITOP_HPP_
#define MEMINITOP_HPP_

template <class Scalar>
struct MemInitOp {
  Scalar* ptr;
  size_t n;
  inline void operator()(size_t i)
  {
    ptr[i] = 0;
  }
};

#endif
