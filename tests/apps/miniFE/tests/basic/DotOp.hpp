#ifndef DOTOP_HPP_
#define DOTOP_HPP_

template <class Scalar>
struct DotOp {
  typedef Scalar ReductionType;

  const Scalar* x;
  const Scalar* y;

  size_t n;

  ReductionType result;

  inline DotOp() {
    result = identity();
  }

  static inline KERNEL_PREFIX ReductionType identity()
  {
    return 0.0;
  }

  inline KERNEL_PREFIX ReductionType reduce(ReductionType u, ReductionType v) const
  {
    return u+v;
  }

  inline KERNEL_PREFIX Scalar generate(int i) const
  {
    return x[i]*y[i];
  }
};

#endif
