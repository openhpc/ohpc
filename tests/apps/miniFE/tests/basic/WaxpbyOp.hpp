#ifndef WAXPBYOP_HPP_
#define WAXPBYOP_HPP_

#ifndef KERNEL_PREFIX 
#define KERNEL_PREFIX
#endif

template <class Scalar>
struct WaxpbyOp {
      Scalar* w;
  const Scalar* x;
  const Scalar* y;
  Scalar alpha, beta;
  size_t n;
  KERNEL_PREFIX void operator()(size_t i) const
  {
    //here we count on the caller (ComputeNode) to pass in 'i'
    //that is in the range 0..n-1
    w[i] = alpha*x[i] + beta*y[i];
  }
};

template <class Scalar>
struct FusedWaxpbyOp {
      Scalar* w;
  const Scalar* x;
  const Scalar* y;
  Scalar alpha, beta;
      Scalar* w2;
  const Scalar* x2;
  const Scalar* y2;
  Scalar alpha2, beta2;
  size_t n;
  KERNEL_PREFIX void operator()(size_t i) const
  {
    //here we count on the caller (ComputeNode) to pass in 'i'
    //that is in the range 0..n-1
    w[i] = alpha*x[i] + beta*y[i];
    w2[i] = alpha2*x2[i] + beta2*y2[i];
  }
};

#endif
