#ifndef FUSEDMATVECDOTOP_HPP_
#define FUSEDMATVECDOTOP_HPP_

#ifndef KERNEL_PREFIX
#define KERNEL_PREFIX
#endif

template <typename MatrixType,
          typename VectorType>
struct FusedMatvecDotOp {

  typedef typename VectorType::GlobalOrdinalType GlobalOrdinalType;
  typedef typename VectorType::LocalOrdinalType LocalOrdinalType;
  typedef typename VectorType::ScalarType ScalarType;
  typedef ScalarType ReductionType;

  size_t n;

  const LocalOrdinalType*  Arowoffsets;
  const GlobalOrdinalType* Acols;
  const ScalarType*        Acoefs;

  const ScalarType* x;
        ScalarType* y;
  ScalarType beta;

  ReductionType result;

  inline FusedMatvecDotOp() {
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

  inline KERNEL_PREFIX ScalarType generate(int row)
  {
    //we count on the caller (ComputeNode) to pass in 'row'
    //in range 0..n-1
  
    ScalarType sum = beta*y[row];

    for(LocalOrdinalType i=Arowoffsets[row]; i<Arowoffsets[row+1]; ++i) {
      sum += Acoefs[i]*x[Acols[i]];
    }

    y[row] = sum;
    return x[row]*sum;
  }
};

#endif
