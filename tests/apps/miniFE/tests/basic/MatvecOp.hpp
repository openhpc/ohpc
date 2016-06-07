#ifndef _MatvecOp_hpp_
#define _MatvecOp_hpp_

#ifndef KERNEL_PREFIX
#define KERNEL_PREFIX
#endif

#include <CSRMatrix.hpp>
#include <ELLMatrix.hpp>
#include <ComputeNodeType.hpp>

template<typename MatrixType>
struct MatvecOp {
};

template<>
struct MatvecOp<miniFE::CSRMatrix<MINIFE_SCALAR,MINIFE_LOCAL_ORDINAL,MINIFE_GLOBAL_ORDINAL, ComputeNodeType> > {
  MatvecOp(miniFE::CSRMatrix<MINIFE_SCALAR,MINIFE_LOCAL_ORDINAL,MINIFE_GLOBAL_ORDINAL, ComputeNodeType>& A)
  : n(A.rows.size()),
    Arowoffsets(&A.row_offsets[0]),
    Acols(&A.packed_cols[0]),
    Acoefs(&A.packed_coefs[0])
  {
  }

  size_t n;

  typedef MINIFE_GLOBAL_ORDINAL GlobalOrdinalType;
  typedef MINIFE_LOCAL_ORDINAL LocalOrdinalType;
  typedef MINIFE_SCALAR ScalarType;

  const LocalOrdinalType*  Arowoffsets;
  const GlobalOrdinalType* Acols;
  const ScalarType*        Acoefs;

  const ScalarType* x;
        ScalarType* y;
  ScalarType beta;

  inline KERNEL_PREFIX void operator()(int row)
  {
    //we count on the caller (ComputeNode) to pass in 'row'
    //in range 0..n-1
  
    ScalarType sum = beta*y[row];

    for(LocalOrdinalType i=Arowoffsets[row]; i<Arowoffsets[row+1]; ++i) {
      sum += Acoefs[i]*x[Acols[i]];
    }

    y[row] = sum;
  }

};//struct MatvecOp

template<>
struct MatvecOp<miniFE::ELLMatrix<MINIFE_SCALAR,MINIFE_LOCAL_ORDINAL,MINIFE_GLOBAL_ORDINAL, ComputeNodeType> > {
  MatvecOp(miniFE::ELLMatrix<MINIFE_SCALAR,MINIFE_LOCAL_ORDINAL,MINIFE_GLOBAL_ORDINAL, ComputeNodeType>& A)
  : n(A.rows.size()),
    Acols(&A.cols[0]),
    Acoefs(&A.coefs[0]),
    ncols_per_row(A.num_cols_per_row)
  {
  }

  size_t n;

  typedef MINIFE_GLOBAL_ORDINAL GlobalOrdinalType;
  typedef MINIFE_LOCAL_ORDINAL LocalOrdinalType;
  typedef MINIFE_SCALAR ScalarType;

  const GlobalOrdinalType* Acols;
  const ScalarType*        Acoefs;
  int ncols_per_row;

  const ScalarType* x;
        ScalarType* y;
  ScalarType beta;

  inline KERNEL_PREFIX void operator()(int row)
  {
    //we count on the caller (ComputeNode) to pass in 'row'
    //in range 0..n-1
  
    ScalarType sum = beta*y[row];

    for(LocalOrdinalType i=0; i<ncols_per_row; ++i) {
      GlobalOrdinalType col = Acols[row*ncols_per_row + i];
      ScalarType coef      = Acoefs[row*ncols_per_row + i];
      if (coef != 0) sum += coef*x[col];
    }

    y[row] = sum;
  }

};//struct MatvecOp

#endif

