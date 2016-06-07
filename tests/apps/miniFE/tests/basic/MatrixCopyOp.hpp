#ifndef _MatrixCopyOp_hpp_
#define _MatrixCopyOp_hpp_

template<typename MatrixType>
struct MatrixCopyOp {
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinalType;
  typedef typename MatrixType::LocalOrdinalType LocalOrdinalType;
  typedef typename MatrixType::ScalarType ScalarType;

  const GlobalOrdinalType* src_rows;
  const LocalOrdinalType*  src_rowoffsets;
  const GlobalOrdinalType* src_cols;
  const ScalarType*        src_coefs;

  GlobalOrdinalType* dest_rows;
  LocalOrdinalType*  dest_rowoffsets;
  GlobalOrdinalType* dest_cols;
  ScalarType*        dest_coefs;
  int n;

  inline void operator()(int i)
  {
    dest_rows[i] = src_rows[i];
    dest_rowoffsets[i] = src_rowoffsets[i];
    for(int j=src_rowoffsets[i]; j<src_rowoffsets[i+1]; ++j) {
      dest_cols[j] = src_cols[j];
      dest_coefs[j] = src_coefs[j];
    }
  }
};

#endif

