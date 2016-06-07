#ifndef _LockingMatrix_hpp_
#define _LockingMatrix_hpp_

//@HEADER
// ************************************************************************
// 
//               miniFE: simple finite-element assembly and linear-solve
//                 Copyright (2006) Sandia Corporation
// 
// Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
// license for use of this work by or on behalf of the U.S. Government.
// 
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//  
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//  
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// Questions? Contact Michael A. Heroux (maherou@sandia.gov) 
// 
// ************************************************************************
//@HEADER

#include <vector>

#include <Lock.hpp>

namespace miniFE {

template<typename MatrixType>
class LockingMatrix {
public:
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;
  typedef typename MatrixType::ScalarType Scalar;

  LockingMatrix(MatrixType& A) : A_(A), myFirstRow_(0), myLastRow_(0), numMyRows_(0), row_locks_()
  {
    if (A_.rows.size() > 0) {
      myFirstRow_ = A_.rows[0];
      myLastRow_ = A_.rows[A_.rows.size()-1];
    }
    numMyRows_ = myLastRow_-myFirstRow_+1;
    row_locks_.resize(numMyRows_);
  }

  void sum_in(GlobalOrdinal row, size_t row_len, const GlobalOrdinal* col_indices, const Scalar* values)
  {
    int local_row = row - myFirstRow_;
    if (local_row >= 0 && local_row < numMyRows_) {
      LockM<int> lock(row_locks_[local_row]);
      sum_into_row(row, row_len, col_indices, values, A_);
    }
  }

private:
  MatrixType& A_;
  GlobalOrdinal myFirstRow_;
  GlobalOrdinal myLastRow_;
  size_t numMyRows_;
  std::vector<tbb::atomic<int> > row_locks_;
};

}//namespace miniFE

#endif

