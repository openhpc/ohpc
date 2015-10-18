#ifndef _LockingVector_hpp_
#define _LockingVector_hpp_

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

template<typename VectorType>
class LockingVector {
public:
  typedef typename VectorType::GlobalOrdinalType GlobalOrdinal;
  typedef typename VectorType::ScalarType Scalar;

  LockingVector(VectorType& x) : x_(x), myFirstRow_(0), myLastRow_(0), numMyRows_(0), row_locks_()
  {
    if (x_.local_size > 0) {
      myFirstRow_ = x_.startIndex;
      myLastRow_ = myFirstRow_ + x_.local_size - 1;
    }
    numMyRows_ = myLastRow_-myFirstRow_+1;
    row_locks_.resize(numMyRows_);
  }

  void sum_in(size_t num_indices, const GlobalOrdinal* indices, const Scalar* values)
  {
    for(int i=0; i<num_indices; ++i) {
      GlobalOrdinal row = indices[i];
      int local_row = row - myFirstRow_;
      if (local_row >= 0 && local_row < numMyRows_) {
        LockV<int> lock(row_locks_[local_row]);
        sum_into_vector(1, &row, &values[i], x_);
      }
    }
  }

private:
  VectorType& x_;
  GlobalOrdinal myFirstRow_;
  GlobalOrdinal myLastRow_;
  size_t numMyRows_;
  std::vector<tbb::atomic<int> > row_locks_;
};

}//namespace miniFE

#endif

