#ifndef _ELLMatrix_hpp_
#define _ELLMatrix_hpp_

//@HEADER
// ************************************************************************
//
// MiniFE: Simple Finite Element Assembly and Solve
// Copyright (2006-2013) Sandia Corporation
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
//
// ************************************************************************
//@HEADER

#include <cstddef>
#include <vector>
#include <algorithm>
#ifdef HAVE_MPI
#include <mpi.h>
#endif

namespace miniFE {

template<typename Scalar,
         typename LocalOrdinal,
         typename GlobalOrdinal>
struct
ELLMatrix {
  ELLMatrix()
   : has_local_indices(false),
     rows(),
     cols(), coefs(),
     num_cols(0),
     num_cols_per_row(0)
#ifdef HAVE_MPI
     ,external_index(), external_local_index(), elements_to_send(),
      neighbors(), recv_length(), send_length(), send_buffer(), request()
#endif
  {
  }

  ~ELLMatrix()
  {}

  typedef Scalar        ScalarType;
  typedef LocalOrdinal  LocalOrdinalType;
  typedef GlobalOrdinal GlobalOrdinalType;

  bool                       has_local_indices;
  std::vector<GlobalOrdinal> rows;
  std::vector<GlobalOrdinal> cols;
  std::vector<Scalar>        coefs;
  LocalOrdinal               num_cols;
  LocalOrdinal               num_cols_per_row;

#ifdef HAVE_MPI
  std::vector<GlobalOrdinal> external_index;
  std::vector<GlobalOrdinal>  external_local_index;
  std::vector<GlobalOrdinal> elements_to_send;
  std::vector<int>           neighbors;
  std::vector<LocalOrdinal>  recv_length;
  std::vector<LocalOrdinal>  send_length;
  std::vector<Scalar>        send_buffer;
  std::vector<MPI_Request>   request;
#endif

  size_t num_nonzeros() const
  {
    return rows.size()*num_cols_per_row;
  }

  void reserve_space(unsigned nrows, unsigned ncols_per_row)
  {
    rows.resize(nrows);
    cols.resize(nrows * ncols_per_row);
    coefs.resize(nrows * ncols_per_row);
    num_cols_per_row = ncols_per_row;
  }

  void get_row_pointers(GlobalOrdinalType row, size_t& row_length,
                        GlobalOrdinalType*& cols_ptr,
                        ScalarType*& coefs_ptr)
  {
    ptrdiff_t local_row = -1;
    //first see if we can get the local-row index using fast direct lookup:
    if (rows.size() >= 1) {
      ptrdiff_t idx = row - rows[0];
      if (idx < rows.size() && rows[idx] == row) {
        local_row = idx;
      }
    }
 
    //if we didn't get the local-row index using direct lookup, try a
    //more expensive binary-search:
    if (local_row == -1) {
      typename std::vector<GlobalOrdinal>::iterator row_iter =
          std::lower_bound(rows.begin(), rows.end(), row);
  
      //if we still haven't found row, it's not local so jump out:
      if (row_iter == rows.end() || *row_iter != row) {
        row_length = 0;
        return;
      }
  
      local_row = row_iter - rows.begin();
    }

    cols_ptr = &cols[local_row*num_cols_per_row];
    coefs_ptr = &coefs[local_row*num_cols_per_row];
    
    int idx = num_cols_per_row-1;
    while(idx>=0) {
      if (cols_ptr[idx] != 0) break;
      --idx;
    }
    row_length = idx+1;
  }
};

}//namespace miniFE

#endif

