#ifndef _Vector_hpp_
#define _Vector_hpp_

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

#include <MemInitOp.hpp>

namespace miniFE {


template<typename Scalar,
         typename LocalOrdinal,
         typename GlobalOrdinal,
         typename ComputeNode>
struct Vector {
  typedef ComputeNode ComputeNodeType;
  typedef Scalar ScalarType;
  typedef LocalOrdinal LocalOrdinalType;
  typedef GlobalOrdinal GlobalOrdinalType;

  Vector(GlobalOrdinal startIdx, LocalOrdinal local_sz, ComputeNode& cn)
   : startIndex(startIdx),
     local_size(local_sz),
     coefs(local_size),
     compute_node(cn)
  {
    MemInitOp<Scalar> mem_init;
    mem_init.ptr = &coefs[0];
    mem_init.n = local_size;
#ifdef MINIFE_HAVE_CUDA
//we don't want to run this mem-init kernel on cuda, we want
//to just run it locally on the host.
    for(size_t i=0; i<mem_init.n; ++i) {
      mem_init(i);
    }
#else
    cn.parallel_for(local_size, mem_init);
#endif
  }

  ~Vector()
  {
  }

  GlobalOrdinal startIndex;
  LocalOrdinal local_size;
  std::vector<Scalar> coefs;
  ComputeNode& compute_node;
};


}//namespace miniFE

#endif

