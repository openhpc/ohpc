#ifndef _Lock_hpp_
#define _Lock_hpp_

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

#ifdef MINIFE_HAVE_TBB

#include <iostream>
#include <tbb/atomic.h>

namespace miniFE {

static tbb::atomic<size_t> miniFE_num_matrix_conflicts;
static tbb::atomic<size_t> miniFE_num_vector_conflicts;

//We have two lock classes, LockM and LockV. The only reason for
//this is so that they can separately track the number of conflicts
//for matrix accesses versus vector accesses (by incrementing the
//above counters).
//The LockingMatrix class uses LockM, LockingVector uses LockV.

template<typename T>
class LockM {
public:
   // Constructors/destructors
   LockM(tbb::atomic<T>& row)
       : locked_row_(row)
   {
     if (++locked_row_ != 1) {
       unsigned counter = 0;
       while(locked_row_ != 1) {
         ++counter;
       }
       ++miniFE_num_matrix_conflicts;
     }
   }
   ~LockM()
   { --locked_row_; }

private:
   tbb::atomic<T>& locked_row_;
   LockM(const LockM&);
   LockM& operator=(const LockM&);
};

template<typename T>
class LockV {
public:
   // Constructors/destructors
   LockV(tbb::atomic<T>& row)
       : locked_row_(row)
   {
     if (++locked_row_ != 1) {
       unsigned counter = 0;
       while(locked_row_ != 1) {
         ++counter;
       }
       ++miniFE_num_vector_conflicts;
     }
   }
   ~LockV()
   { --locked_row_; }

private:
   tbb::atomic<T>& locked_row_;
   LockV(const LockV&);
   LockV& operator=(const LockV&);
};

}//namespace miniFE

#else
#error "ERROR, this file shouldn't be compiled if MINIFE_HAVE_TBB isn't defined."
#endif

#endif

