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

#ifndef _Box_hpp_
#define _Box_hpp_

/**
  * a 'Box' is 3 pairs of ints, where each pair specifies a lower
  * and upper bound for one of the 3 spatial dimensions.
  *
  * This struct stores the 3 pairs as a simple array of 6 ints,
  * but defines the bracket operator so that it can be referenced
  * using 2-dimensional array notation like this:
  * int xmin = box[0][0]; int xmax = box[0][1];
  * int ymin = box[1][0]; int ymax = box[1][1];
  * int zmin = box[2][0]; int zmax = box[2][1];
 */
struct Box {
  int ranges[6];
#ifdef __CUDACC__
__host__ __device__ __inline__
#endif
  int* operator[](int xyz) { return &ranges[xyz*2]; }
#ifdef __CUDACC__
__host__ __device__ __inline__
#endif
  const int* operator[](int xyz) const { return &ranges[xyz*2]; }
};

#endif

