#ifndef _gauss_pts_hpp_
#define _gauss_pts_hpp_

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

#ifndef KERNEL_PREFIX 
#define KERNEL_PREFIX
#endif

namespace miniFE {

template<typename Scalar>
inline
KERNEL_PREFIX void gauss_pts(int N, Scalar* pts, Scalar* wts)
{
  const Scalar x2 = 0.577350269; // 1.0/sqrt(3.0)
  const Scalar x3 = 0.77459667; // sqrt(3.0/5.0)
  const Scalar w1 = 0.55555556; // 5.0/9.0
  const Scalar w2 = 0.88888889; // 8.0/9.0

  switch(N) {
  case 1:
    pts[0] = 0.0; wts[0] = 2.0;
    break;
  case 2:
    pts[0] = -x2; wts[0] = 1.0;
    pts[1] = x2;  wts[1] = 1.0;
    break;
  case 3:
    pts[0] =  -x3;  wts[0] = w1;
    pts[1] =  0.0;  wts[1] = w2;
    pts[2] =   x3;  wts[2] = w1;
    break;
  default:
    break;
  }
}

}//namespace miniFE

#endif

