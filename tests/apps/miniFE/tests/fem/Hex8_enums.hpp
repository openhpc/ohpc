#ifndef _Hex8_enums_hpp_
#define _Hex8_enums_hpp_

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

namespace miniFE {

namespace Hex8 {

//   !!!!!!!
//Important note: there are places in miniFE code where
//loops over spatialDim are unrolled (spatialDim is assumed to be 3).
//Thus, changing this enum is not enough to make miniFE code
//work for spatialDim values other than 3.
//   !!!!!!!
enum {
  spatialDim = 3,
  numNodesPerElem = 8,
  numGaussPointsPerDim = 2
};

}//namespace Hex8

}//namespace miniFE

#endif

