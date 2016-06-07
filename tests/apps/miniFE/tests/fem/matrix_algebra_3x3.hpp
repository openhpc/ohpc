#ifndef _matrix_algebra_3x3_hpp_
#define _matrix_algebra_3x3_hpp_

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
#ifdef __CUDACC__
  __host__ __device__
#endif
KERNEL_PREFIX void fill(Scalar* begin, Scalar* end, const Scalar& val)
{
  while(begin != end) {*begin++ = val;}
}

template<typename Scalar>
KERNEL_PREFIX void inverse_and_determinant3x3(const Scalar* J, Scalar* invJ, Scalar& detJ)
{
  //hardwired "3x3" in function-name allows us to assume
  //that J and invJ have length 9:

  Scalar J00 = J[0];
  Scalar J01 = J[1];
  Scalar J02 = J[2];

  Scalar J10 = J[3];
  Scalar J11 = J[4];
  Scalar J12 = J[5];

  Scalar J20 = J[6];
  Scalar J21 = J[7];
  Scalar J22 = J[8];

  Scalar term0 = J22*J11 - J21*J12;
  Scalar term1 = J22*J01 - J21*J02;
  Scalar term2 = J12*J01 - J11*J02;

  detJ = J00*term0 - J10*term1 + J20*term2;

  Scalar inv_detJ = 1.0/detJ;

  invJ[0] =  term0*inv_detJ;
  invJ[1] = -term1*inv_detJ;
  invJ[2] =  term2*inv_detJ;

  invJ[3] = -(J22*J10 - J20*J12)*inv_detJ;
  invJ[4] =  (J22*J00 - J20*J02)*inv_detJ;
  invJ[5] = -(J12*J00 - J10*J02)*inv_detJ;

  invJ[6] =  (J21*J10 - J20*J11)*inv_detJ;
  invJ[7] = -(J21*J00 - J20*J01)*inv_detJ;
  invJ[8] =  (J11*J00 - J10*J01)*inv_detJ;
}

template<typename Scalar>
KERNEL_PREFIX void matmat3x3(const Scalar* A, const Scalar* B, Scalar* C)
{
  //hardwired "3x3" in function-name allows us to assume args have length 9:
  //A,B,C are all assumed to be ordered such that columns are contiguous.

  const Scalar zero = 0;
  miniFE::fill(C, C+9, zero);

  for(int i=0; i<3; ++i) {
    for(int j=0; j<3; ++j) {
      C[i+j*3] = A[i+0]*B[j*3+0]
               + A[i+3]*B[j*3+1]
               + A[i+6]*B[j*3+2];
    }
  }
}

template<typename Scalar>
KERNEL_PREFIX Scalar determinant3x3(const Scalar* J)
{
  //hardwired "3x3" in function-name allows us to assume that J has length 9:

  Scalar J00 = J[0];
  Scalar J01 = J[1];
  Scalar J02 = J[2];

  Scalar J10 = J[3];
  Scalar J11 = J[4];
  Scalar J12 = J[5];

  Scalar J20 = J[6];
  Scalar J21 = J[7];
  Scalar J22 = J[8];

  Scalar term0 = J22*J11 - J21*J12;
  Scalar term1 = J22*J01 - J21*J02;
  Scalar term2 = J12*J01 - J11*J02;

  Scalar detJ = J00*term0 - J10*term1 + J20*term2;

  return detJ;
}

template<typename Scalar>
KERNEL_PREFIX void matmat3x3_X_3xn(const Scalar* A, int n, const Scalar* B, Scalar* C)
{
  //A is 3x3, B is 3xn. So C is also 3xn.
  //A,B,C are all assumed to be ordered such that columns are contiguous.

  Scalar* Cj = C;
  const Scalar* Bj = B;
  for(int j=0; j<n; ++j) {
    Cj[0] = A[0]*Bj[0] + A[3]*Bj[1] + A[6]*Bj[2];
    Cj[1] = A[1]*Bj[0] + A[4]*Bj[1] + A[7]*Bj[2];
    Cj[2] = A[2]*Bj[0] + A[5]*Bj[1] + A[8]*Bj[2];
    Bj += 3;
    Cj += 3;
  }
}

template<typename Scalar>
KERNEL_PREFIX void matTransMat3x3_X_3xn(const Scalar* A, int n, const Scalar* B, Scalar* C)
{
  //A is 3x3, B is 3xn. So C is also 3xn.
  //A,B,C are all assumed to be ordered such that columns are contiguous.

  Scalar* Cj = C;
  const Scalar* Bj = B;
  for(int j=0; j<n; ++j) {
    Cj[0] = A[0]*Bj[0] + A[1]*Bj[1] + A[2]*Bj[2];
    Cj[1] = A[3]*Bj[0] + A[4]*Bj[1] + A[5]*Bj[2];
    Cj[2] = A[6]*Bj[0] + A[7]*Bj[1] + A[8]*Bj[2];
    Bj += 3;
    Cj += 3;
  }
}

}//namespace miniFE

#endif

