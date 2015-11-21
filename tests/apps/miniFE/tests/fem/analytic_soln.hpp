#ifndef _analytic_soln_hpp_
#define _analytic_soln_hpp_

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

#include <cmath>

#ifndef MINIFE_SCALAR
#define MINIFE_SCALAR double;
#endif

namespace miniFE {

typedef MINIFE_SCALAR Scalar;

// The 'soln' function below computes the analytic solution for
// steady state temperature in a brick-shaped domain (formally called
// a rectangular parallelepiped). The inputs to the function are
// the x,y,z coordinates of the point at which temperature is to be
// computed, and the number of terms p,q in the series expansion.
//
// The equations used for the temperature solution are equations 9 and 10
// in section 6.2 of Carslaw & Jaeger, "Conduction of Heat in Solids".
//
// The paralellepiped being used is defined by this domain:
// 0 <= x <= 1.0
// 0 <= y <= 1.0
// 0 <= z <= 1.0
//
// With boundary conditions prescribing the temperature to be 1.0 on
// the x==1.0 face, and 0.0 on all other faces.
//
// Thus, in the equations from Carslaw & Jaeger, the following constants
// are used:
//
// a == b == c == 1.0  (the extents of the domain)
// v1 == 0.0           (temperature at x == 0.0)
// v2 == 1.0           (temperature at x == 1.0)
//

const Scalar PI = 3.141592653589793238462;
const Scalar PI_SQR = PI*PI;
const Scalar term0 = 16.0/(PI_SQR);

inline Scalar fcn_l(int p, int q)
{
  return std::sqrt((2*p+1)*(2*p+1)*PI_SQR + (2*q+1)*(2*q+1)*PI_SQR);
}

inline Scalar fcn(int n, Scalar u)
{
  return (2*n+1)*PI*u;
}

inline Scalar soln(Scalar x, Scalar y, Scalar z, int max_p, int max_q)
{
  Scalar sum = 0;
  for(int p=0; p<=max_p; ++p) {
    const Scalar p21y = fcn(p, y);
    const Scalar sin_py = std::sin(p21y)/(2*p+1);
    for(int q=0; q<=max_q; ++q) {
      const Scalar q21z = fcn(q, z);
      const Scalar sin_qz = std::sin(q21z)/(2*q+1);

      const Scalar l = fcn_l(p, q);

      const Scalar sinh1 = std::sinh(l*x);
      const Scalar sinh2 = std::sinh(l);

      const Scalar tmp = (sinh1*sin_py)*(sin_qz/sinh2);

      //if the scalar l gets too big, sinh(l) becomes inf.
      //if that happens, tmp is a NaN.
      //crude check for NaN:
      //if tmp != tmp, tmp is NaN
      if (tmp == tmp) {
        sum += tmp;
      }
      else {
        //if we got a NaN, break out of this inner loop and go to
        //the next iteration of the outer loop.
        break;
      }
    }
  }
  return term0*sum;
}

}//namespace miniFE

#endif /* _analytic_soln_hpp_ */
