
//@HEADER
// ************************************************************************
// 
//               Mantevo: A collection of mini-applications for HPC
//                 Copyright (2008) Sandia Corporation
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
#include "mytimer.hpp"
template <typename Scalar>
int vectorTests(int numTrials, const std::vector<Scalar> & x, std::vector<Scalar> & y,
		std::vector<double> & times) {  

  Scalar alpha = 2.0;
  double t0;

  size_t n = x.size();

  double tstart = mytimer(); // Initial time

  t0 = mytimer();
  for (int j=0;j<numTrials; j++)
    for (size_t i=0; i<n; i++) y[i] = alpha * x[i] + y[i];
  times[1] = mytimer() - t0;

  const Scalar * xp = &x[0];
  Scalar * yp=&y[0]; // get addresses

  t0 = mytimer();
  for (int j=0;j<numTrials; j++)
    for (size_t i=0; i<n; i++) yp[i] = alpha * xp[i] + yp[i];
  times[2] = mytimer() - t0;

  t0 = mytimer();
  for (int j=0;j<numTrials; j++) {
    const Scalar * xp = &x[0];
    Scalar * yp=&y[0]; // get addresses
    for (size_t i=0; i<n; i++) {*yp = alpha * *xp++ + *yp; yp++;}
  }
  times[3] = mytimer() - t0;

  times[0] = mytimer() - tstart;

  return(0);
}
