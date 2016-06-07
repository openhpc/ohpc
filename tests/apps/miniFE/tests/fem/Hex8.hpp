#ifndef _Hex8_hpp_
#define _Hex8_hpp_

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

#include <gauss_pts.hpp>
#include <matrix_algebra_3x3.hpp>
#include <Hex8_enums.hpp>

namespace miniFE {

namespace Hex8 {

template<typename Scalar>
KERNEL_PREFIX void shape_fns(const Scalar* x, Scalar* values_at_nodes)
{
  //assumptions: values_at_nodes has length numNodesPerElem
  //             x has length 3 (hard-coded spatialDim)

  const Scalar u = 1.0 - x[0];
  const Scalar v = 1.0 - x[1];
  const Scalar w = 1.0 - x[2];

  const Scalar up1 = 1.0 + x[0];
  const Scalar vp1 = 1.0 + x[1];
  const Scalar wp1 = 1.0 + x[2];

  values_at_nodes[0] = 0.125 *   u *   v *   w;//(1-x)*(1-y)*(1-z)
  values_at_nodes[1] = 0.125 * up1 *   v *   w;//(1+x)*(1-y)*(1-z)
  values_at_nodes[2] = 0.125 * up1 * vp1 *   w;//(1+x)*(1+y)*(1-z)
  values_at_nodes[3] = 0.125 *   u * vp1 *   w;//(1-x)*(1+y)*(1-z)
  values_at_nodes[4] = 0.125 *   u *   v * wp1;//(1-x)*(1-y)*(1+z)
  values_at_nodes[5] = 0.125 * up1 *   v * wp1;//(1+x)*(1-y)*(1+z)
  values_at_nodes[6] = 0.125 * up1 * vp1 * wp1;//(1+x)*(1+y)*(1+z)
  values_at_nodes[7] = 0.125 *   u * vp1 * wp1;//(1-x)*(1+y)*(1+z)
}

template<typename Scalar>
KERNEL_PREFIX void gradients(const Scalar* x, Scalar* values_per_fn)
{
  //assumptions values_per_fn has length 24 (numNodesPerElem*spatialDim)
  //        spatialDim == 3

  const Scalar u = 1.0 - x[0];
  const Scalar v = 1.0 - x[1];
  const Scalar w = 1.0 - x[2];

  const Scalar up1 = 1.0 + x[0];
  const Scalar vp1 = 1.0 + x[1];
  const Scalar wp1 = 1.0 + x[2];

//fn 0
  values_per_fn[0] = -0.125 *  v *  w;
  values_per_fn[1] = -0.125 *  u *  w;
  values_per_fn[2] = -0.125 *  u *  v;
//fn 1
  values_per_fn[3] =  0.125 *  v   *  w;
  values_per_fn[4] = -0.125 *  up1 *  w;
  values_per_fn[5] = -0.125 *  up1 *  v;
//fn 2
  values_per_fn[6] =  0.125 *  vp1 *  w;
  values_per_fn[7] =  0.125 *  up1 *  w;
  values_per_fn[8] = -0.125 *  up1 *  vp1;
//fn 3
  values_per_fn[9]  = -0.125 *  vp1 *  w;
  values_per_fn[10] =  0.125 *  u   *  w;
  values_per_fn[11] = -0.125 *  u   *  vp1;
//fn 4
  values_per_fn[12] = -0.125 *  v   * wp1;
  values_per_fn[13] = -0.125 *  u   * wp1;
  values_per_fn[14] =  0.125 *  u   * v;
//fn 5
  values_per_fn[15] =  0.125 *  v * wp1;
  values_per_fn[16] = -0.125 *  up1 * wp1;
  values_per_fn[17] =  0.125 *  up1 * v;
//fn 6
  values_per_fn[18] =  0.125 *  vp1 * wp1;
  values_per_fn[19] =  0.125 *  up1 * wp1;
  values_per_fn[20] =  0.125 *  up1 * vp1;
//fn 7
  values_per_fn[21] = -0.125 *  vp1 * wp1;
  values_per_fn[22] =  0.125 *  u   * wp1;
  values_per_fn[23] =  0.125 *  u   * vp1;
}

template<typename Scalar>
KERNEL_PREFIX void gradients_and_detJ(const Scalar* elemNodeCoords,
                                          const Scalar* grad_vals,
                                          Scalar& detJ)
{
/**
  pt is the point at which the jacobian is to be computed.
*/

  //assumptions on the lengths of input arguments:
  //elemNodeCoords has length numNodesPerElem*spatialDim,
  //grad_vals has length numNodesPerElem*spatialDim

  const Scalar zero = 0;

  Scalar J00 = zero;
  Scalar J01 = zero;
  Scalar J02 = zero;

  Scalar J10 = zero;
  Scalar J11 = zero;
  Scalar J12 = zero;

  Scalar J20 = zero;
  Scalar J21 = zero;
  Scalar J22 = zero;

  size_t i_X_spatialDim = 0;
  for(size_t i=0; i<numNodesPerElem; ++i) {
//    size_t offset = 0;
//    for(size_t gd=0; gd<spatialDim; ++gd) {
//
//      Scalar gval = grad_vals[i_X_spatialDim+gd];
//
//      for(size_t jd=0; jd<spatialDim; ++jd) {
//        J[offset++] += gval*elemNodeCoords[i_X_spatialDim+jd];
//      }
//    }
    //for optimization, unroll the above double-loop over spatialDim:
    //(hard-coded assumption that spatialDim == 3)
    J00 += grad_vals[i_X_spatialDim+0]*elemNodeCoords[i_X_spatialDim+0];
    J01 += grad_vals[i_X_spatialDim+0]*elemNodeCoords[i_X_spatialDim+1];
    J02 += grad_vals[i_X_spatialDim+0]*elemNodeCoords[i_X_spatialDim+2];

    J10 += grad_vals[i_X_spatialDim+1]*elemNodeCoords[i_X_spatialDim+0];
    J11 += grad_vals[i_X_spatialDim+1]*elemNodeCoords[i_X_spatialDim+1];
    J12 += grad_vals[i_X_spatialDim+1]*elemNodeCoords[i_X_spatialDim+2];

    J20 += grad_vals[i_X_spatialDim+2]*elemNodeCoords[i_X_spatialDim+0];
    J21 += grad_vals[i_X_spatialDim+2]*elemNodeCoords[i_X_spatialDim+1];
    J22 += grad_vals[i_X_spatialDim+2]*elemNodeCoords[i_X_spatialDim+2];

    i_X_spatialDim += spatialDim;
  }

  Scalar term0 = J22*J11 - J21*J12;
  Scalar term1 = J22*J01 - J21*J02;
  Scalar term2 = J12*J01 - J11*J02;

  detJ = J00*term0 - J10*term1 + J20*term2;
}

template<typename Scalar>
KERNEL_PREFIX void gradients_and_invJ_and_detJ(const Scalar* elemNodeCoords,
                                               const Scalar* grad_vals,
                                               Scalar* invJ,
                                               Scalar& detJ)
{
/**
  pt is the point at which the jacobian is to be computed.
*/

  //assumptions on the lengths of input arguments:
  //pt has length spatialDim,
  //elemNodeCoords has length numNodesPerElem*spatialDim,
  //grad_vals has length numNodesPerElem*spatialDim, and
  //J has length spatialDim*spatialDim

  const Scalar zero = 0;

  //
  //First we compute the jacobian J:
  //
  Scalar J00 = zero;
  Scalar J01 = zero;
  Scalar J02 = zero;

  Scalar J10 = zero;
  Scalar J11 = zero;
  Scalar J12 = zero;

  Scalar J20 = zero;
  Scalar J21 = zero;
  Scalar J22 = zero;

  size_t i_X_spatialDim = 0;
  for(size_t i=0; i<numNodesPerElem; ++i) {
//    size_t offset = 0;
//    for(size_t gd=0; gd<spatialDim; ++gd) {
//
//      Scalar gval = grad_vals[i_X_spatialDim+gd];
//
//      for(size_t jd=0; jd<spatialDim; ++jd) {
//        J[offset++] += gval*elemNodeCoords[i_X_spatialDim+jd];
//      }
//    }
    //for optimization, unroll the above double-loop over spatialDim:
    //(a hard-coded assumption that spatialDim == 3)
    J00 += grad_vals[i_X_spatialDim+0]*elemNodeCoords[i_X_spatialDim+0];
    J01 += grad_vals[i_X_spatialDim+0]*elemNodeCoords[i_X_spatialDim+1];
    J02 += grad_vals[i_X_spatialDim+0]*elemNodeCoords[i_X_spatialDim+2];

    J10 += grad_vals[i_X_spatialDim+1]*elemNodeCoords[i_X_spatialDim+0];
    J11 += grad_vals[i_X_spatialDim+1]*elemNodeCoords[i_X_spatialDim+1];
    J12 += grad_vals[i_X_spatialDim+1]*elemNodeCoords[i_X_spatialDim+2];

    J20 += grad_vals[i_X_spatialDim+2]*elemNodeCoords[i_X_spatialDim+0];
    J21 += grad_vals[i_X_spatialDim+2]*elemNodeCoords[i_X_spatialDim+1];
    J22 += grad_vals[i_X_spatialDim+2]*elemNodeCoords[i_X_spatialDim+2];

    i_X_spatialDim += spatialDim;
  }

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
KERNEL_PREFIX void diffusionMatrix_symm(const Scalar* elemNodeCoords,
                        const Scalar* grad_vals,
                        Scalar* elem_mat)
{
  int len = (numNodesPerElem * (numNodesPerElem+1))/2;
  const Scalar zero = 0;
  miniFE::fill(elem_mat, elem_mat+len, zero);

  Scalar gpts[numGaussPointsPerDim];
  Scalar gwts[numGaussPointsPerDim];

  gauss_pts(numGaussPointsPerDim, gpts, gwts);

  const Scalar k = 1.0;
  Scalar detJ = 0.0;

  Scalar dpsidx[numNodesPerElem], dpsidy[numNodesPerElem], dpsidz[numNodesPerElem];

  Scalar invJ[spatialDim*spatialDim];

  //The following nested loop implements equations 3.4.5 and 3.4.7 on page 88
  //of Reddy & Gartling, "The Finite Element Method in Heat Transfer and Fluid
  //Dynamics", 2nd edition,
  //to compute the element diffusion matrix for the steady conduction equation.

  Scalar pt[spatialDim];

#ifdef MINIFE_DEBUG
  Scalar volume = zero;
#endif

  size_t gv_offset = 0;
  for(size_t ig=0; ig<numGaussPointsPerDim; ++ig) {
    Scalar wi = gwts[ig];

    for(size_t jg=0; jg<numGaussPointsPerDim; ++jg) {
      Scalar wi_wj = wi*gwts[jg];

      for(size_t kg=0; kg<numGaussPointsPerDim; ++kg) {
        Scalar wi_wj_wk = wi_wj*gwts[kg];
        const Scalar* grad_vals_ptr = &grad_vals[gv_offset];
        gv_offset += numNodesPerElem*spatialDim;
        gradients_and_invJ_and_detJ(elemNodeCoords, grad_vals_ptr, invJ, detJ);

#ifdef MINIFE_DEBUG
        volume += detJ;
#endif
        Scalar k_detJ_wi_wj_wk = k*detJ*wi_wj_wk;

        const Scalar* gv = grad_vals_ptr;
        for(int i=0; i<numNodesPerElem; ++i) {
          Scalar gv0 = gv[0], gv1 = gv[1], gv2 = gv[2];
          dpsidx[i] = gv0 * invJ[0] +
                      gv1 * invJ[1] +
                      gv2 * invJ[2];
          dpsidy[i] = gv0 * invJ[3] +
                      gv1 * invJ[4] +
                      gv2 * invJ[5];
          dpsidz[i] = gv0 * invJ[6] +
                      gv1 * invJ[7] +
                      gv2 * invJ[8];
          gv += spatialDim;
        }

        int offset = 0;
        for(int m=0; m<numNodesPerElem; ++m) {
          const Scalar dpsidx_m = dpsidx[m];
          const Scalar dpsidy_m = dpsidy[m];
          const Scalar dpsidz_m = dpsidz[m];

          elem_mat[offset++] += k_detJ_wi_wj_wk *
                              ((dpsidx_m*dpsidx_m) +
                               (dpsidy_m*dpsidy_m) +
                               (dpsidz_m*dpsidz_m));

          for(int n=m+1; n<numNodesPerElem; ++n) {
            elem_mat[offset++] += k_detJ_wi_wj_wk *
                                  ((dpsidx_m * dpsidx[n]) +
                                   (dpsidy_m * dpsidy[n]) +
                                   (dpsidz_m * dpsidz[n]));
          }
        }

      }//for kg
    }//for jg
  }//for ig

//int offset = 0;
//std::cout.precision(16);
//for(int m=0; m<numNodesPerElem; ++m) {
//  for(int n=m; n<numNodesPerElem; ++n) {
//std::cout<<"elem_mat["<<offset<<"] = "<<elem_mat[offset]<<";"<<std::endl;
//   ++offset;
//  }
//}
#ifdef MINIFE_DEBUG
//  std::cout << "element volume: " << volume << std::endl;
//  if (std::abs(volume - 1) > 1.e-7) {
//    std::cout << "element volume is "<<volume<<", expected 1.0."<<std::endl;
//  }
#endif
}

template<typename Scalar>
KERNEL_PREFIX void sourceVector(const Scalar* elemNodeCoords,
                                const Scalar* grad_vals,
                                Scalar* elem_vec)
{
  int len = numNodesPerElem;
  const Scalar zero = 0;
  miniFE::fill(elem_vec, elem_vec+len, zero);

  Scalar gpts[numGaussPointsPerDim];
  Scalar gwts[numGaussPointsPerDim];

  Scalar psi[numNodesPerElem];

  gauss_pts(numGaussPointsPerDim, gpts, gwts);

  Scalar Q = 1.0;

  Scalar pt[spatialDim];

  size_t gv_offset = 0;
  for(size_t ig=0; ig<numGaussPointsPerDim; ++ig) {
    pt[0] = gpts[ig];
    Scalar wi = gwts[ig];

    for(size_t jg=0; jg<numGaussPointsPerDim; ++jg) {
      pt[1] = gpts[jg];
      Scalar wj = gwts[jg];

      for(size_t kg=0; kg<numGaussPointsPerDim; ++kg) {
        pt[2] = gpts[kg];
        Scalar wk = gwts[kg];
    
        shape_fns(pt, psi);
        const Scalar* grad_vals_ptr = &grad_vals[gv_offset];
        gv_offset += numNodesPerElem*spatialDim;
        Scalar detJ;
        gradients_and_detJ(elemNodeCoords, grad_vals_ptr, detJ);
    
        Scalar term = Q*detJ*wi*wj*wk;

        for(int i=0; i<numNodesPerElem; ++i) {
          elem_vec[i] += psi[i]*term;
        }
      }
    }
  }
}

}//namespace Hex8

}//namespace miniFE

#endif

