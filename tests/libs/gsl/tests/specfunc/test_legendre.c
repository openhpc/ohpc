/* specfunc/test_legendre.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004 Gerard Jungman
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/* Author:  G. Jungman */

#include <config.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_sf.h>
#include "test_sf.h"


int test_legendre(void)
{
  gsl_sf_result r;
  double L[256], DL[256];
  int s = 0;
  int sa;

  TEST_SF(s,  gsl_sf_legendre_P1_e, (-0.5, &r), -0.5, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_legendre_P1_e, ( 0.5, &r), 0.5, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s,  gsl_sf_legendre_P2_e, (0.0, &r), -0.5  , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_legendre_P2_e, (0.5, &r), -0.125, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_legendre_P2_e, (1.0, &r), 1.0  , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_legendre_P2_e, (100.0, &r), 14999.5  , TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s,  gsl_sf_legendre_P3_e, ( -0.5, &r), 0.4375, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_legendre_P3_e, (  0.5, &r), -0.4375, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_legendre_P3_e, (  1.0, &r), 1.0        , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_legendre_P3_e, (100.0, &r), 2.49985e+06, TEST_TOL0, GSL_SUCCESS);


  TEST_SF(s, gsl_sf_legendre_Pl_e, (1, -0.5, &r), -0.5, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (1,  1.0e-8, &r), 1.0e-08, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (1,  0.5, &r), 0.5, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (1,  1.0, &r), 1.0, TEST_TOL0, GSL_SUCCESS);
 
  TEST_SF(s, gsl_sf_legendre_Pl_e, (10, -0.5, &r), -0.1882286071777345, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (10,  1.0e-8, &r), -0.24609374999999864648, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (10,  0.5, &r), -0.18822860717773437500, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (10,  1.0, &r), 1.0, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_Pl_e, (99, -0.5, &r), 0.08300778172138770477, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (99,  1.0e-8, &r), -7.958923738716563193e-08, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (99,  0.5, &r), -0.08300778172138770477, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (99,  0.999, &r), -0.3317727359254778874, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (99,  1.0, &r), 1.0, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_Pl_e, (1000, -0.5, &r),   -0.019168251091650277878, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (1000,  1.0e-8, &r), 0.0252250181770982897470252620,  TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (1000,  0.5, &r),   -0.019168251091650277878, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (1000,  1.0, &r),    1.0,                     TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_Pl_e, (4000, -0.5, &r), -0.009585404456573080972, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (4000,  0.5, &r), -0.009585404456573080972, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Pl_e, (4000,  1.0, &r), 1.0, TEST_TOL0, GSL_SUCCESS);

  sa = 0;
  gsl_sf_legendre_Pl_array(100, 0.5, L);
  TEST_SF_VAL(sa, L[0],   +0.0,   1.0, TEST_TOL1);
  TEST_SF_VAL(sa, L[10],  +0.0,  -0.18822860717773437500, TEST_TOL1);
  TEST_SF_VAL(sa, L[100], +0.0,  -0.06051802596186118687, TEST_TOL1);
  gsl_test(sa, "gsl_sf_legendre_Pl_array(100, 0.5)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_Pl_deriv_array(100, 0.5, L, DL);
  TEST_SF_VAL(sa, DL[0],   +0.0,   0.0, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],   +0.0,   1.0, TEST_TOL1);
  TEST_SF_VAL(sa, DL[10],  +0.0,  -2.3171234130859375000, TEST_TOL1);
  TEST_SF_VAL(sa, DL[100], +0.0,  -7.0331691653942815112, TEST_TOL1);
  gsl_test(sa, "gsl_sf_legendre_Pl_deriv_array(100, 0.5)");
  s += sa;
  sa = 0;

  gsl_sf_legendre_Pl_deriv_array(10, 1.0, L, DL);
  TEST_SF_VAL(sa, DL[0],   +0.0,   0.0, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],   +0.0,   1.0, TEST_TOL1);
  TEST_SF_VAL(sa, DL[10],  +0.0,  55.0, TEST_TOL1);
  gsl_test(sa, "gsl_sf_legendre_Pl_deriv_array(10, 1.0)");
  s += sa;

  gsl_sf_legendre_Pl_deriv_array(10, 1.0 - 1.0e-11, L, DL);
  TEST_SF_VAL(sa, DL[0],   +0.0,   0.0, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],   +0.0,   1.0, TEST_TOL1);
  TEST_SF_VAL(sa, DL[10],  +0.0,  54.999999985150000001, TEST_TOL1);
  gsl_test(sa, "gsl_sf_legendre_Pl_deriv_array(10, 1.0 - 1.0e-11)");
  s += sa;

  gsl_sf_legendre_Pl_deriv_array(10, -1.0, L, DL);
  TEST_SF_VAL(sa, DL[0],   +0.0,   0.0, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],   +0.0,   1.0, TEST_TOL1);
  TEST_SF_VAL(sa, DL[10],  +0.0, -55.0, TEST_TOL1);
  gsl_test(sa, "gsl_sf_legendre_Pl_deriv_array(10, -1.0)");
  s += sa;

  gsl_sf_legendre_Pl_deriv_array(10, -1.0 + 1.0e-11, L, DL);
  TEST_SF_VAL(sa, DL[0],   +0.0,   0.0, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],   +0.0,   1.0, TEST_TOL1);
  TEST_SF_VAL(sa, DL[10],  +0.0, -54.999999985150000001, TEST_TOL1);
  gsl_test(sa, "gsl_sf_legendre_Pl_deriv_array(10, -1.0 + 1.0e-11)");
  s += sa;


  TEST_SF(s, gsl_sf_legendre_Plm_e, (10, 0, -0.5, &r), -0.18822860717773437500, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Plm_e, (10, 0, 1.0e-08, &r), -0.24609374999999864648, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Plm_e, (10, 0, 0.5, &r), -0.18822860717773437500, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_Plm_e, (10, 1, -0.5, &r), -2.0066877394361256516, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Plm_e, (10, 1, 1.0e-08, &r), -2.7070312499999951725e-07, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Plm_e, (10, 1, 0.5, &r), 2.0066877394361256516, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_Plm_e, (10, 5, -0.5, &r),    -30086.169706116174977,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Plm_e, (10, 5, 1.0e-08, &r), -0.0025337812499999964949, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Plm_e, (10, 5, 0.5, &r),      30086.169706116174977,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Plm_e, (10, 5, 0.999, &r),   -0.5036411489013270406,    TEST_TOL1, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_Plm_e, (100, 5, -0.5, &r), -6.617107444248382171e+08, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Plm_e, (100, 5, 1.0e-08, &r), 817.8987598063712851, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Plm_e, (100, 5, 0.5, &r), 6.617107444248382171e+08, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Plm_e, (100, 5, 0.999, &r), -1.9831610803806212189e+09, TEST_TOL2, GSL_SUCCESS);


  sa = 0;
  gsl_sf_legendre_Plm_deriv_array(100, 2, -1.0 + 1.0/1125899906842624.0, L, DL);
  TEST_SF_VAL(sa, L[0],   +0.0,  5.3290705182007490275e-15, TEST_TOL1);
  TEST_SF_VAL(sa, L[1],   +0.0, -2.6645352591003721471e-14, TEST_TOL1);
  TEST_SF_VAL(sa, L[98],  +0.0,  2.2646284847349109694e-08, TEST_TOL2);
  gsl_test(sa, "gsl_sf_legendre_Plm_deriv_array(100, 2, -1.0 + 2^(-50)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_Plm_deriv_array(100, 2, 1.0 - 1.0/1125899906842624.0, L, DL);
  TEST_SF_VAL(sa, L[0],   +0.0,  5.3290705182007490275e-15, TEST_TOL1);
  TEST_SF_VAL(sa, L[1],   +0.0,  2.6645352591003721471e-14, TEST_TOL1);
  TEST_SF_VAL(sa, L[10],  +0.0,  5.3343995887188313290e-12, TEST_TOL1);
  TEST_SF_VAL(sa, L[98],  +0.0,  2.2646284847349109694e-08, TEST_TOL2);
  gsl_test(sa, "gsl_sf_legendre_Plm_deriv_array(100, 2, 1.0 - 2^(-50)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_Plm_array(100, 5, 0.5, L);
  TEST_SF_VAL(sa, L[0],  +0.0, -460.3466286991656682, TEST_TOL1);
  TEST_SF_VAL(sa, L[10], +0.0,  38852.51334152290535, TEST_TOL1 );
  TEST_SF_VAL(sa, L[95], +0.0,  6.617107444248382171e+08, TEST_TOL1);
  gsl_test(sa, "gsl_sf_legendre_Plm_array(100, 5, 0.5)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_Plm_array(100, 5, 0.999, L);
  TEST_SF_VAL(sa, L[0],  +0.0,  -0.00016883550990916552255, TEST_TOL2);
  TEST_SF_VAL(sa, L[10], +0.0,  -30.651334850159821525, TEST_TOL2 );
  TEST_SF_VAL(sa, L[95], +0.0,  -1.9831610803806212189e+09, TEST_TOL2);
  gsl_test(sa, "gsl_sf_legendre_Plm_array(100, 5, 0.999)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_Plm_array(100, 5, -0.999, L);
  TEST_SF_VAL(sa, L[0],  +0.0,  -0.00016883550990916552255, TEST_TOL2);
  TEST_SF_VAL(sa, L[10], +0.0,  -30.651334850159821525, TEST_TOL2 );
  TEST_SF_VAL(sa, L[95], +0.0,   1.9831610803806212189e+09, TEST_TOL2);
  gsl_test(sa, "gsl_sf_legendre_Plm_array(100, 5, -0.999)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_Plm_deriv_array(100, 2, 0.999, L, DL);
  TEST_SF_VAL(sa, L[0],   +0.0,  0.00599700000000000000, TEST_TOL1);
  TEST_SF_VAL(sa, L[1],   +0.0,  0.02995501500000000000, TEST_TOL1 );
  TEST_SF_VAL(sa, DL[0],  +0.0,  -5.9940000000000000000, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],  +0.0,  -29.910045000000000000, TEST_TOL1 );
  TEST_SF_VAL(sa, DL[2],  +0.0,  -89.490629790000000000, TEST_TOL1 );
  TEST_SF_VAL(sa, DL[10], +0.0,  -5703.9461633355291972, TEST_TOL1 );
  TEST_SF_VAL(sa, DL[95], +0.0,  6.4518473603456858414E+06, TEST_TOL3);
  gsl_test(sa, "gsl_sf_legendre_Plm_deriv_array(100, 2, 0.999)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_Plm_deriv_array(100, 2, 1.0 - 1.0e-15, L, DL);
  TEST_SF_VAL(sa, DL[0],  +0.0,  -5.9999999999999940000, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],  +0.0,  -29.999999999999910000, TEST_TOL1 );
  TEST_SF_VAL(sa, DL[2],  +0.0,  -89.999999999999490000, TEST_TOL1 );
  TEST_SF_VAL(sa, DL[10], +0.0,  -6005.9999999996936940, TEST_TOL1 );
  TEST_SF_VAL(sa, DL[95], +0.0,  -2.2586255999928454270e+07, TEST_TOL3);
  gsl_test(sa, "gsl_sf_legendre_Plm_deriv_array(100, 2, 1.0 - 1.0e-15)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_Plm_deriv_array(100, 2, -1.0 + 1.0e-15, L, DL);
  TEST_SF_VAL(sa, DL[0],  +0.0,   5.9999999999999940000, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],  +0.0,  -29.999999999999910000, TEST_TOL1 );
  TEST_SF_VAL(sa, DL[95], +0.0,  -2.2586255999928454270e+07, TEST_TOL3);
  gsl_test(sa, "gsl_sf_legendre_Plm_deriv_array(100, 2, -1.0 + 1.0e-15)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_Plm_deriv_array(100, 5, 0.999, L, DL);
  TEST_SF_VAL(sa, DL[0],  +0.0,  0.42187762481054616565, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],  +0.0,  4.6341560284340909936, TEST_TOL1 );
  TEST_SF_VAL(sa, DL[2],  +0.0,  27.759505566959219127, TEST_TOL1 );
  TEST_SF_VAL(sa, DL[10], +0.0,  76051.795860179545484, TEST_TOL1 );
  TEST_SF_VAL(sa, DL[95], +0.0,  3.0344503083851936814e+12, TEST_TOL3);
  gsl_test(sa, "gsl_sf_legendre_Plm_deriv_array(100, 5, 0.999)");
  s += sa;

  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (10, 0, -0.5, &r), -0.24332702369300133776, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (10, 0, 0.5, &r), -0.24332702369300133776, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (10, 0, 0.999, &r), 1.2225754122797385990, TEST_TOL1, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (10, 5, -0.5, &r),    -0.3725739049803293972,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (10, 5, 1.0e-08, &r), -3.1377233589376792243e-08, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (10, 5, 0.5, &r),      0.3725739049803293972,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (10, 5, 0.999, &r),   -6.236870674727370094e-06,  TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (10, 10, -0.5, &r), 0.12876871185785724117, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (10, 10, 0.5, &r), 0.12876871185785724117,  TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (10, 10, 0.999, &r), 1.7320802307583118647e-14, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (200, 1, -0.5, &r),   0.3302975570099492931, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (200, 1, 0.5, &r),   -0.3302975570099492931, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (200, 1, 0.999, &r), -1.4069792055546256912, TEST_TOL2, GSL_SUCCESS);



  /* Test case from alberto@physik.fu-berlin.de */

  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (3, 1, 0.0, &r), 0.323180184114150653007, TEST_TOL2, GSL_SUCCESS);

  /* Other test cases */

  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (200, 1, -0.5, &r), 0.3302975570099492931418227583, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (140,135,1,&r), 0.0, TEST_TOL2, GSL_SUCCESS);

#ifdef EXTENDED
  TEST_SF(s, gsl_sf_legendre_sphPlm_e, (140,135,0.99998689456491752,&r), -6.54265253269093276310395668335e-305, TEST_TOL6, GSL_SUCCESS);
#endif

  sa = 0;
  gsl_sf_legendre_sphPlm_array(100, 5, 0.5, L);
  TEST_SF_VAL(sa, L[0],  +0.0, -0.22609703187800460722, TEST_TOL1);
  TEST_SF_VAL(sa, L[10], +0.0,  0.07452710323813558940, TEST_TOL1);
  TEST_SF_VAL(sa, L[95], +0.0,  0.25865355990880161717, TEST_TOL1);
  gsl_test(sa, "gsl_sf_legendre_sphPlm_array(100, 5, 0.5)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_sphPlm_array(100, 2, 1.0 - 1.0/1125899906842624.0, L);
  TEST_SF_VAL(sa, L[0],  +0.0, 6.8616082064776657177e-16, TEST_TOL2);
  TEST_SF_VAL(sa, L[10], +0.0, 4.8543150313086787324e-14, TEST_TOL2);
  TEST_SF_VAL(sa, L[95], +0.0, 8.3138984963650838973e-12, TEST_TOL2);
  gsl_test(sa, "gsl_sf_legendre_sphPlm_array(100, 2, 1.0 - 2^(-50))");
  s += sa;

  sa = 0;
  gsl_sf_legendre_sphPlm_array(100, 2, -1.0 + 1.0/1125899906842624.0, L);
  TEST_SF_VAL(sa, L[0],  +0.0,  6.8616082064776657177e-16, TEST_TOL2);
  TEST_SF_VAL(sa, L[95], +0.0, -8.3138984963650838973e-12, TEST_TOL2);
  gsl_test(sa, "gsl_sf_legendre_sphPlm_array(100, 2, -1.0 + 2^(-50))");
  s += sa;

  sa = 0;
  gsl_sf_legendre_sphPlm_deriv_array(100, 0, 0.5, L, DL);
  TEST_SF_VAL(sa, DL[0],  +0.0,  0.0, TEST_TOL1);
  TEST_SF_VAL(sa, DL[10], +0.0, -2.9953934850252897591, TEST_TOL1);
  TEST_SF_VAL(sa, DL[95], +0.0, -36.411811015111761007, TEST_TOL1);
  gsl_test(sa, "gsl_sf_legendre_sphPlm_deriv_array(100, 0, 0.5)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_sphPlm_deriv_array(100, 1, 0.5, L, DL);
  TEST_SF_VAL(sa, DL[0],  +0.0,  0.19947114020071633897, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],  +0.0, -0.44603102903819277863, TEST_TOL1);
  TEST_SF_VAL(sa, DL[10], +0.0,  1.3658895325030216565, TEST_TOL1);
  TEST_SF_VAL(sa, DL[99], +0.0, -27.925571865639037118, TEST_TOL1);
  gsl_test(sa, "gsl_sf_legendre_sphPlm_deriv_array(100, 1, 0.5)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_sphPlm_deriv_array(100, 1, 1.0 - 1.0/1125899906842624.0, L, DL);
  TEST_SF_VAL(sa, DL[0],  +0.0,  8.1973898803378530946e+06, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],  +0.0,  1.8329921010504257405e+07, TEST_TOL1);
  TEST_SF_VAL(sa, DL[10], +0.0,  1.8439572562895384115e+08, TEST_TOL1);
  TEST_SF_VAL(sa, DL[99], +0.0,  4.7682463136232210552e+09, TEST_TOL3);
  gsl_test(sa, "gsl_sf_legendre_sphPlm_deriv_array(100, 1, 1.0 - 2^(-50))");
  s += sa;

  sa = 0;
  gsl_sf_legendre_sphPlm_deriv_array(100, 2, 0.5, L, DL);
  TEST_SF_VAL(sa, DL[0],  +0.0, -0.38627420202318958034, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],  +0.0,  0.25549636910832059085, TEST_TOL1);
  TEST_SF_VAL(sa, DL[2],  +0.0,  1.5053547230039006279, TEST_TOL1);
  TEST_SF_VAL(sa, DL[10], +0.0,  0.73576559668648243477, TEST_TOL1);
  TEST_SF_VAL(sa, DL[98], +0.0, 28.444589950264378407, TEST_TOL1);
  gsl_test(sa, "gsl_sf_legendre_sphPlm_deriv_array(100, 2, 0.5)");
  s += sa;

  sa = 0;
  gsl_sf_legendre_sphPlm_deriv_array(100, 5, 0.5, L, DL);
  TEST_SF_VAL(sa, DL[0],  +0.0,  0.75365677292668202407, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],  +0.0,  0.54346962777757450534, TEST_TOL1);
  TEST_SF_VAL(sa, DL[2],  +0.0, -0.98309969029001383773, TEST_TOL1);
  TEST_SF_VAL(sa, DL[3],  +0.0, -2.7728270988954534293, TEST_TOL1);
  TEST_SF_VAL(sa, DL[10], +0.0, -5.7407133315443482193, TEST_TOL1);
  TEST_SF_VAL(sa, DL[95], +0.0, -25.893934624747394561, TEST_TOL1);
  gsl_test(sa, "gsl_sf_legendre_sphPlm_deriv_array(100, 5, 0.5)");
  s += sa;
  sa = 0;

  gsl_sf_legendre_sphPlm_deriv_array(100, 5, 1.0 - 1.0/1125899906842624.0, L, DL);
  TEST_SF_VAL(sa, DL[0],  +0.0,  1.7374288379067753301e-22, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],  +0.0,  6.2643887625426827113e-22, TEST_TOL1);
  TEST_SF_VAL(sa, DL[2],  +0.0,  1.6482697200734667281e-21, TEST_TOL1);
  TEST_SF_VAL(sa, DL[95], +0.0,  3.9890549466071349506e-15, TEST_TOL2);
  gsl_test(sa, "gsl_sf_legendre_sphPlm_deriv_array(100, 5, 1.0 - 2^(-50))");
  s += sa;

  gsl_sf_legendre_sphPlm_deriv_array(100, 5, -1.0 + 1.0/1125899906842624.0, L, DL);
  TEST_SF_VAL(sa, DL[0],  +0.0, -1.7374288379067753301e-22, TEST_TOL1);
  TEST_SF_VAL(sa, DL[1],  +0.0,  6.2643887625426827113e-22, TEST_TOL1);
  TEST_SF_VAL(sa, DL[2],  +0.0, -1.6482697200734667281e-21, TEST_TOL1);
  TEST_SF_VAL(sa, DL[95], +0.0,  3.9890549466071349506e-15, TEST_TOL3);
  gsl_test(sa, "gsl_sf_legendre_sphPlm_deriv_array(100, 5, -1.0 + 2^(-50))");
  s += sa;

  TEST_SF(s, gsl_sf_conicalP_half_e, (0.0, -0.5, &r),   0.8573827581049917129, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (0.0,  0.5, &r),   0.8573827581049917129, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (0.0,  2.0, &r),   0.6062611623284649811, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (0.0,  100.0, &r), 0.07979045091636735635, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_half_e, (10.0, -0.5, &r),    5.345484922591867188e+08, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (10.0,  0.5, &r),    15137.910380385258370, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (10.0,  2.0, &r),    0.4992680691891618544, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (10.0,  100.0, &r), -0.07272008163718195685, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_half_e, (200.0, -1.0e-3, &r),  1.3347639529084185010e+136, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (200.0,  1.0e-8, &r),  1.0928098010940058507e+136, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (200.0,  0.5, &r),     3.895546021611205442e+90,   TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (200.0,  10.0, &r),   -0.04308567180833581268,     TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (200.0,  100.0, &r),  -0.04694669186576399194,     TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (200.0,  1000.0, &r),  0.023698140704121273277,    TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (200.0,  1.0e+8, &r), -0.00006790983312124277891,  TEST_TOL3, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_half_e, (1.0e+8,  1.1, &r),   1.1599311133054742944,  TEST_SQRT_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_half_e, (1.0e+8,  100.0, &r), 0.07971967557381557875, TEST_SQRT_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (0.0, -0.5, &r),  1.7956982494514644808, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (0.0,  0.5, &r),  0.8978491247257322404, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (0.0,  2.0, &r),  0.7984204253272901551, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (0.0,  100.0, &r),  0.4227531369388072584, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (10.0, -0.5, &r),  5.345484922591867181e+07, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (10.0,  0.5, &r),  1513.7910356104985334, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (10.0,  2.0, &r),  0.03439243987215615642, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (10.0,  100.0, &r),  0.003283756665952609624, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (200.0, -0.5, &r),  1.7699538115312304280e+179, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (200.0,  1.0e-8, &r),  5.464049005470029253e+133, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (200.0,  0.5, &r),  1.9477730108056027211e+88, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (200.0,  10.0, &r),  0.0012462575917716355362, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (200.0,  100.0, &r),  -0.0003225881344802625149, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (200.0,  1000.0, &r), -0.00004330652890886567623, TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (200.0,  1.0e+8, &r),  2.0943091278037078483e-07, TEST_TOL3, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (1.0e+8,  1.1, &r), 2.092320445620989618e-09, 16.0*TEST_SQRT_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_mhalf_e, (1.0e+8,  100.0, &r),  -3.359967833599016923e-11, 256.0*TEST_SQRT_TOL0, GSL_SUCCESS);


  TEST_SF(s, gsl_sf_conicalP_0_e, (0.0, -0.5, &r),  1.3728805006183501647, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_0_e, (0.0,  0.5, &r),  1.0731820071493643751, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_0_e, (0.0,  2.0, &r),  0.9012862993604472987, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_0_e, (0.0,  100.0, &r),  0.30091748588199264556, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_0_e, (10.0, -0.5, &r),  1.6795592815421804669e+08, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_0_e, (10.0,  0.5, &r),  4826.034132009618240,      TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_0_e, (10.0,  2.0, &r),  0.18798468917758716146,    TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_0_e, (10.0,  100.0, &r), -0.008622130749987962529, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_0_e, (200.0,  -0.5, &r), 2.502194818646823e+180, TEST_TOL4, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_0_e, (1000.0,  100.0, &r),   0.0017908817653497715844, TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_0_e, (1000.0,  1000.0, &r), -0.0006566893804926284301, TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_0_e, (1000.0,  1.0e+8, &r),  2.3167213561756390068e-06, TEST_TOL4, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_1_e, (0.0, -0.5, &r),    0.4939371126656998499,  TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_1_e, (0.0,  0.5, &r),    0.14933621085538265636, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_1_e, (0.0,  2.0, &r),   -0.13666874968871549533, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_1_e, (0.0,  100.0, &r), -0.10544528203156629098, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_1_e, (10.0, -0.5, &r),    1.7253802958788312520e+09, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_1_e, (10.0,  0.5, &r),    46781.02294059967988,      TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_1_e, (10.0,  2.0, &r),    0.26613342643657444400,    TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_1_e, (10.0,  100.0, &r), -0.23281959695501029796,    TEST_TOL2, GSL_SUCCESS);


  /* FIXME: Mathematica gets some brain-damaged numbers for
   * these x < 0 points. I have checked what I am doing in detail,
   * and it must be right because you can do it by summing
   * manifestly positive definite quantities.
   */
  TEST_SF(s, gsl_sf_conicalP_1_e, (200.0, -0.999, &r), 2.71635193199341135e+270, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_1_e, (200.0, -0.9, &r),   4.2952493176812905e+234,  TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_1_e, (200.0, -0.5, &r),   5.01159205956053439e+182, TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_1_e, (200.0,  0.999, &r), 195733.0396081538,        TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_1_e, (200.0,  10.0, &r), -2.9272610662414349553,    TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_conicalP_1_e, (1000.0, 100.0, &r),  -1.7783258105862399857,    TEST_TOL6, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_1_e, (1000.0, 1000.0, &r),  0.4535161075156427179,    TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_1_e, (1000.0, 1.0e+8, &r),  0.0009983414549874888478, TEST_SQRT_TOL0, GSL_SUCCESS);


  TEST_SF(s, gsl_sf_conicalP_sph_reg_e, (2,  1.0, -0.5, &r),  1.6406279287008789526,      TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_sph_reg_e, (10, 1.0, -0.5, &r),  0.000029315266725049129448, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_sph_reg_e, (20, 1.0, -0.5, &r),  7.335769429462034431e-15,   TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_sph_reg_e, (30, 1.0, -0.5, &r),  1.3235612394267378871e-26,  TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_sph_reg_e, (10, 1.0, 0.5, &r),  2.7016087199857873954e-10, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_sph_reg_e, (20, 1.0, 0.5, &r),  1.1782569701435933399e-24, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_sph_reg_e, (30, 1.0, 0.5, &r),  3.636240588303797919e-41,  TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_sph_reg_e, (10, 1.0, 2.0, &r),  2.4934929626284934483e-10, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_sph_reg_e, (20, 1.0, 2.0, &r),  1.1284762488012616191e-24, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_sph_reg_e, (30, 100.0, 100.0, &r),  -1.6757772087159526048e-64, TEST_TOL6, GSL_SUCCESS);


  TEST_SF(s, gsl_sf_conicalP_cyl_reg_e, (2, 1.0, -0.5, &r),   2.2048510472375258708,       TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_cyl_reg_e, (10, 1.0, -0.5, &r),  0.00007335034531618655690,   TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_cyl_reg_e, (20, 1.0, -0.5, &r),  2.5419860619212164696e-14,   TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_cyl_reg_e, (30, 1.0, -0.5, &r),  5.579714972260536827e-26,    TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_cyl_reg_e, (10, 1.0, 0.5, &r),  1.1674078819646475282e-09,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_cyl_reg_e, (20, 1.0, 0.5, &r),  7.066408031229072207e-24,     TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_cyl_reg_e, (30, 1.0, 0.5, &r),  2.6541973286862588488e-40,    TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_cyl_reg_e, (10, 1.0, 2.0, &r),  1.0736109751890863051e-09,    TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_cyl_reg_e, (20, 1.0, 2.0, &r),  6.760965304863386741e-24,     TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_conicalP_cyl_reg_e, (30, 100.0, 100.0, &r), -4.268753482520651007e-63, TEST_TOL4, GSL_SUCCESS);


  TEST_SF(s, gsl_sf_legendre_H3d_0_e, (1.0e-06, 1.0e-06, &r), 0.9999999999998333333    , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_0_e, (1.0, 0.0, &r), 1.0                      , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_0_e, (1.0, 1.0, &r), 0.7160229153604338713    , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_0_e, (1.0, 100.0, &r), -3.767437313149604566e-44 , TEST_TOL2, GSL_SUCCESS);  
  TEST_SF(s, gsl_sf_legendre_H3d_0_e, (1.0, 500.0, &r), -6.665351935878582205e-218, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_0_e, (100.0, 1.0, &r), -0.004308757035378200029  , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_0_e, (100.0, 10.0, &r), 7.508054627912986427e-07 , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_0_e, (1000.0, 1.0, &r), 0.0007036067909088818319 , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_0_e, (1.0e+08, 1.0, &r), 7.927485371429105968e-09 , TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_0_e, (1.0e+08, 100.0, &r), -3.627118904186918957e-52 , 32.0*TEST_SQRT_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (1.0e-06, 1.0e-06, &r), 3.333333333334222222e-07, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (1.0, 1.0e-10, &r), 4.714045207910316829e-11,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (1.0, 1.0, &r), 0.3397013994799344639,           TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (1.0, 100.0, &r), -7.200624449531811272e-44,     TEST_TOL2, GSL_SUCCESS);  
  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (1.0, 500.0, &r), 4.192260336821728677e-218,     TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (100.0, 0.01, &r), 0.30117664944267412324   , TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (100.0, 1.0, &r), -0.007393833425336299309  , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (100.0, 10.0, &r), -5.031062029821254982e-07 , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (1000.0, 0.001, &r), 0.30116875865090396421   , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (1000.0, 1.0, &r), -0.0004776144516074971885 , TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (1.0e+08, 1.0e-08, &r), 0.30116867893975679722   , TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (1.0e+08, 1.0, &r), 3.0921097047369081582e-09, TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_1_e, (1.0e+08, 100.0, &r), -6.496142701296286936e-52 , 32.0*TEST_SQRT_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 1.0e-06, 1.0e-06, &r),  1.1544011544013627977e-32, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 1.0, 1.0e-10, &r),      2.0224912016958766992e-52, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 1.0, 1.0, &r),          0.011498635037491577728,   TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 1.0, 5.0, &r),          0.0020696945662545205776,  TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 1.0, 7.0, &r),     -0.0017555303787488993676,   TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 1.0, 10.0, &r),     0.00008999979724504887101,  TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 1.0, 100.0, &r),   -4.185397793298567945e-44,   TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 1.0, 500.0, &r),    1.4235113901091961263e-217, TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 100.0, 0.001, &r),  9.642762597222417946e-10,   TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 100.0, 0.002, &r),  3.0821201254308036109e-08,  TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 100.0, 0.01, &r),   0.00009281069019005840532,  TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 100.0, 1.0, &r),   -0.008043100696178624653,    TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 100.0, 10.0, &r),  -3.927678432813974207e-07,   TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 1000.0, 0.001, &r),  0.00009256365284253254503, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 1000.0, 0.01, &r),  -0.05553733815473079983, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 1.0e+08, 1.0e-08, &r),   0.00009256115861125841299, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_H3d_e, (5, 1.0e+08, 100.0, &r),    -6.496143209092860765e-52 , 128.0*TEST_SQRT_TOL0, GSL_SUCCESS);

#if FIXME
  sa = 0;
  gsl_sf_legendre_H3d_array(100, 1.0, 3.0, L);
  TEST_SF_VAL(sa, L[0], +0.0, gsl_sf_legendre_H3d(0, 1.0, 3.0), 1.0e-12);
  TEST_SF_VAL(sa, L[1], +0.0, gsl_sf_legendre_H3d(1, 1.0, 3.0), 1.0e-12);
  TEST_SF_VAL(sa, L[10], +0.0, gsl_sf_legendre_H3d(10, 1.0, 3.0), 1.0e-12);
  TEST_SF_VAL(sa, L[100], +0.0, gsl_sf_legendre_H3d(100, 1.0, 3.0), 1.0e-12);
  gsl_test(sa, "  gsl_sf_legendre_H3d_array(100, 1.0, 3.0)");
  s += sa;
#endif

  /* x = -1 + 2^-16 */
  TEST_SF(s, gsl_sf_legendre_Q0_e, (-0.9999847412109375, &r), -5.8917472200477175158028143531855, TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q0_e, (-0.5, &r), -0.5493061443340548457, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q0_e, (-1e-10, &r), -1.000000000000000000e-10, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q0_e, (0.0, &r), 0.0, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q0_e, (1e-10, &r), 1.000000000000000000e-10, TEST_TOL0, GSL_SUCCESS);
  /* x = 1 - 2^-16 */
  TEST_SF(s, gsl_sf_legendre_Q0_e, (0.9999847412109375, &r), 5.8917472200477175158028143531855, TEST_TOL4, GSL_SUCCESS);
  /* x = 1 + 2^-16 */
  TEST_SF(s, gsl_sf_legendre_Q0_e, ( 1.0000152587890625, &r), 5.8917548494422489138325509750429, TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q0_e, ( 1.5, &r), 0.8047189562170501873, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q0_e, ( 9.99, &r), 0.1004364599660005447, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q0_e, ( 10.0, &r), 0.1003353477310755806, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q0_e, ( 10.01, &r), 0.1002344395571710243, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q0_e, ( 100, &r), 0.010000333353334762015, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q0_e, ( 1e10, &r), 1.000000000000000000e-10, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_Q1_e, (-0.9999847412109375, &r), 4.8916573191196772369, TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, (-0.5, &r), -0.7253469278329725772, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, (-0.01, &r), -0.9998999966664666524, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, (-1e-10, &r), -0.999999999999999999, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, (0.0, &r), -1.0, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, (1e-10, &r), -0.999999999999999999, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, (0.0001, &r), -0.9999999899999999667, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, (0.01, &r), -0.9998999966664666524, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, (0.5, &r), -0.7253469278329725772, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, (0.9999847412109375, &r), 4.8916573191196772369, TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, (1.0000152587890625, &r), 4.8918447504867045145, TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, ( 1.5, &r), 0.20707843432557528095, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, ( 9.99, &r), 3.360235060345441639e-3, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, ( 10.0, &r), 3.353477310755806357e-3, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, ( 10.01, &r), 3.346739967281953346e-3, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, ( 100.0, &r), 3.333533347620158821e-5, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Q1_e, ( 1e10, &r), 3.333333333333333333e-21, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_Ql_e, (10, -0.5, &r), -0.29165813966586752393,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Ql_e, (10,  0.5, &r), 0.29165813966586752393,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Ql_e, (10,  1.5, &r), 0.000014714232718207477406, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_Ql_e, (100, -0.5, &r), -0.09492507395207282096,   TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Ql_e, (100,  0.5, &r), 0.09492507395207282096,    TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Ql_e, (100,  1.5, &r), 1.1628163435044121988e-43, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_legendre_Ql_e, (1000, -0.5, &r), -0.030105074974005303500, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Ql_e, (1000,  0.5, &r), 0.030105074974005303500,  TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_legendre_Ql_e, (1000,  1.1, &r), 1.0757258447825356443e-194, TEST_TOL3, GSL_SUCCESS);

  return s;
}
