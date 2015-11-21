/* specfunc/test_bessel.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 Gerard Jungman
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

static double J[100];
static double Y[100];
static double I[100];
static double K[100];

int test_bessel(void)
{
  gsl_sf_result r;
  int i;
  int s = 0;
  int sa;

  TEST_SF(s, gsl_sf_bessel_J0_e, (0.1, &r),     0.99750156206604003230,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_J0_e, (2.0, &r),     0.22389077914123566805,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_J0_e, (100.0, &r),   0.019985850304223122424,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_J0_e, (1.0e+10, &r), 2.1755917502468917269e-06, TEST_SQRT_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_J1_e, (0.1, &r),      0.04993752603624199756,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_J1_e, (2.0, &r),      0.57672480775687338720,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_J1_e, (100.0, &r),   -0.07714535201411215803,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_J1_e, (1.0e+10, &r), -7.676508175684157103e-06, TEST_TOL4, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Jn_e, (4, 0.1, &r),     2.6028648545684032338e-07, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jn_e, (5, 2.0, &r),     0.007039629755871685484,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jn_e, (10, 20.0, &r),   0.18648255802394508321,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jn_e, (100, 100.0, &r), 0.09636667329586155967,    TEST_TOL0, GSL_SUCCESS);

  /* exercise the BUG#3 problem */
  TEST_SF(s, gsl_sf_bessel_Jn_e, (2, 900.0, &r), -0.019974345269680646400, TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jn_e, (2, 15000.0, &r), -0.0020455820181216382666, TEST_TOL4, GSL_SUCCESS);

#ifdef TEST_LARGE
  TEST_SF(s, gsl_sf_bessel_Jn_e, (0, 1.0e+10, &r), 2.1755917502468917269e-06, TEST_SQRT_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jn_e, (1, 1.0e+10, &r), -7.676508175684157103e-06, TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jn_e, (0, 20000, &r), 0.00556597490495494615709982972, TEST_TOL4, GSL_SUCCESS);
#endif

  /*  Testcase demonstrating long calculation time:
      Time spent in gsl_sf_bessel_J_CF1 for large x<1000 and n<5
      grows in proportion to x 
      Jonny Taylor <j.m.taylor@durham.ac.uk>  */
  TEST_SF(s, gsl_sf_bessel_Jn_e, (45, 900.0, &r),     0.02562434700634278108,    TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Y0_e, (0.1, &r),         -1.5342386513503668441,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Y0_e, (2, &r),            0.5103756726497451196,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Y0_e, (256.0, &r),       -0.03381290171792454909 ,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Y0_e, (4294967296.0, &r), 3.657903190017678681e-06, TEST_SQRT_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Y1_e, (0.1, &r),         -6.45895109470202698800,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Y1_e, (2, &r),           -0.10703243154093754689,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Y1_e, (100.0, &r),       -0.020372312002759793305,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Y1_e, (4294967296.0, &r), 0.000011612249378370766284, TEST_TOL4, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Yn_e, (4, 0.1, &r),            -305832.29793353160319,    TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Yn_e, (5, 2, &r),              -9.935989128481974981,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Yn_e, (100, 100.0, &r),        -0.16692141141757650654,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Yn_e, (100, 4294967296.0, &r),  3.657889671577715808e-06, TEST_SQRT_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Yn_e, (1000, 4294967296.0, &r), 3.656551321485397501e-06, 2.0e-05, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Yn_e, (2, 15000.0, &r), -0.006185217273358617849, TEST_TOL4, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_I0_scaled_e, (1e-10, &r),   0.99999999990000000001,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_I0_scaled_e, (0.1, &r),     0.90710092578230109640,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_I0_scaled_e, (2, &r),       0.30850832255367103953,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_I0_scaled_e, (100.0, &r),   0.03994437929909668265,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_I0_scaled_e, (65536.0, &r), 0.0015583712551952223537, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_I1_scaled_e, (0.1, &r),     0.04529844680880932501,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_I1_scaled_e, (2, &r),       0.21526928924893765916,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_I1_scaled_e, (100.0, &r),   0.03974415302513025267,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_I1_scaled_e, (65536.0, &r), 0.0015583593657207350452, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_In_scaled_e, (  -4,    0.1, &r), 2.3575258620054605307e-07, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_In_scaled_e, (   4,    0.1, &r), 2.3575258620054605307e-07, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_In_scaled_e, (   5,    2.0, &r), 0.0013297610941881578142, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_In_scaled_e, ( 100,  100.0, &r), 1.7266862628167695785e-22, TEST_TOL0, GSL_SUCCESS);

  /* BJG: the "exact" values in the following two tests were originally computed from the
     taylor series for I_nu using "long double" and rescaling.  The last few digits
     were inaccurate due to cumulative roundoff. 
     
     BJG: 2006/05 I have now replaced these with the term asymptotic
     expansion from A&S 9.7.1 which should be fully accurate. */

  TEST_SF(s, gsl_sf_bessel_In_scaled_e, (   2,    1e7, &r), 1.261566024466416433e-4, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_In_scaled_e, (   2,    1e8, &r), 3.989422729212649531e-5, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_I0_e, (0.1, &r), 1.0025015629340956014, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_I0_e, (2.0, &r), 2.2795853023360672674, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_I0_e, (100.0, &r), 1.0737517071310738235e+42, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_I1_e, (0.1, &r), 0.05006252604709269211,      TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_I1_e, (2.0, &r), 1.59063685463732906340,      TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_I1_e, (100.0, &r), 1.0683693903381624812e+42, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_In_e, (   4,    0.1, &r), 2.6054690212996573677e-07, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_In_e, (   5,    2.0, &r), 0.009825679323131702321,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_In_e, ( 100,  100.0, &r), 4.641534941616199114e+21,  TEST_TOL2, GSL_SUCCESS);


  TEST_SF(s, gsl_sf_bessel_K0_scaled_e, (0.1, &r), 2.6823261022628943831, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_K0_scaled_e, (2.0, &r), 0.8415682150707714179, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_K0_scaled_e, (100.0, &r), 0.1251756216591265789, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_K1_scaled_e, (0.1, &r), 10.890182683049696574, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_K1_scaled_e, (2.0, &r), 1.0334768470686885732, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_K1_scaled_e, (100.0, &r), 0.1257999504795785293, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Kn_scaled_e, (   4,    0.1, &r), 530040.2483725626207, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Kn_scaled_e, (   5,    2.0, &r), 69.68655087607675118, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Kn_scaled_e, ( 100,  100.0, &r), 2.0475736731166756813e+19, TEST_TOL1, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_K0_e, (0.1, &r), 2.4270690247020166125, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_K0_e, (2.0, &r), 0.11389387274953343565, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_K0_e, (100.0, &r), 4.656628229175902019e-45, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_K1_e, (0.1, &r), 9.853844780870606135,       TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_K1_e, (2.0, &r), 0.13986588181652242728,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_K1_e, (100.0, &r), 4.679853735636909287e-45, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Kn_e, (   4,    0.1, &r), 479600.2497925682849,     TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Kn_e, (   5,    2.0, &r), 9.431049100596467443,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Kn_e, ( 100,  100.0, &r), 7.617129630494085416e-25, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s,  gsl_sf_bessel_j0_e, (-10.0, &r), -0.05440211108893698134, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j0_e, (0.001, &r), 0.9999998333333416667, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j0_e, (  1.0, &r), 0.84147098480789650670, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j0_e, ( 10.0, &r), -0.05440211108893698134, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j0_e, (100.0, &r), -0.005063656411097587937, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j0_e, (1048576.0, &r), 3.1518281938718287624e-07, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s,  gsl_sf_bessel_j1_e, (-10.0, &r), -0.07846694179875154709, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j1_e, (0.01, &r), 0.003333300000119047399, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j1_e, (  1.0, &r), 0.30116867893975678925, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j1_e, ( 10.0, &r), 0.07846694179875154709, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j1_e, (100.0, &r), -0.008673825286987815220, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j1_e, (1048576.0, &r), -9.000855242905546158e-07, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s,  gsl_sf_bessel_j2_e, (-10.0, &r), 0.07794219362856244547, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j2_e, (0.01, &r), 6.666619047751322551e-06, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j2_e, (  1.0, &r), 0.06203505201137386110, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j2_e, ( 10.0, &r), 0.07794219362856244547, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j2_e, (100.0, &r), 0.004803441652487953480, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_j2_e, (1048576.0, &r), -3.1518539455252413111e-07, TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s,  gsl_sf_bessel_jl_e, (0, 0.0, &r), 1.0, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s,  gsl_sf_bessel_jl_e, (1,       10.0, &r),   0.07846694179875154709000, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_jl_e, (5,        1.0, &r),   0.00009256115861125816357, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_jl_e, (10,      10.0, &r),   0.06460515449256426427,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_jl_e, (100,    100.0, &r),   0.010880477011438336539,   TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_jl_e, (2000, 1048576.0, &r), 7.449384239168568534e-07,  TEST_SQRT_TOL0, GSL_SUCCESS);

  /* related to BUG#3 problem */
  TEST_SF(s, gsl_sf_bessel_jl_e, (2, 900.0, &r),   -0.0011089115568832940086,  TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_jl_e, (2, 15000.0, &r), -0.00005955592033075750554, TEST_TOL4, GSL_SUCCESS);

  /* Bug report by Mario Santos, value computed from AS 10.1.8 */
  TEST_SF(s,  gsl_sf_bessel_jl_e, (100, 1000.0, &r), -0.00025326311230945818285, TEST_TOL4, GSL_SUCCESS);

  /* Bug reported by Koichi Takahashi <ktakahashi@molsci.org>,
     computed from Pari besseljh(n,x) and AS 10.1.1 */

  TEST_SF(s,  gsl_sf_bessel_jl_e, (30, 3878.62, &r), -0.00023285567034330878410434732790, TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_jl_e, (49, 9912.63, &r), 5.2043354544842669214485107019E-5 , TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_jl_e, (49, 9950.35, &r), 5.0077368819565969286578715503E-5 , TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_jl_e, (52, 9930.51, &r), -7.4838588266727718650124475651E-6 , TEST_TOL4, GSL_SUCCESS);

  TEST_SF(s,  gsl_sf_bessel_y0_e, (0.001, &r), -999.99950000004166670, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y0_e, (  1.0, &r), -0.5403023058681397174, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y0_e, ( 10.0, &r), 0.08390715290764524523, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y0_e, (100.0, &r), -0.008623188722876839341, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y0_e, (65536.0, &r), 0.000011014324202158573930, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y0_e, (4294967296.0, &r), 2.0649445131370357007e-10, TEST_SQRT_TOL0, GSL_SUCCESS);

  TEST_SF(s,  gsl_sf_bessel_y1_e, ( 0.01, &r), -10000.499987500069444, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y1_e, (  1.0, &r), -1.3817732906760362241, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y1_e, ( 10.0, &r), 0.06279282637970150586, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y1_e, (100.0, &r), 0.004977424523868819543, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y1_e, (4294967296.0, &r), 1.0756463271573404688e-10, TEST_SQRT_TOL0, GSL_SUCCESS);

  TEST_SF(s,  gsl_sf_bessel_y2_e, ( 0.01, &r), -3.0000500012499791668e+06, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y2_e, (  1.0, &r), -3.605017566159968955, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y2_e, ( 10.0, &r), -0.06506930499373479347, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y2_e, (100.0, &r), 0.008772511458592903927, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_y2_e, (4294967296.0, &r), -2.0649445123857054207e-10, TEST_SQRT_TOL0, GSL_SUCCESS);

  TEST_SF(s,  gsl_sf_bessel_yl_e, (0,        0.01, &r), -99.995000041666528,    TEST_TOL0, GSL_SUCCESS); 
  TEST_SF(s,  gsl_sf_bessel_yl_e, (0,        1.0, &r),  -0.54030230586813972,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_yl_e, (1,       10.0, &r),   0.062792826379701506,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_yl_e, (5,        1.0, &r),  -999.44034339223641,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_yl_e, (10,       0.01, &r), -6.5473079797378378e+30, TEST_TOL0, GSL_SUCCESS); 
  TEST_SF(s,  gsl_sf_bessel_yl_e, (10,      10.0, &r),  -0.172453672088057849,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_yl_e, (100,      1.0, &r),  -6.6830794632586775e+186, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_yl_e, (100,    100.0, &r),  -0.0229838504915622811,   TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s,  gsl_sf_bessel_yl_e, (2000, 1048576.0, &r), 5.9545201447146155e-07,  TEST_SQRT_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_i0_scaled_e, (0.0, &r), 1.0, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_i0_scaled_e, (0.1, &r), 0.9063462346100907067, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_i0_scaled_e, (2.0, &r), 0.24542109027781645493, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_i0_scaled_e, (100.0, &r), 0.005000000000000000000, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_i1_scaled_e, (0.0, &r), 0.0, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_i1_scaled_e, (0.1, &r), 0.030191419289002226846, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_i1_scaled_e, (2.0, &r), 0.131868364583275317610, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_i1_scaled_e, (100.0, &r), 0.004950000000000000000, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_i2_scaled_e, (0.0, &r), 0.0, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_i2_scaled_e, (0.1, &r), 0.0006036559400239012567, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_i2_scaled_e, (2.0, &r), 0.0476185434029034785100, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_i2_scaled_e, (100.0, &r), 0.0048515000000000000000, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_il_scaled_e, (   0, 0.0,   &r), 1.0, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_il_scaled_e, (   1, 0.0,   &r), 0.0, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_il_scaled_e, (   4, 0.001, &r), 1.0571434341190365013e-15, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_il_scaled_e, (   4,   0.1, &r), 9.579352242057134927e-08,  TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_il_scaled_e, (   5,   2.0, &r), 0.0004851564602127540059,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_il_scaled_e, (   5, 100.0, &r), 0.004300446777500000000,   TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_il_scaled_e, ( 100, 100.0, &r), 1.3898161964299132789e-23, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_k0_scaled_e, (0.1, &r), 15.707963267948966192, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_k0_scaled_e, (2.0, &r), 0.7853981633974483096, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_k0_scaled_e, (100.0, &r), 0.015707963267948966192, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_k1_scaled_e, (0.1, &r), 172.78759594743862812, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_k1_scaled_e, (2.0, &r), 1.1780972450961724644, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_k1_scaled_e, (100.0, &r), 0.015865042900628455854, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_k2_scaled_e, (0.1, &r), 5199.335841691107810, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_k2_scaled_e, (2.0, &r), 2.5525440310417070063, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_k2_scaled_e, (100.0, &r), 0.016183914554967819868, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_kl_scaled_e, (   4, 1.0/256.0, &r), 1.8205599816961954439e+14, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_kl_scaled_e, (   4, 1.0/8.0, &r),   6.1173217814406597530e+06, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_kl_scaled_e, (   5,   2.0, &r),     138.10735829492005119,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_kl_scaled_e, ( 100, 100.0, &r),     3.985930768060258219e+18,  TEST_TOL1, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Jnu_e, (0.0001, 1.0, &r),         0.7652115411876708497,  TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, (0.0001, 10.0, &r),       -0.2459270166445205,     TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, (0.0009765625, 10.0, &r), -0.2458500798634692,     TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, (0.75, 1.0, &r),           0.5586524932048917478,  TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, (0.75, 10.0, &r),         -0.04968928974751508135, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, ( 1.0, 0.001, &r), 0.0004999999375000026,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, ( 1.0,   1.0, &r), 0.4400505857449335160,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, ( 1.75,  1.0, &r), 0.168593922545763170103,     TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, (30.0,   1.0, &r), 3.482869794251482902e-42,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, (30.0, 100.0, &r), 0.08146012958117222297,    TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, (10.0,   1.0, &r), 2.6306151236874532070e-10, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, (10.0, 100.0, &r), -0.05473217693547201474,   TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, (10.2, 100.0, &r), -0.03548919161046526864,   TEST_TOL2, GSL_SUCCESS);

  /* related to BUG#3 problem */
  TEST_SF(s, gsl_sf_bessel_Jnu_e, (2.0, 900.0, &r),   -0.019974345269680646400,  TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Jnu_e, (2.0, 15000.0, &r), -0.0020455820181216382666, TEST_TOL4, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Ynu_e, (0.0001, 1.0, &r),  0.08813676933044478439,    TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Ynu_e, (0.0001,10.0, &r),  0.05570979797521875261,    TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Ynu_e, ( 0.75,  1.0, &r), -0.6218694174429746383,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Ynu_e, ( 0.75, 10.0, &r),  0.24757063446760384953,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Ynu_e, ( 1.0, 0.001, &r), -636.6221672311394281,      TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Ynu_e, ( 1.0,   1.0, &r), -0.7812128213002887165,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Ynu_e, (30.0,   1.0, &r), -3.0481287832256432162e+39, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Ynu_e, (30.0, 100.0, &r),  0.006138839212010033452,   TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Ynu_e, (10.0,   1.0, &r), -1.2161801427868918929e+08, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Ynu_e, (10.0, 100.0, &r),  0.05833157423641492875,    TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Ynu_e, (10.2, 100.0, &r),  0.07169383985546287091,    TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Inu_scaled_e, (0.0001,10.0, &r), 0.12783333709581669672,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_scaled_e, ( 1.0, 0.001, &r), 0.0004995003123542213370,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_scaled_e, ( 1.0,   1.0, &r), 0.20791041534970844887,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_scaled_e, (30.0,   1.0, &r), 1.3021094983785914437e-42, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_scaled_e, (30.0, 100.0, &r), 0.0004486987756920986146,  TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_scaled_e, (10.0,   1.0, &r), 1.0127529864692066036e-10, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_scaled_e, (10.0, 100.0, &r), 0.024176682718258828365,   TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_scaled_e, (10.2, 100.0, &r), 0.023691628843913810043,   TEST_TOL3, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Inu_e, (0.0001,10.0, &r), 2815.7166269770030352,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_e, ( 1.0, 0.001, &r), 0.0005000000625000026042,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_e, ( 1.0,   1.0, &r), 0.5651591039924850272,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_e, (30.0,   1.0, &r), 3.539500588106447747e-42,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_e, (30.0, 100.0, &r), 1.2061548704498434006e+40, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_e, (10.0,   1.0, &r), 2.7529480398368736252e-10, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_e, (10.0, 100.0, &r), 6.498975524720147799e+41,  TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Inu_e, (10.2, 100.0, &r), 6.368587361287030443e+41,  TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Knu_scaled_e, (0.0001,10.0, &r), 0.3916319346235421817, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_scaled_e, ( 1.0, 0.001, &r), 1000.9967345590684524, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_scaled_e, ( 1.0,   1.0, &r), 1.6361534862632582465, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_scaled_e, (30.0,   1.0, &r), 1.2792629867539753925e+40, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_scaled_e, (30.0, 100.0, &r), 10.673443449954850040, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_scaled_e, (10.0,   1.0, &r), 4.912296520990198599e+08, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_scaled_e, (10.0, 100.0, &r), 0.20578687173955779807, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_scaled_e, (10.0, 1000.0, &r), 0.04165905142800565788, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_scaled_e, (10.0, 1.0e+8, &r), 0.00012533147624060789938, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_scaled_e, (10.2, 100.0, &r), 0.20995808355244385075, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_Knu_e, (0.0001,0.001, &r), 7.023689431812884141,      TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_e, (0.0001,10.0, &r), 0.000017780062324654874306, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_e, ( 1.0, 0.001, &r), 999.9962381560855743,      TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_e, ( 1.0,   1.0, &r), 0.6019072301972345747,     TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_e, (10.0, 0.001, &r), 1.8579455483904008064e+38, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_e, (10.0,   1.0, &r), 1.8071328990102945469e+08, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_e, (10.0, 100.0, &r), 7.655427977388100611e-45,  TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_e, (10.2, 100.0, &r), 7.810600225948217841e-45,  TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_e, (30.0,   1.0, &r), 4.706145526783626883e+39,  TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_Knu_e, (30.0, 100.0, &r), 3.970602055959398739e-43,  TEST_TOL2, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (0.0001,1.0e-100, &r), 5.439794449319847, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (0.0001,0.0001, &r), 2.232835507214331, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (0.0001,10.0, &r), -10.93743282256098, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, ( 1.0, 1.0e-100, &r), 230.2585092994045, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, ( 1.0, 1.0e-10, &r), 23.025850929940456840, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, ( 1.0, 0.001, &r), 6.907751517131146, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, ( 1.0,   1.0, &r), -0.5076519482107523309, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (30.0, 1.0e-100, &r), 6999.113586185543475, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (30.0,   1.0, &r), 91.34968784026325464, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (30.0, 100.0, &r), -97.63224126416760932, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (100.0, 1.0e-100, &r), 23453.606706185466825, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (100.0, 1.0, &r), 427.7532510250188083, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (100.0, 100.0, &r), -55.53422771502921431, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (1000.0, 1.0e-100, &r), 236856.183755993135, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (10000.0, 1.0e-100, &r), 2.39161558914890695e+06, TEST_TOL0, GSL_SUCCESS);

  /* [bug #31528] gsl_sf_bessel_lnKnu overflows for large nu */

  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (180.0, 2.2, &r), 735.1994170369583930752590258, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_lnKnu_e, (3500.5, 1500.0, &r), 1731.220077116482710070986699, TEST_TOL1, GSL_SUCCESS);

  sa = 0;
  gsl_sf_bessel_Jn_array(3, 38, 1.0, J);
  sa += ( test_sf_frac_diff(J[0],  0.0195633539826684059190  ) > TEST_TOL1);
  sa += ( test_sf_frac_diff(J[1],  0.0024766389641099550438  ) > TEST_TOL1);
  sa += ( test_sf_frac_diff(J[10], 1.9256167644801728904e-14 ) > TEST_TOL1);
  sa += ( test_sf_frac_diff(J[35], 6.911232970971626272e-57  ) > TEST_TOL1);
  gsl_test(sa, "  gsl_sf_bessel_Jn_array");
  s += sa;

  sa = 0;
  gsl_sf_bessel_Yn_array(3, 38, 1.0, Y);
  sa += ( test_sf_frac_diff(Y[0],  -5.821517605964728848      ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(Y[1],  -33.27842302897211870      ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(Y[10], -1.2753618701519837595e+12 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(Y[35], -1.2124435663593357154e+54 ) > TEST_TOL0 );
  gsl_test(sa, "  gsl_sf_bessel_Yn_array");
  s += sa;

  sa = 0;
  gsl_sf_bessel_In_scaled_array(3, 38, 1.0, I);
  sa += ( test_sf_frac_diff(I[0],  0.0081553077728142938170  ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(I[1],  0.0010069302573377758637  ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(I[10], 7.341518665628926244e-15  ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(I[35], 2.5753065298357542893e-57 ) > TEST_TOL0 );
  gsl_test(sa, "  gsl_sf_bessel_In_scaled_array");
  s += sa;

  sa = 0;
  gsl_sf_bessel_In_array(3, 38, 1.0, Y);
  sa += ( test_sf_frac_diff(Y[0],   0.0221684249243319024760  ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(Y[1],   0.0027371202210468663251  ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(Y[10],  1.9956316782072007564e-14 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(Y[35],  7.000408942764452901e-57  ) > TEST_TOL0 );
  gsl_test(sa, "  gsl_sf_bessel_In_array");
  s += sa;

  sa = 0;
  gsl_sf_bessel_Kn_array(3, 38, 1.0, K);
  sa += ( test_sf_frac_diff(K[0],  7.101262824737944506 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(K[1],  44.23241584706284452 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(K[10], 1.9215763927929940846e+12 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(K[35], 1.8789385023806051223e+54 ) > TEST_TOL0 );
  gsl_test(sa, "  gsl_sf_bessel_Kn_array");
  s += sa;

  sa = 0;
  gsl_sf_bessel_Kn_scaled_array(3, 38, 1.0, K);
  sa += ( test_sf_frac_diff(K[0],  19.303233695596904277 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(K[1],  120.23617222591483717 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(K[10], 5.223386190525076473e+12 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(K[35], 5.107484387813251411e+54 ) > TEST_TOL0 );
  gsl_test(sa, "  gsl_sf_bessel_Kn_scaled_array");
  s += sa;

  sa = 0;
  gsl_sf_bessel_jl_array(50, 1.0, J);
  sa += ( test_sf_frac_diff(J[0],  0.84147098480789650670   ) > TEST_TOL2 );
  sa += ( test_sf_frac_diff(J[1],  0.30116867893975678925   ) > TEST_TOL2 );
  sa += ( test_sf_frac_diff(J[10], 7.116552640047313024e-11 ) > TEST_TOL2 );
  sa += ( test_sf_frac_diff(J[50], 3.615274717489787311e-81 ) > TEST_TOL2 );
  gsl_test(sa, "  gsl_sf_bessel_jl_array");
  s += sa;

  sa = 0;
  gsl_sf_bessel_jl_steed_array(99, 1.0, J);
  sa += ( test_sf_frac_diff(J[0],  0.84147098480789650670   ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(J[1],  0.30116867893975678925   ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(J[10], 7.116552640047313024e-11 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(J[50], 3.615274717489787311e-81 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(J[80], 1.136352423414503264e-144 ) > TEST_TOL1 );
  gsl_test(sa, "  gsl_sf_bessel_jl_steed_array");
  s += sa;

  sa = 0;
  gsl_sf_bessel_yl_array(50, 1.0, Y);
  sa += ( test_sf_frac_diff(Y[0],  -0.5403023058681397174 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(Y[1],  -1.3817732906760362241 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(Y[10], -6.722150082562084436e+08  ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(Y[50], -2.7391922846297571576e+78 ) > TEST_TOL0 );
  gsl_test(sa, "  gsl_sf_bessel_yl_array");
  s += sa;

  {
    double Y0[1];
    sa = 0;
    gsl_sf_bessel_yl_array(0, 1.0, Y0);
    sa += ( test_sf_frac_diff(Y0[0],  -0.5403023058681397174 ) > TEST_TOL0 );
    gsl_test(sa, "  gsl_sf_bessel_yl_array (lmax=0)");
    s += sa;
  }

  sa = 0;
  gsl_sf_bessel_il_scaled_array(50, 1.0, I);
  sa += ( test_sf_frac_diff(I[0],  0.43233235838169365410 ) > TEST_TOL2 );
  sa += ( test_sf_frac_diff(I[1],  0.13533528323661269189 ) > TEST_TOL2 );
  sa += ( test_sf_frac_diff(I[10], 2.7343719371837065460e-11 ) > TEST_TOL2 );
  sa += ( test_sf_frac_diff(I[50], 1.3429606061892023653e-81 ) > TEST_TOL2 );
  gsl_test(sa, "  gsl_sf_bessel_il_scaled_array");
  s += sa;

  sa = 0;
  gsl_sf_bessel_il_scaled_array(50, 0.0, I);
  sa += ( test_sf_frac_diff(I[0],  1.0 ) > TEST_TOL2 );
  sa += ( test_sf_frac_diff(I[1],  0.0 ) > TEST_TOL2 );
  sa += ( test_sf_frac_diff(I[10], 0.0 ) > TEST_TOL2 );
  sa += ( test_sf_frac_diff(I[50], 0.0 ) > TEST_TOL2 );
  gsl_test(sa, "  gsl_sf_bessel_il_scaled_array (L=0)");
  s += sa;

  sa = 0;
  gsl_sf_bessel_kl_scaled_array(50, 1.0, K);
  sa += ( test_sf_frac_diff(K[0],  1.5707963267948966192     ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(K[1],  3.1415926535897932385     ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(K[10], 2.7231075458948147010e+09 ) > TEST_TOL0 );
  sa += ( test_sf_frac_diff(K[50], 1.1578440432804522544e+79 ) > TEST_TOL0 );
  gsl_test(sa, "  gsl_sf_bessel_kl_scaled_array");
  s += sa;

  {
    double K0[1];
    sa = 0;
    gsl_sf_bessel_kl_scaled_array(0, 1.0, K0);
    sa += ( test_sf_frac_diff(K[0],  1.5707963267948966192     ) > TEST_TOL0 );
    gsl_test(sa, "  gsl_sf_bessel_kl_scaled_array (lmax=0)");
    s += sa;
  }

  sa = 0;
  sa += ( gsl_sf_bessel_zero_J0_e(0, &r) != GSL_EINVAL );
  sa += ( r.val != 0.0 );
  s += sa;
  TEST_SF(s, gsl_sf_bessel_zero_J0_e, ( 1,  &r),  2.404825557695771, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_J0_e, ( 2,  &r),  5.520078110286304, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_J0_e, (20,  &r), 62.048469190227081, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_J0_e, (25,  &r), 77.756025630388058, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_J0_e, (100, &r), 313.37426607752784, TEST_TOL1, GSL_SUCCESS);

  sa = 0;
  sa += ( gsl_sf_bessel_zero_J1_e(0, &r) != GSL_SUCCESS );
  sa += ( r.val != 0.0 );
  s += sa;
  TEST_SF(s, gsl_sf_bessel_zero_J1_e, ( 1,  &r), 3.831705970207512, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_J1_e, ( 2,  &r), 7.015586669815619, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_J1_e, (20,  &r), 63.61135669848124, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_J1_e, (25,  &r), 79.32048717547630, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_J1_e, (100, &r), 314.9434728377672, TEST_TOL2, GSL_SUCCESS);

  sa = 0;
  sa += ( gsl_sf_bessel_zero_Jnu_e(0.0, 0, &r) != GSL_EINVAL );
  sa += ( r.val != 0.0 );
  s += sa;
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (0.0,  1,  &r),  2.404825557695771, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (0.0,  2,  &r),  5.520078110286304, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (0.0, 20,  &r), 62.048469190227081, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (0.0, 25,  &r), 77.756025630388058, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (0.0, 100, &r), 313.37426607752784, TEST_TOL1, GSL_SUCCESS);

  sa = 0;
  sa += ( gsl_sf_bessel_zero_Jnu_e(1.0, 0, &r) != GSL_SUCCESS );
  sa += (r.val != 0.0);
  s += sa;
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 1.5, 1, &r),  4.4934094579090641, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 1, &r),  8.7714838159599540, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 1.5, 2, &r),  7.7252518369377072, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 2, &r),  12.338604197466944, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 1.5, 3, &r),  10.904121659428900, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 3, &r),  15.700174079711671, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 1.5, 4, &r),  14.066193912831473, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 4, &r),  18.980133875179921, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 1.5, 5, &r),  17.220755271930768, TEST_TOL1, GSL_SUCCESS);

  /* Something wrong with the tolerances on these */
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 5, &r),  22.217799896561268, TEST_SQRT_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 8.0, 5, &r),  26.266814641176644, TEST_SQRT_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (20.0, 5, &r),  41.413065513892636, TEST_SQRT_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 1.5, 6, &r),  20.371302959287563, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 6, &r),  25.430341154222704, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 8.0, 6, &r),  29.545659670998550, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 1.5, 7, &r),  23.519452498689007, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 7, &r),  28.626618307291138, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 8.0, 7, &r),  32.795800037341462, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 1.5, 8, &r),  26.666054258812674, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 8, &r),  31.811716724047763, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (10.0, 8, &r),  38.761807017881651, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 1.5, 9, &r),  29.811598790892959, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 9, &r),  34.988781294559295, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (10.0, 9, &r),  42.004190236671805, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 1.5, 10, &r),  32.956389039822477, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 10, &r),  38.159868561967132, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (15.0, 10, &r),  52.017241278881633, TEST_TOL1, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 11,  &r), 41.326383254047406, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (15.0, 11,  &r), 55.289204146560061, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 12,  &r), 44.4893191232197314, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (15.0, 12,  &r), 58.5458289043850856, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 13,  &r), 47.6493998066970948, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (15.0, 13,  &r), 61.7897598959450550, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 14,  &r), 50.8071652030063595, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (15.0, 14,  &r), 65.0230502510422545, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 15,  &r), 53.9630265583781707, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (15.0, 15,  &r), 68.2473219964207837, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 16,  &r), 57.1173027815042647, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (15.0, 16,  &r), 71.4638758850226630, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 17,  &r), 60.2702450729428077, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (15.0, 17,  &r), 74.6737687121404241, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 18,  &r), 63.4220540458757799, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (15.0, 18,  &r), 77.8778689734863729, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 19,  &r), 66.5728918871182703, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (15.0, 19,  &r), 81.0768977206328326, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 5.0, 20,  &r), 69.722891161716742,  TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (15.0, 20,  &r), 84.271459069716442,  TEST_TOL1, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 23.0, 11,  &r), 65.843393469524653, TEST_TOL6, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 30.0, 11,  &r), 74.797306585175426, TEST_TOL6, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 32.0, 15,  &r), 90.913637691861741, TEST_TOL6, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 50.0, 15,  &r), 113.69747988073942, TEST_TOL6, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (  5.0, 22,  &r), 76.020793430591605, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 10.0, 22,  &r), 83.439189796105756, TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, ( 12.0, 22,  &r), 86.345496520534055, TEST_TOL6, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (100.0, 22,  &r), 199.82150220122519, TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_bessel_zero_Jnu_e, (500.0, 22,  &r), 649.34132440891735, TEST_TOL2, GSL_SUCCESS);

  sa = 0;
  for(i=0; i<100; i++) {
    J[i] = i/10.0;
  }
  gsl_sf_bessel_sequence_Jnu_e(2.0, GSL_MODE_DEFAULT, 100, J);
  sa += ( test_sf_frac_diff(J[1],  0.0012489586587999188454 ) > TEST_TOL1 );
  sa += ( test_sf_frac_diff(J[20], 0.3528340286156377192 ) > TEST_TOL4 );
  sa += ( test_sf_frac_diff(J[50], 0.0465651162777522155 ) > TEST_TOL4 );
  gsl_test(sa, "  gsl_sf_sequence_Jnu_e(2)");
  s += sa;

  sa = 0;
  for(i=0; i<100; i++) {
    J[i] = i;
  }
  gsl_sf_bessel_sequence_Jnu_e(12.0, GSL_MODE_DEFAULT, 100, J);
  sa += ( test_sf_frac_diff(J[1],   4.999718179448405289e-13 ) > TEST_TOL1 );
  sa += ( test_sf_frac_diff(J[5],   7.627813166084551355e-05 ) > TEST_TOL1 );
  sa += ( test_sf_frac_diff(J[7],   2.655620035894568062e-03 ) > TEST_TOL3 );
  sa += ( test_sf_frac_diff(J[10],  6.337025497015601509e-02 ) > TEST_TOL3 );
  sa += ( test_sf_frac_diff(J[15],  0.23666584405476805591 ) > TEST_TOL3 );
  sa += ( test_sf_frac_diff(J[30],  0.14825335109966010021 ) > TEST_TOL3 );
  sa += ( test_sf_frac_diff(J[70],  0.04109913716555364262 ) > TEST_TOL4 );
  sa += ( test_sf_frac_diff(J[99], -0.0015052760501176728  ) > TEST_TOL5 );
  gsl_test(sa, "  gsl_sf_sequence_Jnu_e(12)");
  s += sa;

  sa = 0;
  for(i=0; i<100; i++) {
    J[i] = i * 20;
  }
  gsl_sf_bessel_sequence_Jnu_e(1000.0, GSL_MODE_DEFAULT, 100, J);
  sa += ( test_sf_frac_diff(J[50],  0.04473067294796404088 ) > TEST_TOL5 );
  sa += ( test_sf_frac_diff(J[99],  0.01400619760349853902 ) > TEST_TOL6 );
  gsl_test(sa, "  gsl_sf_sequence_Jnu_e(1000)");
  s += sa;


  return s;
}
