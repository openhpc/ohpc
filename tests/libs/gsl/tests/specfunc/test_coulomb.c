/* specfunc/test_coulomb.c
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

#define PRINT(n) printf("%22.18g  %22.18g  %22.18g  %22.18g\n", F[n], Fp[n], G[n], Gp[n])

#define WKB_TOL (1.0e+04 * TEST_SQRT_TOL0)


int test_coulomb(void)
{
  gsl_sf_result r;
  int status = 0;
  int s = 0;
  
  char message_buff[2048];

  /* const int kmax = 20; */
  /* double F[kmax+1], Fp[kmax+1], G[kmax+1], Gp[kmax+1]; */
  gsl_sf_result F, Fp, G, Gp;
  double Fe, Ge;
  double lam_min;
  double lam_F;
  double eta, x;
  int k_G;

  TEST_SF(s, gsl_sf_hydrogenicR_1_e, (3.0, 2.0, &r),  0.025759948256148471036,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_hydrogenicR_1_e, (3.0, 10.0, &r), 9.724727052062819704e-13, TEST_TOL1, GSL_SUCCESS);
  status += s;

  TEST_SF(s, gsl_sf_hydrogenicR_e, (4, 1, 3.0, 0.0, &r),  0.0,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_hydrogenicR_e, (4, 0, 3.0, 2.0, &r), -0.03623182256981820062,  TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_hydrogenicR_e, (4, 1, 3.0, 2.0, &r), -0.028065049083129581005, TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_hydrogenicR_e, (4, 2, 3.0, 2.0, &r),  0.14583027278668431009,  TEST_TOL0, GSL_SUCCESS);
  status += s;

  TEST_SF(s, gsl_sf_hydrogenicR_e, (100,  0, 3.0, 2.0, &r), -0.00007938950980052281367, TEST_TOL3, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_hydrogenicR_e, (100, 10, 3.0, 2.0, &r),  7.112823375353605977e-12,  TEST_TOL2, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_hydrogenicR_e, (100, 90, 3.0, 2.0, &r),  5.845231751418131548e-245, TEST_TOL2, GSL_SUCCESS);
  status += s;

  lam_F = 0.0;
  k_G   = 0;
  eta = 1.0;
  x = 5.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  0.6849374120059439677, TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp, -0.7236423862556063963, TEST_TOL3);
  s += test_sf_check_result(message_buff,  G, -0.8984143590920205487, TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp, -0.5108047585190350106, TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s,"  gsl_sf_coulomb_wave_FG_e(1.0, 5.0, lam_F=0, lam_G=0)");
  status += s;

  lam_F = 10.0;
  k_G   = 2;
  eta = 1.0;
  x = 5.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  0.0006423773354915823698, TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp,  0.0013299570958719702545, TEST_TOL3);
  s += test_sf_check_result(message_buff,  G,  33.27615734455096130,     TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp, -45.49180102261540580,     TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s,"  gsl_sf_coulomb_wave_FG_e(1.0, 5.0, lam_F=10, lam_G=8)");
  status += s;

  lam_F = 4.0;
  k_G   = 2;
  eta = 50.0;
  x = 120.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  0.0735194711823798495, TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp,  0.6368149124126783325, TEST_TOL3);
  /*
  s += test_sf_check_result(message_buff,  G,  , TEST_TOL5);
  s += test_sf_check_result(message_buff, Gp, , TEST_TOL5);
  */
  printf("%s", message_buff);
  gsl_test(s,"  gsl_sf_coulomb_wave_FG_e(50.0, 120.0, lam_F=4, lam_G=2)");
  status += s;

  lam_F = 0.0;
  k_G = 0;
  eta = -1000.0;
  x = 1.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  9.68222518991341e-02, TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp,  5.12063396274631e+00, TEST_TOL3);
  s += test_sf_check_result(message_buff,  G,  1.13936784379472e-01, TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp, -4.30243486522438e+00, TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(-1000.0, 1.0, lam_F=0, lam_G=0)");
  status += s;

  lam_min = 0.0;
  eta = -50.0;
  x = 5.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  1.52236975714236e-01, TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp,  2.03091041166137e+00, TEST_TOL3);
  s += test_sf_check_result(message_buff,  G,  4.41680690236251e-01, TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp, -6.76485374766869e-01, TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(-50.0, 5.0, lam_F=0, lam_G=0)");
  status += s;

  lam_min = 0.0;
  eta = -50.0;
  x = 1000.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F, -0.2267212182760888523, TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp, -0.9961306810018401525, TEST_TOL3);
  s += test_sf_check_result(message_buff,  G, -0.9497684438900352186, TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp,  0.2377656295411961399, TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(-50.0, 1000.0, lam_F=0, lam_G=0)");
  status += s;

  lam_F = 10.0;
  k_G = 0;
  eta = -50.0;
  x = 5.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F, -3.681143602184922e-01, TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp,  1.338467510317215e+00, TEST_TOL3);
  s += test_sf_check_result(message_buff,  G,  3.315883246109351e-01, TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp,  1.510888628136180e+00, TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(-50.0, 5.0, lam_F=10, lam_G=10)");
  status += s;

  lam_F = 0.0;
  k_G = 0;
  eta = -4.0;
  x = 5.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  4.078627230056172e-01, TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp,  1.098212336357310e+00, TEST_TOL3);
  s += test_sf_check_result(message_buff,  G,  6.743270353832442e-01, TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp, -6.361104272804447e-01, TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(-4.0, 5.0, lam_F=0, lam_G=0");
  status += s;

  lam_F = 3.0;
  k_G = 0;
  eta = -4.0;
  x = 5.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F, -2.568630935581323e-01, TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp,  1.143229422014827e+00, TEST_TOL3);
  s += test_sf_check_result(message_buff,  G,  7.879899223927996e-01, TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp,  3.859905878106713e-01, TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(-4.0, 5.0, lam_F=3, lam_G=3");
  status += s;

  lam_F = 0.0;
  k_G = 0;
  eta = 1.0;
  x = 2.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  6.61781613832681e-01, TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp,  4.81557455709949e-01, TEST_TOL3);
  s += test_sf_check_result(message_buff,  G,  1.27577878476828e+00, TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp, -5.82728813097184e-01, TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(1.0, 2.0, lam_F=0, lam_G=0)");
  status += s;

  lam_F = 0.0;
  k_G = 0;
  eta = 1.0;
  x = 0.5;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  0.08315404535022023302, TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp,  0.22693874616222787568, TEST_TOL3);
  s += test_sf_check_result(message_buff,  G,  3.1060069279548875140,  TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp, -3.549156038719924236,   TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(1.0, 0.5, lam_F=0, lam_G=0)");
  status += s;

  lam_F = 0.5;
  k_G = 0;
  eta = 1.0;
  x = 0.5;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  0.04049078073829290935, TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp,  0.14194939168094778795, TEST_TOL3);
  s += test_sf_check_result(message_buff,  G,  4.720553853049677897,   TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp, -8.148033852319180005,   TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(1.0, 0.5, lam_F=0.5, lam_G=0.5)");
  status += s;

  lam_F = 0.1;
  k_G = 0;
  eta = 1.0;
  x = 0.5;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  0.07365466672379703418, TEST_TOL5);
  s += test_sf_check_result(message_buff, Fp,  0.21147121807178518647, TEST_TOL5);
  s += test_sf_check_result(message_buff,  G,  3.306705446241024890, TEST_TOL5);
  s += test_sf_check_result(message_buff, Gp, -4.082931670935696644, TEST_TOL5);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(1.0, 0.5, lam_F=0.1, lam_G=0.1)");
  status += s;

  lam_F = 0.0;
  k_G = 0;
  eta = 8.0;
  x = 1.05;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  9.882706082810274357e-09, TEST_TOL5);
  s += test_sf_check_result(message_buff, Fp,  4.005167028235547770e-08, TEST_TOL5);
  s += test_sf_check_result(message_buff,  G,  1.333127992006686320e+07, TEST_SQRT_TOL0);
  s += test_sf_check_result(message_buff, Gp, -4.715914530842402330e+07, TEST_SQRT_TOL0);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(8.0, 1.05, lam_F=0, lam_G=0)");
  status += s;

  lam_F = 0.1;
  k_G = 0;
  eta = 8.0;
  x = 1.05;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  9.611416736061987761e-09, TEST_TOL5);
  s += test_sf_check_result(message_buff, Fp,  3.909628126126824140e-08, TEST_TOL5);
  s += test_sf_check_result(message_buff,  G,  1.365928464219262581e+07, 4.0*TEST_SQRT_TOL0);
  s += test_sf_check_result(message_buff, Gp, -4.848117385783386850e+07, 4.0*TEST_SQRT_TOL0);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(8.0, 1.05, lam_F=0.1, lam_G=0.1)");
  status += s;

  lam_F = 0.0;
  k_G = 0;
  eta = 50.0;
  x = 0.1;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  2.807788027954216071e-67, TEST_TOL5);
  s += test_sf_check_result(message_buff, Fp,  9.677600748751576606e-66, TEST_TOL5);
  s += test_sf_check_result(message_buff,  G,  5.579810686998358766e+64, TEST_SQRT_TOL0);
  s += test_sf_check_result(message_buff, Gp, -1.638329512756321424e+66, TEST_SQRT_TOL0);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(50.0, 0.1, lam_F=0, lam_G=0)");
  status += s;

  lam_F = 0.0;
  k_G = 0;
  eta = 10.0;
  x = 5.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  1.7207454091787930614e-06, 10.0*WKB_TOL);
  s += test_sf_check_result(message_buff, Fp,  3.0975994706405458046e-06, 10.0*WKB_TOL);
  s += test_sf_check_result(message_buff,  G,  167637.56609459967623, 10.0*WKB_TOL);
  s += test_sf_check_result(message_buff, Gp, -279370.76655361803075, 10.0*WKB_TOL);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(10.0, 5.0, lam_F=0, lam_G=0)");
  status += s;

  lam_F = 0.0;
  k_G = 0;
  eta = 25.0;
  x = 10.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  1.5451274501076114315e-16, 5.0*WKB_TOL);
  s += test_sf_check_result(message_buff, Fp,  3.1390869393378630928e-16, 5.0*WKB_TOL);
  s += test_sf_check_result(message_buff,  G,  1.6177129008336318136e+15, 5.0*WKB_TOL);
  s += test_sf_check_result(message_buff, Gp, -3.1854062013149740860e+15, 5.0*WKB_TOL);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(25.0, 10.0, lam_F=0, lam_G=0)");
  status += s;

  lam_F = 0.0;
  k_G = 0;
  eta = 1.0;
  x = 9.2;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F, -0.25632012319757955655, TEST_TOL5);
  s += test_sf_check_result(message_buff, Fp,  0.91518792286724220370, TEST_TOL5);
  s += test_sf_check_result(message_buff,  G,  1.03120585918973466110, TEST_SQRT_TOL0);
  s += test_sf_check_result(message_buff, Gp,  0.21946326717491250193, TEST_SQRT_TOL0);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(1.0, 9.2, lam_F=0, lam_G=0)");
  status += s;

  lam_F = 0.0;
  eta = 10.0;
  x = 10.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  0.0016262711250135878249, WKB_TOL);
  s += test_sf_check_result(message_buff, Fp,  0.0017060476320792806014, WKB_TOL);
  s += test_sf_check_result(message_buff,  G,  307.87321661090837987, WKB_TOL);
  s += test_sf_check_result(message_buff, Gp, -291.92772380826822871, WKB_TOL);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(10.0, 10.0, lam_F=0, lam_G=0)");
  status += s;

  lam_F = 0.0;
  eta = 100.0;
  x = 1.0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  8.999367996930662705e-126, 10.0*WKB_TOL);
  s += test_sf_check_result(message_buff, Fp,  1.292746745757069321e-124, 10.0*WKB_TOL);
  s += test_sf_check_result(message_buff,  G,  3.936654148133683610e+123, 10.0*WKB_TOL);
  s += test_sf_check_result(message_buff, Gp, -5.456942268061526371e+124, 10.0*WKB_TOL);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(100.0, 1.0, lam_F=0, lam_G=0)");
  status += s;

  /* compute F_1(eta=0,x=3.25), F'_1 and G_1(eta=0,x=3.25), G'_1 */

  lam_F = 1.0;
  eta = 0.0;
  x = 3.25;
  k_G = 0;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  sin(x)/x - cos(x), TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp,  -sin(x)/(x*x) + cos(x)/x +sin(x), TEST_TOL3);
  s += test_sf_check_result(message_buff,  G,  cos(x)/x + sin(x), TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp,  -cos(x)/(x*x) - sin(x)/x + cos(x), TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(3.25, 0.0, lam_F=1, lam_G=1)");
  status += s;

  /* compute F_1(eta=0,x=3.25), F'_1 and G_0(eta=0,x=3.25), G'_0 */

  lam_F = 1.0;
  eta = 0.0;
  x = 3.25;
  k_G = 1;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F,  sin(x)/x - cos(x), TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp,  -sin(x)/(x*x) + cos(x)/x +sin(x), TEST_TOL3);
  s += test_sf_check_result(message_buff,  G,  cos(x), TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp,  -sin(x), TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(3.25, 0.0, lam_F=1, lam_G=0)");
  status += s;

#ifdef FIXME
  /* compute F_37(eta=0,x), F'_37 and G_36(eta=0,x), G'_36 for
     x=1.2693881947287221e-07 */

  /* For eta=0 expanding A&S 4.3.1 gives

     FplusIG(L,r)={I*exp(-I*r)*sum(k=0,L,((L+k)!/(k!*(L-k)!))*(I^(L-k))*(2*r)^(-k))
     
     or alternatively F+iG can be expressed in terms of bessel functions

     FplusIG(L,r)=sqrt(Pi*x/2)*besselh1(L+1/2,x))
  */

  lam_F = 37.0;
  eta = 0.0;
  x = 1.2693881947287221e-07;
  k_G = 1;
  gsl_sf_coulomb_wave_FG_e(eta, x, lam_F, k_G, &F, &Fp, &G, &Gp, &Fe, &Ge);
  s = 0;
  message_buff[0] = 0;
  s += test_sf_check_result(message_buff,  F, 6.5890724278623412974127e-318 , TEST_TOL3);
  s += test_sf_check_result(message_buff, Fp, 1.97248369961623986509839591990e-309, TEST_TOL3);
  s += test_sf_check_result(message_buff,  G, 4.46663541714903607940730e299, TEST_TOL3);
  s += test_sf_check_result(message_buff, Gp, -1.26674311046140805594543e308 , TEST_TOL3);
  printf("%s", message_buff);
  gsl_test(s, "  gsl_sf_coulomb_wave_FG_e(1.2693881947287221e-07, 0.0, lam_F=37, lam_G=36)");
  status += s;
#endif


  return status;
}
