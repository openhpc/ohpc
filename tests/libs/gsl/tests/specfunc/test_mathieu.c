/* specfunc/test_mathieu.c
 * 
 * Copyright (C) 2003 Lowell Johnson
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* Author:  L. Johnson */

#include <gsl/gsl_test.h>
#include <gsl/gsl_sf.h>
#include "test_sf.h"

#define NVAL 100

static double c[NVAL];

int test_mathieu(void)
{
  gsl_sf_result r;
  gsl_sf_mathieu_workspace *work;
  int s = 0;
  int sa;

  TEST_SF(s, gsl_sf_mathieu_ce_e, (0, 0.0, 0.0, &r),
          0.7071067811865475, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (0, 0.0, M_PI_2, &r),
          0.7071067811865475, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (0, 5.0, 0.0, &r),
          0.04480018165188902, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (0, 5.0, M_PI_2, &r),
          1.334848674698019, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (0, 10.0, 0.0, &r),
          0.007626517570935782, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (0, 10.0, M_PI_2, &r),
          1.468660470712856, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (0, 15.0, 0.0, &r),
          0.001932508315204592, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (0, 15.0, M_PI_2, &r),
          1.550108146686649, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (0, 20.0, 0.0, &r),
          0.0006037438292242197, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (0, 20.0, M_PI_2, &r),
          1.609890857395926, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (0, 25.0, 0.0, &r),
          0.0002158630184146612, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (0, 25.0, M_PI_2, &r),
          1.657510298323475, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (1, 0.0, 0.0, &r),
          1.00000000, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (1, 5.0, 0.0, &r),
          0.2565428793223637, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (1, 10.0, 0.0, &r),
          0.05359874774717657, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (1, 15.0, 0.0, &r),
          0.01504006645382623, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (1, 20.0, 0.0, &r),
          0.005051813764712904, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (1, 25.0, 0.0, &r),
          0.001911051506657645, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (1, 0.0, M_PI_2, &r),
          1.0000000, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (1, 5.0, M_PI_2, &r),
          1.337433887022345, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (1, 10.0, M_PI_2, &r),
          1.468755664102938, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (1, 15.0, M_PI_2, &r),
          1.550115074357552, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (1, 20.0, M_PI_2, &r),
          1.609891592603772, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (1, 25.0, M_PI_2, &r),
          1.657510398374516, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (2, 0.0, 0.0, &r),
          1.00000000, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (2, 0.0, M_PI_2, &r),
          -1.00000000, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (2, 5.0, 0.0, &r),
          0.7352943084006845, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (2, 5.0, M_PI_2, &r),
          -0.7244881519676682, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (2, 10.0, 0.0, &r),
          0.2458883492913189, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (2, 10.0, M_PI_2, &r),
          -0.9267592641263211, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (2, 15.0, 0.0, &r),
          0.07879282784639313, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (2, 15.0, M_PI_2, &r),
          -1.019966226030262, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (2, 20.0, 0.0, &r),
          0.02864894314707431, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (2, 20.0, M_PI_2, &r),
          -1.075293228779687, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (2, 25.0, 0.0, &r),
          0.0115128663308875, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (2, 25.0, M_PI_2, &r),
          -1.116278953295253, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (5, 0.0, 0.0, &r),
          1.00000000, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (5, 5.0, 0.0, &r),
          1.12480725063848, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (5, 10.0, 0.0, &r),
          1.258019941308287, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (5, 15.0, 0.0, &r),
          1.193432230413072, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (5, 20.0, 0.0, &r),
          0.9365755314226215, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (5, 25.0, 0.0, &r),
          0.6106943100506986, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (5, 0.0, M_PI_2, &r),
          1.0000000, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (5, 5.0, M_PI_2, &r),
          0.9060779302023551, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (5, 10.0, M_PI_2, &r),
          0.8460384335355106, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (5, 15.0, M_PI_2, &r),
          0.837949340012484, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (5, 20.0, M_PI_2, &r),
          0.8635431218533667, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (5, 25.0, M_PI_2, &r),
          0.8992683245108413, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (10, 0.0, 0.0, &r),
          1.00000000, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (10, 0.0, M_PI_2, &r),
          -1.00000000, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (10, 5.0, 0.0, &r),
          1.025995027089438, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (10, 5.0, M_PI_2, &r),
          -0.975347487235964, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (10, 10.0, 0.0, &r),
          1.053815992100935, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (10, 10.0, M_PI_2, &r),
          -0.9516453181789554, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (10, 15.0, 0.0, &r),
          1.084106311839221, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (10, 15.0, M_PI_2, &r),
          -0.9285480638845388, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (10, 20.0, 0.0, &r),
          1.117788631259397, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (10, 20.0, M_PI_2, &r),
          -0.9057107845940974, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (10, 25.0, 0.0, &r),
          1.156239918632239, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (10, 25.0, M_PI_2, &r),
          -0.8826919105636903, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (15, 0.0, 0.0, &r),
          1.00000000, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (15, 5.0, 0.0, &r),
          1.011293732529566, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (15, 10.0, 0.0, &r),
          1.022878282438181, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (15, 15.0, 0.0, &r),
          1.034793652236873, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (15, 20.0, 0.0, &r),
          1.047084344162887, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_ce_e, (15, 25.0, 0.0, &r),
          1.059800441813937, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (15, 0.0, M_PI_2, &r),
          -1.0000000, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (15, 5.0, M_PI_2, &r),
          -0.9889607027406357, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (15, 10.0, M_PI_2, &r),
          -0.9781423471832157, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (15, 15.0, M_PI_2, &r),
          -0.9675137031854538, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (15, 20.0, M_PI_2, &r),
          -0.9570452540612817, TEST_SNGL, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_mathieu_se_e, (15, 25.0, M_PI_2, &r),
          -0.9467086958780897, TEST_SNGL, GSL_SUCCESS);

  work = gsl_sf_mathieu_alloc(NVAL, 20.0);
  sa = 0;
  gsl_sf_mathieu_ce_array(0, 5, 0.0, M_PI_2, work, c);
  sa += (test_sf_frac_diff(c[0], 0.7071067811865475) > TEST_SNGL);
  sa += (test_sf_frac_diff(c[2], -1.0) > TEST_SNGL);
  sa += (test_sf_frac_diff(c[4], 1.0) > TEST_SNGL);
  gsl_test(sa, "gsl_sf_mathieu_ce_array");
  s += sa;

  sa = 0;
  gsl_sf_mathieu_ce_array(0, 15, 20.0, 0.0, work, c);
  sa += (test_sf_frac_diff(c[0], 0.0006037438292242197) > TEST_SNGL);
  sa += (test_sf_frac_diff(c[1], 0.005051813764712904) > TEST_SNGL);
  sa += (test_sf_frac_diff(c[2], 0.02864894314707431) > TEST_SNGL);
  sa += (test_sf_frac_diff(c[5], 0.9365755314226215) > TEST_SNGL);
  sa += (test_sf_frac_diff(c[10], 1.117788631259397) > TEST_SNGL);
  sa += (test_sf_frac_diff(c[15], 1.047084344162887) > TEST_SNGL);
  gsl_test(sa, "gsl_sf_mathieu_ce_array");
  s += sa;

  sa = 0;
  gsl_sf_mathieu_se_array(1, 15, 20.0, M_PI_2, work, c);
  sa += (test_sf_frac_diff(c[0], 1.609891592603772) > TEST_SNGL);
  sa += (test_sf_frac_diff(c[4], 0.8635431218533667) > TEST_SNGL);
  sa += (test_sf_frac_diff(c[14], -0.9570452540612817) > TEST_SNGL);
  gsl_test(sa, "gsl_sf_mathieu_se_array");
  s += sa;

  gsl_sf_mathieu_free(work);
  return s;
}
