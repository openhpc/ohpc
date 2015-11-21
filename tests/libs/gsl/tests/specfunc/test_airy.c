/* specfunc/test_airy.c
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


int test_airy(void)
{
  int s = 0;
  int m = GSL_MODE_DEFAULT;
  gsl_sf_result r;

  /** functions */

  TEST_SF(s, gsl_sf_airy_Ai_e, (-500.0, m, &r),              0.0725901201040411396, TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_e, (-5.0, m, &r),                0.3507610090241142,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_e, (-0.3000000000000094, m, &r), 0.4309030952855831,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_e, (0.6999999999999907, m, &r),  0.1891624003981519,    TEST_TOL0, GSL_SUCCESS);

  /*  This original value seemed to be slightly inaccurate in the last place.
      I recomputed it with pari to get the new value which end in 885 
      instead of 882 */
  /*
    TEST_SF(s, gsl_sf_airy_Ai_e, (1.649999999999991, m, &r),   0.05831058618720882,   TEST_TOL0, GSL_SUCCESS);
    */
  
  TEST_SF(s, gsl_sf_airy_Ai_e, (1.649999999999991, m, &r),   0.0583105861872088521,   TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_airy_Ai_e, (2.54999999999999, m, &r),    0.01446149513295428,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_e, (3.499999999999987, m, &r),   0.002584098786989702,  TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_e, (5.39999999999998, m, &r),    4.272986169411866e-05, TEST_TOL0, GSL_SUCCESS);
  
  TEST_SF(s, gsl_sf_airy_Ai_scaled_e, (-5.0, m, &r),                  0.3507610090241142, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_scaled_e, (0.6999999999999907, m, &r), 0.2795125667681217, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_scaled_e, (1.649999999999991, m, &r),  0.2395493001442741, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_scaled_e, (2.54999999999999, m, &r),   0.2183658595899388, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_scaled_e, (3.499999999999987, m, &r),  0.2032920808163519, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_scaled_e, (5.39999999999998, m, &r),   0.1836050093282229, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_airy_Bi_e, (-500.0, m, &r),             -0.094688570132991028, TEST_TOL4, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_e, (-5.0, m, &r),               -0.1383691349016005,   TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_e, (0.6999999999999907, m, &r),  0.9733286558781599,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_e, (1.649999999999991, m, &r),   2.196407956850028,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_e, (2.54999999999999, m, &r),    6.973628612493443,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_e, (3.499999999999987, m, &r),   33.05550675461069,    TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_e, (5.39999999999998, m, &r),    1604.476078241272,    TEST_TOL1, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_airy_Bi_scaled_e, (-5.0, m, &r),                  -0.1383691349016005, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_scaled_e, (0.6999999999999907, m, &r),  0.6587080754582302, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_scaled_e, (1.649999999999991, m, &r),   0.5346449995597539, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_scaled_e, (2.54999999999999, m, &r),    0.461835455542297,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_scaled_e, (3.499999999999987, m, &r),   0.4201771882353061, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_scaled_e, (5.39999999999998, m, &r),    0.3734050675720473, TEST_TOL0, GSL_SUCCESS);


  /** derivatives */

  TEST_SF(s, gsl_sf_airy_Ai_deriv_e, (-5.0, m, &r),                 0.3271928185544435,       TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_deriv_e, (-0.5500000000000094, m, &r), -0.1914604987143629,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_deriv_e, (0.4999999999999906, m, &r),  -0.2249105326646850,    TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_deriv_e, (1.899999999999992, m, &r),   -0.06043678178575718,   TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_deriv_e, (3.249999999999988, m, &r),   -0.007792687926790889,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_deriv_e, (5.199999999999981, m, &r),   -0.0001589434526459543, TEST_TOL1, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_airy_Ai_deriv_scaled_e, (-5.0, m, &r),                0.3271928185544435, TEST_TOL1, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_deriv_scaled_e, (0.5499999999999906, m, &r), -0.2874057279170166, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_deriv_scaled_e, (1.499999999999991, m, &r),  -0.3314199796863637, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_deriv_scaled_e, (2.49999999999999, m, &r),   -0.3661089384751620, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_deriv_scaled_e, (3.649999999999986, m, &r),  -0.3974033831453963, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Ai_deriv_scaled_e, (6.299999999999977, m, &r),  -0.4508799189585947, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_airy_Bi_deriv_e, (-5.0, m, &r),                0.778411773001899,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_deriv_e, (-0.5500000000000094, m, &r), 0.5155785358765014, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_deriv_e, (0.4999999999999906, m, &r),  0.5445725641405883, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_deriv_e, (1.899999999999992, m, &r),   3.495165862891568,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_deriv_e, (3.249999999999988, m, &r),   36.55485149250338,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_deriv_e, (5.199999999999981, m, &r),   2279.748293583233,  TEST_TOL1, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_airy_Bi_deriv_scaled_e, (-5.0, m, &r),               0.778411773001899,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_deriv_scaled_e, (0.5499999999999906, m, &r), 0.4322811281817566, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_deriv_scaled_e, (1.499999999999991, m, &r),  0.5542307563918037, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_deriv_scaled_e, (2.49999999999999, m, &r),   0.6755384441644985, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_deriv_scaled_e, (3.649999999999986, m, &r),  0.7613959373000228, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_Bi_deriv_scaled_e, (6.299999999999977, m, &r),  0.8852064139737571, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_airy_zero_Ai_e, (2, &r),  -4.087949444130970617, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Ai_e, (50,   &r), -38.02100867725525443, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Ai_e, (100, &r),  -60.45555727411669871, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Ai_e, (110, &r),  -64.43135670991324811, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_airy_zero_Bi_e, (2,   &r), -3.271093302836352716, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Bi_e, (50,  &r), -37.76583438165180116, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Bi_e, (100, &r), -60.25336482580837088, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Bi_e, (110, &r), -64.2355167606561537,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Bi_e, (111, &r), -64.6268994819519378,  TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Bi_e, (200, &r), -95.88699147356682665, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_airy_zero_Ai_deriv_e, (2,    &r), -3.248197582179836561, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Ai_deriv_e, (50,   &r), -37.76565910053887108, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Ai_deriv_e, (100,  &r), -60.25329596442479317, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Ai_deriv_e, (110,  &r), -64.23545617243546956, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Ai_deriv_e, (1000, &r), -280.9378080358935071, TEST_TOL0, GSL_SUCCESS);

  TEST_SF(s, gsl_sf_airy_zero_Bi_deriv_e, (2,    &r), -4.073155089071828216, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Bi_deriv_e, (50,   &r), -38.02083574095788210, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Bi_deriv_e, (100,  &r), -60.45548887257140819, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Bi_deriv_e, (110,  &r), -64.43129648944845060, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Bi_deriv_e, (111,  &r), -64.82208737584206093, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Bi_deriv_e, (200,  &r), -96.04731050310324450, TEST_TOL0, GSL_SUCCESS);
  TEST_SF(s, gsl_sf_airy_zero_Bi_deriv_e, (1000, &r), -281.0315164471118527, TEST_TOL0, GSL_SUCCESS);

  return s;
}
