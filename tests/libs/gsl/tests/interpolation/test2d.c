/* interpolation/test2d.c
 * 
 * Copyright 2012 David Zaslavsky
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

#include <config.h>
#include <math.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_interp.h>
#include <gsl/gsl_interp2d.h>
#include <gsl/gsl_spline2d.h>

/* tests a single evaluator function from the low-level interface */
static int
test_single_low_level(
  double (*evaluator)(const gsl_interp2d *, const double[], const double[],
                      const double[], const double, const double,
                      gsl_interp_accel *, gsl_interp_accel *),
  int (*evaluator_e)(const gsl_interp2d *, const double[], const double[],
                     const double[], const double, const double,
                     gsl_interp_accel *, gsl_interp_accel *, double *),
  const gsl_interp2d * interp, const double xarr[], const double yarr[],
  const double zarr[], const double x, const double y,
  gsl_interp_accel * xa, gsl_interp_accel * ya,
  const double expected_results[], size_t i)
{
  if (expected_results != NULL)
    {
      int status;
      double result = evaluator(interp, xarr, yarr, zarr, x, y, xa, ya);
      gsl_test_rel(result, expected_results[i], 1e-10,
                   "low level %s %d", gsl_interp2d_name(interp), i);

      status = evaluator_e(interp, xarr, yarr, zarr, x, y, xa, ya, &result);
      if (status == GSL_SUCCESS)
        {
          gsl_test_rel(result, expected_results[i], 1e-10,
                       "low level _e %s %d", gsl_interp2d_name(interp), i);
        }
    }

  return 0;
}

/* tests a single evaluator function from the high-level interface */
static int
test_single_high_level(
    double (*evaluator)(const gsl_spline2d *, const double, const double,
                        gsl_interp_accel *, gsl_interp_accel *),
    int (*evaluator_e)(const gsl_spline2d *, const double, const double,
                       gsl_interp_accel *, gsl_interp_accel *, double *),
    const gsl_spline2d * interp, const double x, const double y,
    gsl_interp_accel * xa, gsl_interp_accel * ya,
    const double expected_results[], size_t i)
{
  if (expected_results != NULL)
    {
      int status;
      double result = evaluator(interp, x, y, xa, ya);

      gsl_test_rel(result, expected_results[i], 1e-10,
                   "high level %s %d", gsl_spline2d_name(interp), i);

      status = evaluator_e(interp, x, y, xa, ya, &result);
      if (status == GSL_SUCCESS)
        {
          gsl_test_rel(result, expected_results[i], 1e-10,
                       "high level _e %s %d", gsl_spline2d_name(interp), i);
        }
    }

  return 0;
}

/*
 * Tests that a given interpolation type reproduces the data points
 * it is given, and then tests that it correctly reproduces additional
 * values.
 */
static int
test_interp2d(
const double xarr[], const double yarr[], const double zarr[], /* data */
              size_t xsize, size_t ysize,                /* array sizes */
              const double xval[], const double yval[],  /* test points */
              const double zval[],                       /* expected results */
              const double zxval[], const double zyval[],
              const double zxxval[], const double zyyval[], const double zxyval[],
              size_t test_size,                          /* number of test points */
              const gsl_interp2d_type * T)
{
  gsl_interp_accel *xa = gsl_interp_accel_alloc();
  gsl_interp_accel *ya = gsl_interp_accel_alloc();
  int status = 0;
  size_t xi, yi, zi, i;
  gsl_interp2d * interp = gsl_interp2d_alloc(T, xsize, ysize);
  gsl_spline2d * interp_s = gsl_spline2d_alloc(T, xsize, ysize);
  unsigned int min_size = gsl_interp2d_type_min_size(T);

  gsl_test_int(min_size, T->min_size, "gsl_interp2d_type_min_size on %s", gsl_interp2d_name(interp));

  gsl_interp2d_init(interp, xarr, yarr, zarr, xsize, ysize);
  gsl_spline2d_init(interp_s, xarr, yarr, zarr, xsize, ysize);

  /* First check that the interpolation reproduces the given points */
  for (xi = 0; xi < xsize; xi++)
    {
      double x = xarr[xi];
      for (yi = 0; yi < ysize; yi++)
        {
          double y = yarr[yi];
          
          zi = gsl_interp2d_idx(interp, xi, yi);
          test_single_low_level(&gsl_interp2d_eval, &gsl_interp2d_eval_e,
                                interp, xarr, yarr, zarr, x, y,
                                xa, ya, zarr, zi);
          test_single_low_level(&gsl_interp2d_eval_extrap,
                                &gsl_interp2d_eval_e_extrap, interp,
                                xarr, yarr, zarr, x, y, xa, ya, zarr, zi);
          test_single_high_level(&gsl_spline2d_eval, &gsl_spline2d_eval_e,
                                 interp_s, x, y, xa, ya, zarr, zi);
        }
    }

  /* Then check additional points provided */
  for (i = 0; i < test_size; i++)
    {
      double x = xval[i];
      double y = yval[i];
        
      test_single_low_level(&gsl_interp2d_eval,         &gsl_interp2d_eval_e,          interp, xarr, yarr, zarr, x, y, xa, ya, zval, i);
      test_single_low_level(&gsl_interp2d_eval_deriv_x, &gsl_interp2d_eval_deriv_x_e,  interp, xarr, yarr, zarr, x, y, xa, ya, zxval, i);
      test_single_low_level(&gsl_interp2d_eval_deriv_y, &gsl_interp2d_eval_deriv_y_e,  interp, xarr, yarr, zarr, x, y, xa, ya, zyval, i);
      test_single_low_level(&gsl_interp2d_eval_deriv_xx,&gsl_interp2d_eval_deriv_xx_e, interp, xarr, yarr, zarr, x, y, xa, ya, zxxval, i);
      test_single_low_level(&gsl_interp2d_eval_deriv_yy,&gsl_interp2d_eval_deriv_yy_e, interp, xarr, yarr, zarr, x, y, xa, ya, zyyval, i);
      test_single_low_level(&gsl_interp2d_eval_deriv_xy,&gsl_interp2d_eval_deriv_xy_e, interp, xarr, yarr, zarr, x, y, xa, ya, zxyval, i);

      test_single_high_level(&gsl_spline2d_eval,         &gsl_spline2d_eval_e,          interp_s, x, y, xa, ya, zval, i);
      test_single_high_level(&gsl_spline2d_eval_deriv_x, &gsl_spline2d_eval_deriv_x_e,  interp_s, x, y, xa, ya, zxval, i);
      test_single_high_level(&gsl_spline2d_eval_deriv_y, &gsl_spline2d_eval_deriv_y_e,  interp_s, x, y, xa, ya, zyval, i);
      test_single_high_level(&gsl_spline2d_eval_deriv_xx,&gsl_spline2d_eval_deriv_xx_e, interp_s, x, y, xa, ya, zxxval, i);
      test_single_high_level(&gsl_spline2d_eval_deriv_yy,&gsl_spline2d_eval_deriv_yy_e, interp_s, x, y, xa, ya, zyyval, i);
      test_single_high_level(&gsl_spline2d_eval_deriv_xy,&gsl_spline2d_eval_deriv_xy_e, interp_s, x, y, xa, ya, zxyval, i);

      test_single_low_level(&gsl_interp2d_eval_extrap, &gsl_interp2d_eval_e_extrap, interp, xarr, yarr, zarr, x, y, xa, ya, zval, i);
    }

  gsl_interp_accel_free(xa);
  gsl_interp_accel_free(ya);
  gsl_interp2d_free(interp);
  gsl_spline2d_free(interp_s);

  return status;
}

/*
 * Tests bilinear interpolation using a symmetric function, f(x,y)==f(y,x),
 * and diagonal interpolation points (x,y) where x==y. If these tests don't
 * pass, something is seriously broken.
 */
static int
test_bilinear_symmetric()
{
  int status;
  double xarr[] = {0.0, 1.0, 2.0, 3.0};
  double yarr[] = {0.0, 1.0, 2.0, 3.0};
  double zarr[] = {1.0, 1.1, 1.2, 1.3,
                   1.1, 1.2, 1.3, 1.4,
                   1.2, 1.3, 1.4, 1.5,
                   1.3, 1.4, 1.5, 1.6};
  double xval[] = {0.0, 0.5, 1.0, 1.5, 2.5, 3.0};
  double yval[] = {0.0, 0.5, 1.0, 1.5, 2.5, 3.0};
  double zval[] = {1.0, 1.1, 1.2, 1.3, 1.5, 1.6};
  size_t xsize = sizeof(xarr) / sizeof(xarr[0]);
  size_t ysize = sizeof(yarr) / sizeof(yarr[0]);
  size_t test_size = sizeof(xval) / sizeof(xval[0]);

  status = test_interp2d(xarr, yarr, zarr, xsize, ysize, xval, yval, zval,
                         NULL, NULL, NULL, NULL, NULL, test_size,
                         gsl_interp2d_bilinear);
  gsl_test(status, "bilinear interpolation with symmetric values");

  return status;
}

/*
 * Tests bilinear interpolation with an asymmetric function, f(x,y)!=f(y,x),
 * and off-diagonal interpolation points (x,y) where x and y may or may not
 * be equal.
 */
static int
test_bilinear_asymmetric_z()
{
  int status;
  double xarr[] = {0.0, 1.0, 2.0, 3.0};
  double yarr[] = {0.0, 1.0, 2.0, 3.0};
  double zarr[] = {1.0, 1.1, 1.2, 1.4,
                   1.3, 1.4, 1.5, 1.7,
                   1.5, 1.6, 1.7, 1.9,
                   1.6, 1.9, 2.2, 2.3};
  double xval[] = { 0.0, 0.5, 1.0, 1.5,  2.5, 3.0,
                    1.3954, 1.6476, 0.824957,
                    2.41108,  2.98619, 1.36485 };
  double yval[] = {0.0, 0.5, 1.0, 1.5,  2.5, 3.0,
                   0.265371, 2.13849, 1.62114,
                   1.22198, 0.724681, 0.0596087 };

  /* results computed using Mathematica 9.0.1.0 */
  double zval[] = {1.0, 1.2, 1.4, 1.55, 2.025, 2.3,
                   1.2191513, 1.7242442248, 1.5067237,
                   1.626612, 1.6146423, 1.15436761};
  size_t xsize = sizeof(xarr) / sizeof(xarr[0]);
  size_t ysize = sizeof(yarr) / sizeof(yarr[0]);
  size_t test_size = sizeof(xval) / sizeof(xval[0]);

  status = test_interp2d(xarr, yarr, zarr, xsize, ysize, xval, yval, zval,
                         NULL, NULL, NULL, NULL, NULL, test_size,
                         gsl_interp2d_bilinear);
  gsl_test(status, "bilinear interpolation with asymmetric z values");

  return status;
}

static int
test_bicubic()
{
  int status;
  double xarr[] = {0.0, 1.0, 2.0, 3.0};
  double yarr[] = {0.0, 1.0, 2.0, 3.0};
  double zarr[] = {1.0, 1.1, 1.2, 1.3,
                   1.1, 1.2, 1.3, 1.4,
                   1.2, 1.3, 1.4, 1.5,
                   1.3, 1.4, 1.5, 1.6};
  double xval[] = {1.0, 1.5, 2.0};
  double yval[] = {1.0, 1.5, 2.0};
  double zval[] = {1.2, 1.3, 1.4};
  size_t xsize = sizeof(xarr) / sizeof(xarr[0]);
  size_t ysize = sizeof(yarr) / sizeof(yarr[0]);
  size_t test_size = sizeof(xval) / sizeof(xval[0]);

  status = test_interp2d(xarr, yarr, zarr, xsize, ysize, xval, yval, zval,
                         NULL, NULL, NULL, NULL, NULL, test_size,
                         gsl_interp2d_bicubic);
  gsl_test(status, "bicubic interpolation on linear function");

  return status;
}

static int
test_bicubic_nonlinear()
{
  int status;
  double xarr[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0};
  double yarr[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0};
  /* least common multiple of x and y */
  double zarr[] = { 1,  2,  3,  4,  5,  6,  7,  8,
                    2,  2,  6,  4, 10,  6, 14,  8,
                    3,  6,  3, 12, 15,  6, 21, 24,
                    4,  4, 12,  4, 20, 12, 28,  8,
                    5, 10, 15, 20,  5, 30, 35, 40,
                    6,  6,  6, 12, 30,  6, 42, 24,
                    7, 14, 21, 28, 35, 42,  7, 56,
                    8,  8, 24,  8, 40, 24, 56,  8};
  double xval[] = {1.4, 2.3, 4.7, 3.3, 7.5, 6.6, 5.1};
  double yval[] = {1.0, 1.8, 1.9, 2.5, 2.7, 4.1, 3.3};

  /* results computed using GSL 1D cubic interpolation twice */
  double zval[] = { 1.4, 3.11183531264736, 8.27114315792559, 5.03218982537718,
                    22.13230634702637, 23.63206834997871, 17.28553080971182 };
  size_t xsize = sizeof(xarr) / sizeof(xarr[0]);
  size_t ysize = sizeof(yarr) / sizeof(yarr[0]);
  size_t test_size = sizeof(xval) / sizeof(xval[0]);

  status = test_interp2d(xarr, yarr, zarr, xsize, ysize, xval, yval, zval,
                         NULL, NULL, NULL, NULL, NULL, test_size,
                         gsl_interp2d_bicubic);
  gsl_test(status, "bicubic interpolation on nonlinear symmetric function");

  return status;
}

/* This function contributed by Andrew W. Steiner <awsteiner@gmail.com> */
static int
test_bicubic_nonlinear_nonsq()
{
  int status;
  double xarr[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0};
  double yarr[] = {1.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0};
  double zarr[] = { 1,  2,  3,  4,  5,  6,  7,  8, 9, 10,
                    2,  2,  6,  4, 10,  6, 14,  8, 11, 12,
                    3,  6,  3, 12, 15,  6, 21, 24, 13, 14,
                    4,  4, 12,  4, 20, 12, 28,  8, 15, 16,
                    5, 10, 15, 20,  5, 30, 35, 40, 17, 18,
                    6,  6,  6, 12, 30,  6, 42, 24, 19, 20,
                    7, 14, 21, 28, 35, 42,  7, 56, 21, 22,
                    8,  8, 24,  8, 40, 24, 56,  8, 23, 24};
  double xval[] = {1.4, 2.3, 9.7, 3.3, 9.5, 6.6, 5.1};
  double yval[] = {1.0, 1.8, 1.9, 2.5, 2.7, 4.1, 3.3};

  /* results computed using GSL 1D cubic interpolation twice */
  double zval[] = { 1.4, 2.46782030941187003, 10.7717721621846465,
                    4.80725067958096375, 11.6747032398627297,
                    11.2619968682970111, 9.00168877916872567};
  size_t xsize = sizeof(xarr) / sizeof(xarr[0]);
  size_t ysize = sizeof(yarr) / sizeof(yarr[0]);
  size_t test_size = sizeof(xval) / sizeof(xval[0]);

  status = test_interp2d(xarr, yarr, zarr, xsize, ysize, xval, yval, zval,
                         NULL, NULL, NULL, NULL, NULL, test_size,
                         gsl_interp2d_bicubic);
  gsl_test(status, "bicubic interpolation on nonlinear symmetric function");

  return status;
}

/* runs all the tests */
int
test_interp2d_main(void)
{
  int status = 0;

  status += test_bilinear_symmetric();
  status += test_bilinear_asymmetric_z();
  status += test_bicubic();
  status += test_bicubic_nonlinear();
  status += test_bicubic_nonlinear_nonsq();

  return status;
}
