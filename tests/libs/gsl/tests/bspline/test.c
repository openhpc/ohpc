/* bspline/test.c
 *
 * Copyright (C) 2006, 2007, 2009 Brian Gough
 * Copyright (C) 2008, 2011 Rhys Ulerich
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
#include <gsl/gsl_test.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_bspline.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_nan.h>

void
test_bspline(gsl_bspline_workspace * bw, gsl_bspline_deriv_workspace * dbw)
{
  gsl_vector *B;
  gsl_matrix *dB;
  size_t i, j;
  size_t n = 100;
  size_t ncoeffs = gsl_bspline_ncoeffs(bw);
  size_t order = gsl_bspline_order(bw);
  size_t nbreak = gsl_bspline_nbreak(bw);
  double a = gsl_bspline_breakpoint(0, bw);
  double b = gsl_bspline_breakpoint(nbreak - 1, bw);

  B  = gsl_vector_alloc(ncoeffs);
  dB = gsl_matrix_alloc(ncoeffs, 1);

  /* Ensure B-splines form a partition of unity */
  for (i = 0; i < n; i++)
    {
      double xi = a + (b - a) * (i / (n - 1.0));
      double sum = 0;
      gsl_bspline_eval(xi, B, bw);

      for (j = 0; j < ncoeffs; j++)
        {
          double Bj = gsl_vector_get(B, j);
          int s = (Bj < 0 || Bj > 1);
          gsl_test(s,
                   "basis-spline coefficient %u is in range [0,1] for x=%g",
                   j, xi);
          sum += Bj;
        }

      gsl_test_rel(sum, 1.0, order * GSL_DBL_EPSILON,
                   "basis-spline order %u is normalized for x=%g", order,
                   xi);
    }

  /* Ensure B-splines 0th derivatives agree with regular evaluation */
  for (i = 0; i < n; i++)
    {
      double xi = a + (b - a) * (i / (n - 1.0));
      gsl_bspline_eval(xi, B, bw);
      gsl_bspline_deriv_eval(xi, 0, dB, bw, dbw);

      for (j = 0; j < ncoeffs; j++)
        {
          gsl_test_abs(gsl_matrix_get(dB, j, 0), gsl_vector_get(B, j),
                       GSL_DBL_EPSILON,
                       "b-spline order %d basis #%d evaluation and 0th derivative consistent for x=%g",
                       order, j, xi);
        }

    }

  gsl_vector_free(B);
  gsl_matrix_free(dB);
}

int
main(int argc, char **argv)
{
  size_t order, breakpoints, i;

  gsl_ieee_env_setup();

  argc = 0;                     /* prevent warnings about unused parameters */
  argv = 0;

  for (order = 1; order < 10; order++)
    {
      for (breakpoints = 2; breakpoints < 100; breakpoints++)
        {
          double a = -1.23 * order, b = 45.6 * order;
          gsl_bspline_workspace *bw = gsl_bspline_alloc(order, breakpoints);
          gsl_bspline_deriv_workspace *dbw = gsl_bspline_deriv_alloc(order);
          gsl_bspline_knots_uniform(a, b, bw);
          test_bspline(bw, dbw);
          gsl_bspline_deriv_free(dbw);
          gsl_bspline_free(bw);
        }
    }


  for (order = 1; order < 10; order++)
    {
      for (breakpoints = 2; breakpoints < 100; breakpoints++)
        {
          double a = -1.23 * order, b = 45.6 * order;
          gsl_bspline_workspace *bw = gsl_bspline_alloc(order, breakpoints);
          gsl_bspline_deriv_workspace *dbw = gsl_bspline_deriv_alloc(order);
          gsl_vector *k = gsl_vector_alloc(breakpoints);
          for (i = 0; i < breakpoints; i++)
            {
              double f, x;
              f = sqrt(i / (breakpoints - 1.0));
              x = (1 - f) * a + f * b;
              gsl_vector_set(k, i, x);
            };
          gsl_bspline_knots(k, bw);
          test_bspline(bw, dbw);
          gsl_vector_free(k);
          gsl_bspline_deriv_free(dbw);
          gsl_bspline_free(bw);
        }
    }

  /* Spot check known 0th, 1st, 2nd derivative
     evaluations for a particular k = 2 case.  */
  {
    size_t i, j; /* looping */

    const double xloc[4]     =  { 0.0,  1.0,  6.0,  7.0};
    const double deriv[4][3] =
    {
      { -1.0/2.0,  1.0/2.0, 0.0     },
      { -1.0/2.0,  1.0/2.0, 0.0     },
      {      0.0, -1.0/5.0, 1.0/5.0 },
      {      0.0, -1.0/5.0, 1.0/5.0 }
    };

    gsl_bspline_workspace *bw = gsl_bspline_alloc(2, 3);
    gsl_bspline_deriv_workspace *dbw = gsl_bspline_deriv_alloc(2);
    gsl_matrix *dB = gsl_matrix_alloc(gsl_bspline_ncoeffs(bw),
                                      gsl_bspline_order(bw) + 1);

    gsl_vector *breakpts = gsl_vector_alloc(3);
    gsl_vector_set(breakpts, 0, 0.0);
    gsl_vector_set(breakpts, 1, 2.0);
    gsl_vector_set(breakpts, 2, 7.0);
    gsl_bspline_knots(breakpts, bw);


    for (i = 0; i < 4; ++i)  /* at each location */
      {
        /* Initialize dB with poison to ensure we overwrite it */
        gsl_matrix_set_all(dB, GSL_NAN);

        gsl_bspline_deriv_eval(xloc[i], gsl_bspline_order(bw), dB, bw, dbw);
        for (j = 0; j < gsl_bspline_ncoeffs(bw) ; ++j)
          {
            /* check basis function 1st deriv */
            gsl_test_abs(gsl_matrix_get(dB, j, 1), deriv[i][j], GSL_DBL_EPSILON,
                         "b-spline k=%d basis #%d derivative %d at x = %f",
                         gsl_bspline_order(bw), j, 1, xloc[i]);
          }
        for (j = 0; j < gsl_bspline_ncoeffs(bw); ++j)
          {
            /* check k order basis function has k-th deriv equal to 0 */
            gsl_test_abs(gsl_matrix_get(dB, j, gsl_bspline_order(bw)), 0.0,
                         GSL_DBL_EPSILON,
                         "b-spline k=%d basis #%d derivative %d at x = %f",
                         gsl_bspline_order(bw), j, gsl_bspline_order(bw),
                         xloc[i]);
          }
      }

    gsl_matrix_free(dB);
    gsl_bspline_deriv_free(dbw);
    gsl_bspline_free(bw);
    gsl_vector_free(breakpts);
  }

  /* Spot check known 0th, 1st, 2nd derivative
     evaluations for a particular k = 3 case.  */
  {
    size_t i, j; /* looping */
    const double xloc[5]     =  { 0.0, 5.0, 9.0, 12.0, 15.0 };
    const double eval[5][6]  =
    {
      { 4./25.,  69./100.,   3./ 20. ,  0.    , 0.   , 0.    },
      { 0.     ,  4./21. , 143./210. ,  9./70., 0.   , 0.    },
      { 0.     ,  0.     ,   3./ 10. ,  7./10., 0.   , 0.    },
      { 0.     ,  0.     ,   0.      ,  3./4. , 1./4., 0.    },
      { 0.     ,  0.     ,   0.      ,  1./3. , 5./9., 1./9. }
    };
    const double deriv[5][6] =
    {
      { -4./25.,  3./50.,   1./ 10.,  0.    , 0.    , 0.      },
      {  0.    , -2./21.,   1./105.,  3./35., 0.    , 0.      },
      {  0.    ,  0.    ,  -1./5.  ,  1./ 5., 0.    , 0.      },
      {  0.    ,  0.    ,   0.     , -1./ 6., 1./6. , 0.      },
      {  0.    ,  0.    ,   0.     , -1./ 9., 1./27., 2./27. }
    };
    const double deriv2[5][6] =
    {
      { 2./25., -17./150.,   1.0/30.0 ,  0.0     ,  0.     , 0.     },
      { 0.    ,   1./ 42., -11.0/210.0,  1.0/35.0,  0.     , 0.     },
      { 0.    ,   0.     ,   1.0/15.0 ,-11.0/90.0,  1./18. , 0.     },
      { 0.    ,   0.     ,   0.0      ,  1.0/54.0, -7./162., 2./81. },
      { 0.    ,   0.     ,   0.0      ,  1.0/54.0, -7./162., 2./81. }
    };

    gsl_bspline_workspace *bw = gsl_bspline_alloc(3, 5);
    gsl_bspline_deriv_workspace *dbw = gsl_bspline_deriv_alloc(3);

    gsl_matrix *dB = gsl_matrix_alloc(gsl_bspline_ncoeffs(bw),
                                      gsl_bspline_order(bw) + 1);

    gsl_vector *breakpts = gsl_vector_alloc(5);
    gsl_vector_set(breakpts, 0, -3.0);
    gsl_vector_set(breakpts, 1,  2.0);
    gsl_vector_set(breakpts, 2,  9.0);
    gsl_vector_set(breakpts, 3, 12.0);
    gsl_vector_set(breakpts, 4, 21.0);
    gsl_bspline_knots(breakpts, bw);

    for (i = 0; i < 5; ++i)  /* at each location */
      {
        /* Initialize dB with poison to ensure we overwrite it */
        gsl_matrix_set_all(dB, GSL_NAN);
        gsl_bspline_deriv_eval(xloc[i], gsl_bspline_order(bw), dB, bw, dbw);

        /* check basis function evaluation */
        for (j = 0; j < gsl_bspline_ncoeffs(bw); ++j)
          {
            gsl_test_abs(gsl_matrix_get(dB, j, 0), eval[i][j], GSL_DBL_EPSILON,
                         "b-spline k=%d basis #%d derivative %d at x = %f",
                         gsl_bspline_order(bw), j, 0, xloc[i]);
          }
        /* check 1st derivative evaluation */
        for (j = 0; j < gsl_bspline_ncoeffs(bw); ++j)
          {
            gsl_test_abs(gsl_matrix_get(dB, j, 1), deriv[i][j], GSL_DBL_EPSILON,
                         "b-spline k=%d basis #%d derivative %d at x = %f",
                         gsl_bspline_order(bw), j, 1, xloc[i]);
          }
        /* check 2nd derivative evaluation */
        for (j = 0; j < gsl_bspline_ncoeffs(bw); ++j)
          {
            gsl_test_abs(gsl_matrix_get(dB, j, 2), deriv2[i][j], GSL_DBL_EPSILON,
                         "b-spline k=%d basis #%d derivative %d at x = %f",
                         gsl_bspline_order(bw), j, 2, xloc[i]);
          }
      }

    gsl_matrix_free(dB);
    gsl_bspline_deriv_free(dbw);
    gsl_bspline_free(bw);
    gsl_vector_free(breakpts);
  }

  /* Check Greville abscissae functionality on a non-uniform k=1 */
  {
    size_t i; /* looping */

    /* Test parameters */
    const size_t k = 1;
    const double bpoint_data[]    = { 0.0, 0.2, 0.5, 0.75, 1.0 };
    const size_t nbreak           = sizeof(bpoint_data)/sizeof(bpoint_data[0]);

    /* Expected results */
    const double abscissae_data[] = { 0.1, 0.35, 0.625, 0.875 };
    const size_t nabscissae       = sizeof(abscissae_data)/sizeof(abscissae_data[0]);

    gsl_vector_const_view bpoints = gsl_vector_const_view_array(bpoint_data, nbreak);
    gsl_bspline_workspace *w = gsl_bspline_alloc(k, nbreak);
    gsl_bspline_knots((const gsl_vector *) &bpoints, w);

    gsl_test_int(nabscissae, gsl_bspline_ncoeffs(w),
        "b-spline k=%d number of abscissae", k);
    for (i = 0; i < nabscissae; ++i)
      {
        gsl_test_abs(gsl_bspline_greville_abscissa(i, w), abscissae_data[i], 2*k*GSL_DBL_EPSILON,
            "b-spline k=%d Greville abscissa #%d at x = %f", k, i, abscissae_data[i]);
      }

    gsl_bspline_free(w);
  }

  /* Check Greville abscissae functionality on a non-uniform k=2 */
  {
    size_t i; /* looping */

    /* Test parameters */
    const size_t k = 2;
    const double bpoint_data[]    = { 0.0, 0.2, 0.5, 0.75, 1.0 };
    const size_t nbreak           = sizeof(bpoint_data)/sizeof(bpoint_data[0]);

    /* Expected results */
    const double abscissae_data[] = { 0.0, 0.2, 0.5, 0.75, 1.0 };
    const size_t nabscissae       = sizeof(abscissae_data)/sizeof(abscissae_data[0]);

    gsl_vector_const_view bpoints = gsl_vector_const_view_array(bpoint_data, nbreak);
    gsl_bspline_workspace *w = gsl_bspline_alloc(k, nbreak);
    gsl_bspline_knots((const gsl_vector *) &bpoints, w);

    gsl_test_int(nabscissae, gsl_bspline_ncoeffs(w),
        "b-spline k=%d number of abscissae", k);
    for (i = 0; i < nabscissae; ++i)
      {
        gsl_test_abs(gsl_bspline_greville_abscissa(i, w), abscissae_data[i], 2*k*GSL_DBL_EPSILON,
            "b-spline k=%d Greville abscissa #%d at x = %f", k, i, abscissae_data[i]);
      }

    gsl_bspline_free(w);
  }

  /* Check Greville abscissae functionality on non-uniform k=3 */
  {
    size_t i; /* looping */

    /* Test parameters */
    const size_t k = 3;
    const double bpoint_data[]    = { 0.0, 0.2, 0.5, 0.75, 1.0 };
    const size_t nbreak           = sizeof(bpoint_data)/sizeof(bpoint_data[0]);

    /* Expected results */
    const double abscissae_data[] = {      0.0, 1.0/10.0, 7.0/20.0,
                                      5.0/ 8.0, 7.0/ 8.0,      1.0 };
    const size_t nabscissae       = sizeof(abscissae_data)/sizeof(abscissae_data[0]);

    gsl_vector_const_view bpoints = gsl_vector_const_view_array(bpoint_data, nbreak);
    gsl_bspline_workspace *w = gsl_bspline_alloc(k, nbreak);
    gsl_bspline_knots((const gsl_vector *) &bpoints, w);

    gsl_test_int(nabscissae, gsl_bspline_ncoeffs(w),
        "b-spline k=%d number of abscissae", k);
    for (i = 0; i < nabscissae; ++i)
      {
        gsl_test_abs(gsl_bspline_greville_abscissa(i, w), abscissae_data[i], 2*k*GSL_DBL_EPSILON,
            "b-spline k=%d Greville abscissa #%d at x = %f", k, i, abscissae_data[i]);
      }

    gsl_bspline_free(w);
  }

  /* Check Greville abscissae functionality on non-uniform k=4 */
  {
    size_t i; /* looping */

    /* Test parameters */
    const size_t k = 4;
    const double bpoint_data[]    = { 0.0, 0.2, 0.5, 0.75, 1.0 };
    const size_t nbreak           = sizeof(bpoint_data)/sizeof(bpoint_data[0]);

    /* Expected results */
    const double abscissae_data[] = { 0.0,  1.0/15.0,  7.0/30.0,  29.0/60.0,
                                            3.0/ 4.0, 11.0/12.0,        1.0 };
    const size_t nabscissae       = sizeof(abscissae_data)/sizeof(abscissae_data[0]);

    gsl_vector_const_view bpoints = gsl_vector_const_view_array(bpoint_data, nbreak);
    gsl_bspline_workspace *w = gsl_bspline_alloc(k, nbreak);
    gsl_bspline_knots((const gsl_vector *) &bpoints, w);

    gsl_test_int(nabscissae, gsl_bspline_ncoeffs(w),
        "b-spline k=%d number of abscissae", k);
    for (i = 0; i < nabscissae; ++i)
      {
        gsl_test_abs(gsl_bspline_greville_abscissa(i, w), abscissae_data[i], 2*k*GSL_DBL_EPSILON,
            "b-spline k=%d Greville abscissa #%d at x = %f", k, i, abscissae_data[i]);
      }

    gsl_bspline_free(w);
  }

  /* Knots computed from prescribed Greville abscissae for k = 4 */
  {
    size_t i; /* looping */

    /* Test parameters */
    const size_t k = 4;
    const double abscissae_data[] = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0 };
    const size_t nabscissae       = sizeof(abscissae_data)/sizeof(abscissae_data[0]);

    /* Expected results */
    const double bpoint_data[]    = { 1.0, 4.0, 4.0, 4.0, 7.0 };
    const size_t nbreak           = sizeof(bpoint_data)/sizeof(bpoint_data[0]);

    /* Compute knots from Greville abscissae */
    double abserr;
    gsl_vector_const_view abscissae
        = gsl_vector_const_view_array(abscissae_data, nabscissae);
    gsl_bspline_workspace *w = gsl_bspline_alloc(k, nbreak);
    gsl_bspline_knots_greville(&abscissae.vector, w, &abserr);

    for (i = 0; i < nbreak; ++i)
      {
        gsl_test_abs(gsl_bspline_breakpoint(i,w), bpoint_data[i], GSL_DBL_EPSILON*50,
            "b-spline k=%d knots_greville breakpoint #%d", k, i);
      }
    gsl_test_abs(abserr, 0.0, GSL_DBL_EPSILON*15,
        "b-spline k=%d nbreak=%d knots_greville abserr", k, nbreak);

    gsl_bspline_free(w);
  }

  /* Knots computed from prescribed Greville abscissae for k = 8 */
  {
    size_t i; /* looping */

    /* Test parameters */
    const size_t k = 8;

    const double abscissae_data[] = { 1.0, 10.0/7, 13.0/7, 16.0/7, 22.0/7,
                                      4.0, 34.0/7, 40.0/7, 43.0/7, 46.0/7, 7.0 };
    const size_t nabscissae       = sizeof(abscissae_data)/sizeof(abscissae_data[0]);

    /* Expected results */
    const double bpoint_data[]    = { 1.0, 4.0, 4.0, 4.0, 7.0 };
    const size_t nbreak           = sizeof(bpoint_data)/sizeof(bpoint_data[0]);

    /* Compute knots from Greville abscissae */
    double abserr;
    gsl_vector_const_view abscissae
        = gsl_vector_const_view_array(abscissae_data, nabscissae);
    gsl_bspline_workspace *w = gsl_bspline_alloc(k, nbreak);
    gsl_bspline_knots_greville(&abscissae.vector, w, &abserr);

    for (i = 0; i < nbreak; ++i)
      {
        gsl_test_abs(gsl_bspline_breakpoint(i,w), bpoint_data[i], GSL_DBL_EPSILON*50,
            "b-spline k=%d knots_greville breakpoint #%d", k, i);
      }
    gsl_test_abs(abserr, 0.0, GSL_DBL_EPSILON*15,
        "b-spline k=%d nbreak=%d knots_greville abserr", k, nbreak);

    gsl_bspline_free(w);
  }

  /* Knots computed from prescribed Greville abscissae for k = 2 */
  /* Not an interesting calculation but checks the k = 2 edge case */
  {
    size_t i; /* looping */

    /* Test parameters */
    const size_t k = 2;
    const double abscissae_data[] = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0 };
    const size_t nabscissae       = sizeof(abscissae_data)/sizeof(abscissae_data[0]);

    /* Expected results */
    const double bpoint_data[]    = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0 };
    const size_t nbreak           = sizeof(bpoint_data)/sizeof(bpoint_data[0]);

    /* Compute knots from Greville abscissae */
    double abserr;
    gsl_vector_const_view abscissae
        = gsl_vector_const_view_array(abscissae_data, nabscissae);
    gsl_bspline_workspace *w = gsl_bspline_alloc(k, nbreak);
    gsl_bspline_knots_greville(&abscissae.vector, w, &abserr);

    for (i = 0; i < nbreak; ++i)
      {
        gsl_test_abs(gsl_bspline_breakpoint(i,w), bpoint_data[i], GSL_DBL_EPSILON,
            "b-spline k=%d knots_greville breakpoint #%d", k, i);
      }
    gsl_test_abs(abserr, 0.0, GSL_DBL_EPSILON,
        "b-spline k=%d nbreak=%d knots_greville abserr", k, nbreak);

    gsl_bspline_free(w);
  }

  /* Knots computed from prescribed abscissae for edge case when nbreak = 2 */
  {
    size_t i; /* looping */

    /* Test parameters */
    const size_t k = 4;
    double abscissae_data[] = { 1.0, 3.0, 5.0, 7.0 };
    const size_t nabscissae = sizeof(abscissae_data)/sizeof(abscissae_data[0]);

    /* Expected results */
    const double bpoint_data[] = { 1.0, 7.0 };
    const size_t nbreak        = sizeof(bpoint_data)/sizeof(bpoint_data[0]);

    /* Compute knots from Greville abscissae where abscissae are recoverable */
    double abserr;
    gsl_vector_view abscissae
        = gsl_vector_view_array(abscissae_data, nabscissae);
    gsl_bspline_workspace *w = gsl_bspline_alloc(k, nbreak);
    gsl_bspline_knots_greville(&abscissae.vector, w, &abserr);

    /* Check recovery of breakpoints and abscissae */
    for (i = 0; i < nbreak; ++i)
      {
        gsl_test_abs(gsl_bspline_breakpoint(i,w), bpoint_data[i], GSL_DBL_EPSILON,
            "b-spline k=%d knots_greville breakpoint #%d", k, i);
      }
    gsl_test_abs(abserr, 0.0, GSL_DBL_EPSILON,
        "b-spline k=%d nbreak=%d knots_greville abserr", k, nbreak);

    /* Modify interior abscissae so they cannot be recovered with nbreak = 2 */
    /* Then recompute breakpoints and check that abserr is as expected */
    abscissae_data[1] -= 1;
    abscissae_data[2] += 1;
    gsl_bspline_knots_greville(&abscissae.vector, w, &abserr);
    for (i = 0; i < nbreak; ++i)
      {
        gsl_test_abs(gsl_bspline_breakpoint(i,w), bpoint_data[i], GSL_DBL_EPSILON,
            "b-spline k=%d knots_greville breakpoint #%d", k, i);
      }
    gsl_test_abs(abserr, /* deliberate error */ 2.0, GSL_DBL_EPSILON,
        "b-spline k=%d nbreak=%d knots_greville abserr large", k, nbreak);

    gsl_bspline_free(w);
  }

  exit(gsl_test_summary());
}
