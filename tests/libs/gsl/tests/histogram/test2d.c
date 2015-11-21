/* histogram/test2d.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 Brian Gough
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
#include <stdlib.h>
#include <stdio.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_machine.h>
#include <gsl/gsl_histogram2d.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>

#define M 107
#define N 239
#define M1 17
#define N1 23
#define MR 10
#define NR 5

void
test2d (void)
{
  double xr[MR + 1] =
    { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };

  double yr[NR + 1] = { 90.0, 91.0, 92.0, 93.0, 94.0, 95.0 };

  gsl_histogram2d *h, *h1, *g, *hr;
  size_t i, j, k;

  gsl_ieee_env_setup ();

  h = gsl_histogram2d_calloc (M, N);
  h1 = gsl_histogram2d_calloc (M, N);
  g = gsl_histogram2d_calloc (M, N);

  gsl_test (h->xrange == 0,
            "gsl_histogram2d_calloc returns valid xrange pointer");
  gsl_test (h->yrange == 0,
            "gsl_histogram2d_calloc returns valid yrange pointer");
  gsl_test (h->bin == 0, "gsl_histogram2d_calloc returns valid bin pointer");

  gsl_test (h->nx != M, "gsl_histogram2d_calloc returns valid nx");
  gsl_test (h->ny != N, "gsl_histogram2d_calloc returns valid ny");

  hr = gsl_histogram2d_calloc_range (MR, NR, xr, yr);

  gsl_test (hr->xrange == 0,
            "gsl_histogram2d_calloc_range returns valid xrange pointer");
  gsl_test (hr->yrange == 0,
            "gsl_histogram2d_calloc_range returns valid yrange pointer");
  gsl_test (hr->bin == 0,
            "gsl_histogram2d_calloc_range returns valid bin pointer");

  gsl_test (hr->nx != MR, "gsl_histogram2d_calloc_range returns valid nx");
  gsl_test (hr->ny != NR, "gsl_histogram2d_calloc_range returns valid ny");

  {
    int status = 0;
    for (i = 0; i <= MR; i++)
      {
        if (hr->xrange[i] != xr[i])
          {
            status = 1;
          }
      };

    gsl_test (status,
              "gsl_histogram2d_calloc_range creates xrange");
  }

  {
    int status = 0;
    for (i = 0; i <= NR; i++)
      {
        if (hr->yrange[i] != yr[i])
          {
            status = 1;
          }
      };

    gsl_test (status,
              "gsl_histogram2d_calloc_range creates yrange");
  }

  for (i = 0; i <= MR; i++)
    {
      hr->xrange[i] = 0.0;
    }

  for (i = 0; i <= NR; i++)
    {
      hr->yrange[i] = 0.0;
    }

  {
    int status = gsl_histogram2d_set_ranges (hr, xr, MR + 1, yr, NR + 1);

    for (i = 0; i <= MR; i++)
      {
        if (hr->xrange[i] != xr[i])
          {
            status = 1;
          }
      };

    gsl_test (status, "gsl_histogram2d_set_ranges sets xrange");
  }

  {
    int status = 0;
    for (i = 0; i <= NR; i++)
      {
        if (hr->yrange[i] != yr[i])
          {
            status = 1;
          }
      };

    gsl_test (status, "gsl_histogram2d_set_ranges sets yrange");
  }


  k = 0;
  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
          k++;
          gsl_histogram2d_accumulate (h, (double) i, (double) j, (double) k);
        };
    }

  {
    int status = 0;
    k = 0;
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            k++;
            if (h->bin[i * N + j] != (double) k)
              {
                status = 1;
              }
          }
      }
    gsl_test (status,
              "gsl_histogram2d_accumulate writes into array");
  }

  {
    int status = 0;
    k = 0;
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            k++;
            if (gsl_histogram2d_get (h, i, j) != (double) k)
              status = 1;
          };
      }
    gsl_test (status, "gsl_histogram2d_get reads from array");
  }

  for (i = 0; i <= M; i++)
    {
      h1->xrange[i] = 100.0 + i;
    }

  for (i = 0; i <= N; i++)
    {
      h1->yrange[i] = 900.0 + i * i;
    }

  gsl_histogram2d_memcpy (h1, h);

  {
    int status = 0;
    for (i = 0; i <= M; i++)
      {
        if (h1->xrange[i] != h->xrange[i])
          status = 1;
      };
    gsl_test (status, "gsl_histogram2d_memcpy copies bin xranges");
  }

  {
    int status = 0;
    for (i = 0; i <= N; i++)
      {
        if (h1->yrange[i] != h->yrange[i])
          status = 1;
      };
    gsl_test (status, "gsl_histogram2d_memcpy copies bin yranges");
  }

  {
    int status = 0;
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            if (gsl_histogram2d_get (h1, i, j) !=
                gsl_histogram2d_get (h, i, j))
              status = 1;
          }
      }
    gsl_test (status, "gsl_histogram2d_memcpy copies bin values");
  }

  gsl_histogram2d_free (h1);

  h1 = gsl_histogram2d_clone (h);

  {
    int status = 0;
    for (i = 0; i <= M; i++)
      {
        if (h1->xrange[i] != h->xrange[i])
          status = 1;
      };
    gsl_test (status, "gsl_histogram2d_clone copies bin xranges");
  }

  {
    int status = 0;
    for (i = 0; i <= N; i++)
      {
        if (h1->yrange[i] != h->yrange[i])
          status = 1;
      };
    gsl_test (status, "gsl_histogram2d_clone copies bin yranges");
  }

  {
    int status = 0;
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            if (gsl_histogram2d_get (h1, i, j) !=
                gsl_histogram2d_get (h, i, j))
              status = 1;
          }
      }
    gsl_test (status, "gsl_histogram2d_clone copies bin values");
  }


  gsl_histogram2d_reset (h);

  {
    int status = 0;

    for (i = 0; i < M * N; i++)
      {
        if (h->bin[i] != 0)
          status = 1;
      }
    gsl_test (status, "gsl_histogram2d_reset zeros array");
  }

  gsl_histogram2d_free (h);
  h = gsl_histogram2d_calloc (M1, N1);

  {

    int status = 0;

    for (i = 0; i < M1; i++)
      {
        for (j = 0; j < N1; j++)
          {
            gsl_histogram2d_increment (h, (double) i, (double) j);

            for (k = 0; k <= i * N1 + j; k++)
              {
                if (h->bin[k] != 1)
                  {
                    status = 1;
                  }
              }

            for (k = i * N1 + j + 1; k < M1 * N1; k++)
              {
                if (h->bin[k] != 0)
                  {
                    status = 1;
                  }
              }
          }
      }
    gsl_test (status, "gsl_histogram2d_increment increases bin value");
  }

  gsl_histogram2d_free (h);
  h = gsl_histogram2d_calloc (M, N);

  {
    int status = 0;
    for (i = 0; i < M; i++)
      {
        double x0 = 0, x1 = 0;
        gsl_histogram2d_get_xrange (h, i, &x0, &x1);

        if (x0 != i || x1 != i + 1)
          {
            status = 1;
          }
      }
    gsl_test (status,
              "gsl_histogram2d_get_xlowerlimit and xupperlimit");
  }


  {
    int status = 0;
    for (i = 0; i < N; i++)
      {
        double y0 = 0, y1 = 0;
        gsl_histogram2d_get_yrange (h, i, &y0, &y1);

        if (y0 != i || y1 != i + 1)
          {
            status = 1;
          }
      }
    gsl_test (status,
              "gsl_histogram2d_get_ylowerlimit and yupperlimit");
  }


  {
    int status = 0;
    if (gsl_histogram2d_xmax (h) != M)
      status = 1;
    gsl_test (status, "gsl_histogram2d_xmax");
  }

  {
    int status = 0;
    if (gsl_histogram2d_xmin (h) != 0)
      status = 1;
    gsl_test (status, "gsl_histogram2d_xmin");
  }

  {
    int status = 0;
    if (gsl_histogram2d_nx (h) != M)
      status = 1;
    gsl_test (status, "gsl_histogram2d_nx");
  }

  {
    int status = 0;
    if (gsl_histogram2d_ymax (h) != N)
      status = 1;
    gsl_test (status, "gsl_histogram2d_ymax");
  }

  {
    int status = 0;
    if (gsl_histogram2d_ymin (h) != 0)
      status = 1;
    gsl_test (status, "gsl_histogram2d_ymin");
  }

  {
    int status = 0;
    if (gsl_histogram2d_ny (h) != N)
      status = 1;
    gsl_test (status, "gsl_histogram2d_ny");
  }

  h->bin[3 * N + 2] = 123456.0;
  h->bin[4 * N + 3] = -654321;

  {
    double max = gsl_histogram2d_max_val (h);
    gsl_test (max != 123456.0, "gsl_histogram2d_max_val finds maximum value");
  }

  {
    double min = gsl_histogram2d_min_val (h);
    gsl_test (min != -654321.0,
              "gsl_histogram2d_min_val finds minimum value");
  }

  {
    size_t imax, jmax;
    gsl_histogram2d_max_bin (h, &imax, &jmax);
    gsl_test (imax != 3
              || jmax != 2,
              "gsl_histogram2d_max_bin finds maximum value bin");
  }

  {
    size_t imin, jmin;
    gsl_histogram2d_min_bin (h, &imin, &jmin);
    gsl_test (imin != 4
              || jmin != 3, "gsl_histogram2d_min_bin find minimum value bin");
  }

  for (i = 0; i < M * N; i++)
    {
      h->bin[i] = i + 27;
      g->bin[i] = (i + 27) * (i + 1);
    }

  {
    double sum = gsl_histogram2d_sum (h);
    gsl_test (sum != N * M * 27 + ((N * M - 1) * N * M) / 2,
              "gsl_histogram2d_sum sums all bin values");
  }

  {
    /* first test... */
    const double xpos = 0.6;
    const double ypos = 0.85;
    double xmean;
    double ymean;
    size_t xbin;
    size_t ybin;
    gsl_histogram2d *h3 = gsl_histogram2d_alloc (M, N);
    gsl_histogram2d_set_ranges_uniform (h3, 0, 1, 0, 1);
    gsl_histogram2d_increment (h3, xpos, ypos);
    gsl_histogram2d_find (h3, xpos, ypos, &xbin, &ybin);
    xmean = gsl_histogram2d_xmean (h3);
    ymean = gsl_histogram2d_ymean (h3);

    {
      double expected_xmean = (h3->xrange[xbin] + h3->xrange[xbin + 1]) / 2.0;
      double expected_ymean = (h3->yrange[ybin] + h3->yrange[ybin + 1]) / 2.0;
      gsl_test_abs (xmean, expected_xmean, 100.0 * GSL_DBL_EPSILON,
                    "gsl_histogram2d_xmean");
      gsl_test_abs (ymean, expected_ymean, 100.0 * GSL_DBL_EPSILON,
                    "gsl_histogram2d_ymean");
    };
    gsl_histogram2d_free (h3);
  }

  {
    /* test it with bivariate normal distribution */
    const double xmean = 0.7;
    const double ymean = 0.7;
    const double xsigma = 0.1;
    const double ysigma = 0.1;
    const double correl = 0.5;
    const double norm =
      10.0 / M_PI / xsigma / ysigma / sqrt (1.0 - correl * correl);
    size_t xbin;
    size_t ybin;
    gsl_histogram2d *h3 = gsl_histogram2d_alloc (M, N);
    gsl_histogram2d_set_ranges_uniform (h3, 0, 1, 0, 1);
    /* initialize with 2d gauss pdf in two directions */
    for (xbin = 0; xbin < M; xbin++)
      {
        double xi =
          ((h3->xrange[xbin] + h3->xrange[xbin + 1]) / 2.0 - xmean) / xsigma;
        for (ybin = 0; ybin < N; ybin++)
          {
            double yi =
              ((h3->yrange[ybin] + h3->yrange[ybin + 1]) / 2.0 -
               ymean) / ysigma;
            double prob =
              norm * exp (-(xi * xi - 2.0 * correl * xi * yi + yi * yi) /
                          2.0 / (1 - correl * correl));
            h3->bin[xbin * N + ybin] = prob;
          }
      }
    {
      double xs = gsl_histogram2d_xsigma (h3);
      double ys = gsl_histogram2d_ysigma (h3);
      /* evaluate results and compare with parameters */

      gsl_test_abs (gsl_histogram2d_xmean (h3), xmean, 2.0/M,
                    "gsl_histogram2d_xmean histogram mean(x)");
      gsl_test_abs (gsl_histogram2d_ymean (h3), ymean, 2.0/N,
                    "gsl_histogram2d_ymean histogram mean(y)");
      gsl_test_abs (xs, xsigma, 2.0/M,
                    "gsl_histogram2d_xsigma histogram stdev(x)");
      gsl_test_abs (ys, ysigma, 2.0/N,
                    "gsl_histogram2d_ysigma histogram stdev(y)");
      gsl_test_abs (gsl_histogram2d_cov (h3) / xs / ys, correl,
                    2.0/((M < N) ? M : N),
                    "gsl_histogram2d_cov histogram covariance");
    }
    gsl_histogram2d_free (h3);
  }

  gsl_histogram2d_memcpy (h1, g);
  gsl_histogram2d_add (h1, h);

  {
    int status = 0;
    for (i = 0; i < M * N; i++)
      {
        if (h1->bin[i] != g->bin[i] + h->bin[i])
          status = 1;
      }
    gsl_test (status, "gsl_histogram2d_add histogram addition");
  }

  gsl_histogram2d_memcpy (h1, g);
  gsl_histogram2d_sub (h1, h);

  {
    int status = 0;
    for (i = 0; i < M * N; i++)
      {
        if (h1->bin[i] != g->bin[i] - h->bin[i])
          status = 1;
      }
    gsl_test (status, "gsl_histogram2d_sub histogram subtraction");
  }


  gsl_histogram2d_memcpy (h1, g);
  gsl_histogram2d_mul (h1, h);

  {
    int status = 0;
    for (i = 0; i < M * N; i++)
      {
        if (h1->bin[i] != g->bin[i] * h->bin[i])
          status = 1;
      }
    gsl_test (status, "gsl_histogram2d_mul histogram multiplication");
  }

  gsl_histogram2d_memcpy (h1, g);
  gsl_histogram2d_div (h1, h);

  {
    int status = 0;
    for (i = 0; i < M * N; i++)
      {
        if (h1->bin[i] != g->bin[i] / h->bin[i])
          status = 1;
      }
    gsl_test (status, "gsl_histogram2d_div histogram division");
  }

  gsl_histogram2d_memcpy (h1, g);
  gsl_histogram2d_scale (h1, 0.5);

  {
    int status = 0;
    for (i = 0; i < M * N; i++)
      {
        if (h1->bin[i] != 0.5 * g->bin[i])
          status = 1;
      }
    gsl_test (status, "gsl_histogram2d_scale histogram scaling");
  }

  gsl_histogram2d_memcpy (h1, g);
  gsl_histogram2d_shift (h1, 0.25);

  {
    int status = 0;
    for (i = 0; i < M * N; i++)
      {
        if (h1->bin[i] != 0.25 + g->bin[i])
          status = 1;
      }
    gsl_test (status, "gsl_histogram2d_shift histogram shift");
  }

  gsl_histogram2d_free (h);     /* free whatever is in h */

  h = gsl_histogram2d_calloc_uniform (M1, N1, 0.0, 5.0, 0.0, 5.0);

  gsl_test (h->xrange == 0,
            "gsl_histogram2d_calloc_uniform returns valid range pointer");
  gsl_test (h->yrange == 0,
            "gsl_histogram2d_calloc_uniform returns valid range pointer");
  gsl_test (h->bin == 0,
            "gsl_histogram2d_calloc_uniform returns valid bin pointer");
  gsl_test (h->nx != M1, "gsl_histogram2d_calloc_uniform returns valid nx");
  gsl_test (h->ny != N1, "gsl_histogram2d_calloc_uniform returns valid ny");

  gsl_histogram2d_accumulate (h, 0.0, 3.01, 1.0);
  gsl_histogram2d_accumulate (h, 0.1, 2.01, 2.0);
  gsl_histogram2d_accumulate (h, 0.2, 1.01, 3.0);
  gsl_histogram2d_accumulate (h, 0.3, 0.01, 4.0);

  {
    size_t i1, i2, i3, i4;
    size_t j1, j2, j3, j4;
    double expected;
    int status;
    status = gsl_histogram2d_find (h, 0.0, 3.01, &i1, &j1);
    status = gsl_histogram2d_find (h, 0.1, 2.01, &i2, &j2);
    status = gsl_histogram2d_find (h, 0.2, 1.01, &i3, &j3);
    status = gsl_histogram2d_find (h, 0.3, 0.01, &i4, &j4);

    for (i = 0; i < M1; i++)
      {
        for (j = 0; j < N1; j++)
          {
            if (i == i1 && j == j1)
              {
                expected = 1.0;
              }
            else if (i == i2 && j == j2)
              {
                expected = 2.0;
              }
            else if (i == i3 && j == j3)
              {
                expected = 3.0;
              }
            else if (i == i4 && j == j4)
              {
                expected = 4.0;
              }
            else
              {
                expected = 0.0;
              }

            if (h->bin[i * N1 + j] != expected)
              {
                status = 1;
              }
          }
      }
    gsl_test (status, "gsl_histogram2d_find returns index");
  }

  {
    FILE *f = fopen ("test.txt", "w");
    gsl_histogram2d_fprintf (f, h, "%.19e", "%.19e");
    fclose (f);
  }

  {
    FILE *f = fopen ("test.txt", "r");
    gsl_histogram2d *hh = gsl_histogram2d_calloc (M1, N1);
    int status = 0;

    gsl_histogram2d_fscanf (f, hh);

    for (i = 0; i <= M1; i++)
      {
        if (h->xrange[i] != hh->xrange[i])
          {
            printf ("xrange[%d] : %g orig vs %g\n",
                    (int) i, h->xrange[i], hh->xrange[i]);
            status = 1;
          }
      }

    for (j = 0; j <= N1; j++)
      {
        if (h->yrange[j] != hh->yrange[j])
          {
            printf ("yrange[%d] : %g orig vs %g\n",
                    (int) j, h->yrange[j], hh->yrange[j]);
            status = 1;
          }
      }

    for (i = 0; i < M1 * N1; i++)
      {
        if (h->bin[i] != hh->bin[i])
          {
            printf ("bin[%d] : %g orig vs %g\n",
                    (int) i, h->bin[i], hh->bin[i]);
            status = 1;
          }
      }

    gsl_test (status, "gsl_histogram2d_fprintf and fscanf");

    gsl_histogram2d_free (hh);
    fclose (f);
  }

  {
    FILE *f = fopen ("test.dat", "wb");
    gsl_histogram2d_fwrite (f, h);
    fclose (f);
  }

  {
    FILE *f = fopen ("test.dat", "rb");
    gsl_histogram2d *hh = gsl_histogram2d_calloc (M1, N1);
    int status = 0;

    gsl_histogram2d_fread (f, hh);

    for (i = 0; i <= M1; i++)
      {
        if (h->xrange[i] != hh->xrange[i])
          {
            printf ("xrange[%d] : %g orig vs %g\n",
                    (int) i, h->xrange[i], hh->xrange[i]);
            status = 1;
          }
      }

    for (j = 0; j <= N1; j++)
      {
        if (h->yrange[j] != hh->yrange[j])
          {
            printf ("yrange[%d] : %g orig vs %g\n",
                    (int) j, h->yrange[j], hh->yrange[j]);
            status = 1;
          }
      }

    for (i = 0; i < M1 * N1; i++)
      {
        if (h->bin[i] != hh->bin[i])
          {
            printf ("bin[%d] : %g orig vs %g\n",
                    (int) i, h->bin[i], hh->bin[i]);
            status = 1;
          }
      }

    gsl_test (status, "gsl_histogram2d_fwrite and fread");

    gsl_histogram2d_free (hh);
    fclose (f);
  }

  gsl_histogram2d_free (h);
  gsl_histogram2d_free (h1);
  gsl_histogram2d_free (g);
  gsl_histogram2d_free (hr);
}
