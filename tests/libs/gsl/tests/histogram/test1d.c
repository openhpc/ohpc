/* histogram/test.c
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
#include <gsl/gsl_histogram.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>

#define N 397
#define NR 10

void
test1d (void)
{
  double xr[NR + 1] =
  {0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0};

  gsl_histogram *h, *h1, *hr, *g;
  size_t i, j;

  gsl_ieee_env_setup ();

  h = gsl_histogram_calloc (N);
  h1 = gsl_histogram_calloc (N);
  g = gsl_histogram_calloc (N);

  gsl_test (h->range == 0, "gsl_histogram_alloc returns valid range pointer");
  gsl_test (h->bin == 0, "gsl_histogram_alloc returns valid bin pointer");
  gsl_test (h->n != N, "gsl_histogram_alloc returns valid size");


  hr = gsl_histogram_calloc_range (NR, xr);

  gsl_test (hr->range == 0, "gsl_histogram_calloc_range returns valid range pointer");
  gsl_test (hr->bin == 0, "gsl_histogram_calloc_range returns valid bin pointer");
  gsl_test (hr->n != NR, "gsl_histogram_calloc_range returns valid size");

  {
    int status = 0;
    for (i = 0; i <= NR; i++)
      {
        if (hr->range[i] != xr[i])
          {
            status = 1;
          }
      };

    gsl_test (status, "gsl_histogram_calloc_range creates range");
  }

  for (i = 0; i <= NR; i++)
    {
      hr->range[i] = 0.0;
    }

  {
    int status = gsl_histogram_set_ranges (hr, xr, NR+1);

    for (i = 0; i <= NR; i++)
      {
        if (hr->range[i] != xr[i])
          {
            status = 1;
          }
      };

    gsl_test (status, "gsl_histogram_set_range sets range");
  }
    

  for (i = 0; i < N; i++)
    {
      gsl_histogram_accumulate (h, (double) i, (double) i);
    };

  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        if (h->bin[i] != (double) i)
          {
            status = 1;
          }
      };

    gsl_test (status, "gsl_histogram_accumulate writes into array");
  }


  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        if (gsl_histogram_get (h, i) != i)
          status = 1;
      };
    gsl_test (status, "gsl_histogram_get reads from array");
  }

  for (i = 0; i <= N; i++)
    {
      h1->range[i] = 100.0 + i;
    }

  gsl_histogram_memcpy (h1, h);

  {
    int status = 0;
    for (i = 0; i <= N; i++)
      {
        if (h1->range[i] != h->range[i])
          status = 1;
      };
    gsl_test (status, "gsl_histogram_memcpy copies bin ranges");
  }

  {
    int status = 0;
    for (i = 0; i < N; i++)
      {
        if (gsl_histogram_get (h1, i) != gsl_histogram_get (h, i))
          status = 1;
      };
    gsl_test (status, "gsl_histogram_memcpy copies bin values");
  }

  gsl_histogram_free (h1);

  h1 = gsl_histogram_clone (h);

  {
    int status = 0;
    for (i = 0; i <= N; i++)
      {
        if (h1->range[i] != h->range[i])
          status = 1;
      };
    gsl_test (status, "gsl_histogram_clone copies bin ranges");
  }

  {
    int status = 0;
    for (i = 0; i < N; i++)
      {
        if (gsl_histogram_get (h1, i) != gsl_histogram_get (h, i))
          status = 1;
      };
    gsl_test (status, "gsl_histogram_clone copies bin values");
  }

  gsl_histogram_reset (h);

  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        if (h->bin[i] != 0)
          status = 1;
      }
    gsl_test (status, "gsl_histogram_reset zeros array");
  }


  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        gsl_histogram_increment (h, (double) i);

        for (j = 0; j <= i; j++)
          {
            if (h->bin[j] != 1)
              {
                status = 1;
              }
          }

        for (j = i + 1; j < N; j++)
          {
            if (h->bin[j] != 0)
              {
                status = 1;
              }
          }
      }

    gsl_test (status, "gsl_histogram_increment increases bin value");
  }

  {
    int status = 0;
    for (i = 0; i < N; i++)
      {
        double x0 = 0, x1 = 0;

        gsl_histogram_get_range (h, i, &x0, &x1);

        if (x0 != i || x1 != i + 1)
          {
            status = 1;
          }
      }
    gsl_test (status, "gsl_histogram_getbinrange returns bin range");
  }

  {
    int status = 0;
    if (gsl_histogram_max (h) != N)
      status = 1;
    gsl_test (status, "gsl_histogram_max returns maximum");
  }

  {
    int status = 0;
    if (gsl_histogram_min (h) != 0)
      status = 1;
    gsl_test (status, "gsl_histogram_min returns minimum");
  }

  {
    int status = 0;
    if (gsl_histogram_bins (h) != N)
      status = 1;
    gsl_test (status, "gsl_histogram_bins returns number of bins");
  }

  h->bin[2] = 123456.0;
  h->bin[4] = -654321;

  {
    double max = gsl_histogram_max_val (h);
    gsl_test (max != 123456.0, "gsl_histogram_max_val finds maximum value");
  }

  {
    double min = gsl_histogram_min_val (h);
    gsl_test (min != -654321.0, "gsl_histogram_min_val finds minimum value");
  }

  {
    size_t imax = gsl_histogram_max_bin (h);
    gsl_test (imax != 2, "gsl_histogram_max_bin finds maximum value bin");
  }

  {
    size_t imin = gsl_histogram_min_bin (h);
    gsl_test (imin != 4, "gsl_histogram_min_bin find minimum value bin");
  }

  for (i = 0; i < N; i++)
    {
      h->bin[i] = i + 27;
      g->bin[i] = (i + 27) * (i + 1);
    }

  {
    double sum=gsl_histogram_sum (h);
    gsl_test(sum != N*27+((N-1)*N)/2, "gsl_histogram_sum sums all bin values");
  }

  gsl_histogram_memcpy (h1, g);
  gsl_histogram_add (h1, h);

  {
    int status = 0;
    for (i = 0; i < N; i++)
      {
        if (h1->bin[i] != g->bin[i] + h->bin[i])
          status = 1;
      }
    gsl_test (status, "gsl_histogram_add histogram addition");
  }

  gsl_histogram_memcpy (h1, g);
  gsl_histogram_sub (h1, h);

  {
    int status = 0;
    for (i = 0; i < N; i++)
      {
        if (h1->bin[i] != g->bin[i] - h->bin[i])
          status = 1;
      }
    gsl_test (status, "gsl_histogram_sub histogram subtraction");
  }


  gsl_histogram_memcpy (h1, g);
  gsl_histogram_mul (h1, h);

  {
    int status = 0;
    for (i = 0; i < N; i++)
      {
        if (h1->bin[i] != g->bin[i] * h->bin[i])
          status = 1;
      }
    gsl_test (status, "gsl_histogram_mul histogram multiplication");
  }


  gsl_histogram_memcpy (h1, g);
  gsl_histogram_div (h1, h);

  {
    int status = 0;
    for (i = 0; i < N; i++)
      {
        if (h1->bin[i] != g->bin[i] / h->bin[i])
          status = 1;
      }
    gsl_test (status, "gsl_histogram_div histogram division");
  }

  gsl_histogram_memcpy (h1, g);
  gsl_histogram_scale (h1, 0.5);

  {
    int status = 0;
    for (i = 0; i < N; i++)
      {
        if (h1->bin[i] != 0.5 * g->bin[i])
          status = 1;
      }
    gsl_test (status, "gsl_histogram_scale histogram scaling");
  }

  gsl_histogram_memcpy (h1, g);
  gsl_histogram_shift (h1, 0.25);

  {
    int status = 0;
    for (i = 0; i < N; i++)
      {
        if (h1->bin[i] != 0.25 + g->bin[i])
          status = 1;
      }
    gsl_test (status, "gsl_histogram_shift histogram shift");
  }


  gsl_histogram_free (h);       /* free whatever is in h */

  h = gsl_histogram_calloc_uniform (N, 0.0, 1.0);

  gsl_test (h->range == 0,
            "gsl_histogram_calloc_uniform returns valid range pointer");
  gsl_test (h->bin == 0,
            "gsl_histogram_calloc_uniform returns valid bin pointer");
  gsl_test (h->n != N,
            "gsl_histogram_calloc_uniform returns valid size");

  gsl_histogram_accumulate (h, 0.0, 1.0);
  gsl_histogram_accumulate (h, 0.1, 2.0);
  gsl_histogram_accumulate (h, 0.2, 3.0);
  gsl_histogram_accumulate (h, 0.3, 4.0);

  {
    size_t i1, i2, i3, i4;
    double expected;
    int status = gsl_histogram_find (h, 0.0, &i1);
    status = gsl_histogram_find (h, 0.1, &i2);
    status = gsl_histogram_find (h, 0.2, &i3);
    status = gsl_histogram_find (h, 0.3, &i4);

    for (i = 0; i < N; i++)
      {
        if (i == i1)
          {
            expected = 1.0;
          }
        else if (i == i2)
          {
            expected = 2.0;
          }
        else if (i == i3)
          {
            expected = 3.0;
          }
        else if (i == i4)
          {
            expected = 4.0;
          }
        else
          {
            expected = 0.0;
          }

        if (h->bin[i] != expected)
          {
            status = 1;
          }

      }
    gsl_test (status, "gsl_histogram_find returns index");
  }


  {
    FILE *f = fopen ("test.txt", "w");
    gsl_histogram_fprintf (f, h, "%.19e", "%.19e");
    fclose (f);
  }

  {
    FILE *f = fopen ("test.txt", "r");
    gsl_histogram *hh = gsl_histogram_calloc (N);
    int status = 0;

    gsl_histogram_fscanf (f, hh);

    for (i = 0; i < N; i++)
      {
        if (h->range[i] != hh->range[i])
          status = 1;
        if (h->bin[i] != hh->bin[i])
          status = 1;
      }
    if (h->range[N] != hh->range[N])
      status = 1;

    gsl_test (status, "gsl_histogram_fprintf and fscanf");

    gsl_histogram_free (hh);
    fclose (f);
  }

  {
    FILE *f = fopen ("test.dat", "wb");
    gsl_histogram_fwrite (f, h);
    fclose (f);
  }

  {
    FILE *f = fopen ("test.dat", "rb");
    gsl_histogram *hh = gsl_histogram_calloc (N);
    int status = 0;

    gsl_histogram_fread (f, hh);

    for (i = 0; i < N; i++)
      {
        if (h->range[i] != hh->range[i])
          status = 1;
        if (h->bin[i] != hh->bin[i])
          status = 1;
      }
    if (h->range[N] != hh->range[N])
      status = 1;

    gsl_test (status, "gsl_histogram_fwrite and fread");

    gsl_histogram_free (hh);
    fclose (f);
  }

  gsl_histogram_free (h);
  gsl_histogram_free (g);
  gsl_histogram_free (h1);
  gsl_histogram_free (hr);
}
