/* histogram/test_trap.c
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
#include <gsl/gsl_errno.h>
#include <gsl/gsl_ieee_utils.h>

#define N 397

static void my_error_handler (const char *reason, const char *file,
                              int line, int err);
static int status = 0;

void
test1d_trap (void)
{
  gsl_histogram *h;
  double result, lower, upper;
  size_t i;

  gsl_set_error_handler (&my_error_handler);

  gsl_ieee_env_setup ();

  status = 0;
  h = gsl_histogram_calloc (0);
  gsl_test (!status, "gsl_histogram_calloc traps zero-length histogram");
  gsl_test (h != 0,
            "gsl_histogram_calloc returns NULL for zero-length histogram");

  status = 0;
  h = gsl_histogram_calloc_uniform (0, 0.0, 1.0);
  gsl_test (!status,
            "gsl_histogram_calloc_uniform traps zero-length histogram");
  gsl_test (h != 0,
     "gsl_histogram_calloc_uniform returns NULL for zero-length histogram");

  status = 0;
  h = gsl_histogram_calloc_uniform (10, 1.0, 1.0);
  gsl_test (!status,
            "gsl_histogram_calloc_uniform traps equal endpoints");
  gsl_test (h != 0,
            "gsl_histogram_calloc_uniform returns NULL for equal endpoints");

  status = 0;
  h = gsl_histogram_calloc_uniform (10, 2.0, 1.0);
  gsl_test (!status,
            "gsl_histogram_calloc_uniform traps invalid range");
  gsl_test (h != 0,
            "gsl_histogram_calloc_uniform returns NULL for invalid range");

  h = gsl_histogram_calloc_uniform (N, 0.0, 1.0);

  status = gsl_histogram_accumulate (h, 1.0, 10.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram_accumulate traps x at xmax");

  status = gsl_histogram_accumulate (h, 2.0, 100.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram_accumulate traps x above xmax");

  status = gsl_histogram_accumulate (h, -1.0, 1000.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram_accumulate traps x below xmin");

  status = gsl_histogram_increment (h, 1.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram_increment traps x at xmax");

  status = gsl_histogram_increment (h, 2.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram_increment traps x above xmax");

  status = gsl_histogram_increment (h, -1.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram_increment traps x below xmin");


  result = gsl_histogram_get (h, N);
  gsl_test (result != 0, "gsl_histogram_get traps index at n");

  result = gsl_histogram_get (h, N + 1);
  gsl_test (result != 0, "gsl_histogram_get traps index above n");

  status = gsl_histogram_get_range (h, N, &lower, &upper);
  gsl_test (status != GSL_EDOM,
            "gsl_histogram_get_range traps index at n");

  status = gsl_histogram_get_range (h, N + 1, &lower, &upper);
  gsl_test (status != GSL_EDOM,
            "gsl_histogram_get_range traps index above n");


  status = 0;
  gsl_histogram_find (h, -0.01, &i);
  gsl_test (status != GSL_EDOM, "gsl_histogram_find traps x below xmin");

  status = 0;
  gsl_histogram_find (h, 1.0, &i);
  gsl_test (status != GSL_EDOM, "gsl_histogram_find traps x at xmax");

  status = 0;
  gsl_histogram_find (h, 1.1, &i);
  gsl_test (status != GSL_EDOM, "gsl_histogram_find traps x above xmax");

  gsl_histogram_free (h);
}


static void
my_error_handler (const char *reason, const char *file, int line, int err)
{
  if (0)
    printf ("(caught [%s:%d: %s (%d)])\n", file, line, reason, err);
  status = 1;
}
