/* histogram/test2d_trap.c
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
#include <gsl/gsl_histogram2d.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_ieee_utils.h>

#define N 107
#define M 239

static void my_error_handler (const char *reason, const char *file,
                              int line, int err);
static int status = 0;

void
test2d_trap (void)
{
  gsl_histogram2d *h;
  double result, lower, upper;
  size_t i, j;

  gsl_set_error_handler (&my_error_handler);

  gsl_ieee_env_setup ();

  status = 0;
  h = gsl_histogram2d_calloc (0, 10);
  gsl_test (!status, "gsl_histogram_calloc traps zero-width histogram");
  gsl_test (h != 0,
            "gsl_histogram2d_calloc returns NULL for zero-width histogram");


  status = 0;
  h = gsl_histogram2d_calloc (10, 0);
  gsl_test (!status, "gsl_histogram_calloc traps zero-length histogram");
  gsl_test (h != 0,
            "gsl_histogram2d_calloc returns NULL for zero-length histogram");


  status = 0;
  h = gsl_histogram2d_calloc_uniform (0, 10, 0.0, 1.0, 0.0, 1.0);
  gsl_test (!status,
            "gsl_histogram2d_calloc_uniform traps zero-width histogram");
  gsl_test (h != 0,
    "gsl_histogram2d_calloc_uniform returns NULL for zero-width histogram");

  status = 0;
  h = gsl_histogram2d_calloc_uniform (10, 0, 0.0, 1.0, 0.0, 1.0);
  gsl_test (!status,
            "gsl_histogram2d_calloc_uniform traps zero-length histogram");
  gsl_test (h != 0,
   "gsl_histogram2d_calloc_uniform returns NULL for zero-length histogram");

  status = 0;
  h = gsl_histogram2d_calloc_uniform (10, 10, 0.0, 1.0, 1.0, 1.0);
  gsl_test (!status,
            "gsl_histogram2d_calloc_uniform traps equal endpoints");
  gsl_test (h != 0,
         "gsl_histogram2d_calloc_uniform returns NULL for equal endpoints");

  status = 0;
  h = gsl_histogram2d_calloc_uniform (10, 10, 1.0, 1.0, 0.0, 1.0);
  gsl_test (!status,
            "gsl_histogram2d_calloc_uniform traps equal endpoints");
  gsl_test (h != 0,
         "gsl_histogram2d_calloc_uniform returns NULL for equal endpoints");

  status = 0;
  h = gsl_histogram2d_calloc_uniform (10, 10, 0.0, 1.0, 2.0, 1.0);
  gsl_test (!status,
            "gsl_histogram2d_calloc_uniform traps invalid range");
  gsl_test (h != 0,
            "gsl_histogram2d_calloc_uniform returns NULL for invalid range");

  status = 0;
  h = gsl_histogram2d_calloc_uniform (10, 10, 2.0, 1.0, 0.0, 1.0);
  gsl_test (!status,
            "gsl_histogram2d_calloc_uniform traps invalid range");
  gsl_test (h != 0,
            "gsl_histogram2d_calloc_uniform returns NULL for invalid range");

  h = gsl_histogram2d_calloc_uniform (N, M, 0.0, 1.0, 0.0, 1.0);

  status = gsl_histogram2d_accumulate (h, 1.0, 0.0, 10.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_accumulate traps x at xmax");

  status = gsl_histogram2d_accumulate (h, 2.0, 0.0, 100.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_accumulate traps x above xmax");

  status = gsl_histogram2d_accumulate (h, -1.0, 0.0, 1000.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_accumulate traps x below xmin");

  status = gsl_histogram2d_accumulate (h, 0.0, 1.0, 10.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_accumulate traps y at ymax");

  status = gsl_histogram2d_accumulate (h, 0.0, 2.0, 100.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_accumulate traps y above ymax");

  status = gsl_histogram2d_accumulate (h, 0.0, -1.0, 1000.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_accumulate traps y below ymin");


  status = gsl_histogram2d_increment (h, 1.0, 0.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_increment traps x at xmax");

  status = gsl_histogram2d_increment (h, 2.0, 0.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_increment traps x above xmax");

  status = gsl_histogram2d_increment (h, -1.0, 0.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_increment traps x below xmin");


  status = gsl_histogram2d_increment (h, 0.0, 1.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_increment traps y at ymax");

  status = gsl_histogram2d_increment (h, 0.0, 2.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_increment traps y above ymax");

  status = gsl_histogram2d_increment (h, 0.0, -1.0);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_increment traps y below ymin");


  result = gsl_histogram2d_get (h, N, 0);
  gsl_test (result != 0, "gsl_histogram2d_get traps x index at nx");

  result = gsl_histogram2d_get (h, N + 1, 0);
  gsl_test (result != 0, "gsl_histogram2d_get traps x index above nx");

  result = gsl_histogram2d_get (h, 0, M);
  gsl_test (result != 0, "gsl_histogram2d_get traps y index at ny");

  result = gsl_histogram2d_get (h, 0, M + 1);
  gsl_test (result != 0, "gsl_histogram2d_get traps y index above ny");


  status = gsl_histogram2d_get_xrange (h, N, &lower, &upper);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_get_xrange traps index at nx");

  status = gsl_histogram2d_get_xrange (h, N + 1, &lower, &upper);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_get_xrange traps index above nx");

  status = gsl_histogram2d_get_yrange (h, M, &lower, &upper);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_get_yrange traps index at ny");

  status = gsl_histogram2d_get_yrange (h, M + 1, &lower, &upper);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_get_yrange traps index above ny");

  status = 0;
  gsl_histogram2d_find (h, -0.01, 0.0, &i, &j);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_find traps x below xmin");

  status = 0;
  gsl_histogram2d_find (h, 1.0, 0.0, &i, &j);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_find traps x at xmax");

  status = 0;
  gsl_histogram2d_find (h, 1.1, 0.0, &i, &j);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_find traps x above xmax");


  status = 0;
  gsl_histogram2d_find (h, 0.0, -0.01, &i, &j);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_find traps y below ymin");

  status = 0;
  gsl_histogram2d_find (h, 0.0, 1.0, &i, &j);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_find traps y at ymax");

  status = 0;
  gsl_histogram2d_find (h, 0.0, 1.1, &i, &j);
  gsl_test (status != GSL_EDOM, "gsl_histogram2d_find traps y above ymax");

  gsl_histogram2d_free (h);
}


static void
my_error_handler (const char *reason, const char *file, int line, int err)
{
  if (0)
    printf ("(caught [%s:%d: %s (%d)])\n", file, line, reason, err);
  status = 1;
}
