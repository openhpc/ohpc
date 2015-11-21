/* combination/test.c
 * based on permutation/test.c by Brian Gough
 * 
 * Copyright (C) 2001 Szymon Jaroszewicz
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
#include <math.h>
#include <gsl/gsl_combination.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>

size_t c63[20][3] = {
  { 0, 1, 2 },  { 0, 1, 3 },  { 0, 1, 4 },  { 0, 1, 5 },
  { 0, 2, 3 },  { 0, 2, 4 },  { 0, 2, 5 },  { 0, 3, 4 },
  { 0, 3, 5 },  { 0, 4, 5 },  { 1, 2, 3 },  { 1, 2, 4 },
  { 1, 2, 5 },  { 1, 3, 4 },  { 1, 3, 5 },  { 1, 4, 5 },
  { 2, 3, 4 },  { 2, 3, 5 },  { 2, 4, 5 },  { 3, 4, 5 }
} ;

void my_error_handler (const char *reason, const char *file, int line, int err);


int 
main (void)
{
  size_t i, j;
  int status = 0, s;
  gsl_combination * c ;

  gsl_ieee_env_setup ();

  c = gsl_combination_alloc (6,3);

  /* Test combinations in forward order */

  gsl_combination_init_first (c);
  
  i = 0;

  do 
    {
      if ( i >= 20 )
        {
          status = 1;
          break;
        }
      for (j = 0; j < 3; j++)
        {
          status |= (c->data[j] != c63[i][j]);
        }

      {
        int s1 = gsl_combination_valid (c);
        gsl_test (s1, "gsl_combination_valid (%u)", i);
      }

      i++;
    }
  while (gsl_combination_next(c) == GSL_SUCCESS);

  gsl_test(status, "gsl_combination_next, 6 choose 3 combination, 20 steps");

  gsl_combination_next(c);
  gsl_combination_next(c);
  gsl_combination_next(c);
  for (j = 0; j < 3; j++)
    {
      status |= (c->data[j] != c63[19][j]);
    }
  gsl_test(status, "gsl_combination_next on the last combination");

  {
    int s1 = gsl_combination_valid (c);
    gsl_test (s1, "gsl_combination_valid on the last combination");
  }

  {
    gsl_combination * d = gsl_combination_alloc (6,3);
    gsl_combination_memcpy (d, c);

    status = 0;

    for (j = 0; j < 3; j++)
      {
        status |= (d->data[j] != c->data[j]);
      }

    gsl_test (status, "gsl_combination_memcpy, 6 choose 3 combination");
    gsl_combination_free(d);
  }


  /* Now test combinations in reverse order */

  gsl_combination_init_last (c);

  i = 20;
  do 
    {
      if ( i == 0 )
        {
          status = 1;
          break;
        }

      i--;

      for (j = 0; j < 3; j++)
        {
          status |= (c->data[j] != c63[i][j]);
        }

      {
        int s1 = gsl_combination_valid (c);
        gsl_test (s1, "gsl_combination_valid (%u)", i);
      }
    }
  while (gsl_combination_prev(c) == GSL_SUCCESS);

  gsl_test(status, "gsl_combination_prev, 6 choose 3 combination, 20 steps");

  gsl_combination_prev(c);
  gsl_combination_prev(c);
  gsl_combination_prev(c);
  for (j = 0; j < 3; j++)
    {
      status |= (c->data[j] != c63[0][j]);
    }
  gsl_test(status, "gsl_combination_prev on the first combination");

  {
    int s1 = gsl_combination_valid (c);
    gsl_test (s1, "gsl_combination_valid on the first combination");
  }

  {
    gsl_combination * d = gsl_combination_alloc (6,3);
    gsl_combination_memcpy (d, c);

    status = 0;

    for (j = 0; j < 3; j++)
      {
        status |= (d->data[j] != c->data[j]);
      }

    gsl_test (status, "gsl_combination_memcpy, 6 choose 3 combination");
    gsl_combination_free(d);
  }

  gsl_combination_free (c);

  c = gsl_combination_calloc(7, 0);
  /* should return GSL_FAILURE every time */
  status |= (gsl_combination_next(c) != GSL_FAILURE);
  status |= (gsl_combination_next(c) != GSL_FAILURE);
  status |= (gsl_combination_prev(c) != GSL_FAILURE);
  status |= (gsl_combination_prev(c) != GSL_FAILURE);
  gsl_test(status, "gsl_combination 7 choose 0");
  gsl_combination_free (c);

  c = gsl_combination_calloc(7, 7);
  /* should return GSL_FAILURE every time */
  for(j = 0; j < 7; j++)
  {
    status |= (gsl_combination_get(c, j) != j);
  }
  status |= (gsl_combination_next(c) != GSL_FAILURE);
  for(j = 0; j < 7; j++)
  {
    status |= (gsl_combination_get(c, j) != j);
  }
  status |= (gsl_combination_next(c) != GSL_FAILURE);
  for(j = 0; j < 7; j++)
  {
    status |= (gsl_combination_get(c, j) != j);
  }
  status |= (gsl_combination_prev(c) != GSL_FAILURE);
  for(j = 0; j < 7; j++)
  {
    status |= (gsl_combination_get(c, j) != j);
  }
  status |= (gsl_combination_prev(c) != GSL_FAILURE);
  for(j = 0; j < 7; j++)
  {
    status |= (gsl_combination_get(c, j) != j);
  }
  gsl_test(status, "gsl_combination 7 choose 7");
  gsl_combination_free (c);

  c = gsl_combination_calloc(6, 3);

  gsl_set_error_handler (&my_error_handler);

  c->data[0] = 1;
  c->data[1] = 1;
  c->data[2] = 2;
  s = gsl_combination_valid (c);
  gsl_test (!s, "gsl_combination_valid on an invalid combination (1,1,2)");

  c->data[0] = 2;
  c->data[1] = 1;
  c->data[2] = 0;
  s = gsl_combination_valid (c);
  gsl_test (!s, "gsl_combination_valid on an invalid combination (2,1,0)");

  c->data[0] = 1;
  c->data[1] = 2;
  c->data[2] = 0;
  s = gsl_combination_valid (c);
  gsl_test (!s, "gsl_combination_valid on an invalid combination (1,2,0)");

  {
    gsl_combination * d = gsl_combination_alloc (6,4);
    int s = gsl_combination_memcpy (d, c);
    gsl_test (!s, "gsl_combination_memcpy, (6,4) vs (6,3)");
    gsl_combination_free(d);
  }

  {
    gsl_combination * d = gsl_combination_alloc (7,3);
    int s = gsl_combination_memcpy (d, c);
    gsl_test (!s, "gsl_combination_memcpy, (7,3) vs (6,3)");
    gsl_combination_free(d);
  }

  {
    gsl_combination * d = gsl_combination_alloc (7,2);
    int s = gsl_combination_memcpy (d, c);
    gsl_test (!s, "gsl_combination_memcpy, (7,2) vs (6,3)");
    gsl_combination_free(d);
  }


  gsl_combination_free (c);

  exit (gsl_test_summary());
}

void
my_error_handler (const char *reason, const char *file, int line, int err)
{
  if (0) printf ("(caught [%s:%d: %s (%d)])\n", file, line, reason, err) ;
}
