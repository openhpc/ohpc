/* multiset/test.c
 * based on combination/test.c by Szymon Jaroszewicz
 * based on permutation/test.c by Brian Gough
 *
 * Copyright (C) 2001 Szymon Jaroszewicz
 * Copyright (C) 2009 Rhys Ulerich
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
#include <gsl/gsl_multiset.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>

size_t c63[56][3] = {
  { 0,0,0 },  { 0,0,1 },  { 0,0,2 },  { 0,0,3 }, { 0,0,4 },  { 0,0,5 },
              { 0,1,1 },  { 0,1,2 },  { 0,1,3 }, { 0,1,4 },  { 0,1,5 },
                          { 0,2,2 },  { 0,2,3 }, { 0,2,4 },  { 0,2,5 },
                                      { 0,3,3 }, { 0,3,4 },  { 0,3,5 },
                                                 { 0,4,4 },  { 0,4,5 },
                                                             { 0,5,5 },
              { 1,1,1 },  { 1,1,2 },  { 1,1,3 }, { 1,1,4 },  { 1,1,5 },
                          { 1,2,2 },  { 1,2,3 }, { 1,2,4 },  { 1,2,5 },
                                      { 1,3,3 }, { 1,3,4 },  { 1,3,5 },
                                                 { 1,4,4 },  { 1,4,5 },
                                                             { 1,5,5 },
                          { 2,2,2 },  { 2,2,3 }, { 2,2,4 },  { 2,2,5 },
                                      { 2,3,3 }, { 2,3,4 },  { 2,3,5 },
                                                 { 2,4,4 },  { 2,4,5 },
                                                             { 2,5,5 },
                                      { 3,3,3 }, { 3,3,4 },  { 3,3,5 },
                                                 { 3,4,4 },  { 3,4,5 },
                                                             { 3,5,5 },
                                                 { 4,4,4 },  { 4,4,5 },
                                                             { 4,5,5 },
                                                             { 5,5,5 }
} ;

void my_error_handler (const char *reason, const char *file, int line, int err);

int
main (void)
{
  size_t i, j;
  int status = 0, s;
  gsl_multiset * c ;

  gsl_ieee_env_setup ();

  c = gsl_multiset_alloc (6,3);

  /* Test multisets in forward order */

  gsl_multiset_init_first (c);

  i = 0;

  do
    {
      if ( i >= 56 )
        {
          status = 1;
          break;
        }
      for (j = 0; j < 3; j++)
        {
          status |= (c->data[j] != c63[i][j]);
        }

      {
        int s1 = gsl_multiset_valid (c);
        gsl_test (s1, "gsl_multiset_valid (%u)", i);
      }

      i++;
    }
  while (gsl_multiset_next(c) == GSL_SUCCESS);

  gsl_test(status, "gsl_multiset_next, 6 choose 3 multiset, 56 steps");

  gsl_multiset_next(c);
  gsl_multiset_next(c);
  gsl_multiset_next(c);
  for (j = 0; j < 3; j++)
    {
      status |= (c->data[j] != c63[55][j]);
    }
  gsl_test(status, "gsl_multiset_next on the last multiset");

  {
    int s1 = gsl_multiset_valid (c);
    gsl_test (s1, "gsl_multiset_valid on the last multiset");
  }

  {
    gsl_multiset * d = gsl_multiset_alloc (6,3);
    gsl_multiset_memcpy (d, c);

    status = 0;

    for (j = 0; j < 3; j++)
      {
        status |= (d->data[j] != c->data[j]);
      }

    gsl_test (status, "gsl_multiset_memcpy, 6 choose 3 multiset");
    gsl_multiset_free(d);
  }


  /* Now test multisets in reverse order */

  gsl_multiset_init_last (c);

  i = 56;
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
        int s1 = gsl_multiset_valid (c);
        gsl_test (s1, "gsl_multiset_valid (%u)", i);
      }
    }
  while (gsl_multiset_prev(c) == GSL_SUCCESS);

  gsl_test(status, "gsl_multiset_prev, 6 choose 3 multiset, 20 steps");

  gsl_multiset_prev(c);
  gsl_multiset_prev(c);
  gsl_multiset_prev(c);
  for (j = 0; j < 3; j++)
    {
      status |= (c->data[j] != c63[0][j]);
    }
  gsl_test(status, "gsl_multiset_prev on the first multiset");

  {
    int s1 = gsl_multiset_valid (c);
    gsl_test (s1, "gsl_multiset_valid on the first multiset");
  }

  {
    gsl_multiset * d = gsl_multiset_alloc (6,3);
    gsl_multiset_memcpy (d, c);

    status = 0;

    for (j = 0; j < 3; j++)
      {
        status |= (d->data[j] != c->data[j]);
      }

    gsl_test (status, "gsl_multiset_memcpy, 6 choose 3 multiset");
    gsl_multiset_free(d);
  }

  gsl_multiset_free (c);

  /* Check k = 3 strictly greater than n = 2 gives results */
  /* {0, 0, 0}, {0, 0, 1}, {0, 1, 1,}, and {1, 1, 1}.      */
  c = gsl_multiset_calloc(2, 3);
  status |= (c->data[0] != 0);
  status |= (c->data[1] != 0);
  status |= (c->data[2] != 0);
  status |= (gsl_multiset_valid(c) != GSL_SUCCESS);
  status |= (gsl_multiset_next(c) == GSL_FAILURE);
  status |= (c->data[0] != 0);
  status |= (c->data[1] != 0);
  status |= (c->data[2] != 1);
  status |= (gsl_multiset_valid(c) != GSL_SUCCESS);
  status |= (gsl_multiset_next(c) == GSL_FAILURE);
  status |= (c->data[0] != 0);
  status |= (c->data[1] != 1);
  status |= (c->data[2] != 1);
  status |= (gsl_multiset_valid(c) != GSL_SUCCESS);
  status |= (gsl_multiset_next(c) == GSL_FAILURE);
  status |= (c->data[0] != 1);
  status |= (c->data[1] != 1);
  status |= (c->data[2] != 1);
  status |= (gsl_multiset_valid(c) != GSL_SUCCESS);
  status |= (gsl_multiset_next(c) != GSL_FAILURE);
  gsl_test(status, "gsl_multiset 2 choose 3");
  gsl_multiset_free (c);

  c = gsl_multiset_calloc(7, 0);
  /* should return GSL_FAILURE every time */
  status |= (gsl_multiset_next(c) != GSL_FAILURE);
  status |= (gsl_multiset_next(c) != GSL_FAILURE);
  status |= (gsl_multiset_prev(c) != GSL_FAILURE);
  status |= (gsl_multiset_prev(c) != GSL_FAILURE);
  gsl_test(status, "gsl_multiset 7 choose 0");
  gsl_multiset_free (c);

  c = gsl_multiset_calloc(1, 1);
  /* should return GSL_FAILURE every time */
  for(j = 0; j < 1; j++)
  {
    status |= (gsl_multiset_get(c, j) != j);
  }
  status |= (gsl_multiset_next(c) != GSL_FAILURE);
  for(j = 0; j < 1; j++)
  {
    status |= (gsl_multiset_get(c, j) != j);
  }
  status |= (gsl_multiset_next(c) != GSL_FAILURE);
  for(j = 0; j < 1; j++)
  {
    status |= (gsl_multiset_get(c, j) != j);
  }
  status |= (gsl_multiset_prev(c) != GSL_FAILURE);
  for(j = 0; j < 1; j++)
  {
    status |= (gsl_multiset_get(c, j) != j);
  }
  status |= (gsl_multiset_prev(c) != GSL_FAILURE);
  for(j = 0; j < 1; j++)
  {
    status |= (gsl_multiset_get(c, j) != j);
  }
  gsl_test(status, "gsl_multiset 7 choose 7");
  gsl_multiset_free (c);

  c = gsl_multiset_calloc(6, 3);

  gsl_set_error_handler (&my_error_handler);

  c->data[0] = 1;
  c->data[1] = 2;
  c->data[2] = 1;
  s = gsl_multiset_valid (c);
  gsl_test (!s, "gsl_multiset_valid on an invalid multiset (1,1,2)");

  c->data[0] = 2;
  c->data[1] = 1;
  c->data[2] = 0;
  s = gsl_multiset_valid (c);
  gsl_test (!s, "gsl_multiset_valid on an invalid multiset (2,1,0)");

  c->data[0] = 1;
  c->data[1] = 2;
  c->data[2] = 0;
  s = gsl_multiset_valid (c);
  gsl_test (!s, "gsl_multiset_valid on an invalid multiset (1,2,0)");

  {
    gsl_multiset * d = gsl_multiset_alloc (6,4);
    int s = gsl_multiset_memcpy (d, c);
    gsl_test (!s, "gsl_multiset_memcpy, (6,4) vs (6,3)");
    gsl_multiset_free(d);
  }

  {
    gsl_multiset * d = gsl_multiset_alloc (7,3);
    int s = gsl_multiset_memcpy (d, c);
    gsl_test (!s, "gsl_multiset_memcpy, (7,3) vs (6,3)");
    gsl_multiset_free(d);
  }

  {
    gsl_multiset * d = gsl_multiset_alloc (7,2);
    int s = gsl_multiset_memcpy (d, c);
    gsl_test (!s, "gsl_multiset_memcpy, (7,2) vs (6,3)");
    gsl_multiset_free(d);
  }


  gsl_multiset_free (c);

  exit (gsl_test_summary());
}

void
my_error_handler (const char *reason, const char *file, int line, int err)
{
  if (0) printf ("(caught [%s:%d: %s (%d)])\n", file, line, reason, err) ;
}
