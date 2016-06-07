/* sort/test_heapsort.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 Thomas Walter, Brian Gough
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

int cmp_dbl (const void *a, const void *b);
void test_heapsort (size_t N);
void initialize (double *data, size_t N);
void cpy (double *dest, double *src, size_t N);
void randomize (double *data, size_t n);
void reverse (double *data, size_t N);
int check (double *data, double *orig, size_t N);
int pcheck (size_t * p, double *data, double *orig, size_t N);

void
test_heapsort (size_t N)
{
  int status;

  double *orig = (double *) malloc (N * sizeof (double));
  double *data = (double *) malloc (N * sizeof (double));
  size_t  *p = (size_t *) malloc (N * sizeof(size_t));

  initialize (orig, N);

  /* Already sorted */
  cpy (data, orig, N);

  status = gsl_heapsort_index (p, data, N, sizeof (double), (gsl_comparison_fn_t) & cmp_dbl);
  status |= pcheck (p, data, orig, N);
  gsl_test (status, "indexing array, n = %u, ordered", N);

  gsl_heapsort (data, N, sizeof (double), (gsl_comparison_fn_t) & cmp_dbl);
  status = check (data, orig, N);

  gsl_test (status, "sorting, array, n = %u, ordered", N);

  /* Reverse the data */

  cpy (data, orig, N);
  reverse (data, N);

  status = gsl_heapsort_index (p, data, N, sizeof (double), (gsl_comparison_fn_t) & cmp_dbl);
  status |= pcheck (p, data, orig, N);
  gsl_test (status, "indexing array, n = %u, reversed", N);

  gsl_heapsort (data, N, sizeof (double), (gsl_comparison_fn_t) & cmp_dbl);
  status = check (data, orig, N);

  gsl_test (status, "sorting, array, n = %u, reversed", N);

  /* Perform some shuffling */

  cpy (data, orig, N);
  randomize (data, N);

  status = gsl_heapsort_index (p, data, N, sizeof (double), (gsl_comparison_fn_t) & cmp_dbl);
  status |= pcheck (p, data, orig, N);
  gsl_test (status, "indexing array, n = %u, randomized", N);

  gsl_heapsort (data, N, sizeof (double), (gsl_comparison_fn_t) & cmp_dbl);
  status = check (data, orig, N);

  gsl_test (status, "sorting, array, n = %u, randomized", N);

  free (orig);
  free (data);
  free (p);
}

void
initialize (double *data, size_t N)
{
  size_t i;

  for (i = 0; i < N; i++)
    {
      data[i] = i;
    }
}

void
cpy (double *dest, double *src, size_t N)
{
  size_t i;

  for (i = 0; i < N; i++)
    {
      dest[i] = src[i];
    }
}

void
randomize (double *data, size_t N)
{
  size_t i;

  for (i = 0; i < N; i++)
    {
      size_t j = urand (N);
      double tmp = data[i];
      data[i] = data[j];
      data[j] = tmp;
    }
}

void
reverse (double *data, size_t N)
{
  size_t i;

  for (i = 0; i < N / 2; i++)
    {
      size_t j = N - i - 1;

      {
        double tmp = data[i];
        data[i] = data[j];
        data[j] = tmp;
      }
    }
}

int
check (double *data, double *orig, size_t N)
{
  size_t i;

  for (i = 0; i < N; i++)
    {
      if (data[i] != orig[i])
        {
          return GSL_FAILURE;
        }
    }

  return GSL_SUCCESS;
}

int
pcheck (size_t * p, double *data, double *orig, size_t N)
{
  size_t i;

  for (i = 0; i < N; i++)
    {
      if (data[p[i]] != orig[i])
        {
          return GSL_FAILURE;
        }
    }

  return GSL_SUCCESS;
}



int
cmp_dbl (const void *a, const void *b)
{
  const double x = *(const double *) a;
  const double y = *(const double *) b;
  if (x > y)
    return 1;
  if (x == y)
    return 0;
  else
    return -1;
}
