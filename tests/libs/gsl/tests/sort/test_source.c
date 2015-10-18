/* sort/test_source.c
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

void TYPE (test_sort_vector) (size_t N, size_t stride);
void FUNCTION (my, initialize) (TYPE (gsl_vector) * v);
void FUNCTION (my, randomize) (TYPE (gsl_vector) * v);
int FUNCTION (my, check) (TYPE (gsl_vector) * data, TYPE (gsl_vector) * orig);
int FUNCTION (my, pcheck) (gsl_permutation * p, TYPE (gsl_vector) * data, TYPE (gsl_vector) * orig);
int FUNCTION (my, scheck) (BASE * x, size_t k, TYPE (gsl_vector) * data);
int FUNCTION (my, lcheck) (BASE * x, size_t k, TYPE (gsl_vector) * data);
int FUNCTION (my, sicheck) (size_t * p, size_t k, gsl_permutation * perm,
                            TYPE (gsl_vector) * data);
int FUNCTION (my, licheck) (size_t * p, size_t k, gsl_permutation * perm,
                            TYPE (gsl_vector) * data);

void
TYPE (test_sort_vector) (size_t N, size_t stride)
{
  int status;
  size_t  k = N/2;

  TYPE (gsl_block) * b1 = FUNCTION (gsl_block, calloc) (N * stride);
  TYPE (gsl_block) * b2 = FUNCTION (gsl_block, calloc) (N * stride);
  TYPE (gsl_block) * b3 = FUNCTION (gsl_block, calloc) (N * stride);

  TYPE (gsl_vector) * orig = FUNCTION (gsl_vector, alloc_from_block) (b1, 0, N, stride);
  TYPE (gsl_vector) * data = FUNCTION (gsl_vector, alloc_from_block) (b2, 0, N, stride);
  TYPE (gsl_vector) * data2 = FUNCTION (gsl_vector, alloc_from_block) (b3, 0, N, stride);

  BASE * small = (BASE *)malloc(k * sizeof(BASE));
  BASE * large = (BASE *)malloc(k * sizeof(BASE));
  size_t * index = (size_t *)malloc(k * sizeof(size_t));

  gsl_permutation *p = gsl_permutation_alloc (N);

  FUNCTION (my, initialize) (orig);

  /* Already sorted */
  FUNCTION (gsl_vector, memcpy) (data, orig);

  status = FUNCTION (gsl_sort_vector, index) (p, data);
  status |= FUNCTION (my, pcheck) (p, data, orig);
  gsl_test (status, "indexing " NAME (gsl_vector) ", n = %u, stride = %u, ordered", N, stride);

  TYPE (gsl_sort_vector) (data);
  status = FUNCTION (my, check) (data, orig);
  gsl_test (status, "sorting, " NAME (gsl_vector) ", n = %u, stride = %u, ordered", N, stride);

  FUNCTION (gsl_sort_vector, smallest) (small, k, data);
  status = FUNCTION (my, scheck) (small, k, orig);
  gsl_test (status, "smallest, " NAME (gsl_vector) ", n = %u, stride = %u, ordered", N, stride);

  FUNCTION (gsl_sort_vector, largest) (large, k, data);
  status = FUNCTION (my, lcheck) (large, k, orig);
  gsl_test (status, "largest, " NAME (gsl_vector) ", n = %u, stride = %u, ordered", N, stride);

  FUNCTION (gsl_sort_vector, smallest_index) (index, k, data);
  status = FUNCTION (my, sicheck) (index, k, p, data);
  gsl_test (status, "smallest index, " NAME (gsl_vector) ", n = %u, stride = %u, ordered", N, stride);

  FUNCTION (gsl_sort_vector, largest_index) (index, k, data);
  status = FUNCTION (my, licheck) (index, k, p, data);
  gsl_test (status, "largest index, " NAME (gsl_vector) ", n = %u, stride = %u, ordered", N, stride);

  /* Reverse the data */

  FUNCTION (gsl_vector, memcpy) (data, orig);
  FUNCTION (gsl_vector, reverse) (data);

  status = FUNCTION (gsl_sort_vector, index) (p, data);
  status |= FUNCTION (my, pcheck) (p, data, orig);
  gsl_test (status, "indexing " NAME (gsl_vector) ", n = %u, stride = %u, reversed", N, stride);

  TYPE (gsl_sort_vector) (data);
  status = FUNCTION (my, check) (data, orig);
  gsl_test (status, "sorting, " NAME (gsl_vector) ", n = %u, stride = %u, reversed", N, stride);

  FUNCTION (gsl_vector, memcpy) (data, orig);
  FUNCTION (gsl_vector, reverse) (data);
  FUNCTION (gsl_vector, memcpy) (data2, data);

  TYPE (gsl_sort_vector2) (data, data2);
  status = FUNCTION (my, check) (data, orig);
  gsl_test (status, "sorting2, " NAME (gsl_vector) ", n = %u, stride = %u, reversed data", N, stride);
  status = FUNCTION (my, check) (data2, orig);
  gsl_test (status, "sorting2, " NAME (gsl_vector) ", n = %u, stride = %u, reversed data2", N, stride);

  FUNCTION (gsl_vector, memcpy) (data, orig);
  FUNCTION (gsl_vector, reverse) (data);

  FUNCTION (gsl_sort_vector, smallest) (small, k, data);
  status = FUNCTION (my, scheck) (small, k, orig);
  gsl_test (status, "smallest, " NAME (gsl_vector) ", n = %u, stride = %u, reversed", N, stride);

  FUNCTION (gsl_sort_vector, largest) (large, k, data);
  status = FUNCTION (my, lcheck) (large, k, orig);
  gsl_test (status, "largest, " NAME (gsl_vector) ", n = %u, stride = %u, reversed", N, stride);

  FUNCTION (gsl_sort_vector, smallest_index) (index, k, data);
  status = FUNCTION (my, sicheck) (index, k, p, data);
  gsl_test (status, "smallest index, " NAME (gsl_vector) ", n = %u, stride = %u, reversed", N, stride);

  FUNCTION (gsl_sort_vector, largest_index) (index, k, data);
  status = FUNCTION (my, licheck) (index, k, p, data);
  gsl_test (status, "largest index, " NAME (gsl_vector) ", n = %u, stride = %u, reversed", N, stride);

  /* Perform some shuffling */

  FUNCTION (gsl_vector, memcpy) (data, orig);
  FUNCTION (my, randomize) (data);
  FUNCTION (gsl_vector, memcpy) (data2, data);

  TYPE (gsl_sort_vector2) (data, data2);
  status = FUNCTION (my, check) (data, orig);
  gsl_test (status, "sorting2, " NAME (gsl_vector) ", n = %u, stride = %u, randomized data", N, stride);
  status = FUNCTION (my, check) (data2, orig);
  gsl_test (status, "sorting2, " NAME (gsl_vector) ", n = %u, stride = %u, randomized data2", N, stride);

  FUNCTION (gsl_vector, memcpy) (data, orig);
  FUNCTION (my, randomize) (data);
  FUNCTION (gsl_vector, memcpy) (data2, data);

  status = FUNCTION (gsl_sort_vector, index) (p, data);
  status |= FUNCTION (my, pcheck) (p, data, orig);
  gsl_test (status, "indexing " NAME (gsl_vector) ", n = %u, stride = %u, randomized", N, stride);

  TYPE (gsl_sort_vector) (data);
  status = FUNCTION (my, check) (data, orig);
  gsl_test (status, "sorting, " NAME (gsl_vector) ", n = %u, stride = %u, randomized", N, stride);

  FUNCTION (gsl_vector, memcpy) (data, data2);

  FUNCTION (gsl_sort_vector, smallest) (small, k, data);
  status = FUNCTION (my, scheck) (small, k, orig);
  gsl_test (status, "smallest, " NAME (gsl_vector) ", n = %u, stride = %u, randomized", N, stride);

  FUNCTION (gsl_sort_vector, largest) (large, k, data);
  status = FUNCTION (my, lcheck) (large, k, orig);
  gsl_test (status, "largest, " NAME (gsl_vector) ", n = %u, stride = %u, randomized", N, stride);

  FUNCTION (gsl_sort_vector, smallest_index) (index, k, data);
  status = FUNCTION (my, sicheck) (index, k, p, data);
  gsl_test (status, "smallest index, " NAME (gsl_vector) ", n = %u, stride = %u, randomized", N, stride);

  FUNCTION (gsl_sort_vector, largest_index) (index, k, data);
  status = FUNCTION (my, licheck) (index, k, p, data);
  gsl_test (status, "largest index, " NAME (gsl_vector) ", n = %u, stride = %u, randomized", N, stride);

  FUNCTION (gsl_vector, free) (orig);
  FUNCTION (gsl_vector, free) (data);
  FUNCTION (gsl_vector, free) (data2);
  FUNCTION (gsl_block, free) (b1);
  FUNCTION (gsl_block, free) (b2);
  FUNCTION (gsl_block, free) (b3);
  gsl_permutation_free (p);
  free (small);
  free (large);
  free (index);
}


void
FUNCTION (my, initialize) (TYPE (gsl_vector) * v)
{
  size_t i;
  ATOMIC k = 0;
  volatile ATOMIC kk;

  /* Must be sorted initially */

  for (i = 0; i < v->size; i++)
    {
      kk = k;
      k++;
      /* Prevent overflow */
      if (k < kk) k = kk;
      FUNCTION (gsl_vector, set) (v, i, k);
    }
}

void
FUNCTION (my, randomize) (TYPE (gsl_vector) * v)
{
  size_t i;

  for (i = 0; i < v->size; i++)
    {
      size_t j = urand (v->size);
      FUNCTION (gsl_vector, swap_elements) (v, i, j);
    }
}

int
FUNCTION (my, check) (TYPE (gsl_vector) * data, TYPE (gsl_vector) * orig)
{
  size_t i;

  for (i = 0; i < data->size; i++)
    {
      if (FUNCTION (gsl_vector, get) (data, i) != FUNCTION (gsl_vector, get) (orig, i))
        {
#if DUMP_ERROR
          size_t j;
          for (j = 0 ; j < data->size; j++) {
            printf("%u: " OUT_FORMAT " " OUT_FORMAT " %c\n", j,
                   FUNCTION (gsl_vector, get) (data, j),
                   FUNCTION (gsl_vector, get) (orig, j),
                   (i == j) ? '*' : ' ');
          }
#endif

          return GSL_FAILURE;
        }
    }

  return GSL_SUCCESS;
}

int
FUNCTION (my, pcheck) (gsl_permutation * p, TYPE (gsl_vector) * data, TYPE (gsl_vector) * orig)
{
  size_t i;

  for (i = 0; i < p->size; i++)
    {
      if (FUNCTION (gsl_vector, get) (data, p->data[i]) != FUNCTION (gsl_vector, get) (orig, i))
        {
          return GSL_FAILURE;
        }
    }

  return GSL_SUCCESS;
}

int
FUNCTION (my, scheck) (BASE * x, size_t k, TYPE (gsl_vector) * data)
{
  size_t i;

  for (i = 0; i < k; i++)
    {
      if (x[i] != FUNCTION (gsl_vector, get) (data, i))
        {
          return GSL_FAILURE;
        }
    }

  return GSL_SUCCESS;
}



int
FUNCTION (my, lcheck) (BASE * x, size_t k, TYPE (gsl_vector) * data)
{
  size_t i;

  for (i = 0; i < k; i++)
    {
      if (x[i] != FUNCTION (gsl_vector, get) (data, data->size - i - 1))
        {
          return GSL_FAILURE;
        }
    }

  return GSL_SUCCESS;
}


int
FUNCTION (my, sicheck) (size_t * p1, size_t k, gsl_permutation * p,
                        TYPE (gsl_vector) * data)
{
  size_t i;

  for (i = 0; i < k; i++)
    {
      if (FUNCTION(gsl_vector,get)(data,p1[i]) 
          != FUNCTION(gsl_vector,get)(data, p->data[i]))
        {
          return GSL_FAILURE;
        }
    }

  return GSL_SUCCESS;
}

int
FUNCTION (my, licheck) (size_t * p1, size_t k, gsl_permutation * p,
                        TYPE (gsl_vector) * data)
{
  size_t i;

  for (i = 0; i < k; i++)
    {
      if (FUNCTION(gsl_vector,get)(data,p1[i]) 
          != FUNCTION(gsl_vector,get)(data, p->data[p->size - i - 1]))
        {
          return GSL_FAILURE;
        }
    }

  return GSL_SUCCESS;
}



