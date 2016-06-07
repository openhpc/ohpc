/* test.c
 * 
 * Copyright (C) 2012-2014 Patrick Alken
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
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_spmatrix.h>

/*
create_random_sparse()
  Create a random sparse matrix with approximately
M*N*density non-zero entries

Inputs: M       - number of rows
        N       - number of columns
        density - sparse density \in [0,1]
                  0 = no non-zero entries
                  1 = all m*n entries are filled
        r       - random number generator

Return: pointer to sparse matrix in triplet format (must be freed by caller)

Notes:
1) non-zero matrix entries are uniformly distributed in [0,1]
*/

static gsl_spmatrix *
create_random_sparse(const size_t M, const size_t N, const double density,
                     const gsl_rng *r)
{
  size_t nnzwanted = (size_t) floor(M * N * GSL_MIN(density, 1.0));
  gsl_spmatrix *m = gsl_spmatrix_alloc_nzmax(M, N,
                                             nnzwanted,
                                             GSL_SPMATRIX_TRIPLET);

  while (gsl_spmatrix_nnz(m) < nnzwanted)
    {
      /* generate a random row and column */
      size_t i = gsl_rng_uniform(r) * M;
      size_t j = gsl_rng_uniform(r) * N;

      /* generate random m_{ij} and add it */
      double x = gsl_rng_uniform(r);
      gsl_spmatrix_set(m, i, j, x);
    }

  return m;
} /* create_random_sparse() */

static void
test_getset(const size_t M, const size_t N, const gsl_rng *r)
{
  int status;
  size_t i, j;

  /* test triplet versions of _get and _set */
  {
    size_t k = 0;
    gsl_spmatrix *m = gsl_spmatrix_alloc(M, N);

    status = 0;
    for (i = 0; i < M; ++i)
      {
        for (j = 0; j < N; ++j)
          {
            double x = (double) ++k;
            double y;

            gsl_spmatrix_set(m, i, j, x);
            y = gsl_spmatrix_get(m, i, j);
            if (x != y)
              status = 1;
          }
      }

    gsl_test(status, "test_getset: M=%zu N=%zu _get != _set", M, N);

    /* test setting an element to 0 */
    gsl_spmatrix_set(m, 0, 0, 1.0);
    gsl_spmatrix_set(m, 0, 0, 0.0);

    status = gsl_spmatrix_get(m, 0, 0) != 0.0;
    gsl_test(status, "test_getset: M=%zu N=%zu m(0,0) = %f",
             M, N, gsl_spmatrix_get(m, 0, 0));

    /* test gsl_spmatrix_set_zero() */
    gsl_spmatrix_set(m, 0, 0, 1.0);
    gsl_spmatrix_set_zero(m);
    status = gsl_spmatrix_get(m, 0, 0) != 0.0;
    gsl_test(status, "test_getset: M=%zu N=%zu set_zero m(0,0) = %f",
             M, N, gsl_spmatrix_get(m, 0, 0));

    /* resassemble matrix to ensure nz is calculated correctly */
    k = 0;
    for (i = 0; i < M; ++i)
      {
        for (j = 0; j < N; ++j)
          {
            double x = (double) ++k;
            gsl_spmatrix_set(m, i, j, x);
          }
      }

    status = gsl_spmatrix_nnz(m) != M * N;
    gsl_test(status, "test_getset: M=%zu N=%zu set_zero nz = %zu",
             M, N, gsl_spmatrix_nnz(m));

    gsl_spmatrix_free(m);
  }

  /* test duplicate values are handled correctly */
  {
    size_t min = GSL_MIN(M, N);
    size_t expected_nnz = min;
    size_t nnz;
    size_t k = 0;
    gsl_spmatrix *m = gsl_spmatrix_alloc(M, N);

    status = 0;
    for (i = 0; i < min; ++i)
      {
        for (j = 0; j < 5; ++j)
          {
            double x = (double) ++k;
            double y;

            gsl_spmatrix_set(m, i, i, x);
            y = gsl_spmatrix_get(m, i, i);
            if (x != y)
              status = 1;
          }
      }

    gsl_test(status, "test_getset: duplicate test M=%zu N=%zu _get != _set", M, N);

    nnz = gsl_spmatrix_nnz(m);
    status = nnz != expected_nnz;
    gsl_test(status, "test_getset: duplicate test M=%zu N=%zu nnz=%zu, expected=%zu",
             M, N, nnz, expected_nnz);

    gsl_spmatrix_free(m);
  }

  /* test compressed version of gsl_spmatrix_get() */
  {
    gsl_spmatrix *T = create_random_sparse(M, N, 0.3, r);
    gsl_spmatrix *C = gsl_spmatrix_compcol(T);

    status = 0;
    for (i = 0; i < M; ++i)
      {
        for (j = 0; j < N; ++j)
          {
            double Tij = gsl_spmatrix_get(T, i, j);
            double Cij = gsl_spmatrix_get(C, i, j);

            if (Tij != Cij)
              status = 1;
          }
      }

    gsl_test(status, "test_getset: M=%zu N=%zu compressed _get", M, N);

    gsl_spmatrix_free(T);
    gsl_spmatrix_free(C);
  }
} /* test_getset() */

static void
test_memcpy(const size_t M, const size_t N, const gsl_rng *r)
{
  int status;

  {
    gsl_spmatrix *at = create_random_sparse(M, N, 0.2, r);
    gsl_spmatrix *ac = gsl_spmatrix_compcol(at);
    gsl_spmatrix *bt, *bc;
  
    bt = gsl_spmatrix_alloc(M, N);
    gsl_spmatrix_memcpy(bt, at);

    status = gsl_spmatrix_equal(at, bt) != 1;
    gsl_test(status, "test_memcpy: _memcpy M=%zu N=%zu triplet format", M, N);

    bc = gsl_spmatrix_alloc_nzmax(M, N, ac->nzmax, GSL_SPMATRIX_CCS);
    gsl_spmatrix_memcpy(bc, ac);

    status = gsl_spmatrix_equal(ac, bc) != 1;
    gsl_test(status, "test_memcpy: _memcpy M=%zu N=%zu compressed column format", M, N);

    gsl_spmatrix_free(at);
    gsl_spmatrix_free(ac);
    gsl_spmatrix_free(bt);
    gsl_spmatrix_free(bc);
  }

  /* test transpose_memcpy */
  {
    gsl_spmatrix *A = create_random_sparse(M, N, 0.3, r);
    gsl_spmatrix *B = gsl_spmatrix_compcol(A);
    gsl_spmatrix *AT = gsl_spmatrix_alloc(N, M);
    gsl_spmatrix *BT = gsl_spmatrix_alloc_nzmax(N, M, 1, GSL_SPMATRIX_CCS);
    size_t i, j;

    gsl_spmatrix_transpose_memcpy(AT, A);
    gsl_spmatrix_transpose_memcpy(BT, B);

    status = 0;
    for (i = 0; i < M; ++i)
      {
        for (j = 0; j < N; ++j)
          {
            double Aij = gsl_spmatrix_get(A, i, j);
            double ATji = gsl_spmatrix_get(AT, j, i);
            double Bij = gsl_spmatrix_get(B, i, j);
            double BTji = gsl_spmatrix_get(BT, j, i);

            if ((Aij != ATji) || (Bij != BTji) || (Aij != Bij))
              status = 1;
          }
      }

    gsl_test(status, "test_memcpy: _transpose_memcpy M=%zu N=%zu triplet format", M, N);

    gsl_spmatrix_free(A);
    gsl_spmatrix_free(AT);
    gsl_spmatrix_free(B);
    gsl_spmatrix_free(BT);
  }
} /* test_memcpy() */

static void
test_ops(const size_t M, const size_t N, const gsl_rng *r)
{
  size_t i, j;
  int status;

  /* test gsl_spmatrix_add */
  {
    gsl_spmatrix *Ta = create_random_sparse(M, N, 0.2, r);
    gsl_spmatrix *Tb = create_random_sparse(M, N, 0.2, r);
    gsl_spmatrix *a = gsl_spmatrix_compcol(Ta);
    gsl_spmatrix *b = gsl_spmatrix_compcol(Tb);
    gsl_spmatrix *c = gsl_spmatrix_alloc_nzmax(M, N, 1, GSL_SPMATRIX_CCS);
    
    gsl_spmatrix_add(c, a, b);

    status = 0;
    for (i = 0; i < M; ++i)
      {
        for (j = 0; j < N; ++j)
          {
            double aij = gsl_spmatrix_get(a, i, j);
            double bij = gsl_spmatrix_get(b, i, j);
            double cij = gsl_spmatrix_get(c, i, j);

            if (aij + bij != cij)
              status = 1;
          }
      }

    gsl_test(status, "test_ops: _add M=%zu N=%zu compressed format", M, N);

    gsl_spmatrix_free(Ta);
    gsl_spmatrix_free(Tb);
    gsl_spmatrix_free(a);
    gsl_spmatrix_free(b);
    gsl_spmatrix_free(c);
  }
} /* test_ops() */

int
main()
{
  gsl_rng *r = gsl_rng_alloc(gsl_rng_default);

  test_memcpy(10, 10, r);
  test_memcpy(10, 15, r);
  test_memcpy(53, 213, r);
  test_memcpy(920, 2, r);
  test_memcpy(2, 920, r);

  test_getset(20, 20, r);
  test_getset(30, 20, r);
  test_getset(15, 210, r);

  test_ops(20, 20, r);
  test_ops(50, 20, r);
  test_ops(20, 50, r);
  test_ops(76, 43, r);

  gsl_rng_free(r);

  exit (gsl_test_summary());
} /* main() */
