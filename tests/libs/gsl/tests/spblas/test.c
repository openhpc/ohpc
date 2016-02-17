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
#include <gsl/gsl_spblas.h>

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
create_random_vector(gsl_vector *v, const gsl_rng *r)
{
  size_t i;

  for (i = 0; i < v->size; ++i)
    {
      double x = gsl_rng_uniform(r);
      gsl_vector_set(v, i, x);
    }
} /* create_random_vector() */

static int
test_vectors(gsl_vector *observed, gsl_vector *expected, const double tol,
             const char *str)
{
  int s = 0;
  size_t N = observed->size;
  size_t i;

  for (i = 0; i < N; ++i)
    {
      double x_obs = gsl_vector_get(observed, i);
      double x_exp = gsl_vector_get(expected, i);

      gsl_test_rel(x_obs, x_exp, tol, "N=%zu i=%zu %s", N, i, str);
    }

  return s;
} /* test_vectors() */

static void
test_dgemv(const size_t N, const size_t M, const double alpha,
           const double beta, const CBLAS_TRANSPOSE_t TransA,
           const gsl_rng *r)
{
  gsl_spmatrix *A = create_random_sparse(M, N, 0.2, r);
  gsl_spmatrix *C;
  gsl_matrix *A_dense = gsl_matrix_alloc(M, N);
  gsl_vector *x, *y, *y_gsl, *y_sp;
  size_t lenX, lenY;

  if (TransA == CblasNoTrans)
    {
      lenX = N;
      lenY = M;
    }
  else
    {
      lenX = M;
      lenY = N;
    }

  x = gsl_vector_alloc(lenX);
  y = gsl_vector_alloc(lenY);
  y_gsl = gsl_vector_alloc(lenY);
  y_sp = gsl_vector_alloc(lenY);

  /* create random dense vectors */
  create_random_vector(x, r);
  create_random_vector(y, r);

  /* copy A into A_dense */
  gsl_spmatrix_sp2d(A_dense, A);

  gsl_vector_memcpy(y_gsl, y);
  gsl_vector_memcpy(y_sp, y);

  /* compute y = alpha*op(A)*x + beta*y0 with gsl */
  gsl_blas_dgemv(TransA, alpha, A_dense, x, beta, y_gsl);

  /* compute y = alpha*op(A)*x + beta*y0 with spblas/triplet */
  gsl_spblas_dgemv(TransA, alpha, A, x, beta, y_sp);

  /* test y_sp = y_gsl */
  test_vectors(y_sp, y_gsl, 1.0e-10, "test_dgemv: triplet format");

  /* compute y = alpha*op(A)*x + beta*y0 with spblas/compcol */
  C = gsl_spmatrix_compcol(A);
  gsl_vector_memcpy(y_sp, y);
  gsl_spblas_dgemv(TransA, alpha, C, x, beta, y_sp);

  /* test y_sp = y_gsl */
  test_vectors(y_sp, y_gsl, 1.0e-10,
               "test_dgemv: compressed column format");

  gsl_spmatrix_free(A);
  gsl_spmatrix_free(C);
  gsl_matrix_free(A_dense);
  gsl_vector_free(x);
  gsl_vector_free(y);
  gsl_vector_free(y_gsl);
  gsl_vector_free(y_sp);
} /* test_dgemv() */

static void
test_dgemm(const double alpha, const size_t M, const size_t N,
           const gsl_rng *r)
{
  const size_t max = GSL_MAX(M, N);
  size_t i, j, k;
  gsl_matrix *A_dense = gsl_matrix_alloc(M, max);
  gsl_matrix *B_dense = gsl_matrix_alloc(max, N);
  gsl_matrix *C_dense = gsl_matrix_alloc(M, N);
  gsl_spmatrix *C = gsl_spmatrix_alloc_nzmax(M, N, 1, GSL_SPMATRIX_CCS);

  for (k = 1; k <= max; ++k)
    {
      gsl_matrix_view Ad = gsl_matrix_submatrix(A_dense, 0, 0, M, k);
      gsl_matrix_view Bd = gsl_matrix_submatrix(B_dense, 0, 0, k, N);
      gsl_spmatrix *TA = create_random_sparse(M, k, 0.2, r);
      gsl_spmatrix *TB = create_random_sparse(k, N, 0.2, r);
      gsl_spmatrix *A = gsl_spmatrix_compcol(TA);
      gsl_spmatrix *B = gsl_spmatrix_compcol(TB);

      gsl_spmatrix_set_zero(C);
      gsl_spblas_dgemm(alpha, A, B, C);

      /* make dense matrices and use standard dgemm to multiply them */
      gsl_spmatrix_sp2d(&Ad.matrix, TA);
      gsl_spmatrix_sp2d(&Bd.matrix, TB);
      gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, alpha, &Ad.matrix,
                     &Bd.matrix, 0.0, C_dense);

      /* compare C and C_dense */
      for (i = 0; i < M; ++i)
        {
          for (j = 0; j < N; ++j)
            {
              double Cij = gsl_spmatrix_get(C, i, j);
              double Dij = gsl_matrix_get(C_dense, i, j);

              gsl_test_rel(Cij, Dij, 1.0e-12, "test_dgemm: _dgemm");
            }
        }

      gsl_spmatrix_free(TA);
      gsl_spmatrix_free(TB);
      gsl_spmatrix_free(A);
      gsl_spmatrix_free(B);
    }

  gsl_spmatrix_free(C);
  gsl_matrix_free(A_dense);
  gsl_matrix_free(B_dense);
  gsl_matrix_free(C_dense);
} /* test_dgemm() */

int
main()
{
  const size_t N_max = 40;
  size_t m, n;
  gsl_rng *r = gsl_rng_alloc(gsl_rng_default);

  for (m = 1; m <= N_max; ++m)
    {
      for (n = 1; n <= N_max; ++n)
        {
          test_dgemv(m, n, 1.0, 0.0, CblasNoTrans, r);
          test_dgemv(m, n, 1.0, 0.0, CblasTrans, r);

          test_dgemv(m, n, 2.4, -0.5, CblasNoTrans, r);
          test_dgemv(m, n, 2.4, -0.5, CblasTrans, r);

          test_dgemv(m, n, 0.1, 10.0, CblasNoTrans, r);
          test_dgemv(m, n, 0.1, 10.0, CblasTrans, r);
        }
    }

  test_dgemm(1.0, 10, 10, r);
  test_dgemm(2.3, 20, 15, r);
  test_dgemm(1.8, 12, 30, r);
  test_dgemm(0.4, 45, 35, r);

  gsl_rng_free(r);

  exit (gsl_test_summary());
} /* main() */
