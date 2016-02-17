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
#include <gsl/gsl_splinalg.h>

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
  size_t i;

  /* set diagonal entries to try to ensure non-singularity */
  for (i = 0; i < GSL_MIN(M, N); ++i)
    {
      double x = gsl_rng_uniform(r);
      gsl_spmatrix_set(m, i, i, x);
    }

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

/*
test_poisson()
  Solve u''(x) = -pi^2 sin(pi*x), u(x) = sin(pi*x)
  epsrel is the relative error threshold with the exact solution
*/
static void
test_poisson(const size_t N, const double epsrel, const int compress)
{
  const gsl_splinalg_itersolve_type *T = gsl_splinalg_itersolve_gmres;
  const size_t n = N - 2;                     /* subtract 2 to exclude boundaries */
  const double h = 1.0 / (N - 1.0);           /* grid spacing */
  const double tol = 1.0e-9;
  const size_t max_iter = 10;
  size_t iter = 0;
  gsl_spmatrix *A = gsl_spmatrix_alloc(n ,n); /* triplet format */
  gsl_spmatrix *B;
  gsl_vector *b = gsl_vector_alloc(n);        /* right hand side vector */
  gsl_vector *u = gsl_vector_calloc(n);       /* solution vector, u0 = 0 */
  gsl_splinalg_itersolve *w = gsl_splinalg_itersolve_alloc(T, n, 0);
  const char *desc = gsl_splinalg_itersolve_name(w);
  size_t i;
  int status;

  /* construct the sparse matrix for the finite difference equation */

  /* first row of matrix */
  gsl_spmatrix_set(A, 0, 0, -2.0);
  gsl_spmatrix_set(A, 0, 1, 1.0);

  /* loop over interior grid points */
  for (i = 1; i < n - 1; ++i)
    {
      gsl_spmatrix_set(A, i, i + 1, 1.0);
      gsl_spmatrix_set(A, i, i, -2.0);
      gsl_spmatrix_set(A, i, i - 1, 1.0);
    }

  /* last row of matrix */
  gsl_spmatrix_set(A, n - 1, n - 1, -2.0);
  gsl_spmatrix_set(A, n - 1, n - 2, 1.0);

  /* scale by h^2 */
  gsl_spmatrix_scale(A, 1.0 / (h * h));

  /* construct right hand side vector */
  for (i = 0; i < n; ++i)
    {
      double xi = (i + 1) * h;
      double bi = -M_PI * M_PI * sin(M_PI * xi);
      gsl_vector_set(b, i, bi);
    }

  if (compress)
    B = gsl_spmatrix_compcol(A);
  else
    B = A;

  /* solve the system */
  do
    {
      status = gsl_splinalg_itersolve_iterate(B, b, tol, u, w);
    }
  while (status == GSL_CONTINUE && ++iter < max_iter);

  gsl_test(status, "%s poisson status s=%d N=%zu", desc, status, N);

  /* check solution against analytic */
  for (i = 0; i < n; ++i)
    {
      double xi = (i + 1) * h;
      double u_gsl = gsl_vector_get(u, i);
      double u_exact = sin(M_PI * xi);

      gsl_test_rel(u_gsl, u_exact, epsrel, "%s poisson N=%zu i=%zu",
                   desc, N, i);
    }

  /* check that the residual satisfies ||r|| <= tol*||b|| */
  {
    gsl_vector *r = gsl_vector_alloc(n);
    double normr, normb;

    gsl_vector_memcpy(r, b);
    gsl_spblas_dgemv(CblasNoTrans, -1.0, A, u, 1.0, r);

    normr = gsl_blas_dnrm2(r);
    normb = gsl_blas_dnrm2(b);

    status = (normr <= tol*normb) != 1;
    gsl_test(status, "%s poisson residual N=%zu normr=%.12e normb=%.12e",
             desc, N, normr, normb);

    gsl_vector_free(r);
  }

  gsl_splinalg_itersolve_free(w);
  gsl_spmatrix_free(A);
  gsl_vector_free(b);
  gsl_vector_free(u);

  if (compress)
    gsl_spmatrix_free(B);
} /* test_poisson() */

/*
test_toeplitz()
  Test Toeplitz matrix T = A + B + C where:
A = diag(a,-1)
B = diag(b)
C = diag(c,1)

rhs = ones(n,1)
*/

static void
test_toeplitz(const size_t N, const double a, const double b,
              const double c)
{
  int status;
  const double tol = 1.0e-10;
  const size_t max_iter = 10;
  const gsl_splinalg_itersolve_type *T = gsl_splinalg_itersolve_gmres;
  const char *desc;
  gsl_spmatrix *A;
  gsl_vector *rhs, *x;
  gsl_splinalg_itersolve *w;
  size_t i, iter = 0;

  if (N <= 1)
    return;

  A = gsl_spmatrix_alloc(N ,N);
  rhs = gsl_vector_alloc(N);
  x = gsl_vector_calloc(N);
  w = gsl_splinalg_itersolve_alloc(T, N, 0);
  desc = gsl_splinalg_itersolve_name(w);

  /* first row */
  gsl_spmatrix_set(A, 0, 0, b);
  gsl_spmatrix_set(A, 0, 1, c);

  /* interior rows */
  for (i = 1; i < N - 1; ++i)
    {
      gsl_spmatrix_set(A, i, i - 1, a);
      gsl_spmatrix_set(A, i, i, b);
      gsl_spmatrix_set(A, i, i + 1, c);
    }

  /* last row */
  gsl_spmatrix_set(A, N - 1, N - 2, a);
  gsl_spmatrix_set(A, N - 1, N - 1, b);

  /* set rhs vector */
  gsl_vector_set_all(rhs, 1.0);

  /* solve the system */
  do
    {
      status = gsl_splinalg_itersolve_iterate(A, rhs, tol, x, w);
    }
  while (status == GSL_CONTINUE && ++iter < max_iter);

  gsl_test(status, "%s toeplitz status s=%d N=%zu a=%f b=%f c=%f",
           desc, status, N, a, b, c);

  /* check that the residual satisfies ||r|| <= tol*||b|| */
  {
    gsl_vector *r = gsl_vector_alloc(N);
    double normr, normb;

    gsl_vector_memcpy(r, rhs);
    gsl_spblas_dgemv(CblasNoTrans, -1.0, A, x, 1.0, r);

    normr = gsl_blas_dnrm2(r);
    normb = gsl_blas_dnrm2(rhs);

    status = (normr <= tol*normb) != 1;
    gsl_test(status, "%s toeplitz residual N=%zu a=%f b=%f c=%f normr=%.12e normb=%.12e",
             desc, N, a, b, c, normr, normb);

    gsl_vector_free(r);
  }

  gsl_vector_free(x);
  gsl_vector_free(rhs);
  gsl_spmatrix_free(A);
  gsl_splinalg_itersolve_free(w);
} /* test_toeplitz() */

static void
test_random(const size_t N, const gsl_rng *r, const int compress)
{
  const gsl_splinalg_itersolve_type *T = gsl_splinalg_itersolve_gmres;
  const double tol = 1.0e-8;
  int status;
  gsl_spmatrix *A = create_random_sparse(N, N, 0.3, r);
  gsl_spmatrix *B;
  gsl_vector *b = gsl_vector_alloc(N);
  gsl_vector *x = gsl_vector_calloc(N);

  /* these random matrices require all N iterations to converge */
  gsl_splinalg_itersolve *w = gsl_splinalg_itersolve_alloc(T, N, N);

  const char *desc = gsl_splinalg_itersolve_name(w);

  create_random_vector(b, r);

  if (compress)
    B = gsl_spmatrix_compcol(A);
  else
    B = A;

  status = gsl_splinalg_itersolve_iterate(B, b, tol, x, w);
  gsl_test(status, "%s random status s=%d N=%zu", desc, status, N);

  /* check that the residual satisfies ||r|| <= tol*||b|| */
  {
    gsl_vector *res = gsl_vector_alloc(N);
    double normr, normb;

    gsl_vector_memcpy(res, b);
    gsl_spblas_dgemv(CblasNoTrans, -1.0, A, x, 1.0, res);

    normr = gsl_blas_dnrm2(res);
    normb = gsl_blas_dnrm2(b);

    status = (normr <= tol*normb) != 1;
    gsl_test(status, "%s random residual N=%zu normr=%.12e normb=%.12e",
             desc, N, normr, normb);

    gsl_vector_free(res);
  }

  gsl_spmatrix_free(A);
  gsl_vector_free(b);
  gsl_vector_free(x);
  gsl_splinalg_itersolve_free(w);

  if (compress)
    gsl_spmatrix_free(B);
} /* test_random() */

int
main()
{
  gsl_rng *r = gsl_rng_alloc(gsl_rng_default);
  size_t n;

  test_poisson(7, 1.0e-1, 0);
  test_poisson(7, 1.0e-1, 1);

  test_poisson(543, 1.0e-5, 0);
  test_poisson(543, 1.0e-5, 1);

  test_poisson(1000, 1.0e-6, 0);
  test_poisson(1000, 1.0e-6, 1);

  test_poisson(5000, 1.0e-7, 0);
  test_poisson(5000, 1.0e-7, 1);

  test_toeplitz(15, 0.01, 1.0, 0.01);
  test_toeplitz(15, 1.0, 1.0, 0.01);
  test_toeplitz(50, 1.0, 2.0, 0.01);
  test_toeplitz(1000, 0.5, 1.0, 0.01);

  for (n = 1; n <= 100; ++n)
    {
      test_random(n, r, 0);
      test_random(n, r, 1);
    }

  gsl_rng_free(r);

  exit (gsl_test_summary());
} /* main() */
