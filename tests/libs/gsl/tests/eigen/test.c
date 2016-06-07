/* eigen/test.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2006, 2007 Gerard Jungman, Patrick Alken, Brian Gough
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

/* Author:  G. Jungman
 */

#include <config.h>
#include <stdlib.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_complex_math.h>
#include <gsl/gsl_eigen.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_sort_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_vector.h>

/******************************************
 * common test code                       *
 ******************************************/

double 
chop_subnormals (double x) 
{
  /* Chop any subnormal values */
  return fabs(x) < GSL_DBL_MIN ? 0 : x;
}

void
create_random_symm_matrix(gsl_matrix *m, gsl_rng *r, int lower, int upper)
{
  size_t i, j;

  for (i = 0; i < m->size1; ++i)
    {
      for (j = i; j < m->size2; ++j)
      {
        double x = gsl_rng_uniform(r) * (upper - lower) + lower;
        gsl_matrix_set(m, i, j, x);
        gsl_matrix_set(m, j, i, x);
      }
    }
} /* create_random_symm_matrix() */

void
create_random_herm_matrix(gsl_matrix_complex *m, gsl_rng *r, int lower,
                          int upper)
{
  size_t i, j;

  for (i = 0; i < m->size1; ++i)
    {
      for (j = i; j < m->size2; ++j)
      {
        gsl_complex z;

        GSL_REAL(z) = gsl_rng_uniform(r) * (upper - lower) + lower;

        if (i == j)
          GSL_IMAG(z) = 0.0;
        else
          GSL_IMAG(z) = gsl_rng_uniform(r) * (upper - lower) + lower;

        gsl_matrix_complex_set(m, i, j, z);
        gsl_matrix_complex_set(m, j, i, gsl_complex_conjugate(z));
      }
    }
} /* create_random_herm_matrix() */

/* with r \in (0,1) if m_{ij} = r^{|i - j|} then m is positive definite */
void
create_random_posdef_matrix(gsl_matrix *m, gsl_rng *r)
{
  size_t i, j;
  double x = gsl_rng_uniform(r);

  for (i = 0; i < m->size1; ++i)
    {
      for (j = i; j < m->size2; ++j)
      {
        double a = pow(x, (double) (j - i));

        gsl_matrix_set(m, i, j, a);
        gsl_matrix_set(m, j, i, a);
      }
    }
} /* create_random_posdef_matrix() */

void
create_random_complex_posdef_matrix(gsl_matrix_complex *m, gsl_rng *r,
                                    gsl_vector_complex *work)
{
  const size_t N = m->size1;
  size_t i, j;
  double x, y;
  gsl_complex z;
  gsl_complex tau;

  GSL_SET_IMAG(&z, 0.0);

  /* make a positive diagonal matrix */
  gsl_matrix_complex_set_zero(m);
  for (i = 0; i < N; ++i)
    {
      x = gsl_rng_uniform(r);
      GSL_SET_REAL(&z, x);
      gsl_matrix_complex_set(m, i, i, z);
    }

  /* now generate random householder reflections and form P D P^H */
  for (i = 0; i < N; ++i)
    {
      /* form complex vector */
      for (j = 0; j < N; ++j)
        {
          x = 2.0 * gsl_rng_uniform(r) - 1.0;
          y = 2.0 * gsl_rng_uniform(r) - 1.0;
          GSL_SET_COMPLEX(&z, x, y);
          gsl_vector_complex_set(work, j, z);
        }

      tau = gsl_linalg_complex_householder_transform(work);
      gsl_linalg_complex_householder_hm(tau, work, m);
      gsl_linalg_complex_householder_mh(gsl_complex_conjugate(tau), work, m);
    }
} /* create_random_complex_posdef_matrix() */

void
create_random_nonsymm_matrix(gsl_matrix *m, gsl_rng *r, int lower,
                             int upper)
{
  size_t i, j;

  for (i = 0; i < m->size1; ++i)
    {
      for (j = 0; j < m->size2; ++j)
      {
        gsl_matrix_set(m,
                       i,
                       j,
                       gsl_rng_uniform(r) * (upper - lower) + lower);
      }
    }
} /* create_random_nonsymm_matrix() */

/* test if A Z = Q S */
void
test_eigen_schur(const gsl_matrix * A, const gsl_matrix * S,
                 const gsl_matrix * Q, const gsl_matrix * Z,
                 size_t count, const char * desc,
                 const char * desc2)
{
  const size_t N = A->size1;
  size_t i, j;

  gsl_matrix * T1 = gsl_matrix_alloc(N, N);
  gsl_matrix * T2 = gsl_matrix_alloc(N, N);

  /* compute T1 = A Z */
  gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, A, Z, 0.0, T1);

  /* compute T2 = Q S */
  gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, Q, S, 0.0, T2);

  for (i = 0; i < N; ++i)
    {
      for (j = 0; j < N; ++j)
        {
          double x = gsl_matrix_get(T1, i, j);
          double y = gsl_matrix_get(T2, i, j);

          gsl_test_abs(x, y, 1.0e8 * GSL_DBL_EPSILON,
                       "%s(N=%u,cnt=%u), %s, schur(%d,%d)", desc, N, count, desc2, i, j);
        }
    }

  gsl_matrix_free (T1);
  gsl_matrix_free (T2);
} /* test_eigen_schur() */

/* test if A is orthogonal */
int
test_eigen_orthog(gsl_matrix *A, size_t count, const char *desc,
                  const char *desc2)
{
  size_t N = A->size1;
  gsl_matrix *M = gsl_matrix_alloc(A->size1, A->size2);
  size_t i, j;

  gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1.0, A, A, 0.0, M);

  for (i = 0; i < A->size1; ++i)
    {
      for (j = 0; j < A->size2; ++j)
        {
          double val;
          double mij = gsl_matrix_get(M, i, j);

          if (i == j)
            val = 1.0;
          else
            val = 0.0;

          gsl_test_abs(mij, val, 1.0e8 * GSL_DBL_EPSILON,
                       "%s(N=%u,cnt=%u), %s, orthog(%d,%d)", desc, N, count, desc2, i, j);
        }
    }

  gsl_matrix_free(M);

  return 1;
} /* test_eigen_orthog() */

void
test_eigenvalues_real (const gsl_vector *eval, const gsl_vector * eval2, 
                       const char * desc, const char * desc2)
{
  const size_t N = eval->size;
  size_t i;

  double emax = 0;

  /* check eigenvalues */
  for (i = 0; i < N; i++) 
    {
      double ei = gsl_vector_get (eval, i);
      if (fabs(ei) > emax) emax = fabs(ei);
    }

  for (i = 0; i < N; i++)
    {
      double ei = gsl_vector_get (eval, i);
      double e2i = gsl_vector_get (eval2, i);
      e2i = chop_subnormals(e2i);
      gsl_test_abs(ei, e2i, emax * 1e8 * GSL_DBL_EPSILON, 
                   "%s, direct eigenvalue(%d), %s",
                   desc, i, desc2);
    }
}

void
test_eigenvalues_complex (const gsl_vector_complex * eval,
                          const gsl_vector_complex * eval2, 
                          const char * desc, const char * desc2)
{
  const size_t N = eval->size;
  size_t i;

  for (i = 0; i < N; i++)
    {
      gsl_complex ei = gsl_vector_complex_get (eval, i);
      gsl_complex e2i = gsl_vector_complex_get (eval2, i);
      gsl_test_rel(GSL_REAL(ei), GSL_REAL(e2i), 10*N*GSL_DBL_EPSILON, 
                   "%s, direct eigenvalue(%d) real, %s",
                   desc, i, desc2);
      gsl_test_rel(GSL_IMAG(ei), GSL_IMAG(e2i), 10*N*GSL_DBL_EPSILON, 
                   "%s, direct eigenvalue(%d) imag, %s",
                   desc, i, desc2);
    }
}

/******************************************
 * symm test code                         *
 ******************************************/

void
test_eigen_symm_results (const gsl_matrix * A, 
                         const gsl_vector * eval, 
                         const gsl_matrix * evec, 
                         size_t count,
                         const char * desc,
                         const char * desc2)
{
  const size_t N = A->size1;
  size_t i, j;
  double emax = 0;

  gsl_vector * x = gsl_vector_alloc(N);
  gsl_vector * y = gsl_vector_alloc(N);

  /* check eigenvalues */
  for (i = 0; i < N; i++) 
    {
      double ei = gsl_vector_get (eval, i);
      if (fabs(ei) > emax) emax = fabs(ei);
    }

  for (i = 0; i < N; i++)
    {
      double ei = gsl_vector_get (eval, i);
      gsl_vector_const_view vi = gsl_matrix_const_column(evec, i);
      gsl_vector_memcpy(x, &vi.vector);
      /* compute y = A x (should = lambda v) */
      gsl_blas_dgemv (CblasNoTrans, 1.0, A, x, 0.0, y);
      for (j = 0; j < N; j++)
        {
          double xj = gsl_vector_get (x, j);
          double yj = gsl_vector_get (y, j);
	  double eixj = chop_subnormals(ei * xj);
          gsl_test_abs(yj, eixj,  emax * 1e8 * GSL_DBL_EPSILON, 
                       "%s, eigenvalue(%d,%d), %s", desc, i, j, desc2);
        }
    }

  /* check eigenvectors are orthonormal */

  for (i = 0; i < N; i++)
    {
      gsl_vector_const_view vi = gsl_matrix_const_column(evec, i);
      double nrm_v = gsl_blas_dnrm2(&vi.vector);
      gsl_test_rel (nrm_v, 1.0, N * GSL_DBL_EPSILON, "%s, normalized(%d), %s", 
                    desc, i, desc2);
    }

  for (i = 0; i < N; i++)
    {
      gsl_vector_const_view vi = gsl_matrix_const_column(evec, i);
      for (j = i + 1; j < N; j++)
        {
          gsl_vector_const_view vj = gsl_matrix_const_column(evec, j);
          double vivj;
          gsl_blas_ddot (&vi.vector, &vj.vector, &vivj);
          gsl_test_abs (vivj, 0.0, N * GSL_DBL_EPSILON, 
                        "%s, orthogonal(%d,%d), %s", desc, i, j, desc2);
        }
    }

  gsl_vector_free(x);
  gsl_vector_free(y);
}

void
test_eigen_symm_matrix(const gsl_matrix * m, size_t count,
                       const char * desc)
{
  const size_t N = m->size1;
  gsl_matrix * A = gsl_matrix_alloc(N, N);
  gsl_vector * eval = gsl_vector_alloc(N);
  gsl_vector * evalv = gsl_vector_alloc(N);
  gsl_vector * x = gsl_vector_alloc(N);
  gsl_vector * y = gsl_vector_alloc(N);
  gsl_matrix * evec = gsl_matrix_alloc(N, N);
  gsl_eigen_symm_workspace * w = gsl_eigen_symm_alloc(N);
  gsl_eigen_symmv_workspace * wv = gsl_eigen_symmv_alloc(N);

  gsl_matrix_memcpy(A, m);

  gsl_eigen_symmv(A, evalv, evec, wv);
  test_eigen_symm_results(m, evalv, evec, count, desc, "unsorted");

  gsl_matrix_memcpy(A, m);

  gsl_eigen_symm(A, eval, w);

  /* sort eval and evalv */
  gsl_vector_memcpy(x, eval);
  gsl_vector_memcpy(y, evalv);
  gsl_sort_vector(x);
  gsl_sort_vector(y);
  test_eigenvalues_real(y, x, desc, "unsorted");

  gsl_eigen_symmv_sort(evalv, evec, GSL_EIGEN_SORT_VAL_ASC);
  test_eigen_symm_results(m, evalv, evec, count, desc, "val/asc");

  gsl_eigen_symmv_sort(evalv, evec, GSL_EIGEN_SORT_VAL_DESC);
  test_eigen_symm_results(m, evalv, evec, count, desc, "val/desc");

  gsl_eigen_symmv_sort(evalv, evec, GSL_EIGEN_SORT_ABS_ASC);
  test_eigen_symm_results(m, evalv, evec, count, desc, "abs/asc");

  gsl_eigen_symmv_sort(evalv, evec, GSL_EIGEN_SORT_ABS_DESC);
  test_eigen_symm_results(m, evalv, evec, count, desc, "abs/desc");

  gsl_matrix_free(A);
  gsl_vector_free(eval);
  gsl_vector_free(evalv);
  gsl_vector_free(x);
  gsl_vector_free(y);
  gsl_matrix_free(evec);
  gsl_eigen_symm_free(w);
  gsl_eigen_symmv_free(wv);
} /* test_eigen_symm_matrix() */

void
test_eigen_symm(void)
{
  size_t N_max = 20;
  size_t n, i;
  gsl_rng *r = gsl_rng_alloc(gsl_rng_default);

  for (n = 1; n <= N_max; ++n)
    {
      gsl_matrix * A = gsl_matrix_alloc(n, n);

      for (i = 0; i < 5; ++i)
        {
          create_random_symm_matrix(A, r, -10, 10);
          test_eigen_symm_matrix(A, i, "symm random");
        }

      gsl_matrix_free(A);
    }

  gsl_rng_free(r);

  {
    double dat1[] =  { 0,  0, -1,  0, 
                       0,  1,  0,  1,
                       -1,  0,  0,  0,
                       0,  1,  0,  0 };
    double dat2[] =  { 1,  0,  0,  0, 
                       0,  2,  0,  0,
                       0,  0,  3,  0,
                       0,  0,  0,  4 };
    gsl_matrix_view m;

    m = gsl_matrix_view_array (dat1, 4, 4);
    test_eigen_symm_matrix(&m.matrix, 0, "symm(4)");

    m = gsl_matrix_view_array (dat2, 4, 4);
    test_eigen_symm_matrix(&m.matrix, 0, "symm(4) diag");
  }

  { 
    double dat[27*27] = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
	0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,
	0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    };
        
    gsl_matrix_view m;
    m = gsl_matrix_view_array (dat, 27, 27);
    test_eigen_symm_matrix(&m.matrix, 0, "symm(27)");
  };

} /* test_eigen_symm() */

/******************************************
 * herm test code                         *
 ******************************************/

void
test_eigen_herm_results (const gsl_matrix_complex * A, 
                         const gsl_vector * eval, 
                         const gsl_matrix_complex * evec, 
                         size_t count,
                         const char * desc,
                         const char * desc2)
{
  const size_t N = A->size1;
  size_t i, j;

  gsl_vector_complex * x = gsl_vector_complex_alloc(N);
  gsl_vector_complex * y = gsl_vector_complex_alloc(N);

  /* check eigenvalues */

  for (i = 0; i < N; i++)
    {
      double ei = gsl_vector_get (eval, i);
      gsl_vector_complex_const_view vi =
        gsl_matrix_complex_const_column(evec, i);
      gsl_vector_complex_memcpy(x, &vi.vector);
      /* compute y = m x (should = lambda v) */
      gsl_blas_zgemv (CblasNoTrans, GSL_COMPLEX_ONE, A, x, 
                      GSL_COMPLEX_ZERO, y);
      for (j = 0; j < N; j++)
        {
          gsl_complex xj = gsl_vector_complex_get (x, j);
          gsl_complex yj = gsl_vector_complex_get (y, j);
          gsl_test_rel(GSL_REAL(yj), ei * GSL_REAL(xj), 1e8*GSL_DBL_EPSILON, 
                       "%s, eigenvalue(%d,%d), real, %s", desc, i, j, desc2);
          gsl_test_rel(GSL_IMAG(yj), ei * GSL_IMAG(xj), 1e8*GSL_DBL_EPSILON, 
                       "%s, eigenvalue(%d,%d), imag, %s", desc, i, j, desc2);
        }
    }

  /* check eigenvectors are orthonormal */

  for (i = 0; i < N; i++)
    {
      gsl_vector_complex_const_view vi = gsl_matrix_complex_const_column(evec, i);
      double nrm_v = gsl_blas_dznrm2(&vi.vector);
      gsl_test_rel (nrm_v, 1.0, N * GSL_DBL_EPSILON, "%s, normalized(%d), %s", 
                    desc, i, desc2);
    }

  for (i = 0; i < N; i++)
    {
      gsl_vector_complex_const_view vi = gsl_matrix_complex_const_column(evec, i);
      for (j = i + 1; j < N; j++)
        {
          gsl_vector_complex_const_view vj 
            = gsl_matrix_complex_const_column(evec, j);
          gsl_complex vivj;
          gsl_blas_zdotc (&vi.vector, &vj.vector, &vivj);
          gsl_test_abs (gsl_complex_abs(vivj), 0.0, 10.0 * N * GSL_DBL_EPSILON, 
                        "%s, orthogonal(%d,%d), %s", desc, i, j, desc2);
        }
    }

  gsl_vector_complex_free(x);
  gsl_vector_complex_free(y);
} /* test_eigen_herm_results() */

void
test_eigen_herm_matrix(const gsl_matrix_complex * m, size_t count,
                       const char * desc)
{
  const size_t N = m->size1;
  gsl_matrix_complex * A = gsl_matrix_complex_alloc(N, N);
  gsl_vector * eval = gsl_vector_alloc(N);
  gsl_vector * evalv = gsl_vector_alloc(N);
  gsl_vector * x = gsl_vector_alloc(N);
  gsl_vector * y = gsl_vector_alloc(N);
  gsl_matrix_complex * evec = gsl_matrix_complex_alloc(N, N);
  gsl_eigen_herm_workspace * w = gsl_eigen_herm_alloc(N);
  gsl_eigen_hermv_workspace * wv = gsl_eigen_hermv_alloc(N);

  gsl_matrix_complex_memcpy(A, m);

  gsl_eigen_hermv(A, evalv, evec, wv);
  test_eigen_herm_results(m, evalv, evec, count, desc, "unsorted");

  gsl_matrix_complex_memcpy(A, m);

  gsl_eigen_herm(A, eval, w);

  /* sort eval and evalv */
  gsl_vector_memcpy(x, eval);
  gsl_vector_memcpy(y, evalv);
  gsl_sort_vector(x);
  gsl_sort_vector(y);
  test_eigenvalues_real(y, x, desc, "unsorted");

  gsl_eigen_hermv_sort(evalv, evec, GSL_EIGEN_SORT_VAL_ASC);
  test_eigen_herm_results(m, evalv, evec, count, desc, "val/asc");

  gsl_eigen_hermv_sort(evalv, evec, GSL_EIGEN_SORT_VAL_DESC);
  test_eigen_herm_results(m, evalv, evec, count, desc, "val/desc");

  gsl_eigen_hermv_sort(evalv, evec, GSL_EIGEN_SORT_ABS_ASC);
  test_eigen_herm_results(m, evalv, evec, count, desc, "abs/asc");

  gsl_eigen_hermv_sort(evalv, evec, GSL_EIGEN_SORT_ABS_DESC);
  test_eigen_herm_results(m, evalv, evec, count, desc, "abs/desc");

  gsl_matrix_complex_free(A);
  gsl_vector_free(eval);
  gsl_vector_free(evalv);
  gsl_vector_free(x);
  gsl_vector_free(y);
  gsl_matrix_complex_free(evec);
  gsl_eigen_herm_free(w);
  gsl_eigen_hermv_free(wv);
} /* test_eigen_herm_matrix() */

void
test_eigen_herm(void)
{
  size_t N_max = 20;
  size_t n, i;
  gsl_rng *r = gsl_rng_alloc(gsl_rng_default);

  for (n = 1; n <= N_max; ++n)
    {
      gsl_matrix_complex * A = gsl_matrix_complex_alloc(n, n);

      for (i = 0; i < 5; ++i)
        {
          create_random_herm_matrix(A, r, -10, 10);
          test_eigen_herm_matrix(A, i, "herm random");
        }

      gsl_matrix_complex_free(A);
    }

  gsl_rng_free(r);

  {
    double dat1[] =  { 0,0,  0,0, -1,0,  0,0, 
                       0,0,  1,0,  0,0,  1,0,
                       -1,0,  0,0,  0,0,  0,0,
                       0,0,  1,0,  0,0,  0,0 };
    double dat2[] =  { 1,0,  0,0, 0,0,  0,0, 
                       0,0,  2,0, 0,0,  0,0,
                       0,0,  0,0, 3,0,  0,0,
                       0,0,  0,0, 0,0,  4,0 };
    gsl_matrix_complex_view m;
    
    m = gsl_matrix_complex_view_array (dat1, 4, 4);
    test_eigen_herm_matrix(&m.matrix, 0, "herm(4)");

    m = gsl_matrix_complex_view_array (dat2, 4, 4);
    test_eigen_herm_matrix(&m.matrix, 1, "herm(4) diag");
  }
} /* test_eigen_herm() */

/******************************************
 * nonsymm test code                      *
 ******************************************/

void
test_eigen_nonsymm_results (const gsl_matrix * m, 
                            const gsl_vector_complex * eval, 
                            const gsl_matrix_complex * evec, 
                            size_t count,
                            const char * desc,
                            const char * desc2)
{
  size_t i,j;
  size_t N = m->size1;

  gsl_vector_complex * x = gsl_vector_complex_alloc(N);
  gsl_vector_complex * y = gsl_vector_complex_alloc(N);
  gsl_matrix_complex * A = gsl_matrix_complex_alloc(N, N);

  /* we need a complex matrix for the blas routines, so copy m into A */
  for (i = 0; i < N; ++i)
    {
      for (j = 0; j < N; ++j)
        {
          gsl_complex z;
          GSL_SET_COMPLEX(&z, gsl_matrix_get(m, i, j), 0.0);
          gsl_matrix_complex_set(A, i, j, z);
        }
    }

  for (i = 0; i < N; i++)
    {
      gsl_complex ei = gsl_vector_complex_get (eval, i);
      gsl_vector_complex_const_view vi = gsl_matrix_complex_const_column(evec, i);
      double norm = gsl_blas_dznrm2(&vi.vector);

      /* check that eigenvector is normalized */
      gsl_test_rel(norm, 1.0, N * GSL_DBL_EPSILON,
                   "nonsymm(N=%u,cnt=%u), %s, normalized(%d), %s", N, count, desc, i, desc2);

      gsl_vector_complex_memcpy(x, &vi.vector);

      /* compute y = m x (should = lambda v) */
      gsl_blas_zgemv (CblasNoTrans, GSL_COMPLEX_ONE, A, x, 
                      GSL_COMPLEX_ZERO, y);

      /* compute x = lambda v */
      gsl_blas_zscal(ei, x);

      /* now test if y = x */
      for (j = 0; j < N; j++)
        {
          gsl_complex xj = gsl_vector_complex_get (x, j);
          gsl_complex yj = gsl_vector_complex_get (y, j);

          /* use abs here in case the values are close to 0 */
          gsl_test_abs(GSL_REAL(yj), GSL_REAL(xj), 1e8*GSL_DBL_EPSILON, 
                       "nonsymm(N=%u,cnt=%u), %s, eigenvalue(%d,%d), real, %s", N, count, desc, i, j, desc2);
          gsl_test_abs(GSL_IMAG(yj), GSL_IMAG(xj), 1e8*GSL_DBL_EPSILON, 
                       "nonsymm(N=%u,cnt=%u), %s, eigenvalue(%d,%d), imag, %s", N, count, desc, i, j, desc2);
        }
    }

  gsl_matrix_complex_free(A);
  gsl_vector_complex_free(x);
  gsl_vector_complex_free(y);
}

void
test_eigen_nonsymm_matrix(const gsl_matrix * m, size_t count,
                          const char * desc,
                          gsl_eigen_nonsymmv_workspace *w)
{
  const size_t N = m->size1;
  gsl_matrix * A = gsl_matrix_alloc(N, N);
  gsl_matrix * Z = gsl_matrix_alloc(N, N);
  gsl_matrix_complex * evec = gsl_matrix_complex_alloc(N, N);
  gsl_vector_complex * eval = gsl_vector_complex_alloc(N);

  /*
   * calculate eigenvalues and eigenvectors - it is sufficient to
   * test gsl_eigen_nonsymmv() since that function calls
   * gsl_eigen_nonsymm() for the eigenvalues
   */ 
  gsl_matrix_memcpy(A, m);
  gsl_eigen_nonsymmv(A, eval, evec, w);
  test_eigen_nonsymm_results (m, eval, evec, count, desc, "unsorted");

  /* test sort routines */
  gsl_eigen_nonsymmv_sort (eval, evec, GSL_EIGEN_SORT_ABS_ASC);
  test_eigen_nonsymm_results (m, eval, evec, count, desc, "abs/asc");

  gsl_eigen_nonsymmv_sort (eval, evec, GSL_EIGEN_SORT_ABS_DESC);
  test_eigen_nonsymm_results (m, eval, evec, count, desc, "abs/desc");

  /* test Schur vectors */
  gsl_matrix_memcpy(A, m);
  gsl_eigen_nonsymmv_Z(A, eval, evec, Z, w);
  gsl_linalg_hessenberg_set_zero(A);
  test_eigen_schur(m, A, Z, Z, count, "nonsymm", desc);

  /* test if Z is an orthogonal matrix */
  if (w->nonsymm_workspace_p->do_balance == 0)
    test_eigen_orthog(Z, count, "nonsymm", desc);

  gsl_matrix_free(A);
  gsl_matrix_free(Z);
  gsl_matrix_complex_free(evec);
  gsl_vector_complex_free(eval);
}

void
test_eigen_nonsymm(void)
{
  size_t N_max = 20;
  size_t n, i;
  gsl_rng *r = gsl_rng_alloc(gsl_rng_default);

  for (n = 1; n <= N_max; ++n)
    {
      gsl_matrix * m = gsl_matrix_alloc(n, n);
      gsl_eigen_nonsymmv_workspace * w = gsl_eigen_nonsymmv_alloc(n);

      for (i = 0; i < 5; ++i)
        {
          create_random_nonsymm_matrix(m, r, -10, 10);

          gsl_eigen_nonsymmv_params(0, w);
          test_eigen_nonsymm_matrix(m, i, "random, unbalanced", w);

          gsl_eigen_nonsymmv_params(1, w);
          test_eigen_nonsymm_matrix(m, i, "random, balanced", w);
        }

      gsl_matrix_free(m);
      gsl_eigen_nonsymmv_free(w);
    }

  gsl_rng_free(r);

  {
    double dat1[] = { 0, 1, 1, 1,
                      1, 1, 1, 1,
                      0, 0, 0, 0,
                      0, 0, 0, 0 };
    double dat2[] = { 1, 1, 0, 1,
                      1, 1, 1, 1,
                      1, 1, 1, 1,
                      0, 1, 0, 0 };
    gsl_matrix_view v;
    gsl_eigen_nonsymmv_workspace * w = gsl_eigen_nonsymmv_alloc(4);
    
    v = gsl_matrix_view_array (dat1, 4, 4);
    test_eigen_nonsymm_matrix(&v.matrix, 0, "integer", w);

    v = gsl_matrix_view_array (dat2, 4, 4);
    test_eigen_nonsymm_matrix(&v.matrix, 1, "integer", w);

    gsl_eigen_nonsymmv_free(w);
  }
} /* test_eigen_nonsymm() */

/******************************************
 * gensymm test code                      *
 ******************************************/

void
test_eigen_gensymm_results (const gsl_matrix * A, 
                            const gsl_matrix * B,
                            const gsl_vector * eval, 
                            const gsl_matrix * evec, 
                            size_t count,
                            const char * desc,
                            const char * desc2)
{
  const size_t N = A->size1;
  size_t i, j;

  gsl_vector * x = gsl_vector_alloc(N);
  gsl_vector * y = gsl_vector_alloc(N);
  gsl_vector * z = gsl_vector_alloc(N);

  /* check A v = lambda B v */
  for (i = 0; i < N; i++)
    {
      double ei = gsl_vector_get (eval, i);
      gsl_vector_const_view vi = gsl_matrix_const_column(evec, i);
      double norm = gsl_blas_dnrm2(&vi.vector);

      /* check that eigenvector is normalized */
      gsl_test_rel(norm, 1.0, N * GSL_DBL_EPSILON,
                   "gensymm(N=%u,cnt=%u), %s, normalized(%d), %s", N, count,
                   desc, i, desc2);

      gsl_vector_memcpy(z, &vi.vector);

      /* compute y = A z */
      gsl_blas_dgemv (CblasNoTrans, 1.0, A, z, 0.0, y);

      /* compute x = B z */
      gsl_blas_dgemv (CblasNoTrans, 1.0, B, z, 0.0, x);

      /* compute x = lambda B z */
      gsl_blas_dscal(ei, x);

      /* now test if y = x */
      for (j = 0; j < N; j++)
        {
          double xj = gsl_vector_get (x, j);
          double yj = gsl_vector_get (y, j);

          gsl_test_rel(yj, xj, 1e9 * GSL_DBL_EPSILON, 
                       "gensymm(N=%u,cnt=%u), %s, eigenvalue(%d,%d), real, %s", N, count, desc, i, j, desc2);
        }
    }

  gsl_vector_free(x);
  gsl_vector_free(y);
  gsl_vector_free(z);
}

void
test_eigen_gensymm(void)
{
  size_t N_max = 20;
  size_t n, i;
  gsl_rng *r = gsl_rng_alloc(gsl_rng_default);

  for (n = 1; n <= N_max; ++n)
    {
      gsl_matrix * A = gsl_matrix_alloc(n, n);
      gsl_matrix * B = gsl_matrix_alloc(n, n);
      gsl_matrix * ma = gsl_matrix_alloc(n, n);
      gsl_matrix * mb = gsl_matrix_alloc(n, n);
      gsl_vector * eval = gsl_vector_alloc(n);
      gsl_vector * evalv = gsl_vector_alloc(n);
      gsl_vector * x = gsl_vector_alloc(n);
      gsl_vector * y = gsl_vector_alloc(n);
      gsl_matrix * evec = gsl_matrix_alloc(n, n);
      gsl_eigen_gensymm_workspace * w = gsl_eigen_gensymm_alloc(n);
      gsl_eigen_gensymmv_workspace * wv = gsl_eigen_gensymmv_alloc(n);

      for (i = 0; i < 5; ++i)
        {
          create_random_symm_matrix(A, r, -10, 10);
          create_random_posdef_matrix(B, r);

          gsl_matrix_memcpy(ma, A);
          gsl_matrix_memcpy(mb, B);

          gsl_eigen_gensymmv(ma, mb, evalv, evec, wv);
          test_eigen_gensymm_results(A, B, evalv, evec, i, "random", "unsorted");

          gsl_matrix_memcpy(ma, A);
          gsl_matrix_memcpy(mb, B);

          gsl_eigen_gensymm(ma, mb, eval, w);

          /* eval and evalv have to be sorted? not sure why */
          gsl_vector_memcpy(x, eval);
          gsl_vector_memcpy(y, evalv);
          gsl_sort_vector(x);
          gsl_sort_vector(y);
          test_eigenvalues_real(y, x, "gensymm, random", "unsorted");

          gsl_eigen_gensymmv_sort(evalv, evec, GSL_EIGEN_SORT_VAL_ASC);
          test_eigen_gensymm_results(A, B, evalv, evec, i, "random", "val/asc");

          gsl_eigen_gensymmv_sort(evalv, evec, GSL_EIGEN_SORT_VAL_DESC);
          test_eigen_gensymm_results(A, B, evalv, evec, i, "random", "val/desc");

          gsl_eigen_gensymmv_sort(evalv, evec, GSL_EIGEN_SORT_ABS_ASC);
          test_eigen_gensymm_results(A, B, evalv, evec, i, "random", "abs/asc");
          gsl_eigen_gensymmv_sort(evalv, evec, GSL_EIGEN_SORT_ABS_DESC);
          test_eigen_gensymm_results(A, B, evalv, evec, i, "random", "abs/desc");
        }

      gsl_matrix_free(A);
      gsl_matrix_free(B);
      gsl_matrix_free(ma);
      gsl_matrix_free(mb);
      gsl_vector_free(eval);
      gsl_vector_free(evalv);
      gsl_vector_free(x);
      gsl_vector_free(y);
      gsl_matrix_free(evec);
      gsl_eigen_gensymm_free(w);
      gsl_eigen_gensymmv_free(wv);
    }

  gsl_rng_free(r);
} /* test_eigen_gensymm() */

/******************************************
 * genherm test code                      *
 ******************************************/

void
test_eigen_genherm_results (const gsl_matrix_complex * A, 
                            const gsl_matrix_complex * B,
                            const gsl_vector * eval, 
                            const gsl_matrix_complex * evec, 
                            size_t count,
                            const char * desc,
                            const char * desc2)
{
  const size_t N = A->size1;
  size_t i, j;

  gsl_vector_complex * x = gsl_vector_complex_alloc(N);
  gsl_vector_complex * y = gsl_vector_complex_alloc(N);

  /* check A v = lambda B v */
  for (i = 0; i < N; i++)
    {
      double ei = gsl_vector_get (eval, i);
      gsl_vector_complex_const_view vi =
        gsl_matrix_complex_const_column(evec, i);
      double norm = gsl_blas_dznrm2(&vi.vector);

      /* check that eigenvector is normalized */
      gsl_test_rel(norm, 1.0, N * GSL_DBL_EPSILON,
                   "genherm(N=%u,cnt=%u), %s, normalized(%d), %s", N, count,
                   desc, i, desc2);

      /* compute y = A z */
      gsl_blas_zgemv (CblasNoTrans, GSL_COMPLEX_ONE, A, &vi.vector, GSL_COMPLEX_ZERO, y);

      /* compute x = B z */
      gsl_blas_zgemv (CblasNoTrans, GSL_COMPLEX_ONE, B, &vi.vector, GSL_COMPLEX_ZERO, x);

      /* compute x = lambda B z */
      gsl_blas_zdscal(ei, x);

      /* now test if y = x */
      for (j = 0; j < N; j++)
        {
          gsl_complex xj = gsl_vector_complex_get (x, j);
          gsl_complex yj = gsl_vector_complex_get (y, j);

          gsl_test_rel(GSL_REAL(yj), GSL_REAL(xj), 1e9 * GSL_DBL_EPSILON, 
                       "genherm(N=%u,cnt=%u), %s, eigenvalue(%d,%d), real, %s", N, count, desc, i, j, desc2);
          gsl_test_abs(GSL_IMAG(yj), GSL_IMAG(xj), 1e9 * GSL_DBL_EPSILON, 
                       "genherm(N=%u,cnt=%u), %s, eigenvalue(%d,%d), imag, %s", N, count, desc, i, j, desc2);
        }
    }

  gsl_vector_complex_free(x);
  gsl_vector_complex_free(y);
}

void
test_eigen_genherm(void)
{
  size_t N_max = 20;
  size_t n, i;
  gsl_rng *r = gsl_rng_alloc(gsl_rng_default);

  for (n = 1; n <= N_max; ++n)
    {
      gsl_matrix_complex * A = gsl_matrix_complex_alloc(n, n);
      gsl_matrix_complex * B = gsl_matrix_complex_alloc(n, n);
      gsl_matrix_complex * ma = gsl_matrix_complex_alloc(n, n);
      gsl_matrix_complex * mb = gsl_matrix_complex_alloc(n, n);
      gsl_vector * eval = gsl_vector_alloc(n);
      gsl_vector * evalv = gsl_vector_alloc(n);
      gsl_vector * x = gsl_vector_alloc(n);
      gsl_vector * y = gsl_vector_alloc(n);
      gsl_vector_complex * work = gsl_vector_complex_alloc(n);
      gsl_matrix_complex * evec = gsl_matrix_complex_alloc(n, n);
      gsl_eigen_genherm_workspace * w = gsl_eigen_genherm_alloc(n);
      gsl_eigen_genhermv_workspace * wv = gsl_eigen_genhermv_alloc(n);

      for (i = 0; i < 5; ++i)
        {
          create_random_herm_matrix(A, r, -10, 10);
          create_random_complex_posdef_matrix(B, r, work);

          gsl_matrix_complex_memcpy(ma, A);
          gsl_matrix_complex_memcpy(mb, B);

          gsl_eigen_genhermv(ma, mb, evalv, evec, wv);
          test_eigen_genherm_results(A, B, evalv, evec, i, "random", "unsorted");

          gsl_matrix_complex_memcpy(ma, A);
          gsl_matrix_complex_memcpy(mb, B);

          gsl_eigen_genherm(ma, mb, eval, w);

          /* eval and evalv have to be sorted? not sure why */
          gsl_vector_memcpy(x, eval);
          gsl_vector_memcpy(y, evalv);
          gsl_sort_vector(x);
          gsl_sort_vector(y);
          test_eigenvalues_real(y, x, "genherm, random", "unsorted");

          gsl_eigen_genhermv_sort(evalv, evec, GSL_EIGEN_SORT_VAL_ASC);
          test_eigen_genherm_results(A, B, evalv, evec, i, "random", "val/asc");

          gsl_eigen_genhermv_sort(evalv, evec, GSL_EIGEN_SORT_VAL_DESC);
          test_eigen_genherm_results(A, B, evalv, evec, i, "random", "val/desc");

          gsl_eigen_genhermv_sort(evalv, evec, GSL_EIGEN_SORT_ABS_ASC);
          test_eigen_genherm_results(A, B, evalv, evec, i, "random", "abs/asc");
          gsl_eigen_genhermv_sort(evalv, evec, GSL_EIGEN_SORT_ABS_DESC);
          test_eigen_genherm_results(A, B, evalv, evec, i, "random", "abs/desc");
        }

      gsl_matrix_complex_free(A);
      gsl_matrix_complex_free(B);
      gsl_matrix_complex_free(ma);
      gsl_matrix_complex_free(mb);
      gsl_vector_free(eval);
      gsl_vector_free(evalv);
      gsl_vector_free(x);
      gsl_vector_free(y);
      gsl_vector_complex_free(work);
      gsl_matrix_complex_free(evec);
      gsl_eigen_genherm_free(w);
      gsl_eigen_genhermv_free(wv);
    }

  gsl_rng_free(r);
} /* test_eigen_genherm() */

/******************************************
 * gen test code                          *
 ******************************************/

typedef struct
{
  gsl_matrix *A;
  gsl_matrix *B;
  gsl_vector_complex *alpha;
  gsl_vector *beta;
  gsl_vector_complex *alphav;
  gsl_vector *betav;
  gsl_vector_complex *eval;
  gsl_vector_complex *evalv;
  gsl_vector *x;
  gsl_vector *y;
  gsl_matrix *Q;
  gsl_matrix *Z;
  gsl_matrix_complex *evec;
  gsl_eigen_gen_workspace *gen_p;
  gsl_eigen_genv_workspace *genv_p;
} test_eigen_gen_workspace;

test_eigen_gen_workspace *
test_eigen_gen_alloc(const size_t n)
{
  test_eigen_gen_workspace *w;

  w = (test_eigen_gen_workspace *) calloc(1, sizeof(test_eigen_gen_workspace));

  w->A = gsl_matrix_alloc(n, n);
  w->B = gsl_matrix_alloc(n, n);
  w->alpha = gsl_vector_complex_alloc(n);
  w->beta = gsl_vector_alloc(n);
  w->alphav = gsl_vector_complex_alloc(n);
  w->betav = gsl_vector_alloc(n);
  w->eval = gsl_vector_complex_alloc(n);
  w->evalv = gsl_vector_complex_alloc(n);
  w->x = gsl_vector_alloc(n);
  w->y = gsl_vector_alloc(n);
  w->Q = gsl_matrix_alloc(n, n);
  w->Z = gsl_matrix_alloc(n, n);
  w->evec = gsl_matrix_complex_alloc(n, n);
  w->gen_p = gsl_eigen_gen_alloc(n);
  w->genv_p = gsl_eigen_genv_alloc(n);

  return (w);
} /* test_eigen_gen_alloc() */

void
test_eigen_gen_free(test_eigen_gen_workspace *w)
{
  gsl_matrix_free(w->A);
  gsl_matrix_free(w->B);
  gsl_vector_complex_free(w->alpha);
  gsl_vector_free(w->beta);
  gsl_vector_complex_free(w->alphav);
  gsl_vector_free(w->betav);
  gsl_vector_complex_free(w->eval);
  gsl_vector_complex_free(w->evalv);
  gsl_vector_free(w->x);
  gsl_vector_free(w->y);
  gsl_matrix_free(w->Q);
  gsl_matrix_free(w->Z);
  gsl_matrix_complex_free(w->evec);
  gsl_eigen_gen_free(w->gen_p);
  gsl_eigen_genv_free(w->genv_p);
  free(w);
} /* test_eigen_gen_free() */

void
test_eigen_gen_results (const gsl_matrix * A, const gsl_matrix * B,
                        const gsl_vector_complex * alpha, 
                        const gsl_vector * beta,
                        const gsl_matrix_complex * evec, 
                        size_t count, const char * desc,
                        const char * desc2)
{
  const size_t N = A->size1;
  size_t i, j;
  gsl_matrix_complex *ma, *mb;
  gsl_vector_complex *x, *y;
  gsl_complex z_one, z_zero;

  ma = gsl_matrix_complex_alloc(N, N);
  mb = gsl_matrix_complex_alloc(N, N);
  y = gsl_vector_complex_alloc(N);
  x = gsl_vector_complex_alloc(N);

  /* ma <- A, mb <- B */
  for (i = 0; i < N; ++i)
    {
      for (j = 0; j < N; ++j)
        {
          gsl_complex z;

          GSL_SET_COMPLEX(&z, gsl_matrix_get(A, i, j), 0.0);
          gsl_matrix_complex_set(ma, i, j, z);

          GSL_SET_COMPLEX(&z, gsl_matrix_get(B, i, j), 0.0);
          gsl_matrix_complex_set(mb, i, j, z);
        }
    }

  GSL_SET_COMPLEX(&z_one, 1.0, 0.0);
  GSL_SET_COMPLEX(&z_zero, 0.0, 0.0);

  /* check eigenvalues */
  for (i = 0; i < N; ++i)
    {
      gsl_vector_complex_const_view vi =
        gsl_matrix_complex_const_column(evec, i);
      gsl_complex ai = gsl_vector_complex_get(alpha, i);
      double bi = gsl_vector_get(beta, i);

      /* compute x = alpha * B * v */
      gsl_blas_zgemv(CblasNoTrans, z_one, mb, &vi.vector, z_zero, x);
      gsl_blas_zscal(ai, x);

      /* compute y = beta * A v */
      gsl_blas_zgemv(CblasNoTrans, z_one, ma, &vi.vector, z_zero, y);
      gsl_blas_zdscal(bi, y);

      /* now test if y = x */
      for (j = 0; j < N; ++j)
        {
          gsl_complex xj = gsl_vector_complex_get(x, j);
          gsl_complex yj = gsl_vector_complex_get(y, j);

          gsl_test_abs(GSL_REAL(yj), GSL_REAL(xj), 1e8*GSL_DBL_EPSILON, 
                       "gen(N=%u,cnt=%u), %s, eigenvalue(%d,%d), real, %s",
                       N, count, desc, i, j, desc2);
          gsl_test_abs(GSL_IMAG(yj), GSL_IMAG(xj), 1e8*GSL_DBL_EPSILON, 
                       "gen(N=%u,cnt=%u), %s, eigenvalue(%d,%d), real, %s",
                       N, count, desc, i, j, desc2);
        }
    }

  gsl_matrix_complex_free(ma);
  gsl_matrix_complex_free(mb);
  gsl_vector_complex_free(y);
  gsl_vector_complex_free(x);
} /* test_eigen_gen_results() */

void
test_eigen_gen_pencil(const gsl_matrix * A, const gsl_matrix * B,
                      size_t count, const char * desc, int test_schur,
                      test_eigen_gen_workspace *w)
{
  const size_t N = A->size1;
  size_t i;

  gsl_matrix_memcpy(w->A, A);
  gsl_matrix_memcpy(w->B, B);

  if (test_schur)
    {
      gsl_eigen_genv_QZ(w->A, w->B, w->alphav, w->betav, w->evec, w->Q, w->Z, w->genv_p);
      test_eigen_schur(A, w->A, w->Q, w->Z, count, "genv/A", desc);
      test_eigen_schur(B, w->B, w->Q, w->Z, count, "genv/B", desc);
    }
  else
    gsl_eigen_genv(w->A, w->B, w->alphav, w->betav, w->evec, w->genv_p);

  test_eigen_gen_results(A, B, w->alphav, w->betav, w->evec, count, desc, "unsorted");

  gsl_matrix_memcpy(w->A, A);
  gsl_matrix_memcpy(w->B, B);

  if (test_schur)
    {
      gsl_eigen_gen_params(1, 1, 0, w->gen_p);
      gsl_eigen_gen_QZ(w->A, w->B, w->alpha, w->beta, w->Q, w->Z, w->gen_p);
      test_eigen_schur(A, w->A, w->Q, w->Z, count, "gen/A", desc);
      test_eigen_schur(B, w->B, w->Q, w->Z, count, "gen/B", desc);
    }
  else
    {
      gsl_eigen_gen_params(0, 0, 0, w->gen_p);
      gsl_eigen_gen(w->A, w->B, w->alpha, w->beta, w->gen_p);
    }

  /* compute eval = alpha / beta values */
  for (i = 0; i < N; ++i)
    {
      gsl_complex z, ai;
      double bi;

      ai = gsl_vector_complex_get(w->alpha, i);
      bi = gsl_vector_get(w->beta, i);
      GSL_SET_COMPLEX(&z, GSL_REAL(ai) / bi, GSL_IMAG(ai) / bi);
      gsl_vector_complex_set(w->eval, i, z);

      ai = gsl_vector_complex_get(w->alphav, i);
      bi = gsl_vector_get(w->betav, i);
      GSL_SET_COMPLEX(&z, GSL_REAL(ai) / bi, GSL_IMAG(ai) / bi);
      gsl_vector_complex_set(w->evalv, i, z);
    }

  /* sort eval and evalv and test them */
  gsl_eigen_nonsymmv_sort(w->eval, NULL, GSL_EIGEN_SORT_ABS_ASC);
  gsl_eigen_nonsymmv_sort(w->evalv, NULL, GSL_EIGEN_SORT_ABS_ASC);
  test_eigenvalues_complex(w->evalv, w->eval, "gen", desc);

  gsl_eigen_genv_sort(w->alphav, w->betav, w->evec, GSL_EIGEN_SORT_ABS_ASC);
  test_eigen_gen_results(A, B, w->alphav, w->betav, w->evec, count, desc, "abs/asc");
  gsl_eigen_genv_sort(w->alphav, w->betav, w->evec, GSL_EIGEN_SORT_ABS_DESC);
  test_eigen_gen_results(A, B, w->alphav, w->betav, w->evec, count, desc, "abs/desc");
} /* test_eigen_gen_pencil() */

void
test_eigen_gen(void)
{
  size_t N_max = 20;
  size_t n, i;
  gsl_rng *r = gsl_rng_alloc(gsl_rng_default);

  for (n = 1; n <= N_max; ++n)
    {
      gsl_matrix * A = gsl_matrix_alloc(n, n);
      gsl_matrix * B = gsl_matrix_alloc(n, n);
      test_eigen_gen_workspace * w = test_eigen_gen_alloc(n);

      for (i = 0; i < 5; ++i)
        {
          create_random_nonsymm_matrix(A, r, -10, 10);
          create_random_nonsymm_matrix(B, r, -10, 10);

          test_eigen_gen_pencil(A, B, i, "random", 0, w);
          test_eigen_gen_pencil(A, B, i, "random", 1, w);
        }

      gsl_matrix_free(A);
      gsl_matrix_free(B);
      test_eigen_gen_free(w);
    }

  gsl_rng_free(r);

  /* this system will test the exceptional shift code */
  {
    double datA[] = { 1, 1, 0,
                      0, 0, -1,
                      1, 0, 0 };
    double datB[] = { -1, 0, -1,
                      0, -1, 0,
                      0, 0, -1 };
    gsl_matrix_view va = gsl_matrix_view_array (datA, 3, 3);
    gsl_matrix_view vb = gsl_matrix_view_array (datB, 3, 3);
    test_eigen_gen_workspace * w = test_eigen_gen_alloc(3);
    
    test_eigen_gen_pencil(&va.matrix, &vb.matrix, 0, "integer", 0, w);
    test_eigen_gen_pencil(&va.matrix, &vb.matrix, 0, "integer", 1, w);

    test_eigen_gen_free(w);
  }
} /* test_eigen_gen() */

int
main()
{
  gsl_ieee_env_setup ();
  gsl_rng_env_setup ();

  test_eigen_symm();
  test_eigen_herm();
  test_eigen_nonsymm();
  test_eigen_gensymm();
  test_eigen_genherm();
  test_eigen_gen();

  exit (gsl_test_summary());
}
