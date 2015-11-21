/* linalg/test.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2004, 2005, 2006, 2007, 2010 Gerard Jungman, Brian Gough
 * Copyright (C) Huan Wu (test_choleskyc_invert and test_choleskyc_invert_dim)
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
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_permute_vector.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_complex_math.h>
#include <gsl/gsl_linalg.h>

#define TEST_SVD_4X4 1

int check (double x, double actual, double eps);
gsl_matrix * create_hilbert_matrix(unsigned long size);
gsl_matrix * create_general_matrix(unsigned long size1, unsigned long size2);
gsl_matrix * create_vandermonde_matrix(unsigned long size);
gsl_matrix * create_moler_matrix(unsigned long size);
gsl_matrix * create_row_matrix(unsigned long size1, unsigned long size2);
gsl_matrix * create_2x2_matrix(double a11, double a12, double a21, double a22);
gsl_matrix * create_diagonal_matrix(double a[], unsigned long size);
gsl_matrix * create_sparse_matrix(unsigned long m, unsigned long n);

int test_matmult(void);
int test_matmult_mod(void);
int test_LU_solve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_LU_solve(void);
int test_LUc_solve_dim(const gsl_matrix_complex * m, const double * actual, double eps);
int test_LUc_solve(void);
int test_QR_solve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_QR_solve(void);
int test_QR_QRsolve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_QR_QRsolve(void);
int test_QR_lssolve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_QR_lssolve(void);
int test_QR_decomp_dim(const gsl_matrix * m, double eps);
int test_QR_decomp(void);
int test_QRPT_solve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_QRPT_solve(void);
int test_QRPT_QRsolve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_QRPT_QRsolve(void);
int test_QRPT_decomp_dim(const gsl_matrix * m, double eps);
int test_QRPT_decomp(void);
int test_QR_update_dim(const gsl_matrix * m, double eps);
int test_QR_update(void);
int test_QRPT_update_dim(const gsl_matrix * m, double eps);
int test_QRPT_update(void);

int test_LQ_solve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_LQ_solve(void);
int test_LQ_LQsolve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_LQ_LQsolve(void);
int test_LQ_lssolve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_LQ_lssolve(void);
int test_LQ_decomp_dim(const gsl_matrix * m, double eps);
int test_LQ_decomp(void);
int test_PTLQ_solve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_PTLQ_solve(void);
int test_PTLQ_LQsolve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_PTLQ_LQsolve(void);
int test_PTLQ_decomp_dim(const gsl_matrix * m, double eps);
int test_PTLQ_decomp(void);
int test_LQ_update_dim(const gsl_matrix * m, double eps);
int test_LQ_update(void);

int test_SV_solve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_SV_solve(void);
int test_SV_decomp_dim(const gsl_matrix * m, double eps);
int test_SV_decomp(void);
int test_SV_decomp_mod_dim(const gsl_matrix * m, double eps);
int test_SV_decomp_mod(void);
int test_SV_decomp_jacobi_dim(const gsl_matrix * m, double eps);
int test_SV_decomp_jacobi(void);
int test_cholesky_solve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_cholesky_solve(void);
int test_cholesky_decomp_dim(const gsl_matrix * m, double eps);
int test_cholesky_decomp(void);
int test_cholesky_invert_dim(const gsl_matrix * m, double eps);
int test_cholesky_invert(void);
int test_HH_solve_dim(const gsl_matrix * m, const double * actual, double eps);
int test_HH_solve(void);
int test_TDS_solve_dim(unsigned long dim, double d, double od, const double * actual, double eps);
int test_TDS_solve(void);
int test_TDN_solve_dim(unsigned long dim, double d, double a, double b, const double * actual, double eps);
int test_TDN_solve(void);
int test_TDS_cyc_solve_one(const unsigned long dim, const double * d, const double * od, const double * r,
                          const double * actual, double eps);
int test_TDS_cyc_solve(void);
int test_TDN_cyc_solve_dim(unsigned long dim, double d, double a, double b, const double * actual, double eps);
int test_TDN_cyc_solve(void);
int test_bidiag_decomp_dim(const gsl_matrix * m, double eps);
int test_bidiag_decomp(void);

int 
check (double x, double actual, double eps)
{
  if (x == actual)
    {
      return 0;
    }
  else if (actual == 0)
    {
      return fabs(x) > eps;
    }
  else
    {
      return (fabs(x - actual)/fabs(actual)) > eps;
    }
}


gsl_vector * 
vector_alloc (size_t n)
{
  size_t p[5] = {3, 5, 7, 11, 13};
  static size_t k = 0;

  size_t stride = p[k];
  k = (k + 1) % 5;

  {
    gsl_block * b = gsl_block_alloc (n * stride);
    gsl_vector * v = gsl_vector_alloc_from_block (b, 0, n, stride);
    v->owner = 1;
    return v;
  }
}

void
vector_free (gsl_vector * v)
{
  gsl_vector_free (v);
}

gsl_matrix *
create_hilbert_matrix(unsigned long size)
{
  unsigned long i, j;
  gsl_matrix * m = gsl_matrix_alloc(size, size);
  for(i=0; i<size; i++) {
    for(j=0; j<size; j++) {
      gsl_matrix_set(m, i, j, 1.0/(i+j+1.0));
    }
  }
  return m;
}

gsl_matrix *
create_general_matrix(unsigned long size1, unsigned long size2)
{
  unsigned long i, j;
  gsl_matrix * m = gsl_matrix_alloc(size1, size2);
  for(i=0; i<size1; i++) {
    for(j=0; j<size2; j++) {
      gsl_matrix_set(m, i, j, 1.0/(i+j+1.0));
    }
  }
  return m;
}

gsl_matrix *
create_singular_matrix(unsigned long size1, unsigned long size2)
{
  unsigned long i, j;
  gsl_matrix * m = gsl_matrix_alloc(size1, size2);
  for(i=0; i<size1; i++) {
    for(j=0; j<size2; j++) {
      gsl_matrix_set(m, i, j, 1.0/(i+j+1.0));
    }
  }

  /* zero the first column */
  for(j = 0; j <size2; j++)
    gsl_matrix_set(m,0,j,0.0);

  return m;
}


gsl_matrix *
create_vandermonde_matrix(unsigned long size)
{
  unsigned long i, j;
  gsl_matrix * m = gsl_matrix_alloc(size, size);
  for(i=0; i<size; i++) {
    for(j=0; j<size; j++) {
      gsl_matrix_set(m, i, j, pow(i + 1.0, size - j - 1.0));
    }
  }
  return m;
}

gsl_matrix *
create_moler_matrix(unsigned long size)
{
  unsigned long i, j;
  gsl_matrix * m = gsl_matrix_alloc(size, size);
  for(i=0; i<size; i++) {
    for(j=0; j<size; j++) {
      gsl_matrix_set(m, i, j, GSL_MIN(i+1,j+1)-2.0);
    }
  }
  return m;
}

gsl_matrix_complex *
create_complex_matrix(unsigned long size)
{
  unsigned long i, j;
  gsl_matrix_complex * m = gsl_matrix_complex_alloc(size, size);
  for(i=0; i<size; i++) {
    for(j=0; j<size; j++) {
      gsl_complex z = gsl_complex_rect(1.0/(i+j+1.0), 1/(i*i+j*j+0.5));
      gsl_matrix_complex_set(m, i, j, z);
    }
  }
  return m;
}

gsl_matrix *
create_row_matrix(unsigned long size1, unsigned long size2)
{
  unsigned long i;
  gsl_matrix * m = gsl_matrix_calloc(size1, size2);
  for(i=0; i<size1; i++) {
      gsl_matrix_set(m, i, 0, 1.0/(i+1.0));
  }

  return m;
}

gsl_matrix *
create_2x2_matrix(double a11, double a12, double a21, double a22)
{
  gsl_matrix * m = gsl_matrix_alloc(2, 2);
  gsl_matrix_set(m, 0, 0, a11);
  gsl_matrix_set(m, 0, 1, a12);
  gsl_matrix_set(m, 1, 0, a21);
  gsl_matrix_set(m, 1, 1, a22);
  return m;
}

gsl_matrix *
create_diagonal_matrix(double a[], unsigned long size)
{
  unsigned long i;
  gsl_matrix * m = gsl_matrix_calloc(size, size);
  for(i=0; i<size; i++) {
      gsl_matrix_set(m, i, i, a[i]);
  }

  return m;
}

double rand_double() {
  static unsigned int x;
  x = (69069 * x + 1) & 0xFFFFFFFFUL;
  return (x  / 4294967296.0);
}

gsl_matrix *
create_sparse_matrix(unsigned long m, unsigned long n) {
  gsl_matrix* A = gsl_matrix_calloc(m, n);
  
  unsigned long int i, j;

  for (i = 0; i < m; i++) {
    for (j = 0; j < n; j++) {
      double a = (rand_double() < 0.6 ? 1 : 0);
      gsl_matrix_set(A, i, j, a);
    }
  }

  for (i = 0; i < m; i++) {
    if (rand_double() < 0.9) {
      gsl_vector_view row = gsl_matrix_row (A, i);
      gsl_vector_set_zero (&row.vector);
    }
  }
  for (i = 0; i < n; i++) {
    if (rand_double() < 0.43) {
      gsl_vector_view col = gsl_matrix_column (A, i);
      gsl_vector_set_zero (&col.vector);
    }
  }

  return A;
}



gsl_matrix * m11;
gsl_matrix * m51;

gsl_matrix * m35;
gsl_matrix * m53;
gsl_matrix * m97;

gsl_matrix * s35;
gsl_matrix * s53;

gsl_matrix * hilb2;
gsl_matrix * hilb3;
gsl_matrix * hilb4;
gsl_matrix * hilb12;

gsl_matrix * row3;
gsl_matrix * row5;
gsl_matrix * row12;

gsl_matrix * A22;
gsl_matrix * A33;
gsl_matrix * A44;
gsl_matrix * A55;

gsl_matrix_complex * c7;

gsl_matrix * inf5; double inf5_data[] = {1.0, 0.0, -3.0, 0.0, -5.0};
gsl_matrix * nan5;
gsl_matrix * dblmin3, * dblmin5, * dblsubnorm5;
gsl_matrix * bigsparse;

double m53_lssolution[] = {52.5992295702070, -337.7263113752073, 
                           351.8823436427604};
double hilb2_solution[] = {-8.0, 18.0} ;
double hilb3_solution[] = {27.0, -192.0, 210.0};
double hilb4_solution[] = {-64.0, 900.0, -2520.0, 1820.0};
double hilb12_solution[] = {-1728.0, 245388.0, -8528520.0, 
                            127026900.0, -1009008000.0, 4768571808.0, 
                            -14202796608.0, 27336497760.0, -33921201600.0,
                            26189163000.0, -11437874448.0, 2157916488.0 };

double c7_solution[] = { 2.40717272023734e+01, -9.84612797621247e+00,
                         -2.69338853034031e+02, 8.75455232472528e+01,
                         2.96661356736296e+03, -1.02624473923993e+03,
                         -1.82073812124749e+04, 5.67384473042410e+03,
                         5.57693879019068e+04, -1.61540963210502e+04,
                         -7.88941207561151e+04, 1.95053812987858e+04,
                         3.95548551241728e+04, -7.76593696255317e+03 };

gsl_matrix * vander2;
gsl_matrix * vander3;
gsl_matrix * vander4;
gsl_matrix * vander12;

double vander2_solution[] = {1.0, 0.0}; 
double vander3_solution[] = {0.0, 1.0, 0.0}; 
double vander4_solution[] = {0.0, 0.0, 1.0, 0.0}; 
double vander12_solution[] = {0.0, 0.0, 0.0, 0.0,
                            0.0, 0.0, 0.0, 0.0, 
                            0.0, 0.0, 1.0, 0.0}; 

gsl_matrix * moler10;

/* matmult now obsolete */
#ifdef MATMULT
int
test_matmult(void)
{
  int s = 0;

  gsl_matrix * A = gsl_matrix_calloc(2, 2);
  gsl_matrix * B = gsl_matrix_calloc(2, 3);
  gsl_matrix * C = gsl_matrix_calloc(2, 3);

  gsl_matrix_set(A, 0, 0, 10.0);
  gsl_matrix_set(A, 0, 1,  5.0);
  gsl_matrix_set(A, 1, 0,  1.0);
  gsl_matrix_set(A, 1, 1, 20.0);

  gsl_matrix_set(B, 0, 0, 10.0);
  gsl_matrix_set(B, 0, 1,  5.0);
  gsl_matrix_set(B, 0, 2,  2.0);
  gsl_matrix_set(B, 1, 0,  1.0);
  gsl_matrix_set(B, 1, 1,  3.0);
  gsl_matrix_set(B, 1, 2,  2.0);

  gsl_linalg_matmult(A, B, C);

  s += ( fabs(gsl_matrix_get(C, 0, 0) - 105.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 0, 1) -  65.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 0, 2) -  30.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 0) -  30.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 1) -  65.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 2) -  42.0) > GSL_DBL_EPSILON );

  gsl_matrix_free(A);
  gsl_matrix_free(B);
  gsl_matrix_free(C);

  return s;
}


int
test_matmult_mod(void)
{
  int s = 0;

  gsl_matrix * A = gsl_matrix_calloc(3, 3);
  gsl_matrix * B = gsl_matrix_calloc(3, 3);
  gsl_matrix * C = gsl_matrix_calloc(3, 3);
  gsl_matrix * D = gsl_matrix_calloc(2, 3);
  gsl_matrix * E = gsl_matrix_calloc(2, 3);
  gsl_matrix * F = gsl_matrix_calloc(2, 2);

  gsl_matrix_set(A, 0, 0, 10.0);
  gsl_matrix_set(A, 0, 1,  5.0);
  gsl_matrix_set(A, 0, 2,  1.0);
  gsl_matrix_set(A, 1, 0,  1.0);
  gsl_matrix_set(A, 1, 1, 20.0);
  gsl_matrix_set(A, 1, 2,  5.0);
  gsl_matrix_set(A, 2, 0,  1.0);
  gsl_matrix_set(A, 2, 1,  3.0);
  gsl_matrix_set(A, 2, 2,  7.0);

  gsl_matrix_set(B, 0, 0, 10.0);
  gsl_matrix_set(B, 0, 1,  5.0);
  gsl_matrix_set(B, 0, 2,  2.0);
  gsl_matrix_set(B, 1, 0,  1.0);
  gsl_matrix_set(B, 1, 1,  3.0);
  gsl_matrix_set(B, 1, 2,  2.0);
  gsl_matrix_set(B, 2, 0,  1.0);
  gsl_matrix_set(B, 2, 1,  3.0);
  gsl_matrix_set(B, 2, 2,  2.0);

  gsl_matrix_set(D, 0, 0, 10.0);
  gsl_matrix_set(D, 0, 1,  5.0);
  gsl_matrix_set(D, 0, 2,  1.0);
  gsl_matrix_set(D, 1, 0,  1.0);
  gsl_matrix_set(D, 1, 1, 20.0);
  gsl_matrix_set(D, 1, 2,  5.0);

  gsl_matrix_set(E, 0, 0, 10.0);
  gsl_matrix_set(E, 0, 1,  5.0);
  gsl_matrix_set(E, 0, 2,  2.0);
  gsl_matrix_set(E, 1, 0,  1.0);
  gsl_matrix_set(E, 1, 1,  3.0);
  gsl_matrix_set(E, 1, 2,  2.0);

  gsl_linalg_matmult_mod(A, GSL_LINALG_MOD_NONE, B, GSL_LINALG_MOD_NONE, C);
  s += ( fabs(gsl_matrix_get(C, 0, 0) - 106.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 0, 1) -  68.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 0, 2) -  32.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 0) -  35.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 1) -  80.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 2) -  52.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 0) -  20.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 1) -  35.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 2) -  22.0) > GSL_DBL_EPSILON );

  gsl_linalg_matmult_mod(A, GSL_LINALG_MOD_TRANSPOSE, B, GSL_LINALG_MOD_NONE, C);
  s += ( fabs(gsl_matrix_get(C, 0, 0) - 102.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 0, 1) -  56.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 0, 2) -  24.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 0) -  73.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 1) -  94.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 2) -  56.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 0) -  22.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 1) -  41.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 2) -  26.0) > GSL_DBL_EPSILON );

  gsl_linalg_matmult_mod(A, GSL_LINALG_MOD_NONE, B, GSL_LINALG_MOD_TRANSPOSE, C);
  s += ( fabs(gsl_matrix_get(C, 0, 0) - 127.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 0, 1) -  27.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 0, 2) -  27.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 0) - 120.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 1) -  71.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 2) -  71.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 0) -  39.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 1) -  24.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 2) -  24.0) > GSL_DBL_EPSILON );

  gsl_linalg_matmult_mod(A, GSL_LINALG_MOD_TRANSPOSE, B, GSL_LINALG_MOD_TRANSPOSE, C);
  s += ( fabs(gsl_matrix_get(C, 0, 0) - 107.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 0, 1) -  15.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 0, 2) -  15.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 0) - 156.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 1) -  71.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 2) -  71.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 0) -  49.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 1) -  30.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 2) -  30.0) > GSL_DBL_EPSILON );

  /* now try for non-symmetric matrices */
  gsl_linalg_matmult_mod(D, GSL_LINALG_MOD_TRANSPOSE, E, GSL_LINALG_MOD_NONE, C);
  s += ( fabs(gsl_matrix_get(C, 0, 0) - 101.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 0, 1) -  53.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 0, 2) -  22.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 0) -  70.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 1) -  85.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 1, 2) -  50.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 0) -  15.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 1) -  20.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(C, 2, 2) -  12.0) > GSL_DBL_EPSILON );


  gsl_linalg_matmult_mod(D, GSL_LINALG_MOD_NONE, E, GSL_LINALG_MOD_TRANSPOSE, F);
  s += ( fabs(gsl_matrix_get(F, 0, 0) - 127.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(F, 0, 1) -  27.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(F, 1, 0) - 120.0) > GSL_DBL_EPSILON );
  s += ( fabs(gsl_matrix_get(F, 1, 1) -  71.0) > GSL_DBL_EPSILON );


  gsl_matrix_free(A);
  gsl_matrix_free(B);
  gsl_matrix_free(C);
  gsl_matrix_free(D);
  gsl_matrix_free(E);
  gsl_matrix_free(F);

  return s;
}
#endif

int
test_LU_solve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  int signum;
  unsigned long i, dim = m->size1;

  gsl_permutation * perm = gsl_permutation_alloc(dim);
  gsl_vector * rhs = gsl_vector_alloc(dim);
  gsl_matrix * lu  = gsl_matrix_alloc(dim,dim);
  gsl_vector * x = gsl_vector_alloc(dim);
  gsl_vector * residual = gsl_vector_alloc(dim);
  gsl_matrix_memcpy(lu,m);
  for(i=0; i<dim; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_LU_decomp(lu, perm, &signum);
  s += gsl_linalg_LU_solve(lu, perm, rhs, x);

  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i),actual[i],eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  s += gsl_linalg_LU_refine(m, lu, perm, rhs, x, residual);

  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i),actual[i],eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g (improved)\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  gsl_vector_free(residual);
  gsl_vector_free(x);
  gsl_matrix_free(lu);
  gsl_vector_free(rhs);
  gsl_permutation_free(perm);

  return s;
}


int test_LU_solve(void)
{
  int f;
  int s = 0;

  f = test_LU_solve_dim(hilb2, hilb2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LU_solve hilbert(2)");
  s += f;

  f = test_LU_solve_dim(hilb3, hilb3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LU_solve hilbert(3)");
  s += f;

  f = test_LU_solve_dim(hilb4, hilb4_solution, 2048.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LU_solve hilbert(4)");
  s += f;

  f = test_LU_solve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  LU_solve hilbert(12)");
  s += f;

  f = test_LU_solve_dim(vander2, vander2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LU_solve vander(2)");
  s += f;

  f = test_LU_solve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LU_solve vander(3)");
  s += f;

  f = test_LU_solve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LU_solve vander(4)");
  s += f;

  f = test_LU_solve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  LU_solve vander(12)");
  s += f;

  return s;
}


int
test_LUc_solve_dim(const gsl_matrix_complex * m, const double * actual, double eps)
{
  int s = 0;
  int signum;
  unsigned long i, dim = m->size1;

  gsl_permutation * perm = gsl_permutation_alloc(dim);
  gsl_vector_complex * rhs = gsl_vector_complex_alloc(dim);
  gsl_matrix_complex * lu  = gsl_matrix_complex_alloc(dim,dim);
  gsl_vector_complex * x = gsl_vector_complex_alloc(dim);
  gsl_vector_complex * residual = gsl_vector_complex_alloc(dim);
  gsl_matrix_complex_memcpy(lu,m);
  for(i=0; i<dim; i++) 
    {
      gsl_complex z = gsl_complex_rect (2.0*i+1.0, 2.0*i+2.0);
      gsl_vector_complex_set(rhs, i, z);
    }
  s += gsl_linalg_complex_LU_decomp(lu, perm, &signum);
  s += gsl_linalg_complex_LU_solve(lu, perm, rhs, x);

  for(i=0; i<dim; i++) {
    gsl_complex z = gsl_vector_complex_get(x, i);
    int foo_r = check(GSL_REAL(z),actual[2*i],eps);
    int foo_i = check(GSL_IMAG(z),actual[2*i+1],eps);
    if(foo_r || foo_i) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, GSL_REAL(z), actual[2*i]);
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, GSL_IMAG(z), actual[2*i+1]);
    }
    s += foo_r + foo_i;
  }

  s += gsl_linalg_complex_LU_refine(m, lu, perm, rhs, x, residual);

  for(i=0; i<dim; i++) {
    gsl_complex z = gsl_vector_complex_get(x, i);
    int foo_r = check(GSL_REAL(z),actual[2*i],eps);
    int foo_i = check(GSL_IMAG(z),actual[2*i+1],eps);
    if(foo_r || foo_i) {
      printf("%3lu[%lu]: %22.18g   %22.18g (improved)\n", dim, i, GSL_REAL(z), actual[2*i]);
      printf("%3lu[%lu]: %22.18g   %22.18g (improved)\n", dim, i, GSL_IMAG(z), actual[2*i+1]);
    }
    s += foo_r + foo_i;
  }

  gsl_vector_complex_free(residual);
  gsl_vector_complex_free(x);
  gsl_matrix_complex_free(lu);
  gsl_vector_complex_free(rhs);
  gsl_permutation_free(perm);

  return s;
}


int test_LUc_solve(void)
{
  int f;
  int s = 0;

  f = test_LUc_solve_dim(c7, c7_solution, 1024.0 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  complex_LU_solve complex(7)");
  s += f;

  return s;
}


int
test_QR_solve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  unsigned long i, dim = m->size1;

  gsl_vector * rhs = gsl_vector_alloc(dim);
  gsl_matrix * qr  = gsl_matrix_alloc(dim,dim);
  gsl_vector * d = gsl_vector_alloc(dim);
  gsl_vector * x = gsl_vector_alloc(dim);

  gsl_matrix_memcpy(qr,m);
  for(i=0; i<dim; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_QR_decomp(qr, d);
  s += gsl_linalg_QR_solve(qr, d, rhs, x);
  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i), actual[i], eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  gsl_vector_free(x);
  gsl_vector_free(d);
  gsl_matrix_free(qr);
  gsl_vector_free(rhs);

  return s;
}

int test_QR_solve(void)
{
  int f;
  int s = 0;

  f = test_QR_solve_dim(hilb2, hilb2_solution, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_solve hilbert(2)");
  s += f;

  f = test_QR_solve_dim(hilb3, hilb3_solution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_solve hilbert(3)");
  s += f;

  f = test_QR_solve_dim(hilb4, hilb4_solution, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_solve hilbert(4)");
  s += f;

  f = test_QR_solve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  QR_solve hilbert(12)");
  s += f;

  f = test_QR_solve_dim(vander2, vander2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_solve vander(2)");
  s += f;

  f = test_QR_solve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_solve vander(3)");
  s += f;

  f = test_QR_solve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_solve vander(4)");
  s += f;

  f = test_QR_solve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  QR_solve vander(12)");
  s += f;

  return s;
}


int
test_QR_QRsolve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  unsigned long i, dim = m->size1;

  gsl_vector * rhs = gsl_vector_alloc(dim);
  gsl_matrix * qr  = gsl_matrix_alloc(dim,dim);
  gsl_matrix * q  = gsl_matrix_alloc(dim,dim);
  gsl_matrix * r  = gsl_matrix_alloc(dim,dim);
  gsl_vector * d = gsl_vector_alloc(dim);
  gsl_vector * x = gsl_vector_alloc(dim);

  gsl_matrix_memcpy(qr,m);
  for(i=0; i<dim; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_QR_decomp(qr, d);
  s += gsl_linalg_QR_unpack(qr, d, q, r);
  s += gsl_linalg_QR_QRsolve(q, r, rhs, x);
  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i), actual[i], eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  gsl_vector_free(x);
  gsl_vector_free(d);
  gsl_matrix_free(qr);
  gsl_matrix_free(q);
  gsl_matrix_free(r);
  gsl_vector_free(rhs);
  return s;
}

int test_QR_QRsolve(void)
{
  int f;
  int s = 0;

  f = test_QR_QRsolve_dim(hilb2, hilb2_solution, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_QRsolve hilbert(2)");
  s += f;

  f = test_QR_QRsolve_dim(hilb3, hilb3_solution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_QRsolve hilbert(3)");
  s += f;

  f = test_QR_QRsolve_dim(hilb4, hilb4_solution, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_QRsolve hilbert(4)");
  s += f;

  f = test_QR_QRsolve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  QR_QRsolve hilbert(12)");
  s += f;

  f = test_QR_QRsolve_dim(vander2, vander2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_QRsolve vander(2)");
  s += f;

  f = test_QR_QRsolve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_QRsolve vander(3)");
  s += f;

  f = test_QR_QRsolve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_QRsolve vander(4)");
  s += f;

  f = test_QR_QRsolve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  QR_QRsolve vander(12)");
  s += f;

  return s;
}


int
test_QR_lssolve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  unsigned long i, M = m->size1, N = m->size2;

  gsl_vector * rhs = gsl_vector_alloc(M);
  gsl_matrix * qr  = gsl_matrix_alloc(M,N);
  gsl_vector * d = gsl_vector_alloc(N);
  gsl_vector * x = gsl_vector_alloc(N);
  gsl_vector * r = gsl_vector_alloc(M);
  gsl_vector * res = gsl_vector_alloc(M);

  gsl_matrix_memcpy(qr,m);
  for(i=0; i<M; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_QR_decomp(qr, d);
  s += gsl_linalg_QR_lssolve(qr, d, rhs, x, res);

  for(i=0; i<N; i++) {
    int foo = check(gsl_vector_get(x, i), actual[i], eps);
    if(foo) {
      printf("(%3lu,%3lu)[%lu]: %22.18g   %22.18g\n", M, N, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  /* compute residual r = b - m x */
  if (M == N) {
    gsl_vector_set_zero(r);
  } else {
    gsl_vector_memcpy(r, rhs);
    gsl_blas_dgemv(CblasNoTrans, -1.0, m, x, 1.0, r);
  };

  for(i=0; i<N; i++) {
    int foo = check(gsl_vector_get(res, i), gsl_vector_get(r,i), sqrt(eps));
    if(foo) {
      printf("(%3lu,%3lu)[%lu]: %22.18g   %22.18g\n", M, N, i, gsl_vector_get(res, i), gsl_vector_get(r,i));
    }
    s += foo;
  }

  gsl_vector_free(r);
  gsl_vector_free(res);
  gsl_vector_free(x);
  gsl_vector_free(d);
  gsl_matrix_free(qr);
  gsl_vector_free(rhs);

  return s;
}

int test_QR_lssolve(void)
{
  int f;
  int s = 0;

  f = test_QR_lssolve_dim(m53, m53_lssolution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_lssolve m(5,3)");
  s += f;

  f = test_QR_lssolve_dim(hilb2, hilb2_solution, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_lssolve hilbert(2)");
  s += f;

  f = test_QR_lssolve_dim(hilb3, hilb3_solution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_lssolve hilbert(3)");
  s += f;

  f = test_QR_lssolve_dim(hilb4, hilb4_solution, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_lssolve hilbert(4)");
  s += f;

  f = test_QR_lssolve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  QR_lssolve hilbert(12)");
  s += f;

  f = test_QR_lssolve_dim(vander2, vander2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_lssolve vander(2)");
  s += f;

  f = test_QR_lssolve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_lssolve vander(3)");
  s += f;

  f = test_QR_lssolve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_lssolve vander(4)");
  s += f;

  f = test_QR_lssolve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  QR_lssolve vander(12)");
  s += f;

  return s;
}


int
test_QR_decomp_dim(const gsl_matrix * m, double eps)
{
  int s = 0;
  unsigned long i,j, M = m->size1, N = m->size2;

  gsl_matrix * qr = gsl_matrix_alloc(M,N);
  gsl_matrix * a  = gsl_matrix_alloc(M,N);
  gsl_matrix * q  = gsl_matrix_alloc(M,M);
  gsl_matrix * r  = gsl_matrix_alloc(M,N);
  gsl_vector * d = gsl_vector_alloc(GSL_MIN(M,N));

  gsl_matrix_memcpy(qr,m);

  s += gsl_linalg_QR_decomp(qr, d);
  s += gsl_linalg_QR_unpack(qr, d, q, r);
  
  /* compute a = q r */
  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, q, r, 0.0, a);

  for(i=0; i<M; i++) {
    for(j=0; j<N; j++) {
      double aij = gsl_matrix_get(a, i, j);
      double mij = gsl_matrix_get(m, i, j);
      int foo = check(aij, mij, eps);
      if(foo) {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, aij, mij);
      }
      s += foo;
    }
  }

  gsl_vector_free(d);
  gsl_matrix_free(qr);
  gsl_matrix_free(a);
  gsl_matrix_free(q);
  gsl_matrix_free(r);

  return s;
}

int test_QR_decomp(void)
{
  int f;
  int s = 0;

  f = test_QR_decomp_dim(m35, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_decomp m(3,5)");
  s += f;

  f = test_QR_decomp_dim(m53, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_decomp m(5,3)");
  s += f;

  f = test_QR_decomp_dim(hilb2, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_decomp hilbert(2)");
  s += f;

  f = test_QR_decomp_dim(hilb3, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_decomp hilbert(3)");
  s += f;

  f = test_QR_decomp_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_decomp hilbert(4)");
  s += f;

  f = test_QR_decomp_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_decomp hilbert(12)");
  s += f;

  f = test_QR_decomp_dim(vander2, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_decomp vander(2)");
  s += f;

  f = test_QR_decomp_dim(vander3, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_decomp vander(3)");
  s += f;

  f = test_QR_decomp_dim(vander4, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_decomp vander(4)");
  s += f;

  f = test_QR_decomp_dim(vander12, 0.0005); /* FIXME: bad accuracy */
  gsl_test(f, "  QR_decomp vander(12)");
  s += f;

  return s;
}

int
test_QRPT_solve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  int signum;
  unsigned long i, dim = m->size1;

  gsl_permutation * perm = gsl_permutation_alloc(dim);
  gsl_vector * rhs = gsl_vector_alloc(dim);
  gsl_matrix * qr  = gsl_matrix_alloc(dim,dim);
  gsl_vector * d = gsl_vector_alloc(dim);
  gsl_vector * x = gsl_vector_alloc(dim);
  gsl_vector * norm = gsl_vector_alloc(dim);

  gsl_matrix_memcpy(qr,m);
  for(i=0; i<dim; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_QRPT_decomp(qr, d, perm, &signum, norm);
  s += gsl_linalg_QRPT_solve(qr, d, perm, rhs, x);
  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i), actual[i], eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  gsl_vector_free(norm);
  gsl_vector_free(x);
  gsl_vector_free(d);
  gsl_matrix_free(qr);
  gsl_vector_free(rhs);
  gsl_permutation_free(perm);

  return s;
}

int test_QRPT_solve(void)
{
  int f;
  int s = 0;

  f = test_QRPT_solve_dim(hilb2, hilb2_solution, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_solve hilbert(2)");
  s += f;

  f = test_QRPT_solve_dim(hilb3, hilb3_solution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_solve hilbert(3)");
  s += f;

  f = test_QRPT_solve_dim(hilb4, hilb4_solution, 2 * 2048.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_solve hilbert(4)");
  s += f;

  f = test_QRPT_solve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  QRPT_solve hilbert(12)");
  s += f;

  f = test_QRPT_solve_dim(vander2, vander2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_solve vander(2)");
  s += f;

  f = test_QRPT_solve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_solve vander(3)");
  s += f;

  f = test_QRPT_solve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_solve vander(4)");
  s += f;

  f = test_QRPT_solve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  QRPT_solve vander(12)");
  s += f;

  return s;
}

int
test_QRPT_QRsolve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  int signum;
  unsigned long i, dim = m->size1;

  gsl_permutation * perm = gsl_permutation_alloc(dim);
  gsl_vector * rhs = gsl_vector_alloc(dim);
  gsl_matrix * qr  = gsl_matrix_alloc(dim,dim);
  gsl_matrix * q  = gsl_matrix_alloc(dim,dim);
  gsl_matrix * r  = gsl_matrix_alloc(dim,dim);
  gsl_vector * d = gsl_vector_alloc(dim);
  gsl_vector * x = gsl_vector_alloc(dim);
  gsl_vector * norm = gsl_vector_alloc(dim);

  gsl_matrix_memcpy(qr,m);
  for(i=0; i<dim; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_QRPT_decomp2(qr, q, r, d, perm, &signum, norm);
  s += gsl_linalg_QRPT_QRsolve(q, r, perm, rhs, x);
  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i), actual[i], eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  gsl_vector_free(norm);
  gsl_vector_free(x);
  gsl_vector_free(d);
  gsl_matrix_free(qr);
  gsl_matrix_free(q);
  gsl_matrix_free(r);
  gsl_vector_free(rhs);
  gsl_permutation_free(perm);

  return s;
}

int test_QRPT_QRsolve(void)
{
  int f;
  int s = 0;

  f = test_QRPT_QRsolve_dim(hilb2, hilb2_solution, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_QRsolve hilbert(2)");
  s += f;

  f = test_QRPT_QRsolve_dim(hilb3, hilb3_solution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_QRsolve hilbert(3)");
  s += f;

  f = test_QRPT_QRsolve_dim(hilb4, hilb4_solution, 2 * 2048.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_QRsolve hilbert(4)");
  s += f;

  f = test_QRPT_QRsolve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  QRPT_QRsolve hilbert(12)");
  s += f;

  f = test_QRPT_QRsolve_dim(vander2, vander2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_QRsolve vander(2)");
  s += f;

  f = test_QRPT_QRsolve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_QRsolve vander(3)");
  s += f;

  f = test_QRPT_QRsolve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_QRsolve vander(4)");
  s += f;

  f = test_QRPT_QRsolve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  QRPT_QRsolve vander(12)");
  s += f;

  return s;
}

int
test_QRPT_decomp_dim(const gsl_matrix * m, double eps)
{
  int s = 0, signum;
  unsigned long i,j, M = m->size1, N = m->size2;

  gsl_matrix * qr = gsl_matrix_alloc(M,N);
  gsl_matrix * a  = gsl_matrix_alloc(M,N);
  gsl_matrix * q  = gsl_matrix_alloc(M,M);
  gsl_matrix * r  = gsl_matrix_alloc(M,N);
  gsl_vector * d = gsl_vector_alloc(GSL_MIN(M,N));
  gsl_vector * norm = gsl_vector_alloc(N);

  gsl_permutation * perm = gsl_permutation_alloc(N);

  gsl_matrix_memcpy(qr,m);

  s += gsl_linalg_QRPT_decomp(qr, d, perm, &signum, norm);
  s += gsl_linalg_QR_unpack(qr, d, q, r);

  /* compute a = q r */
  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, q, r, 0.0, a);


  /* Compute QR P^T by permuting the elements of the rows of QR */

  for (i = 0; i < M; i++) {
    gsl_vector_view row = gsl_matrix_row (a, i);
    gsl_permute_vector_inverse (perm, &row.vector);
  }

  for(i=0; i<M; i++) {
    for(j=0; j<N; j++) {
      double aij = gsl_matrix_get(a, i, j);
      double mij = gsl_matrix_get(m, i, j);
      int foo = check(aij, mij, eps);
      if(foo) {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, aij, mij);
      }
      s += foo;
    }
  }

  gsl_permutation_free (perm);
  gsl_vector_free(norm);
  gsl_vector_free(d);
  gsl_matrix_free(qr);
  gsl_matrix_free(a);
  gsl_matrix_free(q);
  gsl_matrix_free(r);

  return s;
}

int test_QRPT_decomp(void)
{
  int f;
  int s = 0;

  f = test_QRPT_decomp_dim(m35, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_decomp m(3,5)");
  s += f;

  f = test_QRPT_decomp_dim(m53, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_decomp m(5,3)");
  s += f;

  f = test_QRPT_decomp_dim(s35, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_decomp s(3,5)");
  s += f;

  f = test_QRPT_decomp_dim(s53, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_decomp s(5,3)");
  s += f;

  f = test_QRPT_decomp_dim(hilb2, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_decomp hilbert(2)");
  s += f;

  f = test_QRPT_decomp_dim(hilb3, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_decomp hilbert(3)");
  s += f;

  f = test_QRPT_decomp_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_decomp hilbert(4)");
  s += f;

  f = test_QRPT_decomp_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_decomp hilbert(12)");
  s += f;

  f = test_QRPT_decomp_dim(vander2, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_decomp vander(2)");
  s += f;

  f = test_QRPT_decomp_dim(vander3, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_decomp vander(3)");
  s += f;

  f = test_QRPT_decomp_dim(vander4, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_decomp vander(4)");
  s += f;

  f = test_QRPT_decomp_dim(vander12, 0.0005); /* FIXME: bad accuracy */
  gsl_test(f, "  QRPT_decomp vander(12)");
  s += f;

  return s;
}


int
test_QR_update_dim(const gsl_matrix * m, double eps)
{
  int s = 0;
  unsigned long i,j,k, M = m->size1, N = m->size2;

  gsl_matrix * qr1  = gsl_matrix_alloc(M,N);
  gsl_matrix * qr2  = gsl_matrix_alloc(M,N);
  gsl_matrix * q1  = gsl_matrix_alloc(M,M);
  gsl_matrix * r1  = gsl_matrix_alloc(M,N);
  gsl_matrix * q2  = gsl_matrix_alloc(M,M);
  gsl_matrix * r2  = gsl_matrix_alloc(M,N);
  gsl_vector * d = gsl_vector_alloc(GSL_MIN(M,N));
  gsl_vector * solution1 = gsl_vector_alloc(N);
  gsl_vector * solution2 = gsl_vector_alloc(N);
  gsl_vector * u = gsl_vector_alloc(M);
  gsl_vector * v = gsl_vector_alloc(N);
  gsl_vector * w = gsl_vector_alloc(M);

  gsl_matrix_memcpy(qr1,m);
  gsl_matrix_memcpy(qr2,m);

  for(i=0; i<M; i++) gsl_vector_set(u, i, sin(i+1.0));
  for(i=0; i<N; i++) gsl_vector_set(v, i, cos(i+2.0) + sin(i*i+3.0));

  for(i=0; i<M; i++) 
    {
      double ui = gsl_vector_get(u, i);
      for(j=0; j<N; j++) 
        {
          double vj = gsl_vector_get(v, j);
          double qij = gsl_matrix_get(qr1, i, j);
          gsl_matrix_set(qr1, i, j, qij + ui * vj);
        }
    }

  s += gsl_linalg_QR_decomp(qr2, d);
  s += gsl_linalg_QR_unpack(qr2, d, q2, r2);

  /* compute w = Q^T u */
      
  for (j = 0; j < M; j++)
    {
      double sum = 0;
      for (i = 0; i < M; i++)
          sum += gsl_matrix_get (q2, i, j) * gsl_vector_get (u, i);
      gsl_vector_set (w, j, sum);
    }

  s += gsl_linalg_QR_update(q2, r2, w, v);

  /* compute qr2 = q2 * r2 */

  for (i = 0; i < M; i++)
    {
      for (j = 0; j< N; j++)
        {
          double sum = 0;
          for (k = 0; k <= GSL_MIN(j,M-1); k++)
            {
              double qik = gsl_matrix_get(q2, i, k);
              double rkj = gsl_matrix_get(r2, k, j);
              sum += qik * rkj ;
            }
          gsl_matrix_set (qr2, i, j, sum);
        }
    }

  for(i=0; i<M; i++) {
    for(j=0; j<N; j++) {
      double s1 = gsl_matrix_get(qr1, i, j);
      double s2 = gsl_matrix_get(qr2, i, j);
      
      int foo = check(s1, s2, eps);
      if(foo) {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, s1, s2);
      }
      s += foo;
    }
  }

  gsl_vector_free(solution1);
  gsl_vector_free(solution2);
  gsl_vector_free(d);
  gsl_vector_free(u);
  gsl_vector_free(v);
  gsl_vector_free(w);
  gsl_matrix_free(qr1);
  gsl_matrix_free(qr2);
  gsl_matrix_free(q1);
  gsl_matrix_free(r1);
  gsl_matrix_free(q2);
  gsl_matrix_free(r2);

  return s;
}

int test_QR_update(void)
{
  int f;
  int s = 0;

  f = test_QR_update_dim(m35, 2 * 512.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_update m(3,5)");
  s += f;

  f = test_QR_update_dim(m53, 2 * 512.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_update m(5,3)");
  s += f;

  f = test_QR_update_dim(hilb2,  2 * 512.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_update hilbert(2)");
  s += f;

  f = test_QR_update_dim(hilb3,  2 * 512.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_update hilbert(3)");
  s += f;

  f = test_QR_update_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_update hilbert(4)");
  s += f;

  f = test_QR_update_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_update hilbert(12)");
  s += f;

  f = test_QR_update_dim(vander2, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_update vander(2)");
  s += f;

  f = test_QR_update_dim(vander3, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_update vander(3)");
  s += f;

  f = test_QR_update_dim(vander4, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QR_update vander(4)");
  s += f;

  f = test_QR_update_dim(vander12, 0.0005); /* FIXME: bad accuracy */
  gsl_test(f, "  QR_update vander(12)");
  s += f;

  return s;
}


int
test_QRPT_update_dim(const gsl_matrix * m, double eps)
{
  int s = 0, signum;
  unsigned long i,j,k, M = m->size1, N = m->size2;

  gsl_matrix * qr1  = gsl_matrix_alloc(M,N);
  gsl_matrix * qr2  = gsl_matrix_alloc(M,N);
  gsl_matrix * q1  = gsl_matrix_alloc(M,M);
  gsl_matrix * r1  = gsl_matrix_alloc(M,N);
  gsl_matrix * q2  = gsl_matrix_alloc(M,M);
  gsl_matrix * r2  = gsl_matrix_alloc(M,N);
  gsl_vector * d = gsl_vector_alloc(GSL_MIN(M,N));
  gsl_vector * u = gsl_vector_alloc(M);
  gsl_vector * v = gsl_vector_alloc(N);
  gsl_vector * w = gsl_vector_alloc(M);

  gsl_vector * norm = gsl_vector_alloc(N);
  gsl_permutation * perm = gsl_permutation_alloc(N);

  gsl_matrix_memcpy(qr1,m);
  gsl_matrix_memcpy(qr2,m);
  for(i=0; i<M; i++) gsl_vector_set(u, i, sin(i+1.0));
  for(i=0; i<N; i++) gsl_vector_set(v, i, cos(i+2.0) + sin(i*i+3.0));

  for(i=0; i<M; i++) 
    {
      double ui = gsl_vector_get(u, i);
      for(j=0; j<N; j++) 
        {
          double vj = gsl_vector_get(v, j);
          double qij = gsl_matrix_get(qr1, i, j);
          gsl_matrix_set(qr1, i, j, qij + ui * vj);
        }
    }

  s += gsl_linalg_QRPT_decomp(qr2, d, perm, &signum, norm);
  s += gsl_linalg_QR_unpack(qr2, d, q2, r2);

  /* compute w = Q^T u */
      
  for (j = 0; j < M; j++)
    {
      double sum = 0;
      for (i = 0; i < M; i++)
          sum += gsl_matrix_get (q2, i, j) * gsl_vector_get (u, i);
      gsl_vector_set (w, j, sum);
    }

  s += gsl_linalg_QRPT_update(q2, r2, perm, w, v);

  /* Now compute qr2 = q2 * r2 * p^T */

  /* first multiply q2 * r2 */

  for (i = 0; i < M; i++)
    {
      for (j = 0; j< N; j++)
        {
          double sum = 0;
          for (k = 0; k <= GSL_MIN(j,M-1); k++)
            {
              double qik = gsl_matrix_get(q2, i, k);
              double rkj = gsl_matrix_get(r2, k, j);
              sum += qik * rkj ;
            }
          gsl_matrix_set (qr2, i, j, sum);
        }
    }

  /* now apply permutation to get qr2 = q2 * r2 * p^T */

  for (i = 0; i < M ; i++)
    {
      gsl_vector_view r_i = gsl_matrix_row(qr2, i);
      gsl_permute_vector_inverse(perm, &r_i.vector);
    }


  for(i=0; i<M; i++) {
    for(j=0; j<N; j++) {
      double s1 = gsl_matrix_get(qr1, i, j);
      double s2 = gsl_matrix_get(qr2, i, j);
      
      int foo = check(s1, s2, eps);
      if(foo) {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, s1, s2);
      }
      s += foo;
    }
  }

  gsl_permutation_free (perm);
  gsl_vector_free(norm);
  gsl_vector_free(d);
  gsl_vector_free(u);
  gsl_vector_free(v);
  gsl_vector_free(w);
  gsl_matrix_free(qr1);
  gsl_matrix_free(qr2);
  gsl_matrix_free(q1);
  gsl_matrix_free(r1);
  gsl_matrix_free(q2);
  gsl_matrix_free(r2);

  return s;
}

int test_QRPT_update(void)
{
  int f;
  int s = 0;

  f = test_QRPT_update_dim(m35, 2 * 512.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_update m(3,5)");
  s += f;

  f = test_QRPT_update_dim(m53, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_update m(5,3)");
  s += f;

  f = test_QRPT_update_dim(hilb2,  2 * 512.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_update hilbert(2)");
  s += f;

  f = test_QRPT_update_dim(hilb3,  2 * 512.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_update hilbert(3)");
  s += f;

  f = test_QRPT_update_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_update hilbert(4)");
  s += f;

  f = test_QRPT_update_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_update hilbert(12)");
  s += f;

  f = test_QRPT_update_dim(vander2, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_update vander(2)");
  s += f;

  f = test_QRPT_update_dim(vander3, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_update vander(3)");
  s += f;

  f = test_QRPT_update_dim(vander4, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  QRPT_update vander(4)");
  s += f;

  f = test_QRPT_update_dim(vander12, 0.0005); /* FIXME: bad accuracy */
  gsl_test(f, "  QRPT_update vander(12)");
  s += f;

  return s;
}

int
test_LQ_solve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  unsigned long i, dim = m->size1;

  gsl_vector * rhs = gsl_vector_alloc(dim);
  gsl_matrix * lq  = gsl_matrix_alloc(dim,dim);
  gsl_vector * d = gsl_vector_alloc(dim);
  gsl_vector * x = gsl_vector_alloc(dim);

  gsl_matrix_transpose_memcpy(lq,m);
  for(i=0; i<dim; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_LQ_decomp(lq, d);
  s += gsl_linalg_LQ_solve_T(lq, d, rhs, x);
  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i), actual[i], eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  gsl_vector_free(x);
  gsl_vector_free(d);
  gsl_matrix_free(lq);
  gsl_vector_free(rhs);

  return s;
}

int test_LQ_solve(void)
{
  int f;
  int s = 0;

  f = test_LQ_solve_dim(hilb2, hilb2_solution, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_solve hilbert(2)");
  s += f;

  f = test_LQ_solve_dim(hilb3, hilb3_solution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_solve hilbert(3)");
  s += f;

  f = test_LQ_solve_dim(hilb4, hilb4_solution, 4 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_solve hilbert(4)");
  s += f;

  f = test_LQ_solve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  LQ_solve hilbert(12)");
  s += f;

  f = test_LQ_solve_dim(vander2, vander2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_solve vander(2)");
  s += f;

  f = test_LQ_solve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_solve vander(3)");
  s += f;

  f = test_LQ_solve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_solve vander(4)");
  s += f;

  f = test_LQ_solve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  LQ_solve vander(12)");
  s += f;

  return s;
}




int
test_LQ_LQsolve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  unsigned long i, dim = m->size1;

  gsl_vector * rhs = gsl_vector_alloc(dim);
  gsl_matrix * lq  = gsl_matrix_alloc(dim,dim);
  gsl_matrix * q  = gsl_matrix_alloc(dim,dim);
  gsl_matrix * l  = gsl_matrix_alloc(dim,dim);
  gsl_vector * d = gsl_vector_alloc(dim);
  gsl_vector * x = gsl_vector_alloc(dim);

  gsl_matrix_transpose_memcpy(lq,m);
  for(i=0; i<dim; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_LQ_decomp(lq, d);
  s += gsl_linalg_LQ_unpack(lq, d, q, l);
  s += gsl_linalg_LQ_LQsolve(q, l, rhs, x);
  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i), actual[i], eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  gsl_vector_free(x);
  gsl_vector_free(d);
  gsl_matrix_free(lq);
  gsl_matrix_free(q);
  gsl_matrix_free(l);
  gsl_vector_free(rhs);

  return s;
}

int test_LQ_LQsolve(void)
{
  int f;
  int s = 0;

  f = test_LQ_LQsolve_dim(hilb2, hilb2_solution, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_LQsolve hilbert(2)");
  s += f;

  f = test_LQ_LQsolve_dim(hilb3, hilb3_solution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_LQsolve hilbert(3)");
  s += f;

  f = test_LQ_LQsolve_dim(hilb4, hilb4_solution, 4 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_LQsolve hilbert(4)");
  s += f;

  f = test_LQ_LQsolve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  LQ_LQsolve hilbert(12)");
  s += f;

  f = test_LQ_LQsolve_dim(vander2, vander2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_LQsolve vander(2)");
  s += f;

  f = test_LQ_LQsolve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_LQsolve vander(3)");
  s += f;

  f = test_LQ_LQsolve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_LQsolve vander(4)");
  s += f;

  f = test_LQ_LQsolve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  LQ_LQsolve vander(12)");
  s += f;

  return s;
}


int
test_LQ_lssolve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  unsigned long i, M = m->size1, N = m->size2;

  gsl_vector * rhs = gsl_vector_alloc(M);
  gsl_matrix * lq  = gsl_matrix_alloc(N,M);
  gsl_vector * d = gsl_vector_alloc(N);
  gsl_vector * x = gsl_vector_alloc(N);
  gsl_vector * r = gsl_vector_alloc(M);
  gsl_vector * res = gsl_vector_alloc(M);

  gsl_matrix_transpose_memcpy(lq,m);
  for(i=0; i<M; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_LQ_decomp(lq, d);
  s += gsl_linalg_LQ_lssolve_T(lq, d, rhs, x, res);

  for(i=0; i<N; i++) {
    int foo = check(gsl_vector_get(x, i), actual[i], eps);
    if(foo) {
      printf("(%3lu,%3lu)[%lu]: %22.18g   %22.18g\n", M, N, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }


   /* compute residual r = b - m x */
  if (M == N) {
    gsl_vector_set_zero(r);
  } else {
    gsl_vector_memcpy(r, rhs);
    gsl_blas_dgemv(CblasNoTrans, -1.0, m, x, 1.0, r);
  };

  for(i=0; i<N; i++) {
    int foo = check(gsl_vector_get(res, i), gsl_vector_get(r,i), sqrt(eps));
    if(foo) {
      printf("(%3lu,%3lu)[%lu]: %22.18g   %22.18g\n", M, N, i, gsl_vector_get(res, i), gsl_vector_get(r,i));
    }
    s += foo;
  }

  gsl_vector_free(r);
  gsl_vector_free(res);
  gsl_vector_free(x);
  gsl_vector_free(d);
  gsl_matrix_free(lq);
  gsl_vector_free(rhs);

  return s;
}

int test_LQ_lssolve(void)
{
  int f;
  int s = 0;

  f = test_LQ_lssolve_dim(m53, m53_lssolution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_lssolve m(5,3)");
  s += f;

  f = test_LQ_lssolve_dim(hilb2, hilb2_solution, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_lssolve hilbert(2)");
  s += f;

  f = test_LQ_lssolve_dim(hilb3, hilb3_solution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_lssolve hilbert(3)");
  s += f;

  f = test_LQ_lssolve_dim(hilb4, hilb4_solution, 4 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_lssolve hilbert(4)");
  s += f;

  f = test_LQ_lssolve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  LQ_lssolve hilbert(12)");
  s += f;

  f = test_LQ_lssolve_dim(vander2, vander2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_lssolve vander(2)");
  s += f;

  f = test_LQ_lssolve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_lssolve vander(3)");
  s += f;

  f = test_LQ_lssolve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_lssolve vander(4)");
  s += f;

  f = test_LQ_lssolve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  LQ_lssolve vander(12)");
  s += f;

  return s;
}








int
test_LQ_decomp_dim(const gsl_matrix * m, double eps)
{
  int s = 0;
  unsigned long i,j, M = m->size1, N = m->size2;

  gsl_matrix * lq = gsl_matrix_alloc(M,N);
  gsl_matrix * a  = gsl_matrix_alloc(M,N);
  gsl_matrix * q  = gsl_matrix_alloc(N,N);
  gsl_matrix * l  = gsl_matrix_alloc(M,N);
  gsl_vector * d = gsl_vector_alloc(GSL_MIN(M,N));

  gsl_matrix_memcpy(lq,m);

  s += gsl_linalg_LQ_decomp(lq, d);
  s += gsl_linalg_LQ_unpack(lq, d, q, l);
  
   /* compute a = q r */
  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, l, q, 0.0, a);

  for(i=0; i<M; i++) {
    for(j=0; j<N; j++) {
      double aij = gsl_matrix_get(a, i, j);
      double mij = gsl_matrix_get(m, i, j);
      int foo = check(aij, mij, eps);
      if(foo) {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, aij, mij);
      }
      s += foo;
    }
  }

  gsl_vector_free(d);
  gsl_matrix_free(lq);
  gsl_matrix_free(a);
  gsl_matrix_free(q);
  gsl_matrix_free(l);

  return s;
}

int test_LQ_decomp(void)
{
  int f;
  int s = 0;

  f = test_LQ_decomp_dim(m35, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_decomp m(3,5)");
  s += f;

  f = test_LQ_decomp_dim(m53, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_decomp m(5,3)");
  s += f;

  f = test_LQ_decomp_dim(hilb2, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_decomp hilbert(2)");
  s += f;

  f = test_LQ_decomp_dim(hilb3, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_decomp hilbert(3)");
  s += f;

  f = test_LQ_decomp_dim(hilb4, 4 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_decomp hilbert(4)");
  s += f;

  f = test_LQ_decomp_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_decomp hilbert(12)");
  s += f;

  f = test_LQ_decomp_dim(vander2, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_decomp vander(2)");
  s += f;

  f = test_LQ_decomp_dim(vander3, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_decomp vander(3)");
  s += f;

  f = test_LQ_decomp_dim(vander4, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_decomp vander(4)");
  s += f;

  f = test_LQ_decomp_dim(vander12, 0.0005);  /* FIXME: bad accuracy */
  gsl_test(f, "  LQ_decomp vander(12)");
  s += f;

  return s;
}




int
test_PTLQ_solve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  int signum;
  unsigned long i, dim = m->size1;

  gsl_permutation * perm = gsl_permutation_alloc(dim);
  gsl_vector * rhs = gsl_vector_alloc(dim);
  gsl_matrix * lq  = gsl_matrix_alloc(dim,dim);
  gsl_vector * d = gsl_vector_alloc(dim);
  gsl_vector * x = gsl_vector_alloc(dim);
  gsl_vector * norm = gsl_vector_alloc(dim);

  gsl_matrix_transpose_memcpy(lq,m);
  for(i=0; i<dim; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_PTLQ_decomp(lq, d, perm, &signum, norm);
  s += gsl_linalg_PTLQ_solve_T(lq, d, perm, rhs, x);
  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i), actual[i], eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  gsl_vector_free(norm);
  gsl_vector_free(x);
  gsl_vector_free(d);
  gsl_matrix_free(lq);
  gsl_vector_free(rhs);
  gsl_permutation_free(perm);

  return s;
}

int test_PTLQ_solve(void)
{
  int f;
  int s = 0;

  f = test_PTLQ_solve_dim(hilb2, hilb2_solution, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_solve hilbert(2)");
  s += f;

  f = test_PTLQ_solve_dim(hilb3, hilb3_solution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_solve hilbert(3)");
  s += f;

  f = test_PTLQ_solve_dim(hilb4, hilb4_solution, 2 * 2048.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_solve hilbert(4)");
  s += f;

  f = test_PTLQ_solve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  PTLQ_solve hilbert(12)");
  s += f;

  f = test_PTLQ_solve_dim(vander2, vander2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_solve vander(2)");
  s += f;

  f = test_PTLQ_solve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_solve vander(3)");
  s += f;

  f = test_PTLQ_solve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_solve vander(4)");
  s += f;

  f = test_PTLQ_solve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  PTLQ_solve vander(12)");
  s += f;

  return s;
}


int
test_PTLQ_LQsolve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  int signum;
  unsigned long i, dim = m->size1;

  gsl_permutation * perm = gsl_permutation_alloc(dim);
  gsl_vector * rhs = gsl_vector_alloc(dim);
  gsl_matrix * lq  = gsl_matrix_alloc(dim,dim);
  gsl_matrix * q  = gsl_matrix_alloc(dim,dim);
  gsl_matrix * l  = gsl_matrix_alloc(dim,dim);
  gsl_vector * d = gsl_vector_alloc(dim);
  gsl_vector * x = gsl_vector_alloc(dim);
  gsl_vector * norm = gsl_vector_alloc(dim);

  gsl_matrix_transpose_memcpy(lq,m);
  for(i=0; i<dim; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_PTLQ_decomp2(lq, q, l, d, perm, &signum, norm);
  s += gsl_linalg_PTLQ_LQsolve_T(q, l, perm, rhs, x);
  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i), actual[i], eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  gsl_vector_free(norm);
  gsl_vector_free(x);
  gsl_vector_free(d);
  gsl_matrix_free(lq);
  gsl_vector_free(rhs);
  gsl_permutation_free(perm);

  return s;
}

int test_PTLQ_LQsolve(void)
{
  int f;
  int s = 0;

  f = test_PTLQ_LQsolve_dim(hilb2, hilb2_solution, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_LQsolve hilbert(2)");
  s += f;

  f = test_PTLQ_LQsolve_dim(hilb3, hilb3_solution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_LQsolve hilbert(3)");
  s += f;

  f = test_PTLQ_LQsolve_dim(hilb4, hilb4_solution, 2 * 2048.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_LQsolve hilbert(4)");
  s += f;

  f = test_PTLQ_LQsolve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  PTLQ_LQsolve hilbert(12)");
  s += f;

  f = test_PTLQ_LQsolve_dim(vander2, vander2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_LQsolve vander(2)");
  s += f;

  f = test_PTLQ_LQsolve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_LQsolve vander(3)");
  s += f;

  f = test_PTLQ_LQsolve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_LQsolve vander(4)");
  s += f;

  f = test_PTLQ_LQsolve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  PTLQ_LQsolve vander(12)");
  s += f;

  return s;
}


int
test_PTLQ_decomp_dim(const gsl_matrix * m, double eps)
{
  int s = 0, signum;
  unsigned long i,j, M = m->size1, N = m->size2;

  gsl_matrix * lq = gsl_matrix_alloc(N,M);
  gsl_matrix * a  = gsl_matrix_alloc(N,M);
  gsl_matrix * q  = gsl_matrix_alloc(M,M);
  gsl_matrix * l  = gsl_matrix_alloc(N,M);
  gsl_vector * d = gsl_vector_alloc(GSL_MIN(M,N));
  gsl_vector * norm = gsl_vector_alloc(N);

  gsl_permutation * perm = gsl_permutation_alloc(N);

  gsl_matrix_transpose_memcpy(lq,m);

  s += gsl_linalg_PTLQ_decomp(lq, d, perm, &signum, norm);
  s += gsl_linalg_LQ_unpack(lq, d, q, l);

   /* compute a = l q */
  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, l, q, 0.0, a);


   /* Compute P LQ  by permuting the rows of LQ */

  for (i = 0; i < M; i++) {
    gsl_vector_view col = gsl_matrix_column (a, i);
    gsl_permute_vector_inverse (perm, &col.vector);
  }

  for(i=0; i<M; i++) {
    for(j=0; j<N; j++) {
      double aij = gsl_matrix_get(a, j, i);
      double mij = gsl_matrix_get(m, i, j);
      int foo = check(aij, mij, eps);
      if(foo) {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, aij, mij);
      }
      s += foo;
    }
  }

  gsl_permutation_free (perm);
  gsl_vector_free(norm);
  gsl_vector_free(d);
  gsl_matrix_free(lq);
  gsl_matrix_free(a);
  gsl_matrix_free(q);
  gsl_matrix_free(l);

  return s;
}

int test_PTLQ_decomp(void)
{
  int f;
  int s = 0;

  f = test_PTLQ_decomp_dim(m35, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_decomp m(3,5)");
  s += f;

  f = test_PTLQ_decomp_dim(m53, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_decomp m(5,3)");
  s += f;

  f = test_PTLQ_decomp_dim(s35, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_decomp s(3,5)");
  s += f;

  f = test_PTLQ_decomp_dim(s53, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_decomp s(5,3)");
  s += f;

  f = test_PTLQ_decomp_dim(hilb2, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_decomp hilbert(2)");
  s += f;

  f = test_PTLQ_decomp_dim(hilb3, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_decomp hilbert(3)");
  s += f;

  f = test_PTLQ_decomp_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_decomp hilbert(4)");
  s += f;

  f = test_PTLQ_decomp_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_decomp hilbert(12)");
  s += f;

  f = test_PTLQ_decomp_dim(vander2, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_decomp vander(2)");
  s += f;

  f = test_PTLQ_decomp_dim(vander3, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_decomp vander(3)");
  s += f;

  f = test_PTLQ_decomp_dim(vander4, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  PTLQ_decomp vander(4)");
  s += f;

  f = test_PTLQ_decomp_dim(vander12, 0.0005);  /* FIXME: bad accuracy */
  gsl_test(f, "  PTLQ_decomp vander(12)");
  s += f;

  return s;
}


int
test_LQ_update_dim(const gsl_matrix * m, double eps)
{
  int s = 0;
  unsigned long i,j, M = m->size1, N = m->size2;

  gsl_matrix * lq1  = gsl_matrix_alloc(N,M);
  gsl_matrix * lq2  = gsl_matrix_alloc(N,M);
  gsl_matrix * q1  = gsl_matrix_alloc(M,M);
  gsl_matrix * l1  = gsl_matrix_alloc(N,M);
  gsl_matrix * q2  = gsl_matrix_alloc(M,M);
  gsl_matrix * l2  = gsl_matrix_alloc(N,M);
  gsl_vector * d2 = gsl_vector_alloc(GSL_MIN(M,N));
  gsl_vector * u = gsl_vector_alloc(M);
  gsl_vector * v = gsl_vector_alloc(N);
  gsl_vector * w = gsl_vector_alloc(M);

  gsl_matrix_transpose_memcpy(lq1,m);
  gsl_matrix_transpose_memcpy(lq2,m);
  for(i=0; i<M; i++) gsl_vector_set(u, i, sin(i+1.0));
  for(i=0; i<N; i++) gsl_vector_set(v, i, cos(i+2.0) + sin(i*i+3.0));

  /* lq1 is updated */

  gsl_blas_dger(1.0, v, u, lq1);

  /* lq2 is first decomposed, updated later */

  s += gsl_linalg_LQ_decomp(lq2, d2);
  s += gsl_linalg_LQ_unpack(lq2, d2, q2, l2);

  /* compute w = Q^T u */

  gsl_blas_dgemv(CblasNoTrans, 1.0, q2, u, 0.0, w);

  /* now lq2 is updated */

  s += gsl_linalg_LQ_update(q2, l2, v, w);

  /* multiply q2*l2 */

  gsl_blas_dgemm(CblasNoTrans,CblasNoTrans,1.0,l2,q2,0.0,lq2);

  /*  check lq1==lq2 */

  for(i=0; i<N; i++) {
    for(j=0; j<M; j++) {
      double s1 = gsl_matrix_get(lq1, i, j);
      double s2 = gsl_matrix_get(lq2, i, j);
      
      int foo = check(s1, s2, eps);
#if 0
      if(foo) {
	  printf("LQ:(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, s1, s2);
      }
#endif
      s += foo;
    }
  }

  gsl_vector_free(d2);
  gsl_vector_free(u);
  gsl_vector_free(v);
  gsl_vector_free(w);
  gsl_matrix_free(lq1);
  gsl_matrix_free(lq2);
  gsl_matrix_free(q1);
  gsl_matrix_free(l1);
  gsl_matrix_free(q2);
  gsl_matrix_free(l2);

  return s;
}

int test_LQ_update(void)
{
  int f;
  int s = 0;

  f = test_LQ_update_dim(m35, 2 * 512.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_update m(3,5)");
  s += f;

  f = test_LQ_update_dim(m53, 2 * 512.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_update m(5,3)");
  s += f;

  f = test_LQ_update_dim(hilb2,  2 * 512.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_update hilbert(2)");
  s += f;

  f = test_LQ_update_dim(hilb3,  2 * 512.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_update hilbert(3)");
  s += f;

  f = test_LQ_update_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_update hilbert(4)");
  s += f;

  f = test_LQ_update_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_update hilbert(12)");
  s += f;

  f = test_LQ_update_dim(vander2, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_update vander(2)");
  s += f;

  f = test_LQ_update_dim(vander3, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_update vander(3)");
  s += f;

  f = test_LQ_update_dim(vander4, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  LQ_update vander(4)");
  s += f;

  f = test_LQ_update_dim(vander12, 0.0005);  /* FIXME: bad accuracy */
  gsl_test(f, "  LQ_update vander(12)");
  s += f;

  return s;
}

int
test_SV_solve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  unsigned long i, dim = m->size1;

  gsl_vector * rhs = gsl_vector_alloc(dim);
  gsl_matrix * u  = gsl_matrix_alloc(dim,dim);
  gsl_matrix * q  = gsl_matrix_alloc(dim,dim);
  gsl_vector * d = gsl_vector_alloc(dim);
  gsl_vector * x = gsl_vector_calloc(dim);
  gsl_matrix_memcpy(u,m);
  for(i=0; i<dim; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_SV_decomp(u, q, d, x);
  s += gsl_linalg_SV_solve(u, q, d, rhs, x);
  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i), actual[i], eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }
  gsl_vector_free(x);
  gsl_vector_free(d);
  gsl_matrix_free(u);
  gsl_matrix_free(q);
  gsl_vector_free(rhs);

  return s;
}

int test_SV_solve(void)
{
  int f;
  int s = 0;

  f = test_SV_solve_dim(hilb2, hilb2_solution, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_solve hilbert(2)");
  s += f;

  f = test_SV_solve_dim(hilb3, hilb3_solution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_solve hilbert(3)");
  s += f;

  f = test_SV_solve_dim(hilb4, hilb4_solution, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_solve hilbert(4)");
  s += f;

  f = test_SV_solve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  SV_solve hilbert(12)");
  s += f;

  f = test_SV_solve_dim(vander2, vander2_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_solve vander(2)");
  s += f;

  f = test_SV_solve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_solve vander(3)");
  s += f;

  f = test_SV_solve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_solve vander(4)");
  s += f;

  f = test_SV_solve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  SV_solve vander(12)");
  s += f;

  return s;
}

int
test_SV_decomp_dim(const gsl_matrix * m, double eps)
{
  int s = 0;
  double di1;
  unsigned long i,j, M = m->size1, N = m->size2;
  unsigned long input_nans = 0;

  gsl_matrix * v  = gsl_matrix_alloc(M,N);
  gsl_matrix * a  = gsl_matrix_alloc(M,N);
  gsl_matrix * q  = gsl_matrix_alloc(N,N);
  gsl_matrix * dqt  = gsl_matrix_alloc(N,N);
  gsl_vector * d  = gsl_vector_alloc(N);
  gsl_vector * w  = gsl_vector_alloc(N);

  gsl_matrix_memcpy(v,m);

  /* Check for nans in the input */
  for (i = 0; i<M; i++) {
    for (j = 0; j<N; j++) {
      double m_ij = gsl_matrix_get (m, i, j);
      if (gsl_isnan (m_ij)) input_nans++;
    }
  }

  s = gsl_linalg_SV_decomp(v, q, d, w); 

  if (s) printf("returned error code %d = %s\n", s, gsl_strerror(s));

  /* Check that singular values are non-negative and in non-decreasing
     order */
  
  di1 = 0.0;

  for (i = 0; i < N; i++)
    {
      double di = gsl_vector_get (d, i);

      if (gsl_isnan (di))
        {
          if (input_nans > 0) 
            continue;  /* skip NaNs if present in input */
          else
            {
              s++;
              printf("bad singular value %lu = %22.18g\n", i, di);
            }
        }

      if (di < 0) {
        s++;
        printf("singular value %lu = %22.18g < 0\n", i, di);
      }

      if(i > 0 && di > di1) {
        s++;
        printf("singular value %lu = %22.18g vs previous %22.18g\n", i, di, di1);
      }

      di1 = di;
    }      
  
  /* Scale dqt = D Q^T */
  
  for (i = 0; i < N ; i++)
    {
      double di = gsl_vector_get (d, i);

      for (j = 0; j < N; j++)
        {
          double qji = gsl_matrix_get(q, j, i);
          gsl_matrix_set (dqt, i, j, qji * di);
        }
    }
            
  /* compute a = v dqt */
  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, v, dqt, 0.0, a);

  for(i=0; i<M; i++) {
    for(j=0; j<N; j++) {
      double aij = gsl_matrix_get(a, i, j);
      double mij = gsl_matrix_get(m, i, j);
      int foo = check(aij, mij, eps);
      if(foo) {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, aij, mij);
      }
      s += foo;
    }
  }
  gsl_vector_free(w);
  gsl_vector_free(d);
  gsl_matrix_free(v);
  gsl_matrix_free(a);
  gsl_matrix_free(q);
  gsl_matrix_free(dqt);

  return s;
}

int test_SV_decomp(void)
{
  int f;
  int s = 0;

  f = test_SV_decomp_dim(m11, 2 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp m(1,1)");
  s += f;

  f = test_SV_decomp_dim(m51, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp m(5,1)");
  s += f;

  /* M<N not implemented yet */
#if 0
  f = test_SV_decomp_dim(m35, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp m(3,5)");
  s += f;
#endif
  f = test_SV_decomp_dim(m53, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp m(5,3)");
  s += f;

  f = test_SV_decomp_dim(moler10, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp moler(10)");
  s += f;

  f = test_SV_decomp_dim(hilb2, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp hilbert(2)");
  s += f;

  f = test_SV_decomp_dim(hilb3, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp hilbert(3)");
  s += f;

  f = test_SV_decomp_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp hilbert(4)");
  s += f;

  f = test_SV_decomp_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp hilbert(12)");
  s += f;

  f = test_SV_decomp_dim(vander2, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp vander(2)");
  s += f;

  f = test_SV_decomp_dim(vander3, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp vander(3)");
  s += f;

  f = test_SV_decomp_dim(vander4, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp vander(4)");
  s += f;

  f = test_SV_decomp_dim(vander12, 1e-4);
  gsl_test(f, "  SV_decomp vander(12)");
  s += f;

  f = test_SV_decomp_dim(row3, 10 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp row3");
  s += f;

  f = test_SV_decomp_dim(row5, 128 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp row5");
  s += f;

  f = test_SV_decomp_dim(row12, 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp row12");
  s += f;

  f = test_SV_decomp_dim(inf5, 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp inf5");
  s += f;

  f = test_SV_decomp_dim(nan5, 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp nan5");
  s += f;

  f = test_SV_decomp_dim(dblmin3, 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp dblmin3");
  s += f;

  f = test_SV_decomp_dim(dblmin5, 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp dblmin5");
  s += f;

  f = test_SV_decomp_dim(dblsubnorm5, 100 * 1024 * 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp dblsubnorm5");
  s += f;

  f = test_SV_decomp_dim(bigsparse, 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp bigsparse");
  s += f;


  {
    double i1, i2, i3, i4;
    double lower = -2, upper = 2;

    for (i1 = lower; i1 <= upper; i1++)
      {
        for (i2 = lower; i2 <= upper; i2++)
          {
            for (i3 = lower; i3 <= upper; i3++)
              {
                for (i4 = lower; i4 <= upper; i4++)
                  {
                    gsl_matrix_set (A22, 0,0, i1);
                    gsl_matrix_set (A22, 0,1, i2);
                    gsl_matrix_set (A22, 1,0, i3);
                    gsl_matrix_set (A22, 1,1, i4);
                    
                    f = test_SV_decomp_dim(A22, 16 * GSL_DBL_EPSILON);
                    gsl_test(f, "  SV_decomp (2x2) A=[%g, %g; %g, %g]", i1,i2,i3,i4);
                    s += f;
                  }
              }
          }
      }
  }

  {
    int i;
    double carry = 0, lower = 0, upper = 1;
    double *a = A33->data;

    for (i=0; i<9; i++) {
      a[i] = lower;
    }
    
    while (carry == 0.0) {
      f = test_SV_decomp_dim(A33, 64 * GSL_DBL_EPSILON);
      gsl_test(f, "  SV_decomp (3x3) A=[ %g, %g, %g; %g, %g, %g; %g, %g, %g]",
               a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
      
      /* increment */
      carry=1.0;
      for (i=9; carry > 0.0 && i>0 && i--;) 
        {
          double v=a[i]+carry;
          carry = (v>upper) ? 1.0 : 0.0;
          a[i] = (v>upper) ? lower : v;
        }
    }
  }

#ifdef TEST_SVD_4X4
  {
    int i;
    double carry = 0, lower = 0, upper = 1;
    double *a = A44->data;

    for (i=0; i<16; i++) {
      a[i] = lower;
    }
    
    while (carry == 0.0) {
      f = test_SV_decomp_dim(A44, 64 * GSL_DBL_EPSILON);
      gsl_test(f, "  SV_decomp (4x4) A=[ %g, %g, %g, %g; %g, %g, %g, %g; %g, %g, %g, %g; %g, %g, %g, %g]",
               a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
               a[10], a[11], a[12], a[13], a[14], a[15]);
      
      /* increment */
      carry=1.0;
      for (i=16; carry > 0.0 && i>0 && i--;) 
        {
          double v=a[i]+carry;
          carry = (v>upper) ? 1.0 : 0.0;
          a[i] = (v>upper) ? lower : v;
        }
    }
  }
#endif

  return s;
}


int
test_SV_decomp_mod_dim(const gsl_matrix * m, double eps)
{
  int s = 0;
  double di1;
  unsigned long i,j, M = m->size1, N = m->size2;

  gsl_matrix * v  = gsl_matrix_alloc(M,N);
  gsl_matrix * a  = gsl_matrix_alloc(M,N);
  gsl_matrix * q  = gsl_matrix_alloc(N,N);
  gsl_matrix * x  = gsl_matrix_alloc(N,N);
  gsl_matrix * dqt  = gsl_matrix_alloc(N,N);
  gsl_vector * d  = gsl_vector_alloc(N);
  gsl_vector * w  = gsl_vector_alloc(N);

  gsl_matrix_memcpy(v,m);

  s += gsl_linalg_SV_decomp_mod(v, x, q, d, w); 

  /* Check that singular values are non-negative and in non-decreasing
     order */
  
  di1 = 0.0;

  for (i = 0; i < N; i++)
    {
      double di = gsl_vector_get (d, i);

      if (gsl_isnan (di))
        {
          continue;  /* skip NaNs */
        }

      if (di < 0) {
        s++;
        printf("singular value %lu = %22.18g < 0\n", i, di);
      }

      if(i > 0 && di > di1) {
        s++;
        printf("singular value %lu = %22.18g vs previous %22.18g\n", i, di, di1);
      }

      di1 = di;
    }      
  
  /* Scale dqt = D Q^T */
  
  for (i = 0; i < N ; i++)
    {
      double di = gsl_vector_get (d, i);

      for (j = 0; j < N; j++)
        {
          double qji = gsl_matrix_get(q, j, i);
          gsl_matrix_set (dqt, i, j, qji * di);
        }
    }
            
  /* compute a = v dqt */
  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, v, dqt, 0.0, a);

  for(i=0; i<M; i++) {
    for(j=0; j<N; j++) {
      double aij = gsl_matrix_get(a, i, j);
      double mij = gsl_matrix_get(m, i, j);
      int foo = check(aij, mij, eps);
      if(foo) {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, aij, mij);
      }
      s += foo;
    }
  }
  gsl_vector_free(w);
  gsl_vector_free(d);
  gsl_matrix_free(v);
  gsl_matrix_free(a);
  gsl_matrix_free(q);
  gsl_matrix_free(dqt);
  gsl_matrix_free (x);

  return s;
}

int test_SV_decomp_mod(void)
{
  int f;
  int s = 0;

  f = test_SV_decomp_mod_dim(m11, 2 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod m(1,1)");
  s += f;

  f = test_SV_decomp_mod_dim(m51, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod m(5,1)");
  s += f;

  /* M<N not implemented yet */
#if 0
  f = test_SV_decomp_mod_dim(m35, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod m(3,5)");
  s += f;
#endif
  f = test_SV_decomp_mod_dim(m53, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod m(5,3)");
  s += f;

  f = test_SV_decomp_mod_dim(moler10, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod moler(10)");
  s += f;

  f = test_SV_decomp_mod_dim(hilb2, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod hilbert(2)");
  s += f;

  f = test_SV_decomp_mod_dim(hilb3, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod hilbert(3)");
  s += f;

  f = test_SV_decomp_mod_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod hilbert(4)");
  s += f;

  f = test_SV_decomp_mod_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod hilbert(12)");
  s += f;

  f = test_SV_decomp_mod_dim(vander2, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod vander(2)");
  s += f;

  f = test_SV_decomp_mod_dim(vander3, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod vander(3)");
  s += f;

  f = test_SV_decomp_mod_dim(vander4, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod vander(4)");
  s += f;

  f = test_SV_decomp_mod_dim(vander12, 1e-4);
  gsl_test(f, "  SV_decomp_mod vander(12)");
  s += f;

  f = test_SV_decomp_mod_dim(row3, 10 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod row3");
  s += f;

  f = test_SV_decomp_mod_dim(row5, 128 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod row5");
  s += f;

  f = test_SV_decomp_mod_dim(row12, 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod row12");
  s += f;

  f = test_SV_decomp_mod_dim(inf5, 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod inf5");
  s += f;

  f = test_SV_decomp_mod_dim(nan5, 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_mod nan5");
  s += f;


  {
    double i1, i2, i3, i4;
    double lower = -2, upper = 2;

    for (i1 = lower; i1 <= upper; i1++)
      {
        for (i2 = lower; i2 <= upper; i2++)
          {
            for (i3 = lower; i3 <= upper; i3++)
              {
                for (i4 = lower; i4 <= upper; i4++)
                  {
                    gsl_matrix_set (A22, 0,0, i1);
                    gsl_matrix_set (A22, 0,1, i2);
                    gsl_matrix_set (A22, 1,0, i3);
                    gsl_matrix_set (A22, 1,1, i4);
                    
                    f = test_SV_decomp_mod_dim(A22, 16 * GSL_DBL_EPSILON);
                    gsl_test(f, "  SV_decomp_mod (2x2) A=[%g, %g; %g, %g]", i1,i2,i3,i4);
                    s += f;
                  }
              }
          }
      }
  }

  {
    int i;
    double carry = 0, lower = 0, upper = 1;
    double *a = A33->data;

    for (i=0; i<9; i++) {
      a[i] = lower;
    }
    
    while (carry == 0.0) {
      f = test_SV_decomp_mod_dim(A33, 64 * GSL_DBL_EPSILON);
      gsl_test(f, "  SV_decomp_mod (3x3) A=[ %g, %g, %g; %g, %g, %g; %g, %g, %g]",
               a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
      
      /* increment */
      carry=1.0;
      for (i=9; carry > 0.0 && i>0 && i--;) 
        {
          double v=a[i]+carry;
          carry = (v>upper) ? 1.0 : 0.0;
          a[i] = (v>upper) ? lower : v;
        }
    }
  }

#ifdef TEST_SVD_4X4
  {
    int i;
    double carry = 0, lower = 0, upper = 1;
    double *a = A44->data;

    for (i=0; i<16; i++) {
      a[i] = lower;
    }
    
    while (carry == 0.0) {
      f = test_SV_decomp_mod_dim(A44, 64 * GSL_DBL_EPSILON);
      gsl_test(f, "  SV_decomp_mod (4x4) A=[ %g, %g, %g, %g; %g, %g, %g, %g; %g, %g, %g, %g; %g, %g, %g, %g]",
               a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
               a[10], a[11], a[12], a[13], a[14], a[15]);
      
      /* increment */
      carry=1.0;
      for (i=16; carry>0.0 && i>0 && i--;) 
        {
          double v=a[i]+carry;
          carry = (v>upper) ? 1.0 : 0.0;
          a[i] = (v>upper) ? lower : v;
        }
    }
  }
#endif

  return s;
}


int
test_SV_decomp_jacobi_dim(const gsl_matrix * m, double eps)
{
  int s = 0;
  double di1;
  unsigned long i,j, M = m->size1, N = m->size2;

  gsl_matrix * v  = gsl_matrix_alloc(M,N);
  gsl_matrix * a  = gsl_matrix_alloc(M,N);
  gsl_matrix * q  = gsl_matrix_alloc(N,N);
  gsl_matrix * dqt  = gsl_matrix_alloc(N,N);
  gsl_vector * d  = gsl_vector_alloc(N);

  gsl_matrix_memcpy(v,m);

  s += gsl_linalg_SV_decomp_jacobi(v, q, d); 
  if (s)
    printf("call returned status = %d\n", s);

  /* Check that singular values are non-negative and in non-decreasing
     order */
  
  di1 = 0.0;

  for (i = 0; i < N; i++)
    {
      double di = gsl_vector_get (d, i);

      if (gsl_isnan (di))
        {
          continue;  /* skip NaNs */
        }

      if (di < 0) {
        s++;
        printf("singular value %lu = %22.18g < 0\n", i, di);
      }

      if(i > 0 && di > di1) {
        s++;
        printf("singular value %lu = %22.18g vs previous %22.18g\n", i, di, di1);
      }

      di1 = di;
    }      
  
  /* Scale dqt = D Q^T */
  
  for (i = 0; i < N ; i++)
    {
      double di = gsl_vector_get (d, i);

      for (j = 0; j < N; j++)
        {
          double qji = gsl_matrix_get(q, j, i);
          gsl_matrix_set (dqt, i, j, qji * di);
        }
    }
            
  /* compute a = v dqt */
  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, v, dqt, 0.0, a);

  for(i=0; i<M; i++) {
    for(j=0; j<N; j++) {
      double aij = gsl_matrix_get(a, i, j);
      double mij = gsl_matrix_get(m, i, j);
      int foo = check(aij, mij, eps);
      if(foo) {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, aij, mij);
      }
      s += foo;
    }
  }
  gsl_vector_free(d);
  gsl_matrix_free(v);
  gsl_matrix_free(a);
  gsl_matrix_free(q);
  gsl_matrix_free(dqt);

  return s;
}

int test_SV_decomp_jacobi(void)
{
  int f;
  int s = 0;

  f = test_SV_decomp_jacobi_dim(m11, 2 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi m(1,1)");
  s += f;

  f = test_SV_decomp_jacobi_dim(m51, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi m(5,1)");
  s += f;

  /* M<N not implemented yet */
#if 0
  f = test_SV_decomp_jacobi_dim(m35, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi m(3,5)");
  s += f;
#endif
  f = test_SV_decomp_jacobi_dim(m53, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi m(5,3)");
  s += f;

  f = test_SV_decomp_jacobi_dim(moler10, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi moler(10)");
  s += f;

  f = test_SV_decomp_jacobi_dim(hilb2, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi hilbert(2)");
  s += f;

  f = test_SV_decomp_jacobi_dim(hilb3, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi hilbert(3)");
  s += f;

  f = test_SV_decomp_jacobi_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi hilbert(4)");
  s += f;

  f = test_SV_decomp_jacobi_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi hilbert(12)");
  s += f;

  f = test_SV_decomp_jacobi_dim(vander2, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi vander(2)");
  s += f;

  f = test_SV_decomp_jacobi_dim(vander3, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi vander(3)");
  s += f;

  f = test_SV_decomp_jacobi_dim(vander4, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi vander(4)");
  s += f;

  f = test_SV_decomp_jacobi_dim(vander12, 1e-4);
  gsl_test(f, "  SV_decomp_jacobi vander(12)");
  s += f;

  f = test_SV_decomp_jacobi_dim(row3, 10 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi row3");
  s += f;

  f = test_SV_decomp_jacobi_dim(row5, 128 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi row5");
  s += f;

  f = test_SV_decomp_jacobi_dim(row12, 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi row12");
  s += f;


#ifdef TEST_JACOBI_INF
  f = test_SV_decomp_jacobi_dim(inf5, 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi inf5");
  s += f;

  f = test_SV_decomp_jacobi_dim(nan5, 1024 * GSL_DBL_EPSILON);
  gsl_test(f, "  SV_decomp_jacobi nan5");
  s += f;
#endif

  {
    double i1, i2, i3, i4;
    double lower = -2, upper = 2;

    for (i1 = lower; i1 <= upper; i1++)
      {
        for (i2 = lower; i2 <= upper; i2++)
          {
            for (i3 = lower; i3 <= upper; i3++)
              {
                for (i4 = lower; i4 <= upper; i4++)
                  {
                    gsl_matrix_set (A22, 0,0, i1);
                    gsl_matrix_set (A22, 0,1, i2);
                    gsl_matrix_set (A22, 1,0, i3);
                    gsl_matrix_set (A22, 1,1, i4);
                    
                    f = test_SV_decomp_jacobi_dim(A22, 16 * GSL_DBL_EPSILON);
                    gsl_test(f, "  SV_decomp_jacobi (2x2) A=[%g, %g; %g, %g]", i1,i2,i3,i4);
                    s += f;
                  }
              }
          }
      }
  }

  {
    int i;
    double carry = 0, lower = 0, upper = 1;
    double *a = A33->data;

    for (i=0; i<9; i++) {
      a[i] = lower;
    }
    
    while (carry == 0.0) {
      f = test_SV_decomp_jacobi_dim(A33, 64 * GSL_DBL_EPSILON);
      gsl_test(f, "  SV_decomp_jacobi (3x3) A=[ %g, %g, %g; %g, %g, %g; %g, %g, %g]",
               a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
      
      /* increment */
      carry=1.0;
      for (i=9; carry > 0.0 && i>0 && i--;) 
        {
          double v=a[i]+carry;
          carry = (v>upper) ? 1.0 : 0.0;
          a[i] = (v>upper) ? lower : v;
        }
    }
  }

#ifdef TEST_SVD_4X4
  {
    int i;
    unsigned long k = 0;
    double carry = 0, lower = 0, upper = 1;
    double *a = A44->data;

    for (i=0; i<16; i++) {
      a[i] = lower;
    }
    
    while (carry == 0.0) {
      k++;
      f = test_SV_decomp_jacobi_dim(A44, 64 * GSL_DBL_EPSILON);
      gsl_test(f, "  SV_decomp_jacobi (4x4) A=[ %g, %g, %g, %g; %g, %g, %g, %g; %g, %g, %g, %g; %g, %g, %g, %g] %lu",
               a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
               a[10], a[11], a[12], a[13], a[14], a[15], k);
      /* increment */
      carry=1.0;
      for (i=16; carry > 0.0 && i>0 && i--;) 
        {
          double v=a[i]+carry;
          carry = (v>upper) ? 1.0 : 0.0;
          a[i] = (v>upper) ? lower : v;
        }
    }
  }
#endif

  {
    int i;
    unsigned long k = 0;
    double carry = 0, lower = 0, upper = 1;
    double *a = A55->data;

    for (i=0; i<25; i++) {
      a[i] = lower;
    }
    
    while (carry == 0.0) {
      k++;

      if (k % 1001 == 0)
        {
          f = test_SV_decomp_jacobi_dim(A55, 64 * GSL_DBL_EPSILON);
          gsl_test(f, "  SV_decomp_jacobi (5x5) case=%lu",k);
        }
      
      /* increment */
      carry=1.0;
      for (i=25; carry >0.0 && i>0 && i--;) 
        {
          double v=a[i]+carry;
          carry = (v>upper) ? 1.0 : 0.0;
          a[i] = (v>upper) ? lower : v;
        }
    }
  }


  return s;
}


int
test_cholesky_solve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  unsigned long i, dim = m->size1;

  gsl_vector * rhs = gsl_vector_alloc(dim);
  gsl_matrix * u  = gsl_matrix_alloc(dim,dim);
  gsl_vector * x = gsl_vector_calloc(dim);
  gsl_matrix_memcpy(u,m);
  for(i=0; i<dim; i++) gsl_vector_set(rhs, i, i+1.0);
  s += gsl_linalg_cholesky_decomp(u);
  s += gsl_linalg_cholesky_solve(u, rhs, x);
  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i), actual[i], eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }
  gsl_vector_free(x);
  gsl_matrix_free(u);
  gsl_vector_free(rhs);

  return s;
}

int test_cholesky_solve(void)
{
  int f;
  int s = 0;

  f = test_cholesky_solve_dim(hilb2, hilb2_solution, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_solve hilbert(2)");
  s += f;

  f = test_cholesky_solve_dim(hilb3, hilb3_solution, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_solve hilbert(3)");
  s += f;

  f = test_cholesky_solve_dim(hilb4, hilb4_solution, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_solve hilbert(4)");
  s += f;

  f = test_cholesky_solve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  cholesky_solve hilbert(12)");
  s += f;

  return s;
}


int
test_cholesky_decomp_dim(const gsl_matrix * m, double eps)
{
  int s = 0;
  unsigned long i,j, M = m->size1, N = m->size2;

  gsl_matrix * v  = gsl_matrix_alloc(M,N);
  gsl_matrix * a  = gsl_matrix_alloc(M,N);
  gsl_matrix * l  = gsl_matrix_alloc(M,N);
  gsl_matrix * lt  = gsl_matrix_alloc(N,N);

  gsl_matrix_memcpy(v,m);

  s += gsl_linalg_cholesky_decomp(v);
  
  /* Compute L LT */
  
  for (i = 0; i < N ; i++)
    {
      for (j = 0; j < N; j++)
        {
          double vij = gsl_matrix_get(v, i, j);
          gsl_matrix_set (l, i, j, i>=j ? vij : 0);
          gsl_matrix_set (lt, i, j, i<=j ? vij : 0);
        }
    }
            
  /* compute a = l lt */
  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, l, lt, 0.0, a);

  for(i=0; i<M; i++) {
    for(j=0; j<N; j++) {
      double aij = gsl_matrix_get(a, i, j);
      double mij = gsl_matrix_get(m, i, j);
      int foo = check(aij, mij, eps);
      if(foo) {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, aij, mij);
      }
      s += foo;
    }
  }

  gsl_matrix_free(v);
  gsl_matrix_free(a);
  gsl_matrix_free(l);
  gsl_matrix_free(lt);

  return s;
}

int test_cholesky_decomp(void)
{
  int f;
  int s = 0;

  f = test_cholesky_decomp_dim(hilb2, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_decomp hilbert(2)");
  s += f;

  f = test_cholesky_decomp_dim(hilb3, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_decomp hilbert(3)");
  s += f;

  f = test_cholesky_decomp_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_decomp hilbert(4)");
  s += f;

  f = test_cholesky_decomp_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_decomp hilbert(12)");
  s += f;

  return s;
}

int
test_cholesky_invert_dim(const gsl_matrix * m, double eps)
{
  int s = 0;
  unsigned long i, j, N = m->size1;

  gsl_matrix * v  = gsl_matrix_alloc(N, N);
  gsl_matrix * c  = gsl_matrix_alloc(N, N);

  gsl_matrix_memcpy(v,m);

  s += gsl_linalg_cholesky_decomp(v);
  s += gsl_linalg_cholesky_invert(v);

  gsl_blas_dsymm(CblasLeft, CblasUpper, 1.0, m, v, 0.0, c);

  /* c should be the identity matrix */

  for (i = 0; i < N; ++i)
    {
      for (j = 0; j < N; ++j)
        {
          int foo;
          double cij = gsl_matrix_get(c, i, j);
          double expected;

          if (i == j)
            expected = 1.0;
          else
            expected = 0.0;

          foo = check(cij, expected, eps);

          if (foo)
            printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", N, N, i,j, cij, expected);

          s += foo;
        }
    }

  gsl_matrix_free(v);
  gsl_matrix_free(c);

  return s;
}

int
test_cholesky_invert(void)
{
  int f;
  int s = 0;

  f = test_cholesky_invert_dim(hilb2, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_invert hilbert(2)");
  s += f;

  f = test_cholesky_invert_dim(hilb3, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_invert hilbert(3)");
  s += f;

  f = test_cholesky_invert_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_invert hilbert(4)");
  s += f;

  return s;
}


int
test_cholesky_decomp_unit_dim(const gsl_matrix * m, double eps)
{
  int s = 0;
  const unsigned long M = m->size1;
  const unsigned long N = m->size2;
  unsigned long i,j;

  gsl_matrix * v  = gsl_matrix_alloc(M,N);
  gsl_matrix * a  = gsl_matrix_alloc(M,N);
  gsl_matrix * l  = gsl_matrix_alloc(M,N);
  gsl_matrix * lt = gsl_matrix_alloc(N,N);
  gsl_matrix * dm = gsl_matrix_alloc(M,N);
  gsl_vector * dv = gsl_vector_alloc(M);

  gsl_matrix_memcpy(v,m);

  s += gsl_linalg_cholesky_decomp_unit(v, dv);

  /*
  for(i = 0; i < M; i++)
  {
    for(j = 0; j < N; j++)
    {
      printf("v[%lu,%lu]: %22.18e\n", i,j, gsl_matrix_get(v, i, j));
    }
  }


  for(i = 0; i < M; i++)
  {
    printf("d[%lu]: %22.18e\n", i, gsl_vector_get(dv, i));
  }
  */

  /* put L and transpose(L) into separate matrices */

  for(i = 0; i < N ; i++)
  {
    for(j = 0; j < N; j++)
    {
      const double vij = gsl_matrix_get(v, i, j);
      gsl_matrix_set (l,  i, j, i>=j ? vij : 0);
      gsl_matrix_set (lt, i, j, i<=j ? vij : 0);
    }
  }

  /* put D into its own matrix */

  gsl_matrix_set_zero(dm);
  for(i = 0; i < M; ++i) gsl_matrix_set(dm, i, i, gsl_vector_get(dv, i));

  /* compute a = L * D * transpose(L); uses v for temp space */

  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, dm, lt, 0.0, v);
  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, l,   v, 0.0, a);

  for(i = 0; i < M; i++)
  {
    for(j = 0; j < N; j++)
    {
      const double aij = gsl_matrix_get(a, i, j);
      const double mij = gsl_matrix_get(m, i, j);
      int foo = check(aij, mij, eps);
      if(foo)
      {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, aij, mij);
      }
      s += foo;
    }
  }

  gsl_vector_free(dv);
  gsl_matrix_free(dm);
  gsl_matrix_free(lt);
  gsl_matrix_free(l);
  gsl_matrix_free(v);
  gsl_matrix_free(a);

  return s;
}

int test_cholesky_decomp_unit(void)
{
  int f;
  int s = 0;

  f = test_cholesky_decomp_unit_dim(hilb2, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_decomp_unit hilbert(2)");
  s += f;

  f = test_cholesky_decomp_unit_dim(hilb3, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_decomp_unit hilbert(3)");
  s += f;

  f = test_cholesky_decomp_unit_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_decomp_unit hilbert(4)");
  s += f;

  f = test_cholesky_decomp_unit_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  cholesky_decomp_unit hilbert(12)");
  s += f;

  return s;
}

int
test_choleskyc_solve_dim(const gsl_matrix_complex * m, const gsl_vector_complex * actual, double eps)
{
  int s = 0;
  unsigned long i, dim = m->size1;
  gsl_complex z;
  gsl_vector_complex * rhs = gsl_vector_complex_alloc(dim);
  gsl_matrix_complex * u  = gsl_matrix_complex_alloc(dim,dim);
  gsl_vector_complex * x = gsl_vector_complex_calloc(dim);
  GSL_SET_IMAG(&z, 0.0);
  gsl_matrix_complex_memcpy(u,m);
  for(i=0; i<dim; i++)
    {
      GSL_SET_REAL(&z, i + 1.0);
      gsl_vector_complex_set(rhs, i, z);
    }
  s += gsl_linalg_complex_cholesky_decomp(u);
  s += gsl_linalg_complex_cholesky_solve(u, rhs, x);
  for(i=0; i<dim; i++) {
    gsl_complex y = gsl_vector_complex_get(x, i);
    gsl_complex a = gsl_vector_complex_get(actual, i);
    int foo = check(GSL_REAL(y), GSL_REAL(a), eps);
    int foo2 = check(GSL_IMAG(y), GSL_IMAG(a), eps);
    if(foo || foo2) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, GSL_REAL(y), GSL_REAL(a));
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, GSL_IMAG(y), GSL_IMAG(a));
    }
    s += foo + foo2;
  }
  gsl_vector_complex_free(x);
  gsl_matrix_complex_free(u);
  gsl_vector_complex_free(rhs);

  return s;
} /* test_choleskyc_solve_dim() */

int
test_choleskyc_solve(void)
{
  double data7[] = { 66,0, 0,64, 126,63, 124,-62, 61,-61, 60,60, 0,-59,
                     0,-64, 65,0, 62,-124, -61,-122, -60,-60, 59,-59, -58,0,
                     126,-63, 62,124, 308,0, 180,-240, 59,-177, 174,58, -57,-114,
                     124,62, -61,122, 180,240, 299,0, 174,-58, 57,171, 56,-112,
                     61,61, -60,60, 59,177, 174,58, 119,0, 0,112, 55,-55,
                     60,-60, 59,59, 174,-58, 57,-171, 0,-112, 116,0, -54,-54,
                     0,59, -58,0, -57,114, 56,112, 55,55, -54,54, 60,0 };
  double data7_sol[] = { -0.524944196428570,0.209123883928571,
                         1.052873883928572,0.712444196428571,
                         0.117568824404762,0.443191964285714,
                         0.412862723214286,-0.356696428571429,
                         0.815931919642858,-0.265820312500000,
                         0.777929687500000,0.119484747023810,
                         1.058733258928571,-0.132087053571429 };
  gsl_matrix_complex_view cp7 = gsl_matrix_complex_view_array(data7, 7, 7);
  gsl_vector_complex_view cp7_sol = gsl_vector_complex_view_array(data7_sol, 7);
  int f;
  int s = 0;

  f = test_choleskyc_solve_dim(&cp7.matrix, &cp7_sol.vector, 1024.0 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  complex_cholesky_solve complex(7)");
  s += f;

  return s;
} /* test_choleskyc_solve() */

int
test_choleskyc_decomp_dim(const gsl_matrix_complex * m, double eps)
{
  int s = 0;
  unsigned long i,j, M = m->size1, N = m->size2;

  gsl_matrix_complex * v  = gsl_matrix_complex_alloc(M,N);
  gsl_matrix_complex * a  = gsl_matrix_complex_alloc(M,N);
  gsl_matrix_complex * l  = gsl_matrix_complex_alloc(M,N);
  gsl_matrix_complex * lh  = gsl_matrix_complex_alloc(N,N);

  gsl_matrix_complex_memcpy(v, m);
  gsl_matrix_complex_set_zero(l);
  gsl_matrix_complex_set_zero(lh);

  s += gsl_linalg_complex_cholesky_decomp(v);

  /* Compute L L^H */
  
  for (i = 0; i < N ; i++)
    {
      for (j = 0; j <= i; j++)
        {
          gsl_complex vij = gsl_matrix_complex_get(v, i, j);
          gsl_matrix_complex_set (l, i, j, vij);
          gsl_matrix_complex_set (lh, j, i, gsl_complex_conjugate(vij));
        }
    }
            
  /* compute a = l lh */
  gsl_blas_zgemm (CblasNoTrans,
                  CblasNoTrans,
                  GSL_COMPLEX_ONE,
                  l,
                  lh,
                  GSL_COMPLEX_ZERO,
                  a);

  for(i=0; i<M; i++) {
    for(j=0; j<N; j++) {
      gsl_complex aij = gsl_matrix_complex_get(a, i, j);
      gsl_complex mij = gsl_matrix_complex_get(m, i, j);
      int foo_r = check(GSL_REAL(aij), GSL_REAL(mij), eps);
      int foo_i = check(GSL_IMAG(aij), GSL_IMAG(mij), eps);
      if(foo_r || foo_i) {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, GSL_REAL(aij), GSL_REAL(mij));
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, GSL_IMAG(aij), GSL_IMAG(mij));
      }
      s += foo_r + foo_i;
    }
  }

  gsl_matrix_complex_free(v);
  gsl_matrix_complex_free(a);
  gsl_matrix_complex_free(l);
  gsl_matrix_complex_free(lh);

  return s;
}

int
test_choleskyc_decomp(void)
{
  int f;
  int s = 0;
  double dat3[] = { 59.75,0,       49.25,172.25, 66.75,-162.75,
                    49.25,-172.25, 555.5,0,      -429,-333.5,
                    66.75,162.75,  -429,333.5,   536.5,0 };
  gsl_matrix_complex_view p3 = gsl_matrix_complex_view_array(dat3, 3, 3);

  f = test_choleskyc_decomp_dim(&p3.matrix, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  complex_cholesky_decomp complex(3)");
  s += f;

  return s;
}



int
test_choleskyc_invert_dim(const gsl_matrix_complex * m, double eps)
{
  int s = 0;
  unsigned long i, j, N = m->size1;
  gsl_complex af, bt;
  gsl_matrix_complex * v  = gsl_matrix_complex_alloc(N, N);
  gsl_matrix_complex * c  = gsl_matrix_complex_alloc(N, N);

  gsl_matrix_complex_memcpy(v, m);

  s += gsl_linalg_complex_cholesky_decomp(v);
  s += gsl_linalg_complex_cholesky_invert(v);

  GSL_SET_COMPLEX(&af, 1.0, 0.0);
  GSL_SET_COMPLEX(&bt, 0.0, 0.0);
  gsl_blas_zhemm(CblasLeft, CblasUpper, af, m, v, bt, c);

  /* c should be the identity matrix */

  for (i = 0; i < N; ++i)
    {
      for (j = 0; j < N; ++j)
        {
          int foo;
          gsl_complex cij = gsl_matrix_complex_get(c, i, j);
          double expected, actual;

          /* check real part */
          if (i == j)
            expected = 1.0;
          else
            expected = 0.0;
          actual = GSL_REAL(cij);
          foo = check(actual, expected, eps);
          if (foo)
            printf("REAL(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", 
                   N, N, i,j, actual, expected);
          s += foo;

          /* check imaginary part */
          expected = 0.0;
          actual = GSL_IMAG(cij);
          foo = check(actual, expected, eps);
          if (foo)
            printf("IMAG(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", 
                   N, N, i,j, actual, expected);
          s += foo;

        }
    }

  gsl_matrix_complex_free(v);
  gsl_matrix_complex_free(c);

  return s;
}

int
test_choleskyc_invert(void)
{
  int f;
  int s = 0;

  double dat2[] = { 
      92.303, 0.000,    10.858, 1.798,    
      10.858, -1.798,    89.027, 0.000 
  }; 

  double dat3[] = { 
      59.75,0,       49.25,172.25, 66.75,-162.75,
      49.25,-172.25, 555.5,0,      -429,-333.5,
      66.75,162.75,  -429,333.5,   536.5,0 
  };

  double dat4[] = { 
      102.108, 0.000,    14.721, 1.343,    -17.480, 15.591,    3.308, -2.936,    
      14.721, -1.343,    101.970, 0.000,    11.671, -6.776,    -5.009, -2.665,    
      -17.480, -15.591,    11.671, 6.776,    105.071, 0.000,    3.396, 6.276,    
      3.308, 2.936,    -5.009, 2.665,    3.396, -6.276,    107.128, 0.000 
  }; 

  {
    gsl_matrix_complex_view rv2 = gsl_matrix_complex_view_array(dat2, 2, 2);
    f = test_choleskyc_invert_dim(&rv2.matrix, 2 * 8.0 * GSL_DBL_EPSILON);
    gsl_test(f, "  choleskyc_invert 2x2 Hermitian");
    s += f;
  }

  { 
    gsl_matrix_complex_view rv3 = gsl_matrix_complex_view_array(dat3, 3, 3);
    f = test_choleskyc_invert_dim(&rv3.matrix, 2 * 1024.0 * GSL_DBL_EPSILON);
    gsl_test(f, "  choleskyc_invert 3x3 Hermitian");
    s += f;
  }

  {
    gsl_matrix_complex_view rv4 = gsl_matrix_complex_view_array(dat4, 4, 4);
    f = test_choleskyc_invert_dim(&rv4.matrix, 2 * 64.0 * GSL_DBL_EPSILON);
    gsl_test(f, "  choleskyc_invert 4x4 Hermitian");
    s += f;
  }

  return s;
}



int
test_HH_solve_dim(const gsl_matrix * m, const double * actual, double eps)
{
  int s = 0;
  unsigned long i, dim = m->size1;

  gsl_permutation * perm = gsl_permutation_alloc(dim);
  gsl_matrix * hh  = gsl_matrix_alloc(dim,dim);
  gsl_vector * d = gsl_vector_alloc(dim);
  gsl_vector * x = gsl_vector_alloc(dim);
  gsl_matrix_memcpy(hh,m);
  for(i=0; i<dim; i++) gsl_vector_set(x, i, i+1.0);
  s += gsl_linalg_HH_svx(hh, x);
  for(i=0; i<dim; i++) {
    int foo = check(gsl_vector_get(x, i),actual[i],eps);
    if( foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }
  gsl_vector_free(x);
  gsl_vector_free(d);
  gsl_matrix_free(hh);
  gsl_permutation_free(perm);

  return s;
}

int test_HH_solve(void)
{
  int f;
  int s = 0;

  f = test_HH_solve_dim(hilb2, hilb2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  HH_solve hilbert(2)");
  s += f;

  f = test_HH_solve_dim(hilb3, hilb3_solution, 128.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  HH_solve hilbert(3)");
  s += f;

  f = test_HH_solve_dim(hilb4, hilb4_solution, 2.0 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  HH_solve hilbert(4)");
  s += f;

  f = test_HH_solve_dim(hilb12, hilb12_solution, 0.5);
  gsl_test(f, "  HH_solve hilbert(12)");
  s += f;

  f = test_HH_solve_dim(vander2, vander2_solution, 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  HH_solve vander(2)");
  s += f;

  f = test_HH_solve_dim(vander3, vander3_solution, 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  HH_solve vander(3)");
  s += f;

  f = test_HH_solve_dim(vander4, vander4_solution, 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  HH_solve vander(4)");
  s += f;

  f = test_HH_solve_dim(vander12, vander12_solution, 0.05);
  gsl_test(f, "  HH_solve vander(12)");
  s += f;

  return s;
}


int
test_TDS_solve_dim(unsigned long dim, double d, double od, const double * actual, double eps)
{
  int s = 0;
  unsigned long i;

  gsl_vector * offdiag = vector_alloc(dim-1);
  gsl_vector * diag = vector_alloc(dim);
  gsl_vector * rhs = vector_alloc(dim);
  gsl_vector * x = vector_alloc(dim);

  for(i=0; i<dim; i++) {
    gsl_vector_set(diag, i, d);
    gsl_vector_set(rhs,  i, i + 1.0);
  }
  for(i=0; i<dim-1; i++) {
    gsl_vector_set(offdiag, i, od);
  }

  s += gsl_linalg_solve_symm_tridiag(diag, offdiag, rhs, x);

  for(i=0; i<dim; i++) {
    double si = gsl_vector_get(x, i);
    double ai = actual[i];
    int foo = check(si, ai, eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  vector_free(x);
  vector_free(rhs);
  vector_free(diag);
  vector_free(offdiag);

  return s;
}


int test_TDS_solve(void)
{
  int f;
  int s = 0;
  
  {
    double actual[] =  {0.0, 2.0};
    f = test_TDS_solve_dim(2, 1.0, 0.5, actual, 8.0 * GSL_DBL_EPSILON);
    gsl_test(f, "  solve_TDS dim=2 A");
    s += f;
  }

  {
    double actual[] =  {3.0/8.0, 15.0/8.0};
    f = test_TDS_solve_dim(2, 1.0, 1.0/3.0, actual, 8.0 * GSL_DBL_EPSILON);
    gsl_test(f, "  solve_TDS dim=2 B");
    s += f;
  }

  {
    double actual[] =  {5.0/8.0, 9.0/8.0, 2.0, 15.0/8.0, 35.0/8.0};
    f = test_TDS_solve_dim(5, 1.0, 1.0/3.0, actual, 8.0 * GSL_DBL_EPSILON);
    gsl_test(f, "  solve_TDS dim=5");
    s += f;
  }

  return s;
}

int
test_TDS_cyc_solve_one(const unsigned long dim,
                       const double * d, const double * od,
                       const double * r, const double * actual, double eps)
{
  int s = 0;
  unsigned long i;

  gsl_vector * offdiag = vector_alloc(dim);
  gsl_vector * diag = vector_alloc(dim);
  gsl_vector * rhs = vector_alloc(dim);
  gsl_vector * x = vector_alloc(dim);

  for(i=0; i<dim; i++) {
    gsl_vector_set(diag, i, d[i]);
    gsl_vector_set(rhs,  i, r[i]);
    gsl_vector_set(offdiag, i, od[i]);
  }

  s += gsl_linalg_solve_symm_cyc_tridiag(diag, offdiag, rhs, x);

  for(i=0; i<dim; i++) {
    double si = gsl_vector_get(x, i);
    double ai = actual[i];
    int foo = check(si, ai, eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  vector_free(x);
  vector_free(rhs);
  vector_free(diag);
  vector_free(offdiag);

  return s;
}

int test_TDS_cyc_solve(void)
{
  int f;
  int s = 0;

#ifdef SUPPORT_UNDERSIZE_CYC
  {
    unsigned long dim = 1;
    double diag[] = {  2 };
    double offdiag[] = { 3 };
    double rhs[] = { 7 };
    double actual[] = { 3.5 };
    
    f = test_TDS_cyc_solve_one(dim, diag, offdiag, rhs, actual, 28.0 * GSL_DBL_EPSILON);
    gsl_test(f, "  solve_TDS_cyc dim=%lu A", dim);
    s += f;
  }

  {
    unsigned long dim = 2;
    double diag[] = { 1, 2 };
    double offdiag[] = { 3, 4 };
    double rhs[] = { 7, -7 };
    double actual[] = { -5, 4 };
    
    f = test_TDS_cyc_solve_one(dim, diag, offdiag, rhs, actual, 28.0 * GSL_DBL_EPSILON);
    gsl_test(f, "  solve_TDS_cyc dim=%lu A", dim);
    s += f;
  }
#endif

  {
    unsigned long dim = 3;
    double diag[] = { 1, 1, 1 };
    double offdiag[] = { 3, 3, 3 };
    double rhs[] = { 7, -7, 7 };
    double actual[] = { -2, 5, -2 };
    
    f = test_TDS_cyc_solve_one(dim, diag, offdiag, rhs, actual, 28.0 * GSL_DBL_EPSILON);
    gsl_test(f, "  solve_TDS_cyc dim=%lu A", dim);
    s += f;
  }

  {
    unsigned long dim = 5;
    double diag[] = { 4, 2, 1, 2, 4 };
    double offdiag[] = { 1, 1, 1, 1, 1 };
    double rhs[] = { 30, -24, 3, 21, -30 };
    double actual[] = { 12, 3, -42, 42, -21 };

    /*  f = test_TDS_cyc_solve_one(dim, diag, offdiag, rhs, actual, 7.0 * GSL_DBL_EPSILON);
        FIXME: bad accuracy */
    f = test_TDS_cyc_solve_one(dim, diag, offdiag, rhs, actual, 40.0 * GSL_DBL_EPSILON);
    gsl_test(f, "  solve_TDS_cyc dim=%lu B", dim);
    s += f;
  }

  return s;
}

int
test_TDN_solve_dim(unsigned long dim, double d, double a, double b, const double * actual, double eps)
{
  int s = 0;
  unsigned long i;

  gsl_vector * abovediag = vector_alloc(dim-1);
  gsl_vector * belowdiag = vector_alloc(dim-1);
  gsl_vector * diag = vector_alloc(dim);
  gsl_vector * rhs = vector_alloc(dim);
  gsl_vector * x = vector_alloc(dim);

  for(i=0; i<dim; i++) {
    gsl_vector_set(diag, i, d);
    gsl_vector_set(rhs,  i, i + 1.0);
  }
  for(i=0; i<dim-1; i++) {
    gsl_vector_set(abovediag, i, a);
    gsl_vector_set(belowdiag, i, b);
  }

  s += gsl_linalg_solve_tridiag(diag, abovediag, belowdiag, rhs, x);

  for(i=0; i<dim; i++) {
    double si = gsl_vector_get(x, i);
    double ai = actual[i];
    int foo = check(si, ai, eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  vector_free(x);
  vector_free(rhs);
  vector_free(diag);
  vector_free(abovediag);
  vector_free(belowdiag);

  return s;
}


int test_TDN_solve(void)
{
  int f;
  int s = 0;
  double actual[16];

  actual[0] =  -7.0/3.0;
  actual[1] =  5.0/3.0;
  actual[2] =  4.0/3.0;
  f = test_TDN_solve_dim(3, 1.0, 2.0, 1.0, actual, 2.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  solve_TDN dim=2 A");
  s += f;

  actual[0] =  0.75;
  actual[1] =  0.75;
  actual[2] =  2.625;

  f = test_TDN_solve_dim(3, 1.0, 1.0/3.0, 1.0/2.0, actual, 2.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  solve_TDN dim=2 B");
  s += f;

  actual[0] =  99.0/140.0;
  actual[1] =  41.0/35.0;
  actual[2] =  19.0/10.0;
  actual[3] =  72.0/35.0;
  actual[4] =  139.0/35.0;
  f = test_TDN_solve_dim(5, 1.0, 1.0/4.0, 1.0/2.0, actual, 35.0/8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  solve_TDN dim=5");
  s += f;

  return s;
}

int
test_TDN_cyc_solve_dim(unsigned long dim, double d, double a, double b, const double * actual, double eps)
{
  int s = 0;
  unsigned long i;

  gsl_vector * abovediag = vector_alloc(dim);
  gsl_vector * belowdiag = vector_alloc(dim);
  gsl_vector * diag = vector_alloc(dim);
  gsl_vector * rhs = vector_alloc(dim);
  gsl_vector * x = vector_alloc(dim);

  for(i=0; i<dim; i++) {
    gsl_vector_set(diag, i, d);
    gsl_vector_set(rhs,  i, i + 1.0);
  }
  for(i=0; i<dim; i++) {
    gsl_vector_set(abovediag, i, a);
    gsl_vector_set(belowdiag, i, b);
  }

  s += gsl_linalg_solve_cyc_tridiag(diag, abovediag, belowdiag, rhs, x);

  for(i=0; i<dim; i++) {
    double si = gsl_vector_get(x, i);
    double ai = actual[i];
    int foo = check(si, ai, eps);
    if(foo) {
      printf("%3lu[%lu]: %22.18g   %22.18g\n", dim, i, gsl_vector_get(x, i), actual[i]);
    }
    s += foo;
  }

  vector_free(x);
  vector_free(rhs);
  vector_free(diag);
  vector_free(abovediag);
  vector_free(belowdiag);

  return s;
}


int test_TDN_cyc_solve(void)
{
  int f;
  int s = 0;
  double actual[16];

  actual[0] =  3.0/2.0;
  actual[1] = -1.0/2.0;
  actual[2] =  1.0/2.0;
  f = test_TDN_cyc_solve_dim(3, 1.0, 2.0, 1.0, actual, 32.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  solve_TDN_cyc dim=2 A");
  s += f;

  actual[0] = -5.0/22.0;
  actual[1] = -3.0/22.0;
  actual[2] =  29.0/22.0;
  actual[3] = -9.0/22.0;
  actual[4] =  43.0/22.0;

  f = test_TDN_cyc_solve_dim(5, 3.0, 2.0, 1.0, actual, 66.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  solve_TDN_cyc dim=5");
  s += f;

  return s;
}

int
test_bidiag_decomp_dim(const gsl_matrix * m, double eps)
{
  int s = 0;
  unsigned long i,j,k,r, M = m->size1, N = m->size2;

  gsl_matrix * A  = gsl_matrix_alloc(M,N);
  gsl_matrix * a  = gsl_matrix_alloc(M,N);
  gsl_matrix * b  = gsl_matrix_alloc(N,N);

  gsl_matrix * u  = gsl_matrix_alloc(M,N);
  gsl_matrix * v  = gsl_matrix_alloc(N,N);

  gsl_vector * tau1  = gsl_vector_alloc(N);
  gsl_vector * tau2  = gsl_vector_alloc(N-1);
  gsl_vector * d  = gsl_vector_alloc(N);
  gsl_vector * sd  = gsl_vector_alloc(N-1);

  gsl_matrix_memcpy(A,m);

  s += gsl_linalg_bidiag_decomp(A, tau1, tau2);
  s += gsl_linalg_bidiag_unpack(A, tau1, u, tau2, v, d, sd);

  gsl_matrix_set_zero(b);
  for (i = 0; i < N; i++) gsl_matrix_set(b, i,i, gsl_vector_get(d,i));
  for (i = 0; i < N-1; i++) gsl_matrix_set(b, i,i+1, gsl_vector_get(sd,i));
  
  /* Compute A = U B V^T */
  
  for (i = 0; i < M ; i++)
    {
      for (j = 0; j < N; j++)
        {
          double sum = 0;

          for (k = 0; k < N; k++)
            {
              for (r = 0; r < N; r++)
                {
                  sum += gsl_matrix_get(u, i, k) * gsl_matrix_get (b, k, r)
                    * gsl_matrix_get(v, j, r);
                }
            }
          gsl_matrix_set (a, i, j, sum);
        }
    }

  for(i=0; i<M; i++) {
    for(j=0; j<N; j++) {
      double aij = gsl_matrix_get(a, i, j);
      double mij = gsl_matrix_get(m, i, j);
      int foo = check(aij, mij, eps);
      if(foo) {
        printf("(%3lu,%3lu)[%lu,%lu]: %22.18g   %22.18g\n", M, N, i,j, aij, mij);
      }
      s += foo;
    }
  }

  gsl_matrix_free(A);
  gsl_matrix_free(a);
  gsl_matrix_free(u);
  gsl_matrix_free(v);
  gsl_matrix_free(b);
  gsl_vector_free(tau1);
  gsl_vector_free(tau2);
  gsl_vector_free(d);
  gsl_vector_free(sd);

  return s;
}

int test_bidiag_decomp(void)
{
  int f;
  int s = 0;

  f = test_bidiag_decomp_dim(m53, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  bidiag_decomp m(5,3)");
  s += f;

  f = test_bidiag_decomp_dim(m97, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  bidiag_decomp m(9,7)");
  s += f;

  f = test_bidiag_decomp_dim(hilb2, 2 * 8.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  bidiag_decomp hilbert(2)");
  s += f;

  f = test_bidiag_decomp_dim(hilb3, 2 * 64.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  bidiag_decomp hilbert(3)");
  s += f;

  f = test_bidiag_decomp_dim(hilb4, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  bidiag_decomp hilbert(4)");
  s += f;

  f = test_bidiag_decomp_dim(hilb12, 2 * 1024.0 * GSL_DBL_EPSILON);
  gsl_test(f, "  bidiag_decomp hilbert(12)");
  s += f;

  return s;
}

void
my_error_handler (const char *reason, const char *file, int line, int err)
{
  if (1) printf ("(caught [%s:%d: %s (%d)])\n", file, line, reason, err) ;
}

int main(void)
{
  gsl_ieee_env_setup ();
  gsl_set_error_handler (&my_error_handler);

  m11 = create_general_matrix(1,1);
  m51 = create_general_matrix(5,1);

  m35 = create_general_matrix(3,5);
  m53 = create_general_matrix(5,3);
  m97 = create_general_matrix(9,7);

  s35 = create_singular_matrix(3,5);
  s53 = create_singular_matrix(5,3);

  hilb2 = create_hilbert_matrix(2);
  hilb3 = create_hilbert_matrix(3);
  hilb4 = create_hilbert_matrix(4);
  hilb12 = create_hilbert_matrix(12);

  vander2 = create_vandermonde_matrix(2);
  vander3 = create_vandermonde_matrix(3);
  vander4 = create_vandermonde_matrix(4);
  vander12 = create_vandermonde_matrix(12);

  moler10 = create_moler_matrix(10);

  c7 = create_complex_matrix(7);

  row3 = create_row_matrix(3,3);
  row5 = create_row_matrix(5,5);
  row12 = create_row_matrix(12,12);

  A22 = create_2x2_matrix (0.0, 0.0, 0.0, 0.0);
  A33 = gsl_matrix_alloc(3,3);
  A44 = gsl_matrix_alloc(4,4);
  A55 = gsl_matrix_alloc(5,5);

  inf5 = create_diagonal_matrix (inf5_data, 5);
  gsl_matrix_set(inf5, 3, 3, GSL_POSINF);

  nan5 = create_diagonal_matrix (inf5_data, 5);
  gsl_matrix_set(nan5, 3, 3, GSL_NAN);

  dblmin3 = create_general_matrix (3, 3);
  gsl_matrix_scale(dblmin3, GSL_DBL_MIN);

  dblmin5 = create_general_matrix (5, 5);
  gsl_matrix_scale(dblmin5, GSL_DBL_MIN);

  dblsubnorm5 = create_general_matrix (5, 5);
  gsl_matrix_scale(dblsubnorm5, GSL_DBL_MIN/1024);
  gsl_matrix_set(dblsubnorm5, 0, 0, 0.0);

  bigsparse = create_sparse_matrix(100, 100);

  /* Matmult now obsolete */
#ifdef MATMULT
  gsl_test(test_matmult(),               "Matrix Multiply"); 
  gsl_test(test_matmult_mod(),           "Matrix Multiply with Modification"); 
#endif
  gsl_test(test_bidiag_decomp(),         "Bidiagonal Decomposition");
  gsl_test(test_LU_solve(),              "LU Decomposition and Solve");
  gsl_test(test_LUc_solve(),             "Complex LU Decomposition and Solve");
  gsl_test(test_QR_decomp(),             "QR Decomposition");
  gsl_test(test_QR_solve(),              "QR Solve");
  gsl_test(test_LQ_solve(),              "LQ Solve");
  gsl_test(test_PTLQ_solve(),            "PTLQ Solve");

  gsl_test(test_LQ_decomp(),             "LQ Decomposition");
  gsl_test(test_LQ_LQsolve(),            "LQ LQ Solve");
  gsl_test(test_LQ_lssolve(),            "LQ LS Solve");
  gsl_test(test_LQ_update(),             "LQ Rank-1 Update");
  gsl_test(test_QRPT_decomp(),           "PTLQ Decomposition");
  gsl_test(test_PTLQ_solve(),            "PTLQ Solve");

  gsl_test(test_QR_QRsolve(),            "QR QR Solve");
  gsl_test(test_QR_lssolve(),            "QR LS Solve");
  gsl_test(test_QR_update(),             "QR Rank-1 Update");
  gsl_test(test_QRPT_decomp(),           "QRPT Decomposition");
  gsl_test(test_QRPT_solve(),            "QRPT Solve");
  gsl_test(test_QRPT_QRsolve(),          "QRPT QR Solve");
  gsl_test(test_QRPT_update(),           "QRPT Rank-1 Update");
  gsl_test(test_SV_decomp(),             "Singular Value Decomposition");
  gsl_test(test_SV_decomp_jacobi(),        "Singular Value Decomposition (Jacobi)");
  gsl_test(test_SV_decomp_mod(),         "Singular Value Decomposition (Mod)");
  gsl_test(test_SV_solve(),              "SVD Solve");
  gsl_test(test_cholesky_decomp(),       "Cholesky Decomposition");
  gsl_test(test_cholesky_decomp_unit(),  "Cholesky Decomposition [unit triangular]");
  gsl_test(test_cholesky_solve(),        "Cholesky Solve");
  gsl_test(test_cholesky_invert(),       "Cholesky Inverse");
  gsl_test(test_choleskyc_decomp(),      "Complex Cholesky Decomposition");
  gsl_test(test_choleskyc_solve(),       "Complex Cholesky Solve");
  gsl_test(test_choleskyc_invert(),      "Complex Cholesky Inverse");

  gsl_test(test_HH_solve(),              "Householder solve");
  gsl_test(test_TDS_solve(),             "Tridiagonal symmetric solve");
  gsl_test(test_TDS_cyc_solve(),         "Tridiagonal symmetric cyclic solve");
  gsl_test(test_TDN_solve(),             "Tridiagonal nonsymmetric solve");
  gsl_test(test_TDN_cyc_solve(),         "Tridiagonal nonsymmetric cyclic solve");

  gsl_matrix_free(m11);
  gsl_matrix_free(m35);
  gsl_matrix_free(m51);
  gsl_matrix_free(m53);
  gsl_matrix_free(m97);
  gsl_matrix_free(s35);
  gsl_matrix_free(s53);

  gsl_matrix_free(hilb2);
  gsl_matrix_free(hilb3);
  gsl_matrix_free(hilb4);
  gsl_matrix_free(hilb12);

  gsl_matrix_free(vander2);
  gsl_matrix_free(vander3);
  gsl_matrix_free(vander4);
  gsl_matrix_free(vander12);

  gsl_matrix_free(moler10);

  gsl_matrix_complex_free(c7);
  gsl_matrix_free(row3);
  gsl_matrix_free(row5);
  gsl_matrix_free(row12);

  gsl_matrix_free(A22);
  gsl_matrix_free(A33);
  gsl_matrix_free(A44);
  gsl_matrix_free(A55);

  gsl_matrix_free (inf5);
  gsl_matrix_free (nan5);

  gsl_matrix_free (dblmin3);
  gsl_matrix_free (dblmin5);
  gsl_matrix_free (dblsubnorm5);

  gsl_matrix_free (bigsparse);

  exit (gsl_test_summary());
}
