/**
 *
 * @file testing_spotri.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Hatem Ltaief
 * @date 2010-11-15
 * @generated s Fri Apr  1 11:03:05 2016
 *
 **/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include <plasma.h>
#include <cblas.h>
#include <lapacke.h>
#include <core_blas.h>
#include "testing_smain.h"

enum blas_order_type {
            blas_rowmajor = 101,
            blas_colmajor = 102 };

enum blas_cmach_type {
            blas_base      = 151,
            blas_t         = 152,
            blas_rnd       = 153,
            blas_ieee      = 154,
            blas_emin      = 155,
            blas_emax      = 156,
            blas_eps       = 157,
            blas_prec      = 158,
            blas_underflow = 159,
            blas_overflow  = 160,
            blas_sfmin     = 161};

enum blas_norm_type {
            blas_one_norm       = 171,
            blas_real_one_norm  = 172,
            blas_two_norm       = 173,
            blas_frobenius_norm = 174,
            blas_inf_norm       = 175,
            blas_real_inf_norm  = 176,
            blas_max_norm       = 177,
            blas_real_max_norm  = 178 };

static void
BLAS_error(char *rname, int err, int val, int x) {
  fprintf( stderr, "%s %d %d %d\n", rname, err, val, x );
  abort();
}

static
void
BLAS_sge_norm(enum blas_order_type order, enum blas_norm_type norm,
  int m, int n, const float *a, int lda, float *res) {
  int i, j; float anorm, v;
  char rname[] = "BLAS_sge_norm";

  if (order != blas_colmajor) BLAS_error( rname, -1, order, 0 );

  if (norm == blas_frobenius_norm) {
    anorm = 0.0f;
    for (j = n; j; --j) {
      for (i = m; i; --i) {
        v = a[0];
        anorm += v * v;
        a++;
      }
      a += lda - m;
    }
    anorm = sqrt( anorm );
  } else if (norm == blas_inf_norm) {
    anorm = 0.0f;
    for (i = 0; i < m; ++i) {
      v = 0.0f;
      for (j = 0; j < n; ++j) {
        v += fabsf( a[i + j * lda] );
      }
      if (v > anorm)
        anorm = v;
    }
  } else if (norm == blas_one_norm) {
    anorm = 0.0f;
    for (i = 0; i < m; ++i) {
      v = 0.0f;
      for (j = 0; j < n; ++j) {
        v += fabsf( a[i + j * lda] );
      }
      if (v > anorm)
        anorm = v;
    }
  } else {
    BLAS_error( rname, -2, norm, 0 );
    return;
  }

  if (res) *res = anorm;
}

static
float
BLAS_spow_di(float x, int n) {
  float rv = 1.0;

  if (n < 0) {
    n = -n;
    x = 1.0 / x;
  }

  for (; n; n >>= 1, x *= x) {
    if (n & 1)
      rv *= x;
  }

  return rv;
}

static
float
BLAS_sfpinfo(enum blas_cmach_type cmach) {
  float eps = 1.0, r = 1.0, o = 1.0, b = 2.0;
  int t = 53, l = 1024, m = -1021;
  char rname[] = "BLAS_sfpinfo";

  if ((sizeof eps) == sizeof(float)) {
    t = 24;
    l = 128;
    m = -125;
  } else {
    t = 53; 
    l = 1024;
    m = -1021;
  }

  /* for (i = 0; i < t; ++i) eps *= half; */
  eps = BLAS_spow_di( b, -t );
  /* for (i = 0; i >= m; --i) r *= half; */
  r = BLAS_spow_di( b, m-1 );

  o -= eps;
  /* for (i = 0; i < l; ++i) o *= b; */
  o = (o * BLAS_spow_di( b, l-1 )) * b;

  switch (cmach) {
    case blas_eps: return eps;
    case blas_sfmin: return r;
    default:
      BLAS_error( rname, -1, cmach, 0 );
      break;
  }
  return 0.0;
}

static int check_factorization(int, float*, float*, int, int, float);
static int check_inverse(int, float *, float *, int, int, float);

int testing_spotri(int argc, char **argv)
{

    /* Check for number of arguments*/
    if (argc != 2){
        USAGE("POTRI", "N LDA",
              "   - N    : the size of the matrix\n"
              "   - LDA  : leading dimension of the matrix A\n");
        return -1;
    }

    int N     = atoi(argv[0]);
    int LDA   = atoi(argv[1]);
    float eps;
    int u;
    int info_inverse, info_factorization;

    float *A1   = (float *)malloc(LDA*N*sizeof(float));
    float *A2   = (float *)malloc(LDA*N*sizeof(float));
    float *WORK = (float *)malloc(2*LDA*sizeof(float));

    /* Check if unable to allocate memory */
    if ((!A1)||(!A2)){
        printf("Out of Memory \n ");
        return -2;
    }

    eps = BLAS_sfpinfo( blas_eps );

    /*-------------------------------------------------------------
    *  TESTING SPOTRI
    */
    for(u=0; u<2; u++) {

        /* Initialize A1 and A2 for Symmetric Positif Matrix */
        PLASMA_splgsy( (float)N, N, A1, LDA, 51 );
        PLASMA_slacpy( PlasmaUpperLower, N, N, A1, LDA, A2, LDA );

        printf("\n");
        printf("------ TESTS FOR PLASMA SPOTRI ROUTINE -------  \n");
        printf("            Size of the Matrix %d by %d\n", N, N);
        printf("\n");
        printf(" The matrix A is randomly generated for each test.\n");
        printf("============\n");
        printf(" The relative machine precision (eps) is to be %e \n", eps);
        printf(" Computational tests pass if scaled residuals are less than 60.\n");

        /* PLASMA SPOTRF */
        PLASMA_spotrf(uplo[u], N, A2, LDA);

        /* Check the factorization */
        info_factorization = check_factorization( N, A1, A2, LDA, uplo[u], eps);

        /* PLASMA SPOTRI */
        PLASMA_spotri(uplo[u], N, A2, LDA);

        /* Check the inverse */
        info_inverse = check_inverse(N, A1, A2, LDA, uplo[u], eps);

        if ( (info_inverse == 0) && (info_factorization == 0) ) {
            printf("***************************************************\n");
            printf(" ---- TESTING SPOTRI(%s)................. PASSED !\n", uplostr[u]);
            printf("***************************************************\n");
        }
        else {
            printf("***************************************************\n");
            printf(" - TESTING SPOTRI(%s) ... FAILED !\n", uplostr[u]);
            printf("***************************************************\n");
        }
    }

    free(A1); free(A2); free(WORK);

    return 0;
}


/*------------------------------------------------------------------------
 *  Check the factorization of the matrix A2
 */
static int check_factorization(int N, float *A1, float *A2, int LDA, int uplo, float eps)
{
    float Anorm, Rnorm;
    float alpha;
    int info_factorization;
    int i,j;

    float *Residual = (float *)malloc(N*N*sizeof(float));
    float *L1       = (float *)malloc(N*N*sizeof(float));
    float *L2       = (float *)malloc(N*N*sizeof(float));
    float *work              = (float *)malloc(N*sizeof(float));

    memset((void*)L1, 0, N*N*sizeof(float));
    memset((void*)L2, 0, N*N*sizeof(float));

    alpha= 1.0;

    LAPACKE_slacpy_work(LAPACK_COL_MAJOR,' ', N, N, A1, LDA, Residual, N);

    /* Dealing with L'L or U'U  */
    if (uplo == PlasmaUpper){
        LAPACKE_slacpy_work(LAPACK_COL_MAJOR,'u', N, N, A2, LDA, L1, N);
        LAPACKE_slacpy_work(LAPACK_COL_MAJOR,'u', N, N, A2, LDA, L2, N);
        cblas_strmm(CblasColMajor, CblasLeft, CblasUpper, CblasTrans, CblasNonUnit, N, N, (alpha), L1, N, L2, N);
    }
    else{
        LAPACKE_slacpy_work(LAPACK_COL_MAJOR,'l', N, N, A2, LDA, L1, N);
        LAPACKE_slacpy_work(LAPACK_COL_MAJOR,'l', N, N, A2, LDA, L2, N);
        cblas_strmm(CblasColMajor, CblasRight, CblasLower, CblasTrans, CblasNonUnit, N, N, (alpha), L1, N, L2, N);
    }

    /* Compute the Residual || A -L'L|| */
    for (i = 0; i < N; i++)
        for (j = 0; j < N; j++)
           Residual[j*N+i] = L2[j*N+i] - Residual[j*N+i];

    BLAS_sge_norm( blas_colmajor, blas_inf_norm, N, N, Residual, N, &Rnorm );
    BLAS_sge_norm( blas_colmajor, blas_inf_norm, N, N, A1, LDA, &Anorm );

    printf("============\n");
    printf("Checking the Cholesky Factorization \n");
    printf("-- ||L'L-A||_oo/(||A||_oo.N.eps) = %e \n",Rnorm/(Anorm*N*eps));

    if ( isnan(Rnorm/(Anorm*N*eps)) || isinf(Rnorm/(Anorm*N*eps)) || (Rnorm/(Anorm*N*eps) > 60.0) ){
        printf("-- Factorization is suspicious ! \n");
        info_factorization = 1;
    }
    else{
        printf("-- Factorization is CORRECT ! \n");
        info_factorization = 0;
    }

    free(Residual); free(L1); free(L2); free(work);

    return info_factorization;
}


/*------------------------------------------------------------------------
 *  Check the accuracy of the computed inverse
 */

static int check_inverse(int N, float *A1, float *A2, int LDA, int uplo, float eps )
{
    int info_inverse;
    int i, j;
    float Rnorm, Anorm, Ainvnorm, result;
    float alpha, beta, zone;
    float *work = (float *)malloc(N*N*sizeof(float));

    alpha = -1.0;
    beta  = 0.0;
    zone = 1.0;

    /* Rebuild the other part of the inverse matrix */
    if(uplo == PlasmaUpper){
       for(j=0; j<N; j++)
          for(i=0; i<j; i++)
             *(A2+j+i*LDA) = *(A2+i+j*LDA);
       cblas_ssymm(CblasColMajor, CblasLeft, CblasUpper, N, N, (alpha), A2, LDA, A1, LDA, (beta), work, N);

    }
    else {
       for(j=0; j<N; j++)
          for(i=j; i<N; i++)
             *(A2+j+i*LDA) = *(A2+i+j*LDA);
       cblas_ssymm(CblasColMajor, CblasLeft, CblasLower, N, N, (alpha), A2, LDA, A1, LDA, (beta), work, N);
    }
    
    /* Add the identity matrix to work */
    for(i=0; i<N; i++)
        *(work+i+i*N) = *(work+i+i*N) + zone;

    BLAS_sge_norm( blas_colmajor, blas_one_norm, N, N, work, N, &Rnorm );
    BLAS_sge_norm( blas_colmajor, blas_one_norm, N, N, A1, LDA, &Anorm );
    BLAS_sge_norm( blas_colmajor, blas_one_norm, N, N, A2, LDA, &Ainvnorm );

    if (getenv("PLASMA_TESTING_VERBOSE"))
      printf( "||A||_1=%f\n||Ainv||_1=%f\n||Id - A*Ainv||_1=%e\n", Anorm, Ainvnorm, Rnorm );

    result = Rnorm / ( (Anorm*Ainvnorm)*N*eps ) ;
    printf("============\n");
    printf("Checking the Residual of the inverse \n");
    printf("-- ||Id - A*Ainv||_1/((||A||_1||Ainv||_1).N.eps) = %e \n", result);

    if (  isnan(Ainvnorm) || isinf(Ainvnorm) || isnan(result) || isinf(result) || (result > 60.0) ) {
        printf("-- The inverse is suspicious ! \n");
        info_inverse = 1;
     }
    else{
        printf("-- The inverse is CORRECT ! \n");
        info_inverse = 0;
    }

    free(work);

    return info_inverse;
}
