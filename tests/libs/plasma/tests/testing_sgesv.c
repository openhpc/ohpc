/**
 *
 * @file testing_sgesv.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Bilel Hadri
 * @author Hatem Ltaief
 * @author Matheiu Faverge
 * @date 2010-11-15
 * @generated s Fri Apr  1 11:03:04 2016
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

static int check_solution(int, int , float *, int,
                          float *, float *, int, float);
static int check_estimator(PLASMA_enum, int, float *, int,
                           float *, float, float,
                           float);

int testing_sgesv(int argc, char **argv)
{
    /* Check for valid arguments*/
    if (argc != 4){
        USAGE("GESV", "N LDA NRHS LDB",
              "   - N    : the size of the matrix\n"
              "   - LDA  : leading dimension of the matrix A\n"
              "   - NRHS : number of RHS\n"
              "   - LDB  : leading dimension of the matrix B\n");
        return -1;
    }

    int N     = atoi(argv[0]);
    int LDA   = atoi(argv[1]);
    int NRHS  = atoi(argv[2]);
    int LDB   = atoi(argv[3]);
    float eps;
    int info_solution;
    int i,j,n;
    int LDAxN = LDA*N;
    int LDBxNRHS = LDB*NRHS;

    float *A1 = (float *)malloc(LDA*N   *sizeof(float));
    float *A2 = (float *)malloc(LDA*N   *sizeof(float));
    float *B1 = (float *)malloc(LDB*NRHS*sizeof(float));
    float *B2 = (float *)malloc(LDB*NRHS*sizeof(float));
    int *IPIV = (int *)malloc(N*sizeof(int));

    /* Check if unable to allocate memory */
    if ( (!A1) || (!A2)|| (!B1) || (!B2) || (!IPIV) ) {
        printf("Out of Memory \n ");
        return -2;
    }

    eps = BLAS_sfpinfo(blas_eps);

    /*----------------------------------------------------------
    *  TESTING SGESV
    */

    /* Initialize A1 and A2 Matrix */
    LAPACKE_slarnv_work(1, ISEED, LDAxN, A1);
    for ( i = 0; i < N; i++)
        for (  j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    LAPACKE_slarnv_work(1, ISEED, LDBxNRHS, B1);
    for ( i = 0; i < N; i++)
        for ( j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i];

    /* PLASMA SGESV */
    PLASMA_sgesv(N, NRHS, A2, LDA, IPIV, B2, LDB);

    printf("\n");
    printf("------ TESTS FOR PLASMA SGESV ROUTINE -------  \n");
    printf("            Size of the Matrix %d by %d\n", N, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n", eps);
    printf(" Computational tests pass if scaled residuals are less than 60.\n");

    /* Check the factorization and the solution */
    info_solution = check_solution(N, NRHS, A1, LDA, B1, B2, LDB, eps);

    if ((info_solution == 0)){
        printf("***************************************************\n");
        printf(" ---- TESTING SGESV ............................. PASSED !\n");
        printf("***************************************************\n");
    }
    else{
        printf("************************************************\n");
        printf(" - TESTING SGESV ... FAILED !\n");
        printf("************************************************\n");
    }

    /*-------------------------------------------------------------
    *  TESTING SGETRF + SGETRS
    */

    /* Initialize A1 and A2  */
    LAPACKE_slarnv_work(1, ISEED, LDAxN, A1);
    for ( i = 0; i < N; i++)
        for (  j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    LAPACKE_slarnv_work(1, ISEED, LDBxNRHS, B1);
    for ( i = 0; i < N; i++)
        for ( j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i];

    /* Plasma routines */
    PLASMA_sgetrf(N, N, A2, LDA, IPIV);
    PLASMA_sgetrs(PlasmaNoTrans, N, NRHS, A2, LDA, IPIV, B2, LDB);

    printf("\n");
    printf("------ TESTS FOR PLASMA SGETRF + SGETRS ROUTINE -------  \n");
    printf("            Size of the Matrix %d by %d\n", N, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n", eps);
    printf(" Computational tests pass if scaled residuals are less than 60.\n");

    /* Check the solution */
    info_solution = check_solution(N, NRHS, A1, LDA, B1, B2, LDB, eps);

    if ((info_solution == 0)){
        printf("***************************************************\n");
        printf(" ---- TESTING SGETRF + SGETRS ................... PASSED !\n");
        printf("***************************************************\n");
    }
    else{
        printf("***************************************************\n");
        printf(" - TESTING SGETRF + SGETRS ... FAILED !\n");
        printf("***************************************************\n");
    }

    /*-------------------------------------------------------------
    *  TESTING SGETRF + SLASWP + STRSM + STRSM
    */

    /* Initialize A1 and A2  */
    LAPACKE_slarnv_work(1, ISEED, LDAxN, A1);
    for ( i = 0; i < N; i++)
        for (  j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    LAPACKE_slarnv_work(1, ISEED, LDBxNRHS, B1);
    for ( i = 0; i < N; i++)
        for ( j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i];

    /* PLASMA routines */
    PLASMA_sgetrf(N, N, A2, LDA, IPIV);
    PLASMA_slaswp(NRHS, B2, LDB, 1, N, IPIV, 1);
    PLASMA_strsm(PlasmaLeft, PlasmaLower, PlasmaNoTrans, PlasmaUnit,
                 N, NRHS, 1.0, A2, LDA, B2, LDB);
    PLASMA_strsm(PlasmaLeft, PlasmaUpper, PlasmaNoTrans, PlasmaNonUnit,
                 N, NRHS, 1.0, A2, LDA, B2, LDB);

    printf("\n");
    printf("------ TESTS FOR PLASMA SGETRF + SLASWP + STRSM + STRSM  ROUTINE -------  \n");
    printf("            Size of the Matrix %d by %d\n", N, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n", eps);
    printf(" Computational tests pass if scaled residuals are less than 60.\n");

    /* Check the solution */
    info_solution = check_solution(N, NRHS, A1, LDA, B1, B2, LDB, eps);

    if ((info_solution == 0)){
        printf("***************************************************\n");
        printf(" ---- TESTING SGETRF + SLASWP + STRSM + STRSM ... PASSED !\n");
        printf("***************************************************\n");
    }
    else{
        printf("**************************************************\n");
        printf(" - TESTING SGETRF + SLASWP + STRSM + STRSM ... FAILED !\n");
        printf("**************************************************\n");
    }

    /*-------------------------------------------------------------
     *  TESTING ZGECON on the last call
     */
    for (n=1; n<3; n++)
    {
        float Anorm = PLASMA_slange( norm[n], N, N, A1, LDA );
        float Acond;

        info_solution = PLASMA_sgecon(norm[n], N, A2, LDA, Anorm, &Acond);

        if ( info_solution == 0 ) {
            info_solution = check_estimator(norm[n], N, A1, LDA, A2, Anorm, Acond, eps);
        } else {
            printf(" PLASMA_sgecon returned info = %d\n", info_solution );
        }
        if ((info_solution == 0)){
            printf("***************************************************\n");
            printf(" ---- TESTING SGETRF + ZGECON(%s) .............. PASSED !\n", normstr[n]);
            printf("***************************************************\n");
        }
        else{
            printf("**************************************************\n");
            printf(" - TESTING SGETRF + ZGECON(%s) ... FAILED !\n", normstr[n]);
            printf("**************************************************\n");
        }
    }

    free(A1); free(A2); free(B1); free(B2); free(IPIV);

    return 0;
}

/*------------------------------------------------------------------------
 *  Check the accuracy of the solution of the linear system
 */

static int check_solution(int N, int NRHS, float *A1, int LDA, float *B1, float *B2, int LDB, float eps )
{
    int info_solution;
    float Rnorm, Anorm, Xnorm, Bnorm, result;
    float alpha, beta;
    float *work = (float *)malloc(N*sizeof(float));

    alpha = 1.0;
    beta  = -1.0;

    BLAS_sge_norm( blas_colmajor, blas_inf_norm, N, NRHS, B2, LDB, &Xnorm );
    BLAS_sge_norm( blas_colmajor, blas_inf_norm, N, N,    A1, LDA, &Anorm );
    BLAS_sge_norm( blas_colmajor, blas_inf_norm, N, NRHS, B1, LDB, &Bnorm );

    cblas_sgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, N, NRHS, N, (alpha), A1, LDA, B2, LDB, (beta), B1, LDB);
    BLAS_sge_norm( blas_colmajor, blas_inf_norm, N, NRHS, B1, LDB, &Rnorm );

    if (getenv("PLASMA_TESTING_VERBOSE"))
      printf( "||A||_oo=%f\n||X||_oo=%f\n||B||_oo=%f\n||A X - B||_oo=%e\n", Anorm, Xnorm, Bnorm, Rnorm );

    result = Rnorm / ( (Anorm*Xnorm+Bnorm)*N*eps ) ;
    printf("============\n");
    printf("Checking the Residual of the solution \n");
    printf("-- ||Ax-B||_oo/((||A||_oo||x||_oo+||B||_oo).N.eps) = %e \n", result);

    if (  isnan(Xnorm) || isinf(Xnorm) || isnan(result) || isinf(result) || (result > 60.0) ) {
        printf("-- The solution is suspicious ! \n");
        info_solution = 1;
     }
    else{
        printf("-- The solution is CORRECT ! \n");
        info_solution = 0;
    }
    free(work);
    return info_solution;
}

/*------------------------------------------------------------------------
 *  Check the accuracy of the condition estimator
 */

static int check_estimator(PLASMA_enum norm, int N, float *A1, int LDA,
                           float *A2, float Anorm, float Acond,
                           float eps)
{
    int info_solution;
    float result, Acond_lapack;
    float invcond, invcond_lapack;

    info_solution = LAPACKE_sgecon(LAPACK_COL_MAJOR, lapack_const(norm), N, A2, LDA, Anorm, &Acond_lapack);

    if ( info_solution != 0 ) {
        printf(" PLASMA_sgecon returned info = %d\n", info_solution );
        return info_solution;
    }

    invcond_lapack = 1. / ( Acond_lapack );
    invcond        = 1. / ( Acond );

    printf("============\n");
    printf("Checking the condition number \n");
    printf("-- Acond_plasma = %e, Acond_lapack = %e \n"
           "-- Ainvcond_plasma = %e, Ainvcond_lapack = %e \n",
           Acond, Acond_lapack, invcond, invcond_lapack );

    result = fabs( Acond_lapack - Acond ) / eps;
    if ( result > 60. ) {
        printf("-- The solution is suspicious ! \n");
        info_solution = 1;
     }
    else{
        printf("-- The solution is CORRECT ! \n");
        info_solution = 0;
    }
    return info_solution;
}

