/**
 *
 * @file testing_cgesv.c
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
 * @generated c Fri Apr  1 11:03:04 2016
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
#include "testing_cmain.h"

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
BLAS_cge_norm(enum blas_order_type order, enum blas_norm_type norm,
  int m, int n, const PLASMA_Complex32_t *a, int lda, float *res) {
  int i, j; float anorm, v;
  char rname[] = "BLAS_cge_norm";

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
        v += cabsf( a[i + j * lda] );
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

static int check_solution(int, int , PLASMA_Complex32_t *, int,
                          PLASMA_Complex32_t *, PLASMA_Complex32_t *, int, float);
static int check_estimator(PLASMA_enum, int, PLASMA_Complex32_t *, int,
                           PLASMA_Complex32_t *, float, float,
                           float);

int testing_cgesv(int argc, char **argv)
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

    PLASMA_Complex32_t *A1 = (PLASMA_Complex32_t *)malloc(LDA*N   *sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *A2 = (PLASMA_Complex32_t *)malloc(LDA*N   *sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *B1 = (PLASMA_Complex32_t *)malloc(LDB*NRHS*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *B2 = (PLASMA_Complex32_t *)malloc(LDB*NRHS*sizeof(PLASMA_Complex32_t));
    int *IPIV = (int *)malloc(N*sizeof(int));

    /* Check if unable to allocate memory */
    if ( (!A1) || (!A2)|| (!B1) || (!B2) || (!IPIV) ) {
        printf("Out of Memory \n ");
        return -2;
    }

    eps = BLAS_sfpinfo(blas_eps);

    /*----------------------------------------------------------
    *  TESTING CGESV
    */

    /* Initialize A1 and A2 Matrix */
    LAPACKE_clarnv_work(1, ISEED, LDAxN, A1);
    for ( i = 0; i < N; i++)
        for (  j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    LAPACKE_clarnv_work(1, ISEED, LDBxNRHS, B1);
    for ( i = 0; i < N; i++)
        for ( j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i];

    /* PLASMA CGESV */
    PLASMA_cgesv(N, NRHS, A2, LDA, IPIV, B2, LDB);

    printf("\n");
    printf("------ TESTS FOR PLASMA CGESV ROUTINE -------  \n");
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
        printf(" ---- TESTING CGESV ............................. PASSED !\n");
        printf("***************************************************\n");
    }
    else{
        printf("************************************************\n");
        printf(" - TESTING CGESV ... FAILED !\n");
        printf("************************************************\n");
    }

    /*-------------------------------------------------------------
    *  TESTING CGETRF + CGETRS
    */

    /* Initialize A1 and A2  */
    LAPACKE_clarnv_work(1, ISEED, LDAxN, A1);
    for ( i = 0; i < N; i++)
        for (  j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    LAPACKE_clarnv_work(1, ISEED, LDBxNRHS, B1);
    for ( i = 0; i < N; i++)
        for ( j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i];

    /* Plasma routines */
    PLASMA_cgetrf(N, N, A2, LDA, IPIV);
    PLASMA_cgetrs(PlasmaNoTrans, N, NRHS, A2, LDA, IPIV, B2, LDB);

    printf("\n");
    printf("------ TESTS FOR PLASMA CGETRF + CGETRS ROUTINE -------  \n");
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
        printf(" ---- TESTING CGETRF + CGETRS ................... PASSED !\n");
        printf("***************************************************\n");
    }
    else{
        printf("***************************************************\n");
        printf(" - TESTING CGETRF + CGETRS ... FAILED !\n");
        printf("***************************************************\n");
    }

    /*-------------------------------------------------------------
    *  TESTING CGETRF + CLASWP + CTRSM + CTRSM
    */

    /* Initialize A1 and A2  */
    LAPACKE_clarnv_work(1, ISEED, LDAxN, A1);
    for ( i = 0; i < N; i++)
        for (  j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    LAPACKE_clarnv_work(1, ISEED, LDBxNRHS, B1);
    for ( i = 0; i < N; i++)
        for ( j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i];

    /* PLASMA routines */
    PLASMA_cgetrf(N, N, A2, LDA, IPIV);
    PLASMA_claswp(NRHS, B2, LDB, 1, N, IPIV, 1);
    PLASMA_ctrsm(PlasmaLeft, PlasmaLower, PlasmaNoTrans, PlasmaUnit,
                 N, NRHS, 1.0, A2, LDA, B2, LDB);
    PLASMA_ctrsm(PlasmaLeft, PlasmaUpper, PlasmaNoTrans, PlasmaNonUnit,
                 N, NRHS, 1.0, A2, LDA, B2, LDB);

    printf("\n");
    printf("------ TESTS FOR PLASMA CGETRF + CLASWP + CTRSM + CTRSM  ROUTINE -------  \n");
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
        printf(" ---- TESTING CGETRF + CLASWP + CTRSM + CTRSM ... PASSED !\n");
        printf("***************************************************\n");
    }
    else{
        printf("**************************************************\n");
        printf(" - TESTING CGETRF + CLASWP + CTRSM + CTRSM ... FAILED !\n");
        printf("**************************************************\n");
    }

    /*-------------------------------------------------------------
     *  TESTING ZGECON on the last call
     */
    for (n=1; n<3; n++)
    {
        float Anorm = PLASMA_clange( norm[n], N, N, A1, LDA );
        float Acond;

        info_solution = PLASMA_cgecon(norm[n], N, A2, LDA, Anorm, &Acond);

        if ( info_solution == 0 ) {
            info_solution = check_estimator(norm[n], N, A1, LDA, A2, Anorm, Acond, eps);
        } else {
            printf(" PLASMA_cgecon returned info = %d\n", info_solution );
        }
        if ((info_solution == 0)){
            printf("***************************************************\n");
            printf(" ---- TESTING CGETRF + ZGECON(%s) .............. PASSED !\n", normstr[n]);
            printf("***************************************************\n");
        }
        else{
            printf("**************************************************\n");
            printf(" - TESTING CGETRF + ZGECON(%s) ... FAILED !\n", normstr[n]);
            printf("**************************************************\n");
        }
    }

    free(A1); free(A2); free(B1); free(B2); free(IPIV);

    return 0;
}

/*------------------------------------------------------------------------
 *  Check the accuracy of the solution of the linear system
 */

static int check_solution(int N, int NRHS, PLASMA_Complex32_t *A1, int LDA, PLASMA_Complex32_t *B1, PLASMA_Complex32_t *B2, int LDB, float eps )
{
    int info_solution;
    float Rnorm, Anorm, Xnorm, Bnorm, result;
    PLASMA_Complex32_t alpha, beta;
    float *work = (float *)malloc(N*sizeof(float));

    alpha = 1.0;
    beta  = -1.0;

    BLAS_cge_norm( blas_colmajor, blas_inf_norm, N, NRHS, B2, LDB, &Xnorm );
    BLAS_cge_norm( blas_colmajor, blas_inf_norm, N, N,    A1, LDA, &Anorm );
    BLAS_cge_norm( blas_colmajor, blas_inf_norm, N, NRHS, B1, LDB, &Bnorm );

    cblas_cgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, N, NRHS, N, CBLAS_SADDR(alpha), A1, LDA, B2, LDB, CBLAS_SADDR(beta), B1, LDB);
    BLAS_cge_norm( blas_colmajor, blas_inf_norm, N, NRHS, B1, LDB, &Rnorm );

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

static int check_estimator(PLASMA_enum norm, int N, PLASMA_Complex32_t *A1, int LDA,
                           PLASMA_Complex32_t *A2, float Anorm, float Acond,
                           float eps)
{
    int info_solution;
    float result, Acond_lapack;
    float invcond, invcond_lapack;

    info_solution = LAPACKE_cgecon(LAPACK_COL_MAJOR, lapack_const(norm), N, A2, LDA, Anorm, &Acond_lapack);

    if ( info_solution != 0 ) {
        printf(" PLASMA_cgecon returned info = %d\n", info_solution );
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

