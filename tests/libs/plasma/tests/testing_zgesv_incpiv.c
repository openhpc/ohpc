/**
 *
 * @file testing_zgesv.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Bilel Hadri, Hatem Ltaief
 * @date 2010-11-15
 * @precisions normal z -> c d s
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
#include "testing_zmain.h"

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
BLAS_zge_norm(enum blas_order_type order, enum blas_norm_type norm,
  int m, int n, const PLASMA_Complex64_t *a, int lda, double *res) {
  int i, j; float anorm, v;
  char rname[] = "BLAS_zge_norm";

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
        v += cabs( a[i + j * lda] );
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
double
BLAS_dpow_di(double x, int n) {
  double rv = 1.0;

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
double
BLAS_dfpinfo(enum blas_cmach_type cmach) {
  double eps = 1.0, r = 1.0, o = 1.0, b = 2.0;
  int t = 53, l = 1024, m = -1021;
  char rname[] = "BLAS_dfpinfo";

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
  eps = BLAS_dpow_di( b, -t );
  /* for (i = 0; i >= m; --i) r *= half; */
  r = BLAS_dpow_di( b, m-1 );

  o -= eps;
  /* for (i = 0; i < l; ++i) o *= b; */
  o = (o * BLAS_dpow_di( b, l-1 )) * b;

  switch (cmach) {
    case blas_eps: return eps;
    case blas_sfmin: return r;
    default:
      BLAS_error( rname, -1, cmach, 0 );
      break;
  }
  return 0.0;
}

static int check_solution(int, int , PLASMA_Complex64_t *, int, PLASMA_Complex64_t *, PLASMA_Complex64_t *, int, double);

int testing_zgesv_incpiv(int argc, char **argv)
{
    /* Check for valid arguments*/
    if (argc != 4){
        USAGE("GESV_INCPIV", "N LDA NRHS LDB",
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
    double eps;
    int info_solution;
    int i,j;
    int LDAxN = LDA*N;
    int LDBxNRHS = LDB*NRHS;

    PLASMA_Complex64_t *A1 = (PLASMA_Complex64_t *)malloc(LDA*N*(sizeof*A1));
    PLASMA_Complex64_t *A2 = (PLASMA_Complex64_t *)malloc(LDA*N*(sizeof*A2));
    PLASMA_Complex64_t *B1 = (PLASMA_Complex64_t *)malloc(LDB*NRHS*(sizeof*B1));
    PLASMA_Complex64_t *B2 = (PLASMA_Complex64_t *)malloc(LDB*NRHS*(sizeof*B2));
    PLASMA_desc *L;
    int *IPIV;

    /* Check if unable to allocate memory */
    if ( (!A1) || (!A2)|| (!B1) || (!B2) ) {
        printf("Out of Memory \n ");
        return -2;
    }

    eps = BLAS_dfpinfo(blas_eps);

    /*----------------------------------------------------------
    *  TESTING ZGESV
    */

    /* Initialize A1 and A2 Matrix */
    LAPACKE_zlarnv_work(IONE, ISEED, LDAxN, A1);
    for ( i = 0; i < N; i++)
        for (  j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    LAPACKE_zlarnv_work(IONE, ISEED, LDBxNRHS, B1);
    for ( i = 0; i < N; i++)
        for ( j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i];

    /* PLASMA ZGESV */
    PLASMA_Alloc_Workspace_zgesv_incpiv(N, &L, &IPIV);
    PLASMA_zgesv_incpiv(N, NRHS, A2, LDA, L, IPIV, B2, LDB);

    printf("\n");
    printf("------ TESTS FOR PLASMA INCPIV ZGESV ROUTINE -------  \n");
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
        printf(" ---- TESTING INCPIV ZGESV ............... PASSED !\n");
        printf("***************************************************\n");
    }
    else{
        printf("************************************************\n");
        printf(" - TESTING INCPIV ZGESV ... FAILED !\n");
        printf("************************************************\n");
    }

    /*-------------------------------------------------------------
    *  TESTING ZGETRF + ZGETRS
    */

    /* Initialize A1 and A2  */
    LAPACKE_zlarnv_work(IONE, ISEED, LDAxN, A1);
    for ( i = 0; i < N; i++)
        for (  j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    LAPACKE_zlarnv_work(IONE, ISEED, LDBxNRHS, B1);
    for ( i = 0; i < N; i++)
        for ( j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i];

    /* Plasma routines */
    PLASMA_zgetrf_incpiv(N, N, A2, LDA, L, IPIV);
    PLASMA_zgetrs_incpiv(PlasmaNoTrans, N, NRHS, A2, LDA, L, IPIV, B2, LDB);

    printf("\n");
    printf("------ TESTS FOR PLASMA ZGETRF + ZGETRS ROUTINE -------  \n");
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
        printf(" ---- TESTING INCPIV ZGETRF + ZGETRS ..... PASSED !\n");
        printf("***************************************************\n");
    }
    else{
        printf("***************************************************\n");
        printf(" - TESTING INCPIV ZGETRF + ZGETRS ... FAILED !\n");
        printf("***************************************************\n");
    }

    /*-------------------------------------------------------------
    *  TESTING ZGETRF + ZTRSMPL + ZTRSM
    */

    /* Initialize A1 and A2  */
    LAPACKE_zlarnv_work(IONE, ISEED, LDAxN, A1);
    for ( i = 0; i < N; i++)
        for (  j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    LAPACKE_zlarnv_work(IONE, ISEED, LDBxNRHS, B1);
    for ( i = 0; i < N; i++)
        for ( j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i];

    /* PLASMA routines */
    PLASMA_zgetrf_incpiv(N, N, A2, LDA, L, IPIV);
    PLASMA_ztrsmpl(N, NRHS, A2, LDA, L, IPIV, B2, LDB);
    PLASMA_ztrsm(PlasmaLeft, PlasmaUpper, PlasmaNoTrans, PlasmaNonUnit,
                 N, NRHS, 1.0, A2, LDA, B2, LDB);

    printf("\n");
    printf("------ TESTS FOR PLASMA INCPIV ZGETRF + ZTRSMPL + ZTRSM  ROUTINE -------  \n");
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
        printf(" ---- TESTING INCPIV ZGETRF + ZTRSMPL + ZTRSM ... PASSED !\n");
        printf("***************************************************\n");
    }
    else{
        printf("**************************************************\n");
        printf(" - TESTING INCPIV ZGETRF + ZTRSMPL + ZTRSM ... FAILED !\n");
        printf("**************************************************\n");
    }

    free(A1); free(A2); free(B1); free(B2); free(IPIV);
    PLASMA_Dealloc_Handle_Tile( &L );

    return 0;
}

/*------------------------------------------------------------------------
 *  Check the accuracy of the solution of the linear system
 */

static int check_solution(int N, int NRHS, PLASMA_Complex64_t *A1, int LDA, PLASMA_Complex64_t *B1, PLASMA_Complex64_t *B2, int LDB, double eps )
{
    int info_solution;
    double Rnorm, Anorm, Xnorm, Bnorm, result;
    PLASMA_Complex64_t alpha, beta;
    double *work = (double *)malloc(N*sizeof(double));

    alpha = 1.0;
    beta  = -1.0;

    BLAS_zge_norm( blas_colmajor, blas_inf_norm, N, NRHS, B2, LDB, &Xnorm );
    BLAS_zge_norm( blas_colmajor, blas_inf_norm, N, N,    A1, LDA, &Anorm );
    BLAS_zge_norm( blas_colmajor, blas_inf_norm, N, NRHS, B1, LDB, &Bnorm );

    cblas_zgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, N, NRHS, N, CBLAS_SADDR(alpha), A1, LDA, B2, LDB, CBLAS_SADDR(beta), B1, LDB);
    BLAS_zge_norm( blas_colmajor, blas_inf_norm, N, NRHS, B1, LDB, &Rnorm );

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
