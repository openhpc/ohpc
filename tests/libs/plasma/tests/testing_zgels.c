/**
 *
 * @file testing_zgels.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Bilel Hadri
 * @author Hatem Ltaief
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

#undef REAL
#define COMPLEX

enum blas_order_type {
            blas_rowmajor = 101,
            blas_colmajor = 102 };

enum blas_uplo_type  {
            blas_upper = 121,
            blas_lower = 122 };

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
BLAS_zsy_norm(enum blas_order_type order, enum blas_norm_type norm,
  enum blas_uplo_type uplo, int n, const PLASMA_Complex64_t *a, int lda, double *res) {
  int i, j; double anorm, v;
  char rname[] = "BLAS_zsy_norm";

  if (order != blas_colmajor) BLAS_error( rname, -1, order, 0 );

  if (norm == blas_inf_norm) {
    anorm = 0.0;
    if (blas_upper == uplo) {
      for (i = 0; i < n; ++i) {
        v = 0.0;
        for (j = 0; j < i; ++j) {
          v += cabs( a[j + i * lda] );
        }
        for (j = i; j < n; ++j) {
          v += cabs( a[i + j * lda] );
        }
        if (v > anorm)
          anorm = v;
      }
    } else {
      BLAS_error( rname, -3, norm, 0 );
      return;
    }
  } else {
    BLAS_error( rname, -2, norm, 0 );
    return;
  }

  if (res) *res = anorm;
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

static int check_orthogonality(int, int, int, PLASMA_Complex64_t*, double);
static int check_factorization(int, int, PLASMA_Complex64_t*, PLASMA_Complex64_t*, int, PLASMA_Complex64_t*, double);
static int check_solution(int, int, int, PLASMA_Complex64_t*, int, PLASMA_Complex64_t*, PLASMA_Complex64_t*, int, double);

int testing_zgels(int argc, char **argv)
{
    int mode = 0;

    if ( argc < 1 ){
        goto usage;
    } else {
        mode = atoi(argv[0]);
    }

    /* Check for number of arguments*/
    if ( ((mode == 0) && (argc != 6)) ||
         ((mode != 0) && (argc != 7)) ){
      usage:
        USAGE("GELS", "MODE M N LDA NRHS LDB [RH]",
              "   - MODE : 0: flat, 1: tree (RH needed)\n"
              "   - M    : number of rows of the matrix A\n"
              "   - N    : number of columns of the matrix A\n"
              "   - LDA  : leading dimension of the matrix A\n"
              "   - NRHS : number of RHS\n"
              "   - LDB  : leading dimension of the matrix B\n"
              "   - RH   : Size of each subdomains\n");
        return -1;
    }

    int M     = atoi(argv[1]);
    int N     = atoi(argv[2]);
    int LDA   = atoi(argv[3]);
    int NRHS  = atoi(argv[4]);
    int LDB   = atoi(argv[5]);
    int rh;

    int K = min(M, N);
    double eps;
    int info_ortho, info_solution, info_factorization;
    int i,j;
    int LDAxN = LDA*N;
    int LDBxNRHS = LDB*NRHS;

    PLASMA_Complex64_t *A1 = (PLASMA_Complex64_t *)malloc(LDA*N*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *A2 = (PLASMA_Complex64_t *)malloc(LDA*N*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *B1 = (PLASMA_Complex64_t *)malloc(LDB*NRHS*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *B2 = (PLASMA_Complex64_t *)malloc(LDB*NRHS*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Q  = (PLASMA_Complex64_t *)malloc(LDA*N*sizeof(PLASMA_Complex64_t));
    PLASMA_desc *T;

    /* Check if unable to allocate memory */
    if ((!A1)||(!A2)||(!B1)||(!B2)||(!Q)){
        printf("Out of Memory \n ");
        return -2;
    }

    if ( mode ) {
        rh = atoi(argv[6]);

        PLASMA_Set(PLASMA_HOUSEHOLDER_MODE, PLASMA_TREE_HOUSEHOLDER);
        PLASMA_Set(PLASMA_HOUSEHOLDER_SIZE, rh);
    }

    PLASMA_Alloc_Workspace_zgels(M, N, &T);
    eps = BLAS_dfpinfo( blas_eps );

    /*----------------------------------------------------------
    *  TESTING ZGELS
    */

    /* Initialize A1 and A2 */
    LAPACKE_zlarnv_work(IONE, ISEED, LDAxN, A1);
    for (i = 0; i < M; i++)
        for (j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i] ;

    /* Initialize B1 and B2 */
    LAPACKE_zlarnv_work(IONE, ISEED, LDBxNRHS, B1);
    for (i = 0; i < M; i++)
        for (j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i] ;

    /* PLASMA ZGELS */
    PLASMA_zgels(PlasmaNoTrans, M, N, NRHS, A2, LDA, T, B2, LDB);

    /* PLASMA ZGELS */
    if (M >= N)
       /* Building the economy-size Q */
       PLASMA_zungqr(M, N, K, A2, LDA, T, Q, LDA);
    else
       /* Building the economy-size Q */
       PLASMA_zunglq(M, N, K, A2, LDA, T, Q, LDA);

    printf("\n");
    printf("------ TESTS FOR PLASMA ZGELS ROUTINE -------  \n");
    printf("            Size of the Matrix %d by %d\n", M, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 60.\n");

    /* Check the orthogonality, factorization and the solution */
    info_ortho = check_orthogonality(M, N, LDA, Q, eps);
    info_factorization = check_factorization(M, N, A1, A2, LDA, Q, eps);
    info_solution = check_solution(M, N, NRHS, A1, LDA, B1, B2, LDB, eps);

    if ((info_solution == 0)&(info_factorization == 0)&(info_ortho == 0)) {
        printf("***************************************************\n");
        printf(" ---- TESTING ZGELS ...................... PASSED !\n");
        printf("***************************************************\n");
    }
    else {
        printf("************************************************\n");
        printf(" - TESTING ZGELS ... FAILED !\n");
        printf("************************************************\n");
    }

    /*-------------------------------------------------------------
    *  TESTING ZGEQRF + ZGEQRS or ZGELQF + ZGELQS
    */

    /* Initialize A1 and A2 */
    LAPACKE_zlarnv_work(IONE, ISEED, LDAxN, A1);
    for (i = 0; i < M; i++)
        for (j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    LAPACKE_zlarnv_work(IONE, ISEED, LDBxNRHS, B1);
    for (i = 0; i < M; i++)
        for (j = 0; j < NRHS; j++)
             B2[LDB*j+i] = B1[LDB*j+i];

    if (M >= N) {
        printf("\n");
        printf("------ TESTS FOR PLASMA ZGEQRF + ZGEQRS ROUTINE -------  \n");
        printf("            Size of the Matrix %d by %d\n", M, N);
        printf("\n");
        printf(" The matrix A is randomly generated for each test.\n");
        printf("============\n");
        printf(" The relative machine precision (eps) is to be %e \n", eps);
        printf(" Computational tests pass if scaled residuals are less than 60.\n");

        /* Plasma routines */
        PLASMA_zgeqrf(M, N, A2, LDA, T);
        PLASMA_zungqr(M, N, K, A2, LDA, T, Q, LDA);
        PLASMA_zgeqrs(M, N, NRHS, A2, LDA, T, B2, LDB);

        /* Check the orthogonality, factorization and the solution */
        info_ortho = check_orthogonality(M, N, LDA, Q, eps);
        info_factorization = check_factorization(M, N, A1, A2, LDA, Q, eps);
        info_solution = check_solution(M, N, NRHS, A1, LDA, B1, B2, LDB, eps);

        if ((info_solution == 0)&(info_factorization == 0)&(info_ortho == 0)) {
            printf("***************************************************\n");
            printf(" ---- TESTING ZGEQRF + ZGEQRS ............ PASSED !\n");
            printf("***************************************************\n");
        }
        else{
            printf("***************************************************\n");
            printf(" - TESTING ZGEQRF + ZGEQRS ... FAILED !\n");
            printf("***************************************************\n");
        }
    }
    else  {
        printf("\n");
        printf("------ TESTS FOR PLASMA ZGELQF + ZGELQS ROUTINE -------  \n");
        printf("            Size of the Matrix %d by %d\n", M, N);
        printf("\n");
        printf(" The matrix A is randomly generated for each test.\n");
        printf("============\n");
        printf(" The relative machine precision (eps) is to be %e \n", eps);
        printf(" Computational tests pass if scaled residuals are less than 60.\n");

        /* Plasma routines */
        PLASMA_zgelqf(M, N, A2, LDA, T);
        PLASMA_zunglq(M, N, K, A2, LDA, T, Q, LDA);
        PLASMA_zgelqs(M, N, NRHS, A2, LDA, T, B2, LDB);

       /* Check the orthogonality, factorization and the solution */
       info_ortho = check_orthogonality(M, N, LDA, Q, eps);
       info_factorization = check_factorization(M, N, A1, A2, LDA, Q, eps);
       info_solution = check_solution(M, N, NRHS, A1, LDA, B1, B2, LDB, eps);

       if ( (info_solution == 0) & (info_factorization == 0) & (info_ortho == 0) ) {
          printf("***************************************************\n");
          printf(" ---- TESTING ZGELQF + ZGELQS ............ PASSED !\n");
          printf("***************************************************\n");
       }
       else {
          printf("***************************************************\n");
          printf(" - TESTING ZGELQF + ZGELQS ... FAILED !\n");
          printf("***************************************************\n");
        }
    }

    /*----------------------------------------------------------
    *  TESTING ZGEQRF + ZORMQR + ZTRSM
    */

    /* Initialize A1 and A2 */
    LAPACKE_zlarnv_work(IONE, ISEED, LDAxN, A1);
    for (i = 0; i < M; i++)
        for (j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    memset(B2, 0, LDB*NRHS*sizeof(PLASMA_Complex64_t));
    LAPACKE_zlarnv_work(IONE, ISEED, LDBxNRHS, B1);
    for (i = 0; i < M; i++)
        for (j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i];

    if (M >= N) {
        printf("\n");
        printf("------ TESTS FOR PLASMA ZGEQRF + ZUNMQR + ZTRSM  ROUTINE -------  \n");
        printf("            Size of the Matrix %d by %d\n", M, N);
        printf("\n");
        printf(" The matrix A is randomly generated for each test.\n");
        printf("============\n");
        printf(" The relative machine precision (eps) is to be %e \n",eps);
        printf(" Computational tests pass if scaled residuals are less than 60.\n");

        PLASMA_zgeqrf(M, N, A2, LDA, T);
        PLASMA_zungqr(M, N, K, A2, LDA, T, Q, LDA);
        PLASMA_zunmqr(PlasmaLeft, PlasmaConjTrans, M, NRHS, N, A2, LDA, T, B2, LDB);
        PLASMA_ztrsm(PlasmaLeft, PlasmaUpper, PlasmaNoTrans, PlasmaNonUnit, N, NRHS, 1.0, A2, LDA, B2, LDB);
    }
    else {
        printf("\n");
        printf("------ TESTS FOR PLASMA ZGELQF + ZUNMLQ + ZTRSM  ROUTINE -------  \n");
        printf("            Size of the Matrix %d by %d\n", M, N);
        printf("\n");
        printf(" The matrix A is randomly generated for each test.\n");
        printf("============\n");
        printf(" The relative machine precision (eps) is to be %e \n",eps);
        printf(" Computational tests pass if scaled residuals are less than 60.\n");

        PLASMA_zgelqf(M, N, A2, LDA, T);
        PLASMA_ztrsm(PlasmaLeft, PlasmaLower, PlasmaNoTrans, PlasmaNonUnit, M, NRHS, 1.0, A2, LDA, B2, LDB);
        PLASMA_zunglq(M, N, K, A2, LDA, T, Q, LDA);
        PLASMA_zunmlq(PlasmaLeft, PlasmaConjTrans, N, NRHS, M, A2, LDA, T, B2, LDB);
    }

    /* Check the orthogonality, factorization and the solution */
    info_ortho = check_orthogonality(M, N, LDA, Q, eps);
    info_factorization = check_factorization(M, N, A1, A2, LDA, Q, eps);
    info_solution = check_solution(M, N, NRHS, A1, LDA, B1, B2, LDB, eps);

    if ( (info_solution == 0) & (info_factorization == 0) & (info_ortho == 0) ) {
        if (M >= N) {
            printf("***************************************************\n");
            printf(" ---- TESTING ZGEQRF + ZUNMQR + ZTRSM .... PASSED !\n");
            printf("***************************************************\n");
        }
        else {
            printf("***************************************************\n");
            printf(" ---- TESTING ZGELQF + ZTRSM + ZUNMLQ .... PASSED !\n");
            printf("***************************************************\n");
        }
    }
    else {
        if (M >= N) {
            printf("***************************************************\n");
            printf(" - TESTING ZGEQRF + ZUNMQR + ZTRSM ... FAILED !\n");
            printf("***************************************************\n");
        }
        else {
            printf("***************************************************\n");
            printf(" - TESTING ZGELQF + ZTRSM + ZUNMLQ ... FAILED !\n");
            printf("***************************************************\n");
        }
    }

    free(A1); free(A2); free(B1); free(B2); free(Q);
    PLASMA_Dealloc_Handle_Tile( &T );

    return 0;
}

/*-------------------------------------------------------------------
 * Check the orthogonality of Q
 */

static int check_orthogonality(int M, int N, int LDQ, PLASMA_Complex64_t *Q, double eps)
{
    double alpha, beta;
    double normQ;
    int info_ortho;
    int i;
    int minMN = min(M, N);

    double *work = (double *)malloc(minMN*sizeof(double));

    alpha = 1.0;
    beta  = -1.0;

    /* Build the idendity matrix USE DLASET?*/
    PLASMA_Complex64_t *Id = (PLASMA_Complex64_t *) malloc(minMN*minMN*sizeof(PLASMA_Complex64_t));
    memset((void*)Id, 0, minMN*minMN*sizeof(PLASMA_Complex64_t));
    for (i = 0; i < minMN; i++)
        Id[i*minMN+i] = (PLASMA_Complex64_t)1.0;

    /* Perform Id - Q'Q */
    if (M >= N)
        cblas_zherk(CblasColMajor, CblasUpper, CblasConjTrans, N, M, alpha, Q, LDQ, beta, Id, N);
    else
        cblas_zherk(CblasColMajor, CblasUpper, CblasNoTrans, M, N, alpha, Q, LDQ, beta, Id, M);

    BLAS_zsy_norm( blas_colmajor, blas_inf_norm, blas_upper, minMN, Id, minMN, &normQ );

    printf("============\n");
    printf("Checking the orthogonality of Q \n");
    printf("||Id-Q'*Q||_oo / (N*eps) = %e \n", normQ/(minMN*eps));

    if ( isnan(normQ / (minMN * eps)) || isinf(normQ / (minMN * eps)) || (normQ / (minMN * eps) > 60.0) ) {
        printf("-- Orthogonality is suspicious ! \n");
        info_ortho=1;
    }
    else {
        printf("-- Orthogonality is CORRECT ! \n");
        info_ortho=0;
    }

    free(work); free(Id);

    return info_ortho;
}

/*------------------------------------------------------------
 *  Check the factorization QR
 */

static int check_factorization(int M, int N, PLASMA_Complex64_t *A1, PLASMA_Complex64_t *A2, int LDA, PLASMA_Complex64_t *Q, double eps )
{
    double Anorm, Rnorm;
    PLASMA_Complex64_t alpha, beta;
    int info_factorization;
    int i,j;

    PLASMA_Complex64_t *Ql       = (PLASMA_Complex64_t *)malloc(M*N*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Residual = (PLASMA_Complex64_t *)malloc(M*N*sizeof(PLASMA_Complex64_t));
    double *work              = (double *)malloc(max(M,N)*sizeof(double));

    alpha=1.0;
    beta=0.0;

    if (M >= N) {
        /* Extract the R */
        PLASMA_Complex64_t *R = (PLASMA_Complex64_t *)malloc(N*N*sizeof(PLASMA_Complex64_t));
        memset((void*)R, 0, N*N*sizeof(PLASMA_Complex64_t));
        LAPACKE_zlacpy_work(LAPACK_COL_MAJOR,'u', M, N, A2, LDA, R, N);

        /* Perform Ql=Q*R */
        memset((void*)Ql, 0, M*N*sizeof(PLASMA_Complex64_t));
        cblas_zgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M, N, N, CBLAS_SADDR(alpha), Q, LDA, R, N, CBLAS_SADDR(beta), Ql, M);
        free(R);
    }
    else {
        /* Extract the L */
        PLASMA_Complex64_t *L = (PLASMA_Complex64_t *)malloc(M*M*sizeof(PLASMA_Complex64_t));
        memset((void*)L, 0, M*M*sizeof(PLASMA_Complex64_t));
        LAPACKE_zlacpy_work(LAPACK_COL_MAJOR,'l', M, N, A2, LDA, L, M);

    /* Perform Ql=LQ */
        memset((void*)Ql, 0, M*N*sizeof(PLASMA_Complex64_t));
        cblas_zgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M, N, M, CBLAS_SADDR(alpha), L, M, Q, LDA, CBLAS_SADDR(beta), Ql, M);
        free(L);
    }

    /* Compute the Residual */
    for (i = 0; i < M; i++)
        for (j = 0 ; j < N; j++)
            Residual[j*M+i] = A1[j*LDA+i]-Ql[j*M+i];

    BLAS_zge_norm( blas_colmajor, blas_inf_norm, M, N, Residual, M, &Rnorm );
    BLAS_zge_norm( blas_colmajor, blas_inf_norm, M, N, A2, LDA, &Anorm );

    if (M >= N) {
        printf("============\n");
        printf("Checking the QR Factorization \n");
        printf("-- ||A-QR||_oo/(||A||_oo.N.eps) = %e \n",Rnorm/(Anorm*N*eps));
    }
    else {
        printf("============\n");
        printf("Checking the LQ Factorization \n");
        printf("-- ||A-LQ||_oo/(||A||_oo.N.eps) = %e \n",Rnorm/(Anorm*N*eps));
    }

    if (isnan(Rnorm / (Anorm * N *eps)) || isinf(Rnorm / (Anorm * N *eps)) || (Rnorm / (Anorm * N * eps) > 60.0) ) {
        printf("-- Factorization is suspicious ! \n");
        info_factorization = 1;
    }
    else {
        printf("-- Factorization is CORRECT ! \n");
        info_factorization = 0;
    }

    free(work); free(Ql); free(Residual);

    return info_factorization;
}

/*--------------------------------------------------------------
 * Check the solution
 */

static int check_solution(int M, int N, int NRHS, PLASMA_Complex64_t *A, int LDA, PLASMA_Complex64_t *B, PLASMA_Complex64_t *X, int LDB, double eps)
{
    int info_solution;
    double Rnorm, Anorm, Xnorm, Bnorm;
    PLASMA_Complex64_t zone, mzone, zzero;
    double result;
    double *work = (double *)malloc(max(M, N)* sizeof(double));

    zone  =  1.0;
    mzone = -1.0;
    zzero =  0.0;

    BLAS_zge_norm( blas_colmajor, blas_inf_norm, M, N,    A, LDA, &Anorm );
    BLAS_zge_norm( blas_colmajor, blas_inf_norm, M, NRHS, B, LDB, &Bnorm );
    BLAS_zge_norm( blas_colmajor, blas_inf_norm, N, NRHS, X, LDB, &Xnorm );

    /* Compute Ax - b */
    cblas_zgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M, NRHS, N, CBLAS_SADDR(zone), A, LDA, X, LDB, CBLAS_SADDR(mzone), B, LDB);

    /* Compute A' * (Ax - b) */
    cblas_zgemm(CblasColMajor, CblasConjTrans, CblasNoTrans, N, NRHS, M, CBLAS_SADDR(zone), A, LDA, B, LDB, CBLAS_SADDR(zzero), X, LDB);
    BLAS_zge_norm( blas_colmajor, blas_inf_norm, N, NRHS, X, LDB, &Rnorm );

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
