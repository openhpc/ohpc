/**
 *
 * @file testing_sgels.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Bilel Hadri
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

#undef COMPLEX
#define REAL

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
BLAS_ssy_norm(enum blas_order_type order, enum blas_norm_type norm,
  enum blas_uplo_type uplo, int n, const float *a, int lda, float *res) {
  int i, j; float anorm, v;
  char rname[] = "BLAS_ssy_norm";

  if (order != blas_colmajor) BLAS_error( rname, -1, order, 0 );

  if (norm == blas_inf_norm) {
    anorm = 0.0;
    if (blas_upper == uplo) {
      for (i = 0; i < n; ++i) {
        v = 0.0;
        for (j = 0; j < i; ++j) {
          v += fabsf( a[j + i * lda] );
        }
        for (j = i; j < n; ++j) {
          v += fabsf( a[i + j * lda] );
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

static int check_orthogonality(int, int, int, float*, float);
static int check_factorization(int, int, float*, float*, int, float*, float);
static int check_solution(int, int, int, float*, int, float*, float*, int, float);

int testing_sgels(int argc, char **argv)
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
    float eps;
    int info_ortho, info_solution, info_factorization;
    int i,j;
    int LDAxN = LDA*N;
    int LDBxNRHS = LDB*NRHS;

    float *A1 = (float *)malloc(LDA*N*sizeof(float));
    float *A2 = (float *)malloc(LDA*N*sizeof(float));
    float *B1 = (float *)malloc(LDB*NRHS*sizeof(float));
    float *B2 = (float *)malloc(LDB*NRHS*sizeof(float));
    float *Q  = (float *)malloc(LDA*N*sizeof(float));
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

    PLASMA_Alloc_Workspace_sgels(M, N, &T);
    eps = BLAS_sfpinfo( blas_eps );

    /*----------------------------------------------------------
    *  TESTING SGELS
    */

    /* Initialize A1 and A2 */
    LAPACKE_slarnv_work(IONE, ISEED, LDAxN, A1);
    for (i = 0; i < M; i++)
        for (j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i] ;

    /* Initialize B1 and B2 */
    LAPACKE_slarnv_work(IONE, ISEED, LDBxNRHS, B1);
    for (i = 0; i < M; i++)
        for (j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i] ;

    /* PLASMA SGELS */
    PLASMA_sgels(PlasmaNoTrans, M, N, NRHS, A2, LDA, T, B2, LDB);

    /* PLASMA SGELS */
    if (M >= N)
       /* Building the economy-size Q */
       PLASMA_sorgqr(M, N, K, A2, LDA, T, Q, LDA);
    else
       /* Building the economy-size Q */
       PLASMA_sorglq(M, N, K, A2, LDA, T, Q, LDA);

    printf("\n");
    printf("------ TESTS FOR PLASMA SGELS ROUTINE -------  \n");
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
        printf(" ---- TESTING SGELS ...................... PASSED !\n");
        printf("***************************************************\n");
    }
    else {
        printf("************************************************\n");
        printf(" - TESTING SGELS ... FAILED !\n");
        printf("************************************************\n");
    }

    /*-------------------------------------------------------------
    *  TESTING SGEQRF + SGEQRS or SGELQF + SGELQS
    */

    /* Initialize A1 and A2 */
    LAPACKE_slarnv_work(IONE, ISEED, LDAxN, A1);
    for (i = 0; i < M; i++)
        for (j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    LAPACKE_slarnv_work(IONE, ISEED, LDBxNRHS, B1);
    for (i = 0; i < M; i++)
        for (j = 0; j < NRHS; j++)
             B2[LDB*j+i] = B1[LDB*j+i];

    if (M >= N) {
        printf("\n");
        printf("------ TESTS FOR PLASMA SGEQRF + SGEQRS ROUTINE -------  \n");
        printf("            Size of the Matrix %d by %d\n", M, N);
        printf("\n");
        printf(" The matrix A is randomly generated for each test.\n");
        printf("============\n");
        printf(" The relative machine precision (eps) is to be %e \n", eps);
        printf(" Computational tests pass if scaled residuals are less than 60.\n");

        /* Plasma routines */
        PLASMA_sgeqrf(M, N, A2, LDA, T);
        PLASMA_sorgqr(M, N, K, A2, LDA, T, Q, LDA);
        PLASMA_sgeqrs(M, N, NRHS, A2, LDA, T, B2, LDB);

        /* Check the orthogonality, factorization and the solution */
        info_ortho = check_orthogonality(M, N, LDA, Q, eps);
        info_factorization = check_factorization(M, N, A1, A2, LDA, Q, eps);
        info_solution = check_solution(M, N, NRHS, A1, LDA, B1, B2, LDB, eps);

        if ((info_solution == 0)&(info_factorization == 0)&(info_ortho == 0)) {
            printf("***************************************************\n");
            printf(" ---- TESTING SGEQRF + SGEQRS ............ PASSED !\n");
            printf("***************************************************\n");
        }
        else{
            printf("***************************************************\n");
            printf(" - TESTING SGEQRF + SGEQRS ... FAILED !\n");
            printf("***************************************************\n");
        }
    }
    else  {
        printf("\n");
        printf("------ TESTS FOR PLASMA SGELQF + SGELQS ROUTINE -------  \n");
        printf("            Size of the Matrix %d by %d\n", M, N);
        printf("\n");
        printf(" The matrix A is randomly generated for each test.\n");
        printf("============\n");
        printf(" The relative machine precision (eps) is to be %e \n", eps);
        printf(" Computational tests pass if scaled residuals are less than 60.\n");

        /* Plasma routines */
        PLASMA_sgelqf(M, N, A2, LDA, T);
        PLASMA_sorglq(M, N, K, A2, LDA, T, Q, LDA);
        PLASMA_sgelqs(M, N, NRHS, A2, LDA, T, B2, LDB);

       /* Check the orthogonality, factorization and the solution */
       info_ortho = check_orthogonality(M, N, LDA, Q, eps);
       info_factorization = check_factorization(M, N, A1, A2, LDA, Q, eps);
       info_solution = check_solution(M, N, NRHS, A1, LDA, B1, B2, LDB, eps);

       if ( (info_solution == 0) & (info_factorization == 0) & (info_ortho == 0) ) {
          printf("***************************************************\n");
          printf(" ---- TESTING SGELQF + SGELQS ............ PASSED !\n");
          printf("***************************************************\n");
       }
       else {
          printf("***************************************************\n");
          printf(" - TESTING SGELQF + SGELQS ... FAILED !\n");
          printf("***************************************************\n");
        }
    }

    /*----------------------------------------------------------
    *  TESTING SGEQRF + ZORMQR + STRSM
    */

    /* Initialize A1 and A2 */
    LAPACKE_slarnv_work(IONE, ISEED, LDAxN, A1);
    for (i = 0; i < M; i++)
        for (j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    /* Initialize B1 and B2 */
    memset(B2, 0, LDB*NRHS*sizeof(float));
    LAPACKE_slarnv_work(IONE, ISEED, LDBxNRHS, B1);
    for (i = 0; i < M; i++)
        for (j = 0; j < NRHS; j++)
            B2[LDB*j+i] = B1[LDB*j+i];

    if (M >= N) {
        printf("\n");
        printf("------ TESTS FOR PLASMA SGEQRF + SORMQR + STRSM  ROUTINE -------  \n");
        printf("            Size of the Matrix %d by %d\n", M, N);
        printf("\n");
        printf(" The matrix A is randomly generated for each test.\n");
        printf("============\n");
        printf(" The relative machine precision (eps) is to be %e \n",eps);
        printf(" Computational tests pass if scaled residuals are less than 60.\n");

        PLASMA_sgeqrf(M, N, A2, LDA, T);
        PLASMA_sorgqr(M, N, K, A2, LDA, T, Q, LDA);
        PLASMA_sormqr(PlasmaLeft, PlasmaTrans, M, NRHS, N, A2, LDA, T, B2, LDB);
        PLASMA_strsm(PlasmaLeft, PlasmaUpper, PlasmaNoTrans, PlasmaNonUnit, N, NRHS, 1.0, A2, LDA, B2, LDB);
    }
    else {
        printf("\n");
        printf("------ TESTS FOR PLASMA SGELQF + SORMLQ + STRSM  ROUTINE -------  \n");
        printf("            Size of the Matrix %d by %d\n", M, N);
        printf("\n");
        printf(" The matrix A is randomly generated for each test.\n");
        printf("============\n");
        printf(" The relative machine precision (eps) is to be %e \n",eps);
        printf(" Computational tests pass if scaled residuals are less than 60.\n");

        PLASMA_sgelqf(M, N, A2, LDA, T);
        PLASMA_strsm(PlasmaLeft, PlasmaLower, PlasmaNoTrans, PlasmaNonUnit, M, NRHS, 1.0, A2, LDA, B2, LDB);
        PLASMA_sorglq(M, N, K, A2, LDA, T, Q, LDA);
        PLASMA_sormlq(PlasmaLeft, PlasmaTrans, N, NRHS, M, A2, LDA, T, B2, LDB);
    }

    /* Check the orthogonality, factorization and the solution */
    info_ortho = check_orthogonality(M, N, LDA, Q, eps);
    info_factorization = check_factorization(M, N, A1, A2, LDA, Q, eps);
    info_solution = check_solution(M, N, NRHS, A1, LDA, B1, B2, LDB, eps);

    if ( (info_solution == 0) & (info_factorization == 0) & (info_ortho == 0) ) {
        if (M >= N) {
            printf("***************************************************\n");
            printf(" ---- TESTING SGEQRF + SORMQR + STRSM .... PASSED !\n");
            printf("***************************************************\n");
        }
        else {
            printf("***************************************************\n");
            printf(" ---- TESTING SGELQF + STRSM + SORMLQ .... PASSED !\n");
            printf("***************************************************\n");
        }
    }
    else {
        if (M >= N) {
            printf("***************************************************\n");
            printf(" - TESTING SGEQRF + SORMQR + STRSM ... FAILED !\n");
            printf("***************************************************\n");
        }
        else {
            printf("***************************************************\n");
            printf(" - TESTING SGELQF + STRSM + SORMLQ ... FAILED !\n");
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

static int check_orthogonality(int M, int N, int LDQ, float *Q, float eps)
{
    float alpha, beta;
    float normQ;
    int info_ortho;
    int i;
    int minMN = min(M, N);

    float *work = (float *)malloc(minMN*sizeof(float));

    alpha = 1.0;
    beta  = -1.0;

    /* Build the idendity matrix USE DLASET?*/
    float *Id = (float *) malloc(minMN*minMN*sizeof(float));
    memset((void*)Id, 0, minMN*minMN*sizeof(float));
    for (i = 0; i < minMN; i++)
        Id[i*minMN+i] = (float)1.0;

    /* Perform Id - Q'Q */
    if (M >= N)
        cblas_ssyrk(CblasColMajor, CblasUpper, CblasTrans, N, M, alpha, Q, LDQ, beta, Id, N);
    else
        cblas_ssyrk(CblasColMajor, CblasUpper, CblasNoTrans, M, N, alpha, Q, LDQ, beta, Id, M);

    BLAS_ssy_norm( blas_colmajor, blas_inf_norm, blas_upper, minMN, Id, minMN, &normQ );

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

static int check_factorization(int M, int N, float *A1, float *A2, int LDA, float *Q, float eps )
{
    float Anorm, Rnorm;
    float alpha, beta;
    int info_factorization;
    int i,j;

    float *Ql       = (float *)malloc(M*N*sizeof(float));
    float *Residual = (float *)malloc(M*N*sizeof(float));
    float *work              = (float *)malloc(max(M,N)*sizeof(float));

    alpha=1.0;
    beta=0.0;

    if (M >= N) {
        /* Extract the R */
        float *R = (float *)malloc(N*N*sizeof(float));
        memset((void*)R, 0, N*N*sizeof(float));
        LAPACKE_slacpy_work(LAPACK_COL_MAJOR,'u', M, N, A2, LDA, R, N);

        /* Perform Ql=Q*R */
        memset((void*)Ql, 0, M*N*sizeof(float));
        cblas_sgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M, N, N, (alpha), Q, LDA, R, N, (beta), Ql, M);
        free(R);
    }
    else {
        /* Extract the L */
        float *L = (float *)malloc(M*M*sizeof(float));
        memset((void*)L, 0, M*M*sizeof(float));
        LAPACKE_slacpy_work(LAPACK_COL_MAJOR,'l', M, N, A2, LDA, L, M);

    /* Perform Ql=LQ */
        memset((void*)Ql, 0, M*N*sizeof(float));
        cblas_sgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M, N, M, (alpha), L, M, Q, LDA, (beta), Ql, M);
        free(L);
    }

    /* Compute the Residual */
    for (i = 0; i < M; i++)
        for (j = 0 ; j < N; j++)
            Residual[j*M+i] = A1[j*LDA+i]-Ql[j*M+i];

    BLAS_sge_norm( blas_colmajor, blas_inf_norm, M, N, Residual, M, &Rnorm );
    BLAS_sge_norm( blas_colmajor, blas_inf_norm, M, N, A2, LDA, &Anorm );

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

static int check_solution(int M, int N, int NRHS, float *A, int LDA, float *B, float *X, int LDB, float eps)
{
    int info_solution;
    float Rnorm, Anorm, Xnorm, Bnorm;
    float zone, mzone, zzero;
    float result;
    float *work = (float *)malloc(max(M, N)* sizeof(float));

    zone  =  1.0;
    mzone = -1.0;
    zzero =  0.0;

    BLAS_sge_norm( blas_colmajor, blas_inf_norm, M, N,    A, LDA, &Anorm );
    BLAS_sge_norm( blas_colmajor, blas_inf_norm, M, NRHS, B, LDB, &Bnorm );
    BLAS_sge_norm( blas_colmajor, blas_inf_norm, N, NRHS, X, LDB, &Xnorm );

    /* Compute Ax - b */
    cblas_sgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M, NRHS, N, (zone), A, LDA, X, LDB, (mzone), B, LDB);

    /* Compute A' * (Ax - b) */
    cblas_sgemm(CblasColMajor, CblasTrans, CblasNoTrans, N, NRHS, M, (zone), A, LDA, B, LDB, (zzero), X, LDB);
    BLAS_sge_norm( blas_colmajor, blas_inf_norm, N, NRHS, X, LDB, &Rnorm );

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
