/**
 *
 * @file testing_sgemm.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Emmanuel Agullo
 * @author Mathieu Faverge
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

static int check_solution(PLASMA_enum transA, PLASMA_enum transB, int M, int N, int K,
                          float alpha, float *A, int LDA,
                          float *B, int LDB,
                          float beta, float *Cref, float *Cplasma, int LDC);

int testing_sgemm(int argc, char **argv)
{
    /* Check for number of arguments*/
    if ( argc != 8) {
        USAGE("GEMM", "alpha beta M N K LDA LDB LDC",
              "   - alpha  : alpha coefficient\n"
              "   - beta   : beta coefficient\n"
              "   - M      : number of rows of matrices A and C\n"
              "   - N      : number of columns of matrices B and C\n"
              "   - K      : number of columns of matrix A / number of rows of matrix B\n"
              "   - LDA    : leading dimension of matrix A\n"
              "   - LDB    : leading dimension of matrix B\n"
              "   - LDC    : leading dimension of matrix C\n");
        return -1;
    }

    float alpha = (float) atol(argv[0]);
    float beta = (float) atol(argv[1]);
    int M     = atoi(argv[2]);
    int N     = atoi(argv[3]);
    int K     = atoi(argv[4]);
    int LDA   = atoi(argv[5]);
    int LDB   = atoi(argv[6]);
    int LDC   = atoi(argv[7]);

    float eps;
    int info_solution;
    int i, j, ta, tb;
    int LDAxK = LDA*max(M,K);
    int LDBxN = LDB*max(K,N);
    int LDCxN = LDC*N;

    float *A      = (float *)malloc(LDAxK*sizeof(float));
    float *B      = (float *)malloc(LDBxN*sizeof(float));
    float *C      = (float *)malloc(LDCxN*sizeof(float));
    float *Cinit  = (float *)malloc(LDCxN*sizeof(float));
    float *Cfinal = (float *)malloc(LDCxN*sizeof(float));

    /* Check if unable to allocate memory */
    if ((!A)||(!B)||(!Cinit)||(!Cfinal)){
        printf("Out of Memory \n ");
        return -2;
    }

    eps = LAPACKE_slamch_work('e');

    printf("\n");
    printf("------ TESTS FOR PLASMA SGEMM ROUTINE -------  \n");
    printf("            Size of the Matrix %d by %d\n", M, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 10.\n");

    /*----------------------------------------------------------
     *  TESTING SGEMM
     */

    /* Initialize A, B, C */
    LAPACKE_slarnv_work(IONE, ISEED, LDAxK, A);
    LAPACKE_slarnv_work(IONE, ISEED, LDBxN, B);
    LAPACKE_slarnv_work(IONE, ISEED, LDCxN, C);

#ifdef COMPLEX
    for (ta=0; ta<3; ta++) {
        for (tb=0; tb<3; tb++) {
#else
    for (ta=0; ta<2; ta++) {
        for (tb=0; tb<2; tb++) {
#endif
            for ( i = 0; i < M; i++)
                for (  j = 0; j < N; j++)
                    Cinit[LDC*j+i] = C[LDC*j+i];
            for ( i = 0; i < M; i++)
                for (  j = 0; j < N; j++)
                    Cfinal[LDC*j+i] = C[LDC*j+i];

            /* PLASMA SGEMM */
            PLASMA_sgemm(trans[ta], trans[tb], M, N, K, alpha, A, LDA, B, LDB, beta, Cfinal, LDC);

            /* Check the solution */
            info_solution = check_solution(trans[ta], trans[tb], M, N, K, 
                                           alpha, A, LDA, B, LDB, beta, Cinit, Cfinal, LDC);

            if (info_solution == 0) {
                printf("***************************************************\n");
                printf(" ---- TESTING SGEMM (%s, %s) ............... PASSED !\n", transstr[ta], transstr[tb]);
                printf("***************************************************\n");
            }
            else {
                printf("************************************************\n");
                printf(" - TESTING SGEMM (%s, %s) ... FAILED !\n", transstr[ta], transstr[tb]);
                printf("************************************************\n");
            }
        }
    }
#ifdef _UNUSED_
    }}
#endif
    free(A); free(B); free(C);
    free(Cinit); free(Cfinal);

    return 0;
}

/*--------------------------------------------------------------
 * Check the solution
 */

static int check_solution(PLASMA_enum transA, PLASMA_enum transB, int M, int N, int K,
                          float alpha, float *A, int LDA,
                          float *B, int LDB,
                          float beta, float *Cref, float *Cplasma, int LDC)
{
    int info_solution;
    float Anorm, Bnorm, Cinitnorm, Cplasmanorm, Clapacknorm, Rnorm, result;
    float eps;
    float beta_const;

    float *work = (float *)malloc(max(K,max(M, N))* sizeof(float));
    int Am, An, Bm, Bn;

    beta_const  = -1.0;

    if (transA == PlasmaNoTrans) {
        Am = M; An = K;
    } else {
        Am = K; An = M;
    }
    if (transB == PlasmaNoTrans) {
        Bm = K; Bn = N;
    } else {
        Bm = N; Bn = K;
    }

    Anorm       = LAPACKE_slange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), Am, An, A,       LDA, work);
    Bnorm       = LAPACKE_slange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), Bm, Bn, B,       LDB, work);
    Cinitnorm   = LAPACKE_slange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M,  N,  Cref,    LDC, work);
    Cplasmanorm = LAPACKE_slange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M,  N,  Cplasma, LDC, work);

    cblas_sgemm(CblasColMajor, (CBLAS_TRANSPOSE)transA, (CBLAS_TRANSPOSE)transB, M, N, K, 
                (alpha), A, LDA, B, LDB, (beta), Cref, LDC);

    Clapacknorm = LAPACKE_slange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, N, Cref, LDC, work);

    cblas_saxpy(LDC * N, (beta_const), Cplasma, 1, Cref, 1);

    Rnorm = LAPACKE_slange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, N, Cref, LDC, work);

    eps = LAPACKE_slamch_work('e');

    printf("Rnorm %e, Anorm %e, Bnorm %e, Cinitnorm %e, Cplasmanorm %e, Clapacknorm %e\n", 
           Rnorm, Anorm, Bnorm, Cinitnorm, Cplasmanorm, Clapacknorm);

    result = Rnorm / ((Anorm + Bnorm + Cinitnorm) * N * eps);
    printf("============\n");
    printf("Checking the norm of the difference against reference SGEMM \n");
    printf("-- ||Cplasma - Clapack||_oo/((||A||_oo+||B||_oo+||C||_oo).N.eps) = %e \n", 
           result);

    if (  isnan(Rnorm) || isinf(Rnorm) || isnan(result) || isinf(result) || (result > 10.0) ) {
         printf("-- The solution is suspicious ! \n");
         info_solution = 1;
    }
    else {
         printf("-- The solution is CORRECT ! \n");
         info_solution= 0 ;
    }

    free(work);

    return info_solution;
}
