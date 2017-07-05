/**
 *
 * @file testing_cherk.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Mathieu Faverge
 * @date 2010-11-15
 * @generated c Fri Apr  1 11:03:06 2016
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

static int check_solution(PLASMA_enum uplo, PLASMA_enum trans, int N, int K,
                          float alpha, PLASMA_Complex32_t *A, int LDA,
                          float beta,  PLASMA_Complex32_t *Cref, PLASMA_Complex32_t *Cplasma, int LDC);


int testing_cherk(int argc, char **argv)
{
    /* Check for number of arguments*/
    if ( argc != 6 ){
        USAGE("HERK", "alpha beta M N LDA LDC",
              "   - alpha : alpha coefficient\n"
              "   - beta : beta coefficient\n"
              "   - N : number of columns and rows of matrix C and number of row of matrix A\n"
              "   - K : number of columns of matrix A\n"
              "   - LDA : leading dimension of matrix A\n"
              "   - LDC : leading dimension of matrix C\n");
        return -1;
    }

    float alpha = (float) atol(argv[0]);
    float beta  = (float) atol(argv[1]);
    int N     = atoi(argv[2]);
    int K     = atoi(argv[3]);
    int LDA   = atoi(argv[4]);
    int LDC   = atoi(argv[5]);
    int NKmax = max(N, K);

    float eps;
    int info_solution;
    int u, t;
    size_t LDAxK = LDA*NKmax;
    size_t LDCxN = LDC*N;

    PLASMA_Complex32_t *A      = (PLASMA_Complex32_t *)malloc(LDAxK*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *C      = (PLASMA_Complex32_t *)malloc(LDCxN*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *Cinit  = (PLASMA_Complex32_t *)malloc(LDCxN*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *Cfinal = (PLASMA_Complex32_t *)malloc(LDCxN*sizeof(PLASMA_Complex32_t));

    /* Check if unable to allocate memory */
    if ( (!A) || (!Cinit) || (!Cfinal) ){
        printf("Out of Memory \n ");
        return -2;
    }

    eps = LAPACKE_slamch_work('e');

    printf("\n");
    printf("------ TESTS FOR PLASMA CHERK ROUTINE -------  \n");
    printf("            Size of the Matrix A %d by %d\n", N, K);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 10.\n");

    /*----------------------------------------------------------
    *  TESTING CHERK
    */

    /* Initialize A */
    LAPACKE_clarnv_work(IONE, ISEED, LDAxK, A);

    /* Initialize C */
    PLASMA_cplgsy( (float)0., N, C, LDC, 51 );

    for (u=0; u<2; u++) {
        for (t=0; t<3; t++) {
            if (trans[t] == PlasmaTrans) continue;

            memcpy(Cinit,  C, LDCxN*sizeof(PLASMA_Complex32_t));
            memcpy(Cfinal, C, LDCxN*sizeof(PLASMA_Complex32_t));

            /* PLASMA CHERK */
            PLASMA_cherk(uplo[u], trans[t], N, K, alpha, A, LDA, beta, Cfinal, LDC);

            /* Check the solution */
            info_solution = check_solution(uplo[u], trans[t], N, K,
                                           alpha, A, LDA, beta, Cinit, Cfinal, LDC);

            if (info_solution == 0) {
                printf("***************************************************\n");
                printf(" ---- TESTING CHERK (%5s, %s) ........... PASSED !\n", uplostr[u], transstr[t]);
                printf("***************************************************\n");
            }
            else {
                printf("************************************************\n");
                printf(" - TESTING CHERK (%5s, %s) ... FAILED !\n", uplostr[u], transstr[t]);
                printf("************************************************\n");
            }
        }
    }

    free(A); free(C);
    free(Cinit); free(Cfinal);

    return 0;
}

/*--------------------------------------------------------------
 * Check the solution
 */

static int check_solution(PLASMA_enum uplo, PLASMA_enum trans, int N, int K,
                          float alpha, PLASMA_Complex32_t *A, int LDA,
                          float beta,  PLASMA_Complex32_t *Cref, PLASMA_Complex32_t *Cplasma, int LDC)
{
    int info_solution;
    float Anorm, Cinitnorm, Cplasmanorm, Clapacknorm, Rnorm;
    float eps;
    PLASMA_Complex32_t beta_const;
    float result;
    float *work = (float *)malloc(max(N, K)* sizeof(float));

    beta_const  = -1.0;
    Anorm       = LAPACKE_clange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm),
                                (trans == PlasmaNoTrans) ? N : K,
                                (trans == PlasmaNoTrans) ? K : N, A, LDA, work);
    Cinitnorm   = LAPACKE_clange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Cref,    LDC, work);
    Cplasmanorm = LAPACKE_clange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Cplasma, LDC, work);

    cblas_cherk(CblasColMajor, (CBLAS_UPLO)uplo, (CBLAS_TRANSPOSE)trans,
                N, K, (alpha), A, LDA, (beta), Cref, LDC);

    Clapacknorm = LAPACKE_clange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Cref, LDC, work);

    cblas_caxpy(LDC*N, CBLAS_SADDR(beta_const), Cplasma, 1, Cref, 1);

    Rnorm = LAPACKE_clange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Cref, LDC, work);

    eps = LAPACKE_slamch_work('e');

    printf("Rnorm %e, Anorm %e, Cinitnorm %e, Cplasmanorm %e, Clapacknorm %e\n",
           Rnorm, Anorm, Cinitnorm, Cplasmanorm, Clapacknorm);

    result = Rnorm / ((Anorm + Cinitnorm) * N * eps);

    printf("============\n");
    printf("Checking the norm of the difference against reference CHERK \n");
    printf("-- ||Cplasma - Clapack||_oo/((||A||_oo+||C||_oo).N.eps) = %e \n", result);

    if ( isinf(Clapacknorm) || isinf(Cplasmanorm) || isnan(result) || isinf(result) || (result > 10.0) ) {
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
