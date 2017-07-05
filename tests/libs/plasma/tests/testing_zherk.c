/**
 *
 * @file testing_zherk.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Mathieu Faverge
 * @date 2010-11-15
 * @precisions normal z -> c
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

static int check_solution(PLASMA_enum uplo, PLASMA_enum trans, int N, int K,
                          double alpha, PLASMA_Complex64_t *A, int LDA,
                          double beta,  PLASMA_Complex64_t *Cref, PLASMA_Complex64_t *Cplasma, int LDC);


int testing_zherk(int argc, char **argv)
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

    double alpha = (double) atol(argv[0]);
    double beta  = (double) atol(argv[1]);
    int N     = atoi(argv[2]);
    int K     = atoi(argv[3]);
    int LDA   = atoi(argv[4]);
    int LDC   = atoi(argv[5]);
    int NKmax = max(N, K);

    double eps;
    int info_solution;
    int u, t;
    size_t LDAxK = LDA*NKmax;
    size_t LDCxN = LDC*N;

    PLASMA_Complex64_t *A      = (PLASMA_Complex64_t *)malloc(LDAxK*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *C      = (PLASMA_Complex64_t *)malloc(LDCxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Cinit  = (PLASMA_Complex64_t *)malloc(LDCxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Cfinal = (PLASMA_Complex64_t *)malloc(LDCxN*sizeof(PLASMA_Complex64_t));

    /* Check if unable to allocate memory */
    if ( (!A) || (!Cinit) || (!Cfinal) ){
        printf("Out of Memory \n ");
        return -2;
    }

    eps = LAPACKE_dlamch_work('e');

    printf("\n");
    printf("------ TESTS FOR PLASMA ZHERK ROUTINE -------  \n");
    printf("            Size of the Matrix A %d by %d\n", N, K);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 10.\n");

    /*----------------------------------------------------------
    *  TESTING ZHERK
    */

    /* Initialize A */
    LAPACKE_zlarnv_work(IONE, ISEED, LDAxK, A);

    /* Initialize C */
    PLASMA_zplgsy( (double)0., N, C, LDC, 51 );

    for (u=0; u<2; u++) {
        for (t=0; t<3; t++) {
            if (trans[t] == PlasmaTrans) continue;

            memcpy(Cinit,  C, LDCxN*sizeof(PLASMA_Complex64_t));
            memcpy(Cfinal, C, LDCxN*sizeof(PLASMA_Complex64_t));

            /* PLASMA ZHERK */
            PLASMA_zherk(uplo[u], trans[t], N, K, alpha, A, LDA, beta, Cfinal, LDC);

            /* Check the solution */
            info_solution = check_solution(uplo[u], trans[t], N, K,
                                           alpha, A, LDA, beta, Cinit, Cfinal, LDC);

            if (info_solution == 0) {
                printf("***************************************************\n");
                printf(" ---- TESTING ZHERK (%5s, %s) ........... PASSED !\n", uplostr[u], transstr[t]);
                printf("***************************************************\n");
            }
            else {
                printf("************************************************\n");
                printf(" - TESTING ZHERK (%5s, %s) ... FAILED !\n", uplostr[u], transstr[t]);
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
                          double alpha, PLASMA_Complex64_t *A, int LDA,
                          double beta,  PLASMA_Complex64_t *Cref, PLASMA_Complex64_t *Cplasma, int LDC)
{
    int info_solution;
    double Anorm, Cinitnorm, Cplasmanorm, Clapacknorm, Rnorm;
    double eps;
    PLASMA_Complex64_t beta_const;
    double result;
    double *work = (double *)malloc(max(N, K)* sizeof(double));

    beta_const  = -1.0;
    Anorm       = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm),
                                (trans == PlasmaNoTrans) ? N : K,
                                (trans == PlasmaNoTrans) ? K : N, A, LDA, work);
    Cinitnorm   = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Cref,    LDC, work);
    Cplasmanorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Cplasma, LDC, work);

    cblas_zherk(CblasColMajor, (CBLAS_UPLO)uplo, (CBLAS_TRANSPOSE)trans,
                N, K, (alpha), A, LDA, (beta), Cref, LDC);

    Clapacknorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Cref, LDC, work);

    cblas_zaxpy(LDC*N, CBLAS_SADDR(beta_const), Cplasma, 1, Cref, 1);

    Rnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Cref, LDC, work);

    eps = LAPACKE_dlamch_work('e');

    printf("Rnorm %e, Anorm %e, Cinitnorm %e, Cplasmanorm %e, Clapacknorm %e\n",
           Rnorm, Anorm, Cinitnorm, Cplasmanorm, Clapacknorm);

    result = Rnorm / ((Anorm + Cinitnorm) * N * eps);

    printf("============\n");
    printf("Checking the norm of the difference against reference ZHERK \n");
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
