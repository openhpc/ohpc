/**
 *
 * @file testing_dsyr2k.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Mathieu Faverge
 * @date 2010-11-15
 * @generated d Fri Apr  1 11:03:05 2016
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
#include "testing_dmain.h"

static int check_solution(PLASMA_enum uplo, PLASMA_enum trans, int N, int K,
                          double alpha, double *A, int LDA, 
                          double *B, int LDB,
                          double beta,  double *Cref, double *Cplasma, int LDC);


int testing_dsyr2k(int argc, char **argv)
{
    /* Check for number of arguments*/
    if ( argc != 7 ){
        USAGE("SYR2K", "alpha beta M N LDA LDB LDC",
              "   - alpha : alpha coefficient\n"
              "   - beta : beta coefficient\n"
              "   - N : number of columns and rows of matrix C and number of row of matrix A and B\n"
              "   - K : number of columns of matrix A and B\n"
              "   - LDA : leading dimension of matrix A\n"
              "   - LDB : leading dimension of matrix B\n"
              "   - LDC : leading dimension of matrix C\n");
        return -1;
    }

    double alpha = (double) atol(argv[0]);
    double beta  = (double) atol(argv[1]);
    int N     = atoi(argv[2]);
    int K     = atoi(argv[3]);
    int LDA   = atoi(argv[4]);
    int LDB   = atoi(argv[5]);
    int LDC   = atoi(argv[6]);
    int NKmax = max(N, K);

    double eps;
    int info_solution;
    int u, t;
    size_t LDAxK = LDA*NKmax;
    size_t LDBxK = LDB*NKmax;
    size_t LDCxN = LDC*N;

    double *A      = (double *)malloc(LDAxK*sizeof(double));
    double *B      = (double *)malloc(LDBxK*sizeof(double));
    double *C      = (double *)malloc(LDCxN*sizeof(double));
    double *Cinit  = (double *)malloc(LDCxN*sizeof(double));
    double *Cfinal = (double *)malloc(LDCxN*sizeof(double));

    /* Check if unable to allocate memory */
    if ( (!A) || (!B) || (!Cinit) || (!Cfinal) ){
        printf("Out of Memory \n ");
        return -2;
    }

    eps = LAPACKE_dlamch_work('e');

    printf("\n");
    printf("------ TESTS FOR PLASMA DSYR2K ROUTINE -------  \n");
    printf("            Size of the Matrix C %d by %d\n", N, K);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 10.\n");

    /*----------------------------------------------------------
    *  TESTING DSYR2K
    */

    /* Initialize A,B */
    LAPACKE_dlarnv_work(IONE, ISEED, LDAxK, A);
    LAPACKE_dlarnv_work(IONE, ISEED, LDBxK, B);

    /* Initialize C */
    PLASMA_dplgsy( (double)0., N, C, LDC, 51 );

    for (u=0; u<2; u++) {
        for (t=0; t<2; t++) {

            memcpy(Cinit,  C, LDCxN*sizeof(double));
            memcpy(Cfinal, C, LDCxN*sizeof(double));
            
            /* PLASMA DSYR2K */
            PLASMA_dsyr2k(uplo[u], trans[t], N, K, alpha, A, LDA, B, LDB, beta, Cfinal, LDC);

            /* Check the solution */
            info_solution = check_solution(uplo[u], trans[t], N, K, 
                                           alpha, A, LDA, B, LDB, beta, Cinit, Cfinal, LDC);

            if (info_solution == 0) {
                printf("***************************************************\n");
                printf(" ---- TESTING DSYR2K (%5s, %s) ........... PASSED !\n", uplostr[u], transstr[t]);
                printf("***************************************************\n");
            }
            else {
                printf("************************************************\n");
                printf(" - TESTING DSYR2K (%5s, %s) ... FAILED !\n", uplostr[u], transstr[t]);
                printf("************************************************\n");
            }
        }
    }

    free(A); free(B); free(C);
    free(Cinit); free(Cfinal);

    return 0;
}

/*--------------------------------------------------------------
 * Check the solution
 */

static int check_solution(PLASMA_enum uplo, PLASMA_enum trans, int N, int K,
                          double alpha, double *A, int LDA,
                          double *B, int LDB,
                          double beta, double *Cref, double *Cplasma, int LDC)
{
    int info_solution;
    double Anorm, Bnorm, Cinitnorm, Cplasmanorm, Clapacknorm, Rnorm, result;
    double eps;
    double beta_const;

    double *work = (double *)malloc(max(N, K)* sizeof(double));

    beta_const  = -1.0;
    Anorm       = LAPACKE_dlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), 
                                      (trans == PlasmaNoTrans) ? N : K, 
                                      (trans == PlasmaNoTrans) ? K : N, A, LDA, work);
    Bnorm       = LAPACKE_dlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), 
                                      (trans == PlasmaNoTrans) ? N : K, 
                                      (trans == PlasmaNoTrans) ? K : N, B, LDB, work);
    Cinitnorm   = LAPACKE_dlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Cref,    LDC, work);
    Cplasmanorm = LAPACKE_dlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Cplasma, LDC, work);

    cblas_dsyr2k(CblasColMajor, (CBLAS_UPLO)uplo, (CBLAS_TRANSPOSE)trans, 
                 N, K, (alpha), A, LDA, B, LDB, (beta), Cref, LDC);

    Clapacknorm = LAPACKE_dlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Cref, LDC, work);

    cblas_daxpy(LDC*N, (beta_const), Cplasma, 1, Cref, 1);

    Rnorm = LAPACKE_dlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Cref, LDC, work);

    eps = LAPACKE_dlamch_work('e');
    
    printf("Rnorm %e, Anorm %e, Cinitnorm %e, Cplasmanorm %e, Clapacknorm %e\n",
           Rnorm, Anorm, Cinitnorm, Cplasmanorm, Clapacknorm);

    result = Rnorm / ((Anorm + Bnorm + Cinitnorm) * N * eps);
    printf("============\n");
    printf("Checking the norm of the difference against reference DSYR2K \n");
    printf("-- ||Cplasma - Clapack||_oo/((||A||_oo+||C||_oo).N.eps) = %e \n", result);

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
