/**
 *
 * @file testing_zgeadd.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Emmanuel Agullo
 * @author Mathieu Faverge
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

static int check_tr_solution(PLASMA_enum uplo, PLASMA_enum trans, int M, int N,
                             PLASMA_Complex64_t alpha, PLASMA_Complex64_t *A, int LDA,
                             PLASMA_Complex64_t beta, PLASMA_Complex64_t *Bref, PLASMA_Complex64_t *Bplasma, int LDB);
static int check_ge_solution(PLASMA_enum trans, int M, int N,
                             PLASMA_Complex64_t alpha, PLASMA_Complex64_t *A, int LDA,
                             PLASMA_Complex64_t beta, PLASMA_Complex64_t *Bref, PLASMA_Complex64_t *Bplasma, int LDB);

int testing_zgeadd(int argc, char **argv)
{
    /* Check for number of arguments*/
    if ( argc != 6 ) {
        USAGE("GEADD", "alpha beta M N LDA LDB",
              "   - alpha  : alpha coefficient\n"
              "   - beta   : beta coefficient\n"
              "   - M      : number of rows of matrices A and C\n"
              "   - N      : number of columns of matrices B and C\n"
              "   - LDA    : leading dimension of matrix A\n"
              "   - LDB    : leading dimension of matrix B\n" );
        return -1;
    }

    PLASMA_Complex64_t alpha = (PLASMA_Complex64_t) atol(argv[0]);
    PLASMA_Complex64_t beta  = (PLASMA_Complex64_t) atol(argv[1]);
    int M   = atoi(argv[2]);
    int N   = atoi(argv[3]);
    int LDA = atoi(argv[4]);
    int LDB = atoi(argv[5]);

    double eps;
    int info_solution;
    int t, u;
    int LDAxN = LDA*max(M,N);
    int LDBxN = LDB*N;

    PLASMA_Complex64_t *A      = (PLASMA_Complex64_t *)malloc(LDAxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *B      = (PLASMA_Complex64_t *)malloc(LDBxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Binit  = (PLASMA_Complex64_t *)malloc(LDBxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Bfinal = (PLASMA_Complex64_t *)malloc(LDBxN*sizeof(PLASMA_Complex64_t));

    /* Check if unable to allocate memory */
    if ((!A)||(!B)||(!Binit)||(!Bfinal)){
        printf("Out of Memory \n ");
        return -2;
    }

    eps = LAPACKE_dlamch_work('e');

    printf("\n");
    printf("------ TESTS FOR PLASMA ZGEADD ROUTINE -------  \n");
    printf("            Size of the Matrix %d by %d\n", M, N);
    printf("\n");
    printf(" The matrices A and B are randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 10.\n");

    /*----------------------------------------------------------
     *  TESTING ZGEADD
     */

    /* Initialize A, B */
    LAPACKE_zlarnv_work(IONE, ISEED, LDAxN, A);
    LAPACKE_zlarnv_work(IONE, ISEED, LDBxN, B);

#ifdef COMPLEX
    for (t=0; t<3; t++) {
#else
    for (t=0; t<2; t++) {
#endif
        memcpy( Binit,  B, LDBxN*sizeof(PLASMA_Complex64_t));
        memcpy( Bfinal, B, LDBxN*sizeof(PLASMA_Complex64_t));

        /* PLASMA ZGEADD */
        PLASMA_zgeadd(trans[t], M, N, alpha, A, LDA, beta, Bfinal, LDB);

        /* Check the solution */
        info_solution = check_ge_solution(trans[t], M, N,
                                          alpha, A, LDA,
                                          beta, Binit, Bfinal, LDB);

        if (info_solution == 0) {
            printf("***************************************************\n");
            printf(" ---- TESTING ZGEADD (%s) ............... PASSED !\n", transstr[t]);
            printf("***************************************************\n");
        }
        else {
            printf("************************************************\n");
            printf(" - TESTING ZGEADD (%s) ... FAILED !\n", transstr[t]);
            printf("************************************************\n");
        }
    }
#ifdef _UNUSED_
    }
#endif

    /*----------------------------------------------------------
     *  TESTING TRADD
     */

    LAPACKE_zlarnv_work(IONE, ISEED, LDAxN, A);
    LAPACKE_zlarnv_work(IONE, ISEED, LDBxN, B);

#ifdef COMPLEX
    for (t=0; t<3; t++) {
#else
    for (t=0; t<2; t++) {
#endif
        for (u=0; u<2; u++) {
            memcpy( Binit,  B, LDBxN*sizeof(PLASMA_Complex64_t));
            memcpy( Bfinal, B, LDBxN*sizeof(PLASMA_Complex64_t));

            /* PLASMA ZGEADD */
            PLASMA_ztradd(uplo[u], trans[t], M, N, alpha, A, LDA, beta, Bfinal, LDB);

            /* Check the solution */
            info_solution = check_tr_solution(uplo[u], trans[t], M, N,
                                              alpha, A, LDA,
                                              beta, Binit, Bfinal, LDB);

            if (info_solution == 0) {
                printf("***************************************************\n");
                printf(" ---- TESTING ZTRADD (%s, %s) ............... PASSED !\n", uplostr[u], transstr[t]);
                printf("***************************************************\n");
            }
            else {
                printf("************************************************\n");
                printf(" - TESTING ZTRADD (%s, %s) ... FAILED !\n", uplostr[u], transstr[t]);
                printf("************************************************\n");
            }
        }
    }
#ifdef _UNUSED_
    }
#endif

    free(A); free(B);
    free(Binit); free(Bfinal);

    return 0;
}

/*--------------------------------------------------------------
 * Check the solution
 */

static int check_tr_solution(PLASMA_enum uplo, PLASMA_enum trans, int M, int N,
                             PLASMA_Complex64_t alpha, PLASMA_Complex64_t *A, int LDA,
                             PLASMA_Complex64_t beta, PLASMA_Complex64_t *Bref, PLASMA_Complex64_t *Bplasma, int LDB)
{
    int info_solution;
    double Anorm, Binitnorm, Bplasmanorm, Rnorm, result;
    double eps;
    PLASMA_Complex64_t mzone;

    double *work = (double *)malloc(max(M, N)* sizeof(double));
    int Am, An;

    mzone = -1.0;

    if (trans == PlasmaNoTrans) {
        Am = M; An = N;
    } else {
        Am = N; An = M;
    }

    /* if ( ((trans == PlasmaNoTrans) && (uplo == PlasmaLower)) || */
    /*      ((trans != PlasmaNoTrans) && (uplo == PlasmaUpper)) ) */
    /* { */
    /*     Anorm = LAPACKE_zlantr_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), */
    /*                                 lapack_const(PlasmaLower), lapack_const(PlasmaNonUnit), */
    /*                                 Am, An, A, LDA, work); */
    /* } */
    /* else */
    /* { */
    /*     Anorm = LAPACKE_zlantr_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), */
    /*                                 lapack_const(PlasmaUpper), lapack_const(PlasmaNonUnit), */
    /*                                 Am, An, A, LDA, work); */
    /* } */

    /* Binitnorm   = LAPACKE_zlantr_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), */
    /*                                   lapack_const(uplo), lapack_const(PlasmaNonUnit), */
    /*                                   M, N, Bref,    LDB, work); */
    /* Bplasmanorm = LAPACKE_zlantr_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), */
    /*                                   lapack_const(uplo), lapack_const(PlasmaNonUnit), */
    /*                                   M, N, Bplasma, LDB, work); */

    Anorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm),
                                Am, An, A, LDA, work);
    Binitnorm   = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm),
                                      M, N, Bref,    LDB, work);
    Bplasmanorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm),
                                      M, N, Bplasma, LDB, work);

    CORE_ztradd(uplo, trans, M, N,
                alpha, A,    LDA,
                beta,  Bref, LDB);
    cblas_zaxpy( LDB*N, CBLAS_SADDR(mzone), Bplasma, 1, Bref, 1);

    Rnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaMaxNorm), M, N, Bref, LDB, work);

    eps = LAPACKE_dlamch_work('e');

    printf("Rnorm %e, Anorm %e, Bnorm %e, (alpha A + beta B) norm %e\n",
           Rnorm, Anorm, Binitnorm, Bplasmanorm);

    result = Rnorm / (max(Anorm, Binitnorm) * eps);
    printf("============\n");
    printf("Checking the norm of the difference against reference ZGEADD \n");
    printf("-- || R||_max/(max(||A||_oo,||B||_oo).eps) = %e \n",
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

/*--------------------------------------------------------------
 * Check the solution
 */

static int check_ge_solution(PLASMA_enum trans, int M, int N,
                             PLASMA_Complex64_t alpha, PLASMA_Complex64_t *A, int LDA,
                             PLASMA_Complex64_t beta, PLASMA_Complex64_t *Bref, PLASMA_Complex64_t *Bplasma, int LDB)
{
    int info_solution;
    double Anorm, Binitnorm, Bplasmanorm, Rnorm, result;
    double eps;
    PLASMA_Complex64_t mzone;

    double *work = (double *)malloc(max(M, N)* sizeof(double));
    int Am, An;

    mzone = -1.0;

    if (trans == PlasmaNoTrans) {
        Am = M; An = N;
    } else {
        Am = N; An = M;
    }

    Anorm       = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm),
                                      Am, An, A,       LDA, work);
    Binitnorm   = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm),
                                      M,  N,  Bref,    LDB, work);
    Bplasmanorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm),
                                      M,  N,  Bplasma, LDB, work);

    CORE_zgeadd(trans, M, N,
                alpha, A,    LDA,
                beta,  Bref, LDB);
    cblas_zaxpy( LDB*N, CBLAS_SADDR(mzone), Bplasma, 1, Bref, 1);

    Rnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaMaxNorm), M, N, Bref, LDB, work);

    eps = LAPACKE_dlamch_work('e');

    printf("Rnorm %e, Anorm %e, Bnorm %e, (alpha A + beta B) norm %e\n",
           Rnorm, Anorm, Binitnorm, Bplasmanorm);

    result = Rnorm / (max(Anorm, Binitnorm) * eps);
    printf("============\n");
    printf("Checking the norm of the difference against reference ZGEADD \n");
    printf("-- || R||_max/(max(||A||_oo,||B||_oo).eps) = %e \n",
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
