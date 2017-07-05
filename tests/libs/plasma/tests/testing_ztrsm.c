/**
 *
 * @file testing_ztrsm.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
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

static int check_solution(PLASMA_enum side, PLASMA_enum uplo, PLASMA_enum trans, PLASMA_enum diag,
                          int M, int N, PLASMA_Complex64_t alpha,
                          PLASMA_Complex64_t *A, int LDA,
                          PLASMA_Complex64_t *Bref, PLASMA_Complex64_t *Bplasma, int LDB);

int testing_ztrsm(int argc, char **argv)
{
    /* Check for number of arguments*/
    if ( argc != 5 ) {
        USAGE("TRSM", "alpha M N LDA LDB",
              "   - alpha  : alpha coefficient\n"
              "   - M      : number of rows of matrices B\n"
              "   - N      : number of columns of matrices B\n"
              "   - LDA    : leading dimension of matrix A\n"
              "   - LDB    : leading dimension of matrix B\n");
        return -1;
    }

    PLASMA_Complex64_t alpha = (PLASMA_Complex64_t) atol(argv[0]);
    int M     = atoi(argv[1]);
    int N     = atoi(argv[2]);
    int LDA   = atoi(argv[3]);
    int LDB   = atoi(argv[4]);

    double eps;
    int info_solution;
    int s, u, t, d, i;
    int LDAxM = LDA*max(M,N);
    int LDBxN = LDB*max(M,N);

    PLASMA_Complex64_t *A      = (PLASMA_Complex64_t *)malloc(LDAxM*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *B      = (PLASMA_Complex64_t *)malloc(LDBxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Binit  = (PLASMA_Complex64_t *)malloc(LDBxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Bfinal = (PLASMA_Complex64_t *)malloc(LDBxN*sizeof(PLASMA_Complex64_t));

    /* Check if unable to allocate memory */
    if ( (!A) || (!B) || (!Binit) || (!Bfinal)){
        printf("Out of Memory \n ");
        return -2;
    }

    eps = LAPACKE_dlamch_work('e');

    printf("\n");
    printf("------ TESTS FOR PLASMA ZTRSM ROUTINE -------  \n");
    printf("            Size of the Matrix B : %d by %d\n", M, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 10.\n");

    /*----------------------------------------------------------
     *  TESTING ZTRSM
     */

    /* Initialize A, B, C */
    LAPACKE_zlarnv_work(IONE, ISEED, LDAxM, A);
    LAPACKE_zlarnv_work(IONE, ISEED, LDBxN, B);
    for(i=0;i<max(M,N);i++)
      A[LDA*i+i] = A[LDA*i+i] + 2.0;

    for (s=0; s<2; s++) {
        for (u=0; u<2; u++) {
#ifdef COMPLEX
            for (t=0; t<3; t++) {
#else
            for (t=0; t<2; t++) {
#endif
                for (d=0; d<2; d++) {

                    memcpy(Binit,  B, LDBxN*sizeof(PLASMA_Complex64_t));
                    memcpy(Bfinal, B, LDBxN*sizeof(PLASMA_Complex64_t));

                    /* PLASMA ZTRSM */
                    PLASMA_ztrsm(side[s], uplo[u], trans[t], diag[d],
                                 M, N, alpha, A, LDA, Bfinal, LDB);

                    /* Check the solution */
                    info_solution = check_solution(side[s], uplo[u], trans[t], diag[d],
                                                   M, N, alpha, A, LDA, Binit, Bfinal, LDB);

                    printf("***************************************************\n");
                    if (info_solution == 0) {
                        printf(" ---- TESTING ZTRSM (%s, %s, %s, %s) ...... PASSED !\n",
                               sidestr[s], uplostr[u], transstr[t], diagstr[d]);
                    }
                    else {
                        printf(" ---- TESTING ZTRSM (%s, %s, %s, %s) ... FAILED !\n",
                               sidestr[s], uplostr[u], transstr[t], diagstr[d]);
                    }
                    printf("***************************************************\n");
                }
            }
        }
    }

    free(A); free(B);
    free(Binit); free(Bfinal);

    return 0;
}

/*--------------------------------------------------------------
 * Check the solution
 */
static int check_solution(PLASMA_enum side, PLASMA_enum uplo, PLASMA_enum trans, PLASMA_enum diag,
                          int M, int N, PLASMA_Complex64_t alpha,
                          PLASMA_Complex64_t *A, int LDA,
                          PLASMA_Complex64_t *Bref, PLASMA_Complex64_t *Bplasma, int LDB)
{
    int info_solution;
    double Anorm, Binitnorm, Bplasmanorm, Blapacknorm, Rnorm, result;
    double eps;
    PLASMA_Complex64_t mzone = (PLASMA_Complex64_t)-1.0;

    double *work = (double *)malloc(max(M, N)* sizeof(double));
    int Am, An;

    if (side == PlasmaLeft) {
        Am = M; An = M;
    } else {
        Am = N; An = N;
    }

    Anorm       = LAPACKE_zlantr_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), lapack_const(uplo), lapack_const(diag),
                                Am, An, A, LDA, work);
    Binitnorm   = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, N, Bref,    LDB, work);
    Bplasmanorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, N, Bplasma, LDB, work);

    cblas_ztrsm(CblasColMajor, (CBLAS_SIDE)side, (CBLAS_UPLO)uplo, (CBLAS_TRANSPOSE)trans,
                (CBLAS_DIAG)diag, M, N, CBLAS_SADDR(alpha), A, LDA, Bref, LDB);

    Blapacknorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, N, Bref, LDB, work);

    cblas_zaxpy(LDB * N, CBLAS_SADDR(mzone), Bplasma, 1, Bref, 1);

    Rnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, N, Bref, LDB, work);

    eps = LAPACKE_dlamch_work('e');

    printf("Rnorm %e, Anorm %e, Binitnorm %e, Bplasmanorm %e, Blapacknorm %e\n",
           Rnorm, Anorm, Binitnorm, Bplasmanorm, Blapacknorm);

    result = Rnorm / ((Anorm + Blapacknorm) * max(M,N) * eps);

    printf("============\n");
    printf("Checking the norm of the difference against reference ZTRSM \n");
    printf("-- ||Cplasma - Clapack||_oo/((||A||_oo+||B||_oo).N.eps) = %e \n", result);

    if ( isinf(Blapacknorm) || isinf(Bplasmanorm) || isnan(result) || isinf(result) || (result > 10.0) ) {
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
