/**
 *
 * @file testing_zcungesv.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Emmanuel Agullo
 * @date 2010-11-15
 * @precisions mixed zc -> ds
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

static int check_solution(int, int, PLASMA_Complex64_t*, int, PLASMA_Complex64_t*, PLASMA_Complex64_t*, int, double);

int testing_zcungesv(int argc, char **argv)
{
    /* Check for number of arguments*/
    if (argc != 4){
        USAGE("CUNGESV", "N LDA NRHS LDB",
              "   - N    : the size of the matrix\n"
              "   - LDA  : leading dimension of the matrix A\n"
              "   - NRHS : number of RHS\n"
              "   - LDB  : leading dimension of the RHS B\n");
        return -1;
    }

    int N     = atoi(argv[0]);
    int LDA   = atoi(argv[1]);
    int NRHS  = atoi(argv[2]);
    int LDB   = atoi(argv[3]);
    int ITER;

    double eps;
    int info, info_solution;
    int i,j;
    int LDAxN    = LDA*N;
    int LDBxNRHS = LDB*NRHS;

    PLASMA_Complex64_t *A1 = (PLASMA_Complex64_t *)malloc(LDA*N   *sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *A2 = (PLASMA_Complex64_t *)malloc(LDA*N   *sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *B1 = (PLASMA_Complex64_t *)malloc(LDB*NRHS*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *B2 = (PLASMA_Complex64_t *)malloc(LDB*NRHS*sizeof(PLASMA_Complex64_t));

    /* Check if unable to allocate memory */
    if ( (!A1) || (!A2) || (!B1) || (!B2) ) {
        printf("Out of Memory \n ");
        exit(0);
    }

    eps = LAPACKE_dlamch_work('e');

    /*----------------------------------------------------------
    *  TESTING ZCGELS
    */

    /* Initialize A1 and A2 */
    LAPACKE_zlarnv_work(IONE, ISEED, LDAxN, A1);
    for (i = 0; i < N; i++)
        for (j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i] ;

    /* Initialize B1 and B2 */
    LAPACKE_zlarnv_work(IONE, ISEED, LDBxNRHS, B1);
    for (i = 0; i < N; i++)
        for (j = 0; j < NRHS; j++)
             B2[LDB*j+i] = B1[LDB*j+i] ;

    printf("\n");
    printf("------ TESTS FOR PLASMA ZCUNGESV ROUTINE -------  \n");
    printf("            Size of the Matrix %d by %d\n", N, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 60.\n");

    /* PLASMA ZCUNGESV */
    info = PLASMA_zcungesv(PlasmaNoTrans, N, NRHS, A2, LDA, B1, LDB, B2, LDB, &ITER);
    if (info != PLASMA_SUCCESS ) {
        printf("PLASMA_zcposv is not completed: info = %d\n", info);
        info_solution = 1;
    } else {
        printf(" Solution obtained with %d iterations\n", ITER);
   
        /* Check the orthogonality, factorization and the solution */
        info_solution = check_solution(N, NRHS, A1, LDA, B1, B2, LDB, eps);
    }

    if (info_solution == 0) {
        printf("***************************************************\n");
        printf(" ---- TESTING ZCUNGESV.................... PASSED !\n");
        printf("***************************************************\n");
    }
    else {
        printf("************************************************\n");
        printf(" - TESTING ZCUNGESV .. FAILED !\n");
        printf("************************************************\n");
    }

    free(A1); free(A2); free(B1);  free(B2);
    
    return 0;
}

/*--------------------------------------------------------------
 * Check the solution
 */

static int check_solution(int N, int NRHS, PLASMA_Complex64_t *A1, int LDA, PLASMA_Complex64_t *B1, PLASMA_Complex64_t *B2, int LDB, double eps )
{
    int info_solution;
    double Rnorm, Anorm, Xnorm, Bnorm, result;
    PLASMA_Complex64_t alpha, beta;
    double *work = (double *)malloc(N*sizeof(double));

    alpha = 1.0;
    beta  = -1.0;

    Anorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N,    A1, LDA, work);
    Xnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, NRHS, B2, LDB, work);
    Bnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, NRHS, B1, LDB, work);

    cblas_zgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, N, NRHS, N, CBLAS_SADDR(alpha), A1, LDA, B2, LDB, CBLAS_SADDR(beta), B1, LDB);
    Rnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, NRHS, B1, LDB, work);

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
