/**
 *
 * @file testing_cgetri.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Hatem Ltaief
 * @date 2010-11-15
 * @generated c Fri Apr  1 11:03:04 2016
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

static int check_factorization(int, PLASMA_Complex32_t*, PLASMA_Complex32_t*, int, int*, float);
static int check_inverse(int, PLASMA_Complex32_t *, PLASMA_Complex32_t *, int, int*, float);

int testing_cgetri(int argc, char **argv)
{

    /* Check for number of arguments*/
    if (argc != 2){
        USAGE("GETRI", "N LDA",
              "   - N    : the size of the matrix\n"
              "   - LDA  : leading dimension of the matrix A\n");
        return -1;
    }

    int N     = atoi(argv[0]);
    int LDA   = atoi(argv[1]);
    float eps;
    int info_inverse, info_factorization;
    int i, j;

    PLASMA_Complex32_t *A1   = (PLASMA_Complex32_t *)malloc(LDA*N*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *A2   = (PLASMA_Complex32_t *)malloc(LDA*N*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *WORK = (PLASMA_Complex32_t *)malloc(2*LDA*sizeof(PLASMA_Complex32_t));
    float *D                = (float *)malloc(LDA*sizeof(float));
    int *IPIV = (int *)malloc(N*sizeof(int));

    /* Check if unable to allocate memory */
    if ( (!A1) || (!A2) || (!IPIV) ){
        printf("Out of Memory \n ");
        return -2;
    }

    eps = LAPACKE_slamch_work('e');

    /*-------------------------------------------------------------
    *  TESTING CGETRI
    */

    /* Initialize A1 and A2 Matrix */
    PLASMA_cplrnt(N, N, A1, LDA, 3453);
    for ( i = 0; i < N; i++)
        for (  j = 0; j < N; j++)
            A2[LDA*j+i] = A1[LDA*j+i];

    printf("\n");
    printf("------ TESTS FOR PLASMA CGETRI ROUTINE -------  \n");
    printf("            Size of the Matrix %d by %d\n", N, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n", eps);
    printf(" Computational tests pass if scaled residuals are less than 60.\n");

    /* PLASMA CGETRF */
    PLASMA_cgetrf(N, N, A2, LDA, IPIV);

    /* Check the factorization */
    info_factorization = check_factorization( N, A1, A2, LDA, IPIV, eps);

    /* PLASMA CGETRI */
    PLASMA_cgetri(N, A2, LDA, IPIV);

    /* Check the inverse */
    info_inverse = check_inverse(N, A1, A2, LDA, IPIV, eps);

    if ( (info_inverse == 0) && (info_factorization == 0) ) {
        printf("***************************************************\n");
        printf(" ---- TESTING CGETRI ..................... PASSED !\n");
        printf("***************************************************\n");
    }
    else {
        printf("***************************************************\n");
        printf(" - TESTING CGETRI ... FAILED !\n");
        printf("***************************************************\n");
    }

    free(A1); free(A2); free(IPIV); free(WORK); free(D);

    return 0;
}


/*------------------------------------------------------------------------
 *  Check the factorization of the matrix A2
 */
static int check_factorization(int N, PLASMA_Complex32_t *A1, PLASMA_Complex32_t *A2, int LDA, int *IPIV, float eps)
{
    int info_factorization;
    float Rnorm, Anorm, Xnorm, Bnorm, result;
    PLASMA_Complex32_t alpha, beta;

    alpha = 1.0;
    beta  = -1.0;

    PLASMA_Complex32_t *b  = (PLASMA_Complex32_t *)malloc(LDA*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *x  = (PLASMA_Complex32_t *)malloc(LDA*sizeof(PLASMA_Complex32_t));

    LAPACKE_clarnv_work(1, ISEED, LDA, x);
    LAPACKE_clacpy_work(LAPACK_COL_MAJOR, 'A', N, 1, x, LDA, b, LDA);

    PLASMA_cgetrs( PlasmaNoTrans, N, 1, A2, LDA, IPIV, x, LDA );

    Xnorm = PLASMA_clange(PlasmaInfNorm, N, 1, x,  LDA);
    Anorm = PLASMA_clange(PlasmaInfNorm, N, N, A1, LDA);
    Bnorm = PLASMA_clange(PlasmaInfNorm, N, 1, b,  LDA);

    PLASMA_cgemm( PlasmaNoTrans, PlasmaNoTrans, N, 1, N, 
                  alpha, A1, LDA, x, LDA, beta, b, LDA);
    
    Rnorm = PLASMA_clange(PlasmaInfNorm, N, 1, b, LDA);

    if (getenv("PLASMA_TESTING_VERBOSE"))
      printf( "||A||_oo=%f\n||X||_oo=%f\n||B||_oo=%f\n||A X - B||_oo=%e\n", Anorm, Xnorm, Bnorm, Rnorm );

    result = Rnorm / ( (Anorm*Xnorm+Bnorm)*N*eps ) ;
    printf("============\n");
    printf("Checking the Residual of the solution \n");
    printf("-- ||Ax-B||_oo/((||A||_oo||x||_oo+||B||_oo).N.eps) = %e \n", result);

    if (  isnan(Xnorm) || isinf(Xnorm) || isnan(result) || isinf(result) || (result > 60.0) ) {
        printf("-- The factorization is suspicious ! \n");
        info_factorization = 1;
     }
    else{
        printf("-- The factorization is CORRECT ! \n");
        info_factorization = 0;
    }
    free(x); free(b);

    return info_factorization;
}


/*------------------------------------------------------------------------
 *  Check the accuracy of the computed inverse
 */

static int check_inverse(int N, PLASMA_Complex32_t *A1, PLASMA_Complex32_t *A2, int LDA, int *IPIV, float eps )
{
    int info_inverse;
    int i;
    float Rnorm, Anorm, Ainvnorm, result;
    PLASMA_Complex32_t alpha, beta, zone;
    PLASMA_Complex32_t *work = (PLASMA_Complex32_t *)malloc(N*N*sizeof(PLASMA_Complex32_t));

    alpha = -1.0;
    beta  = 0.0;
    zone = 1.0;

    PLASMA_cgemm( PlasmaNoTrans, PlasmaNoTrans, N, N, N, alpha, A2, LDA, A1, LDA, beta, work, N);

    /* Add the identity matrix to work */
    for(i=0; i<N; i++)
        *(work+i+i*N) = *(work+i+i*N) + zone;

    Rnorm    = PLASMA_clange(PlasmaInfNorm, N, N, work, N);
    Anorm    = PLASMA_clange(PlasmaInfNorm, N, N, A1,   LDA);
    Ainvnorm = PLASMA_clange(PlasmaInfNorm, N, N, A2,   LDA);

    if (getenv("PLASMA_TESTING_VERBOSE"))
      printf( "||A||_1=%f\n||Ainv||_1=%f\n||Id - A*Ainv||_1=%e\n", Anorm, Ainvnorm, Rnorm );

    result = Rnorm / ( (Anorm*Ainvnorm)*N*eps ) ;
    printf("============\n");
    printf("Checking the Residual of the inverse \n");
    printf("-- ||Id - A*Ainv||_1/((||A||_1||Ainv||_1).N.eps) = %e \n", result);

    if (  isnan(Ainvnorm) || isinf(Ainvnorm) || isnan(result) || isinf(result) || (result > 60.0) ) {
        printf("-- The inverse is suspicious ! \n");
        info_inverse = 1;
     }
    else{
        printf("-- The inverse is CORRECT ! \n");
        info_inverse = 0;
    }

    free(work);

    return info_inverse;
}
