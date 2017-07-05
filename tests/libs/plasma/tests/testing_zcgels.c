/**
 *
 * @file testing_zcgels.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @brief Test mixed precision least squares problem with iterative refinement.
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

#ifndef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#endif
#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#endif

int check_orthogonality(int, int, int, PLASMA_Complex64_t*, double);
int check_factorization(int, int, PLASMA_Complex64_t*, PLASMA_Complex64_t*, int, PLASMA_Complex64_t*, double);
int check_solution(int, int, int, PLASMA_Complex64_t*, int, PLASMA_Complex64_t*, PLASMA_Complex64_t*, int, double);

int IONE=1;
int ISEED[4] = {0,0,0,1};   /* initial seed for zlarnv() */

int main (int argc, char **argv)
{
    /* Check for number of arguments*/
    if ( argc != 7){
        printf(" Proper Usage is : ./testing_zcgels ncores M N LDA NRHS LDB with \n - ncores : number of cores \n - M : number of rows of the matrix A \n - N : number of columns of the matrix A \n - LDA : leading dimension of the matrix A \n - NRHS : number of RHS \n - LDB : leading dimension of the matrix B\n");
        exit(1);
    }

    int cores = atoi(argv[1]);
    int M     = atoi(argv[2]);
    int N     = atoi(argv[3]);
    int LDA   = atoi(argv[4]);
    int NRHS  = atoi(argv[5]);
    int LDB   = atoi(argv[6]);
    int LDX   = LDB;
    int ITER;

    int K = min(M, N);
    double eps;
    int info, info_solution;
    /* int info_ortho, info_factorization; */
    int i,j;
    int LDAxN = LDA*N;
    int LDBxNRHS = LDB*NRHS;

    PLASMA_Complex64_t *A1 = (PLASMA_Complex64_t *)malloc(LDA*N*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *A2 = (PLASMA_Complex64_t *)malloc(LDA*N*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *B1 = (PLASMA_Complex64_t *)malloc(LDB*NRHS*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *B2 = (PLASMA_Complex64_t *)malloc(LDB*NRHS*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *X  = (PLASMA_Complex64_t *)malloc(LDX*NRHS*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Q  = (PLASMA_Complex64_t *)malloc(LDA*N*sizeof(PLASMA_Complex64_t));

    /* Check if unable to allocate memory */
    if ((!A1)||(!A2)||(!B1)||(!B2)||(!Q)||(!X)){
        printf("Out of Memory \n ");
        exit(0);
    }

    /* Plasma Initialization */
    PLASMA_Init(cores);

    /*
    PLASMA_Disable(PLASMA_AUTOTUNING);
    PLASMA_Set(PLASMA_TILE_SIZE, 6);
    PLASMA_Set(PLASMA_INNER_BLOCK_SIZE, 3);
    */

    eps = LAPACKE_dlamch_work('e');

    /*----------------------------------------------------------
    *  TESTING ZCGELS
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
    
    for (i = 0; i < K; i++)
        Q[LDA*i+i] = 1.0;
    
    printf("\n");
    printf("------ TESTS FOR PLASMA ZCGELS ROUTINE -------  \n");
    printf("            Size of the Matrix %d by %d\n", M, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 60.\n");

    /* PLASMA ZGELS */
    info = PLASMA_zcgels(PlasmaNoTrans, M, N, NRHS, A2, LDA, B2, LDB, X, LDX, &ITER);
    if (info != PLASMA_SUCCESS ) {
        printf("PLASMA_zcgels is not completed: info = %d\n", info);
        info_solution = 1;
    } else {
        printf(" Solution obtained with %d iterations\n", ITER);

        /* PLASMA ZGELS */
        //    if (M >= N)
        //   /* Building the economy-size Q */
        //   PLASMA_zungqr(M, N, K, A2, LDA, T, Q, LDA);
        //else
        //   /* Building the economy-size Q */
        //   PLASMA_zunglq(M, N, K, A2, LDA, T, Q, LDA);
        /* Check the orthogonality, factorization and the solution */
        //info_ortho = check_orthogonality(M, N, LDA, Q, eps);
        //info_factorization = check_factorization(M, N, A1, A2, LDA, Q, eps);
        info_solution = check_solution(M, N, NRHS, A1, LDA, B1, X, LDB, eps);
    }

    //if ((info_solution == 0)&(info_factorization == 0)&(info_ortho == 0)) {
    if (info_solution == 0) {
        printf("***************************************************\n");
        printf(" ---- TESTING ZCGELS ..................... PASSED !\n");
        printf("***************************************************\n");
    }
    else {
        printf("************************************************\n");
        printf(" - TESTING ZCGELS .. FAILED !\n");
        printf("************************************************\n");
    }


    free(A1); free(A2); free(B1); free(B2); free(X); free(Q);

    PLASMA_Finalize();

    exit(0);
}

/*-------------------------------------------------------------------
 * Check the orthogonality of Q
 */

int check_orthogonality(int M, int N, int LDQ, PLASMA_Complex64_t *Q, double eps)
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

    normQ = LAPACKE_zlansy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), 'u', minMN, Id, minMN, work);

    printf("============\n");
    printf("Checking the orthogonality of Q \n");
    printf("||Id-Q'*Q||_oo / (N*eps) = %e \n",normQ/(minMN*eps));

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

int check_factorization(int M, int N, PLASMA_Complex64_t *A1, PLASMA_Complex64_t *A2, int LDA, PLASMA_Complex64_t *Q, double eps )
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

    Rnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, N, Residual, M, work);
    Anorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, N, A2, LDA, work);

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

int check_solution(int M, int N, int NRHS, PLASMA_Complex64_t *A1, int LDA, PLASMA_Complex64_t *B1, PLASMA_Complex64_t *B2, int LDB, double eps)
{
    int info_solution;
    double Rnorm, Anorm, Xnorm, Bnorm;
    PLASMA_Complex64_t alpha, beta;
    double result;
    double *work = (double *)malloc(max(M, N)* sizeof(double));

    alpha = 1.0;
    beta  = -1.0;

    Anorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, N, A1, LDA, work);
    Xnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, NRHS, B2, LDB, work);
    Bnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, NRHS, B1, LDB, work);

    cblas_zgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M, NRHS, N, CBLAS_SADDR(alpha), A1, LDA, B2, LDB, CBLAS_SADDR(beta), B1, LDB);

    if (M >= N) {
       PLASMA_Complex64_t *Residual = (PLASMA_Complex64_t *)malloc(M*NRHS*sizeof(PLASMA_Complex64_t));
       memset((void*)Residual, 0, M*NRHS*sizeof(PLASMA_Complex64_t));
       cblas_zgemm(CblasColMajor, CblasConjTrans, CblasNoTrans, N, NRHS, M, CBLAS_SADDR(alpha), A1, LDA, B1, LDB, CBLAS_SADDR(beta), Residual, M);
       Rnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, NRHS, Residual, M, work);
       free(Residual);
    }
    else {
       PLASMA_Complex64_t *Residual = (PLASMA_Complex64_t *)malloc(N*NRHS*sizeof(PLASMA_Complex64_t));
       memset((void*)Residual, 0, N*NRHS*sizeof(PLASMA_Complex64_t));
       cblas_zgemm(CblasColMajor, CblasConjTrans, CblasNoTrans, N, NRHS, M, CBLAS_SADDR(alpha), A1, LDA, B1, LDB, CBLAS_SADDR(beta), Residual, N);
       Rnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, NRHS, Residual, N, work);
       free(Residual);
    }

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
