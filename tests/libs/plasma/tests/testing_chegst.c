/**
 *
 * @file testing_chegst.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Hatem Ltaief
 * @date 2010-11-15
 * @generated c Fri Apr  1 11:03:05 2016
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

#undef REAL
#define COMPLEX

static int check_transformation(int, int, int, PLASMA_Complex32_t*, PLASMA_Complex32_t*, int, PLASMA_Complex32_t*, int, float);
static int check_factorization(int, PLASMA_Complex32_t*, PLASMA_Complex32_t*, int, int , float);

int testing_chegst(int argc, char **argv)
{
    /* Check for number of arguments*/
    if (argc != 3) {
        USAGE("HEGST", "N LDA LDB",
              "   - N    : size of the matrices A and B\n"
              "   - LDA  : leading dimension of the matrix A\n"
              "   - LDB  : leading dimension of the matrix B\n");
        return -1;
    }

    float eps = LAPACKE_slamch_work('e');
    int    N     = atoi(argv[0]);
    int    LDA   = atoi(argv[1]);
    int    LDB   = atoi(argv[2]);
    int    info_transformation, info_factorization;
    int    i, u;
    int    LDAxN = LDA*N;
    int    LDBxN = LDB*N;

    PLASMA_Complex32_t *A1    = (PLASMA_Complex32_t *)malloc(LDAxN*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *A2    = (PLASMA_Complex32_t *)malloc(LDAxN*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *B1    = (PLASMA_Complex32_t *)malloc(LDBxN*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *B2    = (PLASMA_Complex32_t *)malloc(LDBxN*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *Ainit = (PLASMA_Complex32_t *)malloc(LDAxN*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *Binit = (PLASMA_Complex32_t *)malloc(LDBxN*sizeof(PLASMA_Complex32_t));

    /* Check if unable to allocate memory */
    if ((!A1)||(!A2)||(!B1)||(!B2)||(!Ainit)||(!Binit)){
        printf("Out of Memory \n ");
        return -2;
    }

    /*----------------------------------------------------------
    *  TESTING CHEGST
    */

    /* Initialize A1 and A2 */
    PLASMA_cplghe(0., N, A1, LDA, 5198);
    LAPACKE_clacpy_work(LAPACK_COL_MAJOR, 'A', N, N, A1, LDA, Ainit, LDA);

    /* Initialize B1 and B2 */
    PLASMA_cplghe((float)N, N, B1, LDB, 4231);
    LAPACKE_clacpy_work(LAPACK_COL_MAJOR, 'A', N, N, B1, LDB, Binit, LDB);

    printf("\n");
    printf("------ TESTS FOR PLASMA CHEGST ROUTINE -------  \n");
    printf("        Size of the Matrix %d by %d\n", N, N);
    printf("\n");
    printf(" The matrices A and B are randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 60.\n");

    /*----------------------------------------------------------
     *  TESTING CHEGST
     */

    for (i=0; i<3; i++) {
        for (u=0; u<2; u++) {
            memcpy(A2, Ainit, LDAxN*sizeof(PLASMA_Complex32_t));
            memcpy(B2, Binit, LDBxN*sizeof(PLASMA_Complex32_t));

            PLASMA_cpotrf(uplo[u], N, B2, LDB);
            PLASMA_chegst(itype[i], uplo[u], N, A2, LDA, B2, LDB);
        
            /* Check the Cholesky factorization and the transformation */
            info_factorization = check_factorization(N, B1, B2, LDB, uplo[u], eps);
            info_transformation = check_transformation(itype[i], uplo[u], N, A1, A2, LDA, B2, LDB, eps);
        
            if ( (info_transformation == 0) && (info_factorization == 0) ) {
                printf("***************************************************\n");
                printf(" ---- TESTING CHEGST (%s, %s) ....... PASSED !\n", itypestr[i], uplostr[u]);
                printf("***************************************************\n");
            }
            else {
                printf("************************************************\n");
                printf(" - TESTING CHEGST (%s, %s) ... FAILED !\n", itypestr[i], uplostr[u]);
                printf("************************************************\n");
            }
        }
    }
        
    free(A1); 
    free(A2); 
    free(B1); 
    free(B2);
    free(Ainit); 
    free(Binit);

    return 0;
}
/*------------------------------------------------------------------------
 *  Check the factorization of the matrix A2
 */
static int check_factorization(int N, PLASMA_Complex32_t *A1, PLASMA_Complex32_t *A2, int LDA, int uplo, float eps)
{
    PLASMA_Complex32_t alpha = 1.0;
    float Anorm, Rnorm, result;
    int info_factorization;
    int i,j;

    PLASMA_Complex32_t *Residual = (PLASMA_Complex32_t *)malloc(N*N*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *L1       = (PLASMA_Complex32_t *)malloc(N*N*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *L2       = (PLASMA_Complex32_t *)malloc(N*N*sizeof(PLASMA_Complex32_t));
    float *work                 = (float *)malloc(N*sizeof(float));

    memset((void*)L1, 0, N*N*sizeof(PLASMA_Complex32_t));
    memset((void*)L2, 0, N*N*sizeof(PLASMA_Complex32_t));

    LAPACKE_clacpy_work(LAPACK_COL_MAJOR,' ', N, N, A1, LDA, Residual, N);

    /* Dealing with L'L or U'U  */
    LAPACKE_clacpy_work(LAPACK_COL_MAJOR, lapack_const(uplo), N, N, A2, LDA, L1, N);
    LAPACKE_clacpy_work(LAPACK_COL_MAJOR, lapack_const(uplo), N, N, A2, LDA, L2, N);
    if (uplo == PlasmaUpper)
       cblas_ctrmm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, CblasConjTrans, CblasNonUnit, N, N, CBLAS_SADDR(alpha), L1, N, L2, N);
    else
       cblas_ctrmm(CblasColMajor, CblasRight, (CBLAS_UPLO)uplo, CblasConjTrans, CblasNonUnit, N, N, CBLAS_SADDR(alpha), L1, N, L2, N);

    /* Compute the Residual || A - L'L|| */
    for (i = 0; i < N; i++)
        for (j = 0; j < N; j++)
           Residual[j*N+i] = L2[j*N+i] - Residual[j*N+i];

    Rnorm = LAPACKE_clange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Residual, N,   work);
    Anorm = LAPACKE_clange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, A1,       LDA, work);

    result = Rnorm / ( Anorm * N * eps );
    printf("============\n");
    printf("Checking the Cholesky Factorization \n");
    printf("-- ||L'L-A||_oo/(||A||_oo.N.eps) = %e \n", result);

    if ( isnan(result) || isinf(result) || (result > 60.0) ){
        printf("-- Factorization is suspicious ! \n");
        info_factorization = 1;
    }
    else{
        printf("-- Factorization is CORRECT ! \n");
        info_factorization = 0;
    }

    free(Residual); free(L1); free(L2); free(work);

    return info_factorization;
}

/*------------------------------------------------------------
 *  Check the Transformation to standard eigenvalue problem
 */

static int check_transformation(int itype, int uplo, int N, PLASMA_Complex32_t *A1, PLASMA_Complex32_t *A2, int LDA, PLASMA_Complex32_t *B2, int LDB, float eps)
{
    PLASMA_Complex32_t alpha = 1.0;
    float Anorm, Rnorm, result;
    int info_transformation;
    int i, j;
    char *str;

    PLASMA_Complex32_t *Residual = (PLASMA_Complex32_t *)malloc(N*N*sizeof(PLASMA_Complex32_t));
    PLASMA_Complex32_t *Aorig    = (PLASMA_Complex32_t *)malloc(N*N*sizeof(PLASMA_Complex32_t));
    float *work                 = (float *)malloc(N*sizeof(float));

    LAPACKE_clacpy_work(LAPACK_COL_MAJOR, 'a',                N, N, A1, LDA, Residual, N);
    LAPACKE_clacpy_work(LAPACK_COL_MAJOR, lapack_const(uplo), N, N, A2, LDA, Aorig,    N);
    
    /* Rebuild the symmetry of A2 */
    if (uplo == PlasmaLower) {
        for (j = 0; j < N; j++)
            for (i = j+1; i < N; i++)
                Aorig[j+i*N] = conjf(Aorig[i+j*N]); 
    } else {
        for (i = 0; i < N; i++)
            for (j = i+1; j < N; j++)
                Aorig[j+i*N] = conjf(Aorig[i+j*N]); 
    }

    if (itype == 1) {
        if (uplo == PlasmaLower) {
            str = "L*A2*L'";
            cblas_ctrmm(CblasColMajor, CblasLeft,  CblasLower, CblasNoTrans,   CblasNonUnit, N, N, CBLAS_SADDR(alpha), B2, LDB, Aorig, N);   
            cblas_ctrmm(CblasColMajor, CblasRight, CblasLower, CblasConjTrans, CblasNonUnit, N, N, CBLAS_SADDR(alpha), B2, LDB, Aorig, N);   
        }
        else{
            str = "U'*A2*U";
            cblas_ctrmm(CblasColMajor, CblasLeft,  CblasUpper, CblasConjTrans, CblasNonUnit, N, N, CBLAS_SADDR(alpha), B2, LDB, Aorig, N);   
            cblas_ctrmm(CblasColMajor, CblasRight, CblasUpper, CblasNoTrans,   CblasNonUnit, N, N, CBLAS_SADDR(alpha), B2, LDB, Aorig, N);   
        }
    }
    else {
        if (uplo == PlasmaLower) {
            str = "inv(L')*A2*inv(L)";
            cblas_ctrsm(CblasColMajor, CblasLeft,  CblasLower, CblasConjTrans, CblasNonUnit, N, N, CBLAS_SADDR(alpha), B2, LDB, Aorig, N);   
            cblas_ctrsm(CblasColMajor, CblasRight, CblasLower, CblasNoTrans,   CblasNonUnit, N, N, CBLAS_SADDR(alpha), B2, LDB, Aorig, N);   
        }
        else{
            str = "inv(U)*A2*inv(U')";
            cblas_ctrsm(CblasColMajor, CblasLeft,  CblasUpper, CblasNoTrans,   CblasNonUnit, N, N, CBLAS_SADDR(alpha), B2, LDB, Aorig, N);   
            cblas_ctrsm(CblasColMajor, CblasRight, CblasUpper, CblasConjTrans, CblasNonUnit, N, N, CBLAS_SADDR(alpha), B2, LDB, Aorig, N);   
            
        }
    }
    
    /* Compute the Residual ( A1 - W ) */
    for (i = 0; i < N; i++)
        for (j = 0; j < N; j++)
            Residual[j*N+i] = Aorig[j*N+i] - Residual[j*N+i];
    
    Rnorm = LAPACKE_clange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, Residual, N,   work);
    Anorm = LAPACKE_clange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, A1,       LDA, work);
    
    result = Rnorm / (Anorm * N *eps);
    printf("============\n");
    printf("Checking the global transformation \n");
    printf("-- ||A1-%s||_oo/(||A1||_oo.N.eps) = %e \n", str, result );

    if (isnan(result) || isinf(result) || (result > 60.0) ) {
        printf("-- Transformation is suspicious ! \n");
        info_transformation = 1;
    }
    else {
        printf("-- Transformation is CORRECT ! \n");
        info_transformation = 0;
    }

    free(Residual); free(Aorig); free(work);

    return info_transformation;
}
