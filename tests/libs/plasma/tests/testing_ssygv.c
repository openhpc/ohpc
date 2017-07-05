/**
 *
 * @file testing_ssygv.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Azzam Haidar
 * @author Hatem Ltaief
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

static int check_orthogonality(int itype, int uplo, int N,
                               float *Z, int LDZ,
                               float *B, float *CHOLB, int LDB,
                               float eps);
static int check_reduction(int itype, int uplo, int N, float *D,
                           float *A, int LDA,
                           float *B, int LDB,
                           float *Z, int LDZ,
                           float eps );
static int check_solution(int N, float *E1, float *E2, float eps);

int testing_ssygv(int argc, char **argv)
{
    /* Check for number of arguments*/
    if (argc != 3) {
        USAGE("HEGV", "N LDA LDB",
              "   - N    : size of the matrices A and B\n"
              "   - LDA  : leading dimension of the matrix A\n"
              "   - LDB  : leading dimension of the matrix B\n");
        return -1;
    }

    float      eps = LAPACKE_slamch_work('e');
    PLASMA_enum vec = PlasmaVec;
    int    N        = atoi(argv[0]);
    int    LDA      = atoi(argv[1]);
    int    LDB      = atoi(argv[2]);
    int    LDQ      = LDA;
    int    LDAxN    = LDA*N;
    int    LDBxN    = LDB*N;
    int    LDQxN    = LDQ*N;

    int info_ortho     = 0;
    int info_solution  = 0;
    int info_reduction = 0;
    int i, u;

    float *A1    = (float *)malloc(LDAxN*sizeof(float));
    float *A2    = (float *)malloc(LDAxN*sizeof(float));
    float *B1    = (float *)malloc(LDBxN*sizeof(float));
    float *B2    = (float *)malloc(LDBxN*sizeof(float));
    float *Q     = (float *)malloc(LDQxN*sizeof(float));
    float *Ainit = (float *)malloc(LDAxN*sizeof(float));
    float *Binit = (float *)malloc(LDBxN*sizeof(float));
    float *W1 = (float *)malloc(N*sizeof(float));
    float *W2 = (float *)malloc(N*sizeof(float));
    float *work = (float *)malloc(3*N* sizeof(float));
    PLASMA_desc *T;

    /* Check if unable to allocate memory */
    if ((!A1)||(!A2)||(!B1)||(!B2)||(!Q)||(!Ainit)||(!Binit)){
        printf("Out of Memory \n ");
        return -2;
    }

    PLASMA_Disable(PLASMA_AUTOTUNING);
    PLASMA_Set(PLASMA_TILE_SIZE, 120);
    PLASMA_Set(PLASMA_INNER_BLOCK_SIZE, 20);
    
    PLASMA_Enable(PLASMA_WARNINGS);
    PLASMA_Enable(PLASMA_ERRORS);
    PLASMA_Alloc_Workspace_ssygv(N, N, &T);

    /*----------------------------------------------------------
    *  TESTING SSYGV
    */
    /* Initialize A1 and Ainit */
    PLASMA_splgsy(0., N, A1, LDA, 5198);
    PLASMA_slacpy(PlasmaUpperLower, N, N, A1, LDA, Ainit, LDA);

    /* Initialize B1 and Binit */
    PLASMA_splgsy((float)N, N, B1, LDB, 4321 );
    PLASMA_slacpy(PlasmaUpperLower, N, N, B1, LDB, Binit, LDB);

    printf("\n");
    printf("------ TESTS FOR PLASMA SSYGV ROUTINE -------  \n");
    printf("        Size of the Matrix %d by %d\n", N, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 60.\n");

    /*----------------------------------------------------------
     *  TESTING SSYGV
     */

    for (i=0; i<3; i++) {
        for (u=0; u<2; u++) {
            memcpy(A2, Ainit, LDAxN*sizeof(float));
            memcpy(B2, Binit, LDBxN*sizeof(float));
            /* CALL SSYGV with itype= 1, 2, 3 and uplo= L,U */ 
            PLASMA_ssygv(itype[i], vec, uplo[u], N, A2, LDA, B2, LDB, W2, T, Q, LDQ);

            /* CALL LAPACK to compute eigenvalues. */
            memcpy(A1, Ainit, LDAxN*sizeof(float));
            memcpy(B1, Binit, LDBxN*sizeof(float));
            LAPACKE_ssygv( LAPACK_COL_MAJOR, 
                     itype[i], lapack_const(vec), lapack_const(uplo[u]),
                     N, A1, LDA, B1, LDB, W1);

            /* CHECK */
            info_solution = check_solution(N, W1, W2, eps);
            /* Check the orthogonality, reduction and the eigen solutions */
            if (vec == PlasmaVec){
                info_ortho = check_orthogonality(itype[i], uplo[u], N, Q, LDQ, Binit, B1, LDB, eps);
                info_reduction = check_reduction(itype[i], uplo[u], N, W2, Ainit, LDA, Binit, LDB, Q, LDQ, eps );
            }

            if ( (info_ortho == 0) & (info_reduction == 0) & (info_solution == 0)) {
                printf("***************************************************\n");
                printf(" ---- TESTING SSYGV (%s, %s) ...................... PASSED !\n", itypestr[i], uplostr[u]);
                printf("***************************************************\n");
            }
            else {
                printf("************************************************\n");
                printf(" - TESTING SSYGV (%s, %s) ... FAILED !\n", itypestr[i], uplostr[u]);
                printf("************************************************\n");
            } 
        }
    }

    PLASMA_Dealloc_Handle_Tile(&T);
    free(A1); 
    free(A2); 
    free(B1); 
    free(B2); 
    free(Q); 
    free(Ainit); 
    free(Binit); 
    free(W1);
    free(W2);
    free(work);

    return 0;
}

/*-------------------------------------------------------------------
 * Check the orthogonality of Z
 */
static int check_orthogonality(int itype, int uplo, int N,
                               float *Z, int LDZ,
                               float *B, float *CHOLB, int LDB,
                               float eps)
{
    static float zone  =  1.0;
    static float mzone = -1.0;
    static float zzero =  0.0;
    static float             done  =  1.0;
    static float             mdone = -1.0;
    PLASMA_enum trans;
    float  normQ, result;
    int     info_ortho;
    float *work = (float *)malloc(N*sizeof(float));
    float *TEMP    = (float *)malloc(N*N*sizeof(float));
    float *Id = (float *) malloc(N*N*sizeof(float));
    char *str;

    /* Build the idendity matrix */
    LAPACKE_slaset_work(LAPACK_COL_MAJOR, 'A', N, N, 0., 1., Id, N);

    /* Perform orth test*/
    if ( itype == 3 ) {
        /*
         * if ITYPE = 3, Z**T*inv(B)*Z = I.
         * inv(B) = inv(L)**T * inv(L) or inv(U)*inv(U)**T
         */ 
        str = "Z**T*inv(B)*Z";

        if( uplo==PlasmaUpper ) {
            trans = PlasmaTrans;
        } else {
            trans = PlasmaNoTrans;
        }

        /* Compute inv(L)*Z or inv(U)**T*Z */ 
        LAPACKE_slacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower),
                            N, N, Z, LDZ, TEMP, N);

        cblas_strsm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, (CBLAS_TRANSPOSE)trans,
                    CblasNonUnit, N, N,
                    (zone), CHOLB, LDB,
                                       TEMP,  N); 

        /* 
         * Compute Z**T*inv(B)*Z-Id = (Z**T*inv(L)**T) * (inv(L)*Z) - Id
         *
         * Note: Z**T*inv(L)**T is the ConjTranspose of the previous result, so we use SSYRK 
         */ 
        cblas_ssyrk(CblasColMajor, (CBLAS_UPLO)uplo, CblasTrans,
                    N, N, done, TEMP, N, mdone, Id, N);
    } else {
        /*
         * if ITYPE = 1 or 2, Z**T*B*Z = I;
         */ 
        str = "Z**T*B*Z";
        cblas_ssymm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    (zone),  B,    LDB,
                                        Z,    LDZ,
                    (zzero), TEMP, N);
        cblas_sgemm(CblasColMajor, CblasTrans, CblasNoTrans, N, N, N,
                    (zone),  Z,    LDZ,
                                        TEMP, N,
                    (mzone), Id  , N);
    }

    normQ = LAPACKE_slansy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), 'U', N, Id, N, work);
    result = normQ / (N * eps);
    printf(" ======================================================\n");
    printf(" | Id-%s |_oo / (N*eps)          : %15.3E \n",str, result );
    printf(" ======================================================\n");

    if ( isnan(result) || isinf(result) || (result > 60.0) ) {
        printf("-- Orthogonality is suspicious ! \n");
        info_ortho=1;
    }
    else {
        printf("-- Orthogonality is CORRECT ! \n");
        info_ortho=0;
    }

    free(work); free(Id); free(TEMP);
    return info_ortho;
}

/*------------------------------------------------------------
 *  Check the reduction 
 */
static int check_reduction(int itype, int uplo, int N, float *D,
                           float *A, int LDA,
                           float *B, int LDB,
                           float *Z, int LDZ,
                           float eps )
{
    float zone  =  1.0;
    float zzero =  0.0;
    float mzone = -1.0;
    float Anorm, Znorm, Rnorm, result;
    int info_reduction;
    int i;
    char *str;
    float *TEMP     = (float *)malloc(N*N*sizeof(float));
    float *Residual = (float *)malloc(N*N*sizeof(float));
    float *work = (float *)malloc(N*sizeof(float));

    if( itype == 1 ) {
        /*
         * | A Z - B Z D | / ( |A| |Z| n ulp )
         */     
        str = " A Z - B Z D ";

        /* Compute TEMP = Z*D */
        LAPACKE_slacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower),
                            N, N, Z, LDZ, TEMP, N);        

        for (i=0; i<N; i++) {
            cblas_sscal(N, D[i], TEMP + i*N, 1);
        }

        /* Compute Residual = B*Z*D = B*TEMP */
        cblas_ssymm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    (zone),  B,        LDB, 
                                        TEMP,     N,   
                    (zzero), Residual, N);

        /* Compute A*Z - B*Z*D */
        cblas_ssymm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    (zone),  A,        LDA,
                                        Z,        LDZ,
                    (mzone), Residual, N);
    }
    else if( itype == 2 ) { 
        /*
         * | A B Z - Z D | / ( |A| |Z| n ulp )
         */     
        str = " A B Z - Z D ";

        /* Compute Residual = Z*D */
        LAPACKE_slacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower),
                            N, N, Z, LDZ, Residual, N);

        for (i=0; i<N; i++) {
            cblas_sscal(N, D[i], Residual + i*N, 1);
        }

        /* Compute TEMP = B*Z */
        cblas_ssymm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    (zone),  B,    LDB,
                                        Z,    LDZ,
                    (zzero), TEMP, N);

        /* Compute A*B*Z - Z*D = A*TEMP-Residual */
        cblas_ssymm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    (zone),  A,        LDA,
                                        TEMP,     N,
                    (mzone), Residual, N);
    } else {
        /*
         * | B A Z - Z D | / ( |A| |Z| n ulp )
         */     
        str = " B A Z - Z D ";

        /* Compute Residual = Z*D */
        LAPACKE_slacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower),
                            N, N, Z, LDZ, Residual, N);

        for (i=0; i<N; i++) {
            cblas_sscal(N, D[i], Residual + i*N, 1);
        }

        /* Compute TEMP = A*Z */
        cblas_ssymm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    (zone),  A,    LDA,
                                        Z,    LDZ,
                    (zzero), TEMP, N);

        /* Compute B*A*Z - Z*D = B*TEMP-Residual */
        cblas_ssymm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    (zone),  B,        LDB,
                                        TEMP,     N,
                    (mzone), Residual, N);
    }

    Rnorm = LAPACKE_slange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaOneNorm), N, N, Residual, N,   work);
    Anorm = LAPACKE_slansy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaOneNorm), lapack_const(uplo), N, A,        LDA, work);
    Znorm = LAPACKE_slange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaOneNorm), N, N, Z,        LDZ, work);

    result = Rnorm / ( Anorm * Znorm * N * eps);
    printf(" ======================================================\n");
    printf(" | %s |_oo/(|A|_oo.|Z|_oo.N.eps) : %15.3E \n", str, result );
    printf(" ======================================================\n");
    
    if ( isnan(result) || isinf(result) || (result > 60.0) ) {
        printf("-- Reduction is suspicious ! \n");
        info_reduction = 1;
    }
    else {
        printf("-- Reduction is CORRECT ! \n");
        info_reduction = 0;
    }

    free(TEMP); free(Residual); free(work);
    return info_reduction;
}
/*--------------------------------------------------------------
 * Check the solution
 */
static int check_solution(int N, float *E1, float *E2, float eps)
{
    int info_solution, i;
    float resid;
    float maxtmp;
    float maxel = fabs( fabs(E1[0]) - fabs(E2[0]) );
    float maxeig = max( fabs(E1[0]), fabs(E2[0]) );

    for (i=1; i<N; i++) {
        resid  = fabs(fabs(E1[i])-fabs(E2[i]));
        maxtmp = max(fabs(E1[i]), fabs(E2[i]));

        /* Update */
        maxeig = max(maxtmp, maxeig);
        maxel  = max(resid,  maxel );
    }
    
    maxel = maxel / (maxeig * N * eps);
    printf(" ======================================================\n");
    printf(" | D - eigcomputed | / (|D| * N * eps) : %15.3E \n",  maxel );
    printf(" ======================================================\n");

    printf("============\n");
    printf("Checking the eigenvalues of A\n");
    if ( isnan(maxel) || isinf(maxel) || (maxel > 100) ) {
        printf("-- The eigenvalues are suspicious ! \n");
        info_solution = 1;
    }
    else{
        printf("-- The eigenvalues are CORRECT ! \n");
        info_solution = 0;
    }
    return info_solution;
}

