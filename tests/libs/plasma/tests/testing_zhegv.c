/**
 *
 * @file testing_zhegv.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Azzam Haidar
 * @author Hatem Ltaief
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

static int check_orthogonality(int itype, int uplo, int N,
                               PLASMA_Complex64_t *Z, int LDZ,
                               PLASMA_Complex64_t *B, PLASMA_Complex64_t *CHOLB, int LDB,
                               double eps);
static int check_reduction(int itype, int uplo, int N, double *D,
                           PLASMA_Complex64_t *A, int LDA,
                           PLASMA_Complex64_t *B, int LDB,
                           PLASMA_Complex64_t *Z, int LDZ,
                           double eps );
static int check_solution(int N, double *E1, double *E2, double eps);

int testing_zhegv(int argc, char **argv)
{
    /* Check for number of arguments*/
    if (argc != 3) {
        USAGE("HEGV", "N LDA LDB",
              "   - N    : size of the matrices A and B\n"
              "   - LDA  : leading dimension of the matrix A\n"
              "   - LDB  : leading dimension of the matrix B\n");
        return -1;
    }

    double      eps = LAPACKE_dlamch_work('e');
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

    PLASMA_Complex64_t *A1    = (PLASMA_Complex64_t *)malloc(LDAxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *A2    = (PLASMA_Complex64_t *)malloc(LDAxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *B1    = (PLASMA_Complex64_t *)malloc(LDBxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *B2    = (PLASMA_Complex64_t *)malloc(LDBxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Q     = (PLASMA_Complex64_t *)malloc(LDQxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Ainit = (PLASMA_Complex64_t *)malloc(LDAxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Binit = (PLASMA_Complex64_t *)malloc(LDBxN*sizeof(PLASMA_Complex64_t));
    double *W1 = (double *)malloc(N*sizeof(double));
    double *W2 = (double *)malloc(N*sizeof(double));
    PLASMA_Complex64_t *work = (PLASMA_Complex64_t *)malloc(3*N* sizeof(PLASMA_Complex64_t));
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
    PLASMA_Alloc_Workspace_zhegv(N, N, &T);

    /*----------------------------------------------------------
    *  TESTING ZHEGV
    */
    /* Initialize A1 and Ainit */
    PLASMA_zplghe(0., N, A1, LDA, 5198);
    PLASMA_zlacpy(PlasmaUpperLower, N, N, A1, LDA, Ainit, LDA);

    /* Initialize B1 and Binit */
    PLASMA_zplghe((double)N, N, B1, LDB, 4321 );
    PLASMA_zlacpy(PlasmaUpperLower, N, N, B1, LDB, Binit, LDB);

    printf("\n");
    printf("------ TESTS FOR PLASMA ZHEGV ROUTINE -------  \n");
    printf("        Size of the Matrix %d by %d\n", N, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 60.\n");

    /*----------------------------------------------------------
     *  TESTING ZHEGV
     */

    for (i=0; i<3; i++) {
        for (u=0; u<2; u++) {
            memcpy(A2, Ainit, LDAxN*sizeof(PLASMA_Complex64_t));
            memcpy(B2, Binit, LDBxN*sizeof(PLASMA_Complex64_t));
            /* CALL ZHEGV with itype= 1, 2, 3 and uplo= L,U */ 
            PLASMA_zhegv(itype[i], vec, uplo[u], N, A2, LDA, B2, LDB, W2, T, Q, LDQ);

            /* CALL LAPACK to compute eigenvalues. */
            memcpy(A1, Ainit, LDAxN*sizeof(PLASMA_Complex64_t));
            memcpy(B1, Binit, LDBxN*sizeof(PLASMA_Complex64_t));
            LAPACKE_zhegv( LAPACK_COL_MAJOR, 
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
                printf(" ---- TESTING ZHEGV (%s, %s) ...................... PASSED !\n", itypestr[i], uplostr[u]);
                printf("***************************************************\n");
            }
            else {
                printf("************************************************\n");
                printf(" - TESTING ZHEGV (%s, %s) ... FAILED !\n", itypestr[i], uplostr[u]);
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
                               PLASMA_Complex64_t *Z, int LDZ,
                               PLASMA_Complex64_t *B, PLASMA_Complex64_t *CHOLB, int LDB,
                               double eps)
{
    static PLASMA_Complex64_t zone  =  1.0;
    static PLASMA_Complex64_t mzone = -1.0;
    static PLASMA_Complex64_t zzero =  0.0;
    static double             done  =  1.0;
    static double             mdone = -1.0;
    PLASMA_enum trans;
    double  normQ, result;
    int     info_ortho;
    double *work = (double *)malloc(N*sizeof(double));
    PLASMA_Complex64_t *TEMP    = (PLASMA_Complex64_t *)malloc(N*N*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Id = (PLASMA_Complex64_t *) malloc(N*N*sizeof(PLASMA_Complex64_t));
    char *str;

    /* Build the idendity matrix */
    LAPACKE_zlaset_work(LAPACK_COL_MAJOR, 'A', N, N, 0., 1., Id, N);

    /* Perform orth test*/
    if ( itype == 3 ) {
        /*
         * if ITYPE = 3, Z**H*inv(B)*Z = I.
         * inv(B) = inv(L)**H * inv(L) or inv(U)*inv(U)**H
         */ 
        str = "Z**H*inv(B)*Z";

        if( uplo==PlasmaUpper ) {
            trans = PlasmaConjTrans;
        } else {
            trans = PlasmaNoTrans;
        }

        /* Compute inv(L)*Z or inv(U)**H*Z */ 
        LAPACKE_zlacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower),
                            N, N, Z, LDZ, TEMP, N);

        cblas_ztrsm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, (CBLAS_TRANSPOSE)trans,
                    CblasNonUnit, N, N,
                    CBLAS_SADDR(zone), CHOLB, LDB,
                                       TEMP,  N); 

        /* 
         * Compute Z**H*inv(B)*Z-Id = (Z**H*inv(L)**H) * (inv(L)*Z) - Id
         *
         * Note: Z**H*inv(L)**H is the ConjTranspose of the previous result, so we use ZHERK 
         */ 
        cblas_zherk(CblasColMajor, (CBLAS_UPLO)uplo, CblasConjTrans,
                    N, N, done, TEMP, N, mdone, Id, N);
    } else {
        /*
         * if ITYPE = 1 or 2, Z**H*B*Z = I;
         */ 
        str = "Z**H*B*Z";
        cblas_zhemm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    CBLAS_SADDR(zone),  B,    LDB,
                                        Z,    LDZ,
                    CBLAS_SADDR(zzero), TEMP, N);
        cblas_zgemm(CblasColMajor, CblasConjTrans, CblasNoTrans, N, N, N,
                    CBLAS_SADDR(zone),  Z,    LDZ,
                                        TEMP, N,
                    CBLAS_SADDR(mzone), Id  , N);
    }

    normQ = LAPACKE_zlanhe_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), 'U', N, Id, N, work);
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
static int check_reduction(int itype, int uplo, int N, double *D,
                           PLASMA_Complex64_t *A, int LDA,
                           PLASMA_Complex64_t *B, int LDB,
                           PLASMA_Complex64_t *Z, int LDZ,
                           double eps )
{
    PLASMA_Complex64_t zone  =  1.0;
    PLASMA_Complex64_t zzero =  0.0;
    PLASMA_Complex64_t mzone = -1.0;
    double Anorm, Znorm, Rnorm, result;
    int info_reduction;
    int i;
    char *str;
    PLASMA_Complex64_t *TEMP     = (PLASMA_Complex64_t *)malloc(N*N*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Residual = (PLASMA_Complex64_t *)malloc(N*N*sizeof(PLASMA_Complex64_t));
    double *work = (double *)malloc(N*sizeof(double));

    if( itype == 1 ) {
        /*
         * | A Z - B Z D | / ( |A| |Z| n ulp )
         */     
        str = " A Z - B Z D ";

        /* Compute TEMP = Z*D */
        LAPACKE_zlacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower),
                            N, N, Z, LDZ, TEMP, N);        

        for (i=0; i<N; i++) {
            cblas_zdscal(N, D[i], TEMP + i*N, 1);
        }

        /* Compute Residual = B*Z*D = B*TEMP */
        cblas_zhemm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    CBLAS_SADDR(zone),  B,        LDB, 
                                        TEMP,     N,   
                    CBLAS_SADDR(zzero), Residual, N);

        /* Compute A*Z - B*Z*D */
        cblas_zhemm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    CBLAS_SADDR(zone),  A,        LDA,
                                        Z,        LDZ,
                    CBLAS_SADDR(mzone), Residual, N);
    }
    else if( itype == 2 ) { 
        /*
         * | A B Z - Z D | / ( |A| |Z| n ulp )
         */     
        str = " A B Z - Z D ";

        /* Compute Residual = Z*D */
        LAPACKE_zlacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower),
                            N, N, Z, LDZ, Residual, N);

        for (i=0; i<N; i++) {
            cblas_zdscal(N, D[i], Residual + i*N, 1);
        }

        /* Compute TEMP = B*Z */
        cblas_zhemm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    CBLAS_SADDR(zone),  B,    LDB,
                                        Z,    LDZ,
                    CBLAS_SADDR(zzero), TEMP, N);

        /* Compute A*B*Z - Z*D = A*TEMP-Residual */
        cblas_zhemm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    CBLAS_SADDR(zone),  A,        LDA,
                                        TEMP,     N,
                    CBLAS_SADDR(mzone), Residual, N);
    } else {
        /*
         * | B A Z - Z D | / ( |A| |Z| n ulp )
         */     
        str = " B A Z - Z D ";

        /* Compute Residual = Z*D */
        LAPACKE_zlacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower),
                            N, N, Z, LDZ, Residual, N);

        for (i=0; i<N; i++) {
            cblas_zdscal(N, D[i], Residual + i*N, 1);
        }

        /* Compute TEMP = A*Z */
        cblas_zhemm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    CBLAS_SADDR(zone),  A,    LDA,
                                        Z,    LDZ,
                    CBLAS_SADDR(zzero), TEMP, N);

        /* Compute B*A*Z - Z*D = B*TEMP-Residual */
        cblas_zhemm(CblasColMajor, CblasLeft, (CBLAS_UPLO)uplo, N, N,
                    CBLAS_SADDR(zone),  B,        LDB,
                                        TEMP,     N,
                    CBLAS_SADDR(mzone), Residual, N);
    }

    Rnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaOneNorm), N, N, Residual, N,   work);
    Anorm = LAPACKE_zlanhe_work(LAPACK_COL_MAJOR, lapack_const(PlasmaOneNorm), lapack_const(uplo), N, A,        LDA, work);
    Znorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaOneNorm), N, N, Z,        LDZ, work);

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
static int check_solution(int N, double *E1, double *E2, double eps)
{
    int info_solution, i;
    double resid;
    double maxtmp;
    double maxel = fabs( fabs(E1[0]) - fabs(E2[0]) );
    double maxeig = max( fabs(E1[0]), fabs(E2[0]) );

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

