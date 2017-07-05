/**
 *
 * @file testing_ssyevd.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Hatem Ltaief
 * @author Azzam Haidar
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

static int check_orthogonality(int, int, float*, int, float);
static int check_reduction(int, int, int, float*, float*, int, float*, float);
static int check_solution(int, float*, float*, float);

int testing_ssyevd(int argc, char **argv)
{
    /* Check for number of arguments*/
    if (argc != 3) {
        USAGE("HEEVD", "MODE N LDA",
              "   - MODE : mode used to generate the matrix\n"
              "   - N    : size of the matrix A\n"
              "   - LDA  : leading dimension of the matrix A\n");
        return -1;
    }

    int    mode  = atoi(argv[0]);
    int    N     = atoi(argv[1]);
    int    LDA   = atoi(argv[2]);
    int    LDQ   = LDA;
    float eps   = LAPACKE_slamch_work('e');
    float dmax  = 1.0;
    float rcond = 1.0e6;
    int INFO=-1;

    PLASMA_enum uplo = PlasmaLower;
    PLASMA_enum vec  = PlasmaVec;
    int info_ortho     = 0;
    int info_solution  = 0;
    int info_reduction = 0;
    int LDAxN = LDA*N;
    int LDQxN = LDQ*N;

    float *A1   = NULL;
    float *A2   = (float *)malloc(LDAxN*sizeof(float));
    float *Q    = NULL;
    float             *W1   = (float *)malloc(N*sizeof(float));
    float             *W2   = (float *)malloc(N*sizeof(float));
    float *work = (float *)malloc(3*N* sizeof(float));
    PLASMA_desc *T;

    /* Check if unable to allocate memory */
    if ( (!A2) || (!W1) || (!W2) ){
        printf("Out of Memory \n ");
        return -2;
    }

    PLASMA_Disable(PLASMA_AUTOTUNING);
    PLASMA_Set(PLASMA_TILE_SIZE, 120);
    PLASMA_Set(PLASMA_INNER_BLOCK_SIZE, 20);

    PLASMA_Enable(PLASMA_WARNINGS);
    PLASMA_Enable(PLASMA_ERRORS);
    PLASMA_Alloc_Workspace_ssyevd(N, N, &T);

    /*----------------------------------------------------------
    *  TESTING SSYEVD
    */
    /* Initialize A1 */
    if (mode == 0){
        int i;
        for (i=0; i<N; i++){
            W1[i] = (float )i+1;
        }
    }
    LAPACKE_slatms_work( LAPACK_COL_MAJOR, N, N,
                         lapack_const(PlasmaDistSymmetric), ISEED,
                         lapack_const(PlasmaHermGeev), W1, mode, rcond,
                         dmax, N, N,
                         lapack_const(PlasmaNoPacking), A2, LDA, work );

    /*
     * Sort the eigenvalue because when computing the tridiag
     * and then the eigenvalue of the DSTQR are sorted.
     * So to avoid testing fail when having good results W1 should be sorted
     */
    LAPACKE_slasrt_work( 'I', N, W1 );

    if ( vec == PlasmaVec ) {
        Q  = (float *)malloc(LDQxN*sizeof(float));
        A1 = (float *)malloc(LDAxN*sizeof(float));

        /* Copy A2 into A1 */
        LAPACKE_slacpy_work(LAPACK_COL_MAJOR, 'A', N, N, A2, LDA, A1, LDA);
    }
    /*
     * PLASMA SSYEVD
     */
    INFO = PLASMA_ssyevd(vec, uplo, N, A2, LDA, W2, T, Q, LDQ);
    if(INFO!=0){
        printf(" ERROR OCCURED INFO %d\n",INFO);
        goto fin;
    }

    printf("\n");
    printf("------ TESTS FOR PLASMA SSYEVD ROUTINE -------  \n");
    printf("        Size of the Matrix %d by %d\n", N, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 60.\n");

    /* Check the orthogonality, reduction and the eigen solutions */
    if (vec == PlasmaVec) {
        info_ortho = check_orthogonality(N, N, Q, LDQ, eps);
        info_reduction = check_reduction(uplo, N, 1, A1, W2, LDA, Q, eps);
    }
    info_solution = check_solution(N, W1, W2, eps);

    if ( (info_solution == 0) & (info_ortho == 0) & (info_reduction == 0) ) {
        printf("***************************************************\n");
        printf(" ---- TESTING SSYEVD ...................... PASSED !\n");
        printf("***************************************************\n");
    }
    else {
        printf("************************************************\n");
        printf(" - TESTING SSYEVD ... FAILED !\n");
        printf("************************************************\n");
    }

 fin:
    PLASMA_Dealloc_Handle_Tile(&T);
    free(A2);
    free(W1);
    free(W2);
    free(work);
    if (Q  != NULL) free(Q);
    if (A1 != NULL) free(A1);

    return 0;
}

/*-------------------------------------------------------------------
 * Check the orthogonality of Q
 */
static int check_orthogonality(int M, int N, float *Q, int LDQ, float eps)
{
    float  done  =  1.0;
    float  mdone = -1.0;
    float  normQ, result;
    int     info_ortho;
    int     minMN = min(M, N);
    float *work = (float *)malloc(minMN*sizeof(float));

    /* Build the idendity matrix */
    float *Id = (float *) malloc(minMN*minMN*sizeof(float));
    LAPACKE_slaset_work(LAPACK_COL_MAJOR, 'A', minMN, minMN, 0., 1., Id, minMN);

    /* Perform Id - Q'Q */
    if (M >= N)
        cblas_ssyrk(CblasColMajor, CblasUpper, CblasTrans, N, M, done, Q, LDQ, mdone, Id, N);
    else
        cblas_ssyrk(CblasColMajor, CblasUpper, CblasNoTrans,   M, N, done, Q, LDQ, mdone, Id, M);

    normQ = LAPACKE_slansy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), 'U', minMN, Id, minMN, work);

    result = normQ / (minMN * eps);
    printf(" ======================================================\n");
    printf(" ||Id-Q'*Q||_oo / (minMN*eps)          : %15.3E \n",  result );
    printf(" ======================================================\n");

    if ( isnan(result) || isinf(result) || (result > 60.0) ) {
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
 *  Check the reduction
 */
static int check_reduction(int uplo, int N, int bw, float *A, float *D, int LDA, float *Q, float eps )
{
    (void) bw;
    float zone  =  1.0;
    float mzone = -1.0;
    float *TEMP     = (float *)malloc(N*N*sizeof(float));
    float *Residual = (float *)malloc(N*N*sizeof(float));
    float *work = (float *)malloc(N*sizeof(float));
    float Anorm, Rnorm, result;
    int info_reduction;
    int i;

    /* Compute TEMP =  Q * LAMBDA */
    LAPACKE_slacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower), N, N, Q, LDA, TEMP, N);
    for (i = 0; i < N; i++){
        cblas_sscal(N, D[i], &(TEMP[i*N]),1);
    }
    /* Compute Residual = A - Q * LAMBDA * Q^H */
    /* A is Hermetian but both upper and lower
     * are assumed valable here for checking
     * otherwise it need to be symetrized before
     * checking.
     */
    LAPACKE_slacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower), N, N, A, LDA, Residual, N);
    cblas_sgemm(CblasColMajor, CblasNoTrans, CblasTrans, N, N, N, (mzone), TEMP, N,  Q, LDA, (zone), Residual,     N);

    Rnorm = LAPACKE_slange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaOneNorm), N, N, Residual, N,   work);
    Anorm = LAPACKE_slansy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaOneNorm), lapack_const(uplo), N, A,       LDA, work);

    result = Rnorm / ( Anorm * N * eps);
    if ( uplo == PlasmaLower ){
        printf(" ======================================================\n");
        printf(" ||A-Q*LAMBDA*Q'||_oo/(||A||_oo.N.eps) : %15.3E \n",  result );
        printf(" ======================================================\n");
    }else{
        printf(" ======================================================\n");
        printf(" ||A-Q'*LAMBDA*Q||_oo/(||A||_oo.N.eps) : %15.3E \n",  result );
        printf(" ======================================================\n");
    }

    if ( isnan(result) || isinf(result) || (result > 60.0) ) {
        printf("-- Reduction is suspicious ! \n");
        info_reduction = 1;
    }
    else {
        printf("-- Reduction is CORRECT ! \n");
        info_reduction = 0;
    }

    free(TEMP); free(Residual);
    free(work);

    return info_reduction;
}
/*------------------------------------------------------------
 *  Check the eigenvalues
 */
static int check_solution(int N, float *E1, float *E2, float eps)
{
    int info_solution, i;
    float resid;
    float maxtmp;
    float maxel = fabs( fabs(E1[0]) - fabs(E2[0]) );
    float maxeig = max( fabs(E1[0]), fabs(E2[0]) );
    for (i = 1; i < N; i++){
        resid   = fabs(fabs(E1[i])-fabs(E2[i]));
        maxtmp  = max(fabs(E1[i]), fabs(E2[i]));

        /* Update */
        maxeig = max(maxtmp, maxeig);
        maxel  = max(resid,  maxel );
    }

    maxel = maxel / (maxeig * N * eps);
    printf(" ======================================================\n");
    printf(" | D - eigcomputed | / (|D| * N * eps) : %15.3E \n",  maxel );
    printf(" ======================================================\n");

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
