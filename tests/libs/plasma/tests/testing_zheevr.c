/**
 *
 * @file testing_zheevr.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Azzam Haidar
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

static int check_orthogonality(int, int, PLASMA_Complex64_t*, int, double);
static int check_reduction(int, int, int, PLASMA_Complex64_t*, double*, int, PLASMA_Complex64_t*, double);
static int check_solution(int, double*, double*, double);

int testing_zheevr(int argc, char **argv)
{
    /* Check for number of arguments*/
    if (argc != 2) {
        USAGE("HEEVR", "N LDA",
              "   - N    : size of the matrix A\n"
              "   - LDA  : leading dimension of the matrix A\n");
        return -1;
    }

    int    N     = atoi(argv[0]);
    int    LDA   = atoi(argv[1]);
    int    LDQ   = LDA;
    int    mode  = 4;
    double eps   = LAPACKE_dlamch_work('e');
    double dmax  = 1.0;
    double rcond = 1.0e6;
    int INFO=-1;

    double abstol = LAPACKE_dlamch_work('s'); /* not used in this version */
    int nbcomputedeig = 0;
    double vl = 0.0;
    double vu = 0.0;
    int il    = 0;
    int iu    = (int)(0.2*(double)N)/N; // this will compute the first 20% of the eigenvectors. note that you will have to switch range  = PlasmaIvec;
    PLASMA_enum range  = PlasmaAllVec;
    PLASMA_enum uplo   = PlasmaLower;
    PLASMA_enum vec    = PlasmaVec;
    int info_ortho     = 0;
    int info_solution  = 0;
    int info_reduction = 0;
    int LDAxN = LDA*N;
    int LDQxN = LDQ*N;

    PLASMA_Complex64_t *A1   = NULL;
    PLASMA_Complex64_t *A2   = (PLASMA_Complex64_t *)malloc(LDAxN*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Q    = NULL;
    double             *W1   = (double *)malloc(N*sizeof(double));
    double             *W2   = (double *)malloc(N*sizeof(double));
    PLASMA_Complex64_t *work = (PLASMA_Complex64_t *)malloc(3*N* sizeof(PLASMA_Complex64_t));
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
    PLASMA_Alloc_Workspace_zheevd(N, N, &T);

    /*----------------------------------------------------------
    *  TESTING ZHEEVR
    */
    /* Initialize A1 */
    LAPACKE_zlatms_work( LAPACK_COL_MAJOR, N, N,
                         lapack_const(PlasmaDistSymmetric), ISEED,
                         lapack_const(PlasmaHermGeev), W1, mode, rcond,
                         dmax, N, N,
                         lapack_const(PlasmaNoPacking), A2, LDA, work );

    /*
     * Sort the eigenvalue because when computing the tridiag
     * and then the eigenvalue of the DSTQR are sorted.
     * So to avoid testing fail when having good results W1 should be sorted
    */
    LAPACKE_dlasrt_work( 'I', N, W1 );

    if ( vec == PlasmaVec ) {
        Q  = (PLASMA_Complex64_t *)malloc(LDQxN*sizeof(PLASMA_Complex64_t));
        A1 = (PLASMA_Complex64_t *)malloc(LDAxN*sizeof(PLASMA_Complex64_t));

        /* Copy A2 into A1 */
        LAPACKE_zlacpy_work(LAPACK_COL_MAJOR, 'A', N, N, A2, LDA, A1, LDA);
    }
    /*
     * PLASMA ZHEEVR
     */
    INFO = PLASMA_zheevr(vec, range, uplo, N, A2, LDA, vl, vu, il, iu, abstol, &nbcomputedeig, W2, T, Q, LDQ);

    if(INFO!=0){
            printf(" ERROR OCCURED INFO %d\n",INFO);
            goto fin;
    }

    printf("\n");
    printf("------ TESTS FOR PLASMA ZHEEVR ROUTINE -------  \n");
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
        printf(" ---- TESTING ZHEEVR ...................... PASSED !\n");
        printf("***************************************************\n");
    }
    else {
        printf("************************************************\n");
        printf(" - TESTING ZHEEVR ... FAILED !\n");
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
static int check_orthogonality(int M, int N, PLASMA_Complex64_t *Q, int LDQ, double eps)
{
    double  done  =  1.0;
    double  mdone = -1.0;
    double  normQ, result;
    int     info_ortho;
    int     minMN = min(M, N);
    double *work = (double *)malloc(minMN*sizeof(double));

    /* Build the idendity matrix */
    PLASMA_Complex64_t *Id = (PLASMA_Complex64_t *) malloc(minMN*minMN*sizeof(PLASMA_Complex64_t));
    LAPACKE_zlaset_work(LAPACK_COL_MAJOR, 'A', minMN, minMN, 0., 1., Id, minMN);

    /* Perform Id - Q'Q */
    if (M >= N)
        cblas_zherk(CblasColMajor, CblasUpper, CblasConjTrans, N, M, done, Q, LDQ, mdone, Id, N);
    else
        cblas_zherk(CblasColMajor, CblasUpper, CblasNoTrans,   M, N, done, Q, LDQ, mdone, Id, M);

    normQ = LAPACKE_zlanhe_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), 'U', minMN, Id, minMN, work);

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
static int check_reduction(int uplo, int N, int bw, PLASMA_Complex64_t *A, double *D, int LDA, PLASMA_Complex64_t *Q, double eps )
{
    PLASMA_Complex64_t zone  =  1.0;
    PLASMA_Complex64_t mzone = -1.0;
    PLASMA_Complex64_t *TEMP     = (PLASMA_Complex64_t *)malloc(N*N*sizeof(PLASMA_Complex64_t));
    PLASMA_Complex64_t *Residual = (PLASMA_Complex64_t *)malloc(N*N*sizeof(PLASMA_Complex64_t));
    double *work = (double *)malloc(N*sizeof(double));
    double Anorm, Rnorm, result;
    int info_reduction;
    int i;

    /* Compute TEMP =  Q * LAMBDA */
    LAPACKE_zlacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower), N, N, Q, LDA, TEMP, N);
    for (i = 0; i < N; i++){
            cblas_zdscal(N, D[i], &(TEMP[i*N]),1);
    }
    /* Compute Residual = A - Q * LAMBDA * Q^H */
    /* A is Hermetian but both upper and lower
     * are assumed valable here for checking
     * otherwise it need to be symetrized before
     * checking.
     */
    LAPACKE_zlacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower), N, N, A, LDA, Residual, N);
    cblas_zgemm(CblasColMajor, CblasNoTrans, CblasConjTrans, N, N, N, CBLAS_SADDR(zone), TEMP, N,  Q, LDA, CBLAS_SADDR(mzone), Residual,     N);

    Rnorm = LAPACKE_zlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaOneNorm), N, N, Residual, N,   work);
    Anorm = LAPACKE_zlanhe_work(LAPACK_COL_MAJOR, lapack_const(PlasmaOneNorm), lapack_const(uplo), N, A,       LDA, work);

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
static int check_solution(int N, double *E1, double *E2, double eps)
{
    int info_solution, i;
    double resid;
    double maxtmp;
    double maxel = fabs( fabs(E1[0]) - fabs(E2[0]) );
    double maxeig = max( fabs(E1[0]), fabs(E2[0]) );
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
