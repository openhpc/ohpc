/**
 *
 * @file testing_dgesvd.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Azzam Haidar
 * @author Hatem Ltaief
 * @date 2010-11-15
 * @generated d Fri Apr  1 11:03:05 2016
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
#include "testing_dmain.h"

#undef COMPLEX
#define REAL

static int check_orthogonality(int, int, int, double*, int, double);
static int check_reduction(int, int, double*, double*, int, double*, int, double*, int, double);
static int check_solution(int, double*, double*, double);

int testing_dgesvd(int argc, char **argv)
{
    int tree = 0;

    if ( argc < 1 ){
        goto usage;
    } else {
        tree = atoi(argv[0]);
    }

    /* Check for number of arguments*/
    if ( ((tree == 0) && (argc != 4)) ||
         ((tree != 0) && (argc != 5)) ){
      usage:
        USAGE("GESVD", "MODE M N LDA [RH]",
              "   - MODE : 0: flat, 1: tree (RH needed)\n"
              "   - M    : number of rows of the matrix A\n"
              "   - N    : number of columns of the matrix A\n"
              "   - LDA  : leading dimension of the matrix A\n"
              "   - RH   : Size of each subdomains\n");
        return -1;
    }

    int M   = atoi(argv[1]);
    int N   = atoi(argv[2]);
    int LDA = atoi(argv[3]);
    int rh;
    if ( tree ) {
        rh = atoi(argv[4]);
        PLASMA_Set(PLASMA_HOUSEHOLDER_MODE, PLASMA_TREE_HOUSEHOLDER);
        PLASMA_Set(PLASMA_HOUSEHOLDER_SIZE, rh);
    }

    if (LDA < M){
        printf("LDA should be >= M !\n");
        return -1;
    }

    double eps  = LAPACKE_dlamch_work('e');
    double dmax = 1.0;
    PLASMA_enum jobu  = PlasmaVec;
    PLASMA_enum jobvt = PlasmaVec;
    int info_orthou    = 0;
    int info_orthovt   = 0;
    int info_solution  = 0;
    int info_reduction = 0;
    int minMN = min(M, N);
    int mode  = 4;
    double rcond = (double) minMN;
    int INFO=-1;

    double *A1   = (double *)malloc(LDA*N*sizeof(double));
    double *S1               = (double *)            malloc(minMN*sizeof(double));
    double *S2               = (double *)            malloc(minMN*sizeof(double));
    double *work = (double *)malloc(3*max(M, N)* sizeof(double));
    double *A2 = NULL;
    double *U  = NULL;
    double *VT = NULL;
    PLASMA_desc *T;

    /* Check if unable to allocate memory */
    if ( (!A1) || (!S1) || (!S2) || (!work) ) {
        printf("Out of Memory \n ");
        return -2;
    }

    PLASMA_Disable(PLASMA_AUTOTUNING);
    PLASMA_Set(PLASMA_TILE_SIZE, 120);
    PLASMA_Set(PLASMA_INNER_BLOCK_SIZE, 20);

    PLASMA_Enable(PLASMA_WARNINGS);
    PLASMA_Enable(PLASMA_ERRORS);
    PLASMA_Alloc_Workspace_dgesvd(M, N, &T);

    /*----------------------------------------------------------
    *  TESTING DGESVD
    */
    /* Initialize A1 */
    LAPACKE_dlatms_work( LAPACK_COL_MAJOR, M, N,
                         lapack_const(PlasmaDistUniform), ISEED,
                         lapack_const(PlasmaNonsymPosv), S1, mode, rcond,
                         dmax, M, N,
                         lapack_const(PlasmaNoPacking), A1, LDA, work );
    free(work);

    /* Copy A1 for check */
    if ( (jobu == PlasmaVec) && (jobvt == PlasmaVec) ) {
        A2 = (double *)malloc(LDA*N*sizeof(double));
        LAPACKE_dlacpy_work(LAPACK_COL_MAJOR, 'A', M, N, A1, LDA, A2, LDA);
    }
    if ( jobu == PlasmaVec ) {
        U = (double *)malloc(M*M*sizeof(double));
        LAPACKE_dlaset_work(LAPACK_COL_MAJOR, 'A', M, M, 0., 1., U, M);
    }
    if ( jobvt == PlasmaVec ) {
        VT = (double *)malloc(N*N*sizeof(double));
        LAPACKE_dlaset_work(LAPACK_COL_MAJOR, 'A', N, N, 0., 1., VT, N);
    }

    /* PLASMA DGESVD */
    INFO = PLASMA_dgesvd(jobu, jobvt, M, N, A1, LDA, S2, T, U, M, VT, N);
    if(INFO!=0){
            printf(" ERROR OCCURED INFO %d\n",INFO);
            goto fin;
    }

    printf("\n");
    printf("------ TESTS FOR PLASMA DGESVD ROUTINE -------  \n");
    printf("        Size of the Matrix %d by %d\n", M, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 60.\n");


    /* Check the orthogonality, reduction and the singular values */
    if ( jobu == PlasmaVec )
        info_orthou = check_orthogonality(PlasmaLeft, M, M, U, M, eps);

    if ( jobvt == PlasmaVec )
        info_orthovt = check_orthogonality(PlasmaRight, N, N, VT, N, eps);

    /*
     * WARNING: For now, Q is associated to Band tridiagonal reduction and
     * not to the final tridiagonal reduction, so we can not call the check
     * Need to accumulate the orthogonal transformations
     * during the bulge chasing to be able to perform the next test!
     */
    if ( (jobu == PlasmaVec) && (jobvt == PlasmaVec) )
        info_reduction = check_reduction(M, N, S2, A2, LDA, U, M, VT, N, eps);

    info_solution = check_solution(minMN, S1, S2, eps);

    if ( (info_solution == 0) & (info_orthou == 0) &
         (info_orthovt == 0) & (info_reduction == 0) ) {
        if (M >= N) {
           printf("***************************************************\n");
           printf(" ---- TESTING DGESVD .. M >= N ........... PASSED !\n");
           printf("***************************************************\n");
        }
        else {
           printf("***************************************************\n");
           printf(" ---- TESTING DGESVD .. M < N ............ PASSED !\n");
           printf("***************************************************\n");
        }
    }
    else {
        if (M >= N) {
           printf("************************************************\n");
           printf(" - TESTING DGESVD .. M >= N .. FAILED !\n");
           printf("************************************************\n");
        }
        else {
           printf("************************************************\n");
           printf(" - TESTING DGESVD .. M < N .. FAILED !\n");
           printf("************************************************\n");
        }
    }

fin:
    if ( A2 != NULL ) free(A2);
    if ( U  != NULL ) free(U);
    if ( VT != NULL ) free(VT);
    free(A1); free(S1); free(S2);
    PLASMA_Dealloc_Handle_Tile(&T);

    return 0;
}

/*-------------------------------------------------------------------
 * Check the orthogonality of U VT
 */
static int check_orthogonality(int side, int M, int N, double *Q, int LDQ, double eps)
{
    double  done =  1.0;
    double  mdone  = -1.0;
    double  normQ, result;
    int     info_ortho;
    int     minMN = min(M, N);
    double *work = (double *)malloc(minMN*sizeof(double));

    /* Build the idendity matrix */
    double *Id = (double *) malloc(minMN*minMN*sizeof(double));
    LAPACKE_dlaset_work(LAPACK_COL_MAJOR, 'A', minMN, minMN, 0., 1., Id, minMN);

    /* Perform Id - Q'Q */
    if (M >= N)
        cblas_dsyrk(CblasColMajor, CblasUpper, CblasTrans, N, M, done, Q, LDQ, mdone, Id, N);
    else
        cblas_dsyrk(CblasColMajor, CblasUpper, CblasNoTrans,   M, N, done, Q, LDQ, mdone, Id, M);

    normQ = LAPACKE_dlansy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), 'U', minMN, Id, minMN, work);

    if (getenv("PLASMA_TESTING_VERBOSE"))
        printf( "||Q||_oo=%f\n", normQ );

    result = normQ / (M * eps);
    if (side == PlasmaLeft)
    {
        printf(" ======================================================\n");
        printf(" ||Id-U'*U||_oo / (M*eps)          : %15.3E \n",  result );
        printf(" ======================================================\n");
    }
    else
    {
        printf(" ======================================================\n");
        printf(" ||Id-VT'*VT||_oo / (M*eps)          : %15.3E \n",  result );
        printf(" ======================================================\n");
    }

    if ( isnan(result) || isinf(result) || (result > 60.0) ) {
        printf("-- Orthogonality is suspicious ! \n");
        info_ortho = 1;
    }
    else {
        printf("-- Orthogonality is CORRECT ! \n");
        info_ortho = 0;
    }

    free(work); free(Id);

    return info_ortho;
}

/*------------------------------------------------------------
 *  Check the bidiagonal reduction
 */
static int check_reduction(int M, int N, double *S, double *A, int LDA,
                           double *U, int LDU, double *VT, int LDVT, double eps )
{
    double zone = 1.0;
    double mzone  = -1.0;
    double Anorm, Rnorm, result;
    int info_reduction;
    int i;
    int maxMN = max(M, N);
    int minMN = min(M, N);

    double *TEMP     = (double *)malloc(maxMN*maxMN*sizeof(double));
    double *Residual = (double *)malloc(maxMN*maxMN*sizeof(double));
    double *work = (double *)malloc(maxMN*sizeof(double));

    memset((void*)TEMP, 0, maxMN*maxMN*sizeof(double));

    /* Compute TEMP =  U * SIGMA */
    /*
     * U is of size (M,M),
     * SIGMA is of size (M,N) but contains only MINMN singular values.
     * Thus TEMP is of size (M,N)
     */
    LAPACKE_dlacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower), M, M, U, LDU, TEMP, M);
    for (i = 0; i < minMN; i++){
            cblas_dscal(M, S[i], &(TEMP[i*M]),1);
    }
    /* Compute Residual = A - U * SIGMA * VT */
    /*
     * VT is of size (N,N)
     */
    LAPACKE_dlacpy_work(LAPACK_COL_MAJOR, lapack_const(PlasmaUpperLower), M, N, A, LDA, Residual, M);
    cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M, N, N, (mzone), TEMP, M,  VT, LDVT, (zone), Residual,     M);

    /* Compute the norms */
    Rnorm = LAPACKE_dlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, N, Residual, M,   work);
    Anorm = LAPACKE_dlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), M, N, A,       LDA, work);

    if (getenv("PLASMA_TESTING_VERBOSE"))
        printf( "||A||_oo=%f\n||A - U*B*VT||_oo=%e\n", Anorm, Rnorm );

    result = Rnorm / ( Anorm * maxMN * eps);
    printf(" ======================================================\n");
    printf(" ||A-U*SIGMA*VT'||_oo/(||A||_oo.N.eps) : %15.3E \n",  result );
    printf(" ======================================================\n");

    if ( isnan(result) || isinf(result) || (result > 60.0) ) {
        printf("-- Reduction is suspicious ! \n");
        info_reduction = 1;
    }
    else {
        printf("-- Reduction is CORRECT ! \n");
        info_reduction = 0;
    }

    free(TEMP);
    free(Residual);
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
    printf(" | S - singularcomputed | / (|S| * N * eps) : %15.3E \n",  maxel );
    printf(" ======================================================\n");

    if ( isnan(maxel) || isinf(maxel) || (maxel > 100) ) {
        printf("-- The singular values are suspicious ! \n");
        info_solution = 1;
    }
    else{
        printf("-- The singular values are CORRECT ! \n");
        info_solution = 0;
    }
    return info_solution;
}
