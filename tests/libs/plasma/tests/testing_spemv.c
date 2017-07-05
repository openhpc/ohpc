/**
 *
 * @file testing_spemv.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Dulceneia Becker
 * @date 2011-10-06
 * @generated s Fri Apr  1 11:03:06 2016
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

#define REAL
#undef COMPLEX

/*--------------------------------------------------------------
 * Check the pemv
 */
static int check_solution(PLASMA_enum trans, PLASMA_enum storev, 
                          int M, int N, int L,
                          float alpha, float *A, int LDA,
                                                    float *X, int INCX,
                          float beta,  float *Y0, int INCY0,
                                                    float *Y,  int INCY,
                          float *W, float *Rnorm)
{
    int k;
    float eps = LAPACKE_slamch_work('e');
    float *work;
    float mzone = -1.0;

    /* Copy x to w */
    if ( trans == PlasmaNoTrans ) {
        k = N;
    } else {
        k = M;
    }
    
    work = (float *)malloc(k * sizeof(float));
    cblas_scopy(k, Y0, INCY0, W, 1);
    
    /* w = a A x + b w */
    cblas_sgemv(CblasColMajor, (CBLAS_TRANSPOSE)trans, 
                M, N,
                (alpha), A,  LDA, 
                                    X,  INCX,
                (beta),  W,  1);

    /* y - w */
    cblas_saxpy(k, (mzone), Y, INCY, W, 1);
    
    /* Max Norm */
    *Rnorm = LAPACKE_slange_work(LAPACK_COL_MAJOR, 'm', 1, k, W, 1, work);
    
    if ( (*Rnorm / (M*N)) > eps) {
        return 1;
    } else {
        return 0;
    }
}

/*--------------------------------------------------------------
 * Testing SPEMV
 */
int testing_spemv(int argc, char **argv)
{
    /* Check for number of arguments*/
    if ( argc != 1) {
        USAGE("PEMV", "N",
              "   - N      : number of columns\n");
        return -1;
    }

    /* Args */
    int arg_n = atoi(argv[0]);

    /* Local variables */
    float *A, *X, *Y, *A0, *Y0, *work;
    float alpha, beta, alpha0, beta0;
    int n    = arg_n;
    int lda  = arg_n;

    int info_solution = 0;
    int i, j, k, t;
    int nbtests = 0;
    int nfails = 0;
    int storev;
    int l = 0;
    int m = n;
    int incx = 1;
    int incy = 1;
    char *cstorev;
    float rnorm;
    float eps = LAPACKE_slamch_work('e');

    /* Allocate Data */
    A    = (float *)malloc(lda*n*sizeof(float));
    A0   = (float *)malloc(lda*n*sizeof(float));
    X    = (float *)malloc(lda*n*sizeof(float));
    Y    = (float *)malloc(lda*n*sizeof(float));
    Y0   = (float *)malloc(    n*sizeof(float));
    work = (float *)malloc(  2*n*sizeof(float));

    LAPACKE_slarnv_work(1, ISEED, 1, &alpha0);
    LAPACKE_slarnv_work(1, ISEED, 1, &beta0 );

    /* Check if unable to allocate memory */
    if ( (!A) || (!X) || (!Y0) || (!work) ) {
        printf("Out of Memory \n ");
        exit(0);
    }
    
     /* Initialize Data */
    PLASMA_splrnt(n, n, A,  lda, 479 );
    PLASMA_splrnt(n, n, X,  lda, 320 );
    PLASMA_splrnt(n, 1, Y0, n,   573 );

    printf("\n");
    printf("------ TESTS FOR PLASMA SPEMV ROUTINE -------  \n");
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf(" The relative machine precision (eps) is %e \n",eps);
    printf(" Computational tests pass if scaled residual is less than eps.\n");
    printf("\n");

    nfails = 0;
    for (i=0; i<6; i++) {
        
        /* m and n cannot be greater than lda (arg_n) */
        switch (i) {
        case 0: l = 0;       m = arg_n;   n = m;        break;
        case 1: l = 0;       m = arg_n;   n = arg_n/2;  break; /**/
        case 2: l = arg_n;   m = l;       n = l;        break;
        case 3: l = arg_n/2; m = l;       n = arg_n;    break;
        case 4: l = arg_n/2; m = arg_n-l; n = l;        break;
        case 5: l = arg_n/3; m = arg_n-l; n = arg_n/2;  break; /**/
        }

        /* Colwise ConjTrans & Rowwise NoTrans */
#ifdef COMPLEX
        for (t=0; t<3; t++) {
#else
        for (t=0; t<2; t++) {
#endif

            /* Swap m and n for transpose cases */
            if ( t == 1 ) {
                k = m; m = n; n = k;
            }

            LAPACKE_slacpy_work( LAPACK_COL_MAJOR, 'A', m, n, 
                                 A, lda, A0, lda);

            if ( trans[t] == PlasmaNoTrans ) {
                storev = PlasmaRowwise;
                cstorev = storevstr[0];

                /* zeroed the upper right triangle */
                int64_t i, j;
                for (j=(n-l); j<n; j++) {
                    for (i=0; i<(j-(n-l)); i++) {
                        A0[i+j*lda] = 0.0;
                    }
                }
            }
            else {
                storev = PlasmaColumnwise;
                cstorev = storevstr[1];

                /* zeroed the lower left triangle */
                int64_t i, j;
                for (j=0; j<(l-1); j++) {
                    for (i=(m-l+1+j); i<m; i++) {
                        A0[i+j*lda] = 0.0;
                    }
                }
            }

            for (j=0; j<3; j++) {

                /* Choose alpha and beta */
                alpha = ( j==1 ) ? 0.0 : alpha0;
                beta  = ( j==2 ) ? 0.0 : beta0;

                /* incx and incy: 1 or lda */
                for (k=0; k<4; k++) {
                    switch (k) {
                    case 0:  incx = 1;    incy = 1;    break;
                    case 1:  incx = 1;    incy = lda;  break;
                    case 2:  incx = lda;  incy = 1;    break;
                    case 3:  incx = lda;  incy = lda;  break;
                    }
                    
                    /* initialize Y with incy */
                    cblas_scopy(n, Y0, 1, Y, incy);
                    
                    /* SPEMV */
                    CORE_spemv( trans[t], storev, m, n, l, 
                                alpha, A, lda, 
                                       X, incx,
                                beta,  Y, incy, 
                                work);
                    
                    /* Check the solution */
                    info_solution = check_solution(trans[t], storev, 
                                                   m, n, l, 
                                                   alpha, A0,  lda,
                                                          X,   incx, 
                                                   beta,  Y0,  1, 
                                                          Y,   incy, 
                                                   work, &rnorm);
                    
                    if ( info_solution != 0 ) {
                        nfails++;
                        printf("Failed: t=%s, s=%s, M=%3d, N=%3d, L=%3d, alpha=%e, incx=%3d, beta=%e, incy=%3d, rnorm=%e\n", 
                               transstr[t], cstorev, m, n, l, (alpha), incx, (beta), incy, rnorm );
                    }
                    nbtests++;
                }
            }
        }
    }

    if ( nfails )
        printf("%d / %d tests failed\n", nfails, nbtests);

    printf("***************************************************\n");
    if (nfails == 0) {
        printf(" ---- TESTING SPEMV ...... PASSED !\n");
    }
    else {
        printf(" ---- TESTING SPEMV ... FAILED !\n");
    }
    printf("***************************************************\n");

    free( A0 );
    free( A );
    free( X );
    free( Y0 );
    free( Y );

    return 0;
}

