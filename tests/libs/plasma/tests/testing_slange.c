/**
 *
 * @file testing_sgemm.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Emmanuel Agullo
 * @author Mathieu Faverge
 * @date 2010-11-15
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

#undef COMPLEX
#define REAL

int testing_slange(int argc, char **argv)
{
    /* Check for number of arguments*/
    if ( argc != 3) {
        USAGE("LANGE", "M N LDA",
              "   - M      : number of rows of matrices A and C\n"
              "   - N      : number of columns of matrices B and C\n"
              "   - LDA    : leading dimension of matrix A\n");
        return -1;
    }

    int M     = atoi(argv[0]);
    int N     = atoi(argv[1]);
    int LDA   = atoi(argv[2]);
    int LDAxN = LDA*N;
    int n, u;
    float eps;

    float *A    = (float *)malloc(LDAxN*sizeof(float));
    float             *work = (float*) malloc(max(M,N)*sizeof(float));
    float normplasma, normlapack, result;

    eps = LAPACKE_slamch_work('e');

    printf("\n");
    printf("------ TESTS FOR PLASMA SLANGE ROUTINE -------  \n");
    printf("            Size of the Matrix %d by %d\n", M, N);
    printf("\n");
    printf(" The matrix A is randomly generated for each test.\n");
    printf("============\n");
    printf(" The relative machine precision (eps) is to be %e \n",eps);
    printf(" Computational tests pass if scaled residuals are less than 10.\n");

    /*----------------------------------------------------------
     *  TESTING SLANGE
     */

    /* Initialize A, B, C */
    PLASMA_splrnt( M, N, A, LDA, 3436 );

    /* PLASMA SLANGE */
    for(n=0; n<4; n++) {
        normplasma = PLASMA_slange(norm[n], M, N, A, LDA);
        normlapack = LAPACKE_slange_work(LAPACK_COL_MAJOR, lapack_const(norm[n]), M, N, A, LDA, work);

        printf("Lapack %e, Plasma %e\n", normlapack, normplasma);

        result = fabs(normplasma - normlapack) / (normlapack * eps);
        switch(norm[n]) {
        case PlasmaMaxNorm:
            /* result should be perfectly equal */
            break;
        case PlasmaInfNorm:
            /* Sum order on the line can differ */
            result = result / (float)N;
            break;
        case PlasmaOneNorm:
            /* Sum order on the column can differ */
            result = result / (float)M;
            break;
        case PlasmaFrobeniusNorm:
            /* Sum oreder on every element can differ */
            result = result / ((float)M * (float)N);
            break;
        }

        printf("***************************************************\n");
        if ( result < 1. ) {
            printf(" ---- TESTING SLANGE (%s)............... PASSED !\n", normstr[n]);
        }
        else {
            printf(" - TESTING SLANGE (%s)... FAILED !\n", normstr[n]);
        }
        printf("***************************************************\n");
    }

    /* PLASMA SLANTR */
    for(n=0; n<4; n++) {
        for(u=0; u<2; u++) {
            int d;
            for(d=0; d<2; d++) {
                normplasma = PLASMA_slantr(norm[n], uplo[u], diag[d], M, N, A, LDA);
                normlapack = LAPACKE_slantr_work(LAPACK_COL_MAJOR, lapack_const(norm[n]), lapack_const(uplo[u]),
                                                 lapack_const(diag[d]), M, N, A, LDA, work);

                printf("Lapack %e, Plasma %e\n", normlapack, normplasma);

                result = fabs(normplasma - normlapack) / (normlapack * eps);
                switch(norm[n]) {
                case PlasmaMaxNorm:
                    /* result should be perfectly equal */
                    break;
                case PlasmaInfNorm:
                    /* Sum order on the line can differ */
                    result = result / (float)N;
                    break;
                case PlasmaOneNorm:
                    /* Sum order on the column can differ */
                    result = result / (float)M;
                    break;
                case PlasmaFrobeniusNorm:
                    /* Sum oreder on every element can differ */
                    result = result / ((float)M * (float)N);
                    break;
                }

                printf("***************************************************\n");
                if ( result < 1. ) {
                    printf(" ---- TESTING SLANTR (%s, %s, %s)......... PASSED !\n",
                           normstr[n], uplostr[u], diagstr[d]);
                }
                else {
                    printf(" - TESTING SLANTR (%s, %s, %s)... FAILED !\n",
                           normstr[n], uplostr[u], diagstr[d]);
                }
                printf("***************************************************\n");
            }
        }
    }

    /* PLASMA SLANSY */
    for(n=0; n<4; n++) {
        for(u=0; u<2; u++) {
            normplasma = PLASMA_slansy(norm[n], uplo[u], min(M,N), A, LDA);
            normlapack = LAPACKE_slansy_work(LAPACK_COL_MAJOR, lapack_const(norm[n]), lapack_const(uplo[u]), min(M,N), A, LDA, work);

            printf("Lapack %e, Plasma %e\n", normlapack, normplasma);

            result = fabs(normplasma - normlapack) / (normlapack * eps);
            switch(norm[n]) {
            case PlasmaMaxNorm:
                /* result should be perfectly equal */
                break;
            case PlasmaInfNorm:
                /* Sum order on the line can differ */
                result = result / (float)N;
                break;
            case PlasmaOneNorm:
                /* Sum order on the column can differ */
                result = result / (float)M;
                break;
            case PlasmaFrobeniusNorm:
                /* Sum oreder on every element can differ */
                result = result / ((float)M * (float)N);
                break;
            }

            printf("***************************************************\n");
            if ( result < 1. ) {
                printf(" ---- TESTING SLANSY (%s, %s)......... PASSED !\n", normstr[n], uplostr[u]);
            }
            else {
                printf(" - TESTING SLANSY (%s, %s)... FAILED !\n", normstr[n], uplostr[u]);
            }
            printf("***************************************************\n");
        }
    }

#ifdef COMPLEX
    /* PLASMA SLANSY */
    {
      int j;
      for (j=0; j<min(M,N); j++) {
        A[j*LDA+j] -= I*cimagf(A[j*LDA+j]);
      }
    }

    for(n=0; n<4; n++) {
        for(u=0; u<2; u++) {
            normplasma = PLASMA_slansy(norm[n], uplo[u], min(M,N), A, LDA);
            normlapack = LAPACKE_slansy_work(LAPACK_COL_MAJOR, lapack_const(norm[n]), lapack_const(uplo[u]), min(M,N), A, LDA, work);

            printf("Lapack %e, Plasma %e\n", normlapack, normplasma);

            result = fabs(normplasma - normlapack) / (normlapack * eps);
            switch(norm[n]) {
            case PlasmaMaxNorm:
                /* result should be perfectly equal */
                break;
            case PlasmaInfNorm:
                /* Sum order on the line can differ */
                result = result / (float)N;
                break;
            case PlasmaOneNorm:
                /* Sum order on the column can differ */
                result = result / (float)M;
                break;
            case PlasmaFrobeniusNorm:
                /* Sum oreder on every element can differ */
                result = result / ((float)M * (float)N);
                break;
            }

            printf("***************************************************\n");
            if ( result < 1. ) {
                printf(" ---- TESTING SLANSY (%s, %s)......... PASSED !\n", normstr[n], uplostr[u]);
            }
            else {
                printf(" - TESTING SLANSY (%s, %s)... FAILED !\n", normstr[n], uplostr[u]);
            }
            printf("***************************************************\n");
        }
    }
#endif

    free(A); free(work);
    return 0;
}
