/**
 *
 * @file
 *
 *  PLASMA is a software package provided by:
 *  University of Tennessee, US,
 *  University of Manchester, UK.
 *
 * @generated from test/test_zgesv.c, normal z -> d, Sun Aug 29 23:28:57 2021
 *
 **/

#include "test.h"
#include "flops.h"
#include "plasma.h"
#include <plasma_core_blas.h>
#include "core_lapack.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <omp.h>

#define REAL

#define A(i_, j_) A[(i_) + (size_t)lda*(j_)]

/***************************************************************************//**
 *
 * @brief Tests DGESV.
 *
 * @param[in,out] param - array of parameters
 * @param[in]     run - whether to run test
 *
 * Sets flags in param indicating which parameters are used.
 * If run is true, also runs test and stores output parameters.
 ******************************************************************************/
void test_dgesv(param_value_t param[], bool run)
{
    //================================================================
    // Mark which parameters are used.
    //================================================================
    param[PARAM_DIM    ].used = PARAM_USE_N;
    param[PARAM_NRHS   ].used = true;
    param[PARAM_PADA   ].used = true;
    param[PARAM_PADB   ].used = true;
    param[PARAM_NB     ].used = true;
    param[PARAM_IB     ].used = true;
    param[PARAM_MTPF   ].used = true;
    if (! run)
        return;

    //================================================================
    // Set parameters.
    //================================================================
    int n = param[PARAM_DIM].dim.n;
    int nrhs = param[PARAM_NRHS].i;

    int lda = imax(1, n+param[PARAM_PADA].i);
    int ldb = imax(1, n+param[PARAM_PADB].i);

    int test = param[PARAM_TEST].c == 'y';
    double tol = param[PARAM_TOL].d * LAPACKE_dlamch('E');

    //================================================================
    // Set tuning parameters.
    //================================================================
    plasma_set(PlasmaTuning, PlasmaDisabled);
    plasma_set(PlasmaNb, param[PARAM_NB].i);
    plasma_set(PlasmaIb, param[PARAM_IB].i);
    plasma_set(PlasmaNumPanelThreads, param[PARAM_MTPF].i);

    //================================================================
    // Allocate and initialize arrays.
    //================================================================
    double *A =
        (double*)malloc((size_t)lda*n*sizeof(double));
    assert(A != NULL);

    double *B =
        (double*)malloc(
            (size_t)ldb*nrhs*sizeof(double));
    assert(B != NULL);

    int *ipiv = (int*)malloc((size_t)n*sizeof(int));
    assert(ipiv != NULL);

    int seed[] = {0, 0, 0, 1};
    lapack_int retval;
    retval = LAPACKE_dlarnv(1, seed, (size_t)lda*n, A);
    assert(retval == 0);

    retval = LAPACKE_dlarnv(1, seed, (size_t)ldb*nrhs, B);
    assert(retval == 0);

    double *Aref = NULL;
    double *Bref = NULL;
    double *work = NULL;
    if (test) {
        Aref = (double*)malloc(
            (size_t)lda*n*sizeof(double));
        assert(Aref != NULL);

        Bref = (double*)malloc(
            (size_t)ldb*nrhs*sizeof(double));
        assert(Bref != NULL);

        memcpy(Aref, A, (size_t)lda*n*sizeof(double));
        memcpy(Bref, B, (size_t)ldb*nrhs*sizeof(double));
    }

    //================================================================
    // Run and time PLASMA.
    //================================================================
    plasma_time_t start = omp_get_wtime();
    plasma_dgesv(n, nrhs, A, lda, ipiv, B, ldb);
    plasma_time_t stop = omp_get_wtime();
    plasma_time_t time = stop-start;

    double flops = flops_dgetrf(n, n) + flops_dgetrs(n, nrhs);
    param[PARAM_TIME].d = time;
    param[PARAM_GFLOPS].d = flops / time / 1e9;

    //================================================================
    // Test results by checking the residual
    //
    //                      || B - AX ||_I
    //                --------------------------- < epsilon
    //                 || A ||_I * || X ||_I * N
    //
    //================================================================
    if (test) {
        double zone  =  1.0;
        double zmone = -1.0;

        work = (double*)malloc((size_t)n*sizeof(double));
        assert(work != NULL);

        double Anorm = LAPACKE_dlange_work(
            LAPACK_COL_MAJOR, 'I', n, n, Aref, lda, work);
        double Xnorm = LAPACKE_dlange_work(
            LAPACK_COL_MAJOR, 'I', n, nrhs, B, ldb, work);

        // Bref -= Aref*B
        cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, n, nrhs, n,
                    (zmone), Aref, lda,
                                        B,    ldb,
                    (zone),  Bref, ldb);

        double Rnorm = LAPACKE_dlange_work(
            LAPACK_COL_MAJOR, 'I', n, nrhs, Bref, ldb, work);
        double residual = Rnorm/(n*Anorm*Xnorm);

        param[PARAM_ERROR].d = residual;
        param[PARAM_SUCCESS].i = residual < tol;
    }

    //================================================================
    // Free arrays.
    //================================================================
    free(A);
    free(B);
    free(ipiv);
    if (test) {
        free(Aref);
        free(Bref);
        free(work);
    }
}
