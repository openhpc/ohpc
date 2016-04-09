/*
 * This file is subject to the license agreement located in the file LICENSE
 * and cannot be distributed without it. This notice cannot be
 * removed or modified.
 */

#include <ocr.h>

#include "math.h"

bool areSame(float a, float b) {
    return (fabs(a-b) < 1e-9);
}

void ditfft2(float *X_real, float *X_imag, float *x_in, int N, int step) {
    if(N == 1) {
        X_real[0] = x_in[0];
        X_imag[0] = 0;
    } else {
        // DFT even side
        ditfft2(X_real, X_imag, x_in, N/2, 2 * step);
        ditfft2(X_real+N/2, X_imag+N/2, x_in+step, N/2, 2 * step);
        int k;
        for(k=0;k<N/2;k++) {
            float t_real = X_real[k];
            float t_imag = X_imag[k];
            double twiddle_real = cos(-2 * M_PI * k / N);
            double twiddle_imag = sin(-2 * M_PI * k / N);
            float xr = X_real[k+N/2];
            float xi = X_imag[k+N/2];

            // (a+bi)(c+di) = (ac - bd) + (bc + ad)i
            X_real[k] = t_real +
                (twiddle_real*xr - twiddle_imag*xi);
            X_imag[k] = t_imag +
                (twiddle_imag*xr + twiddle_real*xi);
            X_real[k+N/2] = t_real -
                (twiddle_real*xr - twiddle_imag*xi);
            X_imag[k+N/2] = t_imag -
                (twiddle_imag*xr + twiddle_real*xi);
        }
    }
}

// Checks whether the 3 input datablocks (in, X_real and X_imag) match the results
// of a serial FFT.
//
// Params: N, number of elements
//
// Dependencies:    0) x_in
//            1) X_real
//            2) X_imag
//            3) user-defined event, allowing the EDT to be triggered on-demand
ocrGuid_t fftVerifyEdt(u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[]) {
    int i;
    float *data_in;
    float *X_real_other;
    float *X_imag_other;
    // Create new data for duplicating X_real and X_imag's results
    float *X_real;
    float *X_imag;
    u64 N = paramv[0];
    PRINTF("Starting verification. depc:%d\n",depc);
    if(depc > 4) {
        data_in = (float*)depv[0].ptr;
        X_real_other = (float*)depv[1].ptr;
        X_imag_other = (float*)depv[2].ptr;
        X_real = (float*)depv[3].ptr;
        X_imag = (float*)depv[4].ptr;
    } else {
        data_in = (float*)depv[0].ptr;
        X_real_other = data_in+N;
        X_imag_other = X_real_other+N;
        X_real = (float*)depv[1].ptr;
        X_imag = (float*)depv[2].ptr;
    }
    bool intact = true;

    for(i=0;i<N;i++) {
        X_real[i] = 0;
        X_imag[i] = 0;
    }
    PRINTF("Finished initializing.\n");

    // Run ditfft2
    ditfft2(X_real, X_imag, data_in, N, 1);

    PRINTF("Serial transform complete.\n");

    // Verify results
    for(i=0;i<N;i++) {
        if(!areSame(X_real_other[i],X_real[i]) ||
           !areSame(X_imag_other[i],X_imag[i])) {
            intact = false;
            PRINTF("Mismatch at index %d\n",i);
            PRINTF("Expected: %f, %f\n", X_real[i], X_imag[i]);
            PRINTF("Computed: %f, %f\n", X_real_other[i], X_imag_other[i]);
            break;
        }
    }
    if(depc > 4) {
        ocrDbDestroy(depv[3].guid);
        ocrDbDestroy(depv[4].guid);
    } else {
        ocrDbDestroy(depv[1].guid);
        ocrDbDestroy(depv[2].guid);
    }
    if(intact) {
        PRINTF("Program produced correct results.\n");
    } else {
        PRINTF("Program output did not match!\n");
    }
    VERIFY(intact, "Output matched expected results\n");

    return NULL_GUID;
}

// Adds a verify EDT to the dependency tree.
// If x is in one large datablock instead of three small ones,
// pass NULL_GUID for the second and third parameters.
// Returns the guid of the EDT's completion event.
ocrGuid_t setUpVerify(ocrGuid_t inDB, ocrGuid_t XrealDB, ocrGuid_t XimagDB, u64 N, ocrGuid_t trigger) {
    ocrGuid_t vGuid,vEventGuid,vTemp, XrealVDB, XimagVDB;
    ocrEdtTemplateCreate(&vTemp, &fftVerifyEdt, 1, EDT_PARAM_UNK);
    // OCR demands the second parameter be non-null
    float *X;

    ocrDbCreate(&XrealVDB, (void**)&X, sizeof(float)*N, 0, NULL_GUID, NO_ALLOC);
    ocrDbCreate(&XimagVDB, (void**)&X, sizeof(float)*N, 0, NULL_GUID, NO_ALLOC);

    if(XimagDB == NULL_GUID) {
        ocrGuid_t verifyDependencies[4] = { inDB, XrealVDB, XimagVDB, trigger };

        ocrEdtCreate(&vGuid, vTemp, EDT_PARAM_DEF, &N, 4, verifyDependencies,
                     EDT_PROP_NONE, NULL_GUID, &vEventGuid);
    } else {
        ocrGuid_t verifyDependencies[6] = { inDB, XrealDB, XimagDB, XrealVDB,
                                            XimagVDB, trigger };

        ocrEdtCreate(&vGuid, vTemp, EDT_PARAM_DEF, &N, 6, verifyDependencies,
                     EDT_PROP_NONE, NULL_GUID, &vEventGuid);
    }
    ocrEdtTemplateDestroy(vTemp);

    return vEventGuid;
}
