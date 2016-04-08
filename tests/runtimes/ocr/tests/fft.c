/*
 * This file is subject to the license agreement located in the file LICENSE
 * and cannot be distributed without it. This notice cannot be
 * removed or modified.
 */

// OCR implementation of the Cooley-Tukey algorithm. Same as
// naive-parallel.c, but recursive creation of StartEDTs stops once
// the matrix size reaches serialBlockSize. For these small matrices ditfft2
// is called to compute the answer serially. This is meant to minimize the overhead
// of creating EDTs while still maximizing parallelism.
//
// EndEDTs are also changed to divide their work to a number of slave EDTs, such that
// each slave handles serialBlockSize elements.
//

#include "ocr.h"

#include "math.h"
#include "stdlib.h"
#include "macros.h"

#define SERIAL_BLOCK_SIZE_DEFAULT (1024*16)

extern void ditfft2(float *X_real, float *X_imag, float *x_in, u32 N, u32 step);
extern ocrGuid_t setUpVerify(ocrGuid_t inDB, ocrGuid_t XrealDB, ocrGuid_t XimagDB, u64 N, ocrGuid_t trigger);

typedef struct {
    ocrGuid_t startTempGuid;
    ocrGuid_t endTempGuid;
    ocrGuid_t endSlaveTempGuid;
    u64 N;
    u64 verbose;
    u64 serialBlockSize;
}iterationPRM_t;

typedef struct {
    ocrGuid_t startTempGuid;
    ocrGuid_t endTempGuid;
    ocrGuid_t endSlaveTempGuid;
    u64 N;
    u64 stepSize;
    u64 offset;
    u64 x_in_offset;
    u64 serialBlockSize;
}startPRM_t;

typedef struct {
    ocrGuid_t startTempGuid;
    ocrGuid_t endTempGuid;
    ocrGuid_t endSlaveTempGuid;
    u64 N;
    u64 stepSize;
    u64 offset;
    u64 x_in_offset;
    u64 serialBlockSize;
}endPRM_t;

typedef struct {
    u64 N;
    u64 step;
    u64 offset;
    u64 kstart;
    u64 kend;
}endSlavePRM_t;

typedef struct {
    u64 N;
    u64 verbose;
    u64 printResults;
    ocrGuid_t startTempGuid;
    ocrGuid_t endTempGuid;
    ocrGuid_t endSlaveTempGuid;
}printPRM_t;

// Performs one entire iteration of FFT.
// These are meant to be chained serially for timing and testing.
ocrGuid_t fftIterationEdt(u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[]) {

    iterationPRM_t *iterationParamvIn = (iterationPRM_t *)paramv;

    ocrGuid_t startTempGuid = iterationParamvIn->startTempGuid;
    ocrGuid_t endTempGuid = iterationParamvIn->endTempGuid;
    ocrGuid_t endSlaveTempGuid = iterationParamvIn->endSlaveTempGuid;
    u64 N = iterationParamvIn->N;
    bool verbose = iterationParamvIn->verbose;
    u64 serialBlockSize = iterationParamvIn->serialBlockSize;
    if(verbose) {
        PRINTF("Creating iteration child\n");
    }

    startPRM_t startParamv;

    startParamv.startTempGuid = startTempGuid;
    startParamv.endTempGuid = endTempGuid;
    startParamv.endSlaveTempGuid = endSlaveTempGuid;
    startParamv.N = N;
    startParamv.stepSize = 1;
    startParamv.offset = 0;
    startParamv.x_in_offset = 0;
    startParamv.serialBlockSize = serialBlockSize;

    ocrGuid_t dependencies[1] = { depv[0].guid };

    ocrGuid_t edtGuid;
    ocrEdtCreate(&edtGuid, startTempGuid, EDT_PARAM_DEF, (u64 *)&startParamv, 1,
                 dependencies, EDT_PROP_FINISH, NULL_HINT, NULL);

    return NULL_GUID;
}

ocrGuid_t fftStartEdt(u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[]) {
    u32 i;
    startPRM_t *startParamvIn = (startPRM_t *)paramv;

    ocrGuid_t startGuid = startParamvIn->startTempGuid;
    ocrGuid_t endGuid = startParamvIn->endTempGuid;
    ocrGuid_t endSlaveGuid = startParamvIn->endSlaveTempGuid;
    float *data = (float*)depv[0].ptr;
    ocrGuid_t dataGuid = depv[0].guid;
    u64 N = startParamvIn->N;;
    u64 step = startParamvIn->stepSize;
    u64 offset = startParamvIn->offset;
    u64 x_in_offset = startParamvIn->x_in_offset;
    u64 serialBlockSize = startParamvIn->serialBlockSize;
    float *x_in = (float*)data;
    float *X_real = (float*)(data+offset + N*step);
    float *X_imag = (float*)(data+offset + 2*N*step);

    PRINTF("Step %d offset: %d N*step: %d\n", step, offset, N*step);

    startPRM_t childParamv;
    startPRM_t childParamv2;

    if(N <= serialBlockSize) {
        ditfft2(X_real, X_imag, x_in+x_in_offset, N, step);
    } else {

        // DFT even side
        childParamv.startTempGuid = startGuid;
        childParamv.endTempGuid = endGuid;
        childParamv.endSlaveTempGuid = endSlaveGuid;
        childParamv.N = N/2;
        childParamv.stepSize = 2*step;
        childParamv.offset = 0 + offset;
        childParamv.x_in_offset = x_in_offset;
        childParamv.serialBlockSize = serialBlockSize;

        childParamv2.startTempGuid = startGuid;
        childParamv2.endTempGuid = endGuid;
        childParamv2.endSlaveTempGuid = endSlaveGuid;
        childParamv2.N = N/2;
        childParamv2.stepSize = 2 * step;
        childParamv2.offset = N/2 + offset;
        childParamv2.x_in_offset = x_in_offset + step;
        childParamv2.serialBlockSize = serialBlockSize;

        PRINTF("Creating children of size %d\n",N/2);
        ocrGuid_t edtGuid, edtGuid2, endEdtGuid, finishEventGuid, finishEventGuid2;

        ocrEdtCreate(&edtGuid, startGuid, EDT_PARAM_DEF, (u64 *)&childParamv,
                     EDT_PARAM_DEF, NULL, EDT_PROP_FINISH, NULL_HINT,
                     &finishEventGuid);
        ocrEdtCreate(&edtGuid2, startGuid, EDT_PARAM_DEF, (u64 *)&childParamv2,
                     EDT_PARAM_DEF, NULL, EDT_PROP_FINISH, NULL_HINT,
                     &finishEventGuid2);
            PRINTF("finishEventGuid after create: 0x"GUIDF"\n", GUIDA(finishEventGuid));

        ocrGuid_t endDependencies[3] = { dataGuid, finishEventGuid, finishEventGuid2 };
        // Do calculations after having divided and conquered
        ocrEdtCreate(&endEdtGuid, endGuid, EDT_PARAM_DEF, paramv, 3,
                     endDependencies, EDT_PROP_FINISH, NULL_HINT, NULL);

        ocrAddDependence(dataGuid, edtGuid, 0, DB_MODE_RW);
        ocrAddDependence(dataGuid, edtGuid2, 0, DB_MODE_RW);
    }

        PRINTF("Task with size %d completed\n",N);
    return NULL_GUID;
}

ocrGuid_t fftEndEdt(u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[]) {
    u32 i;
    endPRM_t *endParamvIn = (endPRM_t *)paramv;

    ocrGuid_t startGuid = endParamvIn->startTempGuid;
    ocrGuid_t endGuid = endParamvIn->endTempGuid;
    ocrGuid_t endSlaveGuid = endParamvIn->endSlaveTempGuid;
    float *data = (float*)depv[0].ptr;
    ocrGuid_t dataGuid = depv[0].guid;
    u64 N = endParamvIn->N;
    u64 step = endParamvIn->stepSize;
    u64 offset = endParamvIn->offset;
    u64 serialBlockSize = endParamvIn->serialBlockSize;
    float *x_in = (float*)data+offset;
    float *X_real = (float*)(data+offset + N*step);
    float *X_imag = (float*)(data+offset + 2*N*step);

        PRINTF("Reached end phase for step %d\n",step);
    u64 *slaveParamv;

    if(N/2 > serialBlockSize) {
        ocrGuid_t slaveGuids[(N/2)/serialBlockSize];
        u64 slaveParamv[5 * (N/2)/serialBlockSize];

            PRINTF("Creating %d slaves for N=%d\n",(N/2)/serialBlockSize,N);

        for(i=0;i<(N/2)/serialBlockSize;i++) {
            endSlavePRM_t slaveParamv;

            slaveParamv.N = N;
            slaveParamv.step = step;
            slaveParamv.offset = offset;
            slaveParamv.kstart = i*serialBlockSize;
            slaveParamv.kend = (i+1)*serialBlockSize;

            ocrEdtCreate(slaveGuids+i, endSlaveGuid, EDT_PARAM_DEF,
                         (u64 *)&slaveParamv, EDT_PARAM_DEF, &dataGuid,
                         EDT_PROP_NONE, NULL_HINT, NULL);
        }
    } else {
        ocrGuid_t slaveGuids[1];
        endSlavePRM_t slaveParamv;

        slaveParamv.N = N;
        slaveParamv.step = step;
        slaveParamv.offset = offset;
        slaveParamv.kstart = 0;
        slaveParamv.kend = N/2;

        ocrEdtCreate(slaveGuids, endSlaveGuid, EDT_PARAM_DEF, (u64 *)&slaveParamv,
                     EDT_PARAM_DEF, &dataGuid, EDT_PROP_NONE, NULL_HINT, NULL);
    }
    return NULL_GUID;
}
ocrGuid_t fftEndSlaveEdt(u32 paramc, u64 *paramv, u32 depc, ocrEdtDep_t depv[]) {
    u32 i;
    float *data = (float*)depv[0].ptr;
    ocrGuid_t dataGuid = depv[0].guid;

    endSlavePRM_t *slaveParamvIn = (endSlavePRM_t *)paramv;

    u64 N = slaveParamvIn->N;
    u64 step = slaveParamvIn->step;
    u64 offset = slaveParamvIn->offset;
    float *x_in = (float*)data+offset;
    float *X_real = (float*)(data+offset + N*step);
    float *X_imag = (float*)(data+offset + 2*N*step);
    u64 kStart = slaveParamvIn->kstart;
    u64 kEnd = slaveParamvIn->kend;

    u32 k;
    for(k=kStart;k<kEnd;k++) {
        float t_real = X_real[k];
        float t_imag = X_imag[k];
        double twiddle_real;
        double twiddle_imag;
        twiddle_imag = sin(-2 * M_PI * k / N);
        twiddle_real = cos(-2 * M_PI * k / N);
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

    return NULL_GUID;
}

ocrGuid_t finalPrintEdt(u32 paramc, u64 *paramv, u32 depc, ocrEdtDep_t depv[]) {

    printPRM_t *printParamvIn = (printPRM_t *)paramv;

    u32 i;
    float *data = (float*)depv[1].ptr;
    ocrGuid_t dataGuid = depv[1].guid;
    u64 N = printParamvIn->N;
    bool verbose = printParamvIn->verbose;
    bool printResults = printParamvIn->printResults;
    float *x_in = (float*)data;
    float *X_real = (float*)(data + N);
    float *X_imag = (float*)(data + 2*N);

    if(verbose) {
        PRINTF("Final print EDT\n");
    }

    if(printResults) {
        PRINTF("Starting values:\n");
        for(i=0;i<N;i++) {
            PRINTF("%d [ %f ]\n",i,x_in[i]);
        }
        PRINTF("\n");

        PRINTF("Final result:\n");
        for(i=0;i<N;i++) {
            PRINTF("%d [%f + %fi]\n",i,X_real[i],X_imag[i]);
        }
    }
    ocrDbDestroy(dataGuid);

    ocrGuid_t startTempGuid = printParamvIn->startTempGuid;
    ocrGuid_t endTempGuid = printParamvIn->endTempGuid;
    ocrGuid_t endSlaveTempGuid = printParamvIn->endSlaveTempGuid;
    ocrEdtTemplateDestroy(startTempGuid);
    ocrEdtTemplateDestroy(endTempGuid);
    ocrEdtTemplateDestroy(endSlaveTempGuid);

PRINTF("FFT calling shutdown\n");
    ocrShutdown();
    return NULL_GUID;
}

bool parseOptions(u32 argc, char **argv, u64 *N, bool *verify, u64 *iterations,
                  bool *verbose, bool *printResults, u64 *serialBlockSize) {
  char c;
  char *buffer = NULL;

  if (argc != 2) {
    PRINTF("Need one argument 'power'\n");
    return false;
  }

  *verify = false;
  *verbose = false;
  *printResults = false;
  *N = 1;
  *iterations = 1;

  s64 power = atoi(argv[1]);
  PRINTF("Power %ld\n", power);
  while(power-- > 0) *N=(*N)*2;
  *verbose = true;
  *verify = true;
  return true;
}


ocrGuid_t mainEdt(u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[]) {
    u64 argc = getArgc(depv[0].ptr);
    u32 i;
    char *argv[argc];

    for(i=0;i<argc;i++) {
        argv[i] = getArgv(depv[0].ptr,i);
    }

    u64 N;
    u64 iterations;
    bool verify;
    bool verbose;
    bool printResults;
    u64 serialBlockSize = SERIAL_BLOCK_SIZE_DEFAULT;
    if(!parseOptions(argc, argv, &N, &verify, &iterations, &verbose, &printResults,
                     &serialBlockSize)) {
        ocrShutdown();
        return NULL_GUID;
    }

    iterationPRM_t iterationParamv;
    startPRM_t startParamv;
    endPRM_t endParamv;
    endSlavePRM_t endSlaveParamv;
    printPRM_t printParamv;

    ocrGuid_t iterationTempGuid,startTempGuid,endTempGuid,printTempGuid,endSlaveTempGuid;
    ocrEdtTemplateCreate(&iterationTempGuid, &fftIterationEdt, PRMNUM(iteration), 2);
    ocrEdtTemplateCreate(&startTempGuid, &fftStartEdt, PRMNUM(start), 1);
    ocrEdtTemplateCreate(&endTempGuid, &fftEndEdt, PRMNUM(end), 3);
    ocrEdtTemplateCreate(&endSlaveTempGuid, &fftEndSlaveEdt, PRMNUM(endSlave), 1);
    ocrEdtTemplateCreate(&printTempGuid, &finalPrintEdt, PRMNUM(print), 2);

    // x_in, X_real, and X_imag in a contiguous block
    float *x;
    ocrGuid_t dataGuid;
    // TODO: OCR cannot handle large datablocks
    ocrDbCreate(&dataGuid, (void **) &x, sizeof(float) * N * 3, 0, NULL_HINT, NO_ALLOC);
    if(verbose) {
        PRINTF("Datablock of size %lu (N=%lu) created\n",sizeof(float)*N*3,N);
    }

    for(i=0;i<N;i++) {
        x[i] = 0;
    }
    x[1] = 1;
    //x[3] = -3;
    //x[4] = 8;
    //x[5] = 9;
    //x[6] = 1;

    iterationParamv.startTempGuid = startTempGuid;
    iterationParamv.endTempGuid = endTempGuid;
    iterationParamv.endSlaveTempGuid = endSlaveTempGuid;
    iterationParamv.N = N;
    iterationParamv.verbose = verbose;
    iterationParamv.serialBlockSize = serialBlockSize;

    ocrGuid_t edtGuid, printEdtGuid, edtEventGuid;

    if(iterations!=1) {
        PRINTF(">1 iterations currently not supported, dialing down to 1 iteration\n");
    }

    ocrEdtCreate(&edtGuid, iterationTempGuid, EDT_PARAM_DEF, (u64 *)&iterationParamv,
                 EDT_PARAM_DEF, NULL, EDT_PROP_FINISH, NULL_HINT,
                 &edtEventGuid);
    ocrEdtTemplateDestroy(iterationTempGuid);

    if(verify) {
        edtEventGuid = setUpVerify(dataGuid, NULL_GUID, NULL_GUID, N, edtEventGuid);
    }

    printParamv.N = N;
    printParamv.verbose = verbose;
    printParamv.printResults = printResults;
    printParamv.startTempGuid = startTempGuid;
    printParamv.endTempGuid = endTempGuid;
    printParamv.endSlaveTempGuid = endSlaveTempGuid;

    ocrGuid_t finishDependencies[2] = { edtEventGuid, dataGuid };
    ocrEdtCreate(&printEdtGuid, printTempGuid, EDT_PARAM_DEF, (u64 *)&printParamv,
                 EDT_PARAM_DEF, finishDependencies, EDT_PROP_NONE, NULL_HINT, NULL);
    ocrEdtTemplateDestroy(printTempGuid);

    edtEventGuid = NULL_GUID;
    ocrAddDependence(dataGuid, edtGuid, 0, DB_MODE_RW);
    ocrAddDependence(edtEventGuid, edtGuid, 1, DB_MODE_CONST);

    return NULL_GUID;
}
