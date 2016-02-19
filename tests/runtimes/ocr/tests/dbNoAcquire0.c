/*
 * This file is subject to the license agreement located in the file LICENSE
 * and cannot be distributed without it. This notice cannot be
 * removed or modified.
 */

#include "ocr.h"

/**
 * DESC: An EDT create a datablock with property DB_PROP_NO_ACQUIRE
 */

#define NB_ELEM 10


ocrGuid_t terminateEdt(u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[]) {
    u64 * dbPtr = depv[0].ptr;
    u64 i = 0;
    while (i < NB_ELEM) {
        ASSERT(dbPtr[i] == i);
        i++;
    }
    ocrShutdown(); // This is the last EDT to execute, terminate
    return NULL_GUID;
}

// Get the DB created in no acquire mode
ocrGuid_t writeEdt(u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[]) {
    // write to it
    ocrGuid_t dbGuid = depv[0].guid;
    u64 * dbPtr = depv[0].ptr;
    u64 i = 0;
    while (i < NB_ELEM) {
        dbPtr[i] = i;
        i++;
    }
    ocrDbRelease(dbGuid);
    ocrGuid_t terminateEdtGuid;
    ocrGuid_t terminateEdtTemplateGuid;
    ocrEdtTemplateCreate(&terminateEdtTemplateGuid, terminateEdt, 0 /*paramc*/, 1 /*depc*/);
    ocrEdtCreate(&terminateEdtGuid, terminateEdtTemplateGuid, 0, NULL, 1, &dbGuid,
                 EDT_PROP_NONE, NULL_GUID, NULL);
    ocrEdtTemplateDestroy(terminateEdtTemplateGuid);
    return NULL_GUID;
}


ocrGuid_t mainEdt(u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[]) {
    ocrGuid_t dbGuid;
    void * dbPtr;
    ocrDbCreate(&dbGuid, &dbPtr, sizeof(u64)*NB_ELEM, DB_PROP_NO_ACQUIRE, NULL_GUID, NO_ALLOC);
    ASSERT(dbPtr == NULL);
    ocrDbRelease(dbGuid); // Do I need to release that or not ?
    ocrGuid_t edtGuid;
    ocrGuid_t edtTplGuid;
    ocrEdtTemplateCreate(&edtTplGuid, writeEdt, 0 /*paramc*/, 1 /*depc*/);
    ocrEdtCreate(&edtGuid, edtTplGuid, EDT_PARAM_DEF, NULL, 1, &dbGuid,
                 EDT_PROP_NONE, NULL_GUID, NULL);
    return NULL_GUID;
}
