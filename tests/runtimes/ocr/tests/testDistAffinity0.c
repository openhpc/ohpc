/*
 * This file is subject to the license agreement located in the file LICENSE
 * and cannot be distributed without it. This notice cannot be
 * removed or modified.
 */



#include "ocr.h"
#include "extensions/ocr-affinity.h"

/**
 * DESC: OCR-DIST - create an EDT that creates a child EDT in the same affinity group
 */

ocrGuid_t shutdownEdt(u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[]) {
    ocrGuid_t currentAffinity;
    ocrAffinityGetCurrent(&currentAffinity);
    PRINTF("shutdownEdt: executing at %lld\n", (u64) currentAffinity);
    ASSERT(((u64)currentAffinity) == paramv[0]);
    ocrShutdown();
    return NULL_GUID;
}

ocrGuid_t remoteEdt(u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[]) {
    ocrGuid_t currentAffinity;
    ocrAffinityGetCurrent(&currentAffinity);
    PRINTF("remoteEdt: executing at affinity %lld\n", (u64) currentAffinity);
    // Create a new EDT with affinity set to current EDT's affinity
    ocrGuid_t shutdownEdtTemplateGuid;
    ocrEdtTemplateCreate(&shutdownEdtTemplateGuid, shutdownEdt, 1, 0);
    ocrGuid_t edtGuid;
    u64 nparamv = (u64) currentAffinity;
    ocrEdtCreate(&edtGuid, shutdownEdtTemplateGuid, EDT_PARAM_DEF, &nparamv, EDT_PARAM_DEF, NULL,
        EDT_PROP_NONE, currentAffinity, NULL);
    return NULL_GUID;
}

ocrGuid_t mainEdt(u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[]) {
    u64 affinityCount;
    ocrAffinityCount(AFFINITY_PD, &affinityCount);
    ASSERT(affinityCount >= 1);
    ocrGuid_t affinities[affinityCount];
    ocrAffinityGet(AFFINITY_PD, &affinityCount, affinities);
    ocrGuid_t edtAffinity = affinities[affinityCount-1];

    ocrGuid_t remoteEdtTemplateGuid;
    ocrEdtTemplateCreate(&remoteEdtTemplateGuid, remoteEdt, 0, 0);

    PRINTF("mainEdt: create remote EDT\n");
    ocrGuid_t edtGuid;
    ocrEdtCreate(&edtGuid, remoteEdtTemplateGuid, EDT_PARAM_DEF, NULL, EDT_PARAM_DEF, NULL,
        EDT_PROP_NONE, edtAffinity, NULL);

    return NULL_GUID;
}
