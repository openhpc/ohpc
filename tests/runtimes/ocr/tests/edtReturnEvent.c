/*
 * This file is subject to the license agreement located in the file LICENSE
 * and cannot be distributed without it. This notice cannot be
 * removed or modified.
 */
#include "ocr.h"
/**
 * DESC: Test using a future Event GUID as an EDT return value
 */

#define N 8

ocrGuid_t consumer(u32 paramc, u64 *paramv, u32 depc, ocrEdtDep_t *depv) {
    int i, *ptr = (int*)depv[0].ptr;
    for(i = 0; i < N; i++) ASSERT(N-i == ptr[i]);
    PRINTF("Everything went OK\n");
    ocrShutdown();
    return NULL_GUID;
}
ocrGuid_t producer2(u32 paramc, u64 *paramv, u32 depc, ocrEdtDep_t *depv) {
    ocrGuid_t db_guid;
    int i, *ptr;
    ocrDbCreate(&db_guid, (void **)&ptr, sizeof(*ptr)*N,
                /*flags=*/DB_PROP_NONE, /*location=*/NULL_GUID, NO_ALLOC);
    for(i = 0; i < N; i++) ptr[i] = N - i;
    return db_guid;
}
ocrGuid_t producer1(u32 paramc, u64 *paramv, u32 depc, ocrEdtDep_t *depv) {
    ocrGuid_t producer_template, producer_edt, producer_done_event;
    ocrEdtTemplateCreate(&producer_template, producer2, 0, 1);
    // Here we create a sticky event to bridge the once event
    // of producer being satisfied and destroyed before we
    // could have returned its guid.
    ocrGuid_t complete_event;
    ocrEventCreate(&complete_event,OCR_EVENT_ONCE_T, EVT_PROP_NONE);
    ocrEdtCreate(&producer_edt, producer_template, 0, NULL, 1, NULL,
                 EDT_PROP_NONE, NULL_GUID, &producer_done_event);
    ocrAddDependence(producer_done_event, complete_event, 0, DB_MODE_CONST);
    ocrAddDependence(NULL_GUID, producer_edt, 0, DB_MODE_CONST);
    return complete_event;
    // return producer_done_event; // WRONG: race condition !
}
ocrGuid_t mainEdt(u32 paramc, u64 *paramv, u32 depc, ocrEdtDep_t *depv) {
    ocrGuid_t producer_template, consumer_template;
    ocrGuid_t producer_edt, consumer_edt;
    ocrGuid_t producer_done_event;
    // The ready_event ensures the producer edt doesn't run before we
    // can add a dependence to its output event
    ocrGuid_t ready_event;
    ocrEventCreate(&ready_event,OCR_EVENT_ONCE_T, EVT_PROP_NONE);
    ocrEdtTemplateCreate(&producer_template, producer1, 0, 1);
    ocrEdtTemplateCreate(&consumer_template, consumer , 0, 1);
    ocrEdtCreate(&producer_edt, producer_template, 0, NULL, 1, &ready_event,
                 EDT_PROP_NONE, NULL_GUID, &producer_done_event);
    ocrEdtCreate(&consumer_edt, consumer_template, 0, NULL, 1, NULL,
                 EDT_PROP_NONE, NULL_GUID, NULL);
    // create consumer dependency on producer output
    ocrAddDependence(producer_done_event,consumer_edt,0,DB_MODE_CONST);
    ocrEventSatisfy(ready_event, NULL_GUID);
    return NULL_GUID;
}
