#include "ocr.h"

ocrGuid_t mainEdt ( u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[]) {
    PRINTF("Hello from mainEdt()\n");
    ocrShutdown();
    return 0;
}
