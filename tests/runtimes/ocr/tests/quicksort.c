#include <ocr.h>


#define CACHE_LINE_SIZE 64
//Size of array to be sorted
#define ARRAY_SIZE 1000
//Range of numbers to be sorted.
#define RANGE 1000000

//Pseudo-RNG.  Gets rid of C stdlib dependence
int getRandNum(int seed){
    int MAX = 1000;
    int i;
    int r[MAX];
    int ret;

    r[0] = seed;
    for(i=1; i<31; i++){
        r[i] = (16807LL * r[i-1]) % 2147483647;
        if (r[i] < 0){
            r[i] += 2147483647;
        }
    }
    for(i=31; i<34; i++){
        r[i] = r[i-31];
    }
    for(i=34; i<344; i++){
        r[i] = r[i-31] + r[i-3];
    }
    for(i=344; i<MAX; i++){
        r[i] = r[i-31] + r[i-3];
        ret = ((unsigned int)r[i]) >> 1;
    }

    return ret;
}

//Insertion sort for very small problem sizes that don't need parallelized
//DSS: fixed error (tracking jmin to swap correct elements)
void sortSerial(u64 *data, u64 low, u64 high)
{
    u64 min, i, j, temp, jmin;
    for(i = low; i <= high-1; i++) {
        min = 0xFFFFFFFFFFFFFFFFUL;
        for(j = i; j <=high; j++)
            if(data[j] < min){
                min = data[j];
                jmin = j;
            }

        temp = data[i];
        data[i] = min;
        data[jmin] = temp;
    }
}

// param 0: low index (inclusive)
// param 1: high index (inclusive)
// param 2: qsort edt template
// dep 0: array
ocrGuid_t qsortTask( u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[])
{
    u64 i;
    u64 low = paramv[0];
    u64 high = paramv[1];
    u64 size = high - low + 1;
    ocrGuid_t qsortTemplate = paramv[2];
    u64 *data = depv[0].ptr;
    if(size * sizeof(u64) <= CACHE_LINE_SIZE)
        sortSerial(data, low, high);
    else {

//Set pivot point. The pivot is randomly selected.
//Below: (size/2) is an arbitrary number, as getRandNum requires a seed.

        u64 pivotIndex = low + (getRandNum(size/2))%(high-low);
        u64 pivot = data[pivotIndex];

        // partition
        u64 curIndex = low, swapIndex = high-1;
        u64 temp;
        data[pivotIndex] = data[high];
        data[high] = pivot;

//Find something smaller and larger than pivot to swap
//DSS: modiefied to search from both ends.  Previous was correct but inefficient

        while(1) {
//look for soemthing bigger
            while((data[curIndex] <= pivot) && (curIndex < swapIndex)) curIndex++;
            if(curIndex == swapIndex) break;
//look for soemthing smaller
            while((data[swapIndex] >= pivot) && (curIndex < swapIndex)) swapIndex--;
            if(curIndex == swapIndex) break;
//swap
            temp = data[swapIndex];
            data[swapIndex] = data[curIndex];
            data[curIndex] = temp;
            curIndex++;
        }


//swap and reset pivot index
        data[high] = data[swapIndex];
        data[swapIndex] = pivot;
        pivotIndex = swapIndex;

        // recursively create EDTs and quicksort the high/low partitioned subarrays.
        ocrGuid_t qsortLowEdt, qsortHighEdt;
        ocrGuid_t qsortLowDataEvt, qsortHighDataEvt;

        ocrEventCreate(&qsortLowDataEvt, OCR_EVENT_ONCE_T, EVT_PROP_NONE);
        ocrEventCreate(&qsortHighDataEvt, OCR_EVENT_ONCE_T, EVT_PROP_NONE);

//DSS: removed unnecessary if test
//        if(pivotIndex <=1){
//            u64 qsortLowParamv[3] = {low, pivotIndex+1, qsortTemplate};
//            ocrEdtCreate(&qsortLowEdt, qsortTemplate, EDT_PARAM_DEF, qsortLowParamv,
//                 EDT_PARAM_DEF, &qsortLowDataEvt, EDT_PROP_FINISH, NULL_GUID, NULL);
//        }else{
        u64 qsortLowParamv[3] = {low, pivotIndex-1, qsortTemplate};
        ocrEdtCreate(&qsortLowEdt, qsortTemplate, EDT_PARAM_DEF, qsortLowParamv,
                 EDT_PARAM_DEF, &qsortLowDataEvt, EDT_PROP_FINISH, NULL_GUID, NULL);

 //       }
        u64 qsortHighParamv[3] = {pivotIndex+1, high, qsortTemplate};
        ocrEdtCreate(&qsortHighEdt, qsortTemplate, EDT_PARAM_DEF, qsortHighParamv,
                 EDT_PARAM_DEF, &qsortHighDataEvt, EDT_PROP_FINISH, NULL_GUID, NULL);

        ocrEventSatisfy(qsortLowDataEvt, depv[0].guid);
        ocrEventSatisfy(qsortHighDataEvt, depv[0].guid);
    }

    return NULL_GUID;
}

//Print validation feedback and quit.
ocrGuid_t finishTask( u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[])
{
    PRINTF("Showing first 30  elements: \n");
    u64 i;
    u64 *data = depv[0].ptr;
    for(i = 0; i < 30; i++)
        PRINTF("%lu \n", data[i]);

    PRINTF("Sorting Finished.  Shutting Down OCR\n");
    ocrShutdown();

    return NULL_GUID;
}


ocrGuid_t mainEdt( u32 paramc, u64* paramv, u32 depc, ocrEdtDep_t depv[])
{
    ocrGuid_t qsortTemplate;
    ocrGuid_t qsortEdt;
    ocrGuid_t dataDb;
    ocrGuid_t outEvt;
    u64 *data;
    ocrEdtTemplateCreate(&qsortTemplate, qsortTask, 3, 1);

    ocrDbCreate(&dataDb, (void**)&data, sizeof(u64) * (ARRAY_SIZE),
        /*flags=*/0, /*location=*/NULL_GUID, NO_ALLOC);

    u64 i;
    for(i = 0; i < ARRAY_SIZE; i++)
        data[i] = getRandNum(i) % RANGE;

    u64 qsortParamv[3] = {0, ARRAY_SIZE-1, qsortTemplate};
    ocrEdtCreate(&qsortEdt, qsortTemplate, EDT_PARAM_DEF, qsortParamv,
        EDT_PARAM_DEF, &dataDb, EDT_PROP_FINISH, NULL_GUID, &outEvt);

    ocrGuid_t finishTemplate;
    ocrGuid_t finishEdt;
    u64 finishParamv = ARRAY_SIZE;
    u64 finishDepv[2] = {dataDb, outEvt};
    ocrEdtTemplateCreate(&finishTemplate, finishTask, 1, 2);
    ocrEdtCreate(&finishEdt, finishTemplate, EDT_PARAM_DEF, &finishParamv,
        EDT_PARAM_DEF, finishDepv, 0, NULL_GUID, NULL);
}
