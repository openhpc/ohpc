#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <assert.h>

#include "util.h"
#include "core/transforms/adios_transforms_hooks_read.h"
#include "core/transforms/adios_transforms_reqgroup.h"

#ifdef APLOD

#include <stdint.h>

#include "aplod.h"
#include "adios_transform_identity_read.h"

#define MAX_COMPONENTS 16
#define MAX_SEEK_SIEVE_SIZE 1048576    // The maximum number of bytes sieving is allowed to sieve over per seek

typedef struct {
    int numComponents;
    int32_t components[MAX_COMPONENTS];
} aplod_meta_t;

void parse_aplod_meta(const void *transform_metadata_void, aplod_meta_t *metaout) {
	const char *transform_metadata = (const char*)transform_metadata_void;
    transform_metadata += sizeof (uint64_t);

    metaout->numComponents = *(int8_t*)transform_metadata;
    transform_metadata += sizeof(int8_t);

    memcpy(metaout->components, transform_metadata, metaout->numComponents * sizeof(int32_t));
    transform_metadata += metaout->numComponents * sizeof(int32_t);
}

typedef struct {
    uint64_t numElements;
    uint64_t startOff;

    int numComponentsToUse;

    char *outputBuf;
} aplod_read_meta_t;

int adios_transform_aplod_is_implemented (void) {return 1;}

int adios_transform_aplod_generate_read_subrequests(adios_transform_read_request *reqgroup,
                                                    adios_transform_pg_read_request *pg_reqgroup)
{
    int i;

    // Retrieve APLOD metadata, determine how many components to use
    aplod_meta_t aplodmeta;
    parse_aplod_meta(pg_reqgroup->transform_metadata, &aplodmeta);

    int numComponentsToUse;
    if (!reqgroup->read_param || strcmp(reqgroup->read_param, "") == 0)
        numComponentsToUse = aplodmeta.numComponents;
    else
        numComponentsToUse = atoi(reqgroup->read_param);

    assert(numComponentsToUse > 0);
    int totalComponentsSize = 0;
    for (i = 0; i < numComponentsToUse; i++)
        totalComponentsSize += aplodmeta.components[i];

    //
    const int fulltypelen = adios_get_type_size(reqgroup->transinfo->orig_type, NULL);
    const uint64_t totalElementsInPG = pg_reqgroup->raw_var_length / fulltypelen; // We can do this because APLOD leaves PG size unchanged
    uint64_t start_off = 0, end_off = totalElementsInPG;

    char *buf;

    // Compute some sieving parameters to check if it makes sense
    compute_sieving_offsets_for_pg_selection(pg_reqgroup->pg_intersection_sel, &pg_reqgroup->pg_bounds_sel->u.bb, &start_off, &end_off);
    const uint64_t sieveElemCount = end_off - start_off;
    const uint64_t maxSeekSize = (totalElementsInPG - sieveElemCount) * fulltypelen;

    // Determine if we should sieve or not. Current heuristic: only if using 1 component (sieving is always better in this case)
    if (numComponentsToUse == 1 || maxSeekSize > MAX_SEEK_SIEVE_SIZE) {
        // Don't sieve: read only necessary segments
        buf = malloc(sieveElemCount * totalComponentsSize);

        int numByteColsDone = 0;

        // Read the element start_pos..end_pos sieving segment from each column
        for (i = 0; i < numComponentsToUse; i++) {
            adios_transform_raw_read_request *subreq = adios_transform_raw_read_request_new_byte_segment(
                    pg_reqgroup,
                    totalElementsInPG * numByteColsDone + start_off * aplodmeta.components[i],
                    sieveElemCount * aplodmeta.components[i],
                    buf + sieveElemCount * numByteColsDone);
            adios_transform_raw_read_request_append(pg_reqgroup, subreq);
            numByteColsDone += aplodmeta.components[i];
        }
    } else {
        // Sieve: read the whole PG
        start_off = 0;
        end_off = totalElementsInPG;

        // Don't sieve
        buf = malloc(totalElementsInPG * totalComponentsSize);

        adios_transform_raw_read_request *subreq = adios_transform_raw_read_request_new_byte_segment(
                pg_reqgroup,
                0,
                totalElementsInPG * totalComponentsSize,
                buf);
        adios_transform_raw_read_request_append(pg_reqgroup, subreq);
    }

    // Set up some temp metadata to help us remember stuff for when the data is available
    aplod_read_meta_t *arm = (aplod_read_meta_t*)malloc(sizeof(aplod_read_meta_t));
    *arm = (aplod_read_meta_t){ .numElements = end_off - start_off, .outputBuf = buf,
                                .startOff = start_off, .numComponentsToUse = numComponentsToUse };
    pg_reqgroup->transform_internal = arm; // Store it for later use

    return 0;
}

// Do nothing for individual subrequest
adios_datablock * adios_transform_aplod_subrequest_completed(adios_transform_read_request *reqgroup,
                                                            adios_transform_pg_read_request *pg_reqgroup,
                                                            adios_transform_raw_read_request *completed_subreq)
{
    return NULL;
}



adios_datablock * adios_transform_aplod_pg_reqgroup_completed(adios_transform_read_request *reqgroup,
                                                             adios_transform_pg_read_request *completed_pg_reqgroup)
{
    uint32_t elementSize = adios_get_type_size(reqgroup->transinfo->orig_type, "");

    aplod_read_meta_t *arm = (aplod_read_meta_t *)completed_pg_reqgroup->transform_internal;
    completed_pg_reqgroup->transform_internal = NULL;
    uint64_t raggedOffset = arm->startOff;
    void *compressed_buff = arm->outputBuf;

    uint32_t numElements = arm->numElements;
    uint64_t decompressed_len = numElements * elementSize;
    void* decompressed_buff = malloc (decompressed_len);

    int8_t numComponents = 0;
    int32_t *componentVector = 0;

    aplod_meta_t aplodmeta;
    parse_aplod_meta(completed_pg_reqgroup->transform_metadata, &aplodmeta);

    APLODConfig_t *config;

    if (reqgroup->transinfo->orig_type == adios_double) {
        config = APLODConfigure (aplodmeta.components, aplodmeta.numComponents, APLOD_DOUBLE, APLOD_LITTLE_E);
    } else if (reqgroup->transinfo->orig_type == adios_real){
        config = APLODConfigure (aplodmeta.components, aplodmeta.numComponents, APLOD_FLOAT, APLOD_LITTLE_E);
    }

    // config->blockLengthElts = numElements; // Bug workaround, disable chunking
    config->blockLengthElts = (numElements >= 65536 ? 65536: numElements);

    APLODReconstructComponents  (config,
                                    numElements,
                                    0,
                                    arm->numComponentsToUse, //aplodmeta.numComponents,
                                    1, // use mask
                                    0, // mask = 0
                                    decompressed_buff,
                                    compressed_buff
                                );

    APLODDestroy(config);

    // Clear the buffer pointers for all raw read requests, because they all point
    // to the same buffer, and would be free'd by the framework if we didn't clear here
    adios_transform_raw_read_request *rrr = completed_pg_reqgroup->subreqs;
    for (; rrr; rrr = rrr->next)
        rrr->data = NULL;

    // Free the actual buffer, and the special metadata container we added
    free (arm->outputBuf);
    free (arm);

    return adios_datablock_new_ragged_offset(reqgroup->transinfo->orig_type,
                                             completed_pg_reqgroup->timestep,
                                             completed_pg_reqgroup->pg_writeblock_sel,
                                             raggedOffset,
                                             decompressed_buff);
}

adios_datablock * adios_transform_aplod_reqgroup_completed(adios_transform_read_request *completed_reqgroup)
{
    return NULL;
}


#else

DECLARE_TRANSFORM_READ_METHOD_UNIMPL(aplod);

#endif

