/*
 * adios_transforms_datablock.c
 *
 *  Created on: Dec 11, 2012
 *      Author: David A. Boyuka II
 */

#include <stdlib.h>
#include <assert.h>

#include "adios_error.h"
#include "adios_types.h"
#include "core/common_read.h"

#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_read.h"
#include "core/transforms/adios_transforms_util.h"
#include "core/transforms/adios_transforms_datablock.h"

// Datablock management

adios_datablock * adios_datablock_new_whole_pg(
		const adios_transform_read_request *read_req,
        const adios_transform_pg_read_request *pg_read_req,
        void *data)
{
	// Return a datablock using the PG's writeblock (this is
	// the most compatible option for processing in the
	// transforms framework)
	return adios_datablock_new(
			read_req->transinfo->orig_type,
			pg_read_req->timestep,
			pg_read_req->pg_writeblock_sel,
			data);
}

adios_datablock * adios_datablock_new(
        enum ADIOS_DATATYPES elem_type,
        int timestep,
        const ADIOS_SELECTION *bounds,
        void *data)
{
    assert(bounds);
    assert(data);
    return adios_datablock_new_ragged_offset(elem_type, timestep, bounds, 0, data);
}

/*
 * Note: only valid for bounding box selections (since there are no ragged
 * arrays for point selections, and no other selection types are supported
 * right now).
 */
adios_datablock * adios_datablock_new_ragged(
        enum ADIOS_DATATYPES elem_type,
        int timestep,
        const ADIOS_SELECTION *bounds,
        const uint64_t *ragged_offsets, void *data) {

    assert(bounds);
    assert(data);
    assert(bounds->type == ADIOS_SELECTION_BOUNDINGBOX);

    const uint64_t ragged_offset = ragged_offsets ?
            compute_linear_offset_in_volume(bounds->u.bb.ndim, ragged_offsets, bounds->u.bb.count) :
            0;

    return adios_datablock_new_ragged_offset(elem_type, timestep, bounds, ragged_offset, data);
}

adios_datablock * adios_datablock_new_ragged_offset(
        enum ADIOS_DATATYPES elem_type,
        int timestep,
        const ADIOS_SELECTION *bounds,
        uint64_t ragged_offset, void *data) {

    assert(bounds);
    assert(data);

    adios_datablock *datablock = malloc(sizeof(adios_datablock));

    datablock->elem_type = elem_type;
    datablock->bounds = copy_selection(bounds);
    datablock->timestep = timestep;
    datablock->ragged_offset = ragged_offset;
    datablock->data = data;

    return datablock;
}

#define MYFREE(p) {if (p) free(p); (p)=NULL;}
void adios_datablock_free(adios_datablock **datablock_ptr, int free_data) {
    adios_datablock *datablock = *datablock_ptr;
    if (datablock) {
        if (datablock->bounds)
            common_read_selection_delete((ADIOS_SELECTION*)datablock->bounds);
        if (free_data)
            MYFREE(datablock->data);
    }
    MYFREE(*datablock_ptr);
}
#undef MYFREE
