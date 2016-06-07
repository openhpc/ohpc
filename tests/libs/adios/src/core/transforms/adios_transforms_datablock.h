/*
 * adios_transforms_datablock.h
 *
 * An ADIOS transform datablock represents a chunk of data that is retrieved by a transform plugin during reading.
 * This block may include exactly the data that was requested, or it may contain more (the excess will be trimmed).
 * For instance, suppose a 10x10 block of a PG was requested, but the transform method can only retrieve data
 * in 16x16 blocks. Instead of reading a 16x16 block and performing the trimming within the transform plugin,
 * the plugin may simply read the 16x16 block and return it immediately, leaving the trimming to the framework.
 *
 *  Created on: Dec 11, 2012
 *      Author: David A. Boyuka II
 */

#ifndef ADIOS_TRANSFORMS_DATABLOCK_H_
#define ADIOS_TRANSFORMS_DATABLOCK_H_

#include "public/adios_types.h"
#include <core/transforms/adios_transforms_reqgroup.h>

typedef struct {
    int timestep;                  // timestep of this datablock
    const ADIOS_SELECTION *bounds; // (global) selection describing the data contained in this datablock
    uint64_t ragged_offset;        // Ragged array offset of the data buffer in this datablock
                                   // (only applicable for bounding-box-type selections)

    enum ADIOS_DATATYPES elem_type;// Datatype of the elements in this datablock
    void *data;                    // Pointer to the data buffer of this datablock
} adios_datablock;

// Datablock management
adios_datablock * adios_datablock_new_whole_pg(
		const adios_transform_read_request *read_req,
        const adios_transform_pg_read_request *pg_read_req,
        void *data);

adios_datablock * adios_datablock_new(
        enum ADIOS_DATATYPES elem_type,
        int timestep,
        const ADIOS_SELECTION *bounds,
        void *data);

adios_datablock * adios_datablock_new_ragged(
        enum ADIOS_DATATYPES elem_type,
        int timestep,
        const ADIOS_SELECTION *bounds,
        const uint64_t *ragged_offsets, void *data);

adios_datablock * adios_datablock_new_ragged_offset(
        enum ADIOS_DATATYPES elem_type,
        int timestep,
        const ADIOS_SELECTION *bounds,
        uint64_t ragged_offset, void *data);

void adios_datablock_free(adios_datablock **datablock, int free_data);

#endif /* ADIOS_TRANSFORMS_DATABLOCK_H_ */
