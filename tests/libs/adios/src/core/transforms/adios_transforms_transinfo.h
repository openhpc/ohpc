/*
 * adios_transforms_transinfo.h
 *
 *  Created on: Apr 29, 2014
 *      Author: David A. Boyuka II
 */
#ifndef ADIOS_TRANSFORMS_TRANSINFO_H_
#define ADIOS_TRANSFORMS_TRANSINFO_H_

#include <stdint.h>

#include <public/adios_read_v2.h> // Internally, always use V2 structs
#include <public/adios_read_ext.h>

// NCSU ALACRITY-ADIOS - struct for original metadata
typedef struct {
    int transform_type; // type actually "enum ADIOS_TRANSFORM_TYPE", but this type is not accessible outside ADIOS internals

    uint16_t transform_metadata_len;
    void *transform_metadata;
    int should_free_transform_metadata; // Used internally by read method and free

    enum ADIOS_DATATYPES orig_type;

    int orig_ndim;
    uint64_t *orig_dims;

    int orig_global;

    ADIOS_VARBLOCK *orig_blockinfo;

    ADIOS_TRANSFORM_METADATA *transform_metadatas;
} ADIOS_TRANSINFO;

#endif /* ADIOS_TRANSFORMS_TRANSINFO_H_ */
