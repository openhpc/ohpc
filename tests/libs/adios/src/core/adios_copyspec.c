/*
 * adios_copyspec.c
 *
 *
 *  Created on: Aug 2, 2012
 *      Author: David A. Boyuka II
 */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include "util.h"
#include "core/adios_copyspec.h"
#include "core/adios_subvolume.h"
#include "core/common_read.h"
#include "adios_selection.h"

#define MYFREE(x) {if (x) free((void*)x);}

//
// Init and free
//
static void adios_copyspec_init_from_bufs(adios_subvolume_copy_spec *copy_spec,
                                          int ndim, const uint64_t *subv_dims,
                                          const uint64_t *dst_dims,
                                          const uint64_t *dst_subv_offsets,
                                          const uint64_t *src_dims,
                                          const uint64_t *src_subv_offsets) {
    const int dimsize = ndim * sizeof(uint64_t);
    copy_spec->ndim             = ndim;
    copy_spec->subv_dims        = subv_dims        ? bufdup(subv_dims, 1, dimsize)        : malloc(dimsize);
    copy_spec->dst_dims         = dst_dims         ? bufdup(dst_dims, 1, dimsize)         : malloc(dimsize);
    copy_spec->dst_subv_offsets = dst_subv_offsets ? bufdup(dst_subv_offsets, 1, dimsize) : malloc(dimsize);
    copy_spec->src_dims         = src_dims         ? bufdup(src_dims, 1, dimsize)         : malloc(dimsize);
    copy_spec->src_subv_offsets = src_subv_offsets ? bufdup(src_subv_offsets, 1, dimsize) : malloc(dimsize);
}

void adios_copyspec_init(adios_subvolume_copy_spec *copy_spec,
                         int ndim, const uint64_t *subv_dims,
                         const uint64_t *dst_dims, const uint64_t *dst_subv_offsets,
                         const uint64_t *src_dims, const uint64_t *src_subv_offsets) {
    assert(ndim > 0 && subv_dims &&
           dst_dims && dst_subv_offsets &&
           src_dims && src_subv_offsets);
    copy_spec->ndim              = ndim;
    copy_spec->subv_dims         = subv_dims;
    copy_spec->dst_dims          = dst_dims;
    copy_spec->dst_subv_offsets  = dst_subv_offsets;
    copy_spec->src_dims          = src_dims;
    copy_spec->src_subv_offsets  = src_subv_offsets;
}

int adios_copyspec_init_from_intersection(adios_subvolume_copy_spec *copy_spec, int ndim,
                                          const uint64_t *dst_dims, const uint64_t *dst_goffsets,
                                          const uint64_t *src_dims, const uint64_t *src_goffsets) {
    // Initialize with number of dimensions and source/destination dimensions
    // Offsets and subvolume dimension are unknown at this time
    // Arg order:
    // copyspec, ndim, subv_dims
    // dst_dims, dst_subv_offsets
    // src_dims, src_subv_offsets
    adios_copyspec_init_from_bufs(copy_spec, ndim, NULL,
                                  dst_dims, NULL,
                                  src_dims, NULL);

    uint64_t *subv_dims = malloc(ndim * sizeof(uint64_t));
    uint64_t *dst_subv_offsets = malloc(ndim * sizeof(uint64_t));
    uint64_t *src_subv_offsets = malloc(ndim * sizeof(uint64_t));

    // Compute the intersection to compute in the rest of the information
    const int intersects =
        intersect_volumes(ndim, dst_dims, dst_goffsets, src_dims, src_goffsets,
                          subv_dims, NULL,
                          dst_subv_offsets, src_subv_offsets);

    if (intersects) {
    	copy_spec->subv_dims = subv_dims;
    	copy_spec->dst_subv_offsets = dst_subv_offsets;
    	copy_spec->src_subv_offsets = src_subv_offsets;
    } else {
    	MYFREE(subv_dims);
    	MYFREE(dst_subv_offsets);
    	MYFREE(src_subv_offsets);
    }

    return intersects;
}

int adios_copyspec_init_from_bb_intersection(adios_subvolume_copy_spec *copy_spec,
                                             const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *dst_bb,
                                             const uint64_t *src_dims, const uint64_t *src_goffsets) {

    assert(dst_bb); assert(src_dims); assert(src_goffsets);
    return adios_copyspec_init_from_intersection(copy_spec, dst_bb->ndim,
                                                 dst_bb->count, dst_bb->start,
                                                 src_dims, src_goffsets);
}

int adios_copyspec_init_from_2bb_intersection(adios_subvolume_copy_spec *copy_spec,
                                              const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *dst_bb,
                                              const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *src_bb) {

    assert(dst_bb); assert(src_bb); assert(dst_bb->ndim == src_bb->ndim);
    return adios_copyspec_init_from_intersection(copy_spec, dst_bb->ndim,
                                                 dst_bb->count, dst_bb->start,
                                                 src_bb->count, src_bb->start);
}


void adios_copyspec_free(adios_subvolume_copy_spec **copy_spec_ptr, int free_buffers) {
    adios_subvolume_copy_spec *copy_spec = *copy_spec_ptr;
    if (free_buffers) {
        MYFREE(copy_spec->subv_dims);
        MYFREE(copy_spec->dst_dims);
        MYFREE(copy_spec->dst_subv_offsets);
        MYFREE(copy_spec->src_dims);
        MYFREE(copy_spec->src_subv_offsets);
    }
    memset(copy_spec, 0, sizeof(adios_subvolume_copy_spec));

    MYFREE(*copy_spec_ptr);
}

//
// Derivative copyspec functions
//

void adios_copyspec_copy(adios_subvolume_copy_spec *dst_copy_spec, const adios_subvolume_copy_spec *src_copy_spec) {
    // Arg order:
    // copyspec, ndim, subv_dims
    // dst_dims, dst_subv_offsets
    // src_dims, src_subv_offsets
    adios_copyspec_init_from_bufs(dst_copy_spec, src_copy_spec->ndim, src_copy_spec->subv_dims,
                                  src_copy_spec->dst_dims, src_copy_spec->dst_subv_offsets,
                                  src_copy_spec->src_dims, src_copy_spec->src_subv_offsets);
}

adios_subvolume_copy_spec * adios_copyspec_dup(const adios_subvolume_copy_spec *copy_spec) {
    adios_subvolume_copy_spec *new_copy_spec = malloc(sizeof(adios_subvolume_copy_spec));
    adios_copyspec_copy(new_copy_spec, copy_spec);
    return new_copy_spec;
}

void adios_copyspec_shrink_dst_to_subv(adios_subvolume_copy_spec * dst_copy_spec, const adios_subvolume_copy_spec *src_copy_spec) {
    const int ndim = src_copy_spec->ndim;

    // Arg order:
    // copyspec, ndim, subv_dims
    // dst_dims, dst_subv_offsets
    // src_dims, src_subv_offsets
    adios_copyspec_init_from_bufs(dst_copy_spec, ndim, src_copy_spec->subv_dims,
                                  src_copy_spec->subv_dims, calloc(ndim, sizeof(uint64_t)),
                                  src_copy_spec->src_dims, src_copy_spec->src_subv_offsets);
}

void adios_copyspec_shrink_src_to_subv(adios_subvolume_copy_spec * dst_copy_spec, const adios_subvolume_copy_spec *src_copy_spec) {
    const int ndim = src_copy_spec->ndim;

    // Arg order:
    // copyspec, ndim, subv_dims
    // dst_dims, dst_subv_offsets
    // src_dims, src_subv_offsets
    adios_copyspec_init_from_bufs(dst_copy_spec, ndim, src_copy_spec->subv_dims,
                                  src_copy_spec->dst_dims, src_copy_spec->dst_subv_offsets,
                                  src_copy_spec->subv_dims, calloc(ndim, sizeof(uint64_t)));
}

//
// Inspection/calculations with copyspecs
//

int adios_copyspec_is_subvolume_src_covering(const adios_subvolume_copy_spec *subv_spec) {
    int dim;
    for (dim = 0; dim < subv_spec->ndim; dim++) {
        if (subv_spec->src_subv_offsets[dim] != 0 ||
            subv_spec->src_dims[dim] != subv_spec->subv_dims[dim])
            return 0;
    }
    return 1;
}

int adios_copyspec_is_subvolume_dst_covering(const adios_subvolume_copy_spec *subv_spec) {
    int dim;
    for (dim = 0; dim < subv_spec->ndim; dim++) {
        if (subv_spec->dst_subv_offsets[dim] != 0 ||
            subv_spec->dst_dims[dim] != subv_spec->subv_dims[dim])
            return 0;
    }
    return 1;
}

int adios_copyspec_is_noop(const adios_subvolume_copy_spec *copy_spec) {
    const int dimsize = copy_spec->ndim * sizeof(uint64_t);
    int dim;

    // If the source, destination and subvolume dimensions are not all equal,
    // this is not a no-op
    if (memcmp(copy_spec->src_dims, copy_spec->dst_dims, dimsize) != 0 ||
        memcmp(copy_spec->src_dims, copy_spec->subv_dims, dimsize) != 0)
        return 0;

    // If the subvolume offsets within the source and destination volumes do
    // not match, this is not a no-op
    for (dim = 0; dim < copy_spec->ndim; dim++) {
        if (copy_spec->dst_subv_offsets[dim] != 0 ||
            copy_spec->src_subv_offsets[dim] != 0)
            return 0;
    }

    // We pass all tests; this is a no-op
    return 1;
}

// Extracts a selection corresponding to the subvolume within the source buffer
ADIOS_SELECTION * adios_copyspec_to_src_selection(adios_subvolume_copy_spec *copy_spec) {
    return common_read_selection_boundingbox(copy_spec->ndim,
                                             bufdup(copy_spec->src_subv_offsets, sizeof(uint64_t), copy_spec->ndim),
                                             bufdup(copy_spec->subv_dims, sizeof(uint64_t), copy_spec->ndim));
}

// Extracts a selection corresponding to the subvolume within the destination buffer
ADIOS_SELECTION * adios_copyspec_to_dst_selection(adios_subvolume_copy_spec *copy_spec) {
    return common_read_selection_boundingbox(copy_spec->ndim,
                                             bufdup(copy_spec->dst_subv_offsets, sizeof(uint64_t), copy_spec->ndim),
                                             bufdup(copy_spec->subv_dims, sizeof(uint64_t), copy_spec->ndim));
}
