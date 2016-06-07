/*
 * adios_subvolume.c
 *
 *  Created on: Jul 25, 2012
 *      Author: David A. Boyuka II
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include "core/util.h"
#include "core/common_read.h"
#include "core/adios_subvolume.h"
#include "core/adios_internals.h"

void vector_add(int ndim, uint64_t *dst_vec, const uint64_t *vec1, const uint64_t *vec2) {
    while (ndim--)
        *dst_vec++ = *vec1++ + *vec2++;
}

void vector_sub(int ndim, uint64_t *dst, const uint64_t *vec1, const uint64_t *vec2) {
    while (ndim--)
        *dst++ = *vec1++ - *vec2++;
}

uint64_t compute_volume(int ndim, const uint64_t *dims) {
    uint64_t volume = 1;
    while (ndim) {
        volume *= *dims++;
        ndim--;
    }
    return volume;
}

int intersect_segments(uint64_t start1, uint64_t len1, uint64_t start2, uint64_t len2,
                       uint64_t *inter_start, uint64_t *inter_len) {
    int end1, end2;
    int inter_end;

    // Swap the segments if segment 1 starts after segment 2, to simplify calculations
    if (start1 > start2) {
        int tmps = start1;
        int tmpl = len1;
        start1 = start2;
        len1 = len2;
        start2 = tmps;
        len2 = tmpl;
    }

    end1 = start1 + len1;
    end2 = start2 + len2;

    // If segment 1 ends before segment 2 starts, no intersection
    if (end1 <= start2)
        return 0;

    if (inter_start) {
        *inter_start = start2;                    // Intersection starts at the beginning of the later segment
    }
    if (inter_len) {
        inter_end = end1 <= end2 ? end1 : end2;    // Intersection ends at the earlier of the two segment ends
        *inter_len = inter_end - *inter_start;
    }

    return 1;
}

int intersect_volumes(int ndim,
                      const uint64_t *dims1, const uint64_t *offset1,
                      const uint64_t *dims2, const uint64_t *offset2,
                      uint64_t *inter_dims, uint64_t *inter_offset,
                      uint64_t *inter_offset_rel1, uint64_t *inter_offset_rel2) {
    // For every dimension, find the intersection in that dimension
    // If ever the volumes are disjoint in some dimension, stop immediately
    // and report no intersection
    int dim;
    uint64_t tmp_inter_offset;
    for (dim = 0; dim < ndim; dim++) {
        const int intersects = intersect_segments(*offset1, *dims1,
                                                  *offset2, *dims2,
                                                  &tmp_inter_offset, inter_dims);
        if (!intersects)
            return 0;

        // Calculate/store offsets as the user requests, and then advance to
        // the next dimension on these as well
        if (inter_offset)
            *inter_offset++ = tmp_inter_offset;
        if (inter_offset_rel1)
            *inter_offset_rel1++ = tmp_inter_offset - *offset1;
        if (inter_offset_rel2)
            *inter_offset_rel2++ = tmp_inter_offset - *offset2;

        // Advance the other arrays to the next dimension, as well
        // NOTE: this must be done after offset calculations, as offset[12] are
        // accessed
        offset1++; dims1++;
        offset2++; dims2++;
        inter_dims++;
    }

    // If we have a non-null intersection in every dimension, the entire
    // volumes intersect, so return true to indicate this
    return 1;
}

int intersect_bb(const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb1,
                 const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb2,
                 uint64_t *inter_offset,
                 uint64_t *inter_offset_rel1, uint64_t *inter_offset_rel2,
                 uint64_t *inter_dims) {

    assert(bb1); assert(bb2);
    assert(bb1->ndim == bb2->ndim);
    return intersect_volumes(
            bb1->ndim, bb1->count, bb1->start, bb2->count, bb2->start,
            inter_dims, inter_offset, inter_offset_rel1, inter_offset_rel2);
}

void copy_subvolume_with_spec(void *dst, const void *src,
                              const adios_subvolume_copy_spec *copy_spec,
                              enum ADIOS_DATATYPES datum_type,
                              enum ADIOS_FLAG swap_endianness) {

    copy_subvolume(dst, src, copy_spec->ndim, copy_spec->subv_dims,
                   copy_spec->dst_dims, copy_spec->dst_subv_offsets,
                   copy_spec->src_dims, copy_spec->src_subv_offsets,
                   datum_type, swap_endianness);
}

void copy_subvolume_ragged_with_spec(void *dst, const void *src,
                                     const adios_subvolume_copy_spec *copy_spec,
                                     const uint64_t *dst_ragged_offsets,
                                     const uint64_t *src_ragged_offsets,
                                     enum ADIOS_DATATYPES datum_type,
                                     enum ADIOS_FLAG swap_endianness) {
    copy_subvolume_ragged(dst, src, copy_spec->ndim, copy_spec->subv_dims,
                          copy_spec->dst_dims, copy_spec->dst_subv_offsets,
                          dst_ragged_offsets,
                          copy_spec->src_dims, copy_spec->src_subv_offsets,
                          src_ragged_offsets,
                          datum_type, swap_endianness);
}

void copy_subvolume_ragged_offset_with_spec(void *dst, const void *src,
                                            const adios_subvolume_copy_spec *copy_spec,
                                            uint64_t dst_ragged_offset,
                                            uint64_t src_ragged_offset,
                                            enum ADIOS_DATATYPES datum_type,
                                            enum ADIOS_FLAG swap_endianness) {
    copy_subvolume_ragged_offset(dst, src, copy_spec->ndim, copy_spec->subv_dims,
                                 copy_spec->dst_dims, copy_spec->dst_subv_offsets,
                                 dst_ragged_offset,
                                 copy_spec->src_dims, copy_spec->src_subv_offsets,
                                 src_ragged_offset,
                                 datum_type, swap_endianness);
}


/*
 * copy_subvolume delegates to the copy_subvolume_helper function, with the
 * following parameter translations:
 * 1) Element size (i.e. 8 bytes for double, etc.) is considered an additional,
 *    fastest-varying dimension
 * 2) All "contiguous" fastest-varying dimensions are collapsed into a single
 *    fastest-varying dimension
 */
void copy_subvolume(void *dst, const void *src, int ndim, const uint64_t *subv_dims,
                    const uint64_t *dst_dims, const uint64_t *dst_subv_offsets,
                    const uint64_t *src_dims, const uint64_t *src_subv_offsets,
                    enum ADIOS_DATATYPES datum_type, enum ADIOS_FLAG swap_endianness) {
    copy_subvolume_ragged(dst, src, ndim, subv_dims,
                          dst_dims, dst_subv_offsets, NULL,
                          src_dims, src_subv_offsets, NULL,
                          datum_type, swap_endianness);
}

void copy_subvolume_ragged(void *dst, const void *src, int ndim, const uint64_t *subv_dims,
                           const uint64_t *dst_dims, const uint64_t *dst_subv_offsets,
                           const uint64_t *dst_ragged_offsets,
                           const uint64_t *src_dims, const uint64_t *src_subv_offsets,
                           const uint64_t *src_ragged_offsets,
                           enum ADIOS_DATATYPES datum_type, enum ADIOS_FLAG swap_endianness) {

    const uint64_t total_src_ragged_offset =
            src_ragged_offsets ?
            compute_linear_offset_in_volume(ndim, src_ragged_offsets,
                                        src_dims) : 0;
    const uint64_t total_dst_ragged_offset =
            dst_ragged_offsets ?
            compute_linear_offset_in_volume(ndim, dst_ragged_offsets,
                                        dst_dims) : 0;

    copy_subvolume_ragged_offset(dst, src, ndim, subv_dims,
                                 dst_dims, dst_subv_offsets,
                                 total_dst_ragged_offset,
                                 src_dims, src_subv_offsets,
                                 total_src_ragged_offset,
                                 datum_type, swap_endianness);
}

/*
 * Copies a given subvolume from 'src' to 'dst'. It recursively copies a
 * hyperplane of progressively lower dimensions, until it reaches the lowest
 * dimension, in which case it simply performs a memcpy.
 *
 * At any given call level to this function, 'src' and 'dst' point to the first
 * element of a hyperplane of the subvolume with dimension 'ndim'.
 *
 * @param dst the destination buffer
 * @param src the source buffer
 * @param ndim the number of dimensions
 * @param next_subv_dim the list of dimensions for the subvolume, starting with
 *        the slowest-changing
 * @param next_dst_stride the list of destination buffer strides for each
 *        dimension
 * @param next_src_stride the list of source buffer strides for each dimension
 * @param type the datatype of the elements in dst/src (NOTE: this is ONLY for
 *        the purpose of endianness swapping; strides/dimensions are in bytes)
 * @param swap_endianness if true, swap the endianness of each element
 */
static void copy_subvolume_helper(char *dst, const char *src,
                                  int ndim, const uint64_t *next_subv_dim,
                                  const uint64_t *next_dst_stride, const uint64_t *next_src_stride,
                                  enum ADIOS_DATATYPES buftype, int swap_endianness) {
    if (ndim == 1) {
        memcpy(dst, src, *next_subv_dim);
        if (swap_endianness) {
            change_endianness(dst, *next_subv_dim, buftype);
        }
    } else {
        int i;
        for (i = 0; i < *next_subv_dim; i++) {
            copy_subvolume_helper(dst, src, ndim - 1,
                                  next_subv_dim + 1, next_dst_stride + 1, next_src_stride + 1,
                                  buftype, swap_endianness);

            src += *next_src_stride;
            dst += *next_dst_stride;
        }
    }
}

/*
 * Same as copy_subvolume_helper, but uses memmove instead of memcpy for
 * safe(r) copies on overlapping buffers.
 */
static void copy_subvolume_helper_safe(char *dst, const char *src,
                                       int ndim, const uint64_t *next_subv_dim,
                                       const uint64_t *next_dst_stride, const uint64_t *next_src_stride,
                                        enum ADIOS_DATATYPES buftype, int swap_endianness) {
    if (ndim == 1) {
        memmove(dst, src, *next_subv_dim);
        if (swap_endianness) {
            change_endianness(dst, *next_subv_dim, buftype);
        }
    } else {
        int i;
        for (i = 0; i < *next_subv_dim; i++) {
            copy_subvolume_helper(dst, src, ndim - 1,
                                  next_subv_dim + 1, next_dst_stride + 1, next_src_stride + 1,
                                  buftype, swap_endianness);

            src += *next_src_stride;
            dst += *next_dst_stride;
        }
    }
}

void copy_subvolume_ragged_offset(void *dst, const void *src, int ndim, const uint64_t *subv_dims,
                                  const uint64_t *dst_dims, const uint64_t *dst_subv_offsets,
                                  uint64_t dst_ragged_offset,
                                  const uint64_t *src_dims, const uint64_t *src_subv_offsets,
                                  uint64_t src_ragged_offset,
                                  enum ADIOS_DATATYPES datum_type, enum ADIOS_FLAG swap_endianness) {

    int i;
    int last_noncovering_dim = 0; // Index of dimension that starts a contiguous block
    int buffers_intersect = 0; // Whether the source and destination memory copy regions may intersect
    uint64_t src_strides[32];
    uint64_t dst_strides[32];
    const int type_size = adios_get_type_size(datum_type, NULL);

    // Find the last dimension for which the subvolume, source and destination
    // spaces do not exactly match (i.e. non-zero offset or unequal lengths).
    for (i = 0; i < ndim; i++) {
        // If the subvolume, source and destination do not exactly match along
        // this dimension, it is marked as incomplete
        if (src_subv_offsets[i] != 0 ||
            dst_subv_offsets[i] != 0 ||
            subv_dims[i] != src_dims[i] ||
            subv_dims[i] != dst_dims[i]) {

            last_noncovering_dim = i;
        }
    }

    // Calculate the volume (number of bytes) of the region subtended by the
    // contiguous dimensions (which start with the last non-covering dimension)
    uint64_t contig_dims_volume = 1;
    for (i = last_noncovering_dim; i < ndim; i++) {
        contig_dims_volume *= subv_dims[i];
    }
    // Add the element size as a new "dimension", to convert from element-space
    // to byte-space
    contig_dims_volume *= type_size; // Assumes non-string type

    // Compute strides for the dimensions
    uint64_t src_volume = type_size;
    uint64_t dst_volume = type_size;
    for (i = ndim - 1; i >= 0; i--) {
        src_strides[i] = src_volume;
        dst_strides[i] = dst_volume;

        src_volume *= src_dims[i];
        dst_volume *= dst_dims[i];
    }

    // Compute the starting offsets for src and dst. In theory we could skip
    // the contiguous dimensions other than the first one, but just to be
    // safe/simple we calculate them all.
    uint64_t src_offset = 0, dst_offset = 0;
    for (i = 0; i < ndim; i++) {
        src_offset += src_subv_offsets[i] * src_strides[i];
        dst_offset += dst_subv_offsets[i] * dst_strides[i];
    }
    // Incorporate ragged offsets
    src_offset -= src_ragged_offset * type_size;
    dst_offset -= dst_ragged_offset * type_size;

    // Save the old value of the first contiguous dimension, then replace it
    // with a "collapsed" dimension consolidating all contiguous dimensions
    // into one
    // We "cheat" a bit by removing the const modifier. This is OK because we
    // carefully put back the original value before returning. This is the only
    // argument modification we do in this function besides filling 'dst'.
    uint64_t first_contig_dim_value_old = subv_dims[last_noncovering_dim];
    ((uint64_t*)subv_dims)[last_noncovering_dim] = contig_dims_volume;

    // Compute whether the memory regions for the copy overlap
    // If so, we need to use a safer copy method
    if (0) { // FOR NOW, DON'T DO THIS, AS WE NEVER USE OVERLAPPING BUFFERS
        uint64_t dst_end_offset = dst_offset;
        uint64_t src_end_offset = src_offset;
        // Match the stride counts as executed by the helper function: all but
        // the last dimension contribute (length - 1) strides, with the last
        // dimension contributing instead its length
        for (i = 0; i < last_noncovering_dim; i++) {
            src_end_offset += (subv_dims[i] - 1) * src_strides[i];
            dst_end_offset += (subv_dims[i] - 1) * dst_strides[i];
        }
        src_end_offset += subv_dims[last_noncovering_dim];
        dst_end_offset += subv_dims[last_noncovering_dim];

        buffers_intersect =
                intersect_segments((uint64_t)dst + dst_offset,
                                   dst_end_offset - dst_offset,
                                   (uint64_t)src + src_offset,
                                   src_end_offset - src_offset,
                                   NULL, NULL);

        buffers_intersect = 0; // Look Ma, no asserts!

        // Enfoce the safety condition for overlapping buffers here
        if (buffers_intersect) {
            assert((char*)src + src_offset >= (char*)dst + dst_offset);
            for (i = 0; i < last_noncovering_dim + 1; i++)
                assert(src_strides[i] >= dst_strides[i]);
        }
    }

    //printf(">>> copy_subvolume is using %d contiguous dimensions...\n", ndim - first_contig_dim);

    // Finally, delegate to the recursive worker function
    if (buffers_intersect) {
        copy_subvolume_helper_safe(
                (char*)dst + dst_offset,            /* Offset dst buffer to the first element */
                (char*)src + src_offset,            /* Offset src buffer to the first element */
                last_noncovering_dim + 1,           /* Number of dimensions, modified for collapsed contiguous dimensions */
                subv_dims,                          /* Subvolume dimensions (modified to collapse all contiguous dimensions) */
                dst_strides,                        /* dst buffer dimension strides */
                src_strides,                        /* src buffer dimension strides */
                datum_type,                         /* The datatype of the buffer elements */
                swap_endianness == adios_flag_yes   /* Whether to swap endianness */
        );
    } else {
        copy_subvolume_helper(
                (char*)dst + dst_offset,            /* Offset dst buffer to the first element */
                (char*)src + src_offset,            /* Offset src buffer to the first element */
                last_noncovering_dim + 1,           /* Number of dimensions, modified for collapsed contiguous dimensions */
                subv_dims,                          /* Subvolume dimensions (modified to collapse all contiguous dimensions) */
                dst_strides,                        /* dst buffer dimension strides */
                src_strides,                        /* src buffer dimension strides */
                datum_type,                         /* The datatype of the buffer elements */
                swap_endianness == adios_flag_yes   /* Whether to swap endianness */
        );
    }

    // Restore the old first contiguous dimension value
    ((uint64_t*)subv_dims)[last_noncovering_dim] = first_contig_dim_value_old;
}

uint64_t compute_linear_offset_in_volume(int ndim, const uint64_t *point, const uint64_t *dims) {
    int dim;
    uint64_t ragged_off = 0;
    uint64_t volume_so_far = 1;
    for (dim = ndim - 1; dim >= 0; dim--) {
        ragged_off += point[dim] * volume_so_far;
        volume_so_far *= dims[dim];
    }
    return ragged_off;
}

void compact_subvolume_ragged_offset(void *buf, int ndim, const uint64_t *subv_dims,
                                     const uint64_t *buf_dims, uint64_t buf_ragged_offset,
                                     const uint64_t *buf_subv_offsets,
                                     enum ADIOS_DATATYPES elem_type) {
    int i;
    uint64_t zero[32];

    // Ensure all arguments are non-NULL, and that the subvolume fits
    // completely within the buffer volume
    assert(buf); assert(buf_dims); assert(subv_dims); assert(buf_subv_offsets);
    for (i = 0; i < ndim; i++)
        assert(buf_dims[i] >= subv_dims[i] + buf_subv_offsets[i]);

    memset(zero, 0, ndim * sizeof(uint64_t));

    adios_subvolume_copy_spec *compact_copyspec = malloc(sizeof(adios_subvolume_copy_spec));
    adios_copyspec_init(compact_copyspec, ndim, subv_dims,
                        subv_dims, zero,
                        buf_dims, buf_subv_offsets);

    // If the compact operation will do something, do it now
    if (!adios_copyspec_is_noop(compact_copyspec)) {
        // Overlapping subvolume copy allowed because it matches the safety
        // condition as defined in comment at the top of adios_subvolume.h.
        // NOTE: we infer no endianness swap, as this is an intra-buffer operation
        copy_subvolume_ragged_offset_with_spec(
                buf, buf, compact_copyspec,
                0, buf_ragged_offset,
                elem_type, adios_flag_no);
    }

    // We use the arguments and a stack array as buffers; don't free them
    adios_copyspec_free(&compact_copyspec, 0);
}

ADIOS_SELECTION * new_derelativized_selection(const ADIOS_SELECTION *sel, const uint64_t *sel_global_offset) {
    ADIOS_SELECTION *new_sel;
    switch (sel->type) {
    case ADIOS_SELECTION_BOUNDINGBOX:
    {
        const int ndim = sel->u.bb.ndim;
        const int dimsize = ndim * sizeof(uint64_t);
        uint64_t * const new_start = malloc(dimsize);
        uint64_t * const new_count = bufdup(sel->u.bb.count, sizeof(uint64_t), ndim);

        // Add the global offset to the bounding box start
        vector_add(ndim, new_start, sel->u.bb.start, sel_global_offset);

        new_sel = common_read_selection_boundingbox(ndim, new_start, new_count);
        break;
    }
    case ADIOS_SELECTION_POINTS:
    {
        const int ndim = sel->u.points.ndim;
        const uint64_t npoints = sel->u.points.npoints;
        uint64_t * const new_points = malloc(npoints * ndim * sizeof(uint64_t));

        uint64_t i;
        const uint64_t *cur_src_point = sel->u.points.points;
        uint64_t *cur_dst_point = new_points;

        // Add the global offset to each point
        for (i = 0; i < npoints; i++) {
            vector_add(ndim, cur_dst_point, cur_src_point, sel_global_offset);
            cur_src_point += ndim;
            cur_dst_point += ndim;
        }

        new_sel = common_read_selection_points(ndim, npoints, new_points);
        break;
    }
    case ADIOS_SELECTION_WRITEBLOCK:
    case ADIOS_SELECTION_AUTO:
    default:
        fprintf(stderr, "Internal error: attempt to call %s on a selection of type %d, but only BOUNDINGBOX (%d) and POINTS (%d) are supported.\n",
                __FUNCTION__, sel->type, ADIOS_SELECTION_BOUNDINGBOX, ADIOS_SELECTION_POINTS);
        assert(0);
        break;
    }

    return new_sel;
}

ADIOS_SELECTION * varblock_to_bb(int ndim, const ADIOS_VARBLOCK *vb) {
    return common_read_selection_boundingbox(ndim,
                                             bufdup(vb->start, sizeof(uint64_t), ndim),
                                             bufdup(vb->count, sizeof(uint64_t), ndim));
}

uint64_t compute_selection_size(const ADIOS_SELECTION *sel) {
    uint64_t sel_size;
    switch (sel->type) {
    case ADIOS_SELECTION_BOUNDINGBOX:
    {
        const int ndim = sel->u.bb.ndim;
        const uint64_t * const sel_count = sel->u.bb.count;
        int i;

        sel_size = 1;
        for (i = 0; i < ndim; i++)
            sel_size *= sel_count[i];

        break;
    }
    case ADIOS_SELECTION_POINTS:
    {
        sel_size = sel->u.points.npoints;
        break;
    }
    case ADIOS_SELECTION_WRITEBLOCK:
    case ADIOS_SELECTION_AUTO:
    default:
        fprintf(stderr, "Internal error: attempt to call %s on a selection of type %d, but only BOUNDINGBOX (%d) and POINTS (%d) are supported.\n",
                __FUNCTION__, sel->type, ADIOS_SELECTION_BOUNDINGBOX, ADIOS_SELECTION_POINTS);
        assert(0);
        break;
    }

    return sel_size;
}
