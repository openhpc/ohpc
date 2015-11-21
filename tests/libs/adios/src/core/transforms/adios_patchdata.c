/*
 * adios_patchdata.c
 *
 * Provides a main function, adios_patch_data, which copies all relevant data
 * from one buffer/selection to another buffer/selection. It supports various
 * combinations of source and destination selection types. In order to
 * minimize implementation, though, it classifies the current selection types
 * as follows:
 *
 * > Global geometric:
 *   > Bounding box
 *   > Points
 * > Local PG
 *   > Writeblock
 * > Other
 *   > Auto
 *
 * Patching is only supported between two selections within the same class.
 *
 *  Created on: Jan 15, 2013
 *      Author: David A. Boyuka II
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include "adios_error.h"
#include "adios_selection.h"
#include "core/common_read.h"
#include "core/adios_subvolume.h"
#include "core/adios_internals.h" // adios_get_type_size()
#include "core/adios_selection_util.h"
#include "core/transforms/adios_patchdata.h"

#define PATCH_UNIMPL(dsttype,srctype) \
    adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, \
                        "Patching of data from '%s' selection to '%s' selection not currently supported", \
                        srctype, dsttype);

// One-to-one global patch functions
inline static uint64_t adios_patch_data_bb_to_bb(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *dst_bb,
                                                 void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *src_bb,
                                                 enum ADIOS_DATATYPES datum_type,
                                                 enum ADIOS_FLAG swap_endianness) {

    const int ndim = dst_bb->ndim;
    const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *inter_bb;
    uint64_t *inter_off_relative_to_dst;
    uint64_t *inter_off_relative_to_src;
    uint64_t volume;

    // Intersect the two bounding boxes
    ADIOS_SELECTION *inter_sel = adios_selection_intersect_bb_bb(dst_bb, src_bb);

    // If there is no intersection, stop now, nothing to do
    if (!inter_sel)
        return 0;

    // (this is guaranteed by the selection intersection code; this is just to check for bugs)
    assert(inter_sel->type == ADIOS_SELECTION_BOUNDINGBOX);
    inter_bb = &inter_sel->u.bb;

    // Compute the offset of the intersection bounding box within each of
    // the source and destination bounding boxes
    assert(dst_bb->ndim == src_bb->ndim);
    inter_off_relative_to_dst = malloc(ndim * sizeof(uint64_t));
    inter_off_relative_to_src = malloc(ndim * sizeof(uint64_t));
    vector_sub(ndim, inter_off_relative_to_dst, inter_bb->start, dst_bb->start);
    vector_sub(ndim, inter_off_relative_to_src, inter_bb->start, src_bb->start);

    // Perform a subvolume memcpy
    copy_subvolume_ragged_offset(
        dst, src, dst_bb->ndim, inter_bb->count,
        dst_bb->count, inter_off_relative_to_dst, dst_ragged_offset,
        src_bb->count, inter_off_relative_to_src, src_ragged_offset,
        datum_type, swap_endianness);

    // Compute the number of elements copied
    volume = compute_volume(ndim, inter_bb->count);

    // Cleanup
    free(inter_off_relative_to_dst);
    free(inter_off_relative_to_src);
    common_read_selection_delete(inter_sel);

    return volume;
}

// Whenever we are patching between a point selection and bounding box, we
// will always iterate over the point selection, check each points for containment
// within the bounding box, and compute byte offsets for the point within the point list
// and bounding box, regardless of whether the point selection is on the source or
// destination buffer. Therefore, we include a helper function with a boolean flag
// to switch the copy direction, and just branch for a few lines during the copy
// operation. This simplifies the code a lot, and reduces the LoC in this file (although
// this comment makes up for a good bit of that savings).
inline static uint64_t adios_patch_data_bb_pts_helper(void *dst, uint64_t dst_ragged_offset, void *src, uint64_t src_ragged_offset,
                                                      const ADIOS_SELECTION_POINTS_STRUCT *pts,
                                                      const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb,
                                                      _Bool isDestPoints, enum ADIOS_DATATYPES datum_type,
                                                      enum ADIOS_FLAG swap_endianness) {
    const int ndim = pts->ndim;
    uint64_t i;
    int j;
    uint64_t pts_copied = 0;
    uint64_t byte_offset_in_bb_buffer, byte_offset_in_pt_buffer;
    const uint64_t *cur_pt;
    uint64_t *bb_byte_strides = malloc(sizeof(uint64_t) * ndim);
    uint64_t *pt_relative_to_bb = malloc(sizeof(uint64_t) * ndim);

    // Compute the strides into the source bounding box array
    int typelen = adios_get_type_size(datum_type, NULL);
    uint64_t bb_volume = typelen;
    for (j = ndim - 1; j >= 0; j--) {
        bb_byte_strides[j] = bb_volume;
        bb_volume *= bb->count[j];
    }

    uint64_t dst_byte_ragged_offset = dst_ragged_offset * typelen;
    uint64_t src_byte_ragged_offset = src_ragged_offset * typelen;

    // Check that the selection dimensions are compatible
    assert(pts->ndim == bb->ndim);

    // Check each point; if it's in the bounding box, perform a copy
    for (i = 0; i < pts->npoints; i++) {
        cur_pt = &pts->points[i * ndim];

        for (j = 0; j < ndim; j++) {
            // If the point's coordinate in some dimension is outside the bounding box
            if (cur_pt[j] < bb->start[j] ||
                cur_pt[j] >= bb->start[j] + bb->count[j]) {
                break;
            }
        }

        // If the point is within the bounding box
        if (j == ndim) {
            vector_sub(ndim, pt_relative_to_bb, cur_pt, bb->start);

            byte_offset_in_bb_buffer = 0;
            for (j = 0; j < ndim; j++)
                byte_offset_in_bb_buffer += pt_relative_to_bb[j] * bb_byte_strides[j];

            byte_offset_in_pt_buffer = i * typelen;

            if (isDestPoints) {
                assert(byte_offset_in_pt_buffer >= dst_byte_ragged_offset);
                assert(byte_offset_in_bb_buffer >= src_byte_ragged_offset);
                memcpy((char*)dst + byte_offset_in_pt_buffer - dst_byte_ragged_offset, (char*)src + byte_offset_in_bb_buffer - src_byte_ragged_offset, typelen);
            } else {
                assert(byte_offset_in_bb_buffer >= dst_byte_ragged_offset);
                assert(byte_offset_in_pt_buffer >= src_byte_ragged_offset);
                memcpy((char*)dst + byte_offset_in_bb_buffer - dst_byte_ragged_offset, (char*)src + byte_offset_in_pt_buffer - src_byte_ragged_offset, typelen);
            }
            pts_copied++;
        }
    }

    free(bb_byte_strides);
    free(pt_relative_to_bb);

    return pts_copied;
}

inline static uint64_t adios_patch_data_pts_to_bb(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *dst_bb,
                                                  void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION_POINTS_STRUCT *src_pts,
                                                  enum ADIOS_DATATYPES datum_type,
                                                  enum ADIOS_FLAG swap_endianness) {
    return adios_patch_data_bb_pts_helper(dst, dst_ragged_offset, src, src_ragged_offset, src_pts, dst_bb, false, datum_type, swap_endianness);
}

inline static uint64_t adios_patch_data_bb_to_pts(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION_POINTS_STRUCT *dst_pts,
                                                  void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *src_bb,
                                                  enum ADIOS_DATATYPES datum_type,
                                                  enum ADIOS_FLAG swap_endianness) {
    return adios_patch_data_bb_pts_helper(dst, dst_ragged_offset, src, src_ragged_offset, dst_pts, src_bb, true, datum_type, swap_endianness);
}

inline static uint64_t adios_patch_data_pts_to_pts(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION_POINTS_STRUCT *dst_pts,
                                                   void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION_POINTS_STRUCT *src_pts,
                                                   enum ADIOS_DATATYPES datum_type,
                                                   enum ADIOS_FLAG swap_endianness) {
    PATCH_UNIMPL("points","points");
    return 0;
}

// One-to-any global patch functions

inline static uint64_t adios_patch_data_to_bb(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *dst_bb,
                                              void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION *src_sel,
                                              enum ADIOS_DATATYPES datum_type,
                                              enum ADIOS_FLAG swap_endianness) {
    switch (src_sel->type) {
    case ADIOS_SELECTION_BOUNDINGBOX:
    {
        const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *src_bb = &src_sel->u.bb;
        return adios_patch_data_bb_to_bb(dst, dst_ragged_offset, dst_bb, src, src_ragged_offset, src_bb, datum_type, swap_endianness);
    }
    case ADIOS_SELECTION_POINTS:
    {
        const ADIOS_SELECTION_POINTS_STRUCT *src_pts = &src_sel->u.points;
        return adios_patch_data_pts_to_bb(dst, dst_ragged_offset, dst_bb, src, src_ragged_offset, src_pts, datum_type, swap_endianness);
    }
    case ADIOS_SELECTION_WRITEBLOCK:
    case ADIOS_SELECTION_AUTO:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Incompatible selection types %d, %d were used while patching decoded "
                                                                      "transformed data into the user buffer (this is an error in the current "
                                                                      "transform plugin)", src_sel->type, ADIOS_SELECTION_BOUNDINGBOX);
        return 0;
    default:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Unknown selection type %d", src_sel->type);
        return 0;
    }

}

inline static uint64_t adios_patch_data_to_pts(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION_POINTS_STRUCT *dst_pts,
                                               void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION *src_sel,
                                               enum ADIOS_DATATYPES datum_type,
                                               enum ADIOS_FLAG swap_endianness) {
    switch (src_sel->type) {
    case ADIOS_SELECTION_BOUNDINGBOX:
    {
        const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *src_bb = &src_sel->u.bb;
        return adios_patch_data_bb_to_pts(dst, dst_ragged_offset, dst_pts, src, src_ragged_offset, src_bb, datum_type, swap_endianness);
    }
    case ADIOS_SELECTION_POINTS:
    {
        const ADIOS_SELECTION_POINTS_STRUCT *src_pts = &src_sel->u.points;
        return adios_patch_data_pts_to_pts(dst, dst_ragged_offset, dst_pts, src, src_ragged_offset, src_pts, datum_type, swap_endianness);
    }
    case ADIOS_SELECTION_WRITEBLOCK:
    case ADIOS_SELECTION_AUTO:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Incompatible selection types %d, %d were used while patching decoded "
                                                                      "transformed data into the user buffer (this is an error in the current "
                                                                      "transform plugin)", src_sel->type, ADIOS_SELECTION_POINTS);
        return 0;
    default:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Unknown selection type %d", src_sel->type);
        return 0;
    }

}

//
// Any-on-any global patch function
//

static int is_global_selection(const ADIOS_SELECTION *sel) {
	return sel->type == ADIOS_SELECTION_BOUNDINGBOX || sel->type == ADIOS_SELECTION_POINTS;
}

uint64_t adios_patch_data_to_global(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION *dst_sel,
                                    void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION *src_sel,
                                    enum ADIOS_DATATYPES datum_type, enum ADIOS_FLAG swap_endianness)
{
	if (!is_global_selection(dst_sel) || !is_global_selection(src_sel)) {
    	adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Internal error: adios_patch_data_to_global called on non-global selection type(s)");
    	return 0;
	}

	switch (dst_sel->type) {
    case ADIOS_SELECTION_BOUNDINGBOX:
    {
        const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *dst_bb = &dst_sel->u.bb;
        return adios_patch_data_to_bb(dst, dst_ragged_offset, dst_bb, src, src_ragged_offset, src_sel, datum_type, swap_endianness);
    }
    case ADIOS_SELECTION_POINTS:
    {
        const ADIOS_SELECTION_POINTS_STRUCT *dst_pts = &dst_sel->u.points;
        return adios_patch_data_to_pts(dst, dst_ragged_offset, dst_pts, src, src_ragged_offset, src_sel, datum_type, swap_endianness);
    }
    default:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Unknown selection type %d", dst_sel->type);
        return 0;
    }
}




// To-local data patching

// One-to-one global patch functions

inline static uint64_t adios_patch_data_bb_to_wb(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION_WRITEBLOCK_STRUCT *dst_wb,
                                                 void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *src_bb,
                                                 const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *vb_bounds,
                                                 enum ADIOS_DATATYPES datum_type,
                                                 enum ADIOS_FLAG swap_endianness)
{
	// If this is a sub-PG selection, it refers only to a range of
	// elements positions in C array order. Thus, element 0 of the
	// output buffer corresponds to element dst_wb->element_offset
	// of the writeblock selection. This is in addition to any existing
	// ragged offset.
	// (for instance, if dst_wb->element_offset is 5, and the existing
	// ragged offset were 3, then the user's buffer would start at element
	// 3 *within* the sub-PG window that starts at element 5, for a total
	// starting offset of 5+3=8 within the whole PG's bounds)
	if (dst_wb->is_sub_pg_selection)
		dst_ragged_offset += dst_wb->element_offset;

	// Use the existing BB to BB patching function
	return adios_patch_data_bb_to_bb(
		dst, dst_ragged_offset, vb_bounds, // Patch into the output buffer, which is bounded by this varblock's bounds
		src, src_ragged_offset, src_bb,    // Patch from the input selection as usual
		datum_type, swap_endianness);      // Pass other info along
}

inline static uint64_t adios_patch_data_pts_to_wb(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION_WRITEBLOCK_STRUCT *dst_wb,
                                                  void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION_POINTS_STRUCT *src_pts,
                                                  const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *vb_bounds,
                                                  enum ADIOS_DATATYPES datum_type,
                                                  enum ADIOS_FLAG swap_endianness)
{
	// If this is a sub-PG selection, it refers only to a range of
	// elements positions in C array order. Thus, element 0 of the
	// output buffer corresponds to element dst_wb->element_offset
	// of the writeblock selection. This is in addition to any existing
	// ragged offset.
	// (for instance, if dst_wb->element_offset is 5, and the existing
	// ragged offset were 3, then the user's buffer would start at element
	// 3 *within* the sub-PG window that starts at element 5, for a total
	// starting offset of 5+3=8 within the whole PG's bounds)
	if (dst_wb->is_sub_pg_selection)
		dst_ragged_offset += dst_wb->element_offset;

	// Use the existing PTS to BB patching function
	return adios_patch_data_pts_to_bb(
		dst, dst_ragged_offset, vb_bounds, // Patch into the output buffer, which is bounded by this varblock's bounds
		src, src_ragged_offset, src_pts,   // Patch from the input selection as usual
		datum_type, swap_endianness);      // Pass other info along
}

inline static uint64_t adios_patch_data_wb_to_wb(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION_WRITEBLOCK_STRUCT *dst_wb,
                                                 void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION_WRITEBLOCK_STRUCT *src_wb,
                                                 const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *vb_bounds,
                                                 enum ADIOS_DATATYPES datum_type,
                                                 enum ADIOS_FLAG swap_endianness)
{
	uint64_t copy_elem_offset, copy_nelems;
	const uint64_t vb_size_in_elements = compute_volume(vb_bounds->ndim, vb_bounds->count);

	const uint64_t dst_elem_offset = dst_wb->is_sub_pg_selection ? dst_wb->element_offset : 0;
	const uint64_t dst_nelems = dst_wb->is_sub_pg_selection ? dst_wb->nelements : vb_size_in_elements;
	const uint64_t src_elem_offset = src_wb->is_sub_pg_selection ? src_wb->element_offset : 0;
	const uint64_t src_nelems = src_wb->is_sub_pg_selection ? src_wb->nelements : vb_size_in_elements;

	// Find out how many elements overlap between the two
	// writeblock selections (due to the potential existence
	// of sub-PG writeblock selections; if both are whole-PG,
	// this overlap will be complete)
	int intersects = intersect_segments(
			dst_elem_offset, dst_nelems,
			src_elem_offset, src_nelems,
			&copy_elem_offset, &copy_nelems
	);

	// Copy any elements that are common to both selections
	// (for whole-PG writeblock selections, this will be all of them)
	if (intersects) {
		int typesize = adios_get_type_size(datum_type, NULL);
		void *copy_dst = (char*)dst + (copy_elem_offset - dst_elem_offset) * typesize;
		void *copy_src = (char*)src + (copy_elem_offset - src_elem_offset) * typesize;

		memcpy(copy_dst, copy_src, copy_nelems * typesize);
		if (swap_endianness == adios_flag_yes)
			change_endianness(copy_dst, copy_nelems * typesize, datum_type);

		return copy_nelems;
	} else {
		return 0;
	}
}

// One-to-any local patch functions

inline static uint64_t adios_patch_data_to_wb(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION_WRITEBLOCK_STRUCT *dst_wb,
                                              void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION *src_sel,
                                              const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *vb_bounds,
                                              enum ADIOS_DATATYPES datum_type,
                                              enum ADIOS_FLAG swap_endianness) {
    switch (src_sel->type) {
    case ADIOS_SELECTION_BOUNDINGBOX:
    {
        const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *src_bb = &src_sel->u.bb;
        return adios_patch_data_bb_to_wb(dst, dst_ragged_offset, dst_wb, src, src_ragged_offset, src_bb, vb_bounds, datum_type, swap_endianness);
    }
    case ADIOS_SELECTION_POINTS:
    {
        const ADIOS_SELECTION_POINTS_STRUCT *src_pts = &src_sel->u.points;
        return adios_patch_data_pts_to_wb(dst, dst_ragged_offset, dst_wb, src, src_ragged_offset, src_pts, vb_bounds, datum_type, swap_endianness);
    }
    case ADIOS_SELECTION_WRITEBLOCK:
    {
        const ADIOS_SELECTION_WRITEBLOCK_STRUCT *src_wb = &src_sel->u.block;
        return adios_patch_data_wb_to_wb(dst, dst_ragged_offset, dst_wb, src, src_ragged_offset, src_wb, vb_bounds, datum_type, swap_endianness);
    }
    case ADIOS_SELECTION_AUTO:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Incompatible selection types %d, %d were used while patching decoded "
                                                                      "transformed data into the user buffer (this is an error in the current "
                                                                      "transform plugin)", src_sel->type, ADIOS_SELECTION_BOUNDINGBOX);
        return 0;
    default:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Unknown selection type %d", src_sel->type);
        return 0;
    }

}

//
// Any-on-any local patch function
//

// NOTE: vb_bounds_sel is the bounding box of the varblock to which dst_sel (a writeblock selection) corresponds
uint64_t adios_patch_data_to_local(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION *dst_sel,
                                   void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION *src_sel,
                                   const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *vb_bounds,
                                   enum ADIOS_DATATYPES datum_type, enum ADIOS_FLAG swap_endianness)
{
	if (is_global_selection(dst_sel)) {
    	adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Internal error: adios_patch_data_to_local called on non-global destination selection type %d", dst_sel->type);
    	return 0;
	}

    switch (dst_sel->type) {
    case ADIOS_SELECTION_WRITEBLOCK:
    {
        const ADIOS_SELECTION_WRITEBLOCK_STRUCT *dst_wb = &dst_sel->u.block;
        return adios_patch_data_to_wb(dst, dst_ragged_offset, dst_wb, src, src_ragged_offset, src_sel, vb_bounds, datum_type, swap_endianness);
    }
    default:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Unknown selection type %d", dst_sel->type);
        return 0;
    }
}
