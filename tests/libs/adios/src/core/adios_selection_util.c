/*
 * adios_selection_util.c
 *
 *  Created on: Jan 5, 2013
 *      Author: David A. Boyuka II
 */

#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "adios_error.h"
#include "adios_selection.h"
#include "adios_subvolume.h"
#include "adios_selection_util.h"
#include "common_read.h"

//
// NOTE: Intersection type guarantees:
// * The intersection of any two selections of the same type returns a third selection of that
//   same type
// * BB  + PTS  -> PTS
//

//
// One-on-one global intersection functions
//
ADIOS_SELECTION * adios_selection_intersect_bb_bb(const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb1,
                                                  const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb2) {
    const int ndim = bb1->ndim;
    uint64_t *new_start = malloc(ndim * sizeof(uint64_t));
    uint64_t *new_count = malloc(ndim * sizeof(uint64_t));

    assert(bb1->ndim == bb2->ndim);
    if (!new_start || !new_count) {
        adios_error(err_no_memory, "Cannot allocate memory for BOUNDINGBOX-BOUNDINGBOX selection intersection");
        return NULL;
    }

    if (intersect_bb(bb1, bb2, new_start, NULL, NULL, new_count)) {
        return common_read_selection_boundingbox(ndim, new_start, new_count);
    } else {
        free(new_start);
        free(new_count);
        return NULL;
    }
}

ADIOS_SELECTION * adios_selection_intersect_bb_pts(const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb1,
                                                   const ADIOS_SELECTION_POINTS_STRUCT *pts2) {
    const int ndim = bb1->ndim;
    const uint64_t max_new_npts = pts2->npoints;

    uint64_t *new_pts = malloc(max_new_npts * ndim * sizeof(uint64_t));
    int j;
    uint64_t *new_pts_ptr = new_pts;
    uint64_t *pts2_ptr;
    const uint64_t * const pts2_end_ptr = pts2->points + pts2->npoints * ndim;
    uint64_t new_npts = 0;

    assert(bb1->ndim == pts2->ndim);
    if (!new_pts) {
        adios_error(err_no_memory, "Cannot allocate memory for BOUNDINGBOX-POINTS selection intersection");
        return NULL;
    }

    // Check every pair of points for equality; whenever a shared point is found, output
    // it into the new point list
    for (pts2_ptr = pts2->points; pts2_ptr < pts2_end_ptr; pts2_ptr += ndim) {
        // Check each dimension component of the point for containment in the bounding box
        for (j = 0; j < ndim; j++)
            if (pts2_ptr[j] < bb1->start[j] ||
                pts2_ptr[j] >= bb1->start[j] + bb1->count[j])
                break;

        // Check whether any component was out of bounds; if so, skip this point; otherwise,
        // output the point
        if (j != ndim) {
            continue;
        } else {
            memcpy(new_pts_ptr, pts2_ptr, ndim * sizeof(uint64_t));
            new_pts_ptr += ndim;
            new_npts++;
        }
    }

    if (new_npts == 0) {
        free(new_pts);
        return NULL;
    } else {
        new_pts = (uint64_t*)realloc(new_pts, new_npts * ndim * sizeof(uint64_t));
        return common_read_selection_points(ndim, new_npts, new_pts);
    }
}

ADIOS_SELECTION * adios_selection_intersect_pts_pts(const ADIOS_SELECTION_POINTS_STRUCT *pts1,
                                                    const ADIOS_SELECTION_POINTS_STRUCT *pts2) {
    const int ndim = pts1->ndim;
    const uint64_t max_new_npts = pts1->npoints > pts2->npoints ? pts1->npoints : pts2->npoints;

    uint64_t *new_pts = malloc(max_new_npts * ndim * sizeof(uint64_t));
    int k;
    uint64_t *new_pts_ptr = new_pts;
    uint64_t *pts1_ptr, *pts2_ptr;
    const uint64_t * const pts1_end_ptr = pts1->points + pts1->npoints * ndim;
    const uint64_t * const pts2_end_ptr = pts2->points + pts2->npoints * ndim;
    uint64_t new_npts = 0;

    assert(pts1->ndim == pts2->ndim);
    if (!new_pts) {
        adios_error(err_no_memory, "Cannot allocate memory for POINTS-POINTS selection intersection");
        return NULL;
    }

    // Check every pair of points for equality; whenever a shared point is found, output
    // it into the new point list
    for (pts1_ptr = pts1->points; pts1_ptr < pts1_end_ptr; pts1_ptr += ndim) {
        for (pts2_ptr = pts2->points; pts2_ptr < pts2_end_ptr; pts2_ptr += ndim) {
            // Check each dimension component of the pair of points for equality
            for (k = 0; k < ndim; k++)
                if (pts1_ptr[k] != pts2_ptr[k])
                    break;

            // Check whether any component was unequal; if so, skip this pair; otherwise,
            // output the shared point
            if (k != ndim) {
                continue;
            } else {
                memcpy(new_pts_ptr, pts1_ptr, ndim * sizeof(uint64_t));
                new_pts_ptr += ndim;
                new_npts++;
            }
        }
    }

    if (new_npts == 0) {
        free(new_pts);
        return NULL;
    } else {
        new_pts = (uint64_t*)realloc(new_pts, new_npts * sizeof(uint64_t));
        return common_read_selection_points(ndim, new_npts, new_pts);
    }
}

//
// One-on-any global intersection functions
//

// s2 can be selection type
inline static ADIOS_SELECTION * adios_selection_intersect_bb(const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb1,
                                                             const ADIOS_SELECTION *s2) {
    switch (s2->type) {
    case ADIOS_SELECTION_BOUNDINGBOX:
    {
        const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb2 = &s2->u.bb;
        return adios_selection_intersect_bb_bb(bb1, bb2);
    }
    case ADIOS_SELECTION_POINTS:
    {
        const ADIOS_SELECTION_POINTS_STRUCT *pts2 = &s2->u.points;
        return adios_selection_intersect_bb_pts(bb1, pts2);
    }
    default:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Unknown selection type %d", s2->type);
        return NULL;
    }
}

// s2 can be any selection type except boundingbox
inline static ADIOS_SELECTION * adios_selection_intersect_pts(const ADIOS_SELECTION_POINTS_STRUCT *pts1,
                                                              const ADIOS_SELECTION *s2) {
    switch (s2->type) {
    case ADIOS_SELECTION_POINTS:
    {
        const ADIOS_SELECTION_POINTS_STRUCT *pts2 = &s2->u.points;
        return adios_selection_intersect_pts_pts(pts1, pts2);
    }
    default:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Unknown selection type %d", s2->type);
        return NULL;
    }
}

//
// Any-on-any global intersection function
//

static int is_global_selection(const ADIOS_SELECTION *sel) {
	return sel->type == ADIOS_SELECTION_BOUNDINGBOX || sel->type == ADIOS_SELECTION_POINTS;
}

// The if statements impose a total order on the selection types, and call this function
// with arguments swapped if they are out of this order. This simplifies the above helper
// functions.
ADIOS_SELECTION * adios_selection_intersect_global(const ADIOS_SELECTION *s1, const ADIOS_SELECTION *s2) {
	if (!is_global_selection(s1) || !is_global_selection(s2)) {
    	adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Internal error: adios_selection_intersect_global called on non-global selection(s)");
    	return NULL;
	}

	switch (s1->type) {
    case ADIOS_SELECTION_BOUNDINGBOX:
    {
        const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb1 = &s1->u.bb;
        return adios_selection_intersect_bb(bb1, s2);
    }
    case ADIOS_SELECTION_POINTS:
    {
        const ADIOS_SELECTION_POINTS_STRUCT *pts1 = &s1->u.points;
        if (s1->type == ADIOS_SELECTION_BOUNDINGBOX) {
            return adios_selection_intersect_global(s2, s1);
        } else {
            return adios_selection_intersect_pts(pts1, s2);
        }
    }
    default:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Unknown selection type %d", s1->type);
        return NULL;
    }
}


//
// One-on-one global intersection functions
//
ADIOS_SELECTION * adios_selection_intersect_wb_wb(const ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb1,
                                                  const ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb2,
                                                  int timestep,
                                                  const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO *transinfo)
{
	int is_abs_idx;
	int wbindex;
	if (wb1->is_absolute_index == wb2->is_absolute_index) {
		const int index1 = wb1->index;
		const int index2 = wb2->index;
		if (index1 != index2)
			return NULL;

		wbindex = index1;
		is_abs_idx = wb1->is_absolute_index;
	} else {
		const int index1 = wb1->is_absolute_index ? wb1->index : adios_get_absolute_writeblock_index(raw_varinfo, wb1->index, timestep);
		const int index2 = wb2->is_absolute_index ? wb2->index : adios_get_absolute_writeblock_index(raw_varinfo, wb2->index, timestep);
		if (index1 != index2)
			return NULL;

		wbindex = index1;
		is_abs_idx = 1;
	}

	if (!wb1->is_sub_pg_selection && !wb2->is_sub_pg_selection) {
		// If neither selection is a sub-PG selection, the result is easy, and we can return immediately
		ADIOS_SELECTION *inter_sel = common_read_selection_writeblock(wbindex);
		inter_sel->u.block.is_absolute_index = is_abs_idx;
		return inter_sel;
	} else if (wb1->is_sub_pg_selection && wb2->is_sub_pg_selection) {
		// Else, if both selections are sub-PG selections, take the overlapping portion (if any)
		uint64_t inter_elem_offset, inter_nelems;

		int intersects = intersect_segments(
				wb1->element_offset, wb1->nelements,
				wb2->element_offset, wb2->nelements,
				&inter_elem_offset, &inter_nelems
		);

		if (intersects) {
			ADIOS_SELECTION *inter_sel = common_read_selection_writeblock(wbindex);
			inter_sel->u.block.is_absolute_index = is_abs_idx;
			inter_sel->u.block.is_sub_pg_selection = 1;
			inter_sel->u.block.element_offset = inter_elem_offset;
			inter_sel->u.block.nelements = inter_nelems;
			return inter_sel;
		} else {
			return NULL;
		}
	} else if (wb1->is_sub_pg_selection) {
		// Else, if only the first selection is sub-PG, so just use its range
		ADIOS_SELECTION *newwb = common_read_selection_writeblock(wb1->index);
		newwb->u.block = *wb1;
		return newwb;
	} else if (wb2->is_sub_pg_selection) {
		// Else, only the second selection is sub-PG, so just use its range
		ADIOS_SELECTION *newwb = common_read_selection_writeblock(wb2->index);
		newwb->u.block = *wb2;
		return newwb;
	} else {
		abort(); // Should not be possible'
		return NULL;
	}
}

//
// One-on-any local intersection functions
//

// s2 can be selection type
inline static ADIOS_SELECTION * adios_selection_intersect_wb(const ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb1,
                                                             const ADIOS_SELECTION *s2,
                                                             int timestep,
                                                             const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO *transinfo)
{
    switch (s2->type) {
    case ADIOS_SELECTION_WRITEBLOCK:
    {
        const ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb2 = &s2->u.block;
        return adios_selection_intersect_wb_wb(wb1, wb2, timestep, raw_varinfo, transinfo);
    }
    default:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Unknown selection type %d", s2->type);
        return NULL;
    }
}

//
// Any-on-any local intersection function
//
ADIOS_SELECTION * adios_selection_intersect_local(const ADIOS_SELECTION *s1, const ADIOS_SELECTION *s2, int timestep, const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO *transinfo)
{
	if (is_global_selection(s1) || is_global_selection(s2)) {
    	adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Internal error: adios_selection_intersect_local called on non-local selection(s)");
    	return NULL;
	}

	switch (s1->type) {
    case ADIOS_SELECTION_WRITEBLOCK:
    {
        const ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb1 = &s1->u.block;
        return adios_selection_intersect_wb(wb1, s2, timestep, raw_varinfo, transinfo);
    }
    case ADIOS_SELECTION_AUTO:
    {
    	adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Unsupported selection type AUTO in adios_selection_intersect_local");
    	return NULL;
    }
    default:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Unknown selection type %d", s1->type);
        return NULL;
    }
}
