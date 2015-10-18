/*
 * adios_transform_identity_read.c
 *
 * An implementation of the "identity" transform, which does nothing, but
 * exercises the transform framework for testing.
 *
 *  Created on: Jul 31, 2012
 *      Author: David A. Boyuka II
 */

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "util.h"
#include "core/adios_internals.h" // adios_get_type_size()
#include "core/adios_subvolume.h"
#include "core/transforms/adios_transforms_hooks_read.h"
#include "core/transforms/adios_transforms_reqgroup.h"

// Implementation of the "identity" transform, which does nothing to
// the data, but exercises the transform framework for testing.

#define MAX_DIMS 32
#define FREE(x) { if (x) { free(x); x = NULL; } }

int adios_transform_identity_is_implemented (void) {return 1;}

void compute_sieving_offsets_for_pg_selection(const ADIOS_SELECTION *intersect_sel,
                                              const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *pgbb,
                                              uint64_t *start_off_ptr, uint64_t *end_off_ptr) {
    uint64_t i;
    uint64_t tmp_point[MAX_DIMS]; // For scratchwork

    uint64_t start_off, end_off; // Start/end byte offsets to read between
    switch (intersect_sel->type) {
    case ADIOS_SELECTION_WRITEBLOCK:
    {
    	if (intersect_sel->u.block.is_sub_pg_selection) {
    		start_off = intersect_sel->u.block.element_offset;
    		end_off = intersect_sel->u.block.element_offset + intersect_sel->u.block.nelements;
    	} else {
    		start_off = 0;
    		end_off = compute_volume(pgbb->ndim, pgbb->count);
    	}
   		break;
    }
    case ADIOS_SELECTION_BOUNDINGBOX:
    {
        const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb = &intersect_sel->u.bb;

        vector_sub(bb->ndim, tmp_point, bb->start, pgbb->start); // Selection box start relative to PG
        start_off = compute_linear_offset_in_volume(bb->ndim, tmp_point, pgbb->count);

        vector_add(bb->ndim, tmp_point, tmp_point, bb->count); // Selection box end relative to PG
        for (i = 0; i < bb->ndim; i++)
            tmp_point[i]--; // Reduce all by 1 because bb->start+bb->count is exclusive
        end_off = compute_linear_offset_in_volume(bb->ndim, tmp_point, pgbb->count) + 1; // Add 1 because this offset points to the last element, and we want one past that since end_off is exclusive

        break;
    }
    case ADIOS_SELECTION_POINTS:
    {
        const ADIOS_SELECTION_POINTS_STRUCT *pts = &intersect_sel->u.points;

        start_off = UINT64_MAX; // Set it to max so that the first point brings it down
        end_off = 0;
        for (i = 0; i < pts->npoints; i++) {
            vector_sub(pts->ndim, tmp_point, pts->points + i * pts->ndim, pgbb->start);

            const uint64_t point_off = compute_linear_offset_in_volume(pts->ndim, tmp_point, pgbb->count);
            if (point_off < start_off)
                start_off = point_off;
            if (point_off > end_off)
                end_off = point_off;
        }

        end_off++; // Advance past the last element to make it exclusive

        break;
    }

    case ADIOS_SELECTION_AUTO:
        /* Unsupported */
        break;
    }

    *start_off_ptr = start_off;
    *end_off_ptr = end_off;
}

int adios_transform_generate_read_subrequests_over_original_data(
		uint64_t original_data_offset_in_pg,
		int should_sieve_points,
		adios_transform_read_request *reqgroup,
        adios_transform_pg_read_request *pg_reqgroup)
{
    int is_point_selection = (pg_reqgroup->pg_intersection_sel->type == ADIOS_SELECTION_POINTS);

    if (!is_point_selection || should_sieve_points) {
        pg_reqgroup->transform_internal = NULL; // Is sieving

        uint64_t start_off, end_off; // Start/end byte offsets to read between
        compute_sieving_offsets_for_pg_selection(pg_reqgroup->pg_intersection_sel, &pg_reqgroup->pg_bounds_sel->u.bb, &start_off, &end_off);

        int datum_size = adios_get_type_size(reqgroup->transinfo->orig_type, NULL); // Get the data type size

        // Allocate a buffer for the read, and create a raw read request for it
        const uint64_t buflen = (end_off - start_off) * datum_size;
        void *buf = malloc(buflen);
        adios_transform_raw_read_request *subreq =
                adios_transform_raw_read_request_new_byte_segment(pg_reqgroup, original_data_offset_in_pg + start_off * datum_size, buflen, buf);

        // Store the ragged start offset
        subreq->transform_internal = malloc(sizeof(uint64_t));
        *(uint64_t*)subreq->transform_internal = start_off;

        // Append the raw read request
        adios_transform_raw_read_request_append(pg_reqgroup, subreq);
    } else {
        const int ndim = pg_reqgroup->pg_intersection_sel->u.points.ndim;
        const uint64_t *points = pg_reqgroup->pg_intersection_sel->u.points.points;

        const int datum_size = adios_get_type_size(reqgroup->transinfo->orig_type, NULL); // Get the data type size
        const int npoints = pg_reqgroup->pg_intersection_sel->u.points.npoints;
        const uint64_t bufsize = npoints * datum_size;

        char *buf = malloc(bufsize);

        int p;
        for (p = 0; p < npoints; p++) {
            const uint64_t *point = &points[p * ndim];
            const uint64_t offset = compute_linear_offset_in_volume(ndim, point, pg_reqgroup->pg_bounds_sel->u.bb.count);

            adios_transform_raw_read_request *subreq =
                    adios_transform_raw_read_request_new_byte_segment(pg_reqgroup, original_data_offset_in_pg + offset * datum_size, 1, buf + p * datum_size);
            adios_transform_raw_read_request_append(pg_reqgroup, subreq);
        }

        pg_reqgroup->transform_internal = buf;
    }

    return 0;
}


adios_datablock * adios_transform_pg_reqgroup_completed_over_original_data(
        adios_transform_read_request *reqgroup,
        adios_transform_pg_read_request *completed_pg_reqgroup)
{
    adios_datablock *db;
    if (completed_pg_reqgroup->transform_internal) {
        void *data = completed_pg_reqgroup->transform_internal;

        db = adios_datablock_new(reqgroup->transinfo->orig_type,
                completed_pg_reqgroup->timestep,
                completed_pg_reqgroup->pg_intersection_sel,
                data);

        FREE(completed_pg_reqgroup->transform_internal);
    } else {
        uint64_t ragged_offset = *(uint64_t*)completed_pg_reqgroup->subreqs->transform_internal;

        // Transfer ownership of the data buffer
        void *pg_data = completed_pg_reqgroup->subreqs->data;
        completed_pg_reqgroup->subreqs->data = NULL;

        db = adios_datablock_new_ragged_offset(reqgroup->transinfo->orig_type,
                completed_pg_reqgroup->timestep,
                completed_pg_reqgroup->pg_writeblock_sel,
                ragged_offset,
                pg_data);

        FREE(completed_pg_reqgroup->subreqs->transform_internal);
    }

    return db;
}

int adios_transform_identity_generate_read_subrequests(adios_transform_read_request *reqgroup,
                                                       adios_transform_pg_read_request *pg_reqgroup) {

    int sieve_points = (reqgroup->read_param && strcmp(reqgroup->read_param, "sieve") == 0);
    return adios_transform_generate_read_subrequests_over_original_data(0, sieve_points, reqgroup, pg_reqgroup);
}

// Do nothing for individual subrequest
adios_datablock * adios_transform_identity_subrequest_completed(
                    adios_transform_read_request *reqgroup,
                    adios_transform_pg_read_request *pg_reqgroup,
                    adios_transform_raw_read_request *completed_subreq) {
    return NULL;
}

adios_datablock * adios_transform_identity_pg_reqgroup_completed(
        adios_transform_read_request *reqgroup,
        adios_transform_pg_read_request *completed_pg_reqgroup) {
	return adios_transform_pg_reqgroup_completed_over_original_data(reqgroup, completed_pg_reqgroup);
}

adios_datablock * adios_transform_identity_reqgroup_completed(
        adios_transform_read_request *completed_reqgroup) {
    return NULL;
}
