/*
 * adios_transform_identity_read.h
 *
 *  Created on: Apr 12, 2013
 *      Author: David A. Boyuka II
 */

#ifndef ADIOS_TRANSFORM_IDENTITY_READ_H_
#define ADIOS_TRANSFORM_IDENTITY_READ_H_

// This function is shared with APLOD
/*
 * Computes the linearized element start and end offsets of a selection (intersect_sel) within
 * a PG (bounded by pgbb). If all elements between these two offsets is read, and then
 * returned as a datablock with ragged offset equal to the start offset returned here,
 * all data within intersect_sel will be present and results patching will work.
 * @param intersect_sel the intersecting portion of some global read selection with a PG. It must
 *        be fully contained in the PG bounds, as specified by pgbb.
 * @param pgbb the bounds of the PG
 * @param start_off_ptr a pointer to where the start offset will be stored
 * @param end_off_ptr a pointer to where the end offset will be stored
 */
void compute_sieving_offsets_for_pg_selection(const ADIOS_SELECTION *intersect_sel,
                                              const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *pgbb,
                                              uint64_t *start_off_ptr, uint64_t *end_off_ptr);

// This function is shared with ALACRITY, and is available to aid any data transform that stores
// the original data unchanged as part of the transformed data.
/*
 * Schedules raw read requests over a copy of the user's original data. This function is expected
 * to be paired with a calls to adios_transform_handle_pg_reqgroup_completed().
 *
 * This original data is assumed to be contiguous at some location within the PG (specified via
 * original_data_offset_in_pg). If should_sieve_points is true, point selections will be serviced
 * using a single read request, rather than reading each point individually.
 *
 * Note: in addition to submitting adios_transform_raw_read_requests, this function also requires
 * full use of the pg_reqgroup->transform_internal field. If the calling data transform needs this
 * field, it should be wrapped after calling this function, then restored before calling
 * adios_transform_handle_pg_reqgroup_completed().
 */
int adios_transform_generate_read_subrequests_over_original_data(
		uint64_t original_data_offset_in_pg,
		int should_sieve_points,
		adios_transform_read_request *reqgroup,
        adios_transform_pg_read_request *pg_reqgroup);

/*
 * Processes raw read requests submitted earlier by
 * adios_transform_generate_read_subrequests_over_original_data(). Should be called on every
 * completed raw read request that was scheduled by
 * adios_transform_generate_read_subrequests_over_original_data().
 */
adios_datablock * adios_transform_pg_reqgroup_completed_over_original_data(
        adios_transform_read_request *reqgroup,
        adios_transform_pg_read_request *completed_pg_reqgroup);

#endif /* ADIOS_TRANSFORM_IDENTITY_READ_H_ */
