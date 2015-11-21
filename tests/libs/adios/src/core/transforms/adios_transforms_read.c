#include <stddef.h>
#include <assert.h>

#include "core/adios_bp_v1.h"
#include "core/adios_internals.h"
#include "core/common_read.h"
#include "adios_selection.h"
#include "adios_error.h"
#include "adios_types.h"
#include "adios_read_v2.h"
#include "adios_read_ext.h"
#include "core/adios_logger.h"
#include "core/util.h"

#include "core/adios_selection_util.h"

#include "core/transforms/adios_transforms_reqgroup.h"
#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_datablock.h"
#include "core/transforms/adios_transforms_hooks_read.h"
#include "core/transforms/adios_transforms_read.h"
#include "core/transforms/adios_transforms_util.h"
#include "core/transforms/adios_patchdata.h"

// Utilities
#define FREE(p) {if (p){free(p); (p)=NULL;}}

// Read request inspection
enum ADIOS_TRANSFORM_REQGROUP_RESULT_MODE adios_transform_read_request_get_mode(const adios_transform_read_request *req) {
    return req->orig_data != NULL ? FULL_RESULT_MODE : PARTIAL_RESULT_MODE;
}

// BLOCKINFO inspection
uint64_t adios_transform_get_transformed_var_size_from_blockinfo(int raw_ndim, const ADIOS_VARBLOCK *raw_block) {
    assert(raw_ndim == 1); // Any time dimension should have been stripped from BLOCKINFO

    // Since we swtiched to 1D local byte arrays, the first (and only) dimension contains what we want
    return raw_block->count[0];
}

//
// Read request management (rest of the file)
//

static uint64_t compute_selection_size_in_bytes(const ADIOS_SELECTION *sel,
                                                enum ADIOS_DATATYPES datum_type,
                                                int timestep,
                                                const ADIOS_VARINFO *raw_varinfo,
                                                const ADIOS_TRANSINFO *transinfo) {
    int typesize = adios_get_type_size(datum_type, NULL);
    int i;
    switch (sel->type) {
    case ADIOS_SELECTION_BOUNDINGBOX:
    {
        const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb = &sel->u.bb;
        const int ndim = bb->ndim;

        uint64_t size = typesize;
        for (i = 0; i < ndim; i++)
            size *= bb->count[i];

        return size;
    }
    case ADIOS_SELECTION_POINTS:
    {
        const ADIOS_SELECTION_POINTS_STRUCT *pts = &sel->u.points;
        return pts->ndim * pts->npoints * typesize;
    }
    case ADIOS_SELECTION_WRITEBLOCK:
    {
        const ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb = &sel->u.block;

        if (wb->is_sub_pg_selection) {
            return wb->nelements * typesize;
        } else {
            const ADIOS_VARBLOCK *theblock;
            uint64_t size = typesize;
            int absolute_idx;

            if (wb->is_absolute_index) {
                absolute_idx = wb->index;
            } else {
                int timestep_start_idx = 0;
                for (i = 0; i < timestep; i++)
                    timestep_start_idx += raw_varinfo->nblocks[i];

                absolute_idx = timestep_start_idx + wb->index;
            }

            theblock = &transinfo->orig_blockinfo[absolute_idx];
            for (i = 0; i < transinfo->orig_ndim; i++)
                size *= theblock->count[i];

            return size;
        }
    }
    case ADIOS_SELECTION_AUTO:
    default:
        adios_error_at_line(err_invalid_argument, __FILE__, __LINE__, "Unsupported selection type %d in data transform read layer", sel->type);
        return 0;
    }
}

/*
 * Determines the block indices corresponding to a start and end timestep.
 * Both the input start/end timesteps and the output start/end blockidx are lower bound inclusive, upper bound exclusive: [start, end)
 */
static void compute_blockidx_range(const ADIOS_VARINFO *raw_varinfo, int from_steps, int to_steps, int *start_blockidx, int *end_blockidx) {
    int timestep;

    // Find the block index for the start and end timestep
    int curblocks = 0;
    for (timestep = 0; timestep < raw_varinfo->nsteps; timestep++) {
        // Find the start block
        if (timestep == from_steps) {
            *start_blockidx = curblocks;
        }
        curblocks += raw_varinfo->nblocks[timestep];
        // Find the end block, then stop
        if (timestep == to_steps - 1) {
            *end_blockidx = curblocks;
            break;
        }
    }
}

// Converts absolute varblock indexes to timestep-relative varblock indexes
static int compute_absolute_blockidx_from_relative_blockidx(const ADIOS_VARINFO *raw_varinfo, int timestep, int timestep_blockidx, int *blockidx_out) {
	if (timestep < 0 || timestep >= raw_varinfo->nsteps) {
		return 0;
	}
    if (timestep_blockidx >= raw_varinfo->nblocks[timestep]) {
    	return 0;
    }

	// Find the block index for the start and end timestep
    int ts;
    int curblocks = 0;
    for (ts = 0; ts < timestep; ts++)
        curblocks += raw_varinfo->nblocks[ts];

    *blockidx_out = curblocks + timestep_blockidx;
    return 1;
}

// Converts rimestep-relative varblock indexes to absolute varblock indexes
static int compute_relative_blockidx_from_absolute_blockidx(const ADIOS_VARINFO *raw_varinfo, int blockidx, int *timestep_out, int *timestep_blockidx_out) {
    // Find the block index for the start and end timestep
    int timestep;
	int curblocks = 0, next_curblocks;
    for (timestep = 0; timestep < raw_varinfo->nsteps; timestep++) {
        next_curblocks = curblocks + raw_varinfo->nblocks[timestep];

        if (blockidx < next_curblocks) {
        	*timestep_out = timestep;
        	*timestep_blockidx_out = blockidx - curblocks;
        	return 1;
        }

        curblocks = next_curblocks;
    }
    return 0;
}

inline static int is_global_selection(const ADIOS_SELECTION *sel) {
	return sel->type != ADIOS_SELECTION_WRITEBLOCK;
}

// Creates a bounding box from a given ADIOS_VARBLOCK
inline static const ADIOS_SELECTION * create_pg_bounds_from_varblock(int ndim, const ADIOS_VARBLOCK *orig_vb) {
    // Commented out for performance
    //const uint64_t *new_start = (uint64_t*)bufdup(orig_vb->start, sizeof(uint64_t), ndim);
    //const uint64_t *new_count = (uint64_t*)bufdup(orig_vb->count, sizeof(uint64_t), ndim);

    //return common_read_selection_boundingbox(ndim, new_start, new_count);
    return common_read_selection_boundingbox(ndim, orig_vb->start, orig_vb->count);
}

// Creates a bounding box for a the varblock specified by a given writeblock
// selection (which may be absolute or timestep-relative)
inline static const ADIOS_SELECTION * create_writeblock_bounds(const ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb, int timestep, const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO *transinfo) {
    int blockidx;
    if (wb->is_absolute_index) {
    	blockidx = wb->index;
    } else {
    	compute_absolute_blockidx_from_relative_blockidx(raw_varinfo, timestep, wb->index, &blockidx);
    }

	// Get the bounds of the varblock corresponding to the given writeblock
    const ADIOS_VARBLOCK *vb_bounds = &transinfo->orig_blockinfo[blockidx];
    return create_pg_bounds_from_varblock(transinfo->orig_ndim, vb_bounds);
}

static int generate_read_request_for_pg(
		const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO *transinfo,
		const ADIOS_SELECTION *sel,
		int timestep, int timestep_blockidx, int blockidx,
		adios_transform_read_request *readreq)
{
    const ADIOS_SELECTION *pg_bounds_sel;
    ADIOS_SELECTION *pg_intersection_sel;
    ADIOS_SELECTION *pg_writeblock_sel;

    const ADIOS_VARBLOCK *raw_vb = &raw_varinfo->blockinfo[blockidx];
    const ADIOS_VARBLOCK *orig_vb = &transinfo->orig_blockinfo[blockidx];

    pg_bounds_sel = create_pg_bounds_from_varblock(transinfo->orig_ndim, orig_vb);
    pg_writeblock_sel = common_read_selection_writeblock(blockidx);
    pg_writeblock_sel->u.block.is_absolute_index = 1;

    // Find the intersection, if any
    if (is_global_selection(sel)) {
    	pg_intersection_sel = adios_selection_intersect_global(pg_bounds_sel, sel);
    } else if (sel->type == ADIOS_SELECTION_WRITEBLOCK) {
    	pg_intersection_sel = adios_selection_intersect_local(pg_writeblock_sel, sel, timestep, raw_varinfo, transinfo);
    } else {
    	abort(); // Should never be called with other types of selections
    }

    // If there is an intersection, generate a corresponding PG read request
    if (pg_intersection_sel) {
        // Make a PG read request group, and fill it with some subrequests, and link it into the read reqgroup
        adios_transform_pg_read_request *new_pg_readreq;
        new_pg_readreq = adios_transform_pg_read_request_new(timestep, timestep_blockidx, blockidx,
                                                              transinfo->orig_ndim, raw_varinfo->ndim,
                                                              orig_vb, raw_vb,
                                                              pg_intersection_sel, pg_bounds_sel,
                                                              transinfo->transform_metadatas[blockidx].content,
                                                              (uint16_t)transinfo->transform_metadatas[blockidx].length);

        adios_transform_generate_read_subrequests(readreq, new_pg_readreq);
        adios_transform_pg_read_request_append(readreq, new_pg_readreq);

        // Don't free pg_bounds_sel or pg_intersection_sel, since they are now
        // held by the adios_transform_pg_read_request struct
        return 1;
    } else {
        // Cleanup
        common_read_selection_delete((ADIOS_SELECTION *)pg_bounds_sel); // OK to delete, because this function only frees the outer struct, not the arrays within
        return 0;
    }
}

static void populate_read_request_for_global_selection(
		const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO *transinfo,
		const ADIOS_SELECTION *sel, int from_steps, int nsteps,
		adios_transform_read_request *readreq)
{
    int blockidx, timestep, timestep_blockidx;
    int start_blockidx, end_blockidx;
    int to_steps = from_steps + nsteps;

    // Compute the blockidx range, given the timesteps
    compute_blockidx_range(raw_varinfo, from_steps, to_steps, &start_blockidx, &end_blockidx);

    // Assemble read requests for each varblock
    blockidx = start_blockidx;
    timestep = from_steps;
    timestep_blockidx = 0;
    while (blockidx != end_blockidx) { //for (blockidx = startblock_idx; blockidx != endblock_idx; blockidx++) {
    	generate_read_request_for_pg(raw_varinfo, transinfo, sel, timestep, timestep_blockidx, blockidx, readreq);

        // Increment block indexes
        blockidx++;
        timestep_blockidx++;
        if (timestep_blockidx == raw_varinfo->nblocks[timestep]) {
            timestep_blockidx = 0;
            timestep++;
        }
    }
}

// Note: from_steps and nsteps are ignored in the absolute writeblock case
static void populate_read_request_for_local_selection(
		const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO *transinfo,
		const ADIOS_SELECTION *sel, int from_steps, int nsteps,
		adios_transform_read_request *readreq)
{
	int timestep, timestep_blockidx, blockidx;
	if (sel->type == ADIOS_SELECTION_WRITEBLOCK) {
		const ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb = &sel->u.block;

		if (wb->is_absolute_index) {
			// For an absolute writeblock, at most one PG is touched (0 if erroneous blockidx)
			blockidx = wb->index;

			// Convert blockidx to timestep and timestep_blockidx
			int valid_blockidx = compute_relative_blockidx_from_absolute_blockidx(raw_varinfo, blockidx, &timestep, &timestep_blockidx);
			if (valid_blockidx) {
				generate_read_request_for_pg(raw_varinfo, transinfo, sel, timestep, timestep_blockidx, blockidx, readreq);
			} else {
				adios_error(err_invalid_timestep, "Writeblock selection with invalid absolute index %d passed to adios_schedule_read, caught in ADIOS transforms layer",
							wb->index);
			}
		} else {
			// For a relative writeblock, one PG may be touched per timestep in the user's timestep range
			timestep_blockidx = wb->index;
			for (timestep = from_steps; timestep < from_steps + nsteps; timestep++) {
				// Convert timestep (loop iterator variable) and timestep_blockidx to blockidx
				int valid_blockidx = compute_absolute_blockidx_from_relative_blockidx(raw_varinfo, timestep, timestep_blockidx, &blockidx);
				if (valid_blockidx) {
					generate_read_request_for_pg(raw_varinfo, transinfo, sel, timestep, timestep_blockidx, blockidx, readreq);
				} else {
					adios_error(err_invalid_timestep, "Writeblock selection with index %d passed to adios_schedule_read is invalid in timestep %d, caught in ADIOS transforms layer",
								wb->index, timestep);
				}
			}
		}
	} else {
		adios_error_at_line(err_operation_not_supported, __FILE__, __LINE__, "Internal error: unsupported selection type %d in populate_read_request_for_local_selection", sel->type);
	}
}

adios_transform_read_request * adios_transform_generate_read_reqgroup(const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO* transinfo, const ADIOS_FILE *fp,
                                                                      const ADIOS_SELECTION *sel, int from_steps, int nsteps, const char *param, void *data) {
    // Declares
    adios_transform_read_request *new_readreq;
    int blockidx, timestep, timestep_blockidx;
    int start_blockidx, end_blockidx;

    enum ADIOS_FLAG swap_endianness = (fp->endianness == get_system_endianness()) ? adios_flag_no : adios_flag_yes;

    // In streaming mode, ignore the user's from_steps/nsteps arguments
    if (fp->is_streaming) {
    	from_steps = 0;
    	nsteps = 1;
    }

    // Precondition checking
    assert(is_transform_type_valid(transinfo->transform_type));
    assert(from_steps >= 0 && from_steps + nsteps <= raw_varinfo->nsteps);

    if (sel->type != ADIOS_SELECTION_BOUNDINGBOX &&
        sel->type != ADIOS_SELECTION_POINTS &&
        sel->type != ADIOS_SELECTION_WRITEBLOCK) {
        adios_error(err_operation_not_supported, "Only bounding box, point , and writeblock selections are currently supported for reads on transformed variables.");
    }

    // Retrieve blockinfos, if they haven't been done retrieved
    if (!raw_varinfo->blockinfo)
        common_read_inq_var_blockinfo_raw(fp, (ADIOS_VARINFO*)raw_varinfo);
    if (!transinfo->orig_blockinfo)
        common_read_inq_trans_blockinfo(fp, raw_varinfo, (ADIOS_TRANSINFO*)transinfo);

    // Allocate a new, empty request group
    new_readreq = adios_transform_read_request_new(fp, raw_varinfo, transinfo, sel, from_steps, nsteps, param, data, swap_endianness);

    if (is_global_selection(sel)) {
    	populate_read_request_for_global_selection(raw_varinfo, transinfo, sel, from_steps, nsteps, new_readreq);
    } else {
    	populate_read_request_for_local_selection(raw_varinfo, transinfo, sel, from_steps, nsteps, new_readreq);
    }

    // If this read request does not intersect any PGs, then clear the new read request and return NULL
    if (new_readreq->num_pg_reqgroups == 0) {
        adios_transform_read_request_free(&new_readreq);
        new_readreq = NULL;
    }

    return new_readreq;
}

/*
 * Called whenever a raw read request has been served by the read layer. Marks
 * all raw_read_requests, pg_read_requests and read_requests as completed as necessary,
 * calls the appropriate hooks in the transform method, and returns an
 * adios_datablock if the transform method produces one.
 */
static adios_datablock * finish_subreq(
        adios_transform_read_request *reqgroup,
        adios_transform_pg_read_request *pg_reqgroup,
        adios_transform_raw_read_request *subreq)
{
    adios_datablock *result, *tmp_result;

    // Mark the subrequest as complete
    assert(!subreq->completed && !pg_reqgroup->completed && !reqgroup->completed);
    adios_transform_raw_read_request_mark_complete(reqgroup, pg_reqgroup, subreq);

    // Invoke all callbacks, depending on what completed, and
    // get at most one ADIOS_VARCHUNK to return
    result = adios_transform_subrequest_completed(reqgroup, pg_reqgroup, subreq);

    if (pg_reqgroup->completed) {
        tmp_result = adios_transform_pg_reqgroup_completed(reqgroup, pg_reqgroup);
        if (tmp_result) {
            assert(!result); // pg_reqgroup_completed returned a result, but subrequest_completed did as well
            result = tmp_result;
        }
    }

    if (reqgroup->completed) {
        tmp_result = adios_transform_read_reqgroup_completed(reqgroup);
        if (tmp_result) {
            assert(!result); // read_reqgroup_completed returned a result, but subrequest_completed or pg_reqgroup_completed did as well
            result = tmp_result;
        }
    }

    return result;
}

static uint64_t apply_datablock_to_buffer_local_selections(
		const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO *transinfo,
		adios_datablock *datablock,
        void **output_buffer, const ADIOS_SELECTION *output_sel,
        ADIOS_SELECTION **out_inter_sel, int want_out_inter_sel,
        enum ADIOS_FLAG swap_endianness)
{
	int may_have_intersection = 1;

	// For writeblock selections, we can use adios_patch_data_to_local,
	// but first we must determine the bounding box of the writeblock selection
    const ADIOS_SELECTION *vb_bounds_sel = create_writeblock_bounds(&output_sel->u.block, datablock->timestep, raw_varinfo, transinfo);

    // Compute the intersection explicitly if it is requested, or
    // if we need to allocate a fitting output buffer
    if (want_out_inter_sel || !*output_buffer) {
        *out_inter_sel = adios_selection_intersect_local(datablock->bounds, output_sel, datablock->timestep, raw_varinfo, transinfo);
        may_have_intersection = (*out_inter_sel ? 1 : 0);
    }

    // Allocate the output buffer if needed (inter_sel is populated by previous if statement)
    if (!*output_buffer) {
    	const uint64_t chunk_buffer_size = compute_selection_size_in_bytes(*out_inter_sel, datablock->elem_type, datablock->timestep, raw_varinfo, transinfo);
		*output_buffer = malloc(chunk_buffer_size);

		// Refitting the output selection to the intersection region, since we
		// just allocated a buffer for that smaller region
		output_sel = *out_inter_sel;
    }

	// Invoke adios_patch_data_to_local to perform the actual patching
	const uint64_t used_count =
			adios_patch_data_to_local(
				*output_buffer, (uint64_t)0, output_sel,
				datablock->data, datablock->ragged_offset, datablock->bounds,
				&vb_bounds_sel->u.bb,
				datablock->elem_type, swap_endianness);

	// Clean up
	common_read_selection_delete((ADIOS_SELECTION *)vb_bounds_sel);
	return used_count;
}

// Note: assumes one or more of datablock->bounds or output_sel is local
static uint64_t apply_datablock_to_buffer_nonlocal_selections(
		const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO *transinfo,
		adios_datablock *datablock,
        void **output_buffer, const ADIOS_SELECTION *output_sel,
        ADIOS_SELECTION **out_inter_sel, int want_out_inter_sel,
        enum ADIOS_FLAG swap_endianness)
{
	int may_have_intersection = 1;
	uint64_t used_count = 0;

	const ADIOS_SELECTION *global_output_buffer_sel = output_sel;
	const ADIOS_SELECTION *global_datablock_bounds = datablock->bounds;

	// Promote output buffer selection and/or datablock selection to global if needed
	if (!is_global_selection(global_output_buffer_sel))
		global_output_buffer_sel = create_writeblock_bounds(&global_output_buffer_sel->u.block, datablock->timestep, raw_varinfo, transinfo);
	if (!is_global_selection(global_datablock_bounds))
		global_datablock_bounds = create_writeblock_bounds(&global_datablock_bounds->u.block, datablock->timestep, raw_varinfo, transinfo);

    // Compute the intersection explicitly if it is requested, or
    // if we need to allocate a fitting output buffer
    if (want_out_inter_sel || !*output_buffer) {
    	*out_inter_sel = adios_selection_intersect_global(global_datablock_bounds, global_output_buffer_sel);
    	may_have_intersection = (*out_inter_sel ? 1 : 0);
    }

    // We can stop immediately if it is known there is no intersection
    if (may_have_intersection) {
    	// Allocate the output buffer if needed (inter_sel is populated by previous if statement)
    	if (!*output_buffer) {
    		const uint64_t chunk_buffer_size = compute_selection_size_in_bytes(*out_inter_sel, datablock->elem_type, datablock->timestep, raw_varinfo, transinfo);
    		*output_buffer = malloc(chunk_buffer_size);

    		// Refitting the output selection to the intersection region, since we
    		// just allocated a buffer for that smaller region
    		if (global_output_buffer_sel != output_sel)
    			common_read_selection_delete((ADIOS_SELECTION *)global_output_buffer_sel);
    		output_sel = *out_inter_sel;
    		global_output_buffer_sel = *out_inter_sel;
    	}

    	// Perform the actual data patching, now that everything is in the global space
    	used_count = adios_patch_data_to_global(
    			*output_buffer, (uint64_t)0, global_output_buffer_sel,
    			datablock->data, datablock->ragged_offset, global_datablock_bounds,
    			datablock->elem_type, swap_endianness);
    }

	// Clean up
	if (global_output_buffer_sel != output_sel)
		common_read_selection_delete((ADIOS_SELECTION *)global_output_buffer_sel);
	if (global_datablock_bounds != datablock->bounds)
		common_read_selection_delete((ADIOS_SELECTION *)global_datablock_bounds);

	return used_count;
}

/*
 * Takes a datablock and applies its data to a given output buffer (described
 * by a given output buffer selection), then frees the given datablock.
 *
 * If *output_buffer is non-NULL, copies the pertinent data from datablock->data
 * into *output_buffer, assuming *output_buffer is shaped like output_sel.
 *
 * If *output_buffer is NULL, allocates a minimum-size buffer to contain the
 * data for the *intersection* of datablock->bounds and output_sel and returns
 * it via *output_buffer.
 *
 * If out_inter_sel != NULL, returns a selection representing the intersection
 * of datablock->bounds and output_sel (i.e., the portion of data that was
 * actually copied) via *out_inter_sel. Otherwise, leaves this argument untouched.
 *
 * This function can handle any combination of datablock bounding selection and
 * output buffer bounding selection:
 * - If both datablock and buffer selections are global (BB, points), performs
 *   a straightforward data patching based on the geometry
 * - If both datablock and buffer selections are local (writeblock), performs
 *   a straghtforward memcpy as appropriate
 * - If the buffer is global but the datablock is local, promotes the datablock
 *   to a bounding box using blockinfo (note: the variable is guaranteed to be
 *   a global array in this case because the user supplied a global selection)
 * - If the buffer is local but the datablock is global, promotes the buffer
 *   to a bounding box using blockinfo (note: the variable may or may not be
 *   a global array in this case; however, even if it is not, both the datablock
 *   and the buffer will be constructed relative to (0,0,...,0), so it will do
 *   the right thing).
 *
 * @return the number of elements patched into the output buffer (0 indicates
 *         no data in the given datablock was pertinent to the given output
 *         buffer selection)
 */
static uint64_t apply_datablock_to_buffer_and_free(
		const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO *transinfo,
		adios_datablock *datablock,
        void **output_buffer, const ADIOS_SELECTION *output_sel, ADIOS_SELECTION **out_inter_sel, enum ADIOS_FLAG swap_endianness)
{
    uint64_t used_count = 0;
    ADIOS_SELECTION *inter_sel = NULL;
    assert(raw_varinfo && transinfo && datablock && output_buffer && output_sel);

    // Check preconditions
    if (datablock->bounds->type != ADIOS_SELECTION_BOUNDINGBOX &&
    	datablock->bounds->type != ADIOS_SELECTION_POINTS &&
    	datablock->bounds->type != ADIOS_SELECTION_WRITEBLOCK)
    {
        adios_error(err_operation_not_supported,
                    "Only results of bounding box, points, or writeblock selection types "
                    "are currently accepted from transform plugins (received selection type %d)\n",
                    datablock->bounds->type);
        return 0;
    }
    if (output_sel->type != ADIOS_SELECTION_BOUNDINGBOX &&
    	output_sel->type != ADIOS_SELECTION_POINTS &&
    	output_sel->type != ADIOS_SELECTION_WRITEBLOCK)
    {
        adios_error_at_line(err_operation_not_supported, __FILE__, __LINE__,
                            "Internal error: only bounding box, points, or writeblock selection types "
                            "are currently supported in apply_datablock_to_buffer_and_free (received selection type %d)\n",
                            datablock->bounds->type);
        return 0;
    }

    // Invoke the appropriate helper function depending
    // on whether at least one of datablock->bounds or
    // output_sel is global
    if (!is_global_selection(datablock->bounds) && !is_global_selection(output_sel)) {
    	used_count = apply_datablock_to_buffer_local_selections(
    			raw_varinfo, transinfo,
    			datablock, output_buffer, output_sel,
    			&inter_sel, (out_inter_sel ? 1 : 0),
    			swap_endianness);
    } else {
    	used_count = apply_datablock_to_buffer_nonlocal_selections(
    			raw_varinfo, transinfo,
    			datablock, output_buffer, output_sel,
    			&inter_sel, (out_inter_sel ? 1 : 0),
    			swap_endianness);
    }

    // Clean up the returned intersection if it is not wanted by the caller
	if (inter_sel) {
		if (out_inter_sel) {
			*out_inter_sel = inter_sel;
		} else {
			// TODO: Deep delete the selection (delete points list, start/count arrays, etc.)
			common_read_selection_delete(inter_sel);
		}
	}

    // Free the datablock
    adios_datablock_free(&datablock, 1);
    return used_count;
}

/*
 * Takes a datablock and applies its data to the user buffer for the given
 * read request group, then frees the given datablock. Assumes there is, in
 * fact, a user buffer (i.e., it is not NULL).
 *
 * Assumes that the datablock selection is of type bounding box.
 *
 * NOTE: also frees the data buffer within the datablock
 *
 * @return non-zero if some data in the datablock intersected the read
 *         request's selection, and was applied; returns 0 otherwise.
 */
static int apply_datablock_to_result_and_free(adios_datablock *datablock,
                                              adios_transform_read_request *reqgroup)
{
    assert(datablock); assert(reqgroup);
    assert(reqgroup->orig_sel);
    assert(reqgroup->orig_data);

    void *output_buffer;
    if (is_global_selection(reqgroup->orig_sel)) {
    	// All timestep results have the same size in bytes, so offset the
    	// output pointer by however many timesteps precede this timestep
    	const int timestep_within_request = datablock->timestep - reqgroup->from_steps;
		output_buffer = (char*)reqgroup->orig_data + timestep_within_request * reqgroup->orig_sel_timestep_size;
    } else if (reqgroup->orig_sel->type == ADIOS_SELECTION_WRITEBLOCK) {
    	// For writeblock selections, computing the output buffer position for
    	// the current timestep is a bit trickier, since varblocks may be
    	// different sizes in different timesteps
    	const ADIOS_SELECTION_WRITEBLOCK_STRUCT *orig_sel_wb = &reqgroup->orig_sel->u.block;

    	// Compute the offset into the output buffer at which the current timestep's output should go
    	// For absolute writeblocks, it's always 0, since there is only 1 timestep involved. For
    	// timestep-relative writeblock selections, we add the size of the writeblock (i.e. varblock)
    	// for all previous timesteps in the user's read request.
    	uint64_t output_buffer_offset = 0;
    	if (!orig_sel_wb->is_absolute_index) {
    		int timestep;
    		for (timestep = reqgroup->from_steps; timestep < datablock->timestep; timestep++) {
        		// Compute the size of the writeblock at this timestep and add it to our offset
        		output_buffer_offset += compute_selection_size_in_bytes(reqgroup->orig_sel, reqgroup->transinfo->orig_type, timestep, reqgroup->raw_varinfo, reqgroup->transinfo);
        	}
    	}

    	// Offset the output pointer by the computed amount
    	output_buffer = (char*)reqgroup->orig_data + output_buffer_offset;
    } else {
    	adios_error_at_line(err_unspecified, __FILE__, __LINE__, "Internal error: unexpected selection type %d, this should not be possible here\n", reqgroup->orig_sel->type);
    }

    // Now that we have the output buffer pointer, actually perform the patch operation
    const uint64_t used_count =
            apply_datablock_to_buffer_and_free(
                    reqgroup->raw_varinfo, reqgroup->transinfo,
                    datablock,
                    &output_buffer, reqgroup->orig_sel, NULL /* don't need the intersection */,
                    reqgroup->swap_endianness);

    return used_count != 0;
}

/*
 * Takes a datablock containing data potentially applicable to the given read
 * request group, identifies that data (if any), and returns it as an
 * ADIOS_VARCHUNK. Additionally, free the datablock.
 */
static ADIOS_VARCHUNK * apply_datablock_to_chunk_and_free(adios_datablock *datablock, adios_transform_read_request *reqgroup) {
    ADIOS_SELECTION *inter_sel;

    assert(datablock); assert(reqgroup);
    assert(reqgroup->orig_sel);

    if (reqgroup->orig_sel->type != ADIOS_SELECTION_BOUNDINGBOX &&
    	reqgroup->orig_sel->type != ADIOS_SELECTION_POINTS &&
    	reqgroup->orig_sel->type != ADIOS_SELECTION_WRITEBLOCK)
    {
    	adios_error(err_operation_not_supported,
                    "Only read selections of bounding box, points, or writeblock selection types "
                    "are currently allowed (received selection type %d) "
    			    "(NOTE: this should have been caught earlier in the code)\n",
                    reqgroup->orig_sel->type);
    }

    if (datablock->bounds->type != ADIOS_SELECTION_BOUNDINGBOX &&
    	datablock->bounds->type != ADIOS_SELECTION_POINTS &&
    	datablock->bounds->type != ADIOS_SELECTION_WRITEBLOCK)
    {
        adios_error(err_operation_not_supported,
                    "Only results of bounding box, points, or writeblock selection types "
                    "are currently accepted from transform plugins (received selection type %d)\n",
                    datablock->bounds->type);
        abort();
    }

    // Special check due to limitations of ADIOS_VARCHUNK return:
    // If the output is a writeblock selection, AND the input is not, AND
    // the variable is a local array, AND we are required to return a chunk
    // at a time, we must warn the user about potentially unexpected output.
    // Because our results would necessarily be a global-selection-based chunk
    // (subvolume, etc.), which has no information about what PG it came from,
    // if the user submitted multiple writeblock selections over a local array
    // variable at once, there would be no way to determine which writeblock
    // a varchunk with a bounding box selection corresponds to.
    //
    // Potential solutions are: submit only one writeblock per schedule/check/perform
    // cycle, use blocking reads, use a global array file, or use a different data
    // transform.
    if (reqgroup->orig_sel->type == ADIOS_SELECTION_WRITEBLOCK &&
    	datablock->bounds->type != ADIOS_SELECTION_WRITEBLOCK &&
    	!reqgroup->transinfo->orig_global &&
    	adios_transform_read_request_get_mode(reqgroup) == PARTIAL_RESULT_MODE)
    {
    	static int warning_printed = 0;
    	if (!warning_printed) {
    		const char *transform_name = adios_transform_plugin_primary_xml_alias(reqgroup->transinfo->transform_type);
    		if (!transform_name) transform_name = "<name unknown>";
    		log_warn("Results for a chunked read using a writeblock selection over a %s-transformed "
    				"variable will return correct results, but in the form of ADIOS_VARCHUNKs with "
    				"non-writeblock selections, so it may be difficult to determine which VARCHUNK "
    				"goes with which writeblock selection if multiple have been submitted at once. "
    				"To avoid this warning, either use blocking reads, use a global array file, or "
    				"select a use data transform. This warning will only be printed once per run.",
    				transform_name);
    		warning_printed = 1;
    	}
    	return NULL;
    }

    ADIOS_SELECTION *chunk_sel = NULL;
    void *chunk_data = NULL;
    uint64_t used_count =
    	apply_datablock_to_buffer_and_free(
			reqgroup->raw_varinfo, reqgroup->transinfo,
			datablock,
			&chunk_data, reqgroup->orig_sel, &chunk_sel, // chunk_data == NULL -> allocate fitted buffer, chunk_sel == NULL -> return intersection selection
			reqgroup->swap_endianness);

    if (used_count) {
    	assert(chunk_data && chunk_sel);

        // Bind the output buffer to the chunk struct
    	ADIOS_VARCHUNK *chunk = (ADIOS_VARCHUNK *)malloc(sizeof(ADIOS_VARCHUNK));
        *chunk = (ADIOS_VARCHUNK) {
        	.varid = reqgroup->raw_varinfo->varid,
        	.data = chunk_data,
        	.type = datablock->elem_type,
        	.sel = chunk_sel,
        	.from_steps = datablock->timestep,
        	.nsteps = 1,
        };

        return chunk;
    } else {
    	return NULL;
    }
}

static ADIOS_VARCHUNK * extract_chunk_from_finished_read_reqgroup(adios_transform_read_request *reqgroup) {
    assert(reqgroup);
    assert(reqgroup->completed);

    ADIOS_VARCHUNK *chunk = malloc(sizeof(ADIOS_VARCHUNK));
    chunk->varid = reqgroup->raw_varinfo->varid;
    chunk->type = reqgroup->transinfo->orig_type;
    chunk->from_steps = reqgroup->from_steps;
    chunk->nsteps = reqgroup->nsteps;

    // Transfer ownership of orig_data
    chunk->data = reqgroup->orig_data;
    reqgroup->orig_data = NULL;

    // Transfer ownership of orig_sel
    chunk->sel = copy_selection(reqgroup->orig_sel);
    reqgroup->orig_sel = NULL;

    return chunk;
}

void adios_transform_cleanup_from_previous_check_reads(adios_transform_read_request **readreqs_head) {
	adios_transform_read_request *readreq = *readreqs_head;
	while (readreq) {
		adios_transform_read_request *next_readreq = readreq->next;

		if (readreq->completed) {
			// If the read request is totally completed, free the whole thing
			adios_transform_read_request_remove(readreqs_head, readreq);
			adios_transform_read_request_free(&readreq);
		} else if (readreq->lent_varchunk_data) {
			// Otherwise, free any internal data buffer that was previously given
			// to the user via an ADIOS_VARCHUNK, but which now may be freed since
			// the user has called adios_check_reads again
			FREE(readreq->lent_varchunk_data);
		}

		readreq = next_readreq;
	}
}

// Take an ADIOS_VARCHUNK that was just read and process it with the transform
// system. If it was part of a read request corresponding to a transformed
// variable, consume it, and (optionally) replace it with a detransformed chunk.
// Otherwise, do nothing, allowing the calling function to manage it as usual.
void adios_transform_process_read_chunk(adios_transform_read_request **reqgroups_head, ADIOS_VARCHUNK ** chunk) {
    adios_transform_read_request *reqgroup;
    adios_transform_pg_read_request *pg_reqgroup;
    adios_transform_raw_read_request *subreq;
    adios_datablock *result;

    // Find the subrequest that matches the VARCHUNK that was just read (if any)
    int found = adios_transform_read_request_list_match_chunk(*reqgroups_head, *chunk, 1, &reqgroup, &pg_reqgroup, &subreq);

    // If no subrequest matches the VARCHUNK, it must correspond to a non-transformed variable.
    // In this case, return immediately and let it be processed as-is.
    if (!found)
        return;

    // Otherwise, this VARCHUNK corresponds to a subrequest.
    // Therefore, consume it, and perhaps replace it with a detransformed chunk.

    // Consume the chunk, as the data will be proceessed by a transform method to
    // produce a new varchunk, so this varchunk should not be processed by the caller.
    // (NOTE: Freeing this does not free the memory it points to, which we ASSUME
    //  is also pointed to by subreq->data, since currently we REQUIRE transform
    //  plugins to supply their own data buffers when submitting read requests)
    common_read_free_chunk(*chunk);
    *chunk = NULL;

    // Next, update the subreq that corresponds to this VARCHUNK as completed, retrieving any
    // produced result
    result = finish_subreq(reqgroup, pg_reqgroup, subreq);

    // Now, if a new adios_datablock is now available as a result of the above completed subreq,
    // apply it as a result for the user in a way appropriate to the current result mode
    if (result) {
        // Then, return data as appropriate depending on the return mode of this read operation
        //   PARTIAL: no user-allocated buffer is given for the full result, so results must be
        //            returned one VARCHUNK at a time.
        //   FULL: the user has supplied a buffer for full results, so patch relevant data from
        //         the returned VARCHUNK into this buffer.
        enum ADIOS_TRANSFORM_REQGROUP_RESULT_MODE result_mode = adios_transform_read_request_get_mode(reqgroup);
        switch (result_mode) {
        case PARTIAL_RESULT_MODE:
            // Apply this VARCHUNK
            *chunk = apply_datablock_to_chunk_and_free(result, reqgroup);

            // (*chunk)->data points to a buffer allocated by the transform plugin, and which
            // is now given to us to own. Record it here so we can free it once the user is done
            // accessing it (i.e., at the next check_reads or on file close).
            reqgroup->lent_varchunk_data = (*chunk)->data;
            break;
        case FULL_RESULT_MODE:
            apply_datablock_to_result_and_free(result, reqgroup);

            // If the whole variable is now ready, return it as a VARCHUNK
            // Otherwise, return no chunk (NULL)
            if (reqgroup->completed) {
                *chunk = extract_chunk_from_finished_read_reqgroup(reqgroup);
            } else {
                assert(!*chunk); // No chunk to return, and *chunk is already NULL
            }
            break;
        }
    } else {
        assert(!*chunk); // No chunk to return, and *chunk is already NULL
    }
}

/*
 * Process all read reqgroups, assuming they have been fully completed,
 * producing all required results based on the raw data read.
 * (This function is called after a blocking perform_reads completes)
 */
void adios_transform_process_all_reads(adios_transform_read_request **reqgroups_head) {
    // Mark all subrequests, PG request groups and read request groups
    // as completed, calling callbacks as needed
    adios_transform_read_request *reqgroup;
    adios_transform_pg_read_request *pg_reqgroup;
    adios_transform_raw_read_request *subreq;
    adios_datablock *result;

    // Complete each read reqgroup in turn
    while ((reqgroup = adios_transform_read_request_pop(reqgroups_head)) != NULL) {
        // Free leftover read request groups immediately, with no further processing
        if (reqgroup->completed) {
            adios_transform_read_request_free(&reqgroup);
            continue;
        }

        // Complete every child PG reqgroup
        for (pg_reqgroup = reqgroup->pg_reqgroups; pg_reqgroup; pg_reqgroup = pg_reqgroup->next) {
            // Skip completed PG reqgroups
            if (pg_reqgroup->completed) continue;

            // Complete every child subreq
            for (subreq = pg_reqgroup->subreqs; subreq; subreq = subreq->next) {
                // Skip completed subreqs
                if (subreq->completed) continue;

                // Mark the subreq as completed
                adios_transform_raw_read_request_mark_complete(reqgroup, pg_reqgroup, subreq);
                assert(subreq->completed);

                // Make the required call to the transform method to apply the results
                result = adios_transform_subrequest_completed(reqgroup, pg_reqgroup, subreq);
                if (result) apply_datablock_to_result_and_free(result, reqgroup);
            }
            assert(pg_reqgroup->completed);

            // Make the required call to the transform method to apply the results
            result = adios_transform_pg_reqgroup_completed(reqgroup, pg_reqgroup);
            if (result) apply_datablock_to_result_and_free(result, reqgroup);
        }
        assert(reqgroup->completed);

        // Make the required call to the transform method to apply the results
        result = adios_transform_read_reqgroup_completed(reqgroup);
        if (result) apply_datablock_to_result_and_free(result, reqgroup);

        // Now that the read reqgroup has been processed, free it (which also frees all children)
        adios_transform_read_request_free(&reqgroup);
    }
}
