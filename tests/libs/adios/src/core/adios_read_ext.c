/*
 * adios_read_ext.c
 *
 *  Created on: Apr 28, 2014
 *      Author: xczou
 */

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "adios_read_ext.h"
#include "core/common_read.h"
#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_transinfo.h"
#include "core/transforms/adios_transforms_read.h"
#include "core/adios_selection_util.h"
#include "core/adios_infocache.h"

// Ensure unique pointer-based values for each one
const data_view_t LOGICAL_DATA_VIEW = &LOGICAL_DATA_VIEW;
const data_view_t PHYSICAL_DATA_VIEW = &PHYSICAL_DATA_VIEW;

const adios_transform_type_t NO_TRANSFORM = adios_transform_none;

// Sets the "data view" for this ADIOS file, which determines how ADIOS presents variables through
// adios_inq_var*, and how reads are evaluated in adios_schedule_reads/adios_check_reads calls.
// Currently, the choice is between a logical and physical view of the data, which only differ for
// transformed variables; a logical view of a transformed variable presents the data as it was
// originally written (this is the default), whereas a physical view presents the transformed data
// as it actually exists on disk.
data_view_t adios_read_set_data_view(ADIOS_FILE *fp, data_view_t data_view) {
	return common_read_set_data_view(fp, data_view);
}

// Populates data transform information about a given variable into an ADIOS_VARTRANSFORM struct
// Return NULL if failed
ADIOS_VARTRANSFORM *  adios_inq_var_transform(const ADIOS_FILE *fp, const ADIOS_VARINFO *varinfo){
	// Get the global metadata
	ADIOS_TRANSINFO* tinfo = common_read_inq_transinfo(fp, varinfo);
	if (tinfo == NULL)
		return NULL;

	// Get the per-PG metadata
	common_read_inq_trans_blockinfo(fp, varinfo, tinfo);
	if (tinfo->orig_blockinfo == NULL || tinfo->transform_metadatas == NULL)
		return NULL;

	// Load all the metadata into the ADIOS_VARTRANSFORM datastructure
	ADIOS_VARTRANSFORM *vartransform = (ADIOS_VARTRANSFORM*) malloc(sizeof(ADIOS_VARTRANSFORM));
	*vartransform = (ADIOS_VARTRANSFORM){
		.varid = varinfo->varid,
		.sum_nblocks = varinfo->sum_nblocks,
		.transform_type = tinfo->transform_type,
		.should_free_transform_metadata = tinfo->should_free_transform_metadata,
		.transform_metadatas = tinfo->transform_metadatas
	};

	// Transfer ownership of the transform_metadatas array to the new struct, then free the struct
	tinfo->transform_metadatas = NULL;
	common_read_free_transinfo(varinfo, tinfo);

	return vartransform;
}

#define MYFREE(p) {if (p){ free((void*)(p)); (p) = NULL; }}
void adios_free_var_transform(ADIOS_VARTRANSFORM *vartransform) {
	if (vartransform->transform_metadatas) {
		if (vartransform->should_free_transform_metadata) {
			int i;
			for (i = 0; i < vartransform->sum_nblocks; i++)
				MYFREE(vartransform->transform_metadatas[i].content);
		}
		MYFREE(vartransform->transform_metadatas);
	}
	MYFREE(vartransform);
}


void adios_free_pg_intersections(ADIOS_PG_INTERSECTIONS **intersections){
	ADIOS_PG_INTERSECTIONS * intsec = *intersections;
	int i = 0;
	for(i=0; i < intsec->npg; i++){
		ADIOS_PG_INTERSECTION inter = intsec->intersections[i];
		common_read_selection_delete(inter.pg_bounds_sel);
		common_read_selection_delete(inter.intersection_sel);
	}
	intsec->npg = 0;
	intsec->intersections = NULL;
	MYFREE(*intersections);
}

#undef MYFREE

adios_transform_type_t adios_get_transform_type_by_uid(const char *transform_uid) {
	return (adios_transform_type_t)adios_transform_find_type_by_uid(transform_uid);
}

// Creates a writeblock selection that only retrieves elements [start_elem, start_elem + num_elems)
// within a variable. An element is a single value of whatever the varaible's datatype is (i.e.,
// 1 element = 1 double if the variable type is double, 1 byte if the variable type is byte, etc.)
ADIOS_SELECTION * adios_selection_writeblock_bounded(int index, uint64_t start_elem, uint64_t num_elems, int is_timestep_relative) {
	ADIOS_SELECTION *sel = common_read_selection_writeblock(index);
	sel->u.block.is_absolute_index = !is_timestep_relative;
	sel->u.block.is_sub_pg_selection = 1;
	sel->u.block.element_offset = start_elem;
	sel->u.block.nelements = num_elems;
	return sel;
}

/*
 * Determines the block indices corresponding to a start and end timestep.
 * Both the input start/end timesteps and the output start/end blockidx are lower bound inclusive, upper bound exclusive: [start, end)
 */
static void compute_blockidx_range(const ADIOS_VARINFO *raw_varinfo, int from_steps, int to_steps, int *start_blockidx, int *end_blockidx) {
    int blockidx;

    // Find the block index for the start and end timestep
    int curblocks = 0;
    for (blockidx = 0; blockidx < raw_varinfo->nsteps; blockidx++) {
        // Find the start block
        if (blockidx == from_steps) {
            *start_blockidx = curblocks;
        }
        curblocks += raw_varinfo->nblocks[blockidx];
        // Find the end block, then stop
        if (blockidx == to_steps - 1) {
            *end_blockidx = curblocks;
            break;
        }
    }
}

inline static ADIOS_SELECTION * create_pg_bounds(int ndim, ADIOS_VARBLOCK *orig_vb) {
    return common_read_selection_boundingbox(ndim, orig_vb->start, orig_vb->count);
}

int adios_get_absolute_writeblock_index(const ADIOS_VARINFO *varinfo, int timestep_relative_idx, int timestep) {
	int i;
	int absolute_idx = timestep_relative_idx;

	assert(varinfo->blockinfo);

	if (timestep < 0 || timestep >= varinfo->nsteps) {
		adios_error(err_invalid_timestep,
					"Timestep %d out of range (min 0, max %d) (at %s:%s)",
					timestep, varinfo->nsteps, __FILE__, __LINE__);
		return -1;
	}
	if (timestep_relative_idx < 0 || timestep_relative_idx >= varinfo->nblocks[timestep]) {
		adios_error(err_invalid_argument,
					"Writeblock %d out of range for timestep %d (min 0, max %d) (at %s:%s)",
					timestep_relative_idx, timestep, varinfo->nsteps, __FILE__, __LINE__);
		return -1;
	}

	for (i = 0; i < timestep; i++)
		absolute_idx += varinfo->nblocks[i];

	return absolute_idx;
}

#define INITIAL_INTERSECTION_CAPACITY 16;
ADIOS_PG_INTERSECTIONS * adios_find_intersecting_pgs(const ADIOS_FILE *fp, int varid, const ADIOS_SELECTION *sel, const int from_step, const int nsteps) {
    // Declares
    adios_transform_read_request *new_reqgroup;
    int blockidx, timestep, timestep_blockidx;
    int curblocks, start_blockidx, end_blockidx;
    int intersects;
    ADIOS_VARBLOCK *raw_vb, *vb;

    enum ADIOS_FLAG swap_endianness = (fp->endianness == get_system_endianness()) ? adios_flag_no : adios_flag_yes;
    int to_steps = from_step + nsteps;

    // As long as we don't free/destroy it, using the infocache from the file will have no effect on future
    // operations using the file (except possibly speeding them up, so "constness" is still respected
    adios_infocache *infocache = common_read_get_file_infocache((ADIOS_FILE*)fp);

    ADIOS_PG_INTERSECTIONS *resulting_intersections = (ADIOS_PG_INTERSECTIONS *)calloc(1, sizeof(ADIOS_PG_INTERSECTIONS));
    resulting_intersections->npg = 0;

    int intersection_capacity = INITIAL_INTERSECTION_CAPACITY;
    resulting_intersections->intersections = (ADIOS_PG_INTERSECTION *)calloc(intersection_capacity, sizeof(ADIOS_PG_INTERSECTION));

    // Precondition checking
    if (sel->type != ADIOS_SELECTION_BOUNDINGBOX &&
        sel->type != ADIOS_SELECTION_POINTS) {
        adios_error(err_operation_not_supported, "Only bounding box and point selections are currently supported during read on transformed variables.");
    }

    // Still respecting constness, since we're going to undo this
    const data_view_t old_view = adios_read_set_data_view((ADIOS_FILE*)fp, LOGICAL_DATA_VIEW); // Temporarily go to logical data view

    const ADIOS_VARINFO *varinfo = adios_infocache_inq_varinfo(fp, infocache, varid);
    assert(from_step >= 0 && to_steps <= varinfo->nsteps);

    // Compute the blockidx range, given the timesteps
    compute_blockidx_range(varinfo, from_step, to_steps, &start_blockidx, &end_blockidx);

    // Retrieve blockinfos, if they haven't been done retrieved
    if (!varinfo->blockinfo)
    	common_read_inq_var_blockinfo(fp, (ADIOS_VARINFO *)varinfo);

    // Undoing view set (returning to const state)
    adios_read_set_data_view((ADIOS_FILE*)fp, old_view); // Reset the data view to whatever it was before

    // Assemble read requests for each varblock
    blockidx = start_blockidx;
    timestep = from_step;
    timestep_blockidx = 0;

    while (blockidx != end_blockidx) { //for (blockidx = startblock_idx; blockidx != endblock_idx; blockidx++) {
        ADIOS_SELECTION *pg_bounds_sel;
        ADIOS_SELECTION *pg_intersection_sel;

        vb = &varinfo->blockinfo[blockidx];
        pg_bounds_sel = create_pg_bounds(varinfo->ndim, vb);

        // Find the intersection, if any
        pg_intersection_sel = adios_selection_intersect_global(pg_bounds_sel, sel);
        if (pg_intersection_sel) {
        	// Expand the PG intersection array, if needed
        	if (resulting_intersections->npg == intersection_capacity) {
        		intersection_capacity *= 2;
        		resulting_intersections->intersections = (ADIOS_PG_INTERSECTION *)realloc(resulting_intersections->intersections, intersection_capacity * sizeof(ADIOS_PG_INTERSECTION));

        		if (!resulting_intersections->intersections) {
        			adios_error (err_no_memory, "Cannot allocate buffer for PG intersection results in adios_find_intersecting_pgs (required %llu bytes)\n", intersection_capacity * sizeof(ADIOS_PG_INTERSECTION));
        			return NULL;
        		}
        	}

        	ADIOS_PG_INTERSECTION *intersection = &resulting_intersections->intersections[resulting_intersections->npg];
        	intersection->timestep = timestep;
        	intersection->blockidx = blockidx;
        	intersection->blockidx_in_timestep = timestep_blockidx;
        	intersection->intersection_sel = pg_intersection_sel;
        	intersection->pg_bounds_sel = pg_bounds_sel;

        	resulting_intersections->npg++;
        } else {
            // Cleanup
            common_read_selection_delete(pg_bounds_sel); // OK to delete, because this function only frees the outer struct, not the arrays within
        }

        // Increment block indexes
        blockidx++;
        timestep_blockidx++;
        if (timestep_blockidx == varinfo->nblocks[timestep]) {
            timestep_blockidx = 0;
            timestep++;
        }
    }

    return resulting_intersections;
}


// What is the dimension order of arrays in the file?
// 0: C ordering (row-major), last dimension is the fastest dimension
// 1: Fortran ordering (column-major), first dimension is the fastest dimension
int adios_read_get_dimension_order (ADIOS_FILE * fp)
{
    return common_read_get_dimension_order (fp);
}




