/*
 * adios_transforms_reqgroup.c
 *
 * All node types from the transform read request tree are implemented here.
 *
 * Note on use of PREPEND rather than APPEND when adding read requests: the ultimate
 * goal is to ensure all raw_read_requests are scheduled with the I/O transport layer
 * according to the pre-order traversal of *how they were added* (not necessarily on
 * actual tree). For instance, if pg_reqgroup A is added before pg_reqgroup B, then
 * all raw_read_requests of A should be scheduled before those of B, and within these
 * groups, in the same order they were appended.
 *
 * However, currently the Read BP transport method stores read requests in *reverse order*
 * from how they are scheduled (internally, it does a list prepend). Thus, the simplest way
 * to counteract this is to also use list prepends in the read request tree, causing a
 * pre-order traversal to yield raw_read_requests in reverse order. When they are scheduled
 * in this order, they will be re-reversed, thus finally ending up in the correct order.
 *
 * This problem could also be solved by maintaining list appends in the tree, and then
 * reversing the list orders before scheduling, or simply iterating over the tree in
 * post-order traversal (although this would require either reworking the singly-linked
 * list structure, or using a fancy algorithm that reverses the list, then reverses it
 * again during iteration). For now, this approach is the safest and most straightforward.
 *
 *
 *  Created on: Jul 30, 2012
 *      Author: David A. Boyuka II
 */

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "core/transforms/adios_transforms_hooks_read.h"
#include "core/transforms/adios_transforms_reqgroup.h"
#include "core/common_read.h"
#include "core/adios_subvolume.h"
#include "adios_selection.h"

// An adios_transform_read_request corresponds to a variable read request
// An adios_transform_pg_read_request corresponds to the portion of a read request intersecting a PG
// An adios_transform_raw_read_request corresponds to a single raw byte range read within a PG

// adios_transform_raw_read_request owns ->sel and ->data (both will be free'd)
// adios_transform_pg_read_request owns ->pg_selection and ->pg_intersection_to_global_copyspec (both will be free'd)
// adios_transform_read_request owns ->orig_sel, ->varinfo and ->transinfo (all will be free'd)

// Also, in all cases, ->transform_internal will be free'd if it is non-NULL
// Thus, it is best to keep only a single level malloc in transform_internal (no pointers to other mallocs)
// If this is not possible, you should free() malloced memory yourself as soon as it is no longer needed

#define MYFREE(p) {if (p) free(p); (p)=NULL;}

// Generic list manipulation
// Assumes the list node struct has a ->next field

#define LIST_APPEND(head, elem, elem_type) \
    if (!(head)) {                         \
        (head) = (elem);                   \
    } else {                               \
        elem_type *_cur = (head);          \
        while (_cur->next)                 \
            _cur = _cur->next;             \
        _cur->next = (elem);               \
    }                                      \

#define LIST_PREPEND(head, elem, elem_type) \
    if (!(head)) {                          \
        (head) = (elem);                    \
    } else {                                \
        (elem)->next = (head);              \
        (head) = elem;                      \
    }                                       \

#define LIST_REMOVE(head, elem, elem_type, removed) \
    LIST_REMOVE_PRED(head, (_cur == (elem)), elem_type, removed)

#define LIST_REMOVE_KEY(head, key, keyfield, elem_type, removed) \
    LIST_REMOVE_PRED(head, (_cur->keyfield == (key)), elem_type, removed)

#define LIST_REMOVE_PRED(head, pred, elem_type, removed)    \
    if (!(head)) {                                    \
        removed = 0;                                \
    } else {                                        \
        elem_type *_prev = NULL;                    \
        elem_type *_cur = (head);                    \
        while (_cur) {                                \
            if (pred)                                \
                break;                                \
            _prev = _cur;                            \
            _cur = _cur->next;                        \
        }                                            \
        if (!_cur) {                                \
            removed = NULL;                            \
        } else {                                    \
            if (!_prev) {                            \
                (head) = (head)->next;                \
            } else {                                \
                _prev->next = _cur->next;            \
            }                                        \
            _cur->next = NULL;                        \
            removed = _cur;                            \
        }                                            \
    }

// Utility functions

static int common_adios_selection_equal(ADIOS_SELECTION *sel1, ADIOS_SELECTION *sel2) {
    if (sel1->type != sel2->type)
        return 0;

    switch (sel1->type) {
    case ADIOS_SELECTION_BOUNDINGBOX:
    {
        ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb1 = &sel1->u.bb;
        ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb2 = &sel2->u.bb;
        return (bb1->ndim == bb2->ndim) &&
               memcmp(bb1->start, bb2->start, bb1->ndim * sizeof(uint64_t)) == 0 &&
               memcmp(bb1->count, bb2->count, bb1->ndim * sizeof(uint64_t)) == 0;
    }
    case ADIOS_SELECTION_WRITEBLOCK:
    {
        ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb1 = &sel1->u.block;
        ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb2 = &sel2->u.block;
        return (wb1->index == wb2->index) &&
               (wb1->is_absolute_index == wb2->is_absolute_index) &&
               (wb1->is_sub_pg_selection == wb2->is_sub_pg_selection) &&
               (!wb1->is_sub_pg_selection || (
                    (wb1->element_offset == wb2->element_offset) &&
                    (wb1->nelements == wb2->nelements)
               ));
    }
    default:
        adios_error(err_operation_not_supported, "Selection types other than bounding box not supported in %s\n", __FUNCTION__);
        return 0;
    }
}

//
// adios_transform_read_subreq struct manipulation
//

adios_transform_raw_read_request * adios_transform_raw_read_request_new(ADIOS_SELECTION *sel, void *data) {
    adios_transform_raw_read_request *new_subreq = malloc(sizeof(adios_transform_raw_read_request));
    new_subreq->raw_sel = sel;
    new_subreq->data = data;
    new_subreq->completed = 0;
    new_subreq->transform_internal = 0;
    new_subreq->next = 0;
    return new_subreq;
}

// Define to use the new writeblock-based raw read interface to the I/O transport layer
// NOTE: Support for NOT using this option (i.e., non-WRITEBLOCK reads) is discontinued, and
// disabling this option will certainly lead to errors!
// TODO: Totally remove this option, leaving only the WRITEBLOCK method, once it has been tested
// and confirmed working/viable
#define RAW_READS_USE_WRITEBLOCK

adios_transform_raw_read_request * adios_transform_raw_read_request_new_byte_segment(const adios_transform_pg_read_request *pg_reqgroup, uint64_t start, uint64_t count, void *data) {
    ADIOS_SELECTION *sel;

#ifdef RAW_READS_USE_WRITEBLOCK
    ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb;

    // NOTE: We use the absolute PG index, along with the is_absolute_index flag below
    sel = common_read_selection_writeblock(pg_reqgroup->blockidx);

    wb = &sel->u.block;
    wb->is_absolute_index = 1;
    wb->is_sub_pg_selection = 1;
    wb->element_offset = start; // Assume element type of the raw variable is byte
    wb->nelements = count;

    // Catch any bugs that cause wrap-around early:
    assert(start <= pg_reqgroup->raw_var_length);
    assert(count <= pg_reqgroup->raw_var_length);
    assert(start + count <= pg_reqgroup->raw_var_length);
#else
    const ADIOS_VARBLOCK *raw_varblock = pg_reqgroup->raw_varblock;
    uint64_t *start_sel, *count_sel;

    // TODO: Move this bounding box construction to a separate function?
    start_sel = (uint64_t*)malloc(2 * sizeof(uint64_t));
    count_sel = (uint64_t*)malloc(2 * sizeof(uint64_t));
    // PG ID dim
    start_sel[0] = raw_varblock->start[0]; // PG ID
    count_sel[0] = 1;
    // Buffer dim
    start_sel[1] = start;
    count_sel[1] = count;

    // Transfer ownership of the our start/count vectors
    sel = common_read_selection_boundingbox(2, start_sel, count_sel);
    start_sel = count_sel = NULL;
#endif

    return adios_transform_raw_read_request_new(sel, data);
}

adios_transform_raw_read_request * adios_transform_raw_read_request_new_whole_pg(const adios_transform_pg_read_request *pg_reqgroup, void *data) {
#ifdef RAW_READS_USE_WRITEBLOCK
    // Use absolute time index, but not sub-PG read
    ADIOS_SELECTION *sel = common_read_selection_writeblock(pg_reqgroup->blockidx);
    sel->u.block.is_absolute_index = 1;
    return adios_transform_raw_read_request_new(sel, data);
#else
    // NEW: raw_varblock is always 1D, with that dimension being byte length
    // OLD, NO LONGER APPLICABLE: raw_varblock has two dimensions: PG ID and byte offset. Thus, the length of this (raw) PG is the length of the 2nd dimension.
    const ADIOS_VARBLOCK *raw_varblock = pg_reqgroup->raw_varblock;
    return adios_transform_raw_read_request_new_byte_segment(pg_reqgroup, 0, raw_varblock->count[1], data);
#endif
}

void adios_transform_raw_read_request_mark_complete(adios_transform_read_request *parent_reqgroup, adios_transform_pg_read_request *parent_pg_reqgroup,
                                          adios_transform_raw_read_request *subreq) {
    if (subreq->completed)
        return;

    subreq->completed = 1;
    parent_pg_reqgroup->num_completed_subreqs++;

    if (parent_pg_reqgroup->num_completed_subreqs == parent_pg_reqgroup->num_subreqs) {
        parent_pg_reqgroup->completed = 1;
        parent_reqgroup->num_completed_pg_reqgroups++;

        if (parent_reqgroup->num_completed_pg_reqgroups == parent_reqgroup->num_pg_reqgroups) {
            parent_reqgroup->completed = 1;
        }
    }
}

// NOTE: MUST have removed the subrequest from the PG request group BEFORE calling this
void adios_transform_raw_read_request_free(adios_transform_raw_read_request **subreq_ptr) {
    adios_transform_raw_read_request *subreq = *subreq_ptr;
    assert(!subreq->next); // Not a perfect check, but will catch many requests that are still linked

    // Free malloc'd resources
    common_read_selection_delete(subreq->raw_sel);
    MYFREE(subreq->data);
    MYFREE(subreq->transform_internal);

    // Clear all data to 0's for safety
    memset(subreq, 0, sizeof(adios_transform_raw_read_request));

    // Free the entire struct and the user's pointer to it
    MYFREE(*subreq_ptr);
}

//
// adios_transform_pg_reqgroup struct manipulation
//

adios_transform_pg_read_request * adios_transform_pg_read_request_new(
        int timestep, int timestep_blockidx, int blockidx,
        int orig_ndim, int raw_ndim,
        const ADIOS_VARBLOCK *orig_varblock,
        const ADIOS_VARBLOCK *raw_varblock,
        const ADIOS_SELECTION *pg_intersection_sel,
        const ADIOS_SELECTION *pg_bounds_sel,
        const void *transform_metadata,
        uint16_t transform_metadata_len) {

    adios_transform_pg_read_request *new_pg_reqgroup;

    assert(orig_varblock);
    assert(blockidx >= 0);

    new_pg_reqgroup = calloc(sizeof(adios_transform_pg_read_request), 1);
    new_pg_reqgroup->timestep = timestep;
    new_pg_reqgroup->blockidx_in_timestep = timestep_blockidx;
    new_pg_reqgroup->blockidx = blockidx;
    new_pg_reqgroup->raw_var_length = adios_transform_get_transformed_var_size_from_blockinfo(raw_ndim, raw_varblock); //raw_varblock->count[0];
    new_pg_reqgroup->raw_ndim = raw_ndim;
    new_pg_reqgroup->orig_ndim = orig_ndim;
    new_pg_reqgroup->raw_varblock = raw_varblock;
    new_pg_reqgroup->orig_varblock = orig_varblock;
    new_pg_reqgroup->pg_intersection_sel = pg_intersection_sel;
    new_pg_reqgroup->pg_bounds_sel = pg_bounds_sel;
    new_pg_reqgroup->transform_metadata = transform_metadata;
    new_pg_reqgroup->transform_metadata_len = transform_metadata_len;

    ADIOS_SELECTION *wbsel = common_read_selection_writeblock(blockidx);
    wbsel->u.block.is_absolute_index = 1;
    new_pg_reqgroup->pg_writeblock_sel = wbsel;

    // Other fields are 0'd

    return new_pg_reqgroup;
}

void adios_transform_raw_read_request_append(adios_transform_pg_read_request *pg_reqgroup, adios_transform_raw_read_request *subreq) {
    LIST_PREPEND(pg_reqgroup->subreqs, subreq, adios_transform_raw_read_request);
    pg_reqgroup->num_subreqs++;
}

int adios_transform_raw_read_request_remove(adios_transform_pg_read_request *pg_reqgroup, adios_transform_raw_read_request *subreq) {
    adios_transform_raw_read_request *removed;
    LIST_REMOVE(pg_reqgroup->subreqs, subreq, adios_transform_raw_read_request, removed);

    if (removed) pg_reqgroup->num_subreqs--;
    return removed != NULL;
}

adios_transform_raw_read_request * adios_transform_raw_read_request_pop(adios_transform_pg_read_request *pg_reqgroup) {
    adios_transform_raw_read_request *to_remove = pg_reqgroup->subreqs;
    if (adios_transform_raw_read_request_remove(pg_reqgroup, to_remove))
        return to_remove;
    else
        return NULL;
}

void adios_transform_pg_read_request_free(adios_transform_pg_read_request **pg_reqgroup_ptr) {
    adios_transform_pg_read_request *pg_reqgroup = *pg_reqgroup_ptr;
    adios_transform_raw_read_request *removed_subreq;

    assert(!pg_reqgroup->next);

    // Free any remaining subrequests
    while ((removed_subreq = adios_transform_raw_read_request_pop(pg_reqgroup)) != NULL) {
        adios_transform_raw_read_request_free(&removed_subreq);
    }

    // Free malloc'd resources
    if (pg_reqgroup->pg_intersection_sel)
        common_read_selection_delete((ADIOS_SELECTION*)pg_reqgroup->pg_intersection_sel);
    if (pg_reqgroup->pg_bounds_sel)
        common_read_selection_delete((ADIOS_SELECTION*)pg_reqgroup->pg_bounds_sel);
    if (pg_reqgroup->pg_writeblock_sel)
    	common_read_selection_delete((ADIOS_SELECTION*)pg_reqgroup->pg_writeblock_sel);
    MYFREE(pg_reqgroup->transform_internal);

    // Clear all data to 0's for safety
    memset(pg_reqgroup, 0, sizeof(adios_transform_pg_read_request));
    // Free the entire struct and the user's pointer to it
    MYFREE(*pg_reqgroup_ptr);
}

//
// adios_transform_read_reqgroup struct manipulation
//

adios_transform_read_request * adios_transform_read_request_new(
        const ADIOS_FILE *fp, const ADIOS_VARINFO *varinfo, const ADIOS_TRANSINFO *transinfo,
        const ADIOS_SELECTION *sel, int from_steps, int nsteps,
        const char *param,
        void *data, enum ADIOS_FLAG swap_endianness) {

    adios_transform_read_request *new_reqgroup;
    assert(fp); assert(varinfo); assert(transinfo);
    assert(nsteps > 0);

    new_reqgroup = calloc(sizeof(adios_transform_read_request), 1);
    new_reqgroup->fp = fp;
    new_reqgroup->raw_varinfo = varinfo;
    new_reqgroup->transinfo = transinfo;

    new_reqgroup->from_steps = from_steps;
    new_reqgroup->nsteps = nsteps;
    new_reqgroup->orig_sel = copy_selection(sel);
    new_reqgroup->read_param = param;
    new_reqgroup->orig_data = data;
    new_reqgroup->swap_endianness = swap_endianness;

    // orig_sel_timestep_size is not meaningful for a writeblock selection, since a
    // writeblock selection may "change size" depending on which timestep is considered
    if (sel->type != ADIOS_SELECTION_WRITEBLOCK) {
		new_reqgroup->orig_sel_timestep_size = compute_selection_size(sel) *
											   common_read_type_size(transinfo->orig_type, NULL);
    }

    // Other fields are 0'd

    return new_reqgroup;
}

void adios_transform_pg_read_request_append(adios_transform_read_request *reqgroup, adios_transform_pg_read_request *pg_reqgroup) {
    LIST_PREPEND(reqgroup->pg_reqgroups, pg_reqgroup, adios_transform_pg_read_request);
    reqgroup->num_pg_reqgroups++;
}

int adios_transform_pg_read_request_remove(adios_transform_read_request *reqgroup, adios_transform_pg_read_request *pg_reqgroup) {
    adios_transform_pg_read_request *removed;
    LIST_REMOVE(reqgroup->pg_reqgroups, pg_reqgroup, adios_transform_pg_read_request, removed);

    if (removed) reqgroup->num_pg_reqgroups--;
    return removed != NULL;
}

adios_transform_pg_read_request * adios_transform_pg_read_request_pop(adios_transform_read_request *reqgroup) {
    adios_transform_pg_read_request *to_remove = reqgroup->pg_reqgroups;
    if (adios_transform_pg_read_request_remove(reqgroup, to_remove))
        return to_remove;
    else
        return NULL;
}

void adios_transform_read_request_append(adios_transform_read_request **head, adios_transform_read_request *new_reqgroup) {
    LIST_PREPEND(*head, new_reqgroup, adios_transform_read_request);
}

adios_transform_read_request * adios_transform_read_request_remove(adios_transform_read_request **head, adios_transform_read_request *reqgroup) {
    adios_transform_read_request *removed;
    LIST_REMOVE(*head, reqgroup, adios_transform_read_request, removed);
    return removed;
}

adios_transform_read_request * adios_transform_read_request_pop(adios_transform_read_request **head) {
    adios_transform_read_request *to_remove = *head;
    if (adios_transform_read_request_remove(head, to_remove))
        return to_remove;
    else
        return NULL;
}

void adios_transform_read_request_free(adios_transform_read_request **reqgroup_ptr) {
    adios_transform_read_request *reqgroup = *reqgroup_ptr;
    adios_transform_pg_read_request *removed_pg_reqgroup;

    assert(!reqgroup->next);

    // Free any remaining subrequests
    while ((removed_pg_reqgroup = adios_transform_pg_read_request_pop(reqgroup)) != NULL) {
        adios_transform_pg_read_request_free(&removed_pg_reqgroup);
    }

    // Free malloc'd resources

    // Free any data buffer lent to the user, but don't free the VARCHUNK; that
    // should have been done already by the user
    if (reqgroup->lent_varchunk_data)
        MYFREE(reqgroup->lent_varchunk_data);

    common_read_selection_delete((ADIOS_SELECTION*)reqgroup->orig_sel); // Remove const

    // DON'T FREE varinfo/transinfo, since they are stored in the infocache
    // common_read_free_transinfo(reqgroup->raw_varinfo, (ADIOS_TRANSINFO*)reqgroup->transinfo); // Remove const
    // common_read_free_varinfo((ADIOS_VARINFO*)reqgroup->raw_varinfo); // Remove const

    MYFREE(reqgroup->transform_internal);

    // Clear all data to 0's for safety
    memset(reqgroup, 0, sizeof(adios_transform_read_request));
    // Free the entire struct and the user's pointer to it
    MYFREE(*reqgroup_ptr);
}

//
// Chunk matching code
//

static int adios_transform_pg_read_request_match_chunk(const adios_transform_pg_read_request *pg_reqgroup,
                                                       const ADIOS_VARCHUNK *chunk, int skip_completed,
                                                       adios_transform_raw_read_request **matching_subreq) {

    adios_transform_raw_read_request *cur;

    for (cur = pg_reqgroup->subreqs; cur; cur = cur->next) {
        if (skip_completed && cur->completed)
            continue;

        if (common_adios_selection_equal(cur->raw_sel, chunk->sel))
            break;
    }

    // cur will be NULL if none matched
    *matching_subreq = cur;
    return cur != NULL;
}

static int adios_transform_read_request_match_chunk(const adios_transform_read_request *reqgroup,
                                                    const ADIOS_VARCHUNK *chunk, int skip_completed,
                                                    adios_transform_pg_read_request **matching_pg_reqgroup,
                                                    adios_transform_raw_read_request **matching_subreq) {

    adios_transform_pg_read_request *cur;

    if (reqgroup->raw_varinfo->varid != chunk->varid)
        return 0;

    int found = 0;
    // Search all PG request groups
    for (cur = reqgroup->pg_reqgroups; cur; cur = cur->next) {
        // Skip completed PG reqgroups if required
        if (skip_completed && cur->completed)
            continue;

        // Skip PG reqgroups that are for other timesteps
        if (cur->timestep != chunk->from_steps)
            continue;

        // Delegate remaining search to the PG regroup
        found = adios_transform_pg_read_request_match_chunk(cur, chunk, skip_completed, matching_subreq);
        if (found)
            break;
    }

    // cur will be NULL if nothing matched
    *matching_pg_reqgroup = cur;
    return found;
}

int adios_transform_read_request_list_match_chunk(const adios_transform_read_request *reqgroup_head,
                                                  const ADIOS_VARCHUNK *chunk, int skip_completed,
                                                  adios_transform_read_request **matching_reqgroup,
                                                  adios_transform_pg_read_request **matching_pg_reqgroup,
                                                  adios_transform_raw_read_request **matching_subreq) {
    int found;
    adios_transform_read_request *cur;
    for (cur = (adios_transform_read_request *)reqgroup_head; cur; cur = cur->next) {
        found = adios_transform_read_request_match_chunk(cur, chunk, skip_completed, matching_pg_reqgroup, matching_subreq);
        if (found)
            break;
    }

    *matching_reqgroup = cur;
    return found;
}
