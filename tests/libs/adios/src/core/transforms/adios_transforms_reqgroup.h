/*
 * adios_transformed_reqgroup.h
 *
 *  Created on: Jul 30, 2012
 *      Author: David A. Boyuka II
 */

#ifndef ADIOS_TRANSFORMS_REQGROUP_H_
#define ADIOS_TRANSFORMS_REQGROUP_H_

#include <stdint.h>
#include "public/adios_read_v2.h"
#include "public/adios_types.h"
#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_transinfo.h"

//
// Read request-related structs:
//
// adios_transform_read_request
//   adios_transform_pg_read_request
//     adios_transform_raw_read_request
//

typedef struct _adios_transform_raw_read_request {
    int             completed; // Whether this request has been completed

    ADIOS_SELECTION *raw_sel; // The raw selection to pose to the read layer
    void            *data;

    void            *transform_internal; // Transform plugin private

    // Linked list
    struct _adios_transform_raw_read_request *next;
} adios_transform_raw_read_request;

typedef struct _adios_transform_pg_read_request {
    int completed; // Whether this request has been completed

    // PG information
    int timestep;                        // The timestep to which this PG belongs
    int blockidx_in_timestep;            // The block ID of this PG within the timestep
    int blockidx;                        // The block ID of this PG within the variable
    uint64_t raw_var_length;             // Transformed variable data length, in bytes
    int raw_ndim;                        // Number of dimensions in the raw (byte array) dimensions (should always be 1)
    int orig_ndim;                       // Number of dimensions in the original (user-view) dimensions
    const ADIOS_VARBLOCK *raw_varblock;  // Points into adios_transform_read_reqgroup->varinfo->blockinfo; do not free here
    const ADIOS_VARBLOCK *orig_varblock; // Points into adios_transform_read_reqgroup->transinfo->orig_blockinfo; do not free here
    const void *transform_metadata;      // Transform metadata associated with this PG
    uint16_t transform_metadata_len;     // Length of the transform metadata associated with this PG

    // Various selections to aid in datablock construction
    const ADIOS_SELECTION *pg_intersection_sel;
    const ADIOS_SELECTION *pg_bounds_sel;
    const ADIOS_SELECTION *pg_writeblock_sel;

    // Subrequests
    int num_subreqs;
    int num_completed_subreqs;
    adios_transform_raw_read_request *subreqs;

    void *transform_internal; // Transform plugin private

    // Linked list
    struct _adios_transform_pg_read_request *next;
} adios_transform_pg_read_request;

typedef struct _adios_transform_read_request {
    int completed; // Whether this request has been completed

    ADIOS_VARCHUNK *lent_varchunk_data; // The data buffer of the last ADIOS_VARCHUNK passed to the user.
                                        // The user is responsible for cleaning up the VARCHUNK itself, but
                                        // we must free this buffer ourselves each time a new check_reads is
                                        // called (or the file is closed)

    const ADIOS_FILE        *fp;

    const ADIOS_VARINFO     *raw_varinfo;
    const ADIOS_TRANSINFO   *transinfo;
    enum ADIOS_FLAG         swap_endianness;

    // Original selection info
    int                     from_steps;
    int                     nsteps;
    const ADIOS_SELECTION   *orig_sel;  // Global space
    const char              *read_param; // Special read parameter passed by user, to be interpreted by the transform method
    void                    *orig_data; // User buffer supplied in schedule_reads (could be NULL)

    // Number of bytes in data within the selection per timestep
    uint64_t                orig_sel_timestep_size;

    // Child PG read requests
    int num_pg_reqgroups;
    int num_completed_pg_reqgroups;
    adios_transform_pg_read_request *pg_reqgroups;

    void *transform_internal; // Transform plugin private

    // Linked list
    struct _adios_transform_read_request *next;
} adios_transform_read_request;

//
// adios_transform_raw_read_request manipulation
//
adios_transform_raw_read_request * adios_transform_raw_read_request_new(ADIOS_SELECTION *sel, void *data);
adios_transform_raw_read_request * adios_transform_raw_read_request_new_byte_segment(const adios_transform_pg_read_request *pg_reqgroup, uint64_t start, uint64_t count, void *data);
adios_transform_raw_read_request * adios_transform_raw_read_request_new_whole_pg(const adios_transform_pg_read_request *pg_reqgroup, void *data);
void adios_transform_raw_read_request_free(adios_transform_raw_read_request **subreq_ptr);

void adios_transform_raw_read_request_mark_complete(adios_transform_read_request *regroup_parent,
                                                    adios_transform_pg_read_request *parent_pg_reqgroup,
                                                    adios_transform_raw_read_request *subreq);

void adios_transform_raw_read_request_append(adios_transform_pg_read_request *pg_reqgroup, adios_transform_raw_read_request *subreq);
int adios_transform_raw_read_request_remove(adios_transform_pg_read_request *pg_reqgroup, adios_transform_raw_read_request *subreq);
adios_transform_raw_read_request * adios_transform_raw_read_request_pop(adios_transform_pg_read_request *pg_reqgroup);

//
// adios_transform_pg_read_request manipulation
//
adios_transform_pg_read_request * adios_transform_pg_read_request_new(int timestep, int timestep_blockidx, int blockidx,
                                                                      int orig_ndim, int raw_ndim,
                                                                      const ADIOS_VARBLOCK *orig_varblock,
                                                                      const ADIOS_VARBLOCK *raw_varblock,
                                                                      const ADIOS_SELECTION *pg_intersection_sel,
                                                                      const ADIOS_SELECTION *pg_bounds_sel,
                                                                      const void *transform_metadata,
                                                                      uint16_t transform_metadata_len);
void adios_transform_pg_read_request_free(adios_transform_pg_read_request **pg_reqgroup_ptr);

void adios_transform_pg_read_request_append(adios_transform_read_request *reqgroup, adios_transform_pg_read_request *pg_reqgroup);
int adios_transform_pg_read_request_remove(adios_transform_read_request *reqgroup, adios_transform_pg_read_request *pg_reqgroup);
adios_transform_pg_read_request * adios_transform_pg_read_request_pop(adios_transform_read_request *reqgroup);

//
// adios_transform_read_reqgroup manipulation
//
adios_transform_read_request * adios_transform_read_request_new(const ADIOS_FILE *fp, const ADIOS_VARINFO *varinfo,
                                                                  const ADIOS_TRANSINFO *transinfo,
                                                                  const ADIOS_SELECTION *sel, int from_steps, int nsteps,
                                                                  const char *param,
                                                                  void *data, enum ADIOS_FLAG swap_endianness);
void adios_transform_read_request_free(adios_transform_read_request **reqgroup_ptr);

void adios_transform_read_request_append(adios_transform_read_request **head, adios_transform_read_request *new_reqgroup);
adios_transform_read_request * adios_transform_read_request_remove(adios_transform_read_request **head, adios_transform_read_request *reqgroup);
adios_transform_read_request * adios_transform_read_request_pop(adios_transform_read_request **head);

int adios_transform_read_request_list_match_chunk(const adios_transform_read_request *reqgroup_head,
                                               const ADIOS_VARCHUNK *chunk, int skip_completed,
                                                  adios_transform_read_request **matching_reqgroup,
                                                  adios_transform_pg_read_request **matching_pg_reqgroup,
                                                  adios_transform_raw_read_request **matching_subreq);

#endif /* ADIOS_TRANSFORMS_REQGROUP_H_ */
