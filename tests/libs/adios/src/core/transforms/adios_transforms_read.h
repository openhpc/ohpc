/*
 * Contains read-specific code for handling variable transforms in ADIOS
 *
 *  Created on: Jun 27, 2012
 *      Author: David A. Boyuka II
 */

#ifndef ADIOS_TRANSFORMS_READ_H_
#define ADIOS_TRANSFORMS_READ_H_

#include "public/adios_error.h"
#include "public/adios_types.h"
#include "public/adios_read_v2.h"
#include "core/adios_subvolume.h"
#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_reqgroup.h"

//
// Read request inspection
//

/*
 * The possible modes in which a read request may be required to
 * return results to the user.
 */
enum ADIOS_TRANSFORM_REQGROUP_RESULT_MODE {
    // The user has supplied a data buffer that can contain the entire result, so populate it in place.
    // Successful completion of the read implies this buffer is fully populated.
    FULL_RESULT_MODE,

    // The user has specified no data buffer, and requests the data to be returned in chunks. Return
    // an ADIOS_VARCHUNK whenever possible, using as little memory as possible.
    // TODO: there is no respect for the user's memory limit in this mode; currently, the transform
    // read layer allocates as much memory as it needs, and there are not many out-of-memory checks.
    PARTIAL_RESULT_MODE
};

/*
 * Used by the transform read layer, this function returns how the given read request is expected
 * to return its results to the user (i.e., one of the options from the above struct).
 */
enum ADIOS_TRANSFORM_REQGROUP_RESULT_MODE adios_transform_read_request_get_mode(const adios_transform_read_request *reqgroup);

//
// BLOCKINFO inspection
//

/*
 * Examines the "raw" BLOCKINFO from some transformed PG, and returns the size (in bytes)
 * of the transformed data contained within that PG.
 */
uint64_t adios_transform_get_transformed_var_size_from_blockinfo(int raw_ndim, const ADIOS_VARBLOCK *raw_block);

//
// Read request handling
//

/*
 * Generates an adios_transform_read_request containing all byte-segment reads to be passed to
 * the read transport layer. Internally, it delegates to the transform plugin read side to
 * generate the actual byte segments. This function performs culling based on PG bounds, and
 * passes user-selection-intersecting regions to the plugin when requesting byte-segment reads.
 */
adios_transform_read_request * adios_transform_generate_read_reqgroup(const ADIOS_VARINFO *vi, const ADIOS_TRANSINFO* ti, const ADIOS_FILE *fp,
                                                                      const ADIOS_SELECTION *sel, int from_steps, int nsteps, const char *param, void *data);

/*
 * Processes a VARCHUNK just returned by the read layer against the given list of outstanding transform
 * read requests.
 */
void adios_transform_process_read_chunk(adios_transform_read_request **reqgroups_head, ADIOS_VARCHUNK ** chunk);

/*
 * Processes all data after a blocking read that has serviced all read requests,
 * completing all transform read requests.
 */
void adios_transform_process_all_reads(adios_transform_read_request **reqgroups_head);

#endif /* ADIOS_TRANSFORMS_READ_H_ */
