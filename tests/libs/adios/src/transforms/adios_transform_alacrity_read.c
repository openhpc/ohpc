#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <assert.h>

#include "core/util.h"
#include "core/adios_logger.h"
#include "core/transforms/adios_transforms_hooks_read.h"
#include "core/transforms/adios_transforms_reqgroup.h"
#include "public/adios_error.h"

#ifdef ALACRITY

#include "alacrity.h"

#include "adios_transform_alacrity_common.h"
#include "adios_transform_identity_read.h" // used to read original data

int adios_transform_alacrity_is_implemented (void) {return 1;}

typedef struct {
	adios_transform_alacrity_metadata alac_meta;
	void *identity_internal;
} alac_pg_read_req_info;

int adios_transform_alacrity_generate_read_subrequests(adios_transform_read_request *reqgroup,
                                                       adios_transform_pg_read_request *pg_reqgroup)
{
	uint64_t read_buffer_size;
	alac_pg_read_req_info *pg_internal = (alac_pg_read_req_info*)malloc(sizeof(alac_pg_read_req_info));

	const int metagood =
		read_alacrity_transform_metadata(
			pg_reqgroup->transform_metadata_len,
			pg_reqgroup->transform_metadata,
			&pg_internal->alac_meta);
	if (!metagood) {
		log_warn("Invalid ALACRITY transform metadata found for PG %d; input file "
				 "is likely corrupt or written using an old version of ALACRITY\n",
				 pg_reqgroup->blockidx);
		free(pg_internal);
		return 1;
	}

	// Based on the format of the ALACRITY partition, schedule the required read requests
	if (pg_internal->alac_meta.has_origdata) {
		// Original data is available, use identity transform read
		// Use the Identity method for doing this, since it's just reading original data
	    adios_transform_generate_read_subrequests_over_original_data(
	    	pg_internal->alac_meta.origdata_offset,
			1, // sieve points
			reqgroup,
			pg_reqgroup
	    );

	    // Wrap the identity-specific internal information so we can reproduce it later
		pg_internal->identity_internal = pg_reqgroup->transform_internal;
	} else if (pg_internal->alac_meta.has_lob) {
		// No original data, but low-order bytes are available, so we can decode the partition
		// Read the whole partition to do this
	    void *buf = malloc(pg_reqgroup->raw_var_length);
	    adios_transform_raw_read_request *subreq = adios_transform_raw_read_request_new_whole_pg(pg_reqgroup, buf);
	    adios_transform_raw_read_request_append(pg_reqgroup, subreq);
	} else {
		// No original data or low-order bytes, must use approximate index decoding
		adios_error(err_unspecified, "Attempt to read from ALACRITY-transformed data with "
				                     "no original data and no low-order bytes encoded. This "
				                     "function is currently unimplemented.\n");
	    return 1;
	}

	pg_reqgroup->transform_internal = pg_internal; // Store necessary information for interpreting the read
	return 0;
}

// Do nothing for individual subrequest
adios_datablock * adios_transform_alacrity_subrequest_completed(adios_transform_read_request *reqgroup,
                                                                adios_transform_pg_read_request *pg_reqgroup,
                                                                adios_transform_raw_read_request *completed_subreq)
{
    return NULL;
}

adios_datablock * adios_transform_alacrity_pg_reqgroup_completed(adios_transform_read_request *reqgroup,
                                                                 adios_transform_pg_read_request *completed_pg_reqgroup)
{
	alac_pg_read_req_info *pg_internal = (alac_pg_read_req_info*)completed_pg_reqgroup->transform_internal;

	if (pg_internal->alac_meta.has_origdata) {
		// If this is an original data read, we delegate to the Identity implementation

		// Temporarily restore the pg_reqgroup->transform_internal to what the Identity implementation is expecting
		completed_pg_reqgroup->transform_internal = pg_internal->identity_internal;

		// Invoke the Identity read implementation
		adios_datablock *ret = adios_transform_pg_reqgroup_completed_over_original_data(reqgroup, completed_pg_reqgroup);

		// Revert to our internal info, so it will be free'd by the Transform framework
		completed_pg_reqgroup->transform_internal = pg_internal;

		// Return
		return ret;
	} else if (pg_internal->alac_meta.has_lob) {
		// If this is a LOB data read, decode the partition

		// Get the buffer with the transformed data that was read (it's in the
		// single child raw read request)
		void *raw_buff = completed_pg_reqgroup->subreqs->data;

		// Compute the size of the original data so we can allocate a buffer
		uint64_t orig_size = adios_get_type_size(reqgroup->transinfo->orig_type, "");
		int d;
		for(d = 0; d < reqgroup->transinfo->orig_ndim; d++)
			orig_size *= (uint64_t)(completed_pg_reqgroup->orig_varblock->count[d]);

		// Allocate a buffer to decode into
		void* orig_data_buff = malloc(orig_size);

		ALPartitionData output_partition;
		uint64_t numElements = 0;

		// Deserialize the ALACRITY partition from the read buffer into a struct
		memstream_t ms = memstreamInitReturn(raw_buff);
		memstreamSkip(&ms, pg_internal->alac_meta.meta_offset);
		ALDeserializeMetadata(&output_partition.metadata, &ms);
		memstreamReset(&ms);
		memstreamSkip(&ms, pg_internal->alac_meta.index_offset);
		ALDeserializeIndex(&output_partition.index, &output_partition.metadata, &ms);
		memstreamReset(&ms);
		memstreamSkip(&ms, pg_internal->alac_meta.lob_offset);
		ALDeserializeData(&output_partition.data, &output_partition.metadata, &ms);
		memstreamReset(&ms);
		memstreamDestroy(&ms, false);

		// The deserialize functions above allocate their own buffers
		output_partition.ownsBuffers = true;

		// Free the read buffer for transformed data (this would be done automatically
		// by the transform framework, but do it here to be safe)
		free(completed_pg_reqgroup->subreqs->data);
		completed_pg_reqgroup->subreqs->data = NULL;

		// Decode the ALPartitionData into an output buffer
		int rtn = ALDecode(&output_partition, orig_data_buff, &numElements);
		if (ALErrorNone != rtn) {
			free(orig_data_buff);
			return NULL;
		}

		ALPartitionDataDestroy(&output_partition);

		return adios_datablock_new_whole_pg(reqgroup, completed_pg_reqgroup, orig_data_buff);
	} else {
		abort(); // Should not happen, since no reads should be scheduled in such a case
		return NULL;
	}
}

adios_datablock * adios_transform_alacrity_reqgroup_completed(adios_transform_read_request *completed_reqgroup)
{
    return NULL;
}


#else

DECLARE_TRANSFORM_READ_METHOD_UNIMPL(alacrity);

#endif

