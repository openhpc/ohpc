/*
 * adios_transform_alacrity_common.h
 *
 *  Created on: Oct 9, 2014
 *      Author: David A. Boyuka II
 */
#ifndef ADIOS_TRANSFORM_ALACRITY_COMMON_H_
#define ADIOS_TRANSFORM_ALACRITY_COMMON_H_

#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <public/adios_error.h>
//#include <assert.h>

#include <memstream.h> // From ALACRITY, helps with (de)serialization of metadata

typedef struct {
	// Loaded from transform metadata buffer
	uint64_t meta_size, index_size, lob_size, origdata_size;

	// Computed
	uint64_t meta_offset, index_offset, lob_offset, origdata_offset;
	int has_lob, has_origdata;
} adios_transform_alacrity_metadata;

static const int ALACRITY_TRANSFORM_METADATA_LEN = sizeof(uint8_t) + 4 * sizeof(uint64_t);

inline static int read_alacrity_transform_metadata(int16_t transform_metadata_len, const void *transform_metadata, adios_transform_alacrity_metadata *alac_metadata) {
	assert(alac_metadata && transform_metadata);
	if (transform_metadata_len != ALACRITY_TRANSFORM_METADATA_LEN) {
		adios_error(err_unspecified,
				    "Unexpected transform metadata size for ALACRITY transform "
				    "(is %hu, expected %hu). File may be corrupt, or may have "
				    "been written using an old version of the ALACRITY data "
				    "transform plugin.\n", transform_metadata_len, ALACRITY_TRANSFORM_METADATA_LEN);
		return 0;
	}

	int ret = 1;
	memstream_t meta_ms = memstreamInitReturn((void*)transform_metadata);

	const int version = memstreamReadChar(&meta_ms);
	if (version == 0) {
		alac_metadata->meta_size = memstreamReadUint64(&meta_ms);
		alac_metadata->index_size = memstreamReadUint64(&meta_ms);
		alac_metadata->lob_size = memstreamReadUint64(&meta_ms);
		alac_metadata->origdata_size = memstreamReadUint64(&meta_ms);

		alac_metadata->meta_offset = 0;
		alac_metadata->index_offset = alac_metadata->meta_offset + alac_metadata->meta_size;
		alac_metadata->lob_offset = alac_metadata->index_offset + alac_metadata->index_size;
		alac_metadata->origdata_offset = alac_metadata->lob_offset + alac_metadata->lob_size;

		alac_metadata->has_lob = (alac_metadata->lob_size != 0);
		alac_metadata->has_origdata = (alac_metadata->origdata_size != 0);
	} else {
		adios_error(err_unspecified,
				    "Unknown ALACRITY transform metadata version %d. "
				    "File may be corrupt, or this build of ADIOS may not be up-to-date.\n",
				    version);
		ret = 0;
	}

	memstreamDestroy(&meta_ms, false);
	return ret;
}

inline static int write_alacrity_transform_metadata(int16_t transform_metadata_len,
		void *transform_metadata, const adios_transform_alacrity_metadata *alac_metadata) {
	static const int ALAC_VERSION = 0;

	assert(alac_metadata && transform_metadata);
	if (transform_metadata_len != ALACRITY_TRANSFORM_METADATA_LEN) {
		adios_error(err_unspecified,
				    "Internal error: ALACRITY transform metadata was not allocated with "
				    "the correct size (is %hu, expected %hu)\n",
				    transform_metadata_len, ALACRITY_TRANSFORM_METADATA_LEN);
		return 0;
	}

	memstream_t meta_ms = memstreamInitReturn(transform_metadata);
	memstreamAppendChar(&meta_ms, ALAC_VERSION);
	memstreamAppendUint64(&meta_ms, alac_metadata->meta_size);
	memstreamAppendUint64(&meta_ms, alac_metadata->index_size);
	memstreamAppendUint64(&meta_ms, alac_metadata->lob_size);
	memstreamAppendUint64(&meta_ms, alac_metadata->origdata_size);
	memstreamDestroy(&meta_ms, false);
	return 1;
}

#endif /* ADIOS_TRANSFORM_ALACRITY_COMMON_H_ */
