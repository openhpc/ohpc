/*
 * adios_transform_identity_write.c
 *
 * An implementation of the "identity" transform, which does nothing, but
 * exercises the transform framework for testing.
 *
 *  Created on: Jul 31, 2012
 *      Author: David A. Boyuka II
 */

#include <stdint.h>
#include <assert.h>

#include "core/adios_internals.h"
#include "core/transforms/adios_transforms_write.h"
#include "core/transforms/adios_transforms_hooks_write.h"


uint16_t adios_transform_identity_get_metadata_size(struct adios_transform_spec *transform_spec) {
    return 0;
}

void adios_transform_identity_transformed_size_growth(
		const struct adios_var_struct *var, const struct adios_transform_spec *transform_spec,
		uint64_t *constant_factor, double *linear_factor, double *capped_linear_factor, uint64_t *capped_linear_cap)
{
	// Do nothing (defaults to "no transform effect on data size")
}

int adios_transform_identity_apply(struct adios_file_struct *fd, struct adios_var_struct *var, uint64_t *transformed_len,
                                   int use_shared_buffer, int *wrote_to_shared_buffer) {
    // Just use what is already in var->data; size remains the same, and no
    // shared buffer is used
    *transformed_len = adios_transform_get_pre_transform_var_size(var);
    *wrote_to_shared_buffer = 0;
    return 1;
}
