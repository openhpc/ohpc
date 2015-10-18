#include <stdint.h>
#include <assert.h>
#include <limits.h>
#include <sys/time.h>

#include "adios_logger.h"
#include "adios_transforms_common.h"
#include "adios_transforms_write.h"
#include "adios_transforms_hooks_write.h"
#include "adios_transforms_util.h"

#ifdef TEMPLATE

uint16_t adios_transform_template_get_metadata_size(struct adios_transform_spec *transform_spec)
{
    return 0; // Set amount of transform-internal metadata space to allocate
}

void adios_transform_template_transformed_size_growth(
		const struct adios_var_struct *var, const struct adios_transform_spec *transform_spec,
		uint64_t *constant_factor, double *linear_factor, double *capped_linear_factor, uint64_t *capped_linear_cap)
{
	// Set growth factors here (or return immediately to default to "no transform effect on data size")
}

int adios_transform_template_apply(struct adios_file_struct *fd,
                                   struct adios_var_struct *var,
                                   uint64_t *transformed_len,
                                   int use_shared_buffer,
                                   int *wrote_to_shared_buffer)
{
    // Get the input data and data length
    const uint64_t input_size = adios_transform_get_pre_transform_var_size(var);
    const void *input_buff = var->data;

    // decide the output buffer
    uint64_t output_size = /* Compute how much output size we need */;
    void* output_buff = NULL;

    if (use_shared_buffer) {
        // If shared buffer is permitted, serialize to there
        assert(shared_buffer_reserve(fd, output_size));

        // Write directly to the shared buffer
        output_buff = fd->buffer + fd->offset;
    } else { // Else, fall back to var->data memory allocation
        output_buff = malloc(output_size);
        assert(output_buff);
    }
    *wrote_to_shared_buffer = use_shared_buffer;

    // Do transform from input_buff into output_buff, and update output_size to the true output size

    // Wrap up, depending on buffer mode
    if (*wrote_to_shared_buffer) {
        shared_buffer_mark_written(fd, output_size);
    } else {
        var->data = output_buff;
        var->data_size = output_size;
        var->free_data = adios_flag_yes;
    }

    *transformed_len = output_size; // Return the size of the data buffer
    return 1;
}

#else

DECLARE_TRANSFORM_WRITE_METHOD_UNIMPL(template)

#endif

