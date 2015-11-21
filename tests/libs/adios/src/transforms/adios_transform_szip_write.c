#include <stdint.h>
#include <assert.h>
#include <limits.h>
#include <sys/time.h>

#include "core/adios_logger.h"
#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_write.h"
#include "core/transforms/adios_transforms_hooks_write.h"
#include "core/transforms/adios_transforms_util.h"

#ifdef SZIP

#include "adios_transform_szip.h"

// assume double precision only
int compress_szip_pre_allocated(const void* input_data, const uint64_t input_len,
                                void* output_data, uint64_t* output_len,
                                const int ndims, const uint64_t* dim)
{
    assert(input_data != NULL && input_len > 0 && output_data != NULL && output_len != NULL && *output_len > 0);

    int rtn = 0;

    SZ_com_t sz_param;

    rtn = init_szip_parameters(&sz_param, ndims, dim);
    if (rtn != 0)
    {
        return -1;
    }

    size_t temp = *output_len;

    rtn = SZ_BufftoBuffCompress(output_data, &temp, input_data, input_len, &sz_param);
    if (SZ_OK != rtn)
    {
        // printf("SZ_BufftoBuffCompress error %d\n", rtn);
        return -1;
    }

    *output_len = temp;

    return 0;
}

uint16_t adios_transform_szip_get_metadata_size(struct adios_transform_spec *transform_spec)
{
    return 0;
}

void adios_transform_szip_transformed_size_growth(
		const struct adios_var_struct *var, const struct adios_transform_spec *transform_spec,
		uint64_t *constant_factor, double *linear_factor, double *capped_linear_factor, uint64_t *capped_linear_cap)
{
	// Do nothing (defaults to "no transform effect on data size")
}

int adios_transform_szip_apply(struct adios_file_struct *fd,
                                    struct adios_var_struct *var,
                                    uint64_t *transformed_len,
                                    int use_shared_buffer,
                                    int *wrote_to_shared_buffer)
{
    // Get the input data and data length
    const uint64_t input_size = adios_transform_get_pre_transform_var_size(var);
    const void *input_buff = var->data;

    // decide the output buffer
    uint64_t output_size = input_size; //adios_transform_szip_calc_vars_transformed_size(adios_transform_szip, input_size, 1);
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

    // compress it
    int ndims = 1;
    uint64_t dim[1] = {input_size/sizeof(double)};

    // double d1 = dclock();
    int rtn = compress_szip_pre_allocated(input_buff, input_size, output_buff, &output_size, ndims, dim);
    // double d2 = dclock();

    if(0 != rtn                     // compression failed for some reason, then just copy the buffer
        || output_size > input_size)  // or size after compression is even larger (not likely to happen since compression lib will return non-zero in this case)
    {
        return 0;
    }

    // printf("compress_szip_succ|%d|%d|%d|%f\n", rtn, input_size, actual_output_size, d2 - d1);

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

DECLARE_TRANSFORM_WRITE_METHOD_UNIMPL(szip)

#endif

