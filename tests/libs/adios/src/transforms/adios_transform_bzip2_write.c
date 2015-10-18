#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <sys/time.h>

#include "core/adios_logger.h"
#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_write.h"
#include "core/transforms/adios_transforms_hooks_write.h"
#include "core/transforms/adios_transforms_util.h"

#ifdef BZIP2

#include "bzlib.h"

int compress_bzip2_pre_allocated(const void* input_data,
                                    const uint64_t input_len,
                                    void* output_data,
                                    uint64_t* output_len,
                                    int blockSize100k)
{
    // bzip2 only support input size of 32 bit integer

    assert(input_data != NULL && input_len > 0 && output_data != NULL && output_len != NULL && *output_len > 0);

    unsigned int input_len_32 = (unsigned int)input_len;
    unsigned int output_len_32 = (unsigned int)(*output_len);

    int bz_rtn = BZ2_bzBuffToBuffCompress((char*)output_data,
                                            &output_len_32,
                                            (char*)input_data,
                                            input_len_32,
                                            blockSize100k,
                                            0,
                                            30);

    if(bz_rtn != BZ_OK)
        return -1;

    *output_len = output_len_32;
    return 0;
}

uint16_t adios_transform_bzip2_get_metadata_size(struct adios_transform_spec *transform_spec)
{
    return (sizeof(uint64_t) + sizeof(char));    // metadata: original data size (uint64_t) + compression succ flag (char)
}

void adios_transform_bzip2_transformed_size_growth(
		const struct adios_var_struct *var, const struct adios_transform_spec *transform_spec,
		uint64_t *constant_factor, double *linear_factor, double *capped_linear_factor, uint64_t *capped_linear_cap)
{
	// Do nothing (defaults to "no transform effect on data size")
}

int adios_transform_bzip2_apply(struct adios_file_struct *fd,
                                struct adios_var_struct *var,
                                uint64_t *transformed_len,
                                int use_shared_buffer,
                                int *wrote_to_shared_buffer)
{
    // Get the input data and data length
    const uint64_t input_size = adios_transform_get_pre_transform_var_size(var);
    const void *input_buff = var->data;

    // parse the compressiong parameter
    /* Pre-specparse code
    if(var->transform_type_param
        && strlen(var->transform_type_param) > 0
        && is_digit_str(var->transform_type_param))
    {
        compress_level = atoi(var->transform_type_param);
        if(compress_level > 9 || compress_level < 1)
        {
            compress_level = 9;
        }
    }
    */
    int compress_level = 9;
    if (var->transform_spec->param_count > 0) {
        compress_level = atoi(var->transform_spec->params[0].key);
        if (compress_level < 1 || compress_level > 9)
            compress_level = 9;
    }


    // decide the output buffer
    uint64_t output_size = input_size; //adios_transform_bzip2_calc_vars_transformed_size(adios_transform_bzip2, input_size, 1);
    void* output_buff = NULL;

    if (use_shared_buffer)    // If shared buffer is permitted, serialize to there
    {
        *wrote_to_shared_buffer = 1;
        if (!shared_buffer_reserve(fd, output_size))
        {
            log_error("Out of memory allocating %llu bytes for %s for bzip2 transform\n", output_size, var->name);
            return 0;
        }

        // Write directly to the shared buffer
        output_buff = fd->buffer + fd->offset;
    }
    else    // Else, fall back to var->data memory allocation
    {
        *wrote_to_shared_buffer = 0;
        output_buff = malloc(output_size);
        if (!output_buff)
        {
            log_error("Out of memory allocating %llu bytes for %s for bzip2 transform\n", output_size, var->name);
            return 0;
        }
    }

    uint64_t actual_output_size = output_size;
    char compress_ok = 1;

    int rtn = compress_bzip2_pre_allocated(input_buff, input_size, output_buff, &actual_output_size, compress_level);

    if(0 != rtn                     // compression failed for some reason, then just copy the buffer
        || actual_output_size > input_size)  // or size after compression is even larger (not likely to happen since compression lib will return non-zero in this case)
    {
        // printf("compression failed, fall back to memory copy\n");
        memcpy(output_buff, input_buff, input_size);
        actual_output_size = input_size;
        compress_ok = 0;    // succ sign set to 0
    }

    // Wrap up, depending on buffer mode
    if (use_shared_buffer)
    {
        shared_buffer_mark_written(fd, actual_output_size);
    }
    else
    {
        var->data = output_buff;
        var->data_size = actual_output_size;
        var->free_data = adios_flag_yes;
    }

    // copy the metadata, simply the original size before compression
    if(var->transform_metadata && var->transform_metadata_len > 0)
    {
        memcpy((char*)var->transform_metadata, &input_size, sizeof(uint64_t));
        memcpy((char*)var->transform_metadata + sizeof(uint64_t), &compress_ok, sizeof(char));
    }

    *transformed_len = actual_output_size; // Return the size of the data buffer

    return 1;
}

#else

DECLARE_TRANSFORM_WRITE_METHOD_UNIMPL(bzip2)

#endif

