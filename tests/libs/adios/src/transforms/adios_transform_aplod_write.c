#include <stdint.h>
#include <assert.h>
#include <limits.h>

#include "core/adios_logger.h"
#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_write.h"
#include "core/transforms/adios_transforms_hooks_write.h"
#include "core/transforms/adios_transforms_util.h"

#ifdef APLOD

#include "aplod.h"

const int MAX_COMPONENTS = 8;

uint16_t adios_transform_aplod_get_metadata_size(struct adios_transform_spec *transform_spec)
{
    // Write the component vector here
    // No more than 8 components per variable
    return (sizeof (uint64_t) + sizeof (int8_t) + 8 * sizeof(int32_t));
}

void adios_transform_aplod_transformed_size_growth(
		const struct adios_var_struct *var, const struct adios_transform_spec *transform_spec,
		uint64_t *constant_factor, double *linear_factor, double *capped_linear_factor, uint64_t *capped_linear_cap)
{
	// Do nothing (defaults to "no transform effect on data size")
}

int adios_transform_aplod_apply(struct adios_file_struct *fd,
                                struct adios_var_struct *var,
                                uint64_t *transformed_len,
                                int use_shared_buffer,
                                int *wrote_to_shared_buffer)
{
    // Get the input data and data length
    const uint64_t input_size = adios_transform_get_pre_transform_var_size(var);
    const void *input_buff = var->data;

    // max size supported is long double
    int32_t componentVector[MAX_COMPONENTS];
    int8_t numComponents = 0;
    int32_t componentTotals = 0;

    // printf ("[%s] byte = %d, real = %d, double = %d, this = %d\n", var->name, adios_byte, adios_real, adios_double, var->pre_transform_type);
    // parse the aplod component vector parameters
    /* Old, pre-specparse parameter parsing
    if(var->transform_type_param)
    {
        char transform_param [1024];
        char *transform_param_ptr       = 0;
        uint16_t transform_param_length = 0;

        char transform_param_option [256];

        uint16_t idx = 0;

        strcpy (transform_param, var->transform_type_param);
        transform_param_ptr     = transform_param;
        transform_param_length  = strlen (transform_param);

        // Get the key
        char *key = strtok (transform_param, "=");

        if (strcmp (key, "CV") == 0) {

            char *value = strtok (key, ",");
            int32_t componentID = 0;

            while (value) {
                int32_t component = atoi (value);
                if (component <= 0) {
                    numComponents = 0;
                    break ;
                }

                componentVector [componentID] = component;
                componentTotals += component;
                componentID ++;
            }
        }
    }
    */
    int i;
    int paramError = 0;
    for (i = 0; i < var->transform_spec->param_count; i++) {
        const char *comp = var->transform_spec->params[i].key;

        int compInt = atoi(comp);

        // Error if any component is non-positive
        if (compInt <= 0 || numComponents >= MAX_COMPONENTS) {
            numComponents = 0;
            paramError = 1;
            break;
        }
        componentVector[numComponents++] = compInt;
        componentTotals += compInt;
    }

    uint32_t pre_transform_type_size = bp_get_type_size (var->pre_transform_type, "");

    // Error if components were specified, and the components don't sum to the type size
    if ((numComponents > 0) && (componentTotals != pre_transform_type_size)) {
        paramError = 1;
    }

    if (paramError) {
        fprintf(stderr, "Warning: at least one APLOD byte component is a non-positive integer, or all components do not sum to the type size (%d) for variable %s/%s. Using default APLOD configuration instead.\n",
                bp_get_type_size (var->pre_transform_type, ""), var->path, var->name);
    }

    if ((numComponents == 0) || (componentTotals != pre_transform_type_size)) {
        if (var->pre_transform_type == adios_double) {
            componentVector [0] = 2;
            componentVector [1] = 2;
            componentVector [2] = 2;
            componentVector [3] = 2;
            numComponents = 4;
        } else if (var->pre_transform_type == adios_real) {
            componentVector [0] = 2;
            componentVector [1] = 2;
            numComponents = 2;
        } else {
            numComponents = 0;

            while (componentTotals < pre_transform_type_size - 2) {
                componentVector [numComponents ++] = 2;
                componentTotals += 2;
            }

            componentVector [numComponents ++] = pre_transform_type_size - componentTotals;
        }
    }

    // decide the output buffer
    uint64_t output_size = input_size;
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

    // APLOD specific code - Start
    uint32_t numElements = input_size / bp_get_type_size (var->pre_transform_type, "");

    APLODConfig_t *config;

    if (var->pre_transform_type == adios_double) {                                                                                                                                                                                          
        config = APLODConfigure (componentVector, numComponents, APLOD_DOUBLE, APLOD_LITTLE_E);
    } else if (var->pre_transform_type == adios_real) {                                                                                                                                                                                     
        config = APLODConfigure (componentVector, numComponents, APLOD_FLOAT, APLOD_LITTLE_E);
    }                                                                                                                                                                                                                                                             
    // config->blockLengthElts = numElements; // Bug workaround, disable chunking
    config->blockLengthElts = (numElements >= 65536 ? 65536: numElements);

    APLODShuffleComponents (config, numElements, 0, numComponents, input_buff, output_buff);
    // APLOD specific code - End

    // Wrap up, depending on buffer mode
    if (*wrote_to_shared_buffer) {
        shared_buffer_mark_written(fd, output_size);
    } else {
        var->data = output_buff;
        var->data_size = output_size;
        var->free_data = adios_flag_yes;
    }
    *transformed_len = output_size; // Return the size of the data buffer

    // Do I copy the PLODHandle_t object as the metadata or do I serialize it into the buffer as well
    if(var->transform_metadata && var->transform_metadata_len > 0)
    {
        memcpy ((char*)var->transform_metadata, &input_size, sizeof(uint64_t));
        memcpy ((char*)var->transform_metadata + sizeof (uint64_t), &numComponents, sizeof (numComponents));
        memcpy ((char*)var->transform_metadata + sizeof (uint64_t) + sizeof (numComponents), componentVector, numComponents * sizeof (int32_t));

    }

    // Cleanup time
    APLODDestroy(config);

    return 1;
}

#else

DECLARE_TRANSFORM_WRITE_METHOD_UNIMPL(aplod)

#endif

