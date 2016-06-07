
#include <stdint.h>
#include <assert.h>

#include "core/adios_logger.h"
#include "core/adios_internals.h"
#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_write.h"
#include "core/transforms/adios_transforms_hooks_write.h"
#include "adios_selection.h"

/*
DECLARE_TRANSFORM_WRITE_METHOD_UNIMPL(none);
DECLARE_TRANSFORM_WRITE_METHOD(identity);
DECLARE_TRANSFORM_WRITE_METHOD(zlib);
DECLARE_TRANSFORM_WRITE_METHOD(bzip2);
DECLARE_TRANSFORM_WRITE_METHOD(szip);
DECLARE_TRANSFORM_WRITE_METHOD(isobar);
DECLARE_TRANSFORM_WRITE_METHOD(aplod);
DECLARE_TRANSFORM_WRITE_METHOD(alacrity);
*/

// PLUGIN DETECT - Generate write-side function declarations for all plugins
#include "core/transforms/plugindetect/detect_plugin_write_hook_decls.h"

// Transform write method registry
adios_transform_write_method TRANSFORM_WRITE_METHODS[num_adios_transform_types];

void adios_transform_init() {
    static int adios_transforms_initialized = 0;
    if (adios_transforms_initialized)
        return;

    /*
    REGISTER_TRANSFORM_WRITE_METHOD(none, adios_transform_none);
    REGISTER_TRANSFORM_WRITE_METHOD(identity, adios_transform_identity);
    REGISTER_TRANSFORM_WRITE_METHOD(zlib, adios_transform_zlib);
    REGISTER_TRANSFORM_WRITE_METHOD(bzip2, adios_transform_bzip2);
    REGISTER_TRANSFORM_WRITE_METHOD(szip, adios_transform_szip);
    REGISTER_TRANSFORM_WRITE_METHOD(isobar, adios_transform_isobar);
    REGISTER_TRANSFORM_WRITE_METHOD(aplod, adios_transform_aplod);
    REGISTER_TRANSFORM_WRITE_METHOD(alacrity, adios_transform_alacrity);
    */

    // PLUGIN DETECT - Register write-side functions from all plugins in the table
    // NOTE: Input macro "TRANSFORM_WRITE_METHODS" specifies the table to register into, but this
    //       is already the name of our table, so no further action is needed
    #include "core/transforms/plugindetect/detect_plugin_write_hook_reg.h"

    adios_transforms_initialized = 1;
}

// Delegate functions

uint16_t adios_transform_get_metadata_size(struct adios_transform_spec *transform_spec) {
    if (!transform_spec) return 0;
    assert(transform_spec->transform_type >= adios_transform_none && transform_spec->transform_type < num_adios_transform_types);
    return TRANSFORM_WRITE_METHODS[transform_spec->transform_type].transform_get_metadata_size(transform_spec);
}

void adios_transform_transformed_size_growth(
		const struct adios_var_struct *var, const struct adios_transform_spec *transform_spec,
		uint64_t *constant_factor, double *linear_factor, double *capped_linear_factor, uint64_t *capped_linear_cap) {

    assert(var->transform_type >= adios_transform_none && var->transform_type < num_adios_transform_types);
    TRANSFORM_WRITE_METHODS[var->transform_type].transform_transformed_size_growth(var, transform_spec, constant_factor, linear_factor, capped_linear_factor, capped_linear_cap);
}

int adios_transform_apply(
        struct adios_file_struct *fd, struct adios_var_struct *var,
        uint64_t *transformed_len, int use_shared_buffer, int *wrote_to_shared_buffer) {

    assert(var->transform_type >= adios_transform_none && var->transform_type < num_adios_transform_types);
    return TRANSFORM_WRITE_METHODS[var->transform_type].transform_apply(fd, var, transformed_len, use_shared_buffer, wrote_to_shared_buffer);
}
