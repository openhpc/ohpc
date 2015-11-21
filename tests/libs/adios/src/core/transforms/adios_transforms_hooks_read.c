/*
 * adios_transforms_hooks_read.c
 *
 *  Created on: Jul 24, 2012
 *      Author: David A. Boyuka II
 */

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "util.h"
#include "core/transforms/adios_transforms_hooks_read.h"
#include "core/transforms/adios_transforms_reqgroup.h"
#include "core/adios_subvolume.h"

/*
DECLARE_TRANSFORM_READ_METHOD_UNIMPL(none);
DECLARE_TRANSFORM_READ_METHOD(identity);
DECLARE_TRANSFORM_READ_METHOD(zlib);
DECLARE_TRANSFORM_READ_METHOD(bzip2);
DECLARE_TRANSFORM_READ_METHOD(szip);
DECLARE_TRANSFORM_READ_METHOD(isobar);
DECLARE_TRANSFORM_READ_METHOD(aplod);
DECLARE_TRANSFORM_READ_METHOD(alacrity);
*/

// PLUGIN DETECT - Generate read-side function declarations for all plugins
#include "core/transforms/plugindetect/detect_plugin_read_hook_decls.h"

// Transform read method registry
adios_transform_read_method TRANSFORM_READ_METHODS[num_adios_transform_types];

void adios_transform_read_init() {
    static int adios_transforms_initialized = 0;
    if (adios_transforms_initialized)
        return;

    /*
    REGISTER_TRANSFORM_READ_METHOD_HOOKS(TRANSFORM_READ_METHODS, none, adios_transform_none);
    REGISTER_TRANSFORM_READ_METHOD_HOOKS(TRANSFORM_READ_METHODS, identity, adios_transform_identity);
    REGISTER_TRANSFORM_READ_METHOD_HOOKS(TRANSFORM_READ_METHODS, zlib, adios_transform_zlib);
    REGISTER_TRANSFORM_READ_METHOD_HOOKS(TRANSFORM_READ_METHODS, bzip2, adios_transform_bzip2);
    REGISTER_TRANSFORM_READ_METHOD_HOOKS(TRANSFORM_READ_METHODS, szip, adios_transform_szip);
    REGISTER_TRANSFORM_READ_METHOD_HOOKS(TRANSFORM_READ_METHODS, isobar, adios_transform_isobar);
    REGISTER_TRANSFORM_READ_METHOD_HOOKS(TRANSFORM_READ_METHODS, aplod, adios_transform_aplod);
    REGISTER_TRANSFORM_READ_METHOD_HOOKS(TRANSFORM_READ_METHODS, alacrity, adios_transform_alacrity);
    */

    // PLUGIN DETECT - Register read-side functions from all plugins in the table
    // NOTE: Input macro "TRANSFORM_READ_METHODS" specifies the table to register into, but this
    //       is already the name of our table, so no further action is needed
    #include "core/transforms/plugindetect/detect_plugin_read_hook_reg.h"

    adios_transforms_initialized = 1;
}

/* return 0 if the method is not implemented (functions generated with DECLARE_TRANSFORM_READ_METHOD_UNIMPL, or 1 if the method is implemented */
int adios_transform_is_implemented (enum ADIOS_TRANSFORM_TYPE transform_type)
{
    assert(is_transform_type_valid(transform_type));
    if (transform_type == adios_transform_none)
        return 1; // method "none" is implemented
    return TRANSFORM_READ_METHODS[transform_type].transform_is_implemented();
}

adios_datablock * adios_transform_subrequest_completed(adios_transform_read_request *reqgroup,
                                                       adios_transform_pg_read_request *pg_reqgroup,
                                                       adios_transform_raw_read_request *completed_subreq) {
    enum ADIOS_TRANSFORM_TYPE transform_type = reqgroup->transinfo->transform_type;
    assert(is_transform_type_valid(transform_type));
    return TRANSFORM_READ_METHODS[transform_type].transform_subrequest_completed(reqgroup, pg_reqgroup, completed_subreq);
}

adios_datablock * adios_transform_pg_reqgroup_completed(adios_transform_read_request *reqgroup,
                                                        adios_transform_pg_read_request *completed_pg_reqgroup) {

    enum ADIOS_TRANSFORM_TYPE transform_type = reqgroup->transinfo->transform_type;
    assert(is_transform_type_valid(transform_type));
    return TRANSFORM_READ_METHODS[transform_type].transform_pg_reqgroup_completed(reqgroup, completed_pg_reqgroup);
}

adios_datablock * adios_transform_read_reqgroup_completed(adios_transform_read_request *completed_reqgroup) {
    enum ADIOS_TRANSFORM_TYPE transform_type = completed_reqgroup->transinfo->transform_type;
    assert(is_transform_type_valid(transform_type));
    return TRANSFORM_READ_METHODS[transform_type].transform_reqgroup_completed(completed_reqgroup);
}

int adios_transform_generate_read_subrequests(adios_transform_read_request *reqgroup, adios_transform_pg_read_request *pg_reqgroup) {
    enum ADIOS_TRANSFORM_TYPE transform_type = reqgroup->transinfo->transform_type;
    assert(is_transform_type_valid(transform_type));
    return TRANSFORM_READ_METHODS[transform_type].transform_generate_read_subrequests(reqgroup, pg_reqgroup);
}


