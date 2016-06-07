/*
 * adios_transforms_hooks_read.h
 *
 *  Created on: Jul 25, 2012
 *      Author: David A. Boyuka II
 */

#ifndef ADIOS_TRANSFORMS_HOOKS_READ_H_
#define ADIOS_TRANSFORMS_HOOKS_READ_H_

#include <stdint.h>
#include "core/adios_bp_v1.h"
#include "core/adios_subvolume.h"
#include "public/adios_read_v2.h"
#include "public/adios_selection.h"
#include "public/adios_error.h"

#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_read.h"
#include "core/transforms/adios_transforms_reqgroup.h"
#include "core/transforms/adios_transforms_datablock.h"
#include "core/transforms/adios_transforms_transinfo.h"

// Initialize the transform system for adios read-only libraries
void adios_transform_read_init();

int adios_transform_generate_read_subrequests(adios_transform_read_request *reqgroup, adios_transform_pg_read_request *pg_reqgroup);

adios_datablock * adios_transform_subrequest_completed(adios_transform_read_request *reqgroup,
                                                       adios_transform_pg_read_request *pg_reqgroup,
                                                       adios_transform_raw_read_request *completed_subreq);

adios_datablock * adios_transform_pg_reqgroup_completed(adios_transform_read_request *reqgroup,
                                                        adios_transform_pg_read_request *completed_pg_reqgroup);

adios_datablock * adios_transform_read_reqgroup_completed(adios_transform_read_request *completed_reqgroup);

////////////////////////////////////////////////
// Transform read method registry/registration
////////////////////////////////////////////////

// Transform read method registry entry
typedef struct {
    int (*transform_is_implemented) (void);

    int (*transform_generate_read_subrequests)(
            adios_transform_read_request *reqgroup,
            adios_transform_pg_read_request *pg_reqgroup);

    adios_datablock * (*transform_subrequest_completed)(
            adios_transform_read_request *reqgroup,
            adios_transform_pg_read_request *pg_reqgroup,
            adios_transform_raw_read_request *completed_subreq);

    adios_datablock * (*transform_pg_reqgroup_completed)(
            adios_transform_read_request *reqgroup,
            adios_transform_pg_read_request *completed_pg_reqgroup);

    adios_datablock * (*transform_reqgroup_completed)(
            adios_transform_read_request *completed_reqgroup);
} adios_transform_read_method;

//
// Every transform plugin has a set of functions that must go through three stages:
// * Declaration: as with a C header
// * Definition: the functions must be defined with bodies (or defined as
//   unimplemented using the DECLARE_TRANSFORM_READ_METHOD_UNIMPL utility macro)
// * Registration: loading pointers to the functions into a callback table
//

// Transform method function declarations
#define DECLARE_TRANSFORM_READ_METHOD(tmethod)                            \
    int adios_transform_##tmethod##_is_implemented (void);                \
    int adios_transform_##tmethod##_generate_read_subrequests(            \
            adios_transform_read_request *reqgroup,                       \
            adios_transform_pg_read_request *pg_reqgroup);                \
   adios_datablock * adios_transform_##tmethod##_subrequest_completed(    \
            adios_transform_read_request *reqgroup,                       \
            adios_transform_pg_read_request *pg_reqgroup,                 \
            adios_transform_raw_read_request *completed_subreq);          \
   adios_datablock * adios_transform_##tmethod##_pg_reqgroup_completed(   \
            adios_transform_read_request *reqgroup,                       \
            adios_transform_pg_read_request *completed_pg_reqgroup);      \
   adios_datablock * adios_transform_##tmethod##_reqgroup_completed(      \
            adios_transform_read_request *completed_reqgroup);

// Transform method function registration
#define TRANSFORM_READ_METHOD_HOOK_LIST(tmethod) \
    adios_transform_##tmethod##_is_implemented, \
    adios_transform_##tmethod##_generate_read_subrequests, \
    adios_transform_##tmethod##_subrequest_completed, \
    adios_transform_##tmethod##_pg_reqgroup_completed, \
    adios_transform_##tmethod##_reqgroup_completed \

#define REGISTER_TRANSFORM_READ_METHOD_HOOKS(ttable, tmethod, method_type) \
    ttable[method_type] = (adios_transform_read_method){ TRANSFORM_READ_METHOD_HOOK_LIST(tmethod) };

// Transform method function helper definitions for unimplemented methods
#define UNIMPL_TRANSFORM_READ_FN(tmethod, func) \
    adios_error(err_operation_not_supported,                                \
                "Transform method %s is not supported for read in this "    \
                "configuration of ADIOS (function missing: %s)\n",            \
                #tmethod, func);

#define DECLARE_TRANSFORM_READ_METHOD_UNIMPL(tmethod)                     \
    int adios_transform_##tmethod##_is_implemented (void) { \
        return 0;                                                         \
    }                                                                     \
    int adios_transform_##tmethod##_generate_read_subrequests(            \
            adios_transform_read_request *reqgroup,                    \
            adios_transform_pg_read_request *pg_reqgroup) {                    \
        UNIMPL_TRANSFORM_READ_FN(tmethod, __FUNCTION__);                \
        return adios_errno;                                                \
    }                                                                    \
    adios_datablock * adios_transform_##tmethod##_subrequest_completed(    \
            adios_transform_read_request *reqgroup,                    \
            adios_transform_pg_read_request *pg_reqgroup,                    \
            adios_transform_raw_read_request *completed_subreq) {        \
        UNIMPL_TRANSFORM_READ_FN(tmethod, __FUNCTION__);                \
        return NULL;                                                    \
    }                                                                    \
    adios_datablock * adios_transform_##tmethod##_pg_reqgroup_completed(    \
            adios_transform_read_request *reqgroup,                    \
            adios_transform_pg_read_request *completed_pg_reqgroup) {        \
        UNIMPL_TRANSFORM_READ_FN(tmethod, __FUNCTION__);                \
        return NULL;                                                    \
    }                                                                    \
    adios_datablock * adios_transform_##tmethod##_reqgroup_completed(    \
            adios_transform_read_request *completed_reqgroup) {        \
        UNIMPL_TRANSFORM_READ_FN(tmethod, __FUNCTION__);                \
        return NULL;                                                    \
    }

#endif /* ADIOS_TRANSFORMS_HOOKS_READ_H_ */
