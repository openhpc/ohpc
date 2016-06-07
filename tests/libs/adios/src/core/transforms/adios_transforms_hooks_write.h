/*
 * adios_transforms_hooks_write.h
 *
 *  Created on: Jul 25, 2012
 *      Author: David A. Boyuka II
 */

#ifndef ADIOS_TRANSFORMS_HOOKS_WRITE_H_
#define ADIOS_TRANSFORMS_HOOKS_WRITE_H_

#include <stdint.h>
#include "core/adios_bp_v1.h"
#include "core/adios_internals.h"
#include "core/transforms/adios_transforms_common.h"
#include "public/adios_error.h"

// Initialize the transform system for adios read/write libraries
void adios_transform_init();

// Delegation functions
uint16_t adios_transform_get_metadata_size(struct adios_transform_spec *transform_spec);
void adios_transform_transformed_size_growth(
		const struct adios_var_struct *var, const struct adios_transform_spec *transform_spec,
		uint64_t *constant_factor, double *linear_factor, double *capped_linear_factor, uint64_t *capped_linear_cap);
int adios_transform_apply(
        struct adios_file_struct *fd, struct adios_var_struct *var,
        uint64_t *transformed_len, int use_shared_buffer, int *wrote_to_shared_buffer);

/////////////////////////////////////////////////
// Transform write method registry/registration
/////////////////////////////////////////////////

// Transform write method registry entry
typedef struct {
	// Expected behavior: returns the number of bytes of metadata storage a given transformed variable will need
	// Note: "metadata storage" is located in the BP footer, and is loaded at file-open time by all processes;
	//       as such, it should be strictly limited to what is necessary to interpret the remaining transformed
	//       data in one pass. This is why the size is (currently) limited to a uint16_t.
	uint16_t (*transform_get_metadata_size)(struct adios_transform_spec *transform_spec);

	// Expected behavior: returns the upper bound "growth factors" that this data transformation could effect
	// on a given variable. These factors are combined to compute the upper bound on a PG's size *before the data
	// to be written is known*
	// There are 3 factors returned:
	//   > constant_factor: regardless of what data is stored in this variable, this transform will always require
	//     up to "constant_factor" bytes extra storage above and beyond the data (for, e.g., offsets, metadata, etc.)
	//   > linear_factor: if X bytes of data are stored in this variable, this transform may require up to
	//     "linear_factor * X" bytes of storage.
	//   > capped_linear_factor: if X bytes of data are stored in this variable:
	//       if X < capped_linear_cap: this transform may require up to "capped_linear_factor * X" extra bytes of
	//         storage (this is on top of linear_factor and constant_factor)
	//       else: this transform may require up to "capped_linear_factor * capped_linear_cap" extra bytes of
	//         storage (this is on top of linear_factor and constant_factor)
	//
	// The final upper bound estimation function is as follows:
	// X' = total_constant_factor + X * max_linear_factor + min(X, max_capped_linear_cap) * max_capped_linear_factor
	//
	// Note: any or all of these factors may remain unset. In this case, constant_factor, capped_linear_factor, and
	// capped_linear_cap all default to 0, and linear_factor defaults to 1.
    void (*transform_transformed_size_growth)(
    		const struct adios_var_struct *var, const struct adios_transform_spec *transform_spec,
    		uint64_t *constant_factor, double *linear_factor, double *capped_linear_factor, uint64_t *capped_linear_cap);

    // Expected behavior: applies this data transform to the given variable. The data to be transformed is stored in
    // var->data. If use_shared_buffer is non-zero, the transform may write the transformed data directly to the ADIOS
    // shared buffer (or it may elect to use a newly-allocated buffer, as described next). If use_shared_buffer is zero,
    // the transformed data must be written to a newly-allocated buffer, and var->data shall be updated to point to this
    // buffer. See adios_transform_zlib_write.c for an example of both methods. In any case, the flag wrote_to_shared_buffer
    // must be updated to indicate whether the transformed data was written to the shared buffer or is located in var->data.
    int (*transform_apply)(
            struct adios_file_struct *fd, struct adios_var_struct *var,
            uint64_t *transformed_len, int use_shared_buffer, int *wrote_to_shared_buffer);
} adios_transform_write_method;

//
// Every transform plugin has a set of functions that must go through three stages:
// * Declaration: as with a C header
// * Definition: the functions must be defined with bodies (or defined as
//   unimplemented using the DECLARE_TRANSFORM_WRITE_METHOD_UNIMPL utility macro)
// * Registration: loading pointers to the functions into a callback table
//

// Transform method function declarations
#define DECLARE_TRANSFORM_WRITE_METHOD(tmethod) \
    uint16_t adios_transform_##tmethod##_get_metadata_size(struct adios_transform_spec *transform_spec); \
    void adios_transform_##tmethod##_transformed_size_growth(const struct adios_var_struct *var, \
    														 const struct adios_transform_spec *transform_spec, \
    														 uint64_t *constant_factor, \
    														 double *linear_factor, \
    														 double *capped_linear_factor, \
    														 uint64_t *capped_linear_cap); \
    int adios_transform_##tmethod##_apply(struct adios_file_struct *fd, struct adios_var_struct *var, \
                                          uint64_t *transformed_len,                                     \
                                          int use_shared_buffer, int *wrote_to_shared_buffer);

// Transform method function registration
#define TRANSFORM_WRITE_METHOD_HOOK_LIST(tmethod) \
    adios_transform_##tmethod##_get_metadata_size, \
    adios_transform_##tmethod##_transformed_size_growth, \
    adios_transform_##tmethod##_apply

#define REGISTER_TRANSFORM_WRITE_METHOD_HOOKS(ttable, tmethod, method_type) \
    ttable[method_type] = (adios_transform_write_method){ TRANSFORM_WRITE_METHOD_HOOK_LIST(tmethod) };

// Transform method function helper definitions for unimplemented methods
#define UNIMPL_TRANSFORM_WRITE_FN(tmethod, func) \
    adios_error(err_operation_not_supported,                                \
                "Transform method %s is not supported for write in this "   \
                "configuration of ADIOS (function missing: %s)\n",          \
                #tmethod, func);

// Note: this is actually a "definition" in the language-semantic sense, but this detail is
//  irrelevant to users, so we name it similarly to DECLARE_TRANSFORM_WRITE_METHOD
#define DECLARE_TRANSFORM_WRITE_METHOD_UNIMPL(tmethod)                                       \
        uint16_t adios_transform_##tmethod##_get_metadata_size(struct adios_transform_spec *transform_spec) { \
            UNIMPL_TRANSFORM_WRITE_FN(tmethod, __FUNCTION__);                                \
            return 0;                                                                        \
        }                                                                                    \
        void adios_transform_##tmethod##_transformed_size_growth( \
        		const struct adios_var_struct *var, const struct adios_transform_spec *transform_spec, \
        		uint64_t *constant_factor, double *linear_factor, double *capped_linear_factor, uint64_t *capped_linear_cap) { \
            UNIMPL_TRANSFORM_WRITE_FN(tmethod, __FUNCTION__);                                \
        }                                                                                    \
        int adios_transform_##tmethod##_apply(struct adios_file_struct *fd,                  \
                                              struct adios_var_struct *var,                  \
                                              uint64_t *transformed_len,                     \
                                              int use_shared_buffer, int *wrote_to_shared_buffer) {  \
            UNIMPL_TRANSFORM_WRITE_FN(tmethod, __FUNCTION__);                                \
            return 0;                                                                        \
        }

#endif /* ADIOS_TRANSFORMS_HOOKS_WRITE_H_ */
