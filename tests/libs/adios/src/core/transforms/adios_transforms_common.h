/*
 * Contains functionality that is common to both reading and writing for
 * handling variable transforms in ADIOS.
 *
 *  Created on: Jun 22, 2012
 *      Author: David A. Boyuka II
 */

#ifndef ADIOS_TRANSFORM_H
#define ADIOS_TRANSFORM_H

#include <stdint.h>
#include "core/adios_bp_v1.h"
#include "core/transforms/adios_transforms_hooks.h" // Includes the hooks header to load and access the ADIOS_TRANSFORM_TYPE enum
//#include "adios_internals.h"

int get_system_endianness();

/////////////////////////////////////
// Variable introspection
/////////////////////////////////////

enum ADIOS_DATATYPES adios_transform_get_var_original_type_var(struct adios_var_struct *var);
enum ADIOS_DATATYPES adios_transform_get_var_original_type_var_header(struct adios_var_header_struct_v1 *var_header);
enum ADIOS_DATATYPES adios_transform_get_var_original_type_index(struct adios_index_var_struct_v1 *var);
int adios_transform_get_var_original_ndims_characteristic(struct adios_index_characteristic_struct_v1 *ch);

struct adios_index_characteristic_dims_struct_v1 * adios_transform_get_var_original_dims_characteristic(struct adios_index_characteristic_struct_v1 *ch);
int adios_transform_get_var_original_ndims_index(struct adios_index_var_struct_v1 *var);

/*
 * Returns whether the given variable is transformed
 * @param var the variable to check
 * @return whether the variable is transformed
 */
int adios_transform_is_var_transformed(const struct adios_index_var_struct_v1 *var);

/*
 * Returns the number of bytes in the transformed form of the variable.
 * Precondition: var has been transformed, and has a transform_type other than
 * adios_transform_none and adios_transform_unknown.
 *
 * @param var the variable
 * @return the number of bytes in the transformed form of 'var'
 */
uint64_t adios_transform_get_var_transformed_size(const struct adios_index_var_struct_v1 *var, int time_index);

//////////////////////////////////////////////////
// Transform characteristic management functions
//////////////////////////////////////////////////

// Init
int adios_transform_init_transform_characteristic(struct adios_index_characteristic_transform_struct *transform);

// Deserialize (this is needed because bp_utils.c uses this)
int adios_transform_deserialize_transform_characteristic(struct adios_index_characteristic_transform_struct *transform, struct adios_bp_buffer_struct_v1 *b);

// Clear (i.e. free/wipe)
int adios_transform_clear_transform_characteristic(struct adios_index_characteristic_transform_struct *transform);

// Swap
int adios_transform_swap_transform_characteristics(struct adios_index_characteristic_transform_struct *c1, struct adios_index_characteristic_transform_struct *c2);

#endif /* ADIOS_TRANSFORM_H */
