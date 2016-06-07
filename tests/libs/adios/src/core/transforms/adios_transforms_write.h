/*
 * Contains write-specific code for handling variable transforms in ADIOS
 *
 *  Created on: Jun 27, 2012
 *      Author: David A. Boyuka II
 */

#ifndef ADIOS_TRANSFORMS_WRITE_H_
#define ADIOS_TRANSFORMS_WRITE_H_

#include "core/adios_bp_v1.h"
#include "core/adios_internals.h"
#include "core/transforms/adios_transforms_common.h"

/*
 * Returns the pre-transform size, in bytes, of a variable. Note: only works on
 * "dimensioned" variables (i.e., not a scalar or a string).
 * @param var the variable to examine
 * @return the pre-transform size, in bytes, of var
 */
uint64_t adios_transform_get_pre_transform_var_size(struct adios_var_struct *var);

/*
 * Modifies the variable definition appropriately for the case that the variable
 * is to be transformed. In the current implementation, this includes converting
 * the variable type to a byte array, storing the old dimension/type metadata,
 * and setting appropriate flag fields.
 *
 * Note: the transform to be applied is assumed to be specified in orig_var->transform_spec.
 */
struct adios_var_struct * adios_transform_define_var(struct adios_var_struct *orig_var);

/*
 * Transforms a given variable orig_var via the given transform type.
 *
 * If use_shared_buffer is true, this function is permitted to write directly
 * to the shared buffer associated with fd (i.e. fd->buffer). It will treat the
 * buffer correctly, using the buffer_write function call or similar to update
 * offsets and allocate memory as needed.
 *
 * If use_shared_buffer is true, this function will update
 * wrote_to_shared_buffer to indicate whether the shared buffer was actually
 * used. If not, the calling function must make the necessary copies from
 * var->data. If use_shared_buffer is false, wrote_to_shared_buffer will
 * not be changed.
 *
 * Note: var->free_data will be set to adios_flag_yes iff a new buffer is
 * malloced and should later be freed by ADIOS.
 *
 * @param fd the file descriptor containing the variable to transform
 * @param var the variable whose data should be transformed (var->data)
 * @param use_shared_buffer a pointer to a boolean dictating whether the shared
 *        buffer may be directly written to.
 * @param wrote_to_shared_buffer a pointer to a boolean that should be updated
 *        indicating whether the transform method actually used the shared
 *        buffer (if not, the caller will manually copy var->data to the shared
 *        buffer). This must never be true if use_shared_buffer is false.
 * @return true for success, false otherwise
 */
int adios_transform_variable_data(struct adios_file_struct * fd,
                                  struct adios_var_struct *var,
                                  int use_shared_buffer,
                                  int *wrote_to_shared_buffer);

/*
 * Computes the worse-case required group size for an entire group.
 * Checks all variables in the group to find which transform types are used,
 * and chooses the worst of the worst-case group sizes to return.
 */
uint64_t adios_transform_worst_case_transformed_group_size(uint64_t group_size, struct adios_file_struct *fd);

//////////////////////////////////////////////////
// Transform characteristic management functions
//////////////////////////////////////////////////

// Init
int adios_transform_init_transform_var(struct adios_var_struct *var);

// Serialize
// Returns number of characteristic flags written, outputs write_length, writes to buffer
uint8_t adios_transform_serialize_transform_characteristic(const struct adios_index_characteristic_transform_struct *transform, uint64_t *write_length,
                                                           char **buffer, uint64_t *buffer_size, uint64_t *buffer_offset);
uint8_t adios_transform_serialize_transform_var(const struct adios_var_struct *var, uint64_t *write_length,
                                                char **buffer, uint64_t *buffer_size, uint64_t *buffer_offset);

// Clear
int adios_transform_clear_transform_var(struct adios_var_struct *var);

// Copy
int adios_transform_copy_transform_characteristic(struct adios_index_characteristic_transform_struct *dst_transform, const struct adios_var_struct *src_var);
int adios_transform_copy_var_transform(struct adios_var_struct *dst_var, const struct adios_var_struct *src_var);

// Calculate overhead
uint64_t adios_transform_calc_transform_characteristic_overhead(struct adios_var_struct *var);

#endif /* ADIOS_TRANSFORMS_WRITE_H_ */
