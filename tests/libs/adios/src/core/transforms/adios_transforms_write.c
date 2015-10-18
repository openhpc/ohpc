
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "core/adios_bp_v1.h"
#include "core/common_adios.h"
#include "core/adios_logger.h"
#include "core/adios_internals.h"
#include "adios_types.h"
#include "core/util.h"

#include "core/transforms/adios_transforms_hooks_write.h"
#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_write.h"
#include "core/transforms/adios_transforms_specparse.h"

////////////////////////////////////////
// adios_group_size support
////////////////////////////////////////
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y) )
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y) )
uint64_t adios_transform_worst_case_transformed_group_size(uint64_t group_size, struct adios_file_struct *fd)
{
	uint64_t transformed_group_size = group_size; // The upper bound on how much data /might/ be transformed
    struct adios_var_struct *cur_var;

    // Aggregated scaling information from all transforms
    // The end result upper bound group size is:
    // GS' = total_constant_factor + GS' * max_linear_factor + min(GS', max_capped_linear_cap) * max_capped_linear_factor
    uint64_t total_constant_factor = 0;
    double max_linear_factor = 1;
    double max_capped_linear_factor = 0;
    uint64_t max_capped_linear_cap = 0;

    // Note: the "max capped linear" component is overestimating by combining the highest factor and cap from all transforms
    // A tighter lower bound could be computed by keeping all capped linear caps/factors and doing some sort of overlap
    // computation. However, this requires O(n vars) storage and extra logic that isn't worth it, given capped factors are
    // very rare, and this method gives a tight bound when none are present.

    for (cur_var = fd->group->vars; cur_var; cur_var = cur_var->next)
    {
    	if (!cur_var->dimensions) // Scalar var
        {
    		// Remove the scalar's size from the group size that can be affected by data transforms, and add it as a constant factor
    		// Even if it's a string, we don't know the content yet, so use an empty string to get minimum size (we are computing an upper bound)
    		transformed_group_size -= adios_get_type_size(cur_var->type, "");
    		total_constant_factor += adios_get_type_size(cur_var->type, "");
        }
    	else if (cur_var->transform_type == adios_transform_none) // Non-transformed, non-scalar var
    	{
            // Do nothing
        }
    	else // Transformed var
    	{
    	    uint64_t constant_factor = 0;
    	    double linear_factor = 1;
    	    double capped_linear_factor = 0;
    	    uint64_t capped_linear_cap = 0;

    	    // Get the growth factors for this transform method/spec
    	    adios_transform_transformed_size_growth(cur_var, cur_var->transform_spec, &constant_factor, &linear_factor, &capped_linear_factor, &capped_linear_cap);

    	    // Combine these growth factors into the maximums for computing the worst case
    	    total_constant_factor += constant_factor;
    	    max_linear_factor = MAX(max_linear_factor, linear_factor);
    	    max_capped_linear_factor = MAX(max_capped_linear_factor, capped_linear_factor);
    	    max_capped_linear_cap = MAX(max_capped_linear_cap, capped_linear_cap);
    	}
    }

    const uint64_t max_transformed_group_size =
    		total_constant_factor +
    		ceil(max_linear_factor * transformed_group_size) +
    		ceil(max_capped_linear_factor * MIN(transformed_group_size, max_capped_linear_cap));

    // Return the maximum worst case for the group size
    // (which can never be less than the starting group size)
    return MAX(group_size, max_transformed_group_size);
}
#undef MAX

////////////////////////////////////////
// Variable conversion to byte array (preparation for transform)
////////////////////////////////////////

static void init_dimension_item(struct adios_dimension_item_struct *dimitem) {
    dimitem->rank = 0;
    dimitem->var = NULL;
    dimitem->attr = NULL;
    dimitem->time_index = adios_flag_no;
}

static struct adios_dimension_struct * new_dimension() {
    struct adios_dimension_struct *dim = (struct adios_dimension_struct *)malloc(sizeof(struct adios_dimension_struct));

    init_dimension_item(&dim->dimension);
    init_dimension_item(&dim->global_dimension);
    init_dimension_item(&dim->local_offset);
    dim->next = 0;
    return dim;
}

// TODO: Delete this function when the new version is confirmed working (i.e., tests with FORTRAN writes and C reads, and vice versa, all work)
#if 0
static int find_time_dimension_old(struct adios_dimension_struct *dim, struct adios_dimension_struct **time_dim, enum ADIOS_FLAG fortran_order_flag) {
    struct adios_dimension_struct *cur_dim;
    int i;
    for (i = 0, cur_dim = dim; cur_dim; cur_dim = cur_dim->next, i++ ) {
        if (cur_dim->dimension.time_index == adios_flag_yes) {
            if (time_dim) *time_dim = cur_dim;
            return i;
        }

        // If we're at the last dimension, and we haven't found a time
        // dimension yet, check whether the global array is zero; if so, we
        // have a time dimension, and must infer the location based on whether
        // this is FORTRAN or not
        if (!cur_dim->next) {
            if (cur_dim->global_dimension.var == NULL && cur_dim->global_dimension.rank == 0) {
                if (fortran_order_flag == adios_flag_yes) {
                    if (time_dim) *time_dim = cur_dim;
                    return i;
                } else if (fortran_order_flag == adios_flag_no) {
                    if (time_dim) *time_dim = dim;
                    return 0;
                }
            }
        }
    }

    if (time_dim) *time_dim = 0;
    return -1;
}
#endif

static int is_dimension_item_zero(struct adios_dimension_item_struct *dim_item) {
    return dim_item->rank == 0 && dim_item->var == NULL && dim_item->attr == NULL;
}

static int is_time_dimension(struct adios_dimension_struct *dim) {
    return dim->dimension.time_index == adios_flag_yes ||
           dim->global_dimension.time_index == adios_flag_yes ||
           dim->local_offset.time_index == adios_flag_yes;
}

#if 0 
// This function is not used anywhere
static int has_time_dimension(struct adios_dimension_struct *dim, int fortran_dim_order) {
    int has_time = 0;
    struct adios_dimension_struct *first_dim = dim;
    struct adios_dimension_struct *last_dim = NULL;
    for (; dim != NULL; dim = dim->next) {
        last_dim = dim; // This will hold the last non-NULL value of dim

        // If this dimension is explicitly marked as time, then there is a time dimension
        if (is_time_dimension(dim))
            has_time = 1;
    }

    // If the last dimension has local dimension != 0 and global dimension == 0 (which we infer means there
    // is one less global dim than local), and the dimension at the expected time dimension position (first for C,
    // last for FORTRAN) has length 1, then we determine there is a time dimension.
    // FIXME: This may be affected by the same bug as mentioned near the end of adios_read_bp_is_var_timed
    //        in read_bp.c, in that this condition is ambiguous for 1D, non-timed, local arrays (if the comment
    //        in read_bp.c is addressed in the future, remove this one as well).
    const struct adios_dimension_struct *candidate_time_dim = (fortran_dim_order ? last_dim : first_dim);
    if (!is_dimension_item_zero(&last_dim->dimension) &&
        is_dimension_item_zero(&last_dim->global_dimension) &&
        candidate_time_dim->dimension.rank == 1) {
        has_time = 1;
    }

    return has_time;
}
#endif

// TODO: Delete this once the replacement is working correctly
// If there is a time dimension the final dimensions will look like this:
//   local:  t  l1 l2 l3 (or l1 l2 l3 t if fortran order)
//   global: g1 g2 g3 0
//   offset: o1 o2 o3 0
#if 0
static void adios_transform_attach_byte_array_dimensions_old(struct adios_group_struct *grp, struct adios_var_struct *var) {
    int i, new_ndim, new_time_dim_pos;
    uint64_t ldims[3];
    uint64_t gdims[3];
    uint64_t odims[3];

    int orig_ndim = count_dimensions(var->pre_transform_dimensions);
    int orig_time_dim_pos = find_time_dimension_old(var->pre_transform_dimensions, NULL, grp->adios_host_language_fortran);

    assert(orig_time_dim_pos == -1 || orig_time_dim_pos == 0 || orig_time_dim_pos == orig_ndim - 1); // Time dimension is either first, last, or non-existant

    ldims[0] = 1; // 1 PG ID in length
    ldims[1] = 0; // unknown bytes in length
    gdims[0] = UINT64_MAX >> 1; // Infinite max PGs
    gdims[1] = UINT64_MAX >> 1; // Infinite max bytes
    odims[0] = 0; // unknown PG ID
    odims[1] = 0; // 0 byte offset
    ldims[2] = gdims[2] = odims[2] = 0; // No 3rd dimension, yet

    // If we are writing from a FORTRAN file, we need to reverse the raw dimensions, so that when
    // it is read back it is properly swapped to C order
    if (grp->adios_host_language_fortran == adios_flag_yes) {
        int dummy = -1;
        swap_order(2, ldims, &dummy);
        swap_order(2, gdims, &dummy);
        swap_order(2, odims, &dummy);
    }

    // Add the time dimension
    if (orig_time_dim_pos == 0) {
        ldims[2] = ldims[1];
        ldims[1] = ldims[0];
        ldims[0] = 1;
        new_ndim = 3;
        new_time_dim_pos = 0;
    } else if (orig_time_dim_pos == orig_ndim - 1) {
        ldims[2] = 1;
        new_ndim = 3;
        new_time_dim_pos = 2;
    } else {
        new_ndim = 2;
        new_time_dim_pos = -1;
    }

    // Construct the dimension linked list
    for (i = 0; i < new_ndim; i++) {
        struct adios_dimension_struct *new_dim = new_dimension();
        new_dim->dimension.time_index = (i == new_time_dim_pos) ? adios_flag_yes : adios_flag_no;
        new_dim->dimension.rank = ldims[i];
        new_dim->global_dimension.rank = gdims[i];
        new_dim->local_offset.rank = odims[i];
        adios_append_dimension(&var->dimensions, new_dim);
    }
}
#endif

// Attaches to the given variable new metadata defining a 1D local array of bytes.
static void adios_transform_attach_byte_array_dimensions(struct adios_var_struct *var) {
    int i;

    //const int fortran_dim_order = (grp->adios_host_language_fortran == adios_flag_yes);
    //const int orig_ndim = count_dimensions(var->pre_transform_dimensions);
    //const int orig_has_time = has_time_dimension(var->pre_transform_dimensions, fortran_dim_order);

    //const int new_ndim = (orig_has_time ? 2 : 1); // 1D byte array, plus a time dimension if the original had one
    //const int new_has_time = orig_has_time;
    //const int new_time_dim_pos = (fortran_dim_order ? new_ndim - 1 : 0); // Place the time dimension last for FORTRAN order, first for C order
    const int new_ndim = 1;
    const int new_has_time = 0;
    const int new_time_dim_pos = 0;

    // Construct the dimension linked list
    for (i = 0; i < new_ndim; i++) {
        struct adios_dimension_struct *new_dim = new_dimension();

        new_dim->dimension.time_index = (new_has_time && i == new_time_dim_pos) ? adios_flag_yes : adios_flag_no;

        // Clear global dimension/local offset arrays to all 0 to indicate a local array
        // For local dimensions, set the time dimension to 1, and the non-time dimension to 0 as a placeholder
        new_dim->dimension.rank = (new_has_time && i == new_time_dim_pos) ? 1 : 0;
        new_dim->global_dimension.rank = 0;
        new_dim->local_offset.rank = 0;

        adios_append_dimension(&var->dimensions, new_dim);
    }
}

static void adios_transform_convert_var_to_byte_array(struct adios_var_struct *var) {
    // Save old metadata
    var->pre_transform_type = var->type;
    var->pre_transform_dimensions = var->dimensions;

    // Convert the type to byte array and clear the dimensions (since they were
    // moved and shouldn't be double-referenced)
    var->type = adios_byte;
    var->dimensions = 0;

    // Attach the new dimension to the variable
    adios_transform_attach_byte_array_dimensions(var);
}

////////////////////////////////////////
// Definition phase - set up transform parameters
////////////////////////////////////////

static int is_scalar(const struct adios_var_struct *var) {
    return var->dimensions == NULL;
}

static int is_timed_scalar(const struct adios_var_struct *var) {
    return var->dimensions &&                                          // Has first dimension
           var->dimensions->next == NULL &&                            // Does not have second dimension
           is_time_dimension(var->dimensions) &&                       // The first dimension is a time dimension
           is_dimension_item_zero(&var->dimensions->global_dimension); // It's not a global array
}

/*
 * Modifies the given variable's metadata to support the data transform specified by
 * orig_var->transform_spec. Also handles error conditions, such as the variable
 * being a scalar (which disallows any data transform).
 */
struct adios_var_struct * adios_transform_define_var(struct adios_var_struct *orig_var) {
    // First detect error conditions that prevent the transform from being applied

	struct adios_transform_spec *transform_spec = orig_var->transform_spec;
    if (!transform_spec) return orig_var;

    // If the variable has a transform, but is a scalar: remove the transform, warn the user, and continue as usual
    if (transform_spec->transform_type != adios_transform_none &&
        (is_scalar(orig_var) || is_timed_scalar(orig_var))) {
        log_warn("Data transforms not allowed on scalars, yet variable %s/%s is marked for transform \"%s\"; not applying data transform.\n",
                 orig_var->path, orig_var->name, transform_spec->transform_type_str);

        orig_var->transform_type = adios_transform_none;
        orig_var->transform_spec->transform_type = adios_transform_none;
        return orig_var;
    }

    // The variable has none of the above errors; apply the transform metadata

    log_debug("Transforming variable %s/%s with type %d\n", orig_var->path, orig_var->name, transform_spec->transform_type);

    // Set transform type and spec
    orig_var->transform_type = transform_spec->transform_type;

    // If there is no transform, nothing else to do
    if (transform_spec->transform_type == adios_transform_none)
        return orig_var;

    // If we get here, transform_type is an actual transformation, so prepare
    // the variable. This entails 1) adding a new dimension variable for
    // the variable (it will become a 1D byte array), and 2) making the
    // variable into a 1D byte array.

    // Convert variable to 1D byte array
    adios_transform_convert_var_to_byte_array(orig_var);
    log_debug("Data Transforms layer: Converted variable %s into byte array internally\n", orig_var->name);

    // Allocate the transform-specific metadata buffer
    orig_var->transform_metadata_len = adios_transform_get_metadata_size(transform_spec);
    if (orig_var->transform_metadata_len)
        orig_var->transform_metadata = malloc(orig_var->transform_metadata_len);

    // Return the modified variable
    return orig_var;
}

////////////////////////////////////////
// Write phase - transformed byte array dimension management
////////////////////////////////////////

// NCSU ALACRITY-ADIOS - Compute the pre-transform size of a variable, in bytes
// Precondition: var is a non-scalar that has been transformed (transform_type != none)
uint64_t adios_transform_get_pre_transform_var_size(struct adios_var_struct *var) {
    assert(var->dimensions);
    assert(var->type != adios_string);
    assert(var->transform_type != adios_transform_none);
    return adios_get_type_size(var->pre_transform_type, NULL) *
           adios_get_dimension_space_size(var,
                                          var->pre_transform_dimensions);
}

/*
 * Stores the given transformed data length (number of bytes) into the appropriate place
 * in the dimensions array (the non-time local dimension).
 */
static int adios_transform_store_transformed_length(struct adios_file_struct * fd, struct adios_var_struct *var, uint64_t transformed_len) {
    struct adios_dimension_struct *dim1, *dim2;
    struct adios_dimension_item_struct *byte_length_ldim;

    // Get the first two dimensions (only the first must always exist)
    dim1 = var->dimensions;
    assert(dim1);
    dim2 = dim1->next;

    if (dim1->dimension.time_index == adios_flag_yes) {
        // If the first dimension is a time dimension, then the byte array dimension must be the second one
        assert(dim2);
        byte_length_ldim = &dim2->dimension;
    } else {
        // Otherwise, either the second dimension is the time dimension, or there is no time dimension
        // Either way, the first dimension is the byte array dimension
        byte_length_ldim = &dim1->dimension;
    }

    // Finally, insert the byte length into the appropriate local dimension "rank" (i.e., as a literal value,
    // as opposed to a reference to a scalar/attribute).
    byte_length_ldim->rank = transformed_len;

    return 1;
}

int adios_transform_variable_data(struct adios_file_struct * fd,
                                  struct adios_var_struct *var,
                                  int use_shared_buffer,
                                  int *wrote_to_shared_buffer) {
    //printf("[TRANSFORM] Would be doing transform ID %d on variable %s here, if it were implemented\n", var->transform_type, var->name);
    //printf("[TRANSFORM] Apparent type of variable is %s, but was originally %s\n", adios_type_to_string_int(var->type), adios_type_to_string_int(var->pre_transform_type));
    //printf("[TRANSFORM] Original size of variable data is %llu\n", adios_transform_get_pre_transform_var_size(fd->group, var));

    assert(fd);
    assert(var);

    if (var->transform_type == adios_transform_none) {
        // If no transform is required, do nothing, and delegate payload
        // writing to the caller by not using the shared buffer
        *wrote_to_shared_buffer = 0;
        return 1;
    }

    assert(var->type == adios_byte); // Assume byte array
    assert(var->transform_type != adios_transform_none);

#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_start ("adios_transform_apply");
#endif
    // Transform the data, get the new length
    uint64_t transformed_len;
    int success = adios_transform_apply(fd, var, &transformed_len, use_shared_buffer, wrote_to_shared_buffer);
#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_stop ("adios_transform_apply");
#endif

    if (!success)
        return 0;

    // Store the new length in the metadata
    adios_transform_store_transformed_length(fd, var, transformed_len);

    return 1;
}



//////////////////////////////////////////////////////////////////////////
// Characteristic support, used in adios_internals.c so cannot be
// in the adios_transforms_common.c, as that isn't always linked
// with adios_internals.c.
//////////////////////////////////////////////////////////////////////////

// TODO: There are 3 exact copies of this function:
//       1) here, 2) core/adios_internals.c, 3) write/adios_adaptive.c
//       Someone should extract to a single copy and make it public via
//       a header file.
static void buffer_write (char ** buffer, uint64_t * buffer_size
                         ,uint64_t * buffer_offset
                         ,const void * data, uint64_t size
                         )
{
    if (*buffer_offset + size > *buffer_size || *buffer == 0)
    {
        char * b = realloc (*buffer, *buffer_offset + size + 1000);
        if (b)
        {
            *buffer = b;
            *buffer_size = (*buffer_offset + size + 1000);
        }
        else
        {
            fprintf (stderr, "Cannot allocate memory in buffer_write.  "
                             "Requested: %llu\n", *buffer_offset + size + 1000);

            return;
        }
    }

    memcpy (*buffer + *buffer_offset, data, size);
    *buffer_offset += size;
}

// Init
int adios_transform_init_transform_var(struct adios_var_struct *var) {
    var->transform_type = adios_transform_none;
    var->transform_spec = adios_transform_parse_spec("none", NULL);
    var->pre_transform_dimensions = 0;
    var->pre_transform_type = adios_unknown;
    //var->transform_type_param_len = 0;
    //var->transform_type_param = 0;
    var->transform_metadata_len = 0;
    var->transform_metadata = 0;
    return 1;
}

// Serialize
static void adios_transform_dereference_dimensions_characteristic(struct adios_index_characteristic_dims_struct_v1 *dst_char_dims, const struct adios_dimension_struct *src_var_dims) {
    uint8_t i;
    uint8_t c = count_dimensions(src_var_dims);

    dst_char_dims->count = c;
    dst_char_dims->dims = malloc(3 * 8 * c); // (local, global, local offset) * count
    assert(dst_char_dims->dims);
    uint64_t *ptr = dst_char_dims->dims;
    for (i = 0; i < c; i++)
    {
        //  Casts to eliminate const-ness problems
        ptr[0] = adios_get_dim_value((struct adios_dimension_item_struct *)&src_var_dims->dimension);
        ptr[1] = adios_get_dim_value((struct adios_dimension_item_struct *)&src_var_dims->global_dimension);
        ptr[2] = adios_get_dim_value((struct adios_dimension_item_struct *)&src_var_dims->local_offset);
        src_var_dims = src_var_dims->next;
        ptr += 3; // Go to the next set of 3
    }
}

static void dereference_dimension_item(struct adios_dimension_item_struct *dst_dim_item, const struct adios_dimension_item_struct *src_dim_item) {
    dst_dim_item->var = NULL;
    dst_dim_item->attr = NULL;
    dst_dim_item->rank = adios_get_dim_value((struct adios_dimension_item_struct *)src_dim_item);
    dst_dim_item->time_index = src_dim_item->time_index;
}

/*
 * Takes a given dimension struct (src_var_dims), converts all dimension items to literal
 * values by reading the value of any reference scalars/attributes. These literal values
 * are stored to a new dimension struct (dst_var_dims) in the "rank" fields. The original
 * struct is unchanged.
 */
static void dereference_dimensions_var(struct adios_dimension_struct **dst_var_dims, const struct adios_dimension_struct *src_var_dims) {
    uint8_t i;
    uint8_t c = count_dimensions(src_var_dims);

    for (i = 0; i < c; i++) {
        struct adios_dimension_struct * d_new =
            (struct adios_dimension_struct *)malloc(sizeof (struct adios_dimension_struct));

        // de-reference dimension id
        dereference_dimension_item(&d_new->dimension, &src_var_dims->dimension);
        dereference_dimension_item(&d_new->global_dimension, &src_var_dims->global_dimension);
        dereference_dimension_item(&d_new->local_offset, &src_var_dims->local_offset);
        d_new->next = 0;

        adios_append_dimension(dst_var_dims, d_new);

        src_var_dims = src_var_dims->next;
    }
}

static void adios_transform_clean_dimensions(struct adios_index_characteristic_dims_struct_v1 *dst_char_dims) {
    dst_char_dims->count = 0;
    if (dst_char_dims->dims)
        free(dst_char_dims->dims);
    dst_char_dims->dims = 0;
}

static void serialize_transform_type(enum ADIOS_TRANSFORM_TYPE transform_type,
                                     uint64_t *write_length, char **buffer, uint64_t *buffer_size, uint64_t *buffer_offset) {
    const char *transform_uid = adios_transform_plugin_uid(transform_type);
    const uint8_t transform_uid_len = (uint8_t)strlen(transform_uid);

    // Write the pre-transform datatype
    buffer_write(buffer, buffer_size, buffer_offset, &transform_uid_len, 1);
    *write_length += 1;

    // Write the number of pre-transform dimensions
    buffer_write (buffer, buffer_size, buffer_offset, transform_uid, transform_uid_len);
    *write_length += transform_uid_len;
}

static uint8_t adios_transform_serialize_transform(enum ADIOS_TRANSFORM_TYPE transform_type,
                                                   enum ADIOS_DATATYPES pre_transform_type,
                                                   const struct adios_index_characteristic_dims_struct_v1 *pre_transform_dimensions,
                                                   uint16_t transform_metadata_len,
                                                   void *transform_metadata,
                                                   uint64_t *write_length, char **buffer, uint64_t *buffer_size, uint64_t *buffer_offset) {

    // Either there is no metadata, or the metadata buffer is non-null
    assert(!transform_metadata_len || transform_metadata);

    *write_length = 0;

    // No transform case
    if (transform_type == adios_transform_none)
        return 0;

    // From this point on, this is an actual transform type

    // Write characteristic flag
    uint8_t flag = (uint8_t)adios_characteristic_transform_type;
    buffer_write(buffer, buffer_size, buffer_offset, &flag, 1);
    *write_length += 1;

    // Write transform type
    //buffer_write(buffer, buffer_size, buffer_offset, &transform_type, 1);
    //*write_length += 1;
    serialize_transform_type(transform_type, write_length, buffer, buffer_size, buffer_offset);

    // Write the pre-transform datatype
    buffer_write(buffer, buffer_size, buffer_offset, &pre_transform_type, 1);
    *write_length += 1;

    // Write the number of pre-transform dimensions
    buffer_write (buffer, buffer_size, buffer_offset, &pre_transform_dimensions->count, 1);
    *write_length += 1;

    // Write the length of pre-transform dimension data about to be written
    uint16_t len = 3 * 8 * pre_transform_dimensions->count;
    buffer_write (buffer, buffer_size, buffer_offset, &len, 2);
    *write_length += 2;

    // Write the pre-transform dimensions
    buffer_write (buffer, buffer_size, buffer_offset, pre_transform_dimensions->dims, len);
    *write_length += len;

    buffer_write (buffer, buffer_size, buffer_offset, &transform_metadata_len, 2);
    *write_length += 2;

    if (transform_metadata_len) {
        buffer_write (buffer, buffer_size, buffer_offset, transform_metadata, transform_metadata_len);
        *write_length += transform_metadata_len;
    }

    return 1; // Return that we wrote 1 characteristic flag
}

uint8_t adios_transform_serialize_transform_characteristic(const struct adios_index_characteristic_transform_struct *transform, uint64_t *write_length,
                                                           char **buffer, uint64_t *buffer_size, uint64_t *buffer_offset) {
    return adios_transform_serialize_transform(
                transform->transform_type, transform->pre_transform_type, &transform->pre_transform_dimensions,
                transform->transform_metadata_len, transform->transform_metadata,
                write_length, buffer, buffer_size, buffer_offset
           );
}

uint8_t adios_transform_serialize_transform_var(const struct adios_var_struct *var, uint64_t *write_length,
                                                char **buffer, uint64_t *buffer_size, uint64_t *buffer_offset) {

    // In this case, we are going to actually serialize the dimensions as a
    // adios_index_characteristic_dims_struct_v1, but it is currently in the
    // form of an adios_dimension_struct. We must convert here before passing
    // to the common serialization routine.

    struct adios_index_characteristic_dims_struct_v1 tmp_dims;
    adios_transform_dereference_dimensions_characteristic(&tmp_dims, var->pre_transform_dimensions);

    // Perform the serialization using the common function with the temp dimension structure
    uint8_t char_write_count =
            adios_transform_serialize_transform(
                var->transform_type, var->pre_transform_type, &tmp_dims,
                var->transform_metadata_len, var->transform_metadata,
                write_length, buffer, buffer_size, buffer_offset
            );

    // Free the temp dimension structure
    adios_transform_clean_dimensions(&tmp_dims);

    return char_write_count;
}

// Clear
int adios_transform_clear_transform_var(struct adios_var_struct *var) {
    var->transform_type = adios_transform_none;
    if (var->transform_spec)
        adios_transform_free_spec(&var->transform_spec); // Also clears to 0

    var->pre_transform_type = 0;

    // Frees and zeros-out pre_transform_dimensions (since last ->next is 0)
    while (var->pre_transform_dimensions)
    {
        struct adios_dimension_struct * dimensions = var->pre_transform_dimensions->next;
        free(var->pre_transform_dimensions);
        var->pre_transform_dimensions = dimensions;
    }

    // Free/clear transform-specific metadata
    var->transform_metadata_len = 0;
    if (var->transform_metadata)
        free(var->transform_metadata);
    var->transform_metadata = 0;

    return 1; // Return success
}

// Copy
int adios_transform_copy_transform_characteristic(struct adios_index_characteristic_transform_struct *dst_transform, const struct adios_var_struct *src_var) {
    adios_transform_init_transform_characteristic(dst_transform);

    dst_transform->transform_type = src_var->transform_type;
    dst_transform->pre_transform_type = src_var->pre_transform_type;

    // Note: the & on the first arg and not the second is intentional (the first isn't a pointer by itself, the second is)
    adios_transform_dereference_dimensions_characteristic(&dst_transform->pre_transform_dimensions, src_var->pre_transform_dimensions);

    dst_transform->transform_metadata_len = src_var->transform_metadata_len;
    if (src_var->transform_metadata_len) {
        dst_transform->transform_metadata = malloc(src_var->transform_metadata_len);
        memcpy(dst_transform->transform_metadata, src_var->transform_metadata, src_var->transform_metadata_len);
    } else {
        dst_transform->transform_metadata = 0;
    }

    return 1;
}

int adios_transform_copy_var_transform(struct adios_var_struct *dst_var, const struct adios_var_struct *src_var) {
	adios_transform_init_transform_var(dst_var);
	// Clean out the "none" transform spec added in adios_transform_init_transform_var
	if (dst_var->transform_spec)
    	adios_transform_free_spec(&dst_var->transform_spec);

	// Copy simple fields
    dst_var->transform_type = src_var->transform_type;
    dst_var->pre_transform_type = src_var->pre_transform_type;

    // Dereferemce all dimensions, forcing them to be literal values ("rank" fields), as
    // required by the function that calls this, adios_copy_var_written().
    dereference_dimensions_var(&dst_var->pre_transform_dimensions, src_var->pre_transform_dimensions);

    // Copy transform spec structure
    if (!dst_var->transform_spec)
    	dst_var->transform_spec = adios_transform_parse_spec("none", NULL);
    adios_transform_spec_copy(dst_var->transform_spec, src_var->transform_spec);

    // Copy any transform-specific metadata
    dst_var->transform_metadata_len = src_var->transform_metadata_len;
    if (src_var->transform_metadata_len && src_var->transform_metadata) {
        dst_var->transform_metadata = bufdup(src_var->transform_metadata, 1, src_var->transform_metadata_len);
    } else {
        dst_var->transform_metadata = 0;
    }

    return 1;
}


static uint64_t calc_transform_uid_overhead(struct adios_var_struct *var) {
    assert(var->transform_type != adios_transform_none && var->transform_type != adios_transform_unknown);

    const char *transform_uid = adios_transform_plugin_uid(var->transform_type);
    return 1 +                    // For the length of the UID string
           strlen(transform_uid); // The UID string itself
}

// Calculate overhead
uint64_t adios_transform_calc_transform_characteristic_overhead(struct adios_var_struct *var) {
    if (var->transform_type == adios_transform_none) {
        return 0; // No overhead needed, since the characteristic won't be written at all
    } else {
        return 1 +    // For characterstic flag
               //1 +    // For transform_type field // no longer used, replaced by transform UID
               calc_transform_uid_overhead(var) + // For the transform UID (serialized form of transform type)
               1 +    // For pre_transform_type field
               adios_calc_var_characteristics_dims_overhead(var->pre_transform_dimensions) + // For pre-transform dimensions field
               2 +    // For transform_metadata_len
               var->transform_metadata_len; // For transform_metadata
    }
}
