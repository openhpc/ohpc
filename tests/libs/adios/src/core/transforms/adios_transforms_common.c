/*
 * All variable transformation code (excepting that for integration into
 * existing ADIOS code) resides in this file.
 *
 * - Definition of variable transformation -
 * A variable transformation takes a single variable (as defined in XML, or
 * via the noxml interface) and transforms the format of the actual data from
 * a "flat form" (where the data elements are simply linearized in some fashion
 * and stored) to a "transformed form" (where the data has been rearranged
 * and/or translated into another form, from which the original data cannot be
 * retrieved without transform-specific code/algorithms).
 *
 * - Implementation -
 *
 * The implementation has gone through a few revisions, each progressively
 * better integrated into ADIOS with less "hacking" (hopefully).
 *
 * -----------------------
 * - Implementation v0.1 -
 * -----------------------
 * This first implementation relies on intercepting variable declarations,
 * inquiries, reads and writes to translate between raw and transformed
 * data and metadata. These translations take place primarily via hooks
 * in common_adios_* functions.
 *
 * In this version, transformed variables are converted into 1D byte arrays,
 * with the original dimension/type metadata discarded. This circumvents the
 * need for low-level ADIOS changes, since after this conversion it is a
 * normal ADIOS variable.
 *
 * This conversion requires the addition of a new auxiliary variable to hold
 * the byte array length (dimension). This variable is visible to the user
 * through, for example, bpdump or bpls.
 *
 * The transformation is marked with the new "transform_type" characteristic,
 * which signals what, if any, transformation is applied.
 *
 * Limitations:
 * > Requires an extra variable for dimension
 * > Writing this extra variable requires access to common_adios_write, which
 *   is not part of the internal library, which causes linker errors; the only
 *   solutions are to copy the code (bad), or link more libraries with the
 *   utility programs (also bad)
 * > Causes two issues with adios_group_size:
 *   1) group size for the payload of the dimension variable (i.e. 8 bytes) is
 *      not automatically included, must be added to the group_size calculation
 *   2) expanded group size for the payload of size-increasing transforms is
 *      not accounted for unless the user does so explicitly
 * > Original metadata is discarded, so the user only sees a byte array
 * > No way to read back the original data, since this metadata is lost
 *
 * -----------------------
 * - Implementation 0.2 -
 * -----------------------
 * Next, we remove the auxiliary dimension variable by using explicit dimension
 * lengths. This has the same effect as putting a literal number into the
 * "dimensions" field in the XML, but it is possible to change it across time
 * steps, which is not possible in the static XML file.
 *
 * Benefits:
 * > Eliminates the extra dimension variable and linkage problems
 * > Eliminates group size problem 1)
 * > Significantly reduces code length
 *
 * Limitations:
 * > Still discards original metadata and precludes meaningful reads
 * > Still has group size problem 2)
 *
 * -----------------------
 * - Implementation 1.0 -
 * -----------------------
 * Next, we add another characteristic, "pretransform_metadata," to hold the
 * original type and dimensions of the variable before transformation. This
 * allows modification of the read API to use this original metadata where
 * appropriate to operate on the variable correctly.
 *
 * Additionally, in order to support timesteps, we add an additional time
 * index dimension variable to the transformed 1D byte array (making it a
 * 2D variable).
 *
 * Benefits:
 * > Maintains original metadata and allows reads
 *
 * Limitations:
 * > Requires additional metadata
 * > Still has group size problem 2)
 *
 * ------------------------------------
 * - Implementation 2.0 (future work) -
 * ------------------------------------
 * A more elegant solution would be to retain the original variable
 * metadata in place, calling it "logical metadata", and instead separate
 * out the "physical" metadata to store the length/format of the stored
 * data. This is more complex, as it involves breaking the implicit
 * assumption throughout the ADIOS codebase that these two metadata
 * types are one and the same. For example, currently the length of a
 * variable buffer (i.e. "physical metadata") is assumed to be
 * sizeof(type)*dim1*...dimN bytes (i.e. derived from the "logical metadata").
 * It is unclear whether the elegance of this approach is worth the
 * effort, and whether it would, in fact, have any tangible benefit
 * over the previously described API-level translation.
 *
 * One potential advantage is a reduction in total metadata space requirement.
 * Implementation 1.X requires an entire new dimension/type block, which
 * requires something like 30 bytes/var. This method would probably only
 * require only 8 bytes per var (the length in bytes of the stored buffer).
 *
 * Benefits:
 * > Reduces metadata from implementations 1.X
 *
 * Limitations:
 * > Harder/riskier to code
 * > Still has group size problem 2)
 *
 * ----------------------------
 * - The "group size" problem -
 * ----------------------------
 * The basic issue is that some transformations may increase the storage
 * footprint of a variable (indexing, for instance), but the user does not
 * account for this when calling adios_group_size. Unfortunately, it cannot
 * be automatically adjusted for in adios_group_size, because we only receive
 * the aggregate group size, and at this point dimension variables have not
 * been filled in, so we cannot calculate the expected size of each variable
 * as a basis to compute an estimated transformed storage footprint.
 *
 * The temporary stop-gap measure will be to find the most "expansive"
 * transform method used on any variable, and apply that expansion ratio
 * to the entire group size as a worst-case bound. This is not a terrible bound
 * for the transform methods currently considered (compression, which actually
 * reduces storage, and NCSU indexing, which increases storage by 30% maximum).
 * However, later transformation methods may increase storage further, thus
 * deteriorating the quality of this bound. Also, this method is rather ugly,
 * so devising a better approach would be desirable.
 *
 *  Created on: Jun 22, 2012
 *      Author: David A. Boyuka II
 */

#include "adios.h"
#include "core/adios_bp_v1.h"
#include "core/adios_internals.h"
#include "core/adios_endianness.h"
#include "core/adios_logger.h"
#include "core/bp_utils.h"
#include "core/common_adios.h"

#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_hooks.h"

#include <assert.h>
#include <stdint.h>


// Returns true for big endian, false for little endian
int get_system_endianness() {
    uint16_t word = 0x1234;
    return *(uint8_t*)(&word) == 0x12; // Returns 1 (big endian) iff the high byte comes first
}

////////////////////////////////////////
// Variable introspection
////////////////////////////////////////

enum ADIOS_DATATYPES adios_transform_get_var_original_type_var(struct adios_var_struct *var)
{
    if (var->transform_type != adios_transform_none)
        return var->pre_transform_type;
    else
        return var->type;
}

enum ADIOS_DATATYPES adios_transform_get_var_original_type_var_header(struct adios_var_header_struct_v1 *var_header)
{
    if (var_header->characteristics.transform.transform_type != adios_transform_none)
        return var_header->characteristics.transform.pre_transform_type;
    else
        return var_header->type;
}

enum ADIOS_DATATYPES adios_transform_get_var_original_type_index(struct adios_index_var_struct_v1 *var) {
    if (var->characteristics[0].transform.transform_type != adios_transform_none)
        return var->characteristics[0].transform.pre_transform_type;
    else
        return var->type;
}

int adios_transform_get_var_original_ndims_characteristic(struct adios_index_characteristic_struct_v1 *ch) {
    if (ch->transform.transform_type != adios_transform_none)
        return ch->transform.pre_transform_dimensions.count;
    else
        return ch->dims.count;
}

struct adios_index_characteristic_dims_struct_v1 * adios_transform_get_var_original_dims_characteristic(struct adios_index_characteristic_struct_v1 *ch) {
    if (ch->transform.transform_type != adios_transform_none)
        return &ch->transform.pre_transform_dimensions;
    else
        return &ch->dims;
}

int adios_transform_get_var_original_ndims_index(struct adios_index_var_struct_v1 *var) {
    return adios_transform_get_var_original_ndims_characteristic(&var->characteristics[0]);
}

int adios_transform_is_var_transformed(const struct adios_index_var_struct_v1 *var) {
    assert(var);
    if (var->characteristics_count < 1)
        return 0;

    return var->characteristics[0].transform.transform_type != adios_transform_none;
}

uint64_t adios_transform_get_var_transformed_size(const struct adios_index_var_struct_v1 *var, int time_index) {
    struct adios_index_characteristic_dims_struct_v1 *dims;
    int dim;
    uint64_t size = 1;

    //int is_global;
    uint64_t *ldims, *gdims, *offsets;

    assert(var);
    assert(adios_transform_is_var_transformed(var));
    assert(time_index < var->characteristics_count);

    dims = &var->characteristics[time_index].dims;
    ldims = malloc(sizeof(uint64_t) * dims->count);
    gdims = malloc(sizeof(uint64_t) * dims->count);
    offsets = malloc(sizeof(uint64_t) * dims->count);
    //is_global = bp_get_dimension_generic_notime(dims, ldims, gdims, offsets, 0);
    free(gdims);
    free(offsets);

    // var is non-null and is transformed, so it should be a 1D byte array.
    for (dim = 0; dim < dims->count; dim++) {
        size *= ldims[dim];
    }
    free(ldims);

    return size;
}

//////////////////////////////////////////////////
// Transform characteristic management functions
//////////////////////////////////////////////////

// Init
int adios_transform_init_transform_characteristic(struct adios_index_characteristic_transform_struct *transform) {
    transform->transform_type = adios_transform_none;
    transform->pre_transform_dimensions.count = 0;
    transform->pre_transform_dimensions.dims = 0;
    transform->pre_transform_type = adios_unknown;
    transform->transform_metadata_len = 0;
    transform->transform_metadata = 0;
    return 1;
}

// Utility functions
// TODO: These are defined local bp_utils.c. Maybe should put them in a header?
#define BUFREAD8(b,var)  var = (uint8_t) *(b->buff + b->offset); \
                         b->offset += 1;
#define BUFREAD16(b,var) var = *(uint16_t *) (b->buff + b->offset); \
                         if (b->change_endianness == adios_flag_yes) \
                             swap_16(var); \
                         b->offset += 2;
#define BUFREAD64(b,var) var = *(uint64_t *) (b->buff + b->offset); \
                         if (b->change_endianness == adios_flag_yes) \
                             swap_64(var); \
                         b->offset += 8;
#define BUFREAD(b,dst,len) memcpy((dst), (b->buff + b->offset), (len)); \
                           b->offset += (len);

static enum ADIOS_TRANSFORM_TYPE deserialize_transform_type(struct adios_bp_buffer_struct_v1 *b) {
    // Read the length of the transform UID
    uint8_t transform_uid_len;
    BUFREAD8(b, transform_uid_len);

    // Read the transform UID itself (e.g., "zlib" or "ncsu-isobar")
    char *transform_uid = calloc(1, transform_uid_len + 1);
    BUFREAD(b, transform_uid, transform_uid_len);

    enum ADIOS_TRANSFORM_TYPE transform_type = adios_transform_find_type_by_uid(transform_uid);

    free(transform_uid);
    return transform_type;
}

// Deserialize
int adios_transform_deserialize_transform_characteristic(struct adios_index_characteristic_transform_struct *transform, struct adios_bp_buffer_struct_v1 *b) {
    // The adios_characterstic_transform flag has already been read

    uint8_t i;
    uint16_t len, meta_len;

    //BUFREAD8(b, transform->transform_type);
    transform->transform_type = deserialize_transform_type(b);

    BUFREAD8(b, transform->pre_transform_type);
    BUFREAD8(b, transform->pre_transform_dimensions.count);

    BUFREAD16(b, len);
    transform->pre_transform_dimensions.dims = (uint64_t*)malloc(len);

    // Make sure length and count match up
    assert(len == 3 * 8 * transform->pre_transform_dimensions.count);

    // Read each set of 3 dimension components (dim, global dim, local offset)
    for (i = 0; i < 3 * transform->pre_transform_dimensions.count; i++) {
        BUFREAD64(b, transform->pre_transform_dimensions.dims[i]);
    }

    BUFREAD16(b, meta_len);

    if (meta_len) {
        transform->transform_metadata_len = meta_len;
        transform->transform_metadata = malloc(meta_len);
        assert(transform->transform_metadata);

        BUFREAD(b, transform->transform_metadata, meta_len);
    } else {
        transform->transform_metadata = 0;
    }

    return is_transform_type_valid(transform->transform_type);
}

// Clear
int adios_transform_clear_transform_characteristic(struct adios_index_characteristic_transform_struct *transform) {
    transform->transform_type = adios_transform_none;

    transform->pre_transform_type = 0;

    if (transform->pre_transform_dimensions.dims)
        free(transform->pre_transform_dimensions.dims);
    transform->pre_transform_dimensions.dims = 0;

    transform->transform_metadata_len = 0;
    if (transform->transform_metadata)
        free(transform->transform_metadata);
    transform->transform_metadata = 0;

    return 1; // Return success
}

int adios_transform_swap_transform_characteristics(struct adios_index_characteristic_transform_struct *trans1,
                                                   struct adios_index_characteristic_transform_struct *trans2) {
    struct adios_index_characteristic_transform_struct tmp;
    tmp = *trans1;
    *trans1 = *trans2;
    *trans2 = tmp;
    return 1;
}
