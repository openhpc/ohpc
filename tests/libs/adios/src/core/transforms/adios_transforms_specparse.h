/*
 * adios_transforms_specparse.h
 *
 *  Created on: May 20, 2013
 *      Author: David A. Boyuka II
 */

#ifndef ADIOS_TRANSFORMS_SPECPARSE_H_
#define ADIOS_TRANSFORMS_SPECPARSE_H_

#include "core/transforms/plugindetect/detect_plugin_types.h"

struct adios_transform_spec_kv_pair{
    const char *key;
    const char *value;
};

struct adios_transform_spec {
    enum ADIOS_TRANSFORM_TYPE transform_type;
    const char *transform_type_str;

    int param_count;
    struct adios_transform_spec_kv_pair * params;

    // Internal
    int backing_str_len;
    char *backing_str;
};

/*
 * Parses the transform spec string (i.e. transform="zlib:5"), returning a struct
 * describing the result
 * @param transform_spec_str the transform spec string
 * @param pre-allocated struct to fill in; if null this func allocates the memory
 * @return the parsed transform spec
 */
struct adios_transform_spec * adios_transform_parse_spec(const char *transform_spec_str,
                                                         struct adios_transform_spec *spec_in);

/*
 * Copies a source transform spec struct into an already-allocated
 * dest transform spec struct, freeing any buffers previously
 * held by the dest struct. After this call, src and dst have the same
 * content (though all pointers are independent).
 */
void adios_transform_spec_copy(struct adios_transform_spec *dst, const struct adios_transform_spec *src);

/*
 * Frees all memory held by a adios_transform_spec struct and zeros all fields,
 * but does not free the struct itself.
 * @param spec the transform spec to free
 */
void adios_transform_clear_spec(struct adios_transform_spec *spec);

/*
 * Frees an adios_transform_spec struct.
 * @param spec the transform spec to free
 */
void adios_transform_free_spec(struct adios_transform_spec **spec);

#endif /* ADIOS_TRANSFORMS_SPECPARSE_H_ */
