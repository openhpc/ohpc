/*
 * adios_transforms_hooks.c
 *
 * Includes functions common to both read and write interaction with transform plugins.
 * Primarily, maintains a basic info table for each transform method, including:
 *   type: the integer ID of the transform method (i.e., enum ADIOS_TRANSFORM_TYPE)
 *   uid: the short string unique ID (UID) identifying each transform method for
 *         portability between systems, etc.
 *   description: a human-readable description of the transform method
 *
 *  Created on: Feb 14, 2013
 *      Author: David A. Boyuka II
 */

#include <assert.h>
#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_hooks.h"
#include "core/transforms/plugindetect/plugin_info_types.h"

//
// Build plugin info and aliases tables
//

// Defines ADIOS_TRANSFORM_INFOS and ADIOS_TRANSFORM_ALIASES
#include "core/transforms/plugindetect/detect_plugin_infos.h"

//
// General plugin info inspection
//

// Transform ID -> plugin info retrieval
static adios_transform_plugin_info_t * find_plugin_info(enum ADIOS_TRANSFORM_TYPE transform_type) {
    int i;
    for (i = 0; i < num_adios_transform_types; i++)
        if (ADIOS_TRANSFORM_METHOD_INFOS[i].type == transform_type)
            return &ADIOS_TRANSFORM_METHOD_INFOS[i];
    return NULL;
}

static adios_transform_plugin_xml_aliases_t * find_plugin_xml_aliases(enum ADIOS_TRANSFORM_TYPE transform_type) {
    int i;
    for (i = 0; i < num_adios_transform_types; i++)
        if (ADIOS_TRANSFORM_METHOD_ALIASES[i].type == transform_type)
            return &ADIOS_TRANSFORM_METHOD_ALIASES[i];
    return NULL;
}

const char * adios_transform_plugin_uid(enum ADIOS_TRANSFORM_TYPE transform_type) {
    adios_transform_plugin_info_t *info = find_plugin_info(transform_type);
    if (info) return info->uid;
    else      return NULL;
}

const char * adios_transform_plugin_desc(enum ADIOS_TRANSFORM_TYPE transform_type) {
    adios_transform_plugin_info_t *info = find_plugin_info(transform_type);
    if (info) return info->description;
    else      return NULL;
}

int adios_transform_plugin_num_xml_aliases(enum ADIOS_TRANSFORM_TYPE transform_type) {
    adios_transform_plugin_xml_aliases_t *aliases = find_plugin_xml_aliases(transform_type);
    if (aliases) return 1;
    else         return 0;
}

const char ** adios_transform_plugin_xml_aliases(enum ADIOS_TRANSFORM_TYPE transform_type) {
    adios_transform_plugin_xml_aliases_t *aliases = find_plugin_xml_aliases(transform_type);
    if (aliases) return &aliases->xmlAlias;
    else         return NULL;
}

const char * adios_transform_plugin_primary_xml_alias(enum ADIOS_TRANSFORM_TYPE transform_type) {
    adios_transform_plugin_xml_aliases_t *aliases = find_plugin_xml_aliases(transform_type);
    if (aliases) return aliases->xmlAlias;
    else         return NULL;
}

////////////////////////////////////////
// Transform UID -> ID
////////////////////////////////////////

/*
 * @param uid the UID (unique identifier) of a transform type
 * @return the ADIOS_TRANSFORM_TYPE corresponding to the given UID, based on
 *         the current configuration of ADIOS, or adios_transform_unknown if
 *         no transform type has been registered with that UID.
 */
enum ADIOS_TRANSFORM_TYPE adios_transform_find_type_by_uid(const char *uid) {
    int i;
    for (i = adios_transform_none; i < num_adios_transform_types; i++) {
        const adios_transform_plugin_info_t *info = &ADIOS_TRANSFORM_METHOD_INFOS[i];
        if (strcmp(uid, info->uid) == 0) {
            return info->type;
        }
    }
    return adios_transform_unknown;
}

////////////////////////////////////////
// Transform XML alias -> Transform ID conversion
////////////////////////////////////////

/*
 * @param xml_alias the name of a transform type as specified in the ADIOS XML
 * @return the ADIOS_TRANSFORM_TYPE corresponding to that alias, or
 *         adios_transform_unknown if it does not match any registered
 *         transform type
 */
enum ADIOS_TRANSFORM_TYPE adios_transform_find_type_by_xml_alias(const char *xml_alias) {
    enum ADIOS_TRANSFORM_TYPE plugin_type;
	int j;
    for (plugin_type = adios_transform_none; plugin_type < num_adios_transform_types; plugin_type++) {
        const int naliases = adios_transform_plugin_num_xml_aliases(plugin_type);
        const char **aliases = adios_transform_plugin_xml_aliases(plugin_type);

        for (j = 0; j < naliases; j++) {
            if (strcasecmp(xml_alias, aliases[j]) == 0)
                return plugin_type;
        }
    }
    return adios_transform_unknown;
}

/////////////////////////////////////////
// Other transform inspection functions
/////////////////////////////////////////

/*
 * @return non-zero if transform_type is a valid transform type (including "none"), else
 *         return zero (when transform_type is out of range, or is equal to adios_transform_unknown).
 */
int is_transform_type_valid(enum ADIOS_TRANSFORM_TYPE transform_type) {
    return transform_type >= adios_transform_none &&
           transform_type < num_adios_transform_types;
}
