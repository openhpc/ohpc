/*
 * detect_plugin_infos.h
 *
 * Builds the plugin info and XML alias tables.
 * These tables default to being called "ADIOS_TRANSFORM_METHOD_INFOS" and
 * "ADIOS_TRANSFORM_METHOD_ALIASES", respectively; this can be changed by
 * defining these names as macros before #include'ing this file.
 *
 * This code is rather ugly, but it's necessary to enable the type of easy
 * plugin registration that is achieved without resorting to Autotools
 * magic, which looks even worse.
 *
 *  Created on: Feb 15, 2013
 *      Author: David A. Boyuka II
 */

#ifndef DETECT_PLUGIN_INFOS_H_
#define DETECT_PLUGIN_INFOS_H_

#include "core/transforms/adios_transforms_hooks.h"
#include "core/transforms/plugindetect/plugin_info_types.h"

//
// Helper macros
//

// Define some helper macros that define the record format for the
// info and aliases tables
#define _PLUGIN_INFO_RECORD(TYPEID, UID, DESC) \
    { adios_transform_##TYPEID, UID, DESC },

#define _ARRAYLEN(arr) (sizeof(arr)/sizeof(arr[0]))
#define _PLUGIN_ALIASES_RECORD(TYPEID, XMLALIAS) \
    { adios_transform_##TYPEID, (const char *)(XMLALIAS) },

//
// Actual table building
//

// Build the info table

// Define the registration macro
#define REGISTER_TRANSFORM_PLUGIN(TYPEID, XMLALIAS, UID, DESC) \
        _PLUGIN_INFO_RECORD(TYPEID, UID, DESC)

adios_transform_plugin_info_t ADIOS_TRANSFORM_METHOD_INFOS[] = {
    REGISTER_TRANSFORM_PLUGIN(none, "none", "none", "No data transform")

    #include "transforms/transform_plugins.h" // Include rows based on the plugin register statements
};

// Undef the registration macro
#undef REGISTER_TRANSFORM_PLUGIN

// Build the XML alias table

// Define the registration macro
#define REGISTER_TRANSFORM_PLUGIN(TYPEID, XMLALIAS, UID, DESC) \
        _PLUGIN_ALIASES_RECORD(TYPEID, XMLALIAS)

adios_transform_plugin_xml_aliases_t ADIOS_TRANSFORM_METHOD_ALIASES[] = {
    REGISTER_TRANSFORM_PLUGIN(none, "none", "none", "No data transform")

    #include "transforms/transform_plugins.h" // Include rows based on the plugin register statements
};

// Undef the registration macro
#undef REGISTER_TRANSFORM_PLUGIN

// Clean up macros
#undef _PLUGIN_INFO_RECORD
#undef _PLUGIN_ALIASES_RECORD
#undef _ARRAYLEN

#endif /* DETECT_PLUGIN_INFOS_H_ */
