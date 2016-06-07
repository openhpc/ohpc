/*
 * detect_plugin_types.h
 *
 * Using the plugin registry, (src/transforms/transform_plugins.h), builds the
 * ADIOS_TRANSFORM_TYPE enum, including the standard "unknown", "none", and
 * "num_transform_types" entries.
 *
 *  Created on: Feb 15, 2013
 *      Author: David A. Boyuka II
 */

#ifndef DETECT_PLUGIN_TYPES_H_
#define DETECT_PLUGIN_TYPES_H_

// Set up detection macro
#define REGISTER_TRANSFORM_PLUGIN(TYPEID, XMLALIAS, UID, DESC) \
    adios_transform_##TYPEID,

// Build the enum table
enum ADIOS_TRANSFORM_TYPE {
    adios_transform_unknown   = -1,
    adios_transform_none      = 0,

    #include "transforms/transform_plugins.h" // Translate plugin register entries into enum entries

    num_adios_transform_types // Not counting unknown
};

// Clean up macro
#undef REGISTER_TRANSFORM_PLUGIN

#endif /* DETECT_PLUGIN_TYPES_H_ */
