/*
 * detect_plugin_read_hooks.h
 *
 * Using the plugin registry, (src/transforms/transform_plugins.h), builds the
 * table of transform plugin read-side hooks. This should be included
 * inside a C file, as it defines a global table with memory.
 *
 *  Created on: Apr 1, 2013
 *      Author: David A. Boyuka II
 */

#ifndef DETECT_PLUGIN_READ_HOOKS_H_
#define DETECT_PLUGIN_READ_HOOKS_H_

// INPUT MACRO: Plugins will be registered in the table TRANSFORM_READ_METHODS

// NOTE: Uses the adios_transform_read_method struct from this include
// NOTE: Uses the REGISTER_TRANSFORM_READ_METHOD_HOOKS macro from this include
#include "src/core/transforms/adios_transforms_hooks_read.h"

// SETUP - Set up detection macro
#define REGISTER_TRANSFORM_PLUGIN(TYPEID, XMLALIAS, UID, DESC) \
    REGISTER_TRANSFORM_READ_METHOD_HOOKS(TRANSFORM_READ_METHODS, TYPEID, adios_transform_##TYPEID) // From adios_transforms_hooks_read.h

// DETECT
REGISTER_TRANSFORM_READ_METHOD_HOOKS(TRANSFORM_READ_METHODS, none, adios_transform_none); // Stub for "none" method
#include "transforms/transform_plugins.h"   // Translate plugin register entries into function declarations

// CLEANUP - Clean up macro
#undef REGISTER_TRANSFORM_PLUGIN

#endif /* DETECT_PLUGIN_READ_HOOKS_H_ */
