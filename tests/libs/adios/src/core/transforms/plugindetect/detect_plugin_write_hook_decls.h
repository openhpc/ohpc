/*
 * detect_plugin_write_hooks.h
 *
 * Using the plugin registry, (src/transforms/transform_plugins.h), generates
 * declarations of transform plugin write-side hooks.
 *
 *  Created on: Apr 1, 2013
 *      Author: David A. Boyuka II
 */

#ifndef DETECT_PLUGIN_WRITE_HOOK_DECLS_H_
#define DETECT_PLUGIN_WRITE_HOOK_DECLS_H_

// NOTE: Uses the DECLARE_TRANSFORM_WRITE_METHOD macro from this include
// NOTE: Uses the DECLARE_TRANSFORM_WRITE_METHOD_UNIMPL macro from this include
#include "src/core/transforms/adios_transforms_hooks_write.h"

// SETUP - Set up detection macro
#define REGISTER_TRANSFORM_PLUGIN(TYPEID, XMLALIAS, UID, DESC) \
    DECLARE_TRANSFORM_WRITE_METHOD(TYPEID) // From adios_transforms_hooks_write.h

// DETECT
DECLARE_TRANSFORM_WRITE_METHOD_UNIMPL(none); // Declare a stub for the "none" method
#include "transforms/transform_plugins.h"   // Translate plugin register entries into function declarations

// CLEANUP - Clean up macro
#undef REGISTER_TRANSFORM_PLUGIN

#endif /* DETECT_PLUGIN_WRITE_HOOK_DECLS_H_ */
