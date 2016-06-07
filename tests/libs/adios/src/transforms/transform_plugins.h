/*
 * This file contains a registry of all transform plugins to be compiled into ADIOS.
 * If you wish to add a new transform plugin, you must add a line here.
 */

// Arguments: type ID (C identifier), in-XML name, method UID, human-readable description
REGISTER_TRANSFORM_PLUGIN(identity, "identity", "identity", "Identity transform")
REGISTER_TRANSFORM_PLUGIN(zlib, "zlib", "zlib", "zlib compression")
REGISTER_TRANSFORM_PLUGIN(bzip2, "bzip2", "bzip2", "bzip2 compression")
REGISTER_TRANSFORM_PLUGIN(szip, "szip", "szip", "szip compression")
REGISTER_TRANSFORM_PLUGIN(isobar, "isobar", "ncsu-isobar", "ISOBAR compression")
REGISTER_TRANSFORM_PLUGIN(aplod, "aplod", "ncsu-aplod", "APLOD byte-columnar precision-level-of-detail access format")
REGISTER_TRANSFORM_PLUGIN(alacrity, "alacrity", "ncsu-alacrity", "ALACRITY indexing")

