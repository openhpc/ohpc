/*
 * adios_patchdata.h
 *
 *  Created on: Jan 15, 2013
 *      Author: David A. Boyuka II
 */

#ifndef ADIOS_PATCHDATA_H_
#define ADIOS_PATCHDATA_H_

#include <public/adios_types.h>
#include <public/adios_selection.h>

/*
 * Copies data from a source buffer to a destination buffer, where the buffers each contain
 * data to fill given ADIOS_SELECTIONs. The data to be copied is the intersection of the source
 * and destination buffers, as defined by their selections. This function can only operate on
 * global selections (e.g., bounding box, points), not varblock-relative selections (e.g., writeblock).
 */
uint64_t adios_patch_data_to_global(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION *dst_sel,
                          void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION *src_sel,
                          enum ADIOS_DATATYPES datum_type,
                          enum ADIOS_FLAG swap_endianness);

/*
 * Same as adios_patch_data_to_global, but supports varblock-relative selections, and thus takes the
 * additional argument "vb_bounds_sel" to define the bounds of the varblock in this context.
 */
uint64_t adios_patch_data_to_local(void *dst, uint64_t dst_ragged_offset, const ADIOS_SELECTION *dst_sel,
                                   void *src, uint64_t src_ragged_offset, const ADIOS_SELECTION *src_sel,
                                   const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *vb_bounds,
                                   enum ADIOS_DATATYPES datum_type, enum ADIOS_FLAG swap_endianness);

#endif /* ADIOS_PATCHDATA_H_ */
