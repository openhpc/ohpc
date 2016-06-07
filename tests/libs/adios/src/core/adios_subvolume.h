/*
 * adios_subvolume.h
 *
 * Utility functions for manipulating subvolumes of multi-dimensional arrays.
 *
 * --- Note on overlapping subvolume copies ---
 * In some special cases, it may be desirable to perform a subvolume copy where
 * the source and destination volumes overlap, and therefore memcpy operations
 * may also overlap. This operation is supported with the condition that the
 * following conditions are met:
 * 1) The source pointer is not before the destination pointer
 * 2) The size of each stride in the source volume is at least the
 *    size of the corresponding stride in the destination volume
 *
 * If these conditions hold, no source memory will ever be overwritten
 * before it is copied. This can be proven inductively:
 * 1) The first memmove will copy a block of data to an equal or lower
 *    address than its source address, since the source pointer is
 *    not before the destination pointer
 * 2) At any later point, the additional offset into the source buffer
 *    is at least the addition offset into the destination buffer,
 *    since this offset is a positive linear combination of the strides
 *    for each buffer, and the source stride is at least that of the
 *    destination stride for every dimension. Therefore, the final copy
 *    destination address is still at least that of the final source
 *    address
 *
 * Note that this works even with ragged arrays, as we only consider
 * the start offsets, which incorporate the ragged offset, and strides,
 * which are not affected by the ragged offset.
 *
 * If a subvolume copy is attempted where both of these safety conditions do
 * not hold, an assertion will fail.
 *
 *  Created on: Jul 25, 2012
 *      Author: David A. Boyuka II
 */


#ifndef ADIOS_SUBVOLUME_H_
#define ADIOS_SUBVOLUME_H_

#include <stdint.h>
#include "public/adios_types.h"
#include "public/adios_read_v2.h"
#include "public/adios_selection.h"
#include "core/adios_copyspec.h"

void vector_add(int ndim, uint64_t *dst_vec, const uint64_t *vec1, const uint64_t *vec2);
void vector_sub(int ndim, uint64_t *dst, const uint64_t *vec1, const uint64_t *vec2);

/*
 * Simple computation of the volume in elements of a given array.
 */
uint64_t compute_volume(int ndim, const uint64_t *dims);

/*
 * Calculates the intersection, if any, between two line segments in a 1D space.

 * If the line segments intersect, this function will return a non-zero value, and
 * the output arguments (inter_start, inter_len) will be populated with the offset
 * and length of the intersection region, respectively.
 *
 * If the volumes are disjoint, this function will return 0, and the content of
 * the output arguments is undefined.
 *
 * @param start1 the offset of the first line segment
 * @param len1 the length of the first line segment
 * @param start2 the offset of the second line segment
 * @param len2 the length of the second line segment
 * @param inter_start a pointer to a uint64_t to hold the intersection region's offset
 * @param inter_len a pointer to a uint64_t to hold the intersection region's length
 */
int intersect_segments(uint64_t start1, uint64_t len1, uint64_t start2, uint64_t len2,
                       uint64_t *inter_start, uint64_t *inter_len);
/*
 * Calculates the intersection, if any, between two volumes. For each volume,
 * dimensions and global offsets must be specified. If the volumes do
 * intersect, the size dimensions of the intersection are returned, as well as
 * the offset of the intersection in three forms: global, and relative to each
 * of the two volumes.
 *
 * All global offsets (offset1, offset2, inter_offset) are defined relative to
 * the same global coordinate space (typically a global array).
 *
 * All buffer arguments (everything but ndim) must be arrays of uint64_t of
 * length at least ndim (or NULL, in the case of the optional output arguments;
 * see below).
 *
 * If the volumes intersect, this function will return a non-zero value, and
 * the output arguments (inter_offset, inter_offset_rel1, inter_offset_rel2,
 * and inter_dims) will be populated with the global offset, offset relative
 * to volume 1's offset, offset relative to volume 2's offset, and dimensions
 * of the intersection volume, respectively. An appropriate buffer must be
 * supplied for inter_dims, but NULL may be supplied for any of the other
 * three output parameters if that information is not desired.
 *
 * If the volumes are disjoint, this function will return 0, and the content of
 * the output arguments is undefined.
 *
 * @param offset1 the global offset of volume 1
 * @param dims1 the dimensions of volume 1
 * @param offset2 the global offset of volume 2
 * @param dims2 the dimensions of volume 2
 * @param inter_offset a buffer to hold the offset of the intersection
 *        volume, or NULL if this information isn't required.
 * @param inter_offset_rel1 a buffer to hold the offset of the intersection
 *        volume relative to offset1, or NULL if this information isn't
 *        required.
 * @param inter_offset_rel2 a buffer to hold the offset of the intersection
 *        volume relative to offset2, or NULL if this information isn't
 *        required.
 * @param inter_dims a buffer to hold the dimensions of the intersection volume
 */
int intersect_volumes(int ndim,
                      const uint64_t *offset1, const uint64_t *dims1,
                      const uint64_t *offset2, const uint64_t *dims2,
                      uint64_t *inter_offset,
                      uint64_t *inter_offset_rel1, uint64_t *inter_offset_rel2,
                      uint64_t *inter_dims);

/*
 * Same as intersect_volume, but derive the volume bounds from bounding box
 * structs.
 */
int intersect_bb(const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb1,
                 const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb2,
                 uint64_t *inter_offset,
                 uint64_t *inter_offset_rel1, uint64_t *inter_offset_rel2,
                 uint64_t *inter_dims);

/*
 * Copies a multi-dimensional subvolume from one buffer to another.
 *
 * 'src' and 'dst' are assumed to be buffers laid out in C array dimension
 * order (the first dimension is the slowest-changing). 'dst' is assumed to
 * have the specified dimensions to accommodate the incoming subvolume.
 *
 * NOTE: See the section on "safety condition for overlapping subvolume copies"
 *       at the top of adios_subvolume.h for important information on
 *       overlapping copies.
 *
 * @param dst the destination buffer
 * @param src the source buffer
 * @param ndim the number of dimensions in the source and destination space
 * @param subv_dims the dimensions of the subvolume to copy
 * @param dst_dims the dimensions of the entire destination buffer
 * @param dst_subv_offsets the offsets at which to write the subvolume
 * @param src_dims the dimensions of the entire source buffer
 * @param src_subv_offsets the offsets from which to read the subvolume
 * @param datum_type the ADIOS datatype of the elements in the source and
 *        destination buffers
 */
void copy_subvolume(void *dst, const void *src, int ndim, const uint64_t *subv_dims,
                    const uint64_t *dst_dims, const uint64_t *dst_subv_offsets,
                    const uint64_t *src_dims, const uint64_t *src_subv_offsets,
                    enum ADIOS_DATATYPES datum_type,
                    enum ADIOS_FLAG swap_endianness);

/*
 * The same as copy_subvolume, with the addition of optional ragged src/dst
 * arrays. These arrays are ragged iff the pointer supplied does not point to
 * the logical (0,0,...,0) element of the corresponding array, but instead
 * points to some element (r1,r2,...,rn) with some ri != 0. In this case,
 * the corresponding {src,dst}_ragged_offsets designates the element pointed to
 * by the corresponding pointer.
 */
void copy_subvolume_ragged(void *dst, const void *src, int ndim, const uint64_t *subv_dims,
                           const uint64_t *dst_dims, const uint64_t *dst_subv_offsets,
                           const uint64_t *dst_ragged_offsets,
                           const uint64_t *src_dims, const uint64_t *src_subv_offsets,
                           const uint64_t *src_ragged_offsets,
                           enum ADIOS_DATATYPES datum_type, enum ADIOS_FLAG swap_endianness);

/*
 * Same as copy_subvolume_ragged, but takes a scalar byte offset for ragged
 * arrays instead of an array of element offsets.
 */
void copy_subvolume_ragged_offset(void *dst, const void *src, int ndim, const uint64_t *subv_dims,
                                  const uint64_t *dst_dims, const uint64_t *dst_subv_offsets,
                                  uint64_t dst_ragged_offset,
                                  const uint64_t *src_dims, const uint64_t *src_subv_offsets,
                                  uint64_t src_ragged_offset,
                                  enum ADIOS_DATATYPES datum_type, enum ADIOS_FLAG swap_endianness);

/*
 * Same as copy_subvolume, but derives most of the parameters from the supplied
 * subvolume copy spec.
 */
void copy_subvolume_with_spec(void *dst, const void *src,
                              const adios_subvolume_copy_spec *copy_spec,
                              enum ADIOS_DATATYPES datum_type,
                              enum ADIOS_FLAG swap_endianness);

/*
 * Same as copy_subvolume_ragged, but derives most of the parameters from the
 * supplied subvolume copy spec.
 */
void copy_subvolume_ragged_with_spec(void *dst, const void *src,
                                     const adios_subvolume_copy_spec *copy_spec,
                                     const uint64_t *dst_ragged_offsets,
                                     const uint64_t *src_ragged_offsets,
                                     enum ADIOS_DATATYPES datum_type,
                                     enum ADIOS_FLAG swap_endianness);

/*
 * Same as copy_subvolume_ragged_offset, but derives most of the parameters
 * from the supplied subvolume copy spec.
 */
void copy_subvolume_ragged_offset_with_spec(void *dst, const void *src,
                                            const adios_subvolume_copy_spec *copy_spec,
                                            uint64_t dst_ragged_offset,
                                            uint64_t src_ragged_offset,
                                            enum ADIOS_DATATYPES datum_type,
                                            enum ADIOS_FLAG swap_endianness);

/*
 * Computes the element offset of the beginning of a ragged multidimensional
 * volume array relative to the beginning of the corresponding complete volume
 * array.
 *
 * @param ndim number of dimensions of the volume
 * @param point the point within the volume
 * @param dims the dimensions of the volume
 * @return the linearized element offset of the point within the volume
 */
uint64_t compute_linear_offset_in_volume(int ndim, const uint64_t *point, const uint64_t *dims);

/*
 * Compacts a subvolume within a buffer volume to the beginning of the buffer,
 * leaving the rest of the buffer in an undefined state.
 *
 * For example, given a 10x10 subvolume of a 20x20 buffer volume, it will shift
 * the contents of that subvolume to the first 100 elements of the buffer,
 * leaving the remaining 300 elements undefined.
 *
 * @param buf the buffer for the containing volume
 * @param ndim the dimensionality of space for the volumes
 * @param subv_dims the dimensions of the subvolume
 * @param buf_dims the dimensions of the containing volume
 * @param buf_ragged_offset the ragged offset of the containing volume (if the
 *        containing volume is a complete array, set this to 0)
 * @param buf_subv_offsets the offset of the subvolume within the containing
 *        volume
 * @param elem_type the datatype of the elements of the buffer
 */
void compact_subvolume_ragged_offset(void *buf, int ndim, const uint64_t *subv_dims,
                                     const uint64_t *buf_dims, uint64_t buf_ragged_offset,
                                     const uint64_t *buf_subv_offsets,
                                     enum ADIOS_DATATYPES elem_type);


/*
 * Takes a selection that is relative to a given offset, and derelativizes it
 * from that point. For a bounding box selection, this involves shifting the
 * start coordinate of the bounding box by the given offset. For a point
 * selection, this involves adding the given offset to each point.
 *
 * Only bounding box and point selections are supported at this time;
 * attempting to derelativize some other selection type will cause a failed
 * assertion.
 *
 * @param sel the relative selection
 * @param the offset about which to derelativize the given selection
 * @return a new selection that is derelativized.
 */
ADIOS_SELECTION * new_derelativized_selection(const ADIOS_SELECTION *sel, const uint64_t *sel_global_offset);

/*
 * Converts an ADIOS_VARBLOCK to a corresponding ADIOS_SELECTION of type
 * bounding box, which will be a selection exactly matching the bounds of the
 * given ADIOS_VARBLOCK within the global space.
 * @param ndim the dimensionality of space
 * @param the ADIOS_VARBLOCK to convert to a bounding box selection
 * @return an ADIOS_SELECTION of type bounding box corresponding to the given
 *         ADIOS_VARBLOCK.
 */
ADIOS_SELECTION * varblock_to_bb(int ndim, const ADIOS_VARBLOCK *vb);

uint64_t compute_selection_size(const ADIOS_SELECTION *sel);

#endif /* ADIOS_SUBVOLUME_H_ */
