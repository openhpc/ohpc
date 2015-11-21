/*
 * adios_copyspec.h
 *
 * Defines the adios_subvolume_copy_spec, which describes a logical copy
 * operation of a subvolume (i.e., multi-dimensional array) from a source
 * volume to a destination volume, as well as various functions for
 * constructing and deriving such copy specs. The source and destinations
 * volumes exist in the same global coordinate space. The subvolume is defined
 * with dimensions within the same coordinate space, and is linked to the
 * source and destination volumes by an offset within each of these volumes.
 *
 * Subvolume copy specs are somewhat related to ADIOS selection bounding boxes.
 * A subvolume copy spec could be fully defined by four bounding boxes: one
 * each to define the source and destination volumes, and one each to define
 * the location and size of the subvolume within each of these volumes. However,
 * such a decomposition stores significant redundant, and thus potentially
 * contradictory, information, including storing the subvolume dimensions twice
 * and the dimensionality of global space four times. Thus, a more coherent and
 * concise representation of a copy operation is desirable, and so is supplied
 * in this file.
 *
 * --- Subvolume copy spec definition ---
 *
 * A subvolume copy spec is defined by the following properties:
 *
 * 1) The dimensionality of global space
 * 2) The source volume dimensions and global offsets
 * 3) The destination volume dimensions and global offsets
 * 4) The subvolume dimensions, and an offsets within the each of the source
 *    and destination volumes
 *
 * For example:
 *
 *                B=(20,30)
 *     +---------------+
 *     |               |
 *     |     D=(15,25) |
 *     |    +-----+    |                          F=(50,25)
 *     |    |     |~~~~|~~~+   +-----------------------+
 *     |    |     |    |   !   |                       |
 *     |    +-----+    |   !   |                       |
 *     | (10,20)=C     |   !   |                       |
 *     |               |   !   |            H=(45,15)  |
 *     |               |   !   |           +-----+     |
 *     |               |   +~~~|~~~~~~~~~~>|     |
 *     |               |       |           |     |     |
 *     |               |       +-----------+-----+-----+
 *     |               |  E=(30,10)    G=(40,10)
 *     |               |
 *     +---------------+
 * A=(5,5)
 *
 * In this case, (A,B) is the source volume, (E,F) is the destination volume,
 * and (C,D) and (G,H) are the subvolume, as located within the source and
 * destination volumes, respectively. In this case:
 *
 * Dimensionality of global space             = 2
 * Source dims                    = B-A       = (15,25)
 * Source goffsets                = A         = (5,5)
 * Destination dims               = F-E       = (20,15)
 * Destination goffsets           = E         = (30,10)
 * Subvolume dims                 = D-C = G-H = (5,5)
 * Subvolume source offsets       = C-A       = (5,15)
 * Subvolume destination offsets  = G-E       = (10,0)
 *
 *  Created on: Aug 2, 2012
 *      Author: David A. Boyuka II
 */

#ifndef ADIOS_COPYSPEC_H_
#define ADIOS_COPYSPEC_H_

/*
 * A struct representing the copy of a subvolume of a source volume to an
 * equal-sized subvolume of a destination volume. See the comment at the top
 * of this file for more details.
 */
typedef struct {
    int ndim;
    const uint64_t *subv_dims;
    const uint64_t *dst_dims;
    const uint64_t *dst_subv_offsets;
    const uint64_t *src_dims;
    const uint64_t *src_subv_offsets;
} adios_subvolume_copy_spec;

//
// Init and free
//

/*
 * Initializes a copyspec with the given arrays. The arrays are used as-is and
 * not copied, so the user must be careful they remain available until this
 * copyspec is no longer needed. If the arrays should not be free'd when this
 * copyspec is free'd, the user must pass 0 as the the free_buffers argument to
 * adios_copyspec_free.
 *
 * Note: all offsets are in elements, not bytes. Furthermore, the element type
 * is unspecified, and must be passed at copy time.
 */
void adios_copyspec_init(adios_subvolume_copy_spec *copy_spec,
                         int ndim, const uint64_t *subv_dims,
                         const uint64_t *dst_dims, const uint64_t *dst_subv_offsets,
                         const uint64_t *src_dims, const uint64_t *src_subv_offsets);

/*
 * Initializes a copy spec to describe a copy from the given source volume
 * (src_dims and src_goffsets) to the given destination volume (described by
 * dst_dims and dst_goffsets), with the subvolume to copy being defined by the
 * intersection of these two volumes. I.e.,
 *
 *   srcv = src_dims, src_goffsets
 *   dstv = dst_dims, dst_goffsets
 *   subv = src & dst   // & = intersection
 *
 * copy_spec must not have been previously initialized, or must have been
 * subsequently free'd.
 *
 * If the source and destination volumes do indeed intersect (non-empty
 * intersection), a non-zero integer is returned and copy_spec is populated.
 * Otherwise (if the source and destination volumes are disjoint), 0 is
 * returned, and the contents of copy_spec are undefined.
 *
 * @param copy_spec the copy spec to initialize as specified above
 * @param dst_dims the dimensions of the destination volume
 * @param dst_goffsets the global offset of the destination volume
 * @param src_dims the dimensions of the source volume
 * @param src_goffsets the global offset of the source volume
 * @return a non-zero integer if the source and destination volumes have a
 *         non-empty intersection, in which case copy_spec is initialized, and
 *         0 otherwise, in which case the contents of copy_spec are undefined.
 */
int adios_copyspec_init_from_intersection(adios_subvolume_copy_spec *copy_spec, int ndim,
                                          const uint64_t *dst_dims, const uint64_t *dst_goffsets,
                                          const uint64_t *src_dims, const uint64_t *src_goffsets);

/*
 * Same as adios_copyspec_init_from_intersection, but the destination volume is
 * instead described by a bounding box struct.
 */
int adios_copyspec_init_from_bb_intersection(adios_subvolume_copy_spec *copy_spec,
                                             const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *dst_selection,
                                             const uint64_t *src_dims, const uint64_t *src_goffsets);

/*
 * Same as adios_copyspec_init_from_intersection, but both the source and
 * destination volumes are instead described by bounding box structs.
 */
int adios_copyspec_init_from_2bb_intersection(adios_subvolume_copy_spec *copy_spec,
                                              const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *dst_bb,
                                              const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *src_bb);


/*
 * Frees the resources associated with the given copyspec, and clears its
 * contents for safety. The user must specify whether the coordinate arrays
 * stored within the copyspec are also free'd, as the user may or may not
 * have used shared arrays for the init function.
 *
 * @param copy_spec the copyspec to free
 * @param free_buffers if non-zero, all buffers within the copyspec will be
 *        free'd; if zero, they will be left allocated and unchanged.
 */
void adios_copyspec_free(adios_subvolume_copy_spec **copy_spec, int free_buffers);

//
// Derivative copyspec functions
//

/*
 * Copies the contents of one copyspec into another, such that both are
 * an independent.
 *
 * @param dst_copy_spec the source copyspec
 * @param src_copy_spec the destination copyspec
 */
void adios_copyspec_copy(adios_subvolume_copy_spec *dst_copy_spec, const adios_subvolume_copy_spec *src_copy_spec);

/*
 * Creates and returns a duplicate of the given copyspec. The memory for the
 * return copyspec is obtained via malloc, and so must later be free'd by the
 * user.
 * @param copy_spec the copyspec to duplicate
 * @return a new, indepedent copyspec with the same content as the given
 *         copyspec.
 */
adios_subvolume_copy_spec * adios_copyspec_dup(const adios_subvolume_copy_spec *copy_spec);

/*
 * Derives a new copy spec that is the same as the source copy spec, but with
 * the destination volume mapped to the exact size of the subvolume. That is,
 *
 *   dst_dims := subv_dims
 *   dst_subv_offsets := {0,0, ..., 0}
 *
 * dst_copy_spec must not have been previously initialized, or must have been
 * subsequently free'd.
 *
 * @param dst_copy_spec the copy spec to populate with the new specifications.
 * @param src_copy_spec the copy spec to use as the basis for the derived copy
 *                      spec.
 */
void adios_copyspec_shrink_dst_to_subv(adios_subvolume_copy_spec * dst_copy_spec, const adios_subvolume_copy_spec *src_copy_spec);

/*
 * Derives a new copy spec that is the same as the source copy spec, but with
 * the source volume mapped to the exact size of the subvolume. That is,
 *
 *   src_dims := subv_dims
 *   src_subv_offsets := {0,0, ..., 0}
 *
 * The derived copy spec to be populated must not have been previously
 * initialized, or must have been subsequently free'd.
 *
 * @param dst_copy_spec the copy spec to populate with the new specifications.
 * @param src_copy_spec the copy spec to use as the basis for the derived copy
 *                      spec.
 */
void adios_copyspec_shrink_src_to_subv(adios_subvolume_copy_spec * dst_copy_spec, const adios_subvolume_copy_spec *src_copy_spec);

//
// Inspection/calculations with copyspecs
//

/*
 * @return non-zero if the copyspec's subvolume entirely covers the source
 *         volume, 0 otherwise.
 */
int adios_copyspec_is_subvolume_src_covering(const adios_subvolume_copy_spec *copyspec);

/*
 * @return non-zero if the copyspec's subvolume entirely covers the
 *         destination volume, 0 otherwise.
 */
int adios_copyspec_is_subvolume_dst_covering(const adios_subvolume_copy_spec *copyspec);

int adios_copyspec_is_noop(const adios_subvolume_copy_spec *copy_spec);

/*
 * @return an ADIOS_SELECTION describing the subvolume within the source
 *         volume.
 */
ADIOS_SELECTION * adios_copyspec_to_src_selection(adios_subvolume_copy_spec *copy_spec);

/*
 * @return an ADIOS_SELECTION describing the subvolume within the destination
 *         volume.
 */
ADIOS_SELECTION * adios_copyspec_to_dst_selection(adios_subvolume_copy_spec *copy_spec);

#endif /* ADIOS_COPYSPEC_H_ */
