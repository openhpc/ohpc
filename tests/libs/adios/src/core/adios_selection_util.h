/*
 * adios_selection_util.h
 *
 *  Created on: Jan 5, 2013
 *      Author: David A. Boyuka II
 */

#ifndef ADIOS_SELECTION_UTIL_H_
#define ADIOS_SELECTION_UTIL_H_

#include <public/adios_selection.h>
#include <public/adios_read_v2.h>
#include <core/transforms/adios_transforms_transinfo.h>

ADIOS_SELECTION * adios_selection_intersect_bb_bb(const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb1,
                                                  const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb2);
ADIOS_SELECTION * adios_selection_intersect_bb_pts(const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb1,
                                                   const ADIOS_SELECTION_POINTS_STRUCT *pts2);
ADIOS_SELECTION * adios_selection_intersect_pts_pts(const ADIOS_SELECTION_POINTS_STRUCT *pts1,
                                                    const ADIOS_SELECTION_POINTS_STRUCT *pts2);

/*
 * Takes the intersection between two given ADIOS selections of any type.
 * Only certain combinations of intersections are supported, as listed below.
 * An attempt to intersect any other pairing of selection types will result
 * in an adios_error being raised.
 * @param s1 the first selection
 * @param s2 the second selection
 * @return a newly-allocated selection struct representing the intersection of s1 and s2, or NULL
 *         if s1 and s2 do not intersect.
 */
ADIOS_SELECTION * adios_selection_intersect_global(const ADIOS_SELECTION *s1, const ADIOS_SELECTION *s2);

// Local (PG-relative) selection intersections

ADIOS_SELECTION * adios_selection_intersect_wb_wb(const ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb1,
                                                  const ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb2,
                                                  int timestep,
                                                  const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO *transinfo);

ADIOS_SELECTION * adios_selection_intersect_local(const ADIOS_SELECTION *s1, const ADIOS_SELECTION *s2, int timestep, const ADIOS_VARINFO *raw_varinfo, const ADIOS_TRANSINFO *transinfo);

#endif /* ADIOS_SELECTION_UTIL_H_ */
