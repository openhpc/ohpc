/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/*
 *   Streaming/Chunking/Selection Read API in C for ADIOS BP format 
 *
 *   A SELECTION is the data ranges resulting from a QUERY over a file and variable(s).
 *
 *   A SELECTION can be a list of bounding boxes and point-sets. Other data structures
 *   are not supported. Any query will result in such a selection. 
 *   Other selections are the write-block and auto. 
 *
 *   Write block is a block of data written
 *   by a writer process, it is identified by an index. If each writer outputs one block
 *   then the index equals to the rank of the write process. With multi-var writing and
 *   multiple steps in a file, index runs from 0 as rank 0 process' first block. 
 *
 *   Auto selection lets the read method to automatically choose what to return. It will
 *   be a block / blocks of selected writers. 
 *
 *   If a query is a simple bounding box, the selection is the bounding box itself, so
 *   the application does not need to retrieve the selection to work on the read data.
 */
#ifndef __ADIOS_SELECTION_H__
#define __ADIOS_SELECTION_H__

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*************************/
/* Types used in the API */
/*************************/

/* Type of selection */
enum ADIOS_SELECTION_TYPE {
    ADIOS_SELECTION_BOUNDINGBOX  = 0,   /* Contiguous block of data defined by offsets and counts in each dimension */
    ADIOS_SELECTION_POINTS       = 1,   /* List of individual points                                                */
    ADIOS_SELECTION_WRITEBLOCK   = 2,   /* Selection of an individual block written by a writer process             */
    ADIOS_SELECTION_AUTO         = 3    /* Let the method decide what to return                                     */
};


/* A Bounding Box */
typedef struct { 
    int       ndim;
    uint64_t *start;
    uint64_t *count;
} ADIOS_SELECTION_BOUNDINGBOX_STRUCT;

/* A list of points.
 * It is a 1D array of indices, linearized for all dimension
 *     (e.g.  [i1,j1,k1,i2,j2,k2,...,in,jn,kn] for n points in a 3D space.
 */
typedef struct { 
    int       ndim;
    uint64_t  npoints;
    uint64_t *points;
} ADIOS_SELECTION_POINTS_STRUCT;

/* A selected block produced by a writer
 * Identified with an index in current versions.
 */
typedef struct { 
    int index;

    // NCSU ALACRITY-ADIOS:
    //     Adding timestep-relative vs. absolute writeblock selections, as
    //     well as sub-PG selection support. Both of these are currently only
    //     used by the transform layer
    int is_absolute_index;   // 0 if 'index' is relative to the current timestep, != 0
                             // otherwie (i.e., absolute index)
    int is_sub_pg_selection; // Whether this writeblock selection contains sub-PG bounds.
                             // The following fields only matter if this is != 0

    // Reads the linear range of elements in [element_offset, element_offset + nelements)
    uint64_t element_offset;
    uint64_t nelements;
} ADIOS_SELECTION_WRITEBLOCK_STRUCT;

/* Let the read method decide what to return to each reading client. 
 * Hints are method-dependent parameters to influence what and how to 
 * return (e.g. the ordering of returned chunks, decomposition among
 * read processes, etc.)
 */
typedef struct { 
    char     *hints;
} ADIOS_SELECTION_AUTO_STRUCT;

/** Selection for reading a subset of a variable. 
 *  // A selection is an additive list of bounding boxes and point-sets 
 */
//typedef struct ADIOS_SELECTION_STRUCT  ADIOS_SELECTION;
typedef struct { 
       enum ADIOS_SELECTION_TYPE    type; /* Type of selection */
       union {
            ADIOS_SELECTION_BOUNDINGBOX_STRUCT bb;
            ADIOS_SELECTION_POINTS_STRUCT points;
            ADIOS_SELECTION_WRITEBLOCK_STRUCT block;
            ADIOS_SELECTION_AUTO_STRUCT autosel;
       } u;
       /*ADIOS_SELECTION             *next;*/
} ADIOS_SELECTION;

#ifndef __INCLUDED_FROM_FORTRAN_API__

/** Boundingbox selection to read a subset of a non-scalar variable.
 *  IN:
 *       ndim      Number of dimensions
 *       start     array of offsets to start reading in each dimension
 *       count     number of data elements to read in each dimension
 *  RETURN:        A selection which can be used to read variables
 */
ADIOS_SELECTION * adios_selection_boundingbox (int ndim, const uint64_t *start, const uint64_t *count);


/** Selection for a selection of an enumeration of positions.
 *  IN:
 *       ndim      Number of dimensions
 *       npoints   Number of points of the selection
 *       points    1D array of indices, compacted for all dimension
 *                 (e.g.  [i1,j1,k1,i2,j2,k2,...,in,jn,kn] for
 *                 n points in a 3D space.
 */
ADIOS_SELECTION* adios_selection_points (int ndim, uint64_t npoints, const uint64_t *points);

/** Selection for a block of data coming from a certain producer.
 *
 *  IN:
 *       index      Identifier of the written block, starting from 0 for the first block
 *                  written by producer rank 0. If each writer outputs one block
 *                  then the index equals to the rank of the write process. 
 *                  With multi-var writing and multiple steps in a file, index should be
 *                  calculated by the reading application using outside information beyond
 *                  what is provided by the ADIOS Read API.
 */
ADIOS_SELECTION* adios_selection_writeblock (int index);

/** Let the method decide what data gets to what reader process.
 *  This selection enables each reading method to provide an 'optimal'
 *  data transfer from writers to readers. It depends on the method and the 
 *  circumstances, what this selection actually means.
 *
 *  E.g. in situ processing: readers on a compute node will receive all data 
 *       from the writers on the same compute node.
 *
 *  IN:
 *       hints    Method dependent parameters to influence what and how to 
 *                return (e.g. decomposition; ordering of returned chunks)
 */
ADIOS_SELECTION* adios_selection_auto (char * hints);

/** Make a strided hyperslab selection the same way as in HDF5. 
 *  IN:
 *       ndim      Number of dimentsions
 *       start     array of offsets to start reading in each dimension
 *       strides   striding steps, NULL=1 in all dimensions (= boundingbox) 
 *       count     number of data elements to read in each dimension
 *       blocks    block size at each stride, NULL=1 in all dimensions
 */
/*
   No support: Klasky, Podhorszki
   Support: 

ADIOS_SELECTION* adios_selection_hyperslab (uint64_t ndim, uint64_t *start, uint64_t *strides, 
                                    uint64_t *count, uint64_t *blocks);
*/


/** Delete a selection and free up memory used by the selection.
  * IN: selection
  * RESULT: None
  * The ADIOS_SELECTION object can be simply freed by free(), too. 
  */
void adios_selection_delete (ADIOS_SELECTION *selection);


#endif  /*__INCLUDED_FROM_FORTRAN_API__*/

#ifdef __cplusplus
}
#endif

#endif  /*__ADIOS_SELECTION_H__*/
