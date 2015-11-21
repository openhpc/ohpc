/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/*
 *   Read C API for ADIOS BP format files
 *   Define -DADIOS_USE_READ_API_1 at compile time when including "adios_read.h" 
 *   to use this version of the API. 
 */
#ifndef __ADIOS_READ_V1_H__
#define __ADIOS_READ_V1_H__

#include <stdint.h>

#include "adios_mpi.h"
#include "adios_read_v1_defs.h"
#include "adios_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/*************************/
/* Types used in the API */
/*************************/

typedef struct {
        uint64_t fh;                /* File handler                                                       */
        int      groups_count;      /* Number of adios groups in file                                     */
        int      vars_count;        /* Number of variables in all groups                                  */
        int      attrs_count;       /* Number of attributes in all groups                                 */
        int      tidx_start;        /* First timestep in file, usually 1                                  */
        int      ntimesteps;        /* Number of timesteps in file. There is always at least one timestep */
        int      version;           /* ADIOS BP version of file format                                    */
        uint64_t file_size;         /* Size of file in bytes                                              */
        int      endianness;        /* 0: little endian, 1: big endian (you do not need to care)          */
        char     ** group_namelist; /* Names of the adios groups in the file (cf. groups_count)           */
        void     * internal_data;   /* Data for internal use                                              */
} ADIOS_FILE;

typedef struct {
        uint64_t gh;                /* Group handler                                           */
        int      grpid;             /* group index (0..ADIOS_FILE.groups_count-1)              */
        int      vars_count;        /* Number of variables in this adios group                 */
        char     ** var_namelist;   /* Variable names in a char* array                         */
        int      attrs_count;       /* Number of attributes in this adios group                */
        char     ** attr_namelist;  /* Attribute names in a char* array                        */
        ADIOS_FILE * fp;            /* pointer to the parent ADIOS_FILE struct                 */
        /* streaming API: */
        int      timestep;          /* The current (only) timestep */
        int      lasttimestep;      /* The currently available latest timestep in the stream   */

} ADIOS_GROUP;

/* NCSU - Added more statistics to be returned to the end user */
typedef struct {
        int        grpid;           /* group index (0..ADIOS_FILE.groups_count-1)                     */
        int        varid;           /* variable index (0..ADIOS_GROUP.var_count-1)                    */
        enum ADIOS_DATATYPES type;  /* type of variable                                               */
        int        ndim;            /* number of dimensions, 0 for scalars                            */
        uint64_t * dims;            /* size of each dimension                                         */
        int        timedim;         /* -1: variable has no timesteps in file, >=0: which dimension is time     */
        int        characteristics_count; /* number of characteristic                                 */
        void     * value;           /* value of a scalar variable, NULL for array.                    */
        void     * gmin;            /* minimum value in an array variable, = value for a scalar       */
        void     * gmax;            /* maximum value of an array variable (over all timesteps)        */
        double   * gavg;            /* average value of an array variable (over all timesteps)        */
        double   * gstd_dev;        /* standard deviation value of an array variable (over all timesteps)        */
        void     ** mins;           /* minimum per each timestep (array of timestep elements)         */
        void     ** maxs;           /* maximum per each timestep (array of timestep elements)         */
        double   ** avgs;           /* average per each timestep (array of timestep elements)         */
        double   ** std_devs;       /* standard deviation per each timestep (array of timestep elements)         */
        struct ADIOS_HIST           /* Histogram */
        {
            uint32_t num_breaks;
            double max;
            double min;
            double *breaks;
            uint32_t **frequenciess;
            uint32_t *gfrequencies;
        } *hist;

        void * internal_data;       /* internal storage for version 2 read API data */
} ADIOS_VARINFO;

/* Needed by ADIOS_TRANSINFO below */
typedef struct {
    uint64_t * start;      /* offset start point in global array ('ndim' elements)         */
    uint64_t * count;      /* local sizes in global array ('ndim' elements)                */
} ADIOS_VARBLOCK;

/* The list of the available read methods */
enum ADIOS_READ_METHOD {
         ADIOS_READ_METHOD_BP           = 0    /* Read from ADIOS BP file (written by POSIX, MPI etc methods) */
        ,ADIOS_READ_METHOD_BP_STAGED    = 1    /* Read from ADIOS BP file (written by POSIX, MPI_AMR etc methods */
        ,ADIOS_READ_METHOD_BP_AGGREGATE = 1    /* Alias of STAGED method                                      */
        ,ADIOS_READ_METHOD_HDF5         = 2    /* Read from HDF5 file (written by PHDF5 method)               */
        ,ADIOS_READ_METHOD_DART         = 3    /* Read from memory written by DART method                     */
        ,ADIOS_READ_METHOD_DIMES        = 4    /* Read from memory written by DIMES method                    */
        ,ADIOS_READ_METHOD_NSSI         = 5    /* Read from memory written by NSSI method                     */
        ,ADIOS_READ_METHOD_DATATAP      = 6    /* Read from memory written by DATATAP method                  */
        ,ADIOS_READ_METHOD_BP_STAGED1   = 7    /* Read from ADIOS BP file (written by POSIX, MPI_AMR etc methods) */
};

#ifndef __INCLUDED_FROM_FORTRAN_API__

/** Functions that return a pointer to some data structures (fopen, gopen), return NULL
    on error and set adios_errno to a non-zero value and writes an error string.
    You have direct access to that string so you can print it.
    Do not write anything into it, please.
    Only the last error message is always available.
*/
extern int adios_errno;
const char *adios_errmsg();

/** Set the reading method for the next adios_fopen.
 *  IN:  method   read method to use
 *  RETURN:       0 if accepted, <0 on error
 *  It is optional to use it before calling adios_fopen. Default is ADIOS_READ_METHOD_BP.
 */
int adios_set_read_method (enum ADIOS_READ_METHOD method);

/** Set the application's ID for adios_read_init()
 *  when using a staging method (DART, DIMES, NSSI or DATATAP).
 *  The ID should be unique for each application accessing the staging area
 *  IN:  id   a number unique for this application
 *  RETURN:       0 if accepted, <0 on error
 *  It is optional to use it before calling adios_init. Default is 1. 
 *  It has no effect for file based methods.
 *  Note: this function is defined both in adios.h and adios_read.h so that
 *  writing-only and reading-only applications can both use it.
 */
int adios_set_application_id (int id);

/** Initialize and finalize the read method.
 *  This is needed for the DART method only and only if multiple fopen()...fclose() cycles
 *  are used. In such a case, init/finalize will perform the connection/disconnection to
 *  the DART server once.
 *  For other methods, these functions do nothing.
 */
int adios_read_init(MPI_Comm comm);
int adios_read_finalize();

/** Open an adios file.
 *  IN:  fname    pathname of file to be opened
 *       comm     the MPI communicator of all processes that want to read data from the file
 *                 if compile with -D_NOMPI, pass any integer here.
 *  RETURN:       pointer to an ADIOS_FILE struct, NULL on error (sets adios_errno)
 */
ADIOS_FILE * adios_fopen (const char * fname, MPI_Comm comm);

/** Close an adios file.
 *  It will free the content of the underlying data structures and the fp pointer itself.
 *  IN:   fp       pointer to an ADIOS_FILE struct
 *  RETURN: 0 OK, !=0 on error (also sets adios_errno)
 */
int adios_fclose (ADIOS_FILE *fp);

/** This function can be called if user places
 *  the wrong sequences of dims for a var
 */
void adios_reset_dimension_order (ADIOS_FILE *fp, int is_fortran);


/** Open an adios group. Usually there is one adios group in a file,
 *  but there can be more than one.
 *  IN:  fp       pointer to an (opened) ADIOS_FILE struct
 *       grpname  name of the group
 *  RETURN:       pointer to an ADIOS_GROUP struct, NULL on error (sets adios_errno)
 */
ADIOS_GROUP * adios_gopen (ADIOS_FILE *fp, const char * grpname);

/** Open a group by index
 *       grpid    index of group (0..fp->groups_count-1)
 *                in fp->group_namelist of ADIOS_FILE struct
 */
ADIOS_GROUP * adios_gopen_byid (ADIOS_FILE *fp, int grpid);

/** Close an adios group.
 *  To free the data structures allocated at gopen, you need to call this function
 *  IN:  gp       pointer to an (opened) ADIOS_GROUP struct
 *  RETURN: 0 OK, !=0 on error (also sets adios_errno)
 */
int adios_gclose (ADIOS_GROUP *gp);


/** Inquiry about one variable in a group.
 *  This function does not read anything from the file but processes info
 *  already in memory after fopen and gopen.
 *  It allocates memory for the ADIOS_VARINFO struct and content, so
 *  you need to free resources later with adios_free_varinfo().
 *
 *  IN:  gp       pointer to an (opened) ADIOS_GROUP struct
 *       varname  name of the variable
 *  RETURN:       pointer to and ADIOS_VARINFO struct, NULL on error (sets adios_errno)
 */
ADIOS_VARINFO * adios_inq_var (ADIOS_GROUP *gp, const char * varname);

/* NCSU - Timeseries analysis functions */
double adios_stat_cor (ADIOS_VARINFO * vix, ADIOS_VARINFO * viy, char * characteristic, uint32_t time_start, uint32_t time_end, uint32_t lag);
double adios_stat_cov (ADIOS_VARINFO * vix, ADIOS_VARINFO * viy, char * characteristic, uint32_t time_start, uint32_t time_end, uint32_t lag);
/** Inquiry a variable by index
 *       varid    index of variable (0..gp->vars_count-1)
 *                in gp->vars_namelist of ADIOS_GROUP struct
 */
ADIOS_VARINFO * adios_inq_var_byid (ADIOS_GROUP *gp, int varid);

/** Free memory used by an ADIOS_VARINFO struct */
void adios_free_varinfo (ADIOS_VARINFO *cp);

/** Read a variable (slice) from the file.
 *  You need to allocate the memory for the data.
 *  IN:  gp        pointer to an (opened) ADIOS_GROUP struct
 *       varname   name of the variable
 *       start     array of offsets to start reading in each dimension
 *       count     number of data elements to read in each dimension
 *  OUT: data      data of the variable
 *  RETURN: the number of bytes read, <0 on error, sets adios_errno too
 */
int64_t adios_read_var (ADIOS_GROUP    * gp,
                        const char     * varname,
                        const uint64_t * start,
                        const uint64_t * count,
                        void           * data);

/** Read a local variable from the file. This function is for reading BP only.
 *  To read a global variable, you should use adios_read_var() instead.
 *  You need to allocate memory for the data.
 *  IN:  gp        pointer to an (opened) ADIOS_GROUP struct
 *       varname   name of the variable
 *       idx       which local variable to read in. For example, if variable NX
                   is dumped out by every processor, this idx denotes which copy of NX
                   you want to read in. The NX dumped out by rank 0 will have idx 0.
 *       start     array of offsets to start reading in each dimension
 *       count     number of data elements to read in each dimension
 *  OUT: data      data of the variable
 *  RETURN: the number of bytes read, <0 on error, sets adios_errno too
 */
int64_t adios_read_local_var (ADIOS_GROUP    * gp,
                              const char     * varname,
                              int            idx,
                              const uint64_t * start,
                              const uint64_t * count,
                              void           * data);

/** Read a variable by index
 *       varid    index of variable (0..gp->vars_count-1)
 *                in gp->vars_namelist of ADIOS_GROUP struct
 */
int64_t adios_read_var_byid (ADIOS_GROUP * gp, int varid,
                             const uint64_t * start, const uint64_t * count,
                             void * data);

/** Get an attribute in a group.
 *  This function does not read anything from the file but processes info
 *  already in memory after fopen and gopen.
 *  The memory for the data is allocated within the library.
 *  You can use free() to free the memory after use.
 *
 *  IN:  gp       pointer to an (opened) ADIOS_GROUP struct
 *       attrname name of the attribute
 *  OUT: type     adios type of attribute (see enum ADIOS_DATATYPES in adios_types.h)
 *       size     memory size of value (n+1 for a string of n characters)
 *       data     pointer to the value. You need to cast it afterward according to the type.
 *  RETURN: 0 OK, error: set and return adios_errno
 */
int adios_get_attr (ADIOS_GROUP           * gp,
                    const char            * attrname,
                    enum ADIOS_DATATYPES  * type,
                    int                   * size,
                    void                 ** data);

/** Convenience function to get an attribute by name
 *       attrid   index of attribute (0..gp->attrs_count-1)
 *                in gp->attr_namelist of ADIOS_GROUP struct
 */
int adios_get_attr_byid (ADIOS_GROUP * gp, int attrid, enum ADIOS_DATATYPES * type,
                         int * size, void ** data);

/** Return the name of an adios type */
const char * adios_type_to_string (enum ADIOS_DATATYPES type);

/** Return the memory size of one data element of an adios type.
 *  If the type is adios_string, and the second argument is
 *  the string itself, it returns strlen(data)+1.
 *  For other types, it does not care about data and returns
 *  the size occupied by one element.
 */
int adios_type_size(enum ADIOS_DATATYPES type, void *data);

/** Test function to print basic info about the file to stdout */
void adios_print_fileinfo (ADIOS_FILE *fp);
void adios_print_groupinfo (ADIOS_GROUP *gp);

#endif  /*__INCLUDED_FROM_FORTRAN_API__*/

#ifdef __cplusplus
}
#endif

#endif  /*__ADIOS_READ_V1_H__*/
