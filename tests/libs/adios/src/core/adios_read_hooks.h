/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#ifndef ADIOS_READ_HOOKS_H
#define ADIOS_READ_HOOKS_H

#include "config.h"
#include <stdint.h>
#include <string.h>
#include "public/adios_read_v2.h"
#include "core/transforms/adios_transforms_transinfo.h" // NCSU ALACRITY-ADIOS
#include "core/util.h" // PairStruct*

#define FORWARD_DECLARE(a) \
int adios_read_##a##_init_method (MPI_Comm comm, PairStruct *params); \
int adios_read_##a##_finalize_method (); \
ADIOS_FILE * adios_read_##a##_open (const char * fname, MPI_Comm comm, enum ADIOS_LOCKMODE lock_mode, float timeout_sec); \
ADIOS_FILE * adios_read_##a##_open_file (const char * fname, MPI_Comm comm); \
int adios_read_##a##_close (ADIOS_FILE *fp); \
int adios_read_##a##_advance_step (ADIOS_FILE *fp, int last, float timeout_sec); \
void adios_read_##a##_release_step (ADIOS_FILE *fp); \
ADIOS_VARINFO * adios_read_##a##_inq_var_byid (const ADIOS_FILE *gp, int varid); \
int adios_read_##a##_inq_var_stat (const ADIOS_FILE *fp, ADIOS_VARINFO * varinfo, int per_step_stat, int per_block_stat); \
int adios_read_##a##_inq_var_blockinfo (const ADIOS_FILE *fp, ADIOS_VARINFO * varinfo); \
int adios_read_##a##_schedule_read_byid (const ADIOS_FILE * fp, const ADIOS_SELECTION * sel, int varid, int from_steps, int nsteps, void * data) ; \
int adios_read_##a##_perform_reads (const ADIOS_FILE *fp, int blocking); \
int adios_read_##a##_check_reads (const ADIOS_FILE * fp, ADIOS_VARCHUNK ** chunk); \
int adios_read_##a##_get_attr_byid (const ADIOS_FILE * fp, int attrid, enum ADIOS_DATATYPES * type, int * size, void ** data); \
int adios_read_##a##_get_dimension_order (const ADIOS_FILE *fp); \
void adios_read_##a##_reset_dimension_order (const ADIOS_FILE *fp, int is_fortran); \
void adios_read_##a##_get_groupinfo (const ADIOS_FILE *fp, int *ngroups, char ***group_namelist, uint32_t **nvars_per_group, uint32_t **nattrs_per_group); \
int adios_read_##a##_is_var_timed (const ADIOS_FILE *fp, int varid); \
/* NCSU ALACRITY-ADIOS */ \
ADIOS_TRANSINFO * adios_read_##a##_inq_var_transinfo(const ADIOS_FILE *gp, const ADIOS_VARINFO *vi); \
int adios_read_##a##_inq_var_trans_blockinfo(const ADIOS_FILE *gp, const ADIOS_VARINFO *vi, ADIOS_TRANSINFO *ti);

//////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////
//// SETUP YOUR NEW READ METHODS BELOW (FOLLOW THE PATTERN):                  ////
//// 1. Add an entry to the adios_read.h/ADIOS_READ_METHOD                    ////
//// 2. Update the ADIOS_METHOD_COUNT                                         ////
//// 2. Add a FOWARD_DECLARE line (assuming standard naming)                  ////
//////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////

#define ADIOS_READ_METHOD_COUNT 9

// forward declare the functions (or dummies for internals use)
FORWARD_DECLARE(bp)
FORWARD_DECLARE(bp_staged)
FORWARD_DECLARE(bp_staged1)
#if HAVE_DATASPACES
FORWARD_DECLARE(dataspaces)
#endif
#if HAVE_FLEXPATH
FORWARD_DECLARE(flexpath)
#endif
#if HAVE_ICEE
FORWARD_DECLARE(icee)
#endif
#if HAVE_DIMES
FORWARD_DECLARE(dimes)
#endif
#if HAVE_NSSI
FORWARD_DECLARE(nssi)
#endif
#if HAVE_DATATAP
FORWARD_DECLARE(datatap)
#endif
//FORWARD_DECLARE(hdf5)


typedef int  (* ADIOS_READ_INIT_METHOD_FN) (MPI_Comm comm, PairStruct * params);
typedef int  (* ADIOS_READ_FINALIZE_METHOD_FN) ();
typedef ADIOS_FILE * (* ADIOS_READ_OPEN_FN) (const char * fname, MPI_Comm comm, 
                                 enum ADIOS_LOCKMODE lock_mode, float timeout_sec);
typedef ADIOS_FILE * (* ADIOS_READ_OPEN_FILE_FN) (const char * fname, MPI_Comm comm);
typedef int  (* ADIOS_READ_CLOSE_FN) (ADIOS_FILE *fp);
typedef int  (* ADIOS_ADVANCE_STEP_FN) (ADIOS_FILE *fp, int last, float timeout_sec);
typedef void (* ADIOS_RELEASE_STEP_FN) (ADIOS_FILE *fp);
typedef ADIOS_VARINFO * (* ADIOS_INQ_VAR_BYID_FN) (const ADIOS_FILE *fp, int varid);
typedef int  (* ADIOS_INQ_VAR_STAT_FN) (const ADIOS_FILE *fp, ADIOS_VARINFO *varinfo,
                                 int per_step_stat, int per_block_stat);
typedef int  (* ADIOS_INQ_VAR_BLOCKINFO_FN) (const ADIOS_FILE *fp, ADIOS_VARINFO *varinfo);
typedef int  (* ADIOS_SCHEDULE_READ_BYID_FN) (const ADIOS_FILE * fp, const ADIOS_SELECTION * sel, 
                                 int varid, int from_steps, int nsteps, void * data);
typedef int  (* ADIOS_PERFORM_READS_FN) (const ADIOS_FILE *fp, int blocking);
typedef int  (* ADIOS_CHECK_READS_FN) (const ADIOS_FILE * fp, ADIOS_VARCHUNK ** chunk);
typedef int  (* ADIOS_GET_ATTR_BYID_FN) (const ADIOS_FILE * fp, int attrid, 
                                 enum ADIOS_DATATYPES * type, int * size, void ** data);
typedef int  (* ADIOS_GET_DIMENSION_ORDER_FN) (const ADIOS_FILE *fp);
typedef void (* ADIOS_RESET_DIMENSION_ORDER_FN) (const ADIOS_FILE *fp, int is_fortran);
typedef void (* ADIOS_GET_GROUPINFO_FN) (const ADIOS_FILE *fp, int *ngroups, char ***group_namelist, uint32_t **nvars_per_group, uint32_t **nattrs_per_group); 
typedef int  (* ADIOS_IS_VAR_TIMED_FN) (const ADIOS_FILE *fp, int varid); 
typedef ADIOS_TRANSINFO * (*ADIOS_READ_INQ_VAR_TRANSINFO)(const ADIOS_FILE *gp, const ADIOS_VARINFO *vi); /* NCSU ALACRITY-ADIOS */
typedef int (*ADIOS_READ_INQ_VAR_TRANS_BLOCKINFO)(const ADIOS_FILE *gp, const ADIOS_VARINFO *vi, ADIOS_TRANSINFO *ti); /* NCSU ALACRITY-ADIOS */

struct adios_read_hooks_struct
{
    char * method_name;
    ADIOS_READ_INIT_METHOD_FN       adios_read_init_method_fn;
    ADIOS_READ_FINALIZE_METHOD_FN   adios_read_finalize_method_fn;
    ADIOS_READ_OPEN_FN              adios_read_open_fn;
    ADIOS_READ_OPEN_FILE_FN         adios_read_open_file_fn;
    ADIOS_READ_CLOSE_FN             adios_read_close_fn;
    ADIOS_ADVANCE_STEP_FN           adios_advance_step_fn;
    ADIOS_RELEASE_STEP_FN           adios_release_step_fn;
    ADIOS_INQ_VAR_BYID_FN           adios_inq_var_byid_fn;
    ADIOS_INQ_VAR_STAT_FN           adios_inq_var_stat_fn;
    ADIOS_INQ_VAR_BLOCKINFO_FN      adios_inq_var_blockinfo_fn;
    ADIOS_SCHEDULE_READ_BYID_FN     adios_schedule_read_byid_fn;
    ADIOS_PERFORM_READS_FN          adios_perform_reads_fn;
    ADIOS_CHECK_READS_FN            adios_check_reads_fn;
    ADIOS_GET_ATTR_BYID_FN          adios_get_attr_byid_fn;
    ADIOS_GET_DIMENSION_ORDER_FN    adios_get_dimension_order_fn;
    ADIOS_RESET_DIMENSION_ORDER_FN  adios_reset_dimension_order_fn;
    ADIOS_GET_GROUPINFO_FN          adios_get_groupinfo_fn;
    ADIOS_IS_VAR_TIMED_FN           adios_is_var_timed_fn;
    ADIOS_READ_INQ_VAR_TRANSINFO    adios_inq_var_transinfo_fn;
    ADIOS_READ_INQ_VAR_TRANS_BLOCKINFO    adios_inq_var_trans_blockinfo_fn;
};

void adios_read_hooks_init (struct adios_read_hooks_struct ** t);

#undef FORWARD_DECLARE
#endif
