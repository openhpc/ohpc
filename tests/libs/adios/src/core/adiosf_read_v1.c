/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define __INCLUDED_FROM_FORTRAN_API__
#include "adios_read.h"
#include "adios_error.h"
#include "core/bp_utils.h" // bp_get_type_size()
//#include "core/bp_types.h"
#include "core/common_read.h"
#include "core/futils.h"
#define BYTE_ALIGN 8

#ifdef DMALLOC
#include "dmalloc.h"
#endif

#ifdef __cplusplus
extern "C"  /* prevent C++ name mangling */
#endif

#ifdef BUILD_WITH_CMAKE
  #include "FC.h"
#endif

static enum ADIOS_READ_METHOD lastmethod = ADIOS_READ_METHOD_BP;

extern int adios_errno;

// Re-define the ADIOS_GROUP struct here, because we need to emulate the group view behavior
// of the old read API
typedef struct {
    uint64_t gh;                /* Group handler                                           */
    int      grpid;             /* group index (0..ADIOS_FILE.groups_count-1)              */
    int      vars_count;        /* Number of variables in this adios group                 */
    char     ** var_namelist;   /* Variable names in a char* array                         */
    int      attrs_count;       /* Number of attributes in this adios group                */
    char     ** attr_namelist;  /* Attribute names in a char* array                        */
    ADIOS_FILE * fp;            /* pointer to the parent ADIOS_FILE struct                 */
} ADIOS_GROUP_V1;

/*********************/
/* FORTRAN INTERFACE */
/*********************/
void FC_FUNC_(adios_errmsg, adios_errmsg) (char *msg, int msg_len)
{
    futils_cstr_to_fstr( adios_get_last_errmsg(), (char *)msg, msg_len);
}

void FC_FUNC_(adios_set_read_method, ADIOS_SET_READ_METHOD) (int *fmethod, int *err)
{
    switch (*fmethod) {
        // Translate from V1 method IDs to the new API's methods
        case 0: //ADIOS_READ_METHOD_BP_V1:
            lastmethod = ADIOS_READ_METHOD_BP;
            break;
        case 1: //ADIOS_READ_METHOD_BP_STAGED_V1:
            lastmethod = ADIOS_READ_METHOD_BP_AGGREGATE;
            break;
        case 7: //ADIOS_READ_METHOD_BP_STAGED1_V1:
            lastmethod = ADIOS_READ_METHOD_BP_AGGREGATE;
            break;
        case 3: //ADIOS_READ_METHOD_DART_V1:
            lastmethod = ADIOS_READ_METHOD_DATASPACES;
            break;
        case 4: //ADIOS_READ_METHOD_DIMES_V1:
            lastmethod = ADIOS_READ_METHOD_DIMES;
            break;

            /* Unsupported methods */
        case 2: //ADIOS_READ_METHOD_HDF5_V1:
        case 5: //ADIOS_READ_METHOD_NSSI_V1:
        case 6: //ADIOS_READ_METHOD_DATATAP_V1:
        default:
            lastmethod = ADIOS_READ_METHOD_BP;
            break;
    }
    *err = 0;
}

void FC_FUNC_(adios_read_init, ADIOS_READ_INIT) (int * fcomm, int * err)
{
    MPI_Comm comm = MPI_Comm_f2c (*((int *) fcomm));
    futils_called_from_fortran_set();
    *err =  common_read_init_method (lastmethod, comm, NULL);
    if (*err)
        fprintf(stderr, "Error: %s\n", adios_get_last_errmsg());
}

void FC_FUNC_(adios_read_finalize, ADIOS_READ_FINALIZE) (int * err)
{
    *err = common_read_finalize_method (lastmethod);
    futils_called_from_fortran_unset();
    if (*err)
        fprintf(stderr, "Error: %s\n", adios_get_last_errmsg());
}

void FC_FUNC_(adios_fopen, ADIOS_FOPEN)
        (int64_t * fp,
         char    * fname,
         void    * fcomm,
         int     * groups_count,
         int     * err,
         int       fname_len)
{
    ADIOS_FILE *afp;
    char *namestr;
    char **group_namelist;
    MPI_Comm comm = MPI_Comm_f2c (*((int *) fcomm));
    futils_called_from_fortran_set();

    namestr = futils_fstr_to_cstr(fname, fname_len);
    if (namestr != NULL) {
        afp = common_read_open_file (namestr, lastmethod, comm);
        if (afp != NULL) {
            *groups_count = common_read_get_grouplist(afp, &group_namelist);
        } else {
            *groups_count = 0;
        }
        *fp = (int64_t) afp;
        free(namestr);
    } else {
        *fp = (int64_t) NULL;
    }
    *err = adios_errno;
    if (*err)
        fprintf(stderr, "Error: %s\n", adios_get_last_errmsg());
}

void FC_FUNC_(adios_reset_dimension_order, ADIOS_RESET_DIMENSION_ORDER)
        (int64_t * fp,
         int * flag)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    common_read_reset_dimension_order(afp, *flag);
}

void FC_FUNC_(adios_fclose, ADIOS_FCLOSE) (int64_t * fp, int * err)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    *err = common_read_close (afp);
    futils_called_from_fortran_unset();
    if (*err)
        fprintf(stderr, "Error: %s\n", adios_get_last_errmsg());
}

void FC_FUNC_(adios_inq_file, ADIOS_INQ_FILE)
        (int64_t * fp,
         int     * vars_count,
         int     * attrs_count,
         int     * tstart,
         int     * ntsteps,
         void    * gnamelist,
         int     * err,
         int       gnamelist_len)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    int i;
    char **group_namelist;
    int ngroups;
    if (afp != NULL) {
        *vars_count = afp->nvars;
        *attrs_count = afp->nattrs;
        *tstart = 1;
        *ntsteps = afp->last_step-afp->current_step+1;
        ngroups = common_read_get_grouplist(afp, &group_namelist);
        for (i=0;i<ngroups;i++) {
            futils_cstr_to_fstr( group_namelist[i], (char *)gnamelist+i*gnamelist_len, gnamelist_len);
        }
        *err = 0;
    } else {
        *vars_count = 0;
        *attrs_count = 0;
        *tstart = 0;
        *ntsteps = 0;
        *err = 1;
    }
}

void FC_FUNC_(adios_gopen, ADIOS_GOPEN)
        (int64_t * fp,
         int64_t * gp,
         char    * grpname,
         int     * vars_count,
         int     * attrs_count,
         int     * err,
         int       grpname_len)
{
    char *namestr;
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    char **group_namelist;
    int ngroups, grpid;

    namestr = futils_fstr_to_cstr(grpname, grpname_len);
    if (namestr != NULL) {

        // get group list and find the group name
        ngroups = common_read_get_grouplist(afp, &group_namelist);
        for (grpid=0;grpid<(ngroups);grpid++) {
            if (!strcmp(group_namelist[grpid], namestr))
                break;
        }

        if (grpid < ngroups) {

            // Set group view of file for this group
            common_read_group_view (afp, grpid);

            *vars_count = afp->nvars;
            *attrs_count = afp->nattrs;
            *gp = (int64_t)afp;
        } else {
            adios_error ( err_invalid_group, "Invalid group name %s", grpname);
            *gp = (int64_t) NULL;
        }
        free(namestr);

    } else {
        *gp = (int64_t) NULL;
    }
    *err = adios_errno;
    if (*err)
        fprintf(stderr, "Error: %s\n", adios_get_last_errmsg());
}

void FC_FUNC_(adios_gclose, ADIOS_GCLOSE) (int64_t * gp, int * err)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *gp;
    // restore full file view
    *err=common_read_group_view (afp, -1);
    if (*err)
        fprintf(stderr, "Error: %s\n", adios_get_last_errmsg());
}

void FC_FUNC_(adios_inq_group, ADIOS_INQ_GROUP)
        (int64_t * gp,
         void    * vnamelist,
         void    * anamelist,
         int     * timestep,
         int     * lasttimestep,
         int     * err,
         int       vnamelist_len,
         int       anamelist_len)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *gp;
    int i;
    if (afp != NULL) {
        for (i=0;i<afp->nvars;i++) {
            futils_cstr_to_fstr( afp->var_namelist[i], (char *)vnamelist+i*vnamelist_len, vnamelist_len);
        }
        for (i=0;i<afp->nattrs;i++) {
            futils_cstr_to_fstr( afp->attr_namelist[i], (char *)anamelist+i*anamelist_len, anamelist_len);
        }
        *timestep = 1;
        *lasttimestep = afp->last_step;
        *err = 0;
    } else {
        *err = 1;
    }
}

void FC_FUNC_(adios_inq_var, ADIOS_INQ_VAR)
        (int64_t  * gp,
         char     * varname,
         int      * type,
         int      * ndim,
         uint64_t * dims,
         int      * timedim,
         int      * err,
         int        varname_len)
{
    char *varstr;
    int  i;
    ADIOS_FILE *afp = (ADIOS_FILE *) *gp;
    ADIOS_VARINFO *vi = NULL;

    varstr = futils_fstr_to_cstr(varname, varname_len);
    if (varstr != NULL) {
        vi = common_read_inq_var (afp, varstr);
    }
    if (vi != NULL) {
        *type = vi->type;

        /* TIME dimension should be emulated here !!! */
        int tidx;
        if (vi->nsteps > 1) {
            *ndim = vi->ndim + 1;
            *timedim = vi->ndim;
            dims[0] = vi->nsteps;
            tidx = 1;
        } else {
            *ndim = vi->ndim;
            *timedim = -1;
            tidx = 0;
        }

        for (i=0; i<vi->ndim; i++)
            dims[i+tidx] = vi->dims[i];

        common_read_free_varinfo(vi);

    } else {
        *type = adios_unknown;
        *ndim = 0;
        *timedim = -1;
    }
    *err = adios_errno;
    if (*err < 0)
        fprintf(stderr, "Error: %s\n", adios_get_last_errmsg());
}

void FC_FUNC_(adios_read_var, ADIOS_READ_VAR)
        (int64_t  * gp,
         char     * varname,
         uint64_t * start,
         uint64_t * count,
         void     * data,
         int64_t  * read_bytes,
         int varname_len)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *gp;
    char *varstr;
    int i;
    varstr = futils_fstr_to_cstr(varname, varname_len);
    if (varstr != NULL) {
        /* First get the number of dimensions of the variable */
        ADIOS_VARINFO * vi = common_read_inq_var (afp, varstr);
        if (!vi)  {
            *read_bytes = adios_errno;
            return;
        }
        int from_step = 0, nsteps = 1;
        /* TIME dimension should be emulated here !!! */
        if (vi->nsteps > 1) {
            from_step = (int) start[vi->ndim];
            nsteps    = (int) count[vi->ndim];
        }

        ADIOS_SELECTION * sel = common_read_selection_boundingbox (vi->ndim, start, count);

        common_read_schedule_read (afp, sel, varstr, from_step, nsteps, NULL /* NCSU ALACRITY-ADIOS */, data);
        int ret = common_read_perform_reads (afp, 1);
        if (ret == err_no_error) {
            /* should return the number of bytes read */
            *read_bytes = (int64_t) common_read_type_size (vi->type, data);
            *read_bytes *= nsteps;
            for (i=0; i<vi->ndim; i++)
                *read_bytes *= (int64_t) count[i];
        } else {
            *read_bytes = (int64_t) adios_errno;
        }

        common_read_free_varinfo(vi);
        common_read_selection_delete(sel);
        free(varstr);
    } else {
        *read_bytes = adios_errno;
    }
    if (*read_bytes < 0)
        fprintf(stderr, "Error: %s\n", adios_get_last_errmsg());
}

void FC_FUNC_(adios_read_local_var, ADIOS_READ_LOCAL_VAR)
        (int64_t  * gp,
         char     * varname,
         int      * idx,
         uint64_t * start,
         uint64_t * count,
         void     * data,
         int64_t  * read_bytes,
         int varname_len)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *gp;
    ADIOS_VARINFO *vi = NULL;
    char *varstr;
    int i;
    varstr = futils_fstr_to_cstr(varname, varname_len);
    if (varstr != NULL) {
        /* The new API does not support reading write block with subselection.
           Only complete blocks can be read.
           Here we check if the passed selection equals the whole block.
           Otherwise we return an out of bound error, although it probably should
           be a "within bounds" error

           Version 1:
           - start/count has no time dimension for the local read
           - idx is for all timesteps, 0...timesteps*blocks
           Version 2:
           - idx is from 0..blocks for each timestep
           - need to say which timestep
         */

        /* First get the number of dimensions of the variable */
        vi = common_read_inq_var (afp, varstr);
    }

    if (vi != NULL) {
        /* get step from the idx, and check validity of idx */
        int step = 0;
        int idx_in_step = *idx;
        step = 0;
        while (step < vi->nsteps && idx_in_step >= vi->nblocks[step]) {
            idx_in_step -= vi->nblocks[step];
            step++;
        }
        if (step == vi->nsteps) {
            adios_error (err_out_of_bound, "ADIOS ERROR: local "
                    "variable %s has only %d blocks in file. "
                    "Requested index %d\n", varstr, vi->sum_nblocks, idx);
            *read_bytes = adios_errno;
            return;
        }
        common_read_inq_var_blockinfo (afp, vi); // get info on each block

        /* check dimensions */
        for (i=0; i<vi->ndim; i++) {
            if (start[i] != 0 || count[i] != vi->blockinfo[*idx].count[i]) {
                adios_error (err_out_of_bound, "ADIOS ERROR: when reading a local "
                        "variable, only the whole block can be requested; subselections "
                        "are not allowed. Variable %s, block %d, dimension %d size is %lld, "
                        "requested %lld from offset %lld\n",
                        varstr, *idx, vi->blockinfo[*idx].count[i], count[i], start[i]);
                *read_bytes = adios_errno;
                return;
            }
        }

        ADIOS_SELECTION * sel = common_read_selection_writeblock (idx_in_step);

        common_read_schedule_read_byid (afp, sel, vi->varid, step, 1, NULL /* NCSU ALACRITY-ADIOS */, data);
        int ret = common_read_perform_reads (afp, 1);
        if (ret == err_no_error) {
            /* should return the number of bytes read */
            *read_bytes = (int64_t) common_read_type_size (vi->type, data);
            for (i=0; i<vi->ndim; i++)
                *read_bytes *= (int64_t) count[i];
        } else {
            *read_bytes = (int64_t) adios_errno;
        }

        common_read_free_varinfo(vi);
        common_read_selection_delete(sel);
        free(varstr);

    } else {
        // return error from futils or inq_var
        *read_bytes = adios_errno;
    }

    if (*read_bytes < 0)
        fprintf(stderr, "Error: %s\n", adios_get_last_errmsg());
}

/* Specific function for each data type */
void FC_FUNC_(adios_read_var_int1, ADIOS_READ_VAR_INT1) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }
void FC_FUNC_(adios_read_var_int2, ADIOS_READ_VAR_INT2) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }
void FC_FUNC_(adios_read_var_int4, ADIOS_READ_VAR_INT4) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }
void FC_FUNC_(adios_read_var_int8, ADIOS_READ_VAR_INT8) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }
void FC_FUNC_(adios_read_var_real4, ADIOS_READ_VAR_REAL4) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }
void FC_FUNC_(adios_read_var_real8, ADIOS_READ_VAR_REAL8) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }
void FC_FUNC_(adios_read_var_char, ADIOS_READ_VAR_CHAR) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }
void FC_FUNC_(adios_read_var_complex8, ADIOS_READ_VAR_COMPLEX8) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }
void FC_FUNC_(adios_read_var_complex16, ADIOS_READ_VAR_COMPLEX16) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }
void FC_FUNC_(adios_read_var_logical1, ADIOS_READ_VAR_LOGICAL1) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }
void FC_FUNC_(adios_read_var_logical2, ADIOS_READ_VAR_LOGICAL2) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }
void FC_FUNC_(adios_read_var_logical4, ADIOS_READ_VAR_LOGICAL4) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }
void FC_FUNC_(adios_read_var_logical8, ADIOS_READ_VAR_LOGICAL8) (int64_t * gp, char * varname, uint64_t * start, uint64_t * count, void * data, int64_t * read_bytes, int varname_len) { FC_FUNC_(adios_read_var, ADIOS_READ_VAR) (gp, varname, start, count, data, read_bytes, varname_len); }

void FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS)
        (int64_t * gp,
         char    * varname,
         void    * value,
         void    * gmin,
         void    * gmax,
         double  * gavg,
         double  * gstd_dev,
         void   ** mins,
         void   ** maxs,
         double ** avgs,
         double ** std_devs,
         int     * err,
         int       varname_len)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *gp;
    ADIOS_VARINFO *vi = NULL;
    char *varstr;
    int size;

    varstr = futils_fstr_to_cstr(varname, varname_len);
    if (varstr != NULL) {
        /* First get the number of dimensions of the variable */
        vi = common_read_inq_var (afp, varname);
    }
    if (vi != NULL) {
        size = bp_get_type_size(vi->type, vi->value);
        // get statistics information per each step
        common_read_inq_var_stat (afp, vi, 1, 0);
        if (vi->type == adios_string) size++;
        if (vi->value) memcpy(value, vi->value, size);

        if (vi->statistics) {
            if (vi->type == adios_complex || vi->type == adios_double_complex) {
                /* For complex numbers, the statistics in ADIOS_VARINFO, like
                   gmin, gavg, std_devs etc, are of base type double. They also
                   have an additional dimension that stores the statistics for
                   the magnitude, the real part, and the imaginary part of the
                   complex number, individually. For example, gmin[0] holds the
                   overall minimum value of the magnitude of the complex numbers.
                   gmin[1] and gmin [2] contain the global minimums for the real
                   and the imaginary parts, respectively.
                 */
                if (vi->statistics->min)
                    memcpy((char *) gmin, (char *) vi->statistics->min, 3*size);
                if (vi->statistics->max)
                    memcpy(((char *) gmax), (char *) vi->statistics->max, 3*size);
                if (vi->statistics->avg)
                    memcpy(gavg, vi->statistics->avg, 3*sizeof(double));
                if (vi->statistics->std_dev)
                    memcpy(gstd_dev, vi->statistics->std_dev, 3*sizeof(double));

                /* FIXME: I do not know if mins is **void and mins[i] is a void * allocated
                   separately, or mins is a continuous array now.
                   Copy content accordingly back to the Fortran client.
                */
                int c;
                double ** v_mins = NULL;
                double ** v_maxs = NULL;
                double ** v_avgs = NULL;
                double ** v_std_devs = NULL;
                if (vi->statistics->steps) {
                    v_mins = (double **) vi->statistics->steps->mins;
                    v_maxs = (double **) vi->statistics->steps->maxs;
                    v_avgs = (double **) vi->statistics->steps->avgs;
                    v_std_devs = (double **) vi->statistics->steps->std_devs;
                }

                for (c = 0; c < 3; c ++)
                {
                    if (v_mins && v_mins[c]) memcpy(((double **) mins)[c], v_mins[c], vi->nsteps * size);
                    if (v_maxs && v_maxs[c]) memcpy(((double **) maxs)[c], v_maxs[c], vi->nsteps * size);
                    if (v_avgs && v_avgs[c]) memcpy(avgs[c], v_avgs[c], vi->nsteps * sizeof(double));
                    if (v_std_devs && v_std_devs[c]) memcpy(std_devs[c], v_std_devs[c], vi->nsteps * sizeof(double));
                }

            } else {
                if (vi->statistics->min)
                    memcpy((char *) gmin, (char *) vi->statistics->min, size);
                if (vi->statistics->max)
                    memcpy((char *) gmax, (char *) vi->statistics->max, size);
                if (vi->statistics->avg)
                    memcpy(gavg, vi->statistics->avg, sizeof(double));
                if (vi->statistics->std_dev)
                    memcpy(gstd_dev, vi->statistics->std_dev, sizeof(double));

                if (vi->statistics->steps) {
                    if (vi->statistics->steps->mins)
                        memcpy((char *) mins, (char *) vi->statistics->steps->mins, vi->nsteps * size);
                    if (vi->statistics->steps->maxs)
                        memcpy((char *) maxs, (char *) vi->statistics->steps->maxs, vi->nsteps * size);
                    if (vi->statistics->steps->avgs)
                        memcpy(avgs, vi->statistics->steps->avgs, vi->nsteps * sizeof(double));
                    if (vi->statistics->steps->std_devs)
                        memcpy(std_devs, vi->statistics->steps->std_devs, vi->nsteps * sizeof(double));
                }
            }
        }
        common_read_free_varinfo(vi);
    }
    *err = adios_errno;
    if (*err < 0)
        fprintf(stderr, "Error: %s\n", adios_get_last_errmsg());
}

/* Specific function for each data type */
void FC_FUNC_(adios_get_statistics_int1, ADIOS_GET_STATISTICS_INT1) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }
void FC_FUNC_(adios_get_statistics_int2, ADIOS_GET_STATISTICS_INT2) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }
void FC_FUNC_(adios_get_statistics_int4, ADIOS_GET_STATISTICS_INT4) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }
void FC_FUNC_(adios_get_statistics_int8, ADIOS_GET_STATISTICS_INT8) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }
void FC_FUNC_(adios_get_statistics_real4, ADIOS_GET_STATISTICS_REAL4) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }
void FC_FUNC_(adios_get_statistics_real8, ADIOS_GET_STATISTICS_REAL8) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }
void FC_FUNC_(adios_get_statistics_complex8, ADIOS_GET_STATISTICS_COMPLEX8) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }
void FC_FUNC_(adios_get_statistics_complex16, ADIOS_GET_STATISTICS_COMPLEX16) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }
void FC_FUNC_(adios_get_statistics_char, ADIOS_GET_STATISTICS_CHAR) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }
void FC_FUNC_(adios_get_statistics_logical1, ADIOS_GET_STATISTICS_LOGICAL1) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }
void FC_FUNC_(adios_get_statistics_logical2, ADIOS_GET_STATISTICS_LOGICAL2) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }
void FC_FUNC_(adios_get_statistics_logical4, ADIOS_GET_STATISTICS_LOGICAL4) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }
void FC_FUNC_(adios_get_statistics_logical8, ADIOS_GET_STATISTICS_LOGICAL8) (int64_t * gp, char * varname, void * value, void * gmin, void * gmax, void * gavg, void * gstd_dev, void * mins, void * maxs, void * avgs, void * std_devs, int * err, int varname_len) { FC_FUNC_(adios_get_statistics, ADIOS_GET_STATISTICS) (gp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err, varname_len); }

void FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR)
        (int64_t * gp,
         char    * attrname,
         void    * attr,
         int     * err,
         int       attrname_len)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *gp;
    char *attrstr;
    void *data;
    int size;
    enum ADIOS_DATATYPES type;
    attrstr = futils_fstr_to_cstr(attrname, attrname_len);
    if (attrstr != NULL) {
        *err = common_read_get_attr (afp, attrstr, &type, &size, &data);
        if (data) {
            memcpy(attr, data, size);
            free(data);
        }
    } else {
        *err = adios_errno;
    }
    if (*err < 0)
        fprintf(stderr, "Error: %s\n", adios_get_last_errmsg());
}

void FC_FUNC_(adios_inq_attr, ADIOS_INQ_ATTR)
        (int64_t * gp,
         char    * attrname,
         int     * type,
         int     * size,
         int     * err,
         int       attrname_len)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *gp;
    char *attrstr;
    void *data;
    attrstr = futils_fstr_to_cstr(attrname, attrname_len);
    if (attrstr != NULL) {
        *err = common_read_get_attr (afp, attrstr, (enum ADIOS_DATATYPES *)type, size, &data);
        free(data);
    } else {
        *err = adios_errno;
    }
    if (*err < 0)
        fprintf(stderr, "Error: %s\n", adios_get_last_errmsg());
}

/* Specific function for each data type */
void FC_FUNC_(adios_get_attr_int1, ADIOS_GET_ATTR_INT1) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_int2, ADIOS_GET_ATTR_INT2) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_int4, ADIOS_GET_ATTR_INT4) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_int8, ADIOS_GET_ATTR_INT8) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_real4, ADIOS_GET_ATTR_REAL4) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_real8, ADIOS_GET_ATTR_REAL8) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_complex8, ADIOS_GET_ATTR_COMPLEX8) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_complex16, ADIOS_GET_ATTR_COMPLEX16) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_char, ADIOS_GET_ATTR_CHAR) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_logical1, ADIOS_GET_ATTR_LOGICAL1) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_logical2, ADIOS_GET_ATTR_LOGICAL2) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_logical4, ADIOS_GET_ATTR_LOGICAL4) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_logical8, ADIOS_GET_ATTR_LOGICAL8) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }

