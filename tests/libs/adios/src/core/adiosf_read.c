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
#include "core/bp_utils.h" // bp_get_type_size
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

extern int adios_errno;

#define PRINT_ERRMSG() fprintf(stderr, "ADIOS READ ERROR: %s\n", adios_get_last_errmsg())

#ifdef BUILD_WITH_CMAKE
  #include "FC.h"
#endif

/*********************/
/* FORTRAN INTERFACE */
/*********************/
void FC_FUNC_(adios_errmsg, adios_errmsg) (char *msg, int msg_len)
{
    futils_cstr_to_fstr( adios_get_last_errmsg(), (char *)msg, msg_len);
}

void FC_FUNC_(adios_read_init_method, ADIOS_READ_INIT_METHOD) (
        int *fmethod, int * fcomm, char * parameters, int * err, int parameters_len)
{
    MPI_Comm comm = MPI_Comm_f2c (*((int *) fcomm));
    enum ADIOS_READ_METHOD method = (enum ADIOS_READ_METHOD) *fmethod;
    char *paramstr;
    futils_called_from_fortran_set();

    paramstr = futils_fstr_to_cstr(parameters, parameters_len);
    *err = common_read_init_method (method, comm, paramstr);
    free(paramstr);
    if (*err)
        PRINT_ERRMSG();
}

void FC_FUNC_(adios_read_finalize_method, ADIOS_READ_FINALIZE_METHOD) (int *fmethod, int * err)
{
    enum ADIOS_READ_METHOD method = (enum ADIOS_READ_METHOD) *fmethod;
    *err = common_read_finalize_method (method);
    futils_called_from_fortran_unset();
    if (*err)
        PRINT_ERRMSG();
}


void FC_FUNC_(adios_read_open, ADIOS_READ_OPEN)
        (int64_t * fp,
         char    * fname,
         int     * fmethod,
         int     * fcomm,
         int     * lockmode,
         float   * timeout_sec,
         int     * err,
         int       fname_len)
{
    ADIOS_FILE *afp;
    char *namestr;
    enum ADIOS_READ_METHOD method = (enum ADIOS_READ_METHOD) *fmethod;
    enum ADIOS_LOCKMODE lockm = (enum ADIOS_LOCKMODE) *lockmode;
    MPI_Comm comm = MPI_Comm_f2c (*((int *) fcomm));
    futils_called_from_fortran_set();

    namestr = futils_fstr_to_cstr(fname, fname_len);
    if (namestr != NULL) {
        afp = common_read_open (namestr, method, comm, lockm, *timeout_sec);
        *fp = (int64_t) afp;
        free(namestr);
    } else {
        *fp = (int64_t) NULL;
    }
    *err = adios_errno;
    if (*err)
        PRINT_ERRMSG();
}


void FC_FUNC_(adios_read_open_file, ADIOS_READ_OPEN_FILE)
        (int64_t * fp,
         char    * fname,
         int     * fmethod,
         int     * fcomm,
         int     * err,
         int       fname_len)
{
    ADIOS_FILE *afp;
    char *namestr;
    enum ADIOS_READ_METHOD method = (enum ADIOS_READ_METHOD) *fmethod;
    MPI_Comm comm = MPI_Comm_f2c (*((int *) fcomm));
    futils_called_from_fortran_set();

    namestr = futils_fstr_to_cstr(fname, fname_len);
    if (namestr != NULL) {
        afp = common_read_open_file (namestr, method, comm);
        *fp = (int64_t) afp;
        free(namestr);
    } else {
        *fp = (int64_t) NULL;
    }
    *err = adios_errno;
    if (*err)
        PRINT_ERRMSG();
}


int FC_FUNC_(adios_advance_step, ADIOS_ADVANCE_STEP) 
        (int64_t *fp, int *last, float *timeout_sec, int *err)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    *err = common_read_advance_step (afp, *last, *timeout_sec);
    if (*err)
        PRINT_ERRMSG();
    return *err;
}


void FC_FUNC_(adios_release_step, ADIOS_RELEASE_STEP) (int64_t *fp, int *err)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    common_read_release_step (afp);
    *err = adios_errno;
    if (*err)
        PRINT_ERRMSG();
}


void FC_FUNC_(adios_reset_dimension_order, ADIOS_RESET_DIMENSION_ORDER)
        (int64_t * fp, int * flag)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    common_read_reset_dimension_order(afp, *flag);
    if (adios_errno)
        PRINT_ERRMSG();
}

void FC_FUNC_(adios_read_close, ADIOS_READ_CLOSE) (int64_t * fp, int * err)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    *err = common_read_close (afp);
    futils_called_from_fortran_unset();
    if (*err)
        PRINT_ERRMSG();
}

void FC_FUNC_(adios_inq_ngroups, ADIOS_INQ_NGROUPS) 
        (int64_t * fp, int * groups_count, int * err)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    if (afp != NULL) {
        char ** group_namelist;
        *groups_count = common_read_get_grouplist (afp, &group_namelist);
        *err = adios_errno;
        if (*err)
            PRINT_ERRMSG();
    } else {
        *groups_count = 0;
        *err = 1;
    }
}

void FC_FUNC_(adios_inq_groupnames, ADIOS_INQ_GROUPNAMES)
        (int64_t * fp, void * gnamelist, int * err, int gnamelist_len)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    adios_errno = err_no_error;
    if (afp != NULL) {
        char ** group_namelist;
        int ngroups = common_read_get_grouplist (afp, &group_namelist);
        int i;
        for (i=0;i<ngroups;i++) {
            futils_cstr_to_fstr( group_namelist[i], (char *)gnamelist+i*gnamelist_len, gnamelist_len);
        }
        *err = adios_errno;
        if (*err)
            PRINT_ERRMSG();
    } else {
        *err = 1;
    }
}

void FC_FUNC_(adios_group_view, ADIOS_GROUP_VIEW) 
        (int64_t * fp, int * groupid, int * err)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    if (afp != NULL) {
        *err = common_read_group_view (afp, *groupid);
        if (*err)
            PRINT_ERRMSG();
    } else {
        *err = 1;
    }
}

void FC_FUNC_(adios_inq_file, ADIOS_INQ_FILE) 
        (int64_t * fp,
         int     * vars_count,
         int     * attrs_count,
         int     * current_step,
         int     * last_step,
         int     * err)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    if (afp != NULL) {
        *vars_count = afp->nvars;
        *attrs_count = afp->nattrs;
        *current_step = afp->current_step;
        *last_step = afp->last_step;
        *err = 0;
    } else {
        *vars_count = 0;
        *attrs_count = 0;
        *current_step = 0;
        *last_step = 0;
        *err = 1;
    }
}

void FC_FUNC_(adios_inq_varnames, ADIOS_INQ_VARNAMES)
        (int64_t * fp, void * vnamelist, int * err, int vnamelist_len)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    int i;
    if (afp != NULL) {
        for (i=0;i<afp->nvars;i++) {
            futils_cstr_to_fstr( afp->var_namelist[i], (char *)vnamelist+i*vnamelist_len, vnamelist_len);
        }
        *err = 0;
    } else {
        *err = 1;
    }
}

void FC_FUNC_(adios_inq_attrnames, ADIOS_INQ_ATTRNAMES)
        (int64_t * fp, void * anamelist, int * err, int anamelist_len)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    int i;
    if (afp != NULL) {
        for (i=0;i<afp->nattrs;i++) {
            futils_cstr_to_fstr( afp->attr_namelist[i], (char *)anamelist+i*anamelist_len, anamelist_len);
        }
        *err = 0;
    } else {
        *err = 1;
    }
}

void FC_FUNC_(adios_inq_var, ADIOS_INQ_VAR) 
        (int64_t  * fp, 
         char     * varname,
         int      * type,
         int      * nsteps,
         int      * ndim,
         uint64_t * dims,
         int      * err,
         int        varname_len)
{
    char *varstr;
    int  i;
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    ADIOS_VARINFO *vi = NULL;

    varstr = futils_fstr_to_cstr(varname, varname_len);
    if (varstr != NULL) {
        vi = common_read_inq_var (afp, varstr);
        free(varstr);
    }
    if (vi != NULL) {
        *type = vi->type;
        *nsteps = vi->nsteps;
        *ndim = vi->ndim;
        for (i=0;i<vi->ndim;i++)
            dims[i] = vi->dims[i];
        common_read_free_varinfo(vi);
    } else {
        *type = adios_unknown;
        *ndim = 0;
        *nsteps = 0;
    }
    *err = adios_errno;
    if (*err < 0)
        PRINT_ERRMSG();
}

void FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) 
        (int64_t  * fp,
         char     * varname,
         void     * data,
         int      * err,
         int varname_len)
{
    char *varstr;
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    ADIOS_VARINFO *vi = NULL;

    varstr = futils_fstr_to_cstr(varname, varname_len);
    if (varstr != NULL) {
        vi = common_read_inq_var (afp, varstr);
        free(varstr);
    }
    if (vi != NULL && vi->value != NULL) {
        int size = bp_get_type_size(vi->type, vi->value);
        memcpy(data, vi->value, size);
        common_read_free_varinfo(vi);
    } else {
        *err = adios_errno;
    }
    *err = adios_errno;
    if (*err < 0)
        PRINT_ERRMSG();
}

void FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) 
        (int64_t  * fp,
         int64_t  * fsel,
         char     * varname,
         int      * from_step,
         int      * nsteps,
         void     * data,
         int      * err,
         int varname_len)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    ADIOS_SELECTION * sel = (ADIOS_SELECTION *) *fsel;
    char *varstr;
    varstr = futils_fstr_to_cstr(varname, varname_len);
    if (varstr != NULL) {
        *err = common_read_schedule_read (afp, sel, varstr, *from_step, *nsteps, NULL /* NCSU ALACRITY-ADIOS */, data);
        free(varstr);
    } else {
        *err = adios_errno;
    }
    if (*err < 0)
        PRINT_ERRMSG();
}

void FC_FUNC_(adios_schedule_read_f2c, ADIOS_SCHEDULE_READ_F2C) (int64_t * fp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (fp,fsel,varname,from_step,nsteps,data,err,varname_len); }

void FC_FUNC_(adios_perform_reads, ADIOS_PERFORM_READS) 
        (int64_t * fp,
         int     * err)
{
    ADIOS_FILE *afp = (ADIOS_FILE *) *fp;
    *err = common_read_perform_reads (afp, 1); // only blocking read implemented for Fortran
    if (*err < 0)
        PRINT_ERRMSG();
}

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
        vi = common_read_inq_var (afp, varstr);
        free(varstr);
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
        PRINT_ERRMSG();
}

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
        free(attrstr);
    } else {
        *err = adios_errno;
    }
    if (*err < 0)
        PRINT_ERRMSG();
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
        free(attrstr);
    } else {
        *err = adios_errno;
    }
    if (*err < 0)
        PRINT_ERRMSG();
}

void FC_FUNC_(adios_selection_boundingbox, ADIOS_SELECTION_BOUNDINGBOX) 
           (int64_t * fsel, int *ndim, uint64_t *start, uint64_t *count)
{   
    ADIOS_SELECTION * sel = common_read_selection_boundingbox (*ndim, start, count);
    *fsel = (int64_t) sel;
}

void FC_FUNC_(adios_selection_points, ADIOS_SELECTION_POINTS) 
            (int64_t *fsel, int *ndim, uint64_t *npoints, uint64_t *points)
{
    ADIOS_SELECTION * sel = common_read_selection_points (*ndim, *npoints, points);
    *fsel = (int64_t) sel;
}

void FC_FUNC_(adios_selection_writeblock, ADIOS_SELECTION_WRITEBLOCK) (int64_t *fsel, int *index)
{
    ADIOS_SELECTION * sel = common_read_selection_writeblock (*index);
    *fsel = (int64_t) sel;
}

void FC_FUNC_(adios_selection_auto, ADIOS_SELECTION_AUTO) (int64_t *fsel, char *hints, int hints_len)
{
    char *hintstr = futils_fstr_to_cstr(hints, hints_len);
    ADIOS_SELECTION * sel = common_read_selection_auto (hintstr);
    *fsel = (int64_t) sel;
    free (hintstr);
}

void FC_FUNC_(adios_selection_delete, ADIOS_SELECTION_AUTO) (int64_t *fsel)
{
    ADIOS_SELECTION * sel = (ADIOS_SELECTION *) *fsel;
    common_read_selection_delete (sel);
    *fsel = 0;
}


/**************************************************************************/
/*                  Specific function for each data type                  */
/**************************************************************************/

/* ADIOS_GET_SCALAR */
void FC_FUNC_(adios_get_scalar_int1, ADIOS_GET_SCALAR_INT1)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }
void FC_FUNC_(adios_get_scalar_int2, ADIOS_GET_SCALAR_INT2)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }
void FC_FUNC_(adios_get_scalar_int4, ADIOS_GET_SCALAR_INT4)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }
void FC_FUNC_(adios_get_scalar_int8, ADIOS_GET_SCALAR_INT8)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }
void FC_FUNC_(adios_get_scalar_real4, ADIOS_GET_SCALAR_REAL4)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }
void FC_FUNC_(adios_get_scalar_real8, ADIOS_GET_SCALAR_REAL8)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }
void FC_FUNC_(adios_get_scalar_char, ADIOS_GET_SCALAR_CHAR)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }
void FC_FUNC_(adios_get_scalar_complex8, ADIOS_GET_SCALAR_COMPLEX8)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }
void FC_FUNC_(adios_get_scalar_complex16, ADIOS_GET_SCALAR_COMPLEX16)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }
void FC_FUNC_(adios_get_scalar_logical1, ADIOS_GET_SCALAR_LOGICAL1)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }
void FC_FUNC_(adios_get_scalar_logical2, ADIOS_GET_SCALAR_LOGICAL2)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }
void FC_FUNC_(adios_get_scalar_logical4, ADIOS_GET_SCALAR_LOGICAL4)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }
void FC_FUNC_(adios_get_scalar_logical8, ADIOS_GET_SCALAR_LOGICAL8)  (int64_t * fp, char * varname, void * data, int * err, int varname_len) { FC_FUNC_(adios_get_scalar, ADIOS_GET_SCALAR) (fp, varname, data, err, varname_len); }

/* ADIOS_SCHEDULE_READ */
/* scalars */
#if 0
void FC_FUNC_(adios_schedule_read_int1_d0, ADIOS_SCHEDULE_READ_INT1_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int2_d0, ADIOS_SCHEDULE_READ_INT2_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int4_d0, ADIOS_SCHEDULE_READ_INT4_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int8_d0, ADIOS_SCHEDULE_READ_INT8_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real4_d0, ADIOS_SCHEDULE_READ_REAL4_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real8_d0, ADIOS_SCHEDULE_READ_REAL8_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
//void FC_FUNC_(adios_schedule_read_char_d0, ADIOS_SCHEDULE_READ_CHAR_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex8_d0, ADIOS_SCHEDULE_READ_COMPLEX8_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex16_d0, ADIOS_SCHEDULE_READ_COMPLEX16_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical1_d0, ADIOS_SCHEDULE_READ_LOGICAL1_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical2_d0, ADIOS_SCHEDULE_READ_LOGICAL2_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical4_d0, ADIOS_SCHEDULE_READ_LOGICAL4_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical8_d0, ADIOS_SCHEDULE_READ_LOGICAL8_D0) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }

/* 1D */
void FC_FUNC_(adios_schedule_read_int1_d1, ADIOS_SCHEDULE_READ_INT1_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int2_d1, ADIOS_SCHEDULE_READ_INT2_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int4_d1, ADIOS_SCHEDULE_READ_INT4_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int8_d1, ADIOS_SCHEDULE_READ_INT8_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real4_d1, ADIOS_SCHEDULE_READ_REAL4_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real8_d1, ADIOS_SCHEDULE_READ_REAL8_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_char_d1, ADIOS_SCHEDULE_READ_CHAR_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex8_d1, ADIOS_SCHEDULE_READ_COMPLEX8_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex16_d1, ADIOS_SCHEDULE_READ_COMPLEX16_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical1_d1, ADIOS_SCHEDULE_READ_LOGICAL1_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical2_d1, ADIOS_SCHEDULE_READ_LOGICAL2_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical4_d1, ADIOS_SCHEDULE_READ_LOGICAL4_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical8_d1, ADIOS_SCHEDULE_READ_LOGICAL8_D1) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }

/* 2D */
void FC_FUNC_(adios_schedule_read_int1_d2, ADIOS_SCHEDULE_READ_INT1_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int2_d2, ADIOS_SCHEDULE_READ_INT2_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int4_d2, ADIOS_SCHEDULE_READ_INT4_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int8_d2, ADIOS_SCHEDULE_READ_INT8_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real4_d2, ADIOS_SCHEDULE_READ_REAL4_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real8_d2, ADIOS_SCHEDULE_READ_REAL8_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_char_d2, ADIOS_SCHEDULE_READ_CHAR_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex8_d2, ADIOS_SCHEDULE_READ_COMPLEX8_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex16_d2, ADIOS_SCHEDULE_READ_COMPLEX16_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical1_d2, ADIOS_SCHEDULE_READ_LOGICAL1_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical2_d2, ADIOS_SCHEDULE_READ_LOGICAL2_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical4_d2, ADIOS_SCHEDULE_READ_LOGICAL4_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical8_d2, ADIOS_SCHEDULE_READ_LOGICAL8_D2) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }

/* 3D */
void FC_FUNC_(adios_schedule_read_int1_d3, ADIOS_SCHEDULE_READ_INT1_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int2_d3, ADIOS_SCHEDULE_READ_INT2_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int4_d3, ADIOS_SCHEDULE_READ_INT4_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int8_d3, ADIOS_SCHEDULE_READ_INT8_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real4_d3, ADIOS_SCHEDULE_READ_REAL4_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real8_d3, ADIOS_SCHEDULE_READ_REAL8_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_char_d3, ADIOS_SCHEDULE_READ_CHAR_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex8_d3, ADIOS_SCHEDULE_READ_COMPLEX8_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex16_d3, ADIOS_SCHEDULE_READ_COMPLEX16_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical1_d3, ADIOS_SCHEDULE_READ_LOGICAL1_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical2_d3, ADIOS_SCHEDULE_READ_LOGICAL2_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical4_d3, ADIOS_SCHEDULE_READ_LOGICAL4_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical8_d3, ADIOS_SCHEDULE_READ_LOGICAL8_D3) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }

/* 4D */
void FC_FUNC_(adios_schedule_read_int1_d4, ADIOS_SCHEDULE_READ_INT1_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int2_d4, ADIOS_SCHEDULE_READ_INT2_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int4_d4, ADIOS_SCHEDULE_READ_INT4_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int8_d4, ADIOS_SCHEDULE_READ_INT8_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real4_d4, ADIOS_SCHEDULE_READ_REAL4_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real8_d4, ADIOS_SCHEDULE_READ_REAL8_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_char_d4, ADIOS_SCHEDULE_READ_CHAR_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex8_d4, ADIOS_SCHEDULE_READ_COMPLEX8_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex16_d4, ADIOS_SCHEDULE_READ_COMPLEX16_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical1_d4, ADIOS_SCHEDULE_READ_LOGICAL1_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical2_d4, ADIOS_SCHEDULE_READ_LOGICAL2_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical4_d4, ADIOS_SCHEDULE_READ_LOGICAL4_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical8_d4, ADIOS_SCHEDULE_READ_LOGICAL8_D4) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }

/* 5D */
void FC_FUNC_(adios_schedule_read_int1_d5, ADIOS_SCHEDULE_READ_INT1_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int2_d5, ADIOS_SCHEDULE_READ_INT2_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int4_d5, ADIOS_SCHEDULE_READ_INT4_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int8_d5, ADIOS_SCHEDULE_READ_INT8_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real4_d5, ADIOS_SCHEDULE_READ_REAL4_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real8_d5, ADIOS_SCHEDULE_READ_REAL8_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_char_d5, ADIOS_SCHEDULE_READ_CHAR_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex8_d5, ADIOS_SCHEDULE_READ_COMPLEX8_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex16_d5, ADIOS_SCHEDULE_READ_COMPLEX16_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical1_d5, ADIOS_SCHEDULE_READ_LOGICAL1_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical2_d5, ADIOS_SCHEDULE_READ_LOGICAL2_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical4_d5, ADIOS_SCHEDULE_READ_LOGICAL4_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical8_d5, ADIOS_SCHEDULE_READ_LOGICAL8_D5) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }

/* 6D */
void FC_FUNC_(adios_schedule_read_int1_d6, ADIOS_SCHEDULE_READ_INT1_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int2_d6, ADIOS_SCHEDULE_READ_INT2_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int4_d6, ADIOS_SCHEDULE_READ_INT4_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_int8_d6, ADIOS_SCHEDULE_READ_INT8_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real4_d6, ADIOS_SCHEDULE_READ_REAL4_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_real8_d6, ADIOS_SCHEDULE_READ_REAL8_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_char_d6, ADIOS_SCHEDULE_READ_CHAR_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex8_d6, ADIOS_SCHEDULE_READ_COMPLEX8_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_complex16_d6, ADIOS_SCHEDULE_READ_COMPLEX16_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical1_d6, ADIOS_SCHEDULE_READ_LOGICAL1_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical2_d6, ADIOS_SCHEDULE_READ_LOGICAL2_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical4_d6, ADIOS_SCHEDULE_READ_LOGICAL4_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
void FC_FUNC_(adios_schedule_read_logical8_d6, ADIOS_SCHEDULE_READ_LOGICAL8_D6) (int64_t * gp, int64_t * fsel, char * varname, int * from_step, int * nsteps, void * data, int * err, int varname_len) { FC_FUNC_(adios_schedule_read, ADIOS_SCHEDULE_READ) (gp, fsel, varname, from_step, nsteps, data, err, varname_len); }
#endif

/* ADIOS_GET_STATISTICS */
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


/* ADIOS_GET_ATTR */
void FC_FUNC_(adios_get_attr_int1, ADIOS_GET_ATTR_INT1) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_int2, ADIOS_GET_ATTR_INT2) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_int4, ADIOS_GET_ATTR_INT4) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_int8, ADIOS_GET_ATTR_INT8) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_real4, ADIOS_GET_ATTR_REAL4) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_real8, ADIOS_GET_ATTR_REAL8) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_complex8, ADIOS_GET_ATTR_COMPLEX8) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_complex16, ADIOS_GET_ATTR_COMPLEX16) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_char, ADIOS_GET_ATTR_CHAR) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len, int attr_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_logical1, ADIOS_GET_ATTR_LOGICAL1) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_logical2, ADIOS_GET_ATTR_LOGICAL2) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_logical4, ADIOS_GET_ATTR_LOGICAL4) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }
void FC_FUNC_(adios_get_attr_logical8, ADIOS_GET_ATTR_LOGICAL8) (int64_t * gp, char * attrname, void * attr, int * err, int attrname_len) { FC_FUNC_(adios_get_attr, ADIOS_GET_ATTR) (gp, attrname, attr, err, attrname_len); }


