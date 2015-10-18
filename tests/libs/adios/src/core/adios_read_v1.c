/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adios_read_v1.h"

#undef ADIOS_FILE
#undef ADIOS_GROUP
#undef ADIOS_VARINFO
#undef ADIOS_HIST
#undef ADIOS_VARBLOCK

#undef ADIOS_READ_METHOD
#undef ADIOS_READ_METHOD_BP
#undef ADIOS_READ_METHOD_BP_STAGED
#undef ADIOS_READ_METHOD_BP_AGGREGATE
#undef ADIOS_READ_METHOD_HDF5
#undef ADIOS_READ_METHOD_DART
#undef ADIOS_READ_METHOD_DIMES
#undef ADIOS_READ_METHOD_NSSI
#undef ADIOS_READ_METHOD_DATATAP
#undef ADIOS_READ_METHOD_BP_STAGED1

#undef adios_set_read_method
#undef adios_fopen
#undef adios_fclose
#undef adios_reset_dimension_order
#undef adios_gopen
#undef adios_gopen_byid
#undef adios_gclose
#undef adios_inq_var
#undef adios_inq_var_byid
#undef adios_stat_cor
#undef adios_stat_cov
#undef adios_free_varinfo
#undef adios_read_var
#undef adios_read_var_byid
#undef adios_read_local_var
#undef adios_get_attr
#undef adios_get_attr_byid
#undef adios_type_to_string
#undef adios_type_size
#undef adios_print_groupinfo
#undef adios_print_fileinfo


#include "core/common_read.h"


#include "adios_error.h"
#include "core/bp_utils.h"
#define BYTE_ALIGN 8

static enum ADIOS_READ_METHOD lastmethod = ADIOS_READ_METHOD_BP;

/*
const char *adios_errmsg ()
{
    return adios_get_last_errmsg();
}
*/


int adios_set_read_method_v1 (enum ADIOS_READ_METHOD_V1 method)
{
    switch (method) {
        case ADIOS_READ_METHOD_BP_V1:
            lastmethod = ADIOS_READ_METHOD_BP;
            break;
        case ADIOS_READ_METHOD_BP_STAGED_V1:
            lastmethod = ADIOS_READ_METHOD_BP_AGGREGATE;
            break;
        case ADIOS_READ_METHOD_BP_STAGED1_V1:
            lastmethod = ADIOS_READ_METHOD_BP_AGGREGATE;
            break;
        case ADIOS_READ_METHOD_DART_V1:
            lastmethod = ADIOS_READ_METHOD_DATASPACES;
            break;
        case ADIOS_READ_METHOD_DIMES_V1:
            lastmethod = ADIOS_READ_METHOD_DIMES;
            break;

        /* Unsupported methods */
        case ADIOS_READ_METHOD_HDF5_V1:
        case ADIOS_READ_METHOD_NSSI_V1:
        case ADIOS_READ_METHOD_DATATAP_V1:
        default:
            lastmethod = ADIOS_READ_METHOD_BP;
            break;
    }
    return 0;
} 


int adios_read_init_v1(MPI_Comm comm)
{
    return common_read_init_method (lastmethod, comm, NULL);
}


int adios_read_finalize_v1()
{
    return common_read_finalize_method (lastmethod);
}

ADIOS_FILE_V1 * adios_fopen_v1 (const char * fname, MPI_Comm comm)
{
    ADIOS_FILE_V1 *fp1 = NULL;
    ADIOS_FILE *fp = common_read_open_file (fname, lastmethod, comm);
    if (fp) {
        fp1 = (ADIOS_FILE_V1 *) malloc (sizeof(ADIOS_FILE_V1));
        if (fp1) {
            fp1->fh          = fp->fh;
            fp1->vars_count  = fp->nvars;  
            fp1->attrs_count = fp->nattrs;
            fp1->tidx_start  = 1;
            fp1->ntimesteps  = fp->last_step;
            fp1->version     = fp->version;  
            fp1->file_size   = fp->file_size;
            fp1->endianness  = fp->endianness; 

            /* FIX: no groups in new version, extra function should support this */
            fp1->groups_count = common_read_get_grouplist(fp, &fp1->group_namelist);

            fp1->internal_data = fp;
        } else {
            adios_error ( err_no_memory, "Cannot allocate memory for file info.\n");
        }
    }
    return fp1;
}

int adios_fclose_v1 (ADIOS_FILE_V1 *fp) 
{
    return common_read_close ((ADIOS_FILE*)fp->internal_data);
}

ADIOS_GROUP_V1 * adios_gopen_v1 (ADIOS_FILE_V1 *fp, const char * grpname)
{
    int grpid;
    adios_errno = 0;
    for (grpid=0;grpid<(fp->groups_count);grpid++) {
        if (!strcmp(fp->group_namelist[grpid], grpname))
            break;
    }
    if (grpid >= fp->groups_count) {
        adios_error ( err_invalid_group, "Invalid group name %s\n", grpname);
        return NULL;
    }
    return adios_gopen_byid_v1 (fp, grpid);
}

ADIOS_GROUP_V1 * adios_gopen_byid_v1 (ADIOS_FILE_V1 *fp, int grpid)
{
    ADIOS_GROUP_V1 * gp = NULL;

    adios_errno = 0;
    if (grpid < 0 || grpid >= fp->groups_count) {
        adios_error ( err_invalid_group, "Invalid group index %d\n", grpid);
        return NULL;
    }

    gp = (ADIOS_GROUP_V1 *) malloc(sizeof(ADIOS_GROUP_V1));
    if (!gp) {
        adios_error ( err_no_memory, "Could not allocate memory for group info\n");
        return NULL;
    }

    ADIOS_FILE *f = (ADIOS_FILE*)fp->internal_data;
    common_read_group_view (f, grpid);

    gp->grpid = grpid;
    gp->gh = (uint64_t) f;
    gp->fp = fp;
    gp->vars_count = f->nvars;
    gp->attrs_count = f->nattrs;
    gp->var_namelist = f->var_namelist;
    gp->attr_namelist = f->attr_namelist;

    return gp;
}

int adios_gclose_v1 (ADIOS_GROUP_V1 *gp)
{
    ADIOS_FILE *f = (ADIOS_FILE*) gp->gh;
    //free_namelist ((gp->var_namelist),gp->vars_count);
    //free_namelist ((gp->attr_namelist),gp->attrs_count);
    gp->var_namelist = NULL;
    gp->attr_namelist = NULL;
    gp->vars_count = 0;
    gp->attrs_count = 0;
    free (gp);
    common_read_group_view (f, -1);
    return 0;
}


int adios_get_attr_v1 (ADIOS_GROUP_V1  * gp, const char * attrname, enum ADIOS_DATATYPES * type,
                    int * size, void ** data)
{
    return common_read_get_attr ((ADIOS_FILE*)gp->fp->internal_data, attrname, type, size, data);
}

int adios_get_attr_byid_v1 (ADIOS_GROUP_V1  * gp, int attrid, 
                    enum ADIOS_DATATYPES * type, int * size, void ** data)
{
    return common_read_get_attr_byid ((ADIOS_FILE*)gp->fp->internal_data, attrid, type, size, data);
}

static ADIOS_VARINFO_V1 * adios_varinfo_to_v1 (ADIOS_GROUP_V1 *gp, ADIOS_VARINFO *vi, int getstat) 
{
    ADIOS_VARINFO_V1 * v = 0;
    if (vi) {
        v = (ADIOS_VARINFO_V1 *) malloc (sizeof(ADIOS_VARINFO_V1));
        
        v->grpid = 0;
        v->varid = vi->varid;
        v->type = vi->type;
        v->value = vi->value; 
             
        /* TIME dimension should be emulated here !!! */
        if (vi->nsteps > 1) {
            v->ndim = vi->ndim + 1;
            v->timedim = 0;
        } else {
            v->ndim = vi->ndim;
            v->timedim = -1;
        }
        int tidx = 0, i;
        v->dims = (uint64_t *) malloc (sizeof(uint64_t) * (v->ndim));
        if (vi->nsteps > 1) {
            v->dims[0] = vi->nsteps;
            tidx=1;
        }
        for (i=0; i<vi->ndim; i++)
            v->dims[i+tidx] = vi->dims[i];


        ADIOS_VARSTAT * stat = NULL;
        if (getstat) {
            common_read_inq_var_stat ((ADIOS_FILE *)gp->fp->internal_data, 
                                       vi, 1, 0);
            stat = vi->statistics;
        }

        if (stat) {
            v->characteristics_count=0;// = stat->characteristics_count; FIXME
            v->gmin = stat->min; 
            v->gmax = stat->max; 
            v->gavg = stat->avg; 
            v->gstd_dev = stat->std_dev;
            if (stat->histogram) {
                v->mins = stat->steps->mins;
                v->maxs = stat->steps->maxs;
                v->avgs = stat->steps->avgs;
                v->std_devs = stat->steps->std_devs;
            } else {
                v->mins = 0;
                v->maxs = 0;
                v->avgs = 0;
                v->std_devs = 0;
            }
            if (stat->histogram) {
                v->hist = (struct ADIOS_HIST_V1*) malloc (sizeof(struct ADIOS_HIST));
                if (v->hist) {
                    v->hist->num_breaks = stat->histogram->num_breaks;
                    v->hist->max = stat->histogram->max;
                    v->hist->min = stat->histogram->min;
                    v->hist->breaks = stat->histogram->breaks;
                    v->hist->frequenciess = stat->histogram->frequencies;
                    v->hist->gfrequencies = stat->histogram->gfrequencies;
                }
            } else {
                v->hist=0;
            }
        }

        v->internal_data = (void *)vi;
    }
    return v;
}

ADIOS_VARINFO_V1 * adios_inq_var_v1 (ADIOS_GROUP_V1  *gp, const char * varname) 
{
    ADIOS_VARINFO * vi = common_read_inq_var ((ADIOS_FILE*)gp->fp->internal_data, varname);
    return adios_varinfo_to_v1 (gp, vi, 1);
}

ADIOS_VARINFO_V1 * adios_inq_var_byid_v1 (ADIOS_GROUP_V1  *gp, int varid)
{
    ADIOS_VARINFO * vi = common_read_inq_var_byid ((ADIOS_FILE*)gp->fp->internal_data, varid);
    return  adios_varinfo_to_v1 (gp, vi, 1);
}

void adios_free_varinfo_v1 (ADIOS_VARINFO_V1 *vp)
{
    /* Should free up ADIOS_VARINFO_V1 and ADIOS_VARINFO too!!! */
    common_read_free_varinfo ((ADIOS_VARINFO*)vp->internal_data);
    free(vp);
}

int64_t adios_read_var_v1 (ADIOS_GROUP_V1  * gp, const char * varname,
                        const uint64_t * start, const uint64_t * count,
                        void * data)
{
    ADIOS_FILE *f = (ADIOS_FILE*)gp->fp->internal_data;

    /* variable name -> varid */
    ADIOS_VARINFO * vi = common_read_inq_var (f, varname);
    if (!vi) 
        return adios_errno;

    return adios_read_var_byid_v1 (gp, vi->varid, start, count, data);

}

int64_t adios_read_var_byid_v1 (ADIOS_GROUP_V1 * gp,
                             int                 varid,
                             const uint64_t    * start,
                             const uint64_t    * count,
                             void              * data)
{
    ADIOS_FILE *f = (ADIOS_FILE*)gp->fp->internal_data;

    /* First get the number of dimensions of the variable */
    ADIOS_VARINFO * vi = common_read_inq_var_byid (f, varid);
    if (!vi) 
        return adios_errno;
    ADIOS_VARINFO_V1 * v = adios_varinfo_to_v1 (gp, vi, 0); // fixes time

    int ndim = v->ndim;
    int tidx = 0;
    int from_step = 0;
    int nsteps = 1;
    if (v->timedim == 0) {
        /* has time dimension, translate to new query */
        ndim--;
        tidx = 1;
        from_step = (int) start[0];
        nsteps    = (int) count[0];
    }

    ADIOS_SELECTION * sel = adios_selection_boundingbox (ndim, start+tidx, count+tidx);

    common_read_schedule_read_byid (f, sel, varid, from_step, nsteps, NULL, data); // NCSU ALACRITY-ADIOS
    int ret = common_read_perform_reads (f, 1);
    int64_t rbytes;
    if (ret == err_no_error) {
        /* should return the number of bytes read */
        rbytes = (int64_t) common_read_type_size (vi->type, data);
        int i;
        for (i=0; i<v->ndim; i++)
            rbytes *= (int64_t) count[i];
    } else {
        rbytes = (int64_t) adios_errno;
    }

    adios_free_varinfo_v1(v);
    adios_selection_delete(sel);
    return rbytes;
}

int64_t adios_read_local_var_v1 (ADIOS_GROUP_V1 * gp,
                              const char        * varname,
                              int                 idx,
                              const uint64_t    * start,
                              const uint64_t    * count,
                              void              * data)
{
    ADIOS_FILE *f = (ADIOS_FILE*)gp->fp->internal_data;
    int i;

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
    ADIOS_VARINFO * vi = common_read_inq_var (f, varname);
    if (!vi) 
        return adios_errno;

    /* get step from the idx, and check validity of idx */
    int step = 0;
    int idx_in_step = idx;
    step = 0;
    while (step < vi->nsteps && idx_in_step >= vi->nblocks[step]) {
        idx_in_step -= vi->nblocks[step];
        step++;
    }
    if (step == vi->nsteps) {
        adios_error (err_out_of_bound, "ADIOS ERROR: local "
                "variable %s has only %d blocks in file. "
                "Requested index %d\n", varname, vi->sum_nblocks, idx);
        return adios_errno;
    }
    common_read_inq_var_blockinfo (f, vi); // get info on each block

    /* check dimensions */
    for (i=0; i<vi->ndim; i++) {
        if (start[i] != 0 || count[i] != vi->blockinfo[idx].count[i]) {
            adios_error (err_out_of_bound, "ADIOS ERROR: when reading a local "
                "variable, only the whole block can be requested; subselections "
                "are not allowed. Variable %s, block %d, dimension %d size is %lld, "
                "requested %lld from offset %lld\n", 
                varname, idx, vi->blockinfo[idx].count[i], count[i], start[i]);
            return adios_errno;
        }
    }

    ADIOS_SELECTION * sel = adios_selection_writeblock (idx_in_step);

    common_read_schedule_read_byid (f, sel, vi->varid, step, 1, NULL, data); // NCSU ALACRITY-ADIOS
    int ret = common_read_perform_reads (f, 1);
    int64_t rbytes;
    if (ret == err_no_error) {
        /* should return the number of bytes read */
        rbytes = (int64_t) common_read_type_size (vi->type, data);
        for (i=0; i<vi->ndim; i++)
            rbytes *= (int64_t) count[i];
    } else {
        rbytes = (int64_t) adios_errno;
    }

    common_read_free_varinfo(vi);
    adios_selection_delete(sel);
    return rbytes;
}

const char * adios_type_to_string_v1 (enum ADIOS_DATATYPES type)
{
    return common_read_type_to_string (type);
}

int adios_type_size_v1(enum ADIOS_DATATYPES type, void *data)
{
    return common_read_type_size(type, data);
}


void adios_print_groupinfo (ADIOS_GROUP_V1 *gp) 
{
    ADIOS_FILE *f = (ADIOS_FILE*)gp->fp->internal_data;
    common_read_print_fileinfo(f);
}


void adios_print_fileinfo_v1 (ADIOS_FILE_V1 *fp) 
{
    ADIOS_FILE *f = (ADIOS_FILE*)fp->internal_data;
    common_read_print_fileinfo(f);
}


#include <math.h>

// NCSU - Timer series analysis, correlation
double adios_stat_cor_v1 (ADIOS_VARINFO_V1 * vix, ADIOS_VARINFO_V1 * viy, char * characteristic, uint32_t time_start, uint32_t time_end, uint32_t lag)
{
    int i;

    double avg_x = 0.0, avg_y = 0.0, avg_lag = 0.0;
    double var_x = 0.0, var_y = 0.0, var_lag = 0.0;
    double cov = 0;

    if (vix == NULL)
    {
        adios_error(err_invalid_argument, "Variable not defined\n");
        return 0;
    }

    // If the vix and viy are not time series objects, return.
    if ((vix->timedim < 0) && (viy->timedim < 0))
    {             
        adios_error(err_invalid_argument, "Covariance must involve timeseries data\n");
        return 0;
    }                                                                    

    uint32_t min = vix->dims[0] - 1;
    if (viy && (min > viy->dims[0] - 1))
        min = viy->dims[0] - 1;         
    
    if(time_start == 0 && time_end == 0) 
    { //global covariance
        if(viy == NULL) {
            adios_error(err_invalid_argument, "Must have two variables for global covariance\n");
            return 0;
        }                                                                          

        // Assign vix to viy, and calculate covariance
        viy = vix;
        time_start = 0;
        time_end = min;
    }
    // Check the bounds of time
    if (    (time_start >= 0) && (time_start <= min)
            &&      (time_end >= 0)   && (time_end <= min)
            &&  (time_start <= time_end))
    {
        if(viy == NULL) //user must want to run covariance against itself
        {
            if(! (time_end+lag) > min)
            {                                                                        
                adios_error(err_invalid_timestep, "Must leave enough timesteps for lag\n");
                return 0;
            }

            if (strcmp(characteristic, "average") == 0 || strcmp(characteristic, "avg") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double (adios_double, vix->avgs[i]) / (time_end - time_start + 1);
                    avg_lag += bp_value_to_double (adios_double, vix->avgs[i + lag]) / (time_end - time_start + 1);
                }

                for (i = time_start; i <= time_end; i ++)
                {
                    double val_x = bp_value_to_double (adios_double, vix->avgs[i]); 
                    double val_lag = bp_value_to_double (adios_double, vix->avgs[i + lag]); 
                    var_x += (val_x - avg_x) * (val_x - avg_x) / (time_end - time_start + 1); 
                    var_lag += (val_lag - avg_lag) * (val_lag - avg_lag) / (time_end - time_start + 1);
                    cov += (val_x - avg_x) * (val_lag - avg_lag) / (time_end - time_start + 1);
                }
            }
            else if (strcmp(characteristic, "standard deviation") == 0 || strcmp(characteristic, "std_dev") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double (adios_double, vix->std_devs[i]) / (time_end - time_start + 1);
                    avg_lag += bp_value_to_double (adios_double, vix->std_devs[i + lag]) / (time_end - time_start + 1);
                }

                for (i = time_start; i <= time_end; i ++)
                {
                    double val_x = bp_value_to_double (adios_double, vix->std_devs[i]);
                    double val_lag = bp_value_to_double (adios_double, vix->std_devs[i + lag]);
                    var_x += (val_x - avg_x) * (val_x - avg_x) / (time_end - time_start + 1);
                    var_lag += (val_lag - avg_lag) * (val_lag - avg_lag) / (time_end - time_start + 1);
                    cov += (val_x - avg_x) * (val_lag - avg_lag) / (time_end - time_start + 1);
                }
            }
            else if (strcmp(characteristic, "minimum") == 0 || strcmp(characteristic, "min") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double (vix->type, vix->mins[i]) / (time_end - time_start + 1);
                    avg_lag += bp_value_to_double (vix->type, vix->mins[i + lag]) / (time_end - time_start + 1);
                }

                for (i = time_start; i <= time_end; i ++)
                {
                    double val_x = bp_value_to_double (vix->type, vix->mins[i]); 
                    double val_lag = bp_value_to_double (vix->type, vix->mins[i + lag]); 
                    var_x += (val_x - avg_x) * (val_x - avg_x) / (time_end - time_start + 1); 
                    var_lag += (val_lag - avg_lag) * (val_lag - avg_lag) / (time_end - time_start + 1);
                    cov += (val_x - avg_x) * (val_lag - avg_lag) / (time_end - time_start + 1);
                }
            }
            else if (strcmp(characteristic, "maximum") == 0 || strcmp(characteristic, "max") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double (vix->type, vix->maxs[i]) / (time_end - time_start + 1);
                    avg_lag += bp_value_to_double (vix->type, vix->maxs[i]) / (time_end - time_start + 1);
                }

                for (i = time_start; i <= time_end; i ++)
                {
                    double val_x = bp_value_to_double (vix->type, vix->maxs[i]); 
                    double val_lag = bp_value_to_double (vix->type, vix->maxs[i + lag]); 
                    var_x += (val_x - avg_x) * (val_x - avg_x) / (time_end - time_start + 1); 
                    var_lag += (val_lag - avg_lag) * (val_lag - avg_lag) / (time_end - time_start + 1);
                    cov += (val_x - avg_x) * (val_lag - avg_lag) / (time_end - time_start + 1);
                }
            }
            else
            {
                adios_error(err_unknown_char, "Unknown characteristic\n");
                return 0;
            }
            return cov / (sqrt (var_x) * sqrt (var_lag));
        }
        else
        {
            if (strcmp(characteristic, "average") == 0 || strcmp(characteristic, "avg") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double(adios_double, vix->avgs[i]) / (time_end - time_start + 1);
                    avg_y += bp_value_to_double(adios_double, viy->avgs[i]) / (time_end - time_start + 1);
                }
                for (i = time_start; i <= time_end; i ++)
                {
                    double val_x = bp_value_to_double (adios_double, vix->avgs[i]); 
                    double val_y = bp_value_to_double (adios_double, viy->avgs[i]); 
                    var_x += (val_x - avg_x) * (val_x - avg_x) / (time_end - time_start + 1); 
                    var_y += (val_y - avg_y) * (val_y - avg_y) / (time_end - time_start + 1);
                    cov += (val_x - avg_x) * (val_y - avg_y) / (time_end - time_start + 1);
                }
            }
            else if (strcmp(characteristic, "standard deviation") == 0 || strcmp(characteristic, "std_dev") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double(adios_double, vix->std_devs[i]) / (time_end - time_start + 1);
                    avg_y += bp_value_to_double(adios_double, viy->std_devs[i]) / (time_end - time_start + 1);
                }
                for (i = time_start; i <= time_end; i ++)
                {
                    double val_x = bp_value_to_double (adios_double, vix->std_devs[i]);
                    double val_y = bp_value_to_double (adios_double, viy->std_devs[i]);
                    var_x += (val_x - avg_x) * (val_x - avg_x) / (time_end - time_start + 1);
                    var_y += (val_y - avg_y) * (val_y - avg_y) / (time_end - time_start + 1);
                    cov += (val_x - avg_x) * (val_y - avg_y) / (time_end - time_start + 1);
                }
            }
            else if (strcmp(characteristic, "minimum") == 0 || strcmp(characteristic, "min") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double(vix->type, vix->mins[i]) / (time_end - time_start + 1);
                    avg_y += bp_value_to_double(viy->type, viy->mins[i]) / (time_end - time_start + 1);
                }
                for (i = time_start; i <= time_end; i ++)
                {
                    double val_x = bp_value_to_double (vix->type, vix->mins[i]); 
                    double val_y = bp_value_to_double (viy->type, viy->mins[i]); 
                    var_x += (val_x - avg_x) * (val_x - avg_x) / (time_end - time_start + 1); 
                    var_y += (val_y - avg_y) * (val_y - avg_y) / (time_end - time_start + 1);
                    cov += (val_x - avg_x) * (val_y - avg_y) / (time_end - time_start + 1);
                }
            }
            else if (strcmp(characteristic, "maximum") == 0 || strcmp(characteristic, "max") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double(vix->type, vix->maxs[i]) / (time_end - time_start + 1);
                    avg_y += bp_value_to_double(vix->type, viy->maxs[i]) / (time_end - time_start + 1);
                }
                for (i = time_start; i <= time_end; i ++)
                {
                    double val_x = bp_value_to_double (vix->type, vix->maxs[i]); 
                    double val_y = bp_value_to_double (viy->type, viy->maxs[i]); 
                    var_x += (val_x - avg_x) * (val_x - avg_x) / (time_end - time_start + 1); 
                    var_y += (val_y - avg_y) * (val_y - avg_y) / (time_end - time_start + 1);
                    cov += (val_x - avg_x) * (val_y - avg_y) / (time_end - time_start + 1);
                }
            }
            else
            {
                adios_error(err_unknown_char, "Unknown characteristic\n");
                return 0;
            }
            return cov / (sqrt (var_x) * sqrt (var_y));
        }
    }
    else
    {
        adios_error(err_invalid_timestep, "Time values out of bounds\n");
        return 0;
    }
}

// NCSU - Time series analysis, covariance
//covariance(x,y) = sum(i=1,..N) [(x_1 - x_mean)(y_i - y_mean)]/N
double adios_stat_cov_v1 (ADIOS_VARINFO_V1 * vix, ADIOS_VARINFO_V1 * viy, char * characteristic, uint32_t time_start, uint32_t time_end, uint32_t lag)
{
    int i;

    double avg_x = 0.0, avg_y = 0.0, avg_lag = 0.0;
    double cov = 0;

    if (vix == NULL)
    {
        adios_error(err_invalid_argument, "Variable not defined\n");
        return 0;
    }

    // If the vix and viy are not time series objects, return.
    if ((vix->timedim < 0) && (viy->timedim < 0))
    {             
        adios_error(err_invalid_argument, "Covariance must involve timeseries data\n");
        return 0;
    }                                                                    

    uint32_t min = vix->dims[0] - 1;
    if (viy && (min > viy->dims[0] - 1))
        min = viy->dims[0] - 1;         
    
    if(time_start == 0 && time_end == 0) 
    { //global covariance
        if(viy == NULL) {
            adios_error(err_invalid_argument, "Must have two variables for global covariance\n");
            return 0;
        }                                                                          

        // Assign vix to viy, and calculate covariance
        viy = vix;
        time_start = 0;
        time_end = min;
    }
    // Check the bounds of time
    if (    (time_start >= 0) && (time_start <= min)
            &&      (time_end >= 0)   && (time_end <= min)
            &&  (time_start <= time_end))
    {
        if(viy == NULL) //user must want to run covariance against itself
        {
            if(! (time_end+lag) > min)
            {                                                                        
                adios_error(err_invalid_timestep, "Must leave enough timesteps for lag\n");
                return 0;
            }

            if (strcmp(characteristic, "average") == 0 || strcmp(characteristic, "avg") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double (adios_double, vix->avgs[i]) / (time_end - time_start + 1);
                    avg_lag += bp_value_to_double (adios_double, vix->avgs[i + lag]) / (time_end - time_start + 1);
                }

                for (i = time_start; i <= time_end; i ++)
                    cov += (bp_value_to_double (adios_double, vix->avgs[i]) - avg_x) * (bp_value_to_double (adios_double, vix->avgs[i+lag]) - avg_lag) / (time_end - time_start + 1);
            }
            else if (strcmp(characteristic, "standard deviation") == 0 || strcmp(characteristic, "std_dev") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double (adios_double, vix->std_devs[i]) / (time_end - time_start + 1);
                    avg_lag += bp_value_to_double (adios_double, vix->std_devs[i + lag]) / (time_end - time_start + 1);
                }

                for (i = time_start; i <= time_end; i ++)
                    cov += (bp_value_to_double (adios_double, vix->std_devs[i]) - avg_x) * (bp_value_to_double (adios_double, vix->std_devs[i+lag]) - avg_lag) / (time_end - time_start + 1);
            }
            else if (strcmp(characteristic, "minimum") == 0 || strcmp(characteristic, "min") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double (vix->type, vix->mins[i]) / (time_end - time_start + 1);
                    avg_lag += bp_value_to_double (vix->type, vix->mins[i + lag]) / (time_end - time_start + 1);
                }

                for (i = time_start; i <= time_end; i ++)
                    cov += (bp_value_to_double (vix->type, vix->mins[i]) - avg_x) * (bp_value_to_double (vix->type, vix->mins[i+lag]) - avg_lag) / (time_end - time_start + 1);
            }
            else if (strcmp(characteristic, "maximum") == 0 || strcmp(characteristic, "max") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double (vix->type, vix->maxs[i]) / (time_end - time_start + 1);
                    avg_lag += bp_value_to_double (vix->type, vix->maxs[i + lag]) / (time_end - time_start + 1);
                }

                for (i = time_start; i <= time_end; i ++)
                    cov += (bp_value_to_double (vix->type, vix->maxs[i]) - avg_x) * (bp_value_to_double (vix->type, vix->maxs[i+lag]) - avg_lag) / (time_end - time_start + 1);
            }
            else
            {
                adios_error(err_unknown_char, "Unknown characteristic\n");
                return 0;
            }
        }
        else
        {
            if (strcmp(characteristic, "average") == 0 || strcmp(characteristic, "avg") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double(adios_double, vix->avgs[i]) / (time_end - time_start + 1);
                    avg_y += bp_value_to_double(adios_double, viy->avgs[i]) / (time_end - time_start + 1);
                }
                for (i = time_start; i <= time_end; i ++)
                {
                    cov += (bp_value_to_double(adios_double, vix->avgs[i]) - avg_x) * (bp_value_to_double(adios_double, viy->avgs[i]) - avg_y) / (time_end - time_start + 1);
                }
            }
            else if (strcmp(characteristic, "standard deviation") == 0 || strcmp(characteristic, "std_dev") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double(adios_double, vix->std_devs[i]) / (time_end - time_start + 1);
                    avg_y += bp_value_to_double(adios_double, viy->std_devs[i]) / (time_end - time_start + 1);
                }
                for (i = time_start; i <= time_end; i ++)
                {
                    cov += (bp_value_to_double(adios_double, vix->std_devs[i]) - avg_x) * (bp_value_to_double(adios_double, viy->std_devs[i]) - avg_y) / (time_end - time_start + 1);
                }
            }
            else if (strcmp(characteristic, "minimum") == 0 || strcmp(characteristic, "min") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double(vix->type, vix->mins[i]) / (time_end - time_start + 1);
                    avg_y += bp_value_to_double(viy->type, viy->mins[i]) / (time_end - time_start + 1);
                }
                for (i = time_start; i <= time_end; i ++)
                {
                    cov += (bp_value_to_double(vix->type, vix->mins[i]) - avg_x) * (bp_value_to_double(viy->type, viy->mins[i]) - avg_y) / (time_end - time_start + 1);
                }
            }
            else if (strcmp(characteristic, "maximum") == 0 || strcmp(characteristic, "max") == 0)
            {
                for (i = time_start; i <= time_end; i ++)
                {
                    avg_x += bp_value_to_double(vix->type, vix->maxs[i]) / (time_end - time_start + 1);
                    avg_y += bp_value_to_double(vix->type, viy->maxs[i]) / (time_end - time_start + 1);
                }
                for (i = time_start; i <= time_end; i ++)
                {
                    cov += (bp_value_to_double(vix->type, vix->maxs[i]) - avg_x) * (bp_value_to_double(viy->type, viy->maxs[i]) - avg_y) / (time_end - time_start + 1);
                }
            }
            else
            {
                adios_error(err_unknown_char, "Unknown characteristic\n");
                return 0;
            }
        }
    }
    else
    {
        adios_error(err_invalid_timestep, "Time values out of bounds\n");
    }
    return cov;
}
