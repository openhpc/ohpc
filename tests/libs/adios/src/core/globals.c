/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/*
 *   Functions, constants globally for both the Write and Read API of Staging methods
 */

#include "globals.h"

static int globals_adios_appid = -1;
static int globals_adios_was_set = 0;
void globals_adios_set_application_id (int id)
{
    globals_adios_appid = id;
    globals_adios_was_set = 1;
}

int globals_adios_get_application_id (int *was_set) 
{
    *was_set = globals_adios_was_set;
    return globals_adios_appid;
}

#ifdef HAVE_DATASPACES
enum DATASPACES_CONNECTION { dataspaces_disconnected = 0, 
                       dataspaces_connected_from_reader = 1,
                       dataspaces_connected_from_writer = 2,
                       dataspaces_connected_from_both = 3
                     };
static enum DATASPACES_CONNECTION globals_adios_connected_to_dataspaces = dataspaces_disconnected;

void globals_adios_set_dataspaces_connected_from_reader()
{ 
    if (globals_adios_connected_to_dataspaces == dataspaces_disconnected)
        globals_adios_connected_to_dataspaces = dataspaces_connected_from_reader;
    else if (globals_adios_connected_to_dataspaces == dataspaces_connected_from_writer)
        globals_adios_connected_to_dataspaces = dataspaces_connected_from_both;
}
void globals_adios_set_dataspaces_disconnected_from_reader()
{ 
    if (globals_adios_connected_to_dataspaces == dataspaces_connected_from_reader)
        globals_adios_connected_to_dataspaces = dataspaces_disconnected;
    else if (globals_adios_connected_to_dataspaces == dataspaces_connected_from_both)
        globals_adios_connected_to_dataspaces = dataspaces_connected_from_writer;
}
void globals_adios_set_dataspaces_connected_from_writer()
{
    if (globals_adios_connected_to_dataspaces == dataspaces_disconnected)
        globals_adios_connected_to_dataspaces = dataspaces_connected_from_writer;
    else if (globals_adios_connected_to_dataspaces == dataspaces_connected_from_reader)
        globals_adios_connected_to_dataspaces = dataspaces_connected_from_both;
}
void globals_adios_set_dataspaces_disconnected_from_writer()
{ 
    if (globals_adios_connected_to_dataspaces == dataspaces_connected_from_writer)
        globals_adios_connected_to_dataspaces = dataspaces_disconnected;
    else if (globals_adios_connected_to_dataspaces == dataspaces_connected_from_both)
        globals_adios_connected_to_dataspaces = dataspaces_connected_from_reader;
}
int  globals_adios_is_dataspaces_connected()
{ 
    return (globals_adios_connected_to_dataspaces != dataspaces_disconnected);
}
int  globals_adios_is_dataspaces_connected_from_reader()
{ 
    return (globals_adios_connected_to_dataspaces == dataspaces_connected_from_reader || 
            globals_adios_connected_to_dataspaces == dataspaces_connected_from_both);
}
int  globals_adios_is_dataspaces_connected_from_writer()
{ 
    return (globals_adios_connected_to_dataspaces == dataspaces_connected_from_writer || 
            globals_adios_connected_to_dataspaces == dataspaces_connected_from_both);
}
int  globals_adios_is_dataspaces_connected_from_both()
{
    return (globals_adios_connected_to_dataspaces == dataspaces_connected_from_both);
}
#endif

#ifdef HAVE_DIMES
enum DIMES_CONNECTION { dimes_disconnected = 0,
                        dimes_connected_from_reader = 1,
                        dimes_connected_from_writer = 2,
                        dimes_connected_from_both = 3
                     };
static enum DIMES_CONNECTION globals_adios_connected_to_dimes = dimes_disconnected;

void globals_adios_set_dimes_connected_from_reader()
{
    if (globals_adios_connected_to_dimes == dimes_disconnected)
        globals_adios_connected_to_dimes = dimes_connected_from_reader;
    else if (globals_adios_connected_to_dimes == dimes_connected_from_writer)
        globals_adios_connected_to_dimes = dimes_connected_from_both;
}
void globals_adios_set_dimes_disconnected_from_reader()
{
    if (globals_adios_connected_to_dimes == dimes_connected_from_reader)
        globals_adios_connected_to_dimes = dimes_disconnected;
    else if (globals_adios_connected_to_dimes == dimes_connected_from_both)
        globals_adios_connected_to_dimes = dimes_connected_from_writer;
}
void globals_adios_set_dimes_connected_from_writer()
{
    if (globals_adios_connected_to_dimes == dimes_disconnected)
        globals_adios_connected_to_dimes = dimes_connected_from_writer;
    else if (globals_adios_connected_to_dimes == dimes_connected_from_reader)
        globals_adios_connected_to_dimes = dimes_connected_from_both;
}
void globals_adios_set_dimes_disconnected_from_writer()
{
    if (globals_adios_connected_to_dimes == dimes_connected_from_writer)
        globals_adios_connected_to_dimes = dimes_disconnected;
    else if (globals_adios_connected_to_dimes == dimes_connected_from_both)
        globals_adios_connected_to_dimes = dimes_connected_from_reader;
}
int  globals_adios_is_dimes_connected()
{
    return (globals_adios_connected_to_dimes != dimes_disconnected);
}
int  globals_adios_is_dimes_connected_from_reader()
{
    return (globals_adios_connected_to_dimes == dimes_connected_from_reader ||
            globals_adios_connected_to_dimes == dimes_connected_from_both);
}
int  globals_adios_is_dimes_connected_from_writer()
{
    return (globals_adios_connected_to_dimes == dimes_connected_from_writer ||
            globals_adios_connected_to_dimes == dimes_connected_from_both);
}
int  globals_adios_is_dimes_connected_from_both()
{
    return (globals_adios_connected_to_dimes == dimes_connected_from_both);
}
#endif

#if NO_DATATAP == 0

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define OPLEN 2
static char OP[OPLEN] = { '(', ')' };
static char *OP_REP[OPLEN] = { "_PPLT_", "_PPRT_" };

char *getFixedName(char *name)
{
    char *tempname = (char *) malloc(sizeof(char) * 255);
    snprintf(tempname, 255, "%s\0", name);
    char *oldname = strdup(name);
    char *loc = NULL;
    int i;

    do
    {
        for (i = 0; i < OPLEN; i++)
        {
    //checking operator OP[i]
            loc = strchr(oldname, OP[i]);
            if (loc == NULL)
                continue;
            *loc = 0;
            snprintf(tempname, 255, "%s%s%s\0", oldname, OP_REP[i], &loc[1]);
            free(oldname);
            oldname = strdup(tempname);
        }
    }
    while (loc != NULL);
    free(oldname);

fprintf(stderr, "im here %s %s %s:%d\n", name, tempname, __FILE__,__LINE__);
    return tempname;
}

char *get_full_path_name(char *name, char *path)
{
    char *full_pathname = (char *) malloc(strlen(name)+strlen(path)+2);
    if(!full_pathname) {
        fprintf(stderr, "cannot allocate memory. %s:%d\n", __FILE__,__LINE__);
        return NULL;
    }
    if (!path || !path[0]) { // null or empty string
        sprintf (full_pathname, "%s\0", name);
    } else if (!strcmp (path, "/")) {
        sprintf (full_pathname, "/%s\0", name);
    }
    else {
        sprintf (full_pathname, "%s/%s\0", path, name);
    }
fprintf(stderr, "im here %s %s %s %s:%d\n", name, path, full_pathname,__FILE__,__LINE__);
    return full_pathname;
}

#endif

#ifdef HAVE_ICEE

#include <stdio.h>

double dsum(const int len, const double* data)
{
    double s = 0.0;
    int i;
    for (i=0; i<len; i++)
        s += data[i];

    return s;
}

void icee_data_print(const int type, const uint64_t varlen, const char* data)
{
    fprintf(stderr, "%10s : %p\n", "*data", data);
    switch (type)
    {
    case 2: // int
        if (data)
        {
            fprintf(stderr, "%10s : %d,%d,%d,...\n", "data", 
                    ((int*)data)[0], ((int*)data)[1], ((int*)data)[2]);
            fprintf(stderr, "%10s : %g\n", "sum", dsum(varlen/8, (double*)data));
        }
        break;
    case 6: // double
        if (data)
        {
            fprintf(stderr, "%10s : %g,%g,%g,...\n", "data", 
                    ((double*)data)[0], ((double*)data)[1], ((double*)data)[2]);
            fprintf(stderr, "%10s : %g\n", "sum", dsum(varlen/8, (double*)data));
        }
        break;
    }
}

void icee_dims_print(const char* name, const int ndims, const uint64_t *dims)
{
    
    switch (ndims)
    {
    case 0:
        fprintf(stderr, "%10s : none\n", name);
        break;
    case 1:
        fprintf(stderr, "%10s : %llu\n", name, dims[0]);
        break;
    case 2:
        fprintf(stderr, "%10s : %llu,%llu\n", 
                name, dims[0], dims[1]);
        break;
    case 3:
        fprintf(stderr, "%10s : %llu,%llu,%llu\n", 
                name, dims[0], dims[1], dims[2]);
        break;
    default:
        fprintf(stderr, "%10s : %llu,%llu,%llu,...\n", 
                name, dims[0], dims[1], dims[2]);
        break;
    }
}

void icee_varinfo_print(const icee_varinfo_rec_ptr_t vp)
{
    fprintf(stderr, "===== varinfo (%p) =====\n", vp);

    if (vp)
    {
        fprintf(stderr, "%10s : %s\n", "varname", vp->varname);
        fprintf(stderr, "%10s : %d\n", "varid", vp->varid);
        fprintf(stderr, "%10s : %d\n", "type", vp->type);
        fprintf(stderr, "%10s : %d\n", "typesize", vp->typesize);
        fprintf(stderr, "%10s : %d\n", "ndims", vp->ndims);
        icee_dims_print("gdims", vp->ndims, vp->gdims);
        icee_dims_print("ldims", vp->ndims, vp->ldims);
        icee_dims_print("offsets", vp->ndims, vp->offsets);
        fprintf(stderr, "%10s : %llu\n", "varlen", vp->varlen);
        icee_data_print(vp->type, vp->varlen, vp->data);
    }
    else
    {
        fprintf(stderr, "varinfo is invalid\n");
    }
}

void icee_fileinfo_print(const void* item)
{
    icee_fileinfo_rec_ptr_t fp = (icee_fileinfo_rec_ptr_t) item;

    fprintf(stderr, "===== fileinfo (%p) =====\n", fp);

    if (fp)
    {
        fprintf(stderr, "%10s : %s\n", "fname", fp->fname);
        fprintf(stderr, "%10s : %d\n", "nvars", fp->nvars);
        fprintf(stderr, "%10s : %d\n", "nchunks", fp->nchunks);
        fprintf(stderr, "%10s : %d\n", "comm_size", fp->comm_size);
        fprintf(stderr, "%10s : %d\n", "comm_rank", fp->comm_rank);
        fprintf(stderr, "%10s : %d\n", "merge_count", fp->merge_count);
        fprintf(stderr, "%10s : %d\n", "timestep", fp->timestep);
        fprintf(stderr, "%10s : %p\n", "varinfo", fp->varinfo);
        fprintf(stderr, "%10s : %p\n", "next", fp->next);

        icee_varinfo_rec_ptr_t vp = fp->varinfo;

        while (vp != NULL)
        {
            icee_varinfo_print(vp);
            vp = vp->next; 
        }

        if (fp->next != NULL)
            icee_fileinfo_print(fp->next);
    }
    else
    {
        fprintf(stderr, "fileinfo is invalid\n");
    }
}

void icee_contactinfo_print(const icee_contactinfo_rec_ptr_t cp)
{
    fprintf(stderr, "===== contactinfo (%p) =====\n", cp);

    if (cp)
    {
        fprintf(stderr, "%10s : %d\n", "stone_id", cp->stone_id);
        fprintf(stderr, "%10s : %s\n", "contact_string", cp->contact_string);
        fdump_attr_list(stderr, attr_list_from_string(cp->contact_string));
        fprintf(stderr, "%10s : %p\n", "next", cp->next);
        if (cp->next != NULL)
            icee_contactinfo_print(cp->next);
    }
    else
    {
        fprintf(stderr, "contactinfo is invalid\n");
    }
}

#endif
