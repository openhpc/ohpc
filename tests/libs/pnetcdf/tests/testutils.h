/*
 *  Copyright (C) 2003, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 */
/* $Id$ */


#ifndef H_UTILS
#define H_UTILS

#ifdef HAVE_CONFIG_H
#include <config.h> /* output of 'configure' */
#endif

#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define CHECK_ERR { \
    if (err != NC_NOERR) { \
        nerrs++; \
        printf("Error at line %d in %s: (%s)\n", \
        __LINE__,__FILE__,ncmpi_strerrno(err)); \
    } \
}

#define CHECK_ERROUT { \
    if (err != NC_NOERR) { \
        nerrs++; \
        printf("Error at line %d in %s: (%s)\n", \
        __LINE__,__FILE__,ncmpi_strerrno(err)); \
        goto err_out; \
    } \
}

#define CHECK_FATAL_ERR { \
    if (err != NC_NOERR) { \
        nerrs++; \
        printf("Error at line %d in %s: (%s)\n", \
        __LINE__,__FILE__,ncmpi_strerrno(err)); \
        MPI_Abort(MPI_COMM_WORLD, -1); \
    } \
}

#define EXP_ERR(exp) { \
    if (err != exp) { \
        nerrs++; \
        printf("Error at line %d in %s: expecting %s but got %s\n", \
        __LINE__,__FILE__,ncmpi_strerrno(exp), ncmpi_strerrno(err)); \
    } \
}

int inq_env_hint(char *hint_key, char **hint_value);

#ifdef PNETCDF_DEBUG
#define PASS_STR "\x1b[32mpass\x1b[0m\n"
#define SKIP_STR "\x1b[32mskip\x1b[0m\n"
#define FAIL_STR "\x1b[31mfail\x1b[0m with %d mismatches\n"
#else
#define PASS_STR "pass\n"
#define SKIP_STR "skip\n"
#define FAIL_STR "fail with %d mismatches\n"
#endif

#define MPI_ERR(err) \
    if (err != MPI_SUCCESS) { \
        char err_string[MPI_MAX_ERROR_STRING+1]; \
        int  err_len; \
        MPI_Error_string(err, err_string, &err_len); \
        printf("MPI Error at file %s line %d (%s)\n",__FILE__,__LINE__,err_string); \
    }

extern char* nc_err_code_name(int err);

#if MPI_VERSION < 3
/* MPI_OFFSET was first defined in MPI standard 2.2 */
#ifndef HAVE_DECL_MPI_OFFSET
#define MPI_OFFSET MPI_LONG_LONG_INT
#endif
#endif

#ifndef HAVE_STRDUP
extern char *strdup(const char *s);
#endif
#ifndef HAVE_STRCASECMP
extern int strcasecmp(const char *s1, const char *s2);
#endif

char* remove_file_system_type_prefix(const char *filename);

#endif
