/*
 *  Copyright (C) 2003, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 */
/* $Id: testutils.h 2675 2016-12-04 18:55:24Z wkliao $ */


#ifndef _UTILS_H
#define _UTILS_H

#include <stdlib.h>
#include <string.h>
#include <limits.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

typedef struct {
	char infname[PATH_MAX];
	char outfname[PATH_MAX];
} params;

void parse_read_args(int argc, char **argv, int rank, params *p);
void parse_write_args(int argc, char **argv, int rank, params *p);

#ifdef PNETCDF_DEBUG
#define PASS_STR "\x1b[32mpass\x1b[0m\n"
#define FAIL_STR "\x1b[31mfail\x1b[0m with %d mismatches\n"
#else
#define PASS_STR "pass\n"
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

#ifndef MPI_OFFSET
#define MPI_OFFSET MPI_LONG_LONG_INT
#endif

#endif
