/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "adios_error.h"
#include "core/adios_logger.h"

#ifdef DMALLOC
#include "dmalloc.h"
#endif

#define ERRMSG_MAXLEN 256

//  adios_errno is extern defined in adios_read.h and adiosf.c
int adios_errno;  

// string to store last error message
// cannot be static because adios_errmsg returns it
char aerr[ERRMSG_MAXLEN];

const char *adios_get_last_errmsg (void) 
{ 
    return aerr; 
}

void adios_clear_error(void)
{
    memset (aerr, 0, ERRMSG_MAXLEN);
    adios_errno = err_no_error;
}

void adios_error (enum ADIOS_ERRCODES errcode, char *fmt, ...) 
{
    va_list ap;
    adios_errno = (int)errcode;
    va_start(ap, fmt);
    (void) vsnprintf(aerr, ERRMSG_MAXLEN, fmt, ap);
    va_end(ap);
    log_error("%s", aerr);
}

void adios_error_at_line (enum ADIOS_ERRCODES errcode, const char* filename, unsigned int linenum, char *fmt, ...)
{
    va_list ap;
    adios_errno = (int)errcode;
    va_start(ap, fmt);
    (void) vsnprintf(aerr, ERRMSG_MAXLEN, fmt, ap);
    va_end(ap);
    log_error("%s", aerr);
}

