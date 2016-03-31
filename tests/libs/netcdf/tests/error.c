/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/error.c,v 1.8 2004/11/16 21:33:07 russ Exp $
 *********************************************************************/

#include <stdio.h>

#ifndef NO_STDARG
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <netcdf.h>
#include "error.h"
static int	error_count = 0;

/*
 * Use for logging error conditions.
 */
#ifndef NO_STDARG
void
error(const char *fmt, ...)
#else
/*VARARGS1*/
void
error(fmt, va_alist)
     const char *fmt ;
     va_dcl
#endif
{
    va_list args ;

    (void) fprintf(stderr,"*** ");

#ifndef NO_STDARG
    va_start(args, fmt) ;
#else
    va_start(args) ;
#endif
    (void) vfprintf(stderr,fmt,args) ;
    va_end(args) ;

    (void) fprintf(stderr, "\n") ;
    error_count++;
}


/*
 * Turn off netCDF library handling of errors.  Caller must check all error
 * returns after calling this, until on_errs() is called.
 */
void
off_errs()
{
    extern int ncopts;		/* error options */
    ncopts &= ~NC_FATAL;	/* make errors nonfatal */
    ncopts &= ~NC_VERBOSE;	/* turn off error messages */
}


/*
 * Let netCDF library handle subsequent errors.  Callers don't need to check
 * error returns after this.  (This is the initial default.)
 */
void
on_errs()
{
    extern int ncopts;		/* error options */
    ncopts |= NC_FATAL;		/* make errors fatal */
    ncopts |= NC_VERBOSE;	/* library prints error messages */
}
