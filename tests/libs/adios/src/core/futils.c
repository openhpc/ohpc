/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include <stdlib.h>
#include <string.h>
#include "core/futils.h"
#include "core/bp_utils.h" // error()
#include "adios_error.h" // err_no_memory

#ifdef DMALLOC
#include "dmalloc.h"
#endif

static int called_from_fortran = 0; // set to 1 when called from Fortran API
void futils_called_from_fortran_set(void) {called_from_fortran = 1;}
void futils_called_from_fortran_unset(void) {called_from_fortran = 0;}
int futils_is_called_from_fortran(void) {return called_from_fortran;}

/** Copy a C string into a Fortran CHARACTER array */
void futils_cstr_to_fstr(const char *cs, char *fs, int flen) 
{
    int clen = strlen(cs);
    if (clen > flen)
        clen = flen;
    strncpy(fs, cs, clen);           /* does not copy the '\0' */
    memset(fs+clen, ' ', flen-clen); /* right pad with spaces the CHARACTER array */
}

/** Trim a Fortran string and allocate a C string and copy content to it and add '\0' 
 *  Need to free() the string later.
 */
char * futils_fstr_to_cstr(const char * fs, int flen)
{
    char *cs;
    int clen = flen;
    while (clen > 0 && fs[clen-1] == ' ')
        clen--;
    cs = (char*) malloc ((size_t) (clen + 1));
    if (cs == NULL) {
        adios_error (err_no_memory, "ERROR: Cannot allocate %d bytes for a C string in ADIOS API", clen+1);
        return NULL;
    }
    strncpy (cs, fs, clen);
    cs[clen] = '\0';
    return cs;
}

