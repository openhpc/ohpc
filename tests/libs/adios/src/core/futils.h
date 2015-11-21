/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#ifndef _FUTILS_H_
#define _FUTILS_H_

/** Copy a C string into a Fortran CHARACTER array (of max length flen).
    Fortran array must already be allocated in Fortran */
void futils_cstr_to_fstr(const char *cs, char *fs, int flen);

/** Trim a Fortran string and allocate a C string and copy content to it and add '\0' 
 *  Need to free() the string later.
 */
char * futils_fstr_to_cstr(const char * fs, int flen);

/* Indicate to ADIOS that we are calling from Fortran. 
   It is used in the read API to know what ordering of dimensions the caller expects.
*/
void futils_called_from_fortran_set(void);
void futils_called_from_fortran_unset(void);
/* Read API asks if the caller is Fortran by this function call */
int futils_is_called_from_fortran(void);

#endif
