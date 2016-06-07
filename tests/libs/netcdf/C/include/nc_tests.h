/** \internal
\file
Common includes, defines, etc., for test code in the libsrc4 and
nc_test4 directories.

This is part of the netCDF package. Copyright 2005 University
Corporation for Atmospheric Research/Unidata. See \ref copyright file
for conditions of use.
*/

#ifndef _NC_TESTS_H
#define _NC_TESTS_H

#include <config.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef USE_PARALLEL
#include "netcdf_par.h"
#endif
#include "netcdf.h"
#include "err_macros.h"

/** Useful define for tests. */
/** \{ */
#define MEGABYTE 1048576
#define HALF_MEG (MEGABYTE/2)
#define MILLION 1000000
#define SIXTEEN_MEG 16777216
#define FOUR_MEG (SIXTEEN_MEG/4)
#define THIRTY_TWO_MEG (SIXTEEN_MEG * 2)
#define SIXTY_FOUR_MEG (SIXTEEN_MEG * 4)
#define ONE_TWENTY_EIGHT_MEG (SIXTEEN_MEG * 8)
/** \} */

#endif /* _NC_TESTS_H */
