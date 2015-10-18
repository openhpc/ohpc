/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2014.  Kitware, Inc. All rights reserved.
 */

/**********************************************************************/
/* Forward declarations of the C structs used in the read API.  This  */
/* header can be directly copied and used in interface code to allow  */
/* classes that use the ADIOS structures in their interface to be     */
/* consumed with no direct knowledge of ADIOS.                        */
/**********************************************************************/
#ifndef __ADIOS_READ_V2_FWD_H__
#define __ADIOS_READ_V2_FWD_H__

#ifdef __cplusplus
extern "C" {
#endif

struct _ADIOS_FILE;
typedef struct _ADIOS_FILE ADIOS_FILE;

struct _ADIOS_VARSTAT;
typedef struct _ADIOS_VARSTAT ADIOS_VARSTAT;

struct _ADIOS_VARBLOCK;
typedef struct _ADIOS_VARBLOCK ADIOS_VARBLOCK;

struct _ADIOS_VARMESH;
typedef struct _ADIOS_VARMESH ADIOS_VARMESH;

struct _ADIOS_VARINFO;
typedef struct _ADIOS_VARINFO ADIOS_VARINFO;

struct _ADIOS_VARCHUNK;
typedef struct _ADIOS_VARCHUNK ADIOS_VARCHUNK;

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
