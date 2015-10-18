/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/*
 *   Functions, constants globally for both the Write and Read API
 */
#ifndef __GLOBALS_H__
#define __GLOBALS_H__

#include "config.h"

/** Set an application ID for this program. 
 *  This function is necessary for methods who needs a unique ID from each participating applications.
 *  Currently, this is the DATASPACES/DIMES methods for code coupling of independent applications.
 *
 *  This function is called from the applicatin through adios_set_application_id()
 */
void globals_adios_set_application_id (int id);


/** Get the application ID set by the application itself.
  * Returns the ID set by the application, -1 if it was not set.
  * It also returns a boolean was_set, 0 if it was not set, 1 otherwise.
  */
int globals_adios_get_application_id (int *was_set);


/* Note: would be nice a <string, int> map for arbitrary globals */
#ifdef HAVE_DATASPACES
void globals_adios_set_dataspaces_connected_from_reader();
void globals_adios_set_dataspaces_disconnected_from_reader();
void globals_adios_set_dataspaces_connected_from_writer();
void globals_adios_set_dataspaces_disconnected_from_writer();
int  globals_adios_is_dataspaces_connected(); // from any
int  globals_adios_is_dataspaces_connected_from_reader();
int  globals_adios_is_dataspaces_connected_from_writer();
int  globals_adios_is_dataspaces_connected_from_both();
#endif /* HAVE_DATASPACES */

#ifdef HAVE_DIMES
void globals_adios_set_dimes_connected_from_reader();
void globals_adios_set_dimes_disconnected_from_reader();
void globals_adios_set_dimes_connected_from_writer();
void globals_adios_set_dimes_disconnected_from_writer();
int  globals_adios_is_dimes_connected(); // from any
int  globals_adios_is_dimes_connected_from_reader();
int  globals_adios_is_dimes_connected_from_writer();
int  globals_adios_is_dimes_connected_from_both();
#endif

#if NO_DATATAP == 0

char *getFixedName(char *name);
char *get_full_path_name(char *name, char *path);

#endif

#ifdef HAVE_ICEE

#ifndef ICEE_MAX_PARALLEL
#define ICEE_MAX_PARALLEL 32
#endif

#ifndef ICEE_MAX_REMOTE
#define ICEE_MAX_REMOTE 32
#endif

#include "core/adios_icee.h"

typedef enum {TCP, UDP, ENET, NNTI, IB} icee_transport_t;
static const char *icee_transport_name[] = {
    "TCP", "UDP", "ENET", "NNTI", "IB",
};

void icee_dims_print(const char* name, const int ndims, const uint64_t *dims);
void icee_varinfo_print(const icee_varinfo_rec_ptr_t vp);
void icee_fileinfo_print(const void* item);
void icee_contactinfo_print(const icee_contactinfo_rec_ptr_t cp);
#endif

#endif  /*__GLOBALS_H__*/
