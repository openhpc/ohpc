/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#ifndef ADIOS_MPI_H
#define ADIOS_MPI_H

#ifdef _NOMPI
/* Sequential processes can use the library compiled with -D_NOMPI */
#   include "mpidummy.h"
#else
/* Parallel applications should use MPI to communicate file info and slices of data */
#   include "mpi.h"
#endif


#endif
