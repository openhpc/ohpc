/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#ifndef ADIOS_TRANSPORT_HOOKS_H
#define ADIOS_TRANSPORT_HOOKS_H

#include "config.h"
#include <stdint.h>
#include <string.h>
#include "core/util.h" /* PairStruct* */
#include "public/adios_mpi.h"

#define FORWARD_DECLARE_EMPTY(a) \
void adios_##a##_init (const PairStruct * parameters \
                      ,struct adios_method_struct * method \
                      ) {} \
int adios_##a##_open (struct adios_file_struct * fd \
                     ,struct adios_method_struct * method, MPI_Comm comm \
                     ) {return 0;} \
enum ADIOS_FLAG adios_##a##_should_buffer (struct adios_file_struct * fd \
                                          ,struct adios_method_struct * method \
                                          ) {return 0;} \
void adios_##a##_write (struct adios_file_struct * fd \
                       ,struct adios_var_struct * v \
                       ,void * data \
                       ,struct adios_method_struct * method \
                       ) {} \
void adios_##a##_get_write_buffer (struct adios_file_struct * fd \
                                  ,struct adios_var_struct * v \
                                  ,uint64_t * size \
                                  ,void ** buffer \
                                  ,struct adios_method_struct * method \
                                  ) {} \
void adios_##a##_read (struct adios_file_struct * fd \
                      ,struct adios_var_struct * v \
                      ,void * buffer \
                      ,uint64_t buffer_size \
                      ,struct adios_method_struct * method \
                      ) {} \
void adios_##a##_close (struct adios_file_struct * fd \
                       ,struct adios_method_struct * method \
                       ) {} \
void adios_##a##_finalize (int mype, struct adios_method_struct * method) {} \
void adios_##a##_end_iteration (struct adios_method_struct * method) {} \
void adios_##a##_start_calculation (struct adios_method_struct * method) {} \
void adios_##a##_stop_calculation (struct adios_method_struct * method) {}

#define FORWARD_DECLARE(a) \
void adios_##a##_init (const PairStruct * parameters \
                      ,struct adios_method_struct * method \
                      ); \
int adios_##a##_open (struct adios_file_struct * fd \
                     ,struct adios_method_struct * method, MPI_Comm comm \
                     ); \
enum ADIOS_FLAG adios_##a##_should_buffer (struct adios_file_struct * fd \
                                          ,struct adios_method_struct * method \
                                          ); \
void adios_##a##_write (struct adios_file_struct * fd \
                       ,struct adios_var_struct * v \
                       ,void * data \
                       ,struct adios_method_struct * method \
                       ); \
void adios_##a##_get_write_buffer (struct adios_file_struct * fd \
                                  ,struct adios_var_struct * v \
                                  ,uint64_t * size \
                                  ,void ** buffer \
                                  ,struct adios_method_struct * method \
                                  ); \
void adios_##a##_read (struct adios_file_struct * fd \
                      ,struct adios_var_struct * v \
                      ,void * buffer \
                      ,uint64_t buffer_size \
                      ,struct adios_method_struct * method \
                      ); \
void adios_##a##_close (struct adios_file_struct * fd \
                       ,struct adios_method_struct * method \
                       ); \
void adios_##a##_finalize (int mype, struct adios_method_struct * method); \
void adios_##a##_end_iteration (struct adios_method_struct * method); \
void adios_##a##_start_calculation (struct adios_method_struct * method); \
void adios_##a##_stop_calculation (struct adios_method_struct * method);
//#endif

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
//// SETUP YOUR NEW TRANSPORT METHODS BELOW (FOLLOW THE PATTERN):          ////
//// 1. Add an entry to the ADIOS_IO_METHOD updating the ADIOS_METHOD_COUNT////
//// 2. Add a FOWARD_DECLARE line (assuming standard naming)               ////
//// 3. Add an entry to ADIOS_PARSE_METHOD_SETUP for the string and ID     ////
//// 4. Add an entry to ADIOS_INIT_TRANSPORTS_SETUP for name to ID         ////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

struct adios_method_struct;
struct adios_file_struct;
struct adios_var_struct;
// the list of the methods that have been integrated
// VTK and POSIX_ASCII are placeholders reserved for future use
enum ADIOS_IO_METHOD {ADIOS_METHOD_UNKNOWN     = -2
              ,ADIOS_METHOD_NULL        = -1
              ,ADIOS_METHOD_MPI         = 0
              ,ADIOS_METHOD_DATATAP     = 1
              ,ADIOS_METHOD_POSIX       = 2
              ,ADIOS_METHOD_DATASPACES  = 3
              ,ADIOS_METHOD_VTK         = 4
              ,ADIOS_METHOD_POSIX_ASCII = 5
              ,ADIOS_METHOD_MPI_CIO     = 6
              ,ADIOS_METHOD_PHDF5       = 7
              ,ADIOS_METHOD_PROVENANCE  = 8
              ,ADIOS_METHOD_MPI_STRIPE  = 9
              ,ADIOS_METHOD_MPI_LUSTRE  = 10
              ,ADIOS_METHOD_MPI_STAGGER = 11
              ,ADIOS_METHOD_MPI_AGG     = 12
              ,ADIOS_METHOD_ADAPTIVE    = 13
              ,ADIOS_METHOD_POSIX1      = 14
              ,ADIOS_METHOD_NC4         = 15
              ,ADIOS_METHOD_MPI_AMR     = 16
              ,ADIOS_METHOD_MPI_AMR1    = 17
              ,ADIOS_METHOD_FLEXPATH    = 18
              ,ADIOS_METHOD_NSSI_STAGING = 19
              ,ADIOS_METHOD_NSSI_FILTER  = 20
              ,ADIOS_METHOD_DIMES        = 21
              ,ADIOS_METHOD_VAR_MERGE   = 22
              ,ADIOS_METHOD_MPI_BGQ     = 23
              ,ADIOS_METHOD_ICEE        = 24
              ,ADIOS_METHOD_COUNT       = 25
};

// forward declare the functions (or dummies for internals use)
#if !defined(_NOMPI) || !defined (ADIOS_EMPTY_TRANSPORTS)
     FORWARD_DECLARE(mpi)
     FORWARD_DECLARE(mpi_lustre)
     //FORWARD_DECLARE(mpi_cio)
     //FORWARD_DECLARE(mpi_stripe)
     //FORWARD_DECLARE(mpi_stagger)
     //FORWARD_DECLARE(mpi_aggregate)
     FORWARD_DECLARE(mpi_amr)
#if HAVE_BGQ
     FORWARD_DECLARE(mpi_bgq)
#endif
     //FORWARD_DECLARE(mpi_amr1)
     FORWARD_DECLARE(phdf5)
     FORWARD_DECLARE(nc4)
     FORWARD_DECLARE(nssi)
     FORWARD_DECLARE(nssi_filter)
     FORWARD_DECLARE(flexpath)
     FORWARD_DECLARE(var_merge)
#endif

#ifdef ADIOS_EMPTY_TRANSPORTS
     //FORWARD_DECLARE_EMPTY(datatap)
     FORWARD_DECLARE_EMPTY(posix)
     FORWARD_DECLARE_EMPTY(posix1)
     //FORWARD_DECLARE_EMPTY(provenance)
     //FORWARD_DECLARE_EMPTY(adaptive)
#else
     FORWARD_DECLARE(datatap)
     FORWARD_DECLARE(posix)
     FORWARD_DECLARE(posix1)
     FORWARD_DECLARE(provenance)
     FORWARD_DECLARE(adaptive)
#endif

#if defined(HAVE_DATASPACES) && !defined(ADIOS_EMPTY_TRANSPORTS) 
FORWARD_DECLARE(dataspaces)
#endif

#if defined(HAVE_DIMES) && !defined(ADIOS_EMPTY_TRANSPORTS) 
FORWARD_DECLARE(dimes)
#endif

#if defined(HAVE_ICEE) && !defined(ADIOS_EMPTY_TRANSPORTS) 
FORWARD_DECLARE(icee)
#endif

#undef FORWARD_DECLARE
#endif
