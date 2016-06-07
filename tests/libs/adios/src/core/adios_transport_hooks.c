/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include "config.h"
#include "string.h" // strdup

#ifdef _INTERNAL
    /* Sequential processes can use the library compiled with -D_NOMPI */
#   define ADIOS_EMPTY_TRANSPORTS
#else
    /* Parallel applications should use MPI to communicate file info and slices of data */
#endif

#include "adios.h"
#include "core/adios_transport_hooks.h"
#include "core/adios_bp_v1.h"
#include "core/adios_internals.h"

#define MATCH_STRING_TO_METHOD(b,d,r) \
if (!strcasecmp (buf,b)) \
{*method=d;*requires_group_comm=r;return 1;}

#define ASSIGN_FNS(a,b,name) \
(*t) [b].method_name = strdup(name); \
(*t) [b].adios_init_fn = adios_##a##_init; \
(*t) [b].adios_open_fn = adios_##a##_open; \
(*t) [b].adios_should_buffer_fn = adios_##a##_should_buffer; \
(*t) [b].adios_write_fn = adios_##a##_write; \
(*t) [b].adios_get_write_buffer_fn = adios_##a##_get_write_buffer; \
(*t) [b].adios_read_fn = adios_##a##_read; \
(*t) [b].adios_close_fn = adios_##a##_close; \
(*t) [b].adios_finalize_fn = adios_##a##_finalize; \
(*t) [b].adios_end_iteration_fn = adios_##a##_end_iteration; \
(*t) [b].adios_start_calculation_fn = adios_##a##_start_calculation; \
(*t) [b].adios_stop_calculation_fn = adios_##a##_stop_calculation;

void adios_free_transports (struct adios_transport_struct * t)
{
    int i;
    for (i=0; i<ADIOS_METHOD_COUNT; i++) {
        if (t[i].method_name) {
            free (t[i].method_name);
            t[i].method_name = 0;
        }
    }
}

void adios_init_transports (struct adios_transport_struct ** t)
{
    *t = (struct adios_transport_struct *)
           calloc (ADIOS_METHOD_COUNT, sizeof (struct adios_transport_struct));

#ifndef ADIOS_EMPTY_TRANSPORTS

#  ifndef _NOMPI

#    if HAVE_MPI
    ASSIGN_FNS(mpi,ADIOS_METHOD_MPI,"MPI")
    ASSIGN_FNS(mpi_lustre,ADIOS_METHOD_MPI_LUSTRE,"MPI_LUSTRE")
    ASSIGN_FNS(mpi_amr,ADIOS_METHOD_MPI_AMR,"MPI_AGGREGATE")
#    if HAVE_BGQ
    ASSIGN_FNS(mpi_bgq,ADIOS_METHOD_MPI_BGQ,"MPI_BGQ")
#    endif
    //Tian's method
    ASSIGN_FNS(var_merge,ADIOS_METHOD_VAR_MERGE,"VAR_MERGE")
#      ifndef NO_RESEARCH_TRANSPORTS
    //ASSIGN_FNS(mpi_stripe,ADIOS_METHOD_MPI_STRIPE)
    //ASSIGN_FNS(mpi_cio,ADIOS_METHOD_MPI_CIO)
    //ASSIGN_FNS(mpi_stagger,ADIOS_METHOD_MPI_STAGGER)
    //ASSIGN_FNS(mpi_aggregate,ADIOS_METHOD_MPI_AGG)
    //ASSIGN_FNS(mpi_amr1,ADIOS_METHOD_MPI_AMR1)
    //ASSIGN_FNS(adaptive,ADIOS_METHOD_ADAPTIVE)
#      endif
#    endif

#    if HAVE_PHDF5
    ASSIGN_FNS(phdf5,ADIOS_METHOD_PHDF5,"PHDF5")
#    endif

#    if HAVE_NC4PAR
    ASSIGN_FNS(nc4,ADIOS_METHOD_NC4,"NC4")
#    endif

#    if HAVE_NSSI
    ASSIGN_FNS(nssi,ADIOS_METHOD_NSSI_STAGING,"NSSI")
    ASSIGN_FNS(nssi_filter,ADIOS_METHOD_NSSI_FILTER,"NSSI_FILTER")
#    endif

#  endif /* _NOMPI */

#  if HAVE_DATATAP
    ASSIGN_FNS(datatap,ADIOS_METHOD_DATATAP,"DATATAP")
#  endif

# if HAVE_FLEXPATH
    ASSIGN_FNS(flexpath,ADIOS_METHOD_FLEXPATH,"FLEXPATH")
# endif
# if HAVE_ICEE
    ASSIGN_FNS(icee,ADIOS_METHOD_ICEE,"ICEE")
# endif

    ASSIGN_FNS(posix,ADIOS_METHOD_POSIX,"POSIX")
    ASSIGN_FNS(posix1,ADIOS_METHOD_POSIX1,"POSIX1")

#  if HAVE_DATASPACES
    ASSIGN_FNS(dataspaces,ADIOS_METHOD_DATASPACES,"DATASPACES")
#  endif

#  if HAVE_DIMES
    ASSIGN_FNS(dimes,ADIOS_METHOD_DIMES,"DIMES")
#  endif

#  ifndef NO_RESEARCH_TRANSPORTS
    //ASSIGN_FNS(provenance,ADIOS_METHOD_PROVENANCE)
#  endif

#endif /* ADIOS_EMPTY_TRANSPORTS */
    }

int adios_parse_method (const char * buf, enum ADIOS_IO_METHOD * method
                       ,int * requires_group_comm
                       )
{
    // add the string<->ID mapping here (also add ID in adios_internals.h)
    // use a '1' for requires a communicator or '0' if not as the last param
#if HAVE_MPI
    MATCH_STRING_TO_METHOD("MPI",ADIOS_METHOD_MPI,1)
    MATCH_STRING_TO_METHOD("MPI_LUSTRE",ADIOS_METHOD_MPI_LUSTRE,1)
    MATCH_STRING_TO_METHOD("MPI_AMR",ADIOS_METHOD_MPI_AMR,1)
#if HAVE_BGQ
    MATCH_STRING_TO_METHOD("MPI_BGQ",ADIOS_METHOD_MPI_BGQ,1)
#endif
    // Tian's method
    MATCH_STRING_TO_METHOD("VAR_MERGE",ADIOS_METHOD_VAR_MERGE,1)

    MATCH_STRING_TO_METHOD("MPI_AGGREGATE",ADIOS_METHOD_MPI_AMR,1)
#ifndef NO_RESEARCH_TRANSPORTS
    MATCH_STRING_TO_METHOD("MPI_STRIPE",ADIOS_METHOD_MPI_STRIPE,1)
    MATCH_STRING_TO_METHOD("MPI_CIO",ADIOS_METHOD_MPI_CIO,1)
    MATCH_STRING_TO_METHOD("MPI_STAGGER",ADIOS_METHOD_MPI_STAGGER,1)
    MATCH_STRING_TO_METHOD("MPI_AGGREGATE1",ADIOS_METHOD_MPI_AGG,1)
    MATCH_STRING_TO_METHOD("MPI_AMR1",ADIOS_METHOD_MPI_AMR1,1)
    MATCH_STRING_TO_METHOD("ADAPTIVE",ADIOS_METHOD_ADAPTIVE,1)
#endif
#endif

#if NO_DATATAP == 0
    MATCH_STRING_TO_METHOD("DATATAP",ADIOS_METHOD_DATATAP,0)
    MATCH_STRING_TO_METHOD("PBIO",ADIOS_METHOD_DATATAP,0)
#endif

    MATCH_STRING_TO_METHOD("POSIX",ADIOS_METHOD_POSIX,0)
    MATCH_STRING_TO_METHOD("POSIX1",ADIOS_METHOD_POSIX1,0)
    MATCH_STRING_TO_METHOD("FB",ADIOS_METHOD_POSIX,0)

#if HAVE_DATASPACES
    MATCH_STRING_TO_METHOD("DART",ADIOS_METHOD_DATASPACES,1)
    MATCH_STRING_TO_METHOD("DATASPACES",ADIOS_METHOD_DATASPACES,1)
#endif

#if HAVE_DIMES
    MATCH_STRING_TO_METHOD("DIMES",ADIOS_METHOD_DIMES,1)
#endif

#if HAVE_PHDF5
    MATCH_STRING_TO_METHOD("PHDF5",ADIOS_METHOD_PHDF5,1)
#endif

#if HAVE_NC4PAR
    MATCH_STRING_TO_METHOD("NC4",ADIOS_METHOD_NC4,1)
#endif

#if HAVE_FLEXPATH
    MATCH_STRING_TO_METHOD("FLEXPATH",ADIOS_METHOD_FLEXPATH,0)
#endif
#if HAVE_ICEE
    MATCH_STRING_TO_METHOD("ICEE",ADIOS_METHOD_ICEE,0)
#endif

#if HAVE_NSSI
    MATCH_STRING_TO_METHOD("NSSI",ADIOS_METHOD_NSSI_STAGING,1)
    MATCH_STRING_TO_METHOD("NSSI_FILTER",ADIOS_METHOD_NSSI_FILTER,1)
#endif

#ifndef NO_RESEARCH_TRANSPORTS
    MATCH_STRING_TO_METHOD("PROVENANCE",ADIOS_METHOD_PROVENANCE,1)
#endif

    MATCH_STRING_TO_METHOD("NULL",ADIOS_METHOD_NULL,0)

    *method = ADIOS_METHOD_UNKNOWN;
    *requires_group_comm = 0;

    return 0;
}
