!
!   Copyright (C) 2013, Northwestern University and Argonne National Laboratory
!   See COPYRIGHT notice in top-level directory.
!
! $Id: freeform.f 2257 2015-12-22 04:50:00Z wkliao $

!
! This program tests whether header file pnetcdf.inc conforms Fortran free form
! for compilation only, not execution
!

program main
    implicit none
    include "mpif.h"
    include "pnetcdf.inc"

    character(LEN=80) pnetcdf_version
    integer err

    call MPI_Init(err)

    pnetcdf_version = nfmpi_inq_libvers()

    call MPI_Finalize(err)

end program main

