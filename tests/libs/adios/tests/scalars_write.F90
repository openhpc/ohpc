!  
!  ADIOS is freely available under the terms of the BSD license described
!  in the COPYING file in the top level directory of this source distribution.
!
!  Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
!

!/*************************************************************/
!/*   Example of writing various types of variable in ADIOS   */
!/*************************************************************/
program scalars
    use adios_write_mod
    implicit none
    include 'mpif.h'

    character(len=25)   :: filename = "scalars.bp"
    integer             :: rank, size, i, ierr
    integer             :: comm

    ! ADIOS variables declarations for matching gwrite_scalars.fh 
    integer                 :: adios_err
    integer*8               :: adios_groupsize, adios_totalsize
    integer*8               :: adios_handle

    ! scalar variables to write out (including a string)
    integer*1           :: v1 = -4
    integer*2           :: v2 = -3
    integer*4           :: v3 = -2
    integer*8           :: v4 = -1

    integer*1           :: v5 = 1
    integer*2           :: v6 = 2
    integer*4           :: v7 = 3
    integer*8           :: v8 = 4

    real*4              :: v9 = 5.0
    real*8              :: v10 = 6.0

    character(len=20)   :: v11 = "ADIOS example"

    complex*8           :: v12 = (8.0, 9.0)
    complex*16          :: v13 = (10.0, 11.0)

    call MPI_Init (ierr)
    call MPI_Comm_dup (MPI_COMM_WORLD, comm, ierr)
    call MPI_Comm_rank (comm, rank, ierr)
    call MPI_Comm_size (comm, size, ierr);

    call adios_init ("scalars.xml", comm, adios_err);
    ! adios_open() opens a 'group in a file', here the 'scalars' group
    call adios_open (adios_handle, "scalars", filename, "w", comm, adios_err);
#include "gwrite_scalars.fh"
    call adios_close (adios_handle, adios_err)

    call MPI_Barrier (comm, ierr);

    call adios_finalize (rank, adios_err);

    call MPI_Finalize (ierr);

end program

