!  
!  ADIOS is freely available under the terms of the BSD license described
!  in the COPYING file in the top level directory of this source distribution.
!
!  Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
!

!/*************************************************************/
!/*      Test of writing of simple arrays in ADIOS with POSIX method         */
!/*                                                           */
!/*     Similar example is examples/Fortran/arrays/arrays_write.c     */
!/*************************************************************/
program posix_method
    use adios_write_mod
    implicit none
    include 'mpif.h'

    character(len=30)   :: filename = "posix_method.bp"
    integer             :: i, j, ierr
    integer             :: rank, size, comm

    ! ADIOS variables declarations for matching gwrite_temperature.fh 
    integer                 :: adios_err
    integer*8               :: adios_groupsize, adios_totalsize
    integer*8               :: adios_handle, adios_buf_size

    ! variables to write out 
    integer, parameter      :: NX = 10, NY = 100
    integer                 :: NXin, NYin
    real*8                  :: t(NX,NY), tin(NX,NY)
    integer                 :: p(NX), pin(NX)

    call MPI_Init (ierr)
    call MPI_Comm_dup (MPI_COMM_WORLD, comm, ierr)
    call MPI_Comm_rank (comm, rank, ierr)
    call MPI_Comm_size (comm, size, ierr)

    do j = 1,NY 
        do i = 1,NX
            t(i,j) = rank*NX*NY + (j-1)*NX + i-1
        enddo     
    enddo

    do i = 1,NX
        p(i) = rank*NX + i
    enddo

    call adios_init ("posix_method.xml", comm, adios_err);


    ! Write data out
    call adios_open (adios_handle, "posix_method", filename, "w", comm, adios_err);
#include "gwrite_posix_method.fh"
    call adios_close (adios_handle, adios_err)


    ! Read data in
    call adios_open (adios_handle, "posix_method", filename, "r", comm, adios_err);

    ! First read in the scalars to calculate the size of the arrays
    adios_buf_size = 4
    call adios_read (adios_handle, "NX", NXin, adios_buf_size, adios_err)
    adios_buf_size = 4
    call adios_read (adios_handle, "NY", NYin, adios_buf_size, adios_err)
    call adios_close (adios_handle, adios_err)

    if (NX /= NXin) then
        write (0, '("Error: NX=",i0," read from file != NX=",i0," written to file.")') NXin, NX
        call exit(1)
    endif
    if (NY /= NYin) then
        write (0, '("Error: NY=",i0," read from file != NY=",i0," written to file.")') NYin, NY
        call exit(1)
    endif

    ! Read the arrays
    call adios_open (adios_handle, "posix_method", filename, "r", comm, adios_err);
    call adios_group_size (adios_handle, adios_groupsize, adios_totalsize, adios_err)
    adios_buf_size = 8 * (NX) * (NY)
    call adios_read (adios_handle, "var_double_2Darray", tin, adios_buf_size, adios_err)
    adios_buf_size = 4 * (NX)
    call adios_read (adios_handle, "var_int_1Darray", pin, adios_buf_size, adios_err)
    call adios_close (adios_handle, adios_err)

    
    ! Check input against output
    do i = 1,NX
        if (p(i)-pin(i) .ge. 0.000001) then
            write (0,'("Error: p read from file != p written to file.")')
            call exit(1)
        endif
    enddo
    do j = 1,NY 
        do i = 1,NX
            if (t(i,j)-tin(i,j) .ge. 0.000001 ) then
                write (0,'("Error: t read from file != t written to file.")')
                call exit(1)
            endif
        enddo     
    enddo

    call adios_finalize (0, adios_err);
    call MPI_Finalize (ierr);

end program

