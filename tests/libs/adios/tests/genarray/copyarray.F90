!
!  ADIOS is freely available under the terms of the BSD license described
!  in the COPYING file in the top level directory of this source distribution.
!
!  Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
!

!
!  COPYARRAY
!
!  Write an ADIOS BP file from many processor for test purposes.
!
!  nx * ny * nz     processes write a 3D array, where each process writes an
!  ndx * ndy * ndz  piece with filling with its rank as integer (4 bytes) value
!
!
! (c) Oak Ridge National Laboratory, 2009
! Author: Norbert Podhorszki
!
module copyarray_comm
    ! arguments
    character(len=256) :: outputfile, inputfile
    integer :: npx, npy, npz  ! # of processors in x-y-z direction
    integer :: ndx, ndy, ndz  ! size of array per processor
    logical :: common_size    ! .true.  if common local sizes are given as argument
                              ! .false. if we have to read sizes from a file

    integer :: gndx, gndy, gndz  ! size of the global array
    integer :: offx,offy,offz    ! offsets of local array in the global array

    real*8, dimension(:,:,:), allocatable :: double_xyz

    ! MPI variables
    integer :: group_comm
    integer :: rank, nproc
    integer :: ierr

    ! ADIOS variables
    character (len=200) :: group
    character (len=200) :: filename
    !character (len=6)   :: nprocstr
    integer*8 :: handle, total_size, group_size, adios_totalsize, adios_buf_size
    integer   :: err

    real*8 :: start_time, end_time, total_time,gbs,sz
    real*8 :: cache_start_time, cache_end_time, cache_total_time


end module copyarray_comm


program genarray
    use copyarray_comm
    use adios_write_mod
    implicit none
    include 'mpif.h'

    call MPI_Init (ierr)
    call MPI_Comm_dup (MPI_COMM_WORLD, group_comm, ierr)
    call MPI_Comm_rank (MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size (group_comm, nproc , ierr)

    call adios_init ("genarray3d.xml", group_comm, ierr)
    !call MPI_Barrier (group_comm, ierr)

    call processArgs()
    if (rank == 0) then
        print *,"Input file:  "//trim(inputfile)
        print *,"Output file: "//trim(outputfile)
        print '(" Process number        : ",i0," x ",i0," x ",i0)', npx,npy,npz
        if (common_size) then
            print '(" Array size per process: ",i0," x ",i0," x ",i0)', ndx,ndy,ndz
        else
            print *," Array sizes per processes taken from file: "//trim(inputfile)
        endif

        if (nproc .ne. npx*npy*npz) then
            print '(" Error: Number of processors ",i0,"does not match ndx*ndy*ndz=",i0)', nproc, npx*npy*npz
            call exit(1)
        endif
    endif

    !write (*,*) 'rank ', rank, "init completed"
    !write (nprocstr,'(i0)') nproc

    call determineLocalSize()
    call determineGlobalSize()
    call determineOffsets()
    call generateLocalArray()

    call MPI_BARRIER(MPI_COMM_WORLD,err)
    start_time = MPI_WTIME()
    call readArray()
    call MPI_BARRIER(MPI_COMM_WORLD,err)
    end_time = MPI_WTIME()
    total_time = end_time - start_time

    sz = adios_totalsize * nproc/1024.d0/1024.d0/1024.d0 !size in GB
    gbs = sz/total_time

    !if (rank==0) write(6,*) total_time
    if (rank==0) write(6,'(a10,d12.2,2x,d12.2,2x,d12.3)') outputfile,sz,total_time,gbs

    call MPI_BARRIER(MPI_COMM_WORLD,err)
    start_time = MPI_WTIME()
    call writeArray()
    call MPI_BARRIER(MPI_COMM_WORLD,err)
    end_time = MPI_WTIME()
    total_time = end_time - start_time

    sz = adios_totalsize * nproc/1024.d0/1024.d0/1024.d0 !size in GB
    gbs = sz/total_time

    !if (rank==0) write(6,*) total_time
    if (rank==0) write(6,'(a10,d12.2,2x,d12.2,2x,d12.3)') outputfile,sz,total_time,gbs

    ! Terminate
    call MPI_Barrier (MPI_COMM_WORLD, ierr)
    call adios_finalize (rank, ierr)
    call MPI_Finalize (ierr)
end program genarray


!!***************************
subroutine determineLocalSize()
    use copyarray_comm
    implicit none
    if (common_size) then
       ! we are done since we know them from argument
    else
       ! have to read from file
       print *, "To be implemented: read sizes from file 1"
       call exit(2)
    endif
end subroutine determineLocalSize

!!***************************
subroutine determineGlobalSize()
    use copyarray_comm
    implicit none
    if (common_size) then
        gndx = npx * ndx
        gndy = npy * ndy
        gndz = npz * ndz
    else
       ! have to read from file
       print *, "To be implemented: read sizes from file 2"
       call exit(2)
    endif
end subroutine determineGlobalSize

!!***************************
subroutine determineOffsets()
    use copyarray_comm
    implicit none
    integer :: posx, posy, posz ! position index in the array
    if (common_size) then
        posx = mod(rank, npx)     ! 1st dim easy: 0, npx, 2npx... are in the same X position
        posy = mod(rank/npx, npy) ! 2nd dim: (0, npx-1) have the same dim (so divide with npx first)
        posz = rank/(npx*npy)     ! 3rd dim: npx*npy processes belong into one dim
        offx = posx * ndx
        offy = posy * ndy
        offz = posz * ndz
    else
       ! have to read from file
       print *, "To be implemented: read sizes from file 3"
       call exit(2)
    endif
end subroutine determineOffsets


!!***************************
subroutine generateLocalArray()
    use copyarray_comm
    implicit none
    integer :: i,j,k
    allocate( double_xyz(1:ndx, 1:ndy, 1:ndz) )
!    do k=1,ndz
!        do j=1,ndy
!            do i=1,ndx
!                double_xyz(i,j,k) = 1.0d0*rank
!            enddo
!        enddo
!    enddo
end subroutine generateLocalArray


!!***************************
subroutine readArray()
    use copyarray_comm
    use adios_write_mod
    use adios_read_mod
    implicit none
    integer*8 adios_handle, adios_groupsize
    integer adios_err
    integer*8 fp, s
    include 'mpif.h'

    call MPI_BARRIER(MPI_COMM_WORLD,adios_err)
    cache_start_time = MPI_WTIME()

    group = "genarray"

! TODO: Fix this to use the new read API

    call adios_open (adios_handle, group, inputfile, "r", group_comm, adios_err)
#include "gread_genarray.fh"
    call MPI_BARRIER(MPI_COMM_WORLD,adios_err)
    cache_end_time = MPI_WTIME()
    cache_total_time = cache_end_time - cache_start_time

    sz = adios_totalsize * nproc/1024.d0/1024.d0/1024.d0 !size in GB
    gbs = sz/cache_total_time

    !if (rank==0) write(6,*) total_time
    if (rank==0) print '("Reading: ",a10,d12.2,2x,d12.2,2x,d12.3)', inputfile,sz,cache_total_time,gbs

    call adios_close (adios_handle, adios_err)


end subroutine readArray

!!***************************
subroutine writeArray()
    use copyarray_comm
    use adios_write_mod
    implicit none
    integer*8 adios_handle, adios_groupsize
    integer adios_err
    include 'mpif.h'

    call MPI_BARRIER(MPI_COMM_WORLD,adios_err)
    cache_start_time = MPI_WTIME()

    group = "genarray"
    call adios_open (adios_handle, group, outputfile, "w", group_comm, adios_err)
#include "gwrite_genarray.fh"
    call MPI_BARRIER(MPI_COMM_WORLD,adios_err)
    cache_end_time = MPI_WTIME()
    cache_total_time = cache_end_time - cache_start_time

    sz = adios_totalsize * nproc/1024.d0/1024.d0/1024.d0 !size in GB
    gbs = sz/cache_total_time

    !if (rank==0) write(6,*) total_time
    if (rank==0) print '("Writing: ",a10,d12.2,2x,d12.2,2x,d12.3)', outputfile,sz,cache_total_time,gbs

    call adios_start_calculation(adios_err)
    call adios_end_iteration(adios_err)
    call adios_stop_calculation(adios_err)

    call adios_close (adios_handle, adios_err)


end subroutine writeArray


!!***************************
subroutine usage()
    print *, "Usage: genarray  input output N  M  K nx  ny  nz"
    print *, "infile: name of input file (file to be copied)"
    print *, "output: name of output file"
    print *, "N:      number of processes in X dimension"
    print *, "M:      number of processes in Y dimension"
    print *, "K:      number of processes in Z dimension"
    print *, "nx:     local array size in X dimension per processor"
    print *, "ny:     local array size in Y dimension per processor"
    print *, "nz:     local array size in Z dimension per processor"
end subroutine usage

!!***************************
subroutine processArgs()
    use copyarray_comm

#ifndef __GFORTRAN__
#ifndef __GNUC__
    interface
         integer function iargc()
         end function iargc
    end interface
#endif
#endif

    character(len=256) :: npx_str, npy_str, npz_str, ndx_str, ndy_str, ndz_str
    integer :: numargs

    !! process arguments
    numargs = iargc()
    !print *,"Number of arguments:",numargs
    if ( numargs < 8 ) then
        call usage()
        call exit(1)
    endif
    call getarg(1, inputfile)
    call getarg(2, outputfile)
    call getarg(3, npx_str)
    call getarg(4, npy_str)
    call getarg(5, npz_str)
    call getarg(6, ndx_str)
    call getarg(7, ndy_str)
    call getarg(8, ndz_str)
    read (npx_str,'(i5)') npx
    read (npy_str,'(i5)') npy
    read (npz_str,'(i5)') npz
    read (ndx_str,'(i6)') ndx
    read (ndy_str,'(i6)') ndy
    read (ndz_str,'(i6)') ndz
    common_size = .true.

end subroutine processArgs
