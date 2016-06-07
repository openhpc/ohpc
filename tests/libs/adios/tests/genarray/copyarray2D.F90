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
!  nx * ny     processes write a 2D array, where each process writes an
!  ndx * ndy   piece with filling with some values as integer (4 bytes) value
!
!  Block written from rank R:
!      R*ndx        ... R*ndx
!      R*ndx+1      ... R*ndx+1
!      ...          ...
!      R*ndx+ndy-1      R*ndx+ndy-1
!
! E.g 2x2 processes writing 2x2 blocks:
!     0 0 | 2 2        0 1 | 2 3
!     1 1 | 3 3        4 5 | 6 7
!     ----+----   or  -----+----
!     4 4 | 6 6        8 9 | 10 11
!     5 5 | 7 7       12 13| 14 15
!
! (c) Oak Ridge National Laboratory, 2009
! Author: Norbert Podhorszki
!
module copyarray2D_comm
    ! arguments
    character(len=256) :: outputfile, inputfile
    integer :: npx, npy    ! # of processors in x-y direction
    integer :: ndx, ndy    ! size of array per processor
    integer :: timesteps   ! number of timesteps to write
    logical :: common_size ! .true.  if common local sizes are given as argument
                           ! .false. if we have to read sizes from a file

    integer :: gndx, gndy  ! size of the global array
    integer :: posx, posy  ! position index in the array
    integer :: offx, offy  ! offsets of local array in the global array

    integer, dimension(:,:), allocatable :: int_xy

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

end module copyarray2D_comm


program genarray
    use copyarray2D_comm
    use adios_write_mod
    implicit none
    include 'mpif.h'

    call MPI_Init (ierr)
    call MPI_Comm_dup (MPI_COMM_WORLD, group_comm, ierr)
    call MPI_Comm_rank (MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size (group_comm, nproc , ierr)

    call adios_init ("genarray.xml", group_comm, ierr)
    !call MPI_Barrier (group_comm, ierr)

    call processArgs()
    if (rank == 0) then
        print *,"Output file: "//trim(outputfile)
        print '(" Process number        : ",i0," x ",i0)', npx,npy
        if (common_size) then
            print '(" Array size per process: ",i0," x ",i0)', ndx,ndy
        else
            print *," Array sizes per processes taken from file: "//trim(inputfile)
        endif

        if (nproc .ne. npx*npy) then
            print '(" Error: Number of processors ",i0,"does not match ndx*ndy=",i0)', nproc, npx*npy
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
    use copyarray2D_comm
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
    use copyarray2D_comm
    implicit none
    if (common_size) then
        gndx = npx * ndx
        gndy = npy * ndy
    else
       ! have to read from file
       print *, "To be implemented: read sizes from file 2"
       call exit(2)
    endif
end subroutine determineGlobalSize

!!***************************
subroutine determineOffsets()
    use copyarray2D_comm
    implicit none
    if (common_size) then
        posx = mod(rank, npx)     ! 1st dim easy: 0, npx, 2npx... are in the same X position
        posy = rank/npx           ! 2nd dim: npx processes belong into one dim
        offx = posx * ndx
        offy = posy * ndy
!        print '("rank=",i0," pos: ",i0,",",i0," offset: ",i0,",",i0)',  &
!                rank, posx, posy, offx, offy
    else
       ! have to read from file
       print *, "To be implemented: read sizes from file 2"
       call exit(2)
    endif
end subroutine determineOffsets


!!***************************
subroutine generateLocalArray()
    use copyarray2D_comm
    implicit none
    integer :: i,j,k, startv
    allocate( int_xy(1:ndx, 1:ndy) )
!    do j=1,ndy
!        startv = (offy+j-1)*gndx + offx
!        do i=1,ndx
!            !int_xy(i,j) =  startv+i-1
!            !int_xy(i,j) = rank*ndx+j-1
!            int_xy(i,j) = rank
!        enddo
!    enddo
end subroutine generateLocalArray


!!***************************
subroutine readArray()
    use copyarray2D_comm
    use adios_write_mod
    use adios_read_mod
    implicit none
    integer :: tstep
    character(2) :: mode = "r"
    include 'mpif.h'

    ! Write out data using ADIOS
    group = "genarray"
    !  calculate how much we write from this processor
    group_size = 4 * 9               + &  ! X,Y, nproc, all size_ and offs_ integers
                 4 * ndx * ndy       + &  ! int_xy
                 4 * ndx * ndy            ! int_xyt

    adios_totalsize = group_size

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    cache_start_time = MPI_WTIME()

    call adios_open (handle, group, inputfile, mode, group_comm, err)
    call adios_group_size (handle, group_size, total_size, err)
    !print '("rank=",i0," total_size=",i0," err=",i0)', rank, total_size, err

    ! write dimensions and nproc
    adios_buf_size = 4
    call adios_read (handle, "X", gndx, adios_buf_size, err)
    adios_buf_size = 4
    call adios_read (handle, "Y", gndy, adios_buf_size, err)
    adios_buf_size = 4
    call adios_read (handle, "npx", npx, adios_buf_size, err)
    adios_buf_size = 4
    call adios_read (handle, "npy", npy, adios_buf_size, err)
    adios_buf_size = 4
    call adios_read (handle, "nproc", nproc, adios_buf_size, err)

    adios_buf_size = 4
    call adios_read (handle, "size_x", ndx, adios_buf_size, err)
    adios_buf_size = 4
    call adios_read (handle, "size_y", ndy, adios_buf_size, err)
    adios_buf_size = 4
    call adios_read (handle, "offs_x", offx, adios_buf_size, err)
    adios_buf_size = 4
    call adios_read (handle, "offs_y", offy, adios_buf_size, err)

    adios_buf_size = 4 * ndx * ndy
    call adios_read (handle, "int_xy", int_xy, adios_buf_size, err)
    adios_buf_size = 4 * ndx * ndy
    call adios_read (handle, "int_xyt", int_xy, adios_buf_size, err)

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    cache_end_time = MPI_WTIME()
    cache_total_time = cache_end_time - cache_start_time

    sz = adios_totalsize * nproc/1024.d0/1024.d0/1024.d0 !size in GB
    gbs = sz/cache_total_time

    !if (rank==0) write(6,*) total_time
    if (rank==0) print '("Reading: ",a10,d12.2,2x,d12.2,2x,d12.3)', outputfile,sz,cache_total_time,gbs

    ! start streaming from buffer to disk
    call adios_close (handle, err)
    print '("rank=",i0,": write completed")', rank
end subroutine readArray

!!***************************
subroutine writeArray()
    use copyarray2D_comm
    use adios_write_mod
    implicit none
    integer :: tstep
    character(2) :: mode = "w"
    include 'mpif.h'

    ! Write out data using ADIOS
    group = "genarray"
    !  calculate how much we write from this processor
    group_size = 4 * 9               + &  ! X,Y, nproc, all size_ and offs_ integers
                 4 * ndx * ndy       + &  ! int_xy
                 4 * ndx * ndy            ! int_xyt

    adios_totalsize = group_size

    do tstep=1,timesteps
        if (tstep > 1) mode = "a"
        !print '("rank=",i0," group=",A," file=",A," group_size=",i0)', rank, trim(group), &
        !    trim(outputfile), group_size

        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        cache_start_time = MPI_WTIME()

        call adios_open (handle, group, outputfile, mode, group_comm, err)
        call adios_group_size (handle, group_size, total_size, err)
        !print '("rank=",i0," total_size=",i0," err=",i0)', rank, total_size, err

        ! write dimensions and nproc
        call adios_write (handle, "X", gndx, err)
        call adios_write (handle, "Y", gndy, err)
        call adios_write (handle, "npx", npx, err)
        call adios_write (handle, "npy", npy, err)
        call adios_write (handle, "nproc", nproc, err)

        call adios_write (handle, "size_x", ndx, err)
        call adios_write (handle, "size_y", ndy, err)
        call adios_write (handle, "offs_x", offx, err)
        call adios_write (handle, "offs_y", offy, err)

        if (tstep == 1) then
            call adios_write (handle, "int_xy", int_xy, err)
        endif
        call adios_write (handle, "int_xyt", int_xy, err)

        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        cache_end_time = MPI_WTIME()
        cache_total_time = cache_end_time - cache_start_time

        sz = adios_totalsize * nproc/1024.d0/1024.d0/1024.d0 !size in GB
        gbs = sz/cache_total_time

        !if (rank==0) write(6,*) total_time
        if (rank==0) print '("Writing: ",a10,d12.2,2x,d12.2,2x,d12.3)', outputfile,sz,cache_total_time,gbs

        ! start streaming from buffer to disk
        call adios_close (handle, err)
!        print '("rank=",i0,": write completed")', rank
    enddo
end subroutine writeArray

!!***************************
subroutine usage()
    print *, "Usage: genarray input output N  M [nx  ny | infile] timesteps"
    print *, "input:  name of input file"
    print *, "output: name of output file"
    print *, "N:      number of processes in X dimension"
    print *, "M:      number of processes in Y dimension"
    print *, "nx:     local array size in X dimension per processor"
    print *, "ny:     local array size in Y dimension per processor"
    print *, "timesteps: number of time steps to write"
end subroutine usage

!!***************************
subroutine processArgs()
    use copyarray2D_comm

#ifndef __GFORTRAN__
#ifndef __GNUC__
    interface
         integer function iargc()
         end function iargc
    end interface
#endif
#endif

    character(len=256) :: npx_str, npy_str, ndx_str, ndy_str, time_str
    integer :: numargs

    !! process arguments
    numargs = iargc()
    !print *,"Number of arguments:",numargs
    if ( numargs < 7 ) then
        call usage()
        call exit(1)
    endif
    call getarg(1, inputfile)
    call getarg(2, outputfile)
    call getarg(3, npx_str)
    call getarg(4, npy_str)
    call getarg(5, ndx_str)
    call getarg(6, ndy_str)
    call getarg(7, time_str)
    read (npx_str,'(i5)') npx
    read (npy_str,'(i5)') npy
    read (ndx_str,'(i6)') ndx
    read (ndy_str,'(i6)') ndy
    read (time_str,'(i6)') timesteps
    common_size = .true.

end subroutine processArgs
