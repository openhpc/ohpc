
!  ADIOS is freely available under the terms of the BSD license described
!  in the COPYING file in the top level directory of this source distribution.
!
!  Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
!

!
!  GENARRAY
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
module genarray_comm
    ! arguments
    character(len=256) :: outputfile, inputfile
    integer :: npx, npy, npz  ! # of processors in x-y-z direction
    integer :: ndx, ndy, ndz  ! size of array per processor
    integer :: timesteps      ! number of timesteps to write
    integer :: sleeptime      ! time to sleep between time steps
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
    integer*8 :: handle, total_size, group_size, adios_totalsize

    real*8 :: start_time, end_time, total_time,gbs,sz
    real*8 :: io_start_time, io_end_time, io_total_time


end module genarray_comm


program genarray
    use genarray_comm
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
        print *,"Output file(s): "//trim(outputfile)//".<step>.bp"
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

    call writeArray()
    ! Terminate
    call MPI_Barrier (MPI_COMM_WORLD, ierr)
    call adios_finalize (rank, ierr)
    call MPI_Finalize (ierr)
end program genarray


!!***************************
subroutine determineLocalSize()
    use genarray_comm
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
    use genarray_comm
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
    use genarray_comm
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
    use genarray_comm
    implicit none
    integer :: i,j,k
    allocate( double_xyz(1:ndx, 1:ndy, 1:ndz) )
    do k=1,ndz
        do j=1,ndy
            do i=1,ndx
                double_xyz(i,j,k) = 1.0d0*rank
            enddo
        enddo
    enddo
end subroutine generateLocalArray


!!***************************
subroutine writeArray()
    use genarray_comm
    use adios_write_mod
    implicit none
    integer*8 adios_handle, adios_groupsize
    integer adios_err
    integer :: tstep
    character(2) :: mode = "w"
    character(len=256) :: outfilename
    include 'mpif.h'


    if (rank==0) print '("Writing: "," filename ",14x,"size(GB)",4x,"io_time(sec)",6x,"GB/s")'
    do tstep=1,timesteps
        !if (tstep > 1) mode = "a"
        double_xyz = tstep + double_xyz
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        io_start_time = MPI_WTIME()
        group = "genarray"
        write (outfilename,'(a,".",i3.3,".bp")') trim(outputfile),tstep
        call adios_open (adios_handle, group, outfilename, mode, group_comm, adios_err)
#include "gwrite_genarray.fh"
        call adios_close (adios_handle, adios_err)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        io_end_time = MPI_WTIME()
        io_total_time = io_end_time - io_start_time
        sz = adios_totalsize * nproc/1024.d0/1024.d0/1024.d0 !size in GB
        gbs = sz/io_total_time
        if (rank==0) print '("Writing: ",a20,d12.2,2x,d12.2,2x,d12.3)', outfilename,sz,io_total_time,gbs
        if (tstep<timesteps) call sleep(sleeptime)
     end do
end subroutine writeArray


!!***************************
subroutine usage()
    print *, "Usage: genarray  output N  M  K  [infile|nx  ny  nz timesteps sleeptime]"
    print *, "output: name of output file"
    print *, "N:      number of processes in X dimension"
    print *, "M:      number of processes in Y dimension"
    print *, "K:      number of processes in Z dimension"
    print *, "nx:     local array size in X dimension per processor"
    print *, "ny:     local array size in Y dimension per processor"
    print *, "nz:     local array size in Z dimension per processor"
    print *, "infile: file that describes nx ny nz for each processor"
    print *, "timesteps: the total number of timesteps to output" 
    print *, "sleeptime: the time to sleep (s)"
end subroutine usage

!!***************************
subroutine processArgs()
    use genarray_comm

#ifndef __GFORTRAN__
#ifndef __GNUC__
    interface
         integer function iargc()
         end function iargc
    end interface
#endif
#endif

    character(len=256) :: npx_str, npy_str, npz_str, ndx_str, ndy_str, ndz_str
    character(len=256) :: time_str,sleep_str
    integer :: numargs

    !! process arguments
    numargs = iargc()
    !print *,"Number of arguments:",numargs
    if ( numargs < 5 ) then
        call usage()
        call exit(1)
    endif
    call getarg(1, outputfile)
    call getarg(2, npx_str)
    call getarg(3, npy_str)
    call getarg(4, npz_str)
    read (npx_str,'(i5)') npx
    read (npy_str,'(i5)') npy
    read (npz_str,'(i5)') npz
    if ( numargs == 5 ) then
        call getarg(5, inputfile)
        ndx = 0
        ndy = 0
        ndz = 0
        common_size = .false.
    else if (numargs == 9) then
        call getarg(5, ndx_str)
        call getarg(6, ndy_str)
        call getarg(7, ndz_str)
        read (ndx_str,'(i6)') ndx
        read (ndy_str,'(i6)') ndy
        read (ndz_str,'(i6)') ndz
        inputfile=char(0)
        common_size = .true.
        call getarg(8, time_str)
        call getarg(9, sleep_str)
        read (time_str,'(i6)') timesteps
        read (sleep_str,'(i6)') sleeptime
    else
        call usage()
        call exit(1)
    endif

end subroutine processArgs
