!  
!  ADIOS is freely available under the terms of the BSD license described
!  in the COPYING file in the top level directory of this source distribution.
!
!  Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
!

program read_bp_f
    implicit none
    include "mpif.h"

    integer :: gcnt, vcnt, acnt, tstart, ntsteps, tstop, vrank, vtype, timedim, ierr
    integer :: comm,i,j,k,l,m
    integer*8 :: read_bytes
    integer*8 :: fh, gh
    integer*8, dimension(10) :: dims, start, readsize
    integer, dimension(1000) :: var, vartrue
    !character, dimension(:), allocatable :: varchar
    !integer, dimension(:), allocatable :: varint
    !real*8, dimension(:), allocatable :: vardouble
    character(100000) :: varchar
    integer, dimension(100000) :: varint
    real*8, dimension(10000000) :: vardouble
    integer :: totalsize
    character (len=100), dimension(5000) :: vnamelist
    character (len=100), dimension(5000) :: anamelist
    character (len=100), dimension(5000) :: gnamelist

    integer :: ilom,ihip,jlom,jhip,klom,khip,nx
    integer, dimension(3) :: vstart, readcount
    integer,dimension(48) :: bconds  ! all boundary conditions as one array
    character(20)    :: vname
    integer          :: one, rank, nproc 
    integer          :: timestep, lasttimestep
    real*8,dimension(102,66,3)  :: b1
    character(256) :: path

    call MPI_Init (ierr)
    comm = MPI_COMM_WORLD
    call mpi_comm_rank(comm, rank, ierr)
    call mpi_comm_size(comm, nproc, ierr)

    call processArgs(path)

    call adios_set_read_method (0, ierr);
    varchar = ' '
    call adios_fopen (fh, path, comm, gcnt, ierr)

    call adios_inq_file (fh,vcnt,acnt,tstart,ntsteps,gnamelist,ierr) 
    tstop = ntsteps+ntsteps-1
    write (*,'("Number of timesteps : ",i0," starting from ",i0)') ntsteps, tstart
    write (*,'("Number of groups : ",i0)') gcnt

    do i=1,gcnt 
        write (*,"(i5, a, a)") i,")  ", trim(gnamelist(i))
    enddo

    call adios_gopen (fh, gh, gnamelist(1), vcnt, acnt, ierr) 
    call adios_inq_group(gh, vnamelist, anamelist, timestep, lasttimestep, ierr)

    write (*,'("Number of variables in group ",a,": ",i0)') trim(gnamelist(1)), vcnt
    do i=1,vcnt 
        write (*,"(i5, a, a)") i,")  ", trim(vnamelist(i))
    enddo

    write (*,'("Number of attributes in group ",a,": ",i0)') trim(gnamelist(1)), acnt
    do i=1,acnt 
        write (*,"(i5, a, a)") i,")  ", trim(anamelist(i))
    enddo

    ! vtype 
    ! 0 byte 
    ! 1 short 
    ! 2 integer
    ! 4 long 
    ! 5 real 
    ! 6 double
    ! 7 long double
    ! 9 string
    ! 10 complex
    ! 11 double_complex
    ! 50 unsigned_byte 
    ! 51 unsigned_short
    ! 52 unsigned_integer
    ! 54 unsigned_long
    !write (*,*) "name    ",  "  ndim    ", "    dims"

    write (*,*)"-----------------------------"
    !do i=1,vcnt 
    !    call adios_inq_var (gh, vnamelist(i), vtype, vrank, dims, timedim, ierr)
    !    start(1:10)=0
    !    readsize(1:10)=1 
    !    totalsize=1
    !    do j=1,vrank
    !        readsize(j) = dims(j)
    !        totalsize=totalsize*dims(j)
    !    enddo
    !    write (*,'(a," ndims=",i0," type=",i0," size=",i0)') trim(vnamelist(i)),vrank,vtype,totalsize
    !    if (vtype == 0) then
    !        !if(allocated(varchar)) deallocate(varchar)
    !        !allocate(varchar(totalsize))
    !        !varchar(1)="!"
    !        call adios_read_var (gh, vnamelist(i), start, readsize, varchar, read_bytes)
    !    else if (vtype == 2) then
    !        !write (*,*) "  totalsize = ", totalsize
    !        !if (totalsize == 1) totalsize = 10
    !        !if(allocated(varint)) then
    !        !    write (*,*) "  dealloc varint"
    !        !    deallocate(varint)
    !        !endif
    !        !write (*,*) "  alloc varint, size = ", totalsize
    !        !allocate(varint(totalsize))
    !        !write (*,*) "  totalsize = ", totalsize
    !        !varint(:)=5
    !        !print *, varint(1:10)
    !        call adios_read_var (gh, vnamelist(i), start, readsize, varint, read_bytes)
    !        print *, varint(1)
    !    else if (vtype == 6) then
    !        !if(allocated(vardouble)) deallocate(vardouble)
    !        !allocate(vardouble(totalsize))
    !        call adios_read_var (gh, vnamelist(i), start, readsize, vardouble, read_bytes)
    !    else
    !        write (*,'(a16,": Only integer or double type is handled here")') trim(vnamelist(i))
    !    endif
    !    if (vrank == 0) then
    !        if (vtype == 0) then
    !            write(*,'("    = ",a)') varchar(1:1)
    !        else if (vtype == 2) then
    !            write(*,'("    = ",i0)') varint(1)
    !        else if (vtype == 6) then 
    !            write(*,'("    = ",d20.10)') vardouble(1)
    !        endif
    !    else
    !        !write (*,*)"-----------------------------"
    !        !write (*,'(a30,t32," dimensions = [",i0,$)') vnamelist(i), dims(1)
    !        write (*,'("   dimensions = [",i0,$)') dims(1)
    !        do j=2,vrank
    !            write (*,'(",",i0,$)') dims(j)
    !        enddo
    !        if (vtype == 0) then
    !            write (*,'("] = ",a)') varchar
    !        else
    !            write (*,'("]")') 
    !        endif
    !    endif
    !enddo

    start(1) = 0
    start(2) = rank
    readsize(1) = 10
    readsize(2) = 1
    call adios_read_var (gh, "temperature", start, readsize, vardouble, read_bytes)
    call adios_gclose(gh, ierr)
    call adios_fclose(fh, ierr)

    do i=1,4
      write (*,*) vardouble(i) 
    enddo

    call MPI_Finalize (ierr)
end program

!!***************************
subroutine usage()
    write (*,*) "Usage: bp_read_f path]"
    write (*,*) "    path: name of bp file"
end subroutine usage

!!***************************
subroutine processArgs(path)

    implicit none
    character(len=256), intent(out) :: path

#ifndef __GFORTRAN__
#ifndef __GNUC__
    interface
         integer function iargc()
         end function iargc
    end interface
#endif
#endif

    integer :: numargs

    !! process arguments
    numargs = iargc()
    !print *,"Number of arguments:",numargs
    if ( numargs < 1 ) then
        call usage()
        call exit(1)
    endif
    call getarg(1, path)

end subroutine processArgs


