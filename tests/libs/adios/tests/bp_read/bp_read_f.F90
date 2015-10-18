!  
!  ADIOS is freely available under the terms of the BSD license described
!  in the COPYING file in the top level directory of this source distribution.
!
!  Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
!

program read_bp_f
    use adios_read_mod
    implicit none
    include "mpif.h"

    integer :: gcnt, vcnt, acnt, tfirst, tlast  ! file info
    integer :: vsteps, vrank, vtype ! variable info
    integer :: ierr
    integer :: comm,nproc,rank
    integer :: i,j,k,l,m
    integer*8 :: fh
    integer*8 :: sel
    integer*8, dimension(10) :: dims, start, readsize

    integer :: an_int
    real*4  :: a_real
    real*8  :: a_double
    complex*8  :: a_complex
    character(len=1024) :: a_string

    !character, dimension(:), allocatable :: varchar
    !integer, dimension(:), allocatable :: varint
    !real*8, dimension(:), allocatable :: vardouble
    character(100000) :: varchar
    integer, dimension(100000) :: varint
    real*8, dimension(10000000) :: vardouble
    integer :: totalsize

    character (len=128), dimension(:), allocatable :: vnamelist
    character (len=128), dimension(:), allocatable :: anamelist
    character (len=128), dimension(:), allocatable :: gnamelist

    character(256) :: path

    call MPI_Init (ierr)
    comm = MPI_COMM_WORLD
    call mpi_comm_rank(comm, rank, ierr)
    call mpi_comm_size(comm, nproc, ierr)

    call processArgs(path)

    call adios_read_init_method (ADIOS_READ_METHOD_BP, comm, "verbose=4", ierr);
    call adios_read_open_file (fh, path, 0, comm, ierr)
    if (ierr .ne. 0) then
        call exit(1)
    endif

    varchar = ' '

    call adios_inq_file (fh,vcnt,acnt,tfirst,tlast,ierr) 
    allocate (vnamelist(vcnt))
    allocate (anamelist(acnt))
    call adios_inq_varnames  (fh, vnamelist, ierr)
    call adios_inq_attrnames (fh, anamelist, ierr)

    write (*,'("Number of variables  : ",i0)') vcnt
    do i=1,vcnt 
        write (*,"(i5, a, a)") i,")  ", trim(vnamelist(i))
    enddo

    write (*,'("Number of attributes : ",i0)') acnt
    do i=1,acnt 
        write (*,"(i5, a, a)") i,")  ", trim(anamelist(i))
    enddo

    write (*,'("Timesteps            : ",i0," - ",i0)') tfirst, tlast
    
    ! Not required, but get the list of groupnames and print here
    call adios_inq_ngroups (fh, gcnt, ierr)
    allocate (gnamelist(gcnt))
    call adios_inq_groupnames (fh, gnamelist, ierr)
    write (*,'("Number of groups : ",i0)') gcnt
    do i=1,gcnt 
        write (*,"(i5, a, a)") i,")  ", trim(gnamelist(i))
    enddo
    deallocate (gnamelist)

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

    write (*,*)"-------- VARIABLE INFO ---------"
    do i=1,vcnt 
        call adios_inq_var (fh, vnamelist(i), vtype, vsteps, vrank, dims, ierr)
        if (vrank == 0) then
            !! scalar variable
            sel = 0
            write (*,'(a,"    scalar: type=",i0," steps=",i0,$)') trim(vnamelist(i)), vtype, vrank
            if (vtype == adios_integer) then
                !call adios_get_scalar (fh, vnamelist(i), an_int, ierr)
                call adios_schedule_read (fh, sel, vnamelist(i), 0, 1,  an_int, ierr)
                call adios_perform_reads (fh, ierr)
                write(*,'("    = ",i0)') an_int
            else if (vtype == adios_real) then 
                call adios_get_scalar (fh, vnamelist(i), a_real, ierr)
                write(*,'("    = ",d20.10)') a_real
            else if (vtype == adios_double) then 
                call adios_get_scalar (fh, vnamelist(i), a_double, ierr)
                write(*,'("    = ",d20.10)') a_double
            else if (vtype == adios_string) then
                call adios_get_scalar (fh, vnamelist(i), a_string, ierr)
                write(*,'("    = ",a)') trim(a_string)
            else if (vtype == adios_complex) then 
                call adios_get_scalar (fh, vnamelist(i), a_complex, ierr)
                write(*,'("    = (",d20.10,",",d20.10,")")') a_complex
            else
                write (*,*) " This type is not handled here"
            endif
        else
            !! array variable
            start(1:10)=0
            readsize(1:10)=1 
            totalsize=1
            write (*,'(a,"    type=",i0," ndims=",i0,"{",$)') trim(vnamelist(i)),vrank,vtype
            do j=1,vrank
                readsize(j) = dims(j)
                totalsize=totalsize*dims(j)
                write(*,'(" ",i0,$)') dims(j)
            enddo
            write(*,'("}")') 
        endif
    enddo

    call adios_read_close(fh, ierr)

    deallocate (vnamelist)
    deallocate (anamelist)

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

