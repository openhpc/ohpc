!     This is part of the netCDF package.
!     Copyright 2006 University Corporation for Atmospheric Research/Unidata.
!     See COPYRIGHT file for conditions of use.

!     This program tests netCDF-4 parallel I/O functions from fortran.

!     We are writing 2D data, a 6 x 12 grid, on 4 processors. Each
!     processor will write it's rank to it's quarter of the array. The
!     result will be (in CDL):
!
! netcdf f90tst_parallel {
! dimensions:
! 	x = 16 ;
! 	y = 16 ;
! variables:
! 	int data(x, y) ;
! data:
! 
!  data =
!   0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
!   0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
!   0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
!   0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
!   0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
!   0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
!   0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
!   0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
!   2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
!   2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
!   2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
!   2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
!   2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
!   2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
!   2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
!   2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3 ;
! }

!     $Id: f90tst_parallel.f90,v 1.6 2010/05/25 13:53:04 ed Exp $

program f90tst_parallel
  use typeSizes
  use netcdf
  implicit none
  include 'mpif.h'
  
  ! This is the name of the data file we will create.
  character (len = *), parameter :: FILE_NAME = "f90tst_parallel.nc"

  integer, parameter :: MAX_DIMS = 2
  integer, parameter :: NX = 16, NY = 16
  integer, parameter :: NUM_PROC = 4
  integer :: ncid, varid, dimids(MAX_DIMS), chunksizes(MAX_DIMS), chunksizes_in(MAX_DIMS)
  integer :: x_dimid, y_dimid, contig
  integer :: data_out(NY / 2, NX / 2), data_in(NY / 2, NX / 2)
  integer :: mode_flag
  integer :: nvars, ngatts, ndims, unlimdimid, file_format
  integer :: x, y, retval
  integer :: p, my_rank, ierr
  integer :: start(MAX_DIMS), count(MAX_DIMS)

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, p, ierr)

  if (my_rank .eq. 0) then
     print *, ' '
     print *, '*** Testing netCDF-4 parallel I/O from Fortran 90.'
  endif

  ! There must be 4 procs for this test.
  if (p .ne. 4) then
     print *, 'Sorry, this test program must be run on four processors.'
     stop 2 
  endif

  ! Create some pretend data.
  do x = 1, NX / 2
     do y = 1, NY / 2
        data_out(y, x) = my_rank
     end do
  end do

  ! Create the netCDF file. 
  mode_flag = IOR(nf90_netcdf4, nf90_classic_model) 
  mode_flag = IOR(mode_flag, nf90_mpiio) 
  call handle_err(nf90_create(FILE_NAME, mode_flag, ncid, comm = MPI_COMM_WORLD, &
       info = MPI_INFO_NULL))

  ! Define the dimensions.
  call handle_err(nf90_def_dim(ncid, "x", NX, x_dimid))
  call handle_err(nf90_def_dim(ncid, "y", NY, y_dimid))
  dimids =  (/ y_dimid, x_dimid /)

  ! Define the variable. 
  call handle_err(nf90_def_var(ncid, "data", NF90_INT, dimids, varid))

  ! With classic model netCDF-4 file, enddef must be called.
  call handle_err(nf90_enddef(ncid))

  ! Determine what part of the variable will be written for this
  ! processor. It's a checkerboard decomposition.
  count = (/ NX / 2, NY / 2 /)
  if (my_rank .eq. 0) then
     start = (/ 1, 1 /)
  else if (my_rank .eq. 1) then
     start = (/ NX / 2 + 1, 1 /)
  else if (my_rank .eq. 2) then
     start = (/ 1, NY / 2 + 1 /)
  else if (my_rank .eq. 3) then
     start = (/ NX / 2 + 1, NY / 2 + 1 /)
  endif

  ! Write this processor's data.
  call handle_err(nf90_put_var(ncid, varid, data_out, start = start, count = count))

  ! Close the file. 
  call handle_err(nf90_close(ncid))

  ! Reopen the file.
  call handle_err(nf90_open(FILE_NAME, IOR(nf90_nowrite, nf90_mpiio), ncid, &
       comm = MPI_COMM_WORLD, info = MPI_INFO_NULL))
  
  ! Check some stuff out.
  call handle_err(nf90_inquire(ncid, ndims, nvars, ngatts, unlimdimid, file_format))
  if (ndims /= 2 .or. nvars /= 1 .or. ngatts /= 0 .or. unlimdimid /= -1 .or. &
       file_format /= nf90_format_netcdf4_classic) stop 3

  ! Set collective access on this variable. This will cause all
  ! reads/writes to happen together on every processor. Fairly
  ! pointless, in this contexct, but I want to at least call this
  ! function once in my testing.
  call handle_err(nf90_var_par_access(ncid, varid, nf90_collective))
      
  ! Read this processor's data.
  call handle_err(nf90_get_var(ncid, varid, data_in, start = start, count = count))

  ! Check the data.
  do x = 1, NX / 2
     do y = 1, NY / 2
        if (data_in(y, x) .ne. my_rank) stop 4
     end do
  end do

  ! Close the file. 
  call handle_err(nf90_close(ncid))

  call MPI_Finalize(ierr)

  if (my_rank .eq. 0) print *,'*** SUCCESS!'

contains
!     This subroutine handles errors by printing an error message and
!     exiting with a non-zero status.
  subroutine handle_err(errcode)
    use netcdf
    implicit none
    integer, intent(in) :: errcode
    
    if(errcode /= nf90_noerr) then
       print *, 'Error: ', trim(nf90_strerror(errcode))
       stop 2
    endif
  end subroutine handle_err
end program f90tst_parallel

