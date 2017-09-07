!
!  Copyright (C) 2013, Northwestern University and Argonne National Laboratory
!  See COPYRIGHT notice in top-level directory.
!
!     This is part of the PnetCDF package.
!
!     $Id: f90tst_vars4.f90 2131 2015-09-25 22:33:12Z wkliao $

!     This program tests PnetCDF variable functions from fortran 90.

program f90tst_vars4
  use mpi
  use pnetcdf
  implicit none

  ! This is the name of the data file we will create.
  character (len = *), parameter :: FILE_NAME = "f90tst_vars4.nc"

  integer, parameter :: MAX_DIMS = 2
  integer, parameter :: NX = 40, NY = 4096
  integer :: data_out(NY, NX), data_in(NY, NX)

  ! We need these ids and other gunk for netcdf.
  integer :: ncid, varid, dimids(MAX_DIMS)
  integer :: x_dimid, y_dimid
  integer :: mode_flag
  integer :: nvars, ngatts, ndims, unlimdimid, file_format
  integer :: x, y
  integer, parameter :: CACHE_SIZE = 1000000
  integer :: xtype_in, natts_in, dimids_in(MAX_DIMS)
  character (len = NF90_MAX_NAME) :: name_in
  integer :: err, ierr, get_args
  integer(KIND=MPI_OFFSET_KIND) :: nx_ll, ny_ll
  character(LEN=256) filename, cmd, msg
  integer my_rank, p

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, p, ierr)

  ! take filename from command-line argument if there is any
  if (my_rank .EQ. 0) then
      filename = FILE_NAME
      err = get_args(cmd, filename)
  endif
  call MPI_Bcast(err, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  if (err .EQ. 0) goto 999

  call MPI_Bcast(filename, 256, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

!  if (p .ne. 1 .AND. my_rank .eq. 0) then
!     print *, 'Warning: ',trim(cmd),' is design to run on 1 process'
!  endif

  ! Create some pretend data.
  do x = 1, NX
     do y = 1, NY
        data_out(y, x) = (x - 1) * NY + (y - 1)
     end do
  end do

  ! Create the netCDF file. 
  mode_flag = IOR(NF90_CLOBBER, NF90_64BIT_DATA)
  call handle_err(nf90mpi_create(MPI_COMM_WORLD, filename, mode_flag, MPI_INFO_NULL, ncid))

  ! Define the dimensions.
  nx_ll = NX
  ny_ll = NY
  call handle_err(nf90mpi_def_dim(ncid, "x", nx_ll, x_dimid))
  call handle_err(nf90mpi_def_dim(ncid, "y", ny_ll, y_dimid))
  dimids =  (/ y_dimid, x_dimid /)

  ! Define the variable. 
  call handle_err(nf90mpi_def_var(ncid, 'data', NF90_INT, dimids, varid))

  ! enddef must be called.
  call handle_err(nf90mpi_enddef(ncid))

  call handle_err(nf90mpi_begin_indep_data(ncid))

  ! Write the pretend data to the file.
  call handle_err(nf90mpi_put_var(ncid, varid, data_out))

  ! Close the file. 
  call handle_err(nf90mpi_close(ncid))

  ! Reopen the file.
  call handle_err(nf90mpi_open(MPI_COMM_WORLD, filename, nf90_nowrite, MPI_INFO_NULL, ncid))
  
  ! Check some stuff out.
  call handle_err(nf90mpi_inquire(ncid, ndims, nvars, ngatts, unlimdimid, file_format))
  if (ndims /= 2 .or. nvars /= 1 .or. ngatts /= 0 .or. unlimdimid /= -1 .or. &
       file_format /= nf90_format_cdf5) stop 3

  call handle_err(nf90mpi_inquire_variable(ncid, varid, name_in, xtype_in, ndims, dimids_in, &
       natts_in))
  if (name_in .ne. 'data' .or. xtype_in .ne. NF90_INT .or. ndims .ne. MAX_DIMS .or. &
       dimids_in(1) /= y_dimid .or. dimids_in(2) /= x_dimid .or. &
       natts_in .ne. 0) &
       stop 4

  ! Check the data.
  call handle_err(nf90mpi_get_var_all(ncid, varid, data_in))
  do x = 1, NX
     do y = 1, NY
        if (data_out(y, x) .ne. data_in(y, x)) stop 3
     end do
  end do

  ! Close the file. 
  call handle_err(nf90mpi_close(ncid))

  msg = '*** TESTING F90 '//trim(cmd)//' for def_var API'
  if (my_rank .eq. 0) call pass_fail(0, msg)

 999 call MPI_Finalize(ierr)

contains
!     This subroutine handles errors by printing an error message and
!     exiting with a non-zero status.
  subroutine handle_err(errcode)
    implicit none
    integer, intent(in) :: errcode
    
    if(errcode /= nf90_noerr) then
       print *, 'Error: ', trim(nf90mpi_strerror(errcode))
       stop 2
    endif
  end subroutine handle_err
end program f90tst_vars4

