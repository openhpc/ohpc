!
!  Copyright (C) 2013, Northwestern University and Argonne National Laboratory
!  See COPYRIGHT notice in top-level directory.
!
!     This is part of the PnetCDF package.
!
!     $Id: f90tst_vars2.f90 2136 2015-10-07 19:25:34Z wkliao $

!     This program tests PnetCDF variable functions from fortran 90.

program f90tst_vars2
  use mpi
  use pnetcdf
  implicit none
  
  ! This is the name of the data file we will create.
  character (len = *), parameter :: FILE_NAME = "f90tst_vars2.nc"

  ! We are writing 2D data, a 6 x 12 grid. 
  integer, parameter :: MAX_DIMS = 2
  integer, parameter :: NX = 6, NY = 12
  integer :: data_out(NY, NX), data_in(NY, NX)
  integer :: data_out_1d(NX), data_in_1d(NX)

  ! We need these ids and other gunk for netcdf.
  integer :: ncid, varid1, varid2, varid3, varid4, varid5, dimids(MAX_DIMS)
  integer :: x_dimid, y_dimid
  integer :: nvars, ngatts, ndims, unlimdimid, file_format
  integer :: x, y
  integer, parameter :: DEFLATE_LEVEL = 4
  integer, parameter :: EightByteInt = selected_int_kind(18)
  integer (kind = EightByteInt) :: TOE_SAN_VALUE = 2147483648_EightByteInt
  character (len = *), parameter :: VAR1_NAME = "Chon-Ji"
  character (len = *), parameter :: VAR2_NAME = "Tan-Gun"
  character (len = *), parameter :: VAR3_NAME = "Toe-San"
  character (len = *), parameter :: VAR4_NAME = "Won-Hyo"
  character (len = *), parameter :: VAR5_NAME = "Yul-Guk"
  integer, parameter :: CACHE_SIZE = 8, CACHE_NELEMS = 571
  integer, parameter :: CACHE_PREEMPTION = 66

  ! Information read back from the file to check correctness.
  integer :: varid1_in, varid2_in, varid3_in, varid4_in, varid5_in
  integer :: xtype_in, ndims_in, natts_in, dimids_in(MAX_DIMS)
  character (len = nf90_max_name) :: name_in
  integer (kind = EightByteInt) :: toe_san_in(1)
  integer :: cmode, err, ierr, get_args
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
  do x = 1, NX
        data_out_1d(x) = x
  end do

  ! Create the netCDF file. 
  cmode = IOR(NF90_CLOBBER, NF90_64BIT_DATA)
  call check(nf90mpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, ncid))

  ! Define the dimensions.
  nx_ll = NX
  ny_ll = NY
  call check(nf90mpi_def_dim(ncid, "x", nx_ll, x_dimid))
  call check(nf90mpi_def_dim(ncid, "y", ny_ll, y_dimid))
  dimids =  (/ y_dimid, x_dimid /)

  ! Define some variables. 
  call check(nf90mpi_def_var(ncid, VAR1_NAME, NF90_INT, dimids, varid1))
  call check(nf90mpi_def_var(ncid, VAR2_NAME, NF90_INT, dimids, varid2))
  call check(nf90mpi_def_var(ncid, VAR3_NAME, NF90_INT64, varid3))
  call check(nf90mpi_def_var(ncid, VAR4_NAME, NF90_INT, x_dimid, varid4))
  call check(nf90mpi_def_var(ncid, VAR5_NAME, NF90_INT, dimids, varid5))

  call check(nf90mpi_enddef(ncid))

  ! enter independent data mode
  call check(nf90mpi_begin_indep_data(ncid))

  ! Write the pretend data to the file.
  call check(nf90mpi_put_var(ncid, varid1, data_out))
  call check(nf90mpi_put_var(ncid, varid2, data_out))
  call check(nf90mpi_put_var(ncid, varid3, TOE_SAN_VALUE))
  call check(nf90mpi_put_var(ncid, varid4, data_out_1d))
  call check(nf90mpi_put_var(ncid, varid5, data_out))

  ! Close the file. 
  call check(nf90mpi_close(ncid))

  ! Reopen the file.
  call check(nf90mpi_open(MPI_COMM_WORLD, filename, nf90_nowrite, MPI_INFO_NULL, ncid))
  
  ! Check some stuff out.
  call check(nf90mpi_inquire(ncid, ndims, nvars, ngatts, unlimdimid, file_format))
  if (ndims /= 2 .or. nvars /= 5 .or. ngatts /= 0 .or. unlimdimid /= -1 .or. &
       file_format /= nf90_format_cdf5) stop 2

  ! Get varids.
  call check(nf90mpi_inq_varid(ncid, VAR1_NAME, varid1_in))
  call check(nf90mpi_inq_varid(ncid, VAR2_NAME, varid2_in))
  call check(nf90mpi_inq_varid(ncid, VAR3_NAME, varid3_in))
  call check(nf90mpi_inq_varid(ncid, VAR4_NAME, varid4_in))
  call check(nf90mpi_inq_varid(ncid, VAR5_NAME, varid5_in))

  ! Check variable 1.
  call check(nf90mpi_inquire_variable(ncid, varid1_in, name_in, xtype_in, ndims_in, dimids_in, natts_in))
  if (name_in .ne. VAR1_NAME .or. xtype_in .ne. NF90_INT .or. ndims_in .ne. MAX_DIMS .or. &
       natts_in .ne. 0 .or. dimids_in(1) .ne. dimids(1) .or. dimids_in(2) .ne. dimids(2)) stop 3

  ! Check variable 2.
  call check(nf90mpi_inquire_variable(ncid, varid2_in, name_in, xtype_in, ndims_in, dimids_in, natts_in))
  if (name_in .ne. VAR2_NAME .or. xtype_in .ne. NF90_INT .or. ndims_in .ne. MAX_DIMS .or. &
       natts_in .ne. 0 .or. dimids_in(1) .ne. dimids(1) .or. dimids_in(2) .ne. dimids(2)) stop 6

  ! Check variable 3.
  call check(nf90mpi_inquire_variable(ncid, varid3_in, name_in, xtype_in, ndims_in, dimids_in, natts_in))
  if (name_in .ne. VAR3_NAME .or. xtype_in .ne. NF90_INT64 .or. ndims_in .ne. 0 .or. &
       natts_in .ne. 0) stop 8
  
  ! Check variable 4.
  call check(nf90mpi_inquire_variable(ncid, varid4_in, name_in, xtype_in, ndims_in, dimids_in, natts_in))
  if (name_in .ne. VAR4_NAME .or. xtype_in .ne. NF90_INT .or. ndims_in .ne. 1 .or. &
       natts_in .ne. 0 .or. dimids_in(1) .ne. x_dimid) stop 10

  ! Check the data.
  call check(nf90mpi_get_var_all(ncid, varid1_in, data_in))
  do x = 1, NX
     do y = 1, NY
        if (data_out(y, x) .ne. data_in(y, x)) stop 12
     end do
  end do
  call check(nf90mpi_get_var_all(ncid, varid2_in, data_in))
  do x = 1, NX
     do y = 1, NY
        if (data_out(y, x) .ne. data_in(y, x)) stop 13
     end do
  end do
  call check(nf90mpi_get_var_all(ncid, varid3_in, toe_san_in))
  if (toe_san_in(1) .ne. TOE_SAN_VALUE) stop 14
  call check(nf90mpi_get_var_all(ncid, varid4_in, data_in_1d))
  do x = 1, NX
     if (data_out_1d(x) .ne. data_in_1d(x)) stop 15
  end do
  call check(nf90mpi_get_var_all(ncid, varid5_in, data_in))
  do x = 1, NX
     do y = 1, NY
        if (data_out(y, x) .ne. data_in(y, x)) stop 12
     end do
  end do

  ! Close the file. 
  call check(nf90mpi_close(ncid))

  msg = '*** TESTING F90 '//trim(cmd)//' for def_var API'
  if (my_rank .eq. 0) call pass_fail(0, msg)

 999 call MPI_Finalize(ierr)

contains
!     This subroutine handles errors by printing an error message and
!     exiting with a non-zero status.
  subroutine check(errcode)
    implicit none
    integer, intent(in) :: errcode
    
    if(errcode /= nf90_noerr) then
       print *, 'Error: ', trim(nf90mpi_strerror(errcode))
       stop 2
    endif
  end subroutine check
end program f90tst_vars2

