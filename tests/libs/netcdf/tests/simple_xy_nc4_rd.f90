! This is part of the netCDF package.
! Copyright 2006 University Corporation for Atmospheric Research/Unidata.
! See COPYRIGHT file for conditions of use.
      
! This is a simple example which reads a small dummy array, from a
! netCDF data file created by the companion program simple_xy_wr.f90.
      
! This is intended to illustrate the use of the netCDF fortran 77
! API. This example program is part of the netCDF tutorial, which can
! be found at:
! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial
      
! Full documentation of the netCDF Fortran 90 API can be found at:
! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90

! $Id: simple_xy_nc4_rd.f90,v 1.3 2009/02/25 12:23:45 ed Exp $

program simple_xy_rd
  use netcdf
  implicit none

  ! This is the name of the data file and variable.
  character (len = *), parameter :: FILE_NAME = "simple_xy_nc4.nc"
  character (len = *), parameter :: VAR_NAME = "data"

  ! We are reading 2D data, a 12 x 6 grid. 
  integer, parameter :: NX = 6, NY = 12, MAX_DIMS = 2
  integer :: data_in(NY, NX)

  ! This will be the netCDF ID for the file and data variable.
  integer :: ncid, varid

  ! Information we will read about the variable.
  character (len = nf90_max_name) :: name
  integer :: xtype, ndims, dimids(MAX_DIMS), natts
  integer :: chunksizes(MAX_DIMS), deflate_level, endianness
  logical :: contiguous, shuffle, fletcher32

  ! Loop indexes, and error handling.
  integer :: x, y

  ! Open the file. NF90_NOWRITE tells netCDF we want read-only access
  ! to the file.
  call check( nf90_open(FILE_NAME, NF90_NOWRITE, ncid) )

  ! Get the varid of the data variable, based on its name.
  call check( nf90_inq_varid(ncid, VAR_NAME, varid) )

  ! Learn about the variable. This uses all optional parameters.
  call check( nf90_inquire_variable(ncid, varid, name, xtype, ndims, &
       dimids, natts, contiguous = contiguous, chunksizes = chunksizes, &
       deflate_level = deflate_level, shuffle = shuffle, &
       fletcher32 = fletcher32, endianness = endianness) )

  ! Make sure we got the correct answers. These depend on what was set
  ! when creating the file in simple_xy_nc4_wr.f90. Endianness will be
  ! whatever is native for the machine that's building this example.
  if (name .ne. VAR_NAME .or. xtype .ne. NF90_INT .or. ndims .ne. 2 .or. &
       natts .ne. 0 .or. contiguous .or. .not. shuffle .or. &
       deflate_level .ne. 1 .or. fletcher32) stop 3

  ! Read the data.
  call check( nf90_get_var(ncid, varid, data_in) )

  ! Check the data.
  do x = 1, NX
     do y = 1, NY
        if (data_in(y, x) /= (x - 1) * NY + (y - 1)) then
           print *, "data_in(", y, ", ", x, ") = ", data_in(y, x)
           stop "Stopped"
        end if
     end do
  end do

  ! Close the file, freeing all resources.
  call check( nf90_close(ncid) )

  print *,"*** SUCCESS reading example file ", FILE_NAME, "! "

contains
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop 2
    end if
  end subroutine check  
end program simple_xy_rd
