!     This is part of the netCDF package.  Copyright 2006 University
!     Corporation for Atmospheric Research/Unidata.  See COPYRIGHT
!     file for conditions of use.

!     This is a very simple example which writes a 2D array of sample
!     data. To handle this in netCDF we create two shared dimensions,
!     "x" and "y", and a netCDF variable, called "data".

!     This example demonstrates the netCDF Fortran 90 API. This is
!     part of the netCDF tutorial, which can be found at:
!     http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial
      
!     Full documentation of the netCDF Fortran 90 API can be found at:
!     http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90

!     $Id: simple_xy_nc4_wr.f90,v 1.6 2010/04/06 19:32:09 ed Exp $

program simple_xy_wr
  use netcdf
  implicit none

  character (len = *), parameter :: FILE_NAME = "simple_xy_nc4.nc"
  integer, parameter :: NDIMS = 2
  integer, parameter :: NX = 6, NY = 12
  integer :: ncid, varid, dimids(NDIMS)
  integer :: x_dimid, y_dimid
  integer :: data_out(NY, NX)
  integer :: chunks(2)
  integer :: deflate_level
  integer :: x, y

  ! Create some pretend data. If this wasn't an example program, we
  ! would have some real data to write, for example, model output.
  do x = 1, NX
     do y = 1, NY
        data_out(y, x) = (x - 1) * NY + (y - 1)
     end do
  end do

  ! Always check the return code of every netCDF function call. In
  ! this example program, wrapping netCDF calls with "call check()"
  ! makes sure that any return which is not equal to nf90_noerr (0)
  ! will print a netCDF error message and exit.

  ! Create the netCDF file. The nf90_clobber parameter tells netCDF to
  ! overwrite this file, if it already exists.
  call check( nf90_create(FILE_NAME, nf90_netcdf4, ncid) )

  ! Define the dimensions. NetCDF will hand back an ID for each. 
  call check( nf90_def_dim(ncid, "x", NX, x_dimid) )
  call check( nf90_def_dim(ncid, "y", NY, y_dimid) )

  ! The dimids array is used to pass the IDs of the dimensions of
  ! the variables. Note that in fortran arrays are stored in
  ! column-major format.
  dimids =  (/ y_dimid, x_dimid /)

  ! Define the variable. The type of the variable in this case is
  ! NF90_INT (4-byte integer). Optional parameters chunking, shuffle,
  ! and deflate_level are used.
  chunks(1) = NY
  chunks(2) = NX
  deflate_level = 1
  call check( nf90_def_var(ncid, "data", NF90_INT, dimids, varid, &
       chunksizes = chunks, shuffle = .TRUE., deflate_level = deflate_level) )

  ! End define mode. This tells netCDF we are done defining metadata.
  call check( nf90_enddef(ncid) )

  ! Write the pretend data to the file. Although netCDF supports
  ! reading and writing subsets of data, in this case we write all the
  ! data in one operation.
  call check( nf90_put_var(ncid, varid, data_out) )

  ! Close the file. This frees up any internal netCDF resources
  ! associated with the file, and flushes any buffers.
  call check( nf90_close(ncid) )

  print *, '*** SUCCESS writing example file ', FILE_NAME, '!'

contains
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop 2
    end if
  end subroutine check  
end program simple_xy_wr
