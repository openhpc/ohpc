!
!  Copyright (C) 2013, Northwestern University and Argonne National Laboratory
!  See COPYRIGHT notice in top-level directory.
!
!     This is part of the PnetCDF package.
!
!     $Id: tst_f90.f90 2136 2015-10-07 19:25:34Z wkliao $

! This program provides an elementary check of some of the parts of the 
!   Fortran 90 interface to netCDF 3.5. It is a Fortran 90 implementation
!   of the nctst.cpp program provided with the C++ interface to netcdf
!   (in the src/cxx directory of the netcdf distribution). 
!
module typeSizes
  implicit none
  public
  integer, parameter ::   OneByteInt = selected_int_kind(2), &
                          TwoByteInt = selected_int_kind(4), &
                         FourByteInt = selected_int_kind(9), &
                        EightByteInt = selected_int_kind(18)

  integer, parameter ::                                          &
                        FourByteReal = selected_real_kind(P =  6, R =  37), &
                       EightByteReal = selected_real_kind(P = 13, R = 307)
contains
  logical function byteSizesOK()
  ! Users may call this function once to ensure that the kind parameters 
  !   the module defines are available with the current compiler. 
  ! We can't ensure that the two REAL kinds are actually four and 
  !   eight bytes long, but we can ensure that they are distinct. 
  ! Early Fortran 90 compilers would sometimes report incorrect results for 
  !   the bit_size intrinsic, but I haven't seen this in a long time. 

    ! Local variables
    integer (kind =  OneByteInt) :: One
    integer (kind =  TwoByteInt) :: Two
    integer (kind = FourByteInt) :: Four

    if (bit_size( One) == 8  .and. bit_size( Two) == 16 .and.  &
        bit_size(Four) == 32 .and.                             &
        FourByteReal > 0 .and. EightByteReal > 0 .and. &
        FourByteReal /= EightByteReal) then
      byteSizesOK = .true.
    else
      byteSizesOK = .false.
    end if
  end function byteSizesOK
end module typeSizes

program netcdfTest
  use mpi
  use typeSizes
  use pnetcdf
  implicit none
  
  ! netcdf related variables
  integer :: ncFileID,                                   &
             latDimID, lonDimID, frTimeDimID, timeDimID, &
             pressVarID, latVarID, lonVarID, frTimeVarID, refTimeVarID, scalarVarID
             
  ! Local variables
  integer (kind = EightByteInt), parameter :: numLats = 4, numLons = 3, &
                        numFrTimes = 2, timeStringLen = 20
  character (len = *), parameter :: FILE_NAME = "tst_f90.nc"
  integer :: counter, err, ierr, get_args
  real, dimension(numLons, numLats, numFrTimes) :: pressure
  integer (kind = FourByteInt), dimension(numFrTimes) :: frTimeVals
  real (kind = FourByteReal) fillVal, scalarVarBuf
  real (kind = FourByteReal), dimension(2) :: validRange
  character (len = 20) frTimeUnits
  real (kind = FourByteReal), dimension(numLats) :: latVarBuf
  real (kind = FourByteReal), dimension(numLons) :: lonVarBuf
  character(LEN=256) filename, cmd, msg
  integer my_rank, p, info

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

! --------------------
  ! Code begins
  ! --------------------
  if(.not. byteSizesOK()) then
    print *, "Compiler does not appear to support required kinds of variables."
    stop
  end if

  call MPI_Info_create(info, ierr)
  ! call MPI_Info_set(info, "romio_pvfs2_posix_write", "enable",ierr)

  ! Create the file
  call check(nf90mpi_create(MPI_COMM_WORLD, filename, nf90_clobber, info, ncFileID))
  
  ! Define the dimensions
  call check(nf90mpi_def_dim(ncid = ncFileID, name = "lat",     len = numLats,           dimid = latDimID))
  call check(nf90mpi_def_dim(ncid = ncFileID, name = "lon",     len = numLons,           dimid = lonDimID))
  call check(nf90mpi_def_dim(ncid = ncFileID, name = "frtime",  len = nf90mpi_unlimited, dimid = frTimeDimID))
  call check(nf90mpi_def_dim(ncid = ncFileID, name = "timelen", len = timeStringLen,     dimid = timeDimID))

  ! Create variables and attributes
  call check(nf90mpi_def_var(ncid = ncFileID, name = "P", xtype = nf90_float,     &
                     dimids = (/ lonDimID, latDimID, frTimeDimID /), varID = pressVarID) )
  call check(nf90mpi_put_att(ncFileID, pressVarID, "long_name",   "pressure at maximum wind"))
  call check(nf90mpi_put_att(ncFileID, pressVarID, "units",       "hectopascals") )
  ! Use 4-byte reals explicitly, to match 4-byte attribute type in test file
  validRange(1) = 0.
  validRange(2) = 1500
  call check(nf90mpi_put_att(ncFileID, pressVarID, "valid_range", validRange))
  ! Use a 4-byte float constant, to match variable type
  fillVal = -9999.0
  call check(nf90mpi_put_att(ncFileID, pressVarID,  "_FillValue", fillVal ) )
                      
  call check(nf90mpi_def_var(ncFileID, "lat", nf90_float, dimids = latDimID, varID = latVarID) )
  call check(nf90mpi_put_att(ncFileID, latVarID, "long_name", "latitude"))
  call check(nf90mpi_put_att(ncFileID, latVarID, "units", "degrees_north"))

  call check(nf90mpi_def_var(ncFileID, "lon", nf90_float, lonDimID, lonVarID) )
  call check(nf90mpi_put_att(ncFileID, lonVarID, "long_name", "longitude"))
  call check(nf90mpi_put_att(ncFileID, lonVarID, "units",     "degrees_east"))

  call check(nf90mpi_def_var(ncFileID, "frtime", nf90_int, frTimeDimID, frTimeVarID) )
  call check(nf90mpi_put_att(ncFileID, frTimeVarID, "long_name", "forecast time"))
  call check(nf90mpi_put_att(ncFileID, frTimeVarID, "units",     "hours"))

  call check(nf90mpi_def_var(ncFileID, "reftime", nf90_char, timeDimID, refTimeVarID) )
  call check(nf90mpi_put_att(ncFileID, refTimeVarID, "long_name", "reference time"))
  call check(nf90mpi_put_att(ncFileID, refTimeVarID, "units",     "text_time"))
                     
  ! In the C++ interface the define a scalar variable - do we know how to do this? 
  call check(nf90mpi_def_var(ncFileID, "ScalarVariable", nf90_real, scalarVarID))
  
  ! Global attributes
  call check(nf90mpi_put_att(ncFileID, nf90_global, "history", &
                     "created by Unidata LDM from NPS broadcast"))
  call check(nf90mpi_put_att(ncFileID, nf90_global, "title", &
                     "NMC Global Product Set: Pressure at Maximum Wind"))
  
  ! Leave define mode
  call check(nf90mpi_enddef(ncfileID))
  
  ! Write the dimension variables
  latVarBuf = (/ -90., -87.5, -85., -82.5 /)
  call check(nf90mpi_put_var_all(ncFileID, latVarID, latVarBuf))
  lonVarBuf = (/ -180, -175, -170 /)
  call check(nf90mpi_put_var_all(ncFileID, lonVarID, lonVarBuf))
  ! Don't use anonymous array here, in case platform has 8-byte integers
  frTimeVals(1) = 12
  frTimeVals(2) = 18
  call check(nf90mpi_put_var_all(ncFileID, frTimeVarID,  frTimeVals                  ) )
  call check(nf90mpi_put_var_all(ncFileID, reftimeVarID, "1992-3-21 12:00"           ) )
  
  ! Write the pressure variable. Write a slab at a time to check incrementing.
  pressure = 949. + real(reshape( (/ (counter, counter = 1, numLats * numLons * numFrTimes) /),  &
                                    (/ numLons, numLats, numFrTimes /) ) )
  call check(nf90mpi_put_var_all(ncFileID, pressVarID, pressure(:, :, 1:1)) )
  call check(nf90mpi_put_var_all(ncFileID, pressVarID, pressure(:, :, 2:2), &
                                 start = (/ 1_EightByteInt, 1_EightByteInt, 2_EightByteInt /)) )
  
  call check(nfmpi_begin_indep_data(ncFileID))
  scalarVarBuf = 10
  call check(nf90mpi_put_var(ncFileID, scalarVarID, scalarVarBuf))
  call check(nfmpi_end_indep_data(ncFileID))

  call check(nf90mpi_close(ncFileID))

  ! Now open the file to read and check a few values
  call check(nf90mpi_open(MPI_COMM_WORLD, filename, NF90_NOWRITE, info, ncFileID))
  call check(nf90mpi_inq_varid(ncFileID,"frtime",frTimeVarID))
  call check(nf90mpi_get_att(ncFileID,frTimeVarID,"units",frTimeUnits))
  if(frTimeUnits .ne. "hours") then
     print *, 'Attribute value not what was written:', frTimeUnits
     stop 2
  endif
  call check(nf90mpi_close(ncFileID))
  call MPI_Info_free(info, ierr)

  msg = '*** TESTING F90 '//trim(cmd)
  if (my_rank .eq. 0) call pass_fail(0, msg)

 999 call MPI_Finalize(ierr)

contains
  ! Internal subroutine - checks error status after each netcdf, prints out text message each time
  !   an error code is returned. 
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90mpi_strerror(status))
      stop 2
    end if
  end subroutine check  
end program netcdfTest
