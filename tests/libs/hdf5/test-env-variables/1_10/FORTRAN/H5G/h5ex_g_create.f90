!************************************************************
!
!  This example shows how to create, open, and close a group.
!
!  This file is intended for use with HDF5 Library version 1.8
!
!************************************************************
PROGRAM main

  USE HDF5
  IMPLICIT NONE

  CHARACTER(LEN=16), PARAMETER :: filename   = "h5ex_g_create.h5"
  INTEGER(HID_T) :: file, group ! Handles
  INTEGER :: hdferr
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create a group named "G1" in the file.
  !
  CALL h5gcreate_f(file, "/G1", group, hdferr)
  !
  ! Close the group.  The handle "group" can no longer be used.
  !
  CALL h5gclose_f(group,hdferr)
  !
  ! Re-open the group, obtaining a new handle.
  !
  CALL h5gopen_f(file, "/G1", group, hdferr)
  !
  ! Close and release resources.
  !
  CALL h5gclose_f(group, hdferr)
  CALL h5fclose_f(file , hdferr)

END PROGRAM main
