!************************************************************
!
!  This example shows how to iterate over group members using
!  H5Literate.
!
!  This file is intended for use with HDF5 Library version 1.8
!  with --enable-fortran2003 
!
!
!************************************************************
MODULE g_iterate
  
  USE HDF5
  USE ISO_C_BINDING
  IMPLICIT NONE

CONTAINS

!************************************************************
!
!  Operator function.  Prints the name and type of the object
!  being examined.
!
! ************************************************************

  INTEGER FUNCTION op_func(loc_id, name, info, operator_data) bind(C)
    
    USE HDF5
    USE ISO_C_BINDING
    IMPLICIT NONE
    
    INTEGER(HID_T), VALUE :: loc_id
    CHARACTER(LEN=1), DIMENSION(1:10) :: name ! must have LEN=1 for bind(C) strings
    TYPE(C_PTR) :: info
    TYPE(C_PTR) :: operator_data
    
    INTEGER   :: status, i, len

    TYPE(H5O_info_t), TARGET :: infobuf 
    TYPE(C_PTR) :: ptr
    CHARACTER(LEN=10) :: name_string

    !
    ! Get type of the object and display its name and type.
    ! The name of the object is passed to this FUNCTION by
    ! the Library.
    !

    DO i = 1, 10
       name_string(i:i) = name(i)(1:1)
    ENDDO

    CALL H5Oget_info_by_name_f(loc_id, name_string, infobuf, status)

    ! Include the string up to the C NULL CHARACTER
    len = 0
    DO
       IF(name_string(len+1:len+1).EQ.C_NULL_CHAR.OR.len.GE.10) EXIT
       len = len + 1
    ENDDO

    IF(infobuf%type.EQ.H5O_TYPE_GROUP_F)THEN
       WRITE(*,*) "Group: ", name_string(1:len)
    ELSE IF(infobuf%type.EQ.H5O_TYPE_DATASET_F)THEN
       WRITE(*,*) "Dataset: ", name_string(1:len)
    ELSE IF(infobuf%type.EQ.H5O_TYPE_NAMED_DATATYPE_F)THEN
       WRITE(*,*) "Datatype: ", name_string(1:len)
    ELSE
       WRITE(*,*) "Unknown: ", name_string(1:len)
    ENDIF

    op_func = 0 ! return successful

  END FUNCTION op_func

END MODULE g_iterate


PROGRAM main

  USE HDF5
  USE ISO_C_BINDING
  USE g_iterate
  
  IMPLICIT NONE

  CHARACTER(LEN=17), PARAMETER :: filename  = "h5ex_g_iterate.h5"
  INTEGER(HID_T) :: file ! Handle
  INTEGER :: status
  TYPE(C_FUNPTR) :: funptr
  TYPE(C_PTR) :: ptr
  INTEGER(hsize_t) :: idx
  INTEGER :: ret_value

  !
  ! Open file.
  !
  CALL H5Fopen_f(filename, H5F_ACC_RDONLY_F, file, status)

  !
  ! Begin iteration.
  !
  WRITE(*,'(A)') "Objects in root group:"

  idx = 0
  funptr = C_FUNLOC(op_func) ! call back function
  ptr    = C_NULL_PTR

  CALL H5Literate_f(file, H5_INDEX_NAME_F, H5_ITER_NATIVE_F, idx, funptr, ptr, ret_value, status)

  !
  ! Close and release resources.
  !
  CALL H5Fclose_f(file, status)

END PROGRAM main

