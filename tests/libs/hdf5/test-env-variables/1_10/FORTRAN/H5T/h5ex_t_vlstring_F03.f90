!************************************************************
!
!  This example shows how to read and write variable-length
!  string datatypes to a dataset using h5dread_f and
!  h5dwrite_f, and F2003 intrinsics C_LOC and C_F_POINTER.  
!  The program first writes variable-length strings to a dataset 
!  with a dataspace of DIM0, then closes the file.  Next, it 
!  reopens the file, reads back the data, and outputs it to 
!  the screen.
!
!  This file is intended for use with HDF5 Library version 1.8
!  and --enable-fortran2003
!
!************************************************************

PROGRAM main

  USE HDF5
  USE ISO_C_BINDING
  
  IMPLICIT NONE

  CHARACTER(LEN=20), PARAMETER :: filename = "h5ex_vlstring_F03.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset  = "DS1"

  INTEGER(HSIZE_T), PARAMETER :: dim0 = 4
  INTEGER(SIZE_T) , PARAMETER :: sdim = 7
  INTEGER(HID_T)  :: file, filetype, space, dset ! Handles
  INTEGER :: hdferr
  INTEGER(HSIZE_T), DIMENSION(1:1) :: dims = (/dim0/)
  INTEGER(HSIZE_T), DIMENSION(1:2) :: maxdims
  
  TYPE(C_PTR), DIMENSION(1:dim0), TARGET :: wdata
  CHARACTER(len=8, KIND=c_char), DIMENSION(1), TARGET  :: A = "Parting"//C_NULL_CHAR
  CHARACTER(len=8, KIND=c_char), DIMENSION(1), TARGET  :: B = "is_such"//C_NULL_CHAR
  CHARACTER(len=6, KIND=c_char), DIMENSION(1), TARGET  :: C = "sweet"//C_NULL_CHAR
  CHARACTER(len=8, KIND=c_char), DIMENSION(1), TARGET  :: D = "sorrow."//C_NULL_CHAR
  TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: rdata ! Read buffer
  CHARACTER(len = 8, kind=c_char),  POINTER :: data ! A pointer to a Fortran string
  TYPE(C_PTR) :: f_ptr
  INTEGER :: i, len

  ! Initialize array of C pointers
  wdata(1) = C_LOC(A(1))     
  wdata(2) = C_LOC(B(1))     
  wdata(3) = C_LOC(C(1))     
  wdata(4) = C_LOC(D(1))     
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create file and memory datatypes.  For this example we will save
  ! the strings as C variable length strings, H5T_STRING is defined
  ! as a variable length string.
  !
  CALL H5Tcopy_f(H5T_STRING, filetype, hdferr)
  !
  ! Create dataspace.
  !
  CALL h5screate_simple_f(1, dims, space, hdferr)
  !
  ! Create the dataset and write the variable-length string data to
  ! it.
  !
  CALL h5dcreate_f(file, dataset, filetype, space, dset, hdferr)

  f_ptr = C_LOC(wdata(1))
  CALL h5dwrite_f(dset, filetype, f_ptr, hdferr )
  !
  ! Close and release resources.
  !
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL H5Tclose_f(filetype, hdferr)
  CALL h5fclose_f(file , hdferr)
  !
  ! Now we begin the read section of this example.
  !
  !
  ! Open file and dataset.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f(file, dataset, dset, hdferr)
  !
  ! Get the datatype.
  !
  CALL H5Dget_type_f(dset, filetype, hdferr)
  !
  ! Get dataspace and allocate memory for read buffer.
  !
  CALL H5Dget_space_f(dset, space, hdferr)
  CALL H5Sget_simple_extent_dims_f(space, dims, maxdims, hdferr)

  ALLOCATE(rdata(1:dims(1)))
  !
  ! Read the data.
  !
  f_ptr = C_LOC(rdata(1))
  CALL h5dread_f(dset, H5T_STRING, f_ptr, hdferr)
  !
  ! Output the data to the screen.
  !
  DO i = 1, dims(1)
     CALL C_F_POINTER(rdata(i), data)
     len = 0
     DO
        IF(DATA(len+1:len+1).EQ.C_NULL_CHAR.OR.len.GE.8) EXIT
        len = len + 1
     ENDDO
     WRITE(*,'(A,"(",I0,"): ",A)') DATASET, i, data(1:len)
  END DO

  DEALLOCATE(rdata)
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL H5Tclose_f(filetype, hdferr)
  CALL h5fclose_f(file , hdferr)

END PROGRAM main
