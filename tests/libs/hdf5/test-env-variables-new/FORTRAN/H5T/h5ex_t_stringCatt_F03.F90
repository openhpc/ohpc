!************************************************************
!
!  This example shows how to read and write fixed C string datatypes
!  to an attribute.  The program first writes C strings to an
!  attribute with a dataspace of DIM0, then closes the file.
!  Next, it reopens the file, reads back the data, and
!  outputs it to the screen.
!
!  This file is intended for use with HDF5 Library verion 1.8
!  with --enable-fortran2003 
!
!************************************************************
PROGRAM main

  USE HDF5
  USE ISO_C_BINDING

  IMPLICIT NONE

  CHARACTER(LEN=24), PARAMETER :: filename  = "h5ex_t_stringCatt_F03.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset   = "DS1"
  CHARACTER(LEN=2) , PARAMETER :: attribute = "A1"
  INTEGER          , PARAMETER :: dim0      = 4
  INTEGER(SIZE_T)  , PARAMETER :: sdim      = 7

  INTEGER(HID_T)  :: file, filetype, memtype, space, dset, attr ! Handles
  INTEGER :: hdferr
  INTEGER(hsize_t), DIMENSION(1:1) :: dims = (/dim0/)
  INTEGER(HSIZE_T), DIMENSION(1:1) :: maxdims
  
  CHARACTER(LEN=sdim), DIMENSION(1:dim0), TARGET :: &
       wdata = (/"Parting","is such", "sweet  ", "sorrow."/)
  CHARACTER(LEN=sdim), DIMENSION(:), ALLOCATABLE, TARGET :: rdata
  INTEGER :: i
  INTEGER(SIZE_T) :: size
  TYPE(C_PTR) :: f_ptr
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
  ! the strings as C strings
  !
  ! Include the NULL TERMINATION of string in C (i.e. add +1 to the length)

  CALL H5Tcopy_f(H5T_C_S1, filetype, hdferr)
  CALL H5Tset_size_f(filetype, sdim+1, hdferr)

  CALL H5Tcopy_f( H5T_FORTRAN_S1, memtype, hdferr)
  CALL H5Tset_size_f(memtype, sdim, hdferr)
  !
  ! Create dataset with a null dataspace.
  !
  CALL H5Screate_f(H5S_NULL_F, space, hdferr)
  CALL h5dcreate_f(file, dataset, H5T_STD_I32LE, space, dset, hdferr)
  CALL H5Sclose_f(space, hdferr)
  !
  ! Create dataspace.
  !
  CALL h5screate_simple_f(1, dims, space, hdferr)
  !
  ! Create the attribute and write the string data to it.
  !
  CALL H5Acreate_f(dset, attribute, filetype, space, attr, hdferr)

  f_ptr = C_LOC(wdata(1)(1:1))
  CALL H5Awrite_f(attr, memtype, f_ptr, hdferr)
  !
  ! Close and release resources.
  !
  CALL h5aclose_f(attr, hdferr)
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL H5tclose_f(filetype, hdferr)
  CALL H5tclose_f(memtype, hdferr)
  CALL h5fclose_f(file , hdferr)
  !
  ! Now we begin the read section of this example.
  !
  ! Open file and dataset.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f(file, dataset, dset, hdferr)
  CALL h5aopen_f(dset, attribute, attr, hdferr)
  !
  ! Get the datatype and its size.
  !
  CALL H5Aget_type_f(attr, filetype, hdferr)
  CALL H5Tget_size_f(filetype, size, hdferr)

  ! Make sure the declared length is large enough
  IF(size.GT.sdim+1)THEN
     PRINT*,'ERROR: Character LEN is to small'
     STOP
  ENDIF
  !
  ! Get dataspace and allocate memory for read buffer.
  ! 
  CALL H5Aget_space_f(attr, space, hdferr)
  CALL H5Sget_simple_extent_dims_f(space, dims, maxdims, hdferr)

  ALLOCATE(rdata(1:dims(1)))
  !
  ! Create the memory datatype.
  !
  CALL H5Tcopy_f(H5T_FORTRAN_S1, memtype, hdferr)
  CALL H5Tset_size_f(memtype, sdim, hdferr)
  !
  ! Read the data.
  !
  f_ptr = C_LOC(rdata(1)(1:1))
  CALL H5Aread_f(attr, memtype, f_ptr, hdferr)
  !
  ! Output the data to the screen.
  !
  DO i = 1, dims(1)
     WRITE(*,'(A,"(",I0,"): ", A)') attribute, i, rdata(i)
  END DO
  !
  ! Close and release resources.
  !
  CALL H5Aclose_f(attr, hdferr)
  CALL H5Dclose_f(dset, hdferr)
  CALL H5Sclose_f(space, hdferr)
  CALL H5Tclose_f(memtype, hdferr)
  CALL H5Fclose_f(file, hdferr)

  DEALLOCATE(rdata)

END PROGRAM main
