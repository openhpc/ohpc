!************************************************************
!
!  This example shows how to read and write variable-length
!  string datatypes to a dataset.  The program first writes
!  variable-length strings to a dataset with a dataspace of
!  DIM0, then closes the file.  Next, it reopens the file,
!  reads back the data, and outputs it to the screen.
!
!  This file is intended for use with HDF5 Library version 1.8
!
!
!************************************************************

PROGRAM main

  USE HDF5
  
  IMPLICIT NONE

  CHARACTER(LEN=18), PARAMETER :: filename  = "h5ex_t_vlstring.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset   = "DS1"

  INTEGER(HSIZE_T), PARAMETER :: dim0      = 4
  INTEGER(HSIZE_T), PARAMETER :: sdim      = 7
  INTEGER(HID_T)  :: file, filetype, space, dset ! Handles
  INTEGER :: hdferr
  INTEGER(HSIZE_T), DIMENSION(1:1) :: dims = (/dim0/)
  INTEGER(HSIZE_T), DIMENSION(1:2) :: maxdims
  
  CHARACTER(LEN=sdim), DIMENSION(1:dim0), TARGET :: &
       wdata = (/"Parting", "is such", "sweet  ", "sorrow."/) ! Write buffer
  CHARACTER(LEN=sdim), DIMENSION(:), ALLOCATABLE :: rdata ! Read buffer
  INTEGER(HSIZE_T), DIMENSION(2) :: data_dims = (/sdim,dim0/)
  INTEGER(SIZE_T), DIMENSION(4) :: str_len = (/7,7,5,7/)
  INTEGER :: i
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
  CALL H5Tset_strpad_f(filetype, H5T_STR_NULLPAD_F, hdferr)
  !
  ! Create dataspace.
  !
  CALL h5screate_simple_f(1, dims, space, hdferr)
  !
  ! Create the dataset and write the variable-length string data to
  ! it.
  !
  CALL h5dcreate_f(file, dataset, filetype, space, dset, hdferr)

  CALL h5dwrite_vl_f(dset, filetype, wdata, data_dims, str_len, hdferr, space)

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
  CALL h5dread_vl_f(dset, filetype, rdata, data_dims, str_len, hdferr, space)

  !
  ! Output the data to the screen.
  !
  DO i = 1, dims(1)
     WRITE(*,'(A,"(",I0,"): ",A)') DATASET, i, rdata(i)
  END DO

  DEALLOCATE(rdata)
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL H5Tclose_f(filetype, hdferr)
  CALL h5fclose_f(file , hdferr)

END PROGRAM main
