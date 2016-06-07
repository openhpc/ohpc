!************************************************************
!
!  This example shows how to read and write fixed string size
!  datatypes to a dataset.  The program first writes strings to a
!  dataset with a dataspace of DIM0, then closes the file.
!  Next, it reopens the file, reads back the data, and
!  outputs it to the screen.
!
!  This file is intended for use with HDF5 Library version 1.8
!  with --enable-fortran2003 
!
! ************************************************************/
PROGRAM main

  USE hdf5
  USE ISO_C_BINDING

  IMPLICIT NONE

  CHARACTER(LEN=20), PARAMETER :: filename  = "h5ex_t_string_F03.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset   = "DS1"
  INTEGER          , PARAMETER :: dim0      = 4
  INTEGER(SIZE_T)  , PARAMETER :: sdim      = 8 

  INTEGER(HID_T)  :: file, filetype, memtype, space, dset ! Handles
  INTEGER :: hdferr

  INTEGER(HSIZE_T), DIMENSION(1:1) :: dims = (/dim0/)
  INTEGER(HSIZE_T), DIMENSION(1:1) :: maxdims

  CHARACTER(LEN=sdim), DIMENSION(1:dim0), TARGET :: &
       wdata = (/"Parting", "is such", "sweet  ", "sorrow."/)
  CHARACTER(LEN=sdim), DIMENSION(:), ALLOCATABLE, TARGET :: rdata
  INTEGER :: i, len
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
  ! Create file datatypes.  For this example we will save
  ! the strings as FORTRAN strings
  !
  CALL H5Tcopy_f(H5T_FORTRAN_S1, filetype, hdferr)
  CALL H5Tset_size_f(filetype, sdim, hdferr)
  !
  ! Create dataspace.
  !
  CALL h5screate_simple_f(1, dims, space, hdferr)
  !
  ! Create the dataset and write the string data to it.
  !
  CALL h5dcreate_f(file, dataset, filetype, space, dset, hdferr)

  f_ptr = C_LOC(wdata(1)(1:1))
  CALL H5Dwrite_f(dset, filetype, f_ptr, hdferr);
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
  ! Open file and dataset.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f(file, dataset, dset, hdferr)
  !
  ! Get the datatype and its size.
  !
  CALL H5Dget_type_f(dset, filetype, hdferr)
  CALL H5Tget_size_f(filetype, size, hdferr)

  ! Make sure the declared length is large enough
  IF(size.GT.sdim)THEN
     PRINT*,'ERROR: Character LEN is to small'
     STOP
  ENDIF
  !
  ! Get dataspace.
  !
  CALL H5Dget_space_f(dset, space, hdferr)
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
  CALL H5Dread_f(dset, memtype, f_ptr, hdferr, space)
  !
  ! Output the data to the screen.
  !
  DO i = 1, dims(1)
     WRITE(*,'(A,"(",I0,"): ", A)') DATASET, i, TRIM(rdata(i))
  END DO
  !
  ! Close and release resources.
  !
  CALL H5Dclose_f(dset, hdferr)
  CALL H5Sclose_f(space, hdferr)
  CALL H5Tclose_f(memtype, hdferr)
  CALL H5Fclose_f(file, hdferr)

  DEALLOCATE(rdata)

END PROGRAM main
