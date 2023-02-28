!************************************************************
!
!  This example shows how to read and write REAL datatypes
!  (using SELECTED_REAL_KIND) to a dataset.  The program first 
!  writes REAL datatypes to a dataset with a dataspace of 
!  DIM0xDIM1, then closes the file.  Next, it reopens the file, 
!  reads back the REAL data, and outputs it to the screen.
!
!  This file is intended for use with HDF5 Library version 1.8
!  with --enable-fortran2003
!
!************************************************************
PROGRAM main

  USE HDF5
  USE ISO_C_BINDING

  IMPLICIT NONE
  ! This should map to REAL*8 on most modern processors
  INTEGER, PARAMETER :: real_kind_15 = SELECTED_REAL_KIND(15,307)

  CHARACTER(LEN=19), PARAMETER :: filename  = "h5ex_t_float_F03.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset   = "DS1"
  INTEGER          , PARAMETER :: dim0      = 4
  INTEGER          , PARAMETER :: dim1      = 7

  INTEGER(HID_T)  :: file, space, dset ! Handles
  INTEGER :: hdferr

  INTEGER(hsize_t),   DIMENSION(1:2) :: dims = (/dim0, dim1/)
  REAL(KIND=real_kind_15), DIMENSION(1:dim0, 1:dim1), TARGET :: wdata ! Write buffer
  REAL(KIND=real_kind_15), DIMENSION(:,:), ALLOCATABLE, TARGET :: rdata ! Read buffer
  INTEGER(HSIZE_T), DIMENSION(1:2) :: maxdims
  INTEGER :: i, j
  TYPE(C_PTR) :: f_ptr
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Initialize DATA.
  !
  DO i = 1, dim0
     DO j = 1, dim1
        wdata(i,j) = REAL(i-1,real_kind_15) / ( REAL(j-1,real_kind_15)+0.5_real_kind_15) + j-1
     ENDDO
  ENDDO
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create dataspace.  Setting maximum size to be the current size.
  !
  CALL h5screate_simple_f(2, dims, space, hdferr)
  !
  ! Create the dataset and write the floating point data to it.  In
  ! this example we will save the data as 64 bit little endian IEEE
  ! floating point numbers, regardless of the native type.  The HDF5
  ! library automatically converts between different floating point
  ! types.
  !
  CALL h5dcreate_f(file, dataset, H5T_IEEE_F64LE, space, dset, hdferr)
  f_ptr = C_LOC(wdata(1,1))
  CALL h5dwrite_f(dset, H5T_NATIVE_DOUBLE, f_ptr, hdferr)
  !
  ! Close and release resources.
  !
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5fclose_f(file , hdferr)
  !
  ! Open file and dataset.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f (file, dataset, dset, hdferr)
  !
  ! Get dataspace and allocate memory for read buffer.
  !
  CALL h5dget_space_f(dset,space, hdferr)
  CALL h5sget_simple_extent_dims_f (space, dims, maxdims, hdferr)

  ALLOCATE(rdata(1:dims(1),1:dims(2)))
  !
  ! Read the data.
  !
  f_ptr = C_LOC(rdata(1,1))
  CALL h5dread_f( dset, H5T_NATIVE_DOUBLE, f_ptr, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '(A,":")') dataset
  DO i=1, dims(1)
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80(" ",f9.4))', ADVANCE='NO') rdata(i,1:dims(2))
     WRITE(*,'(" ]")')
  ENDDO
  !
  ! Close and release resources.
  !
  DEALLOCATE(rdata)
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5fclose_f(file , hdferr)
END PROGRAM main
