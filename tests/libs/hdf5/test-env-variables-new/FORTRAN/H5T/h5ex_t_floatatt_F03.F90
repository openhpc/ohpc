!************************************************************
!
!  This example shows how to read and write floating point
!  datatypes to an attribute.  The program first writes
!  floating point numbers to an attribute with a dataspace of
!  DIM0xDIM1, then closes the file.  Next, it reopens the
!  file, reads back the data, and outputs it to the screen.
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

  CHARACTER(LEN=22), PARAMETER :: filename  = "h5ex_t_floatatt_F03.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset   = "DS1"
  CHARACTER(LEN=2) , PARAMETER :: attribute = "A1"
  INTEGER          , PARAMETER :: dim0      = 4
  INTEGER          , PARAMETER :: dim1      = 7

  INTEGER(HID_T)  :: file, space, dset, attr ! Handles
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
  ! Create dataspace with a null dataspace.
  !
  CALL H5Screate_f(H5S_NULL_F, space, hdferr)
  CALL h5dcreate_f(file, dataset, H5T_STD_I32LE, space, dset, hdferr)
  CALL h5sclose_f(space, hdferr)
  !
  ! Create dataspace.  Setting maximum size to be the current size.
  !
  CALL h5screate_simple_f(2, dims, space, hdferr)
  !
  ! Create the attribute and write the floating point data to it.
  ! In this example we will save the data as 64 bit little endian
  ! IEEE floating point numbers, regardless of the native type.  The
  ! HDF5 library automatically converts between different floating
  ! point types.
  !
  CALL H5Acreate_f(dset, attribute, H5T_IEEE_F64LE, space, attr, hdferr)
  f_ptr = C_LOC(wdata(1,1))
  CALL H5Awrite_f(attr, H5T_NATIVE_DOUBLE, f_ptr, hdferr)
  !
  ! Close and release resources.
  !
  CALL H5Aclose_f(attr, hdferr)
  CALL H5Dclose_f(dset, hdferr)
  CALL H5Fclose_f(file, hdferr)
  !
  ! Now we begin the read section of this example.
  !
  ! Open file and dataset, and attribute.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f(file, dataset, dset, hdferr)
  CALL h5aopen_f(dset, attribute, attr, hdferr)
  !
  ! Get dataspace and allocate memory for read buffer.
  !
  CALL h5aget_space_f(attr, space, hdferr)
  CALL h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)

  ALLOCATE(rdata(1:dims(1),1:dims(2)))
  !
  ! Read the data.
  !
  f_ptr = C_LOC(rdata(1,1))
  CALL h5aread_f( attr, H5T_NATIVE_DOUBLE, f_ptr, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '(A,":")') attribute
  DO i=1, dims(1)
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80(" ",f9.4))', ADVANCE='NO') rdata(i,1:dims(2))
     WRITE(*,'(" ]")')
  ENDDO
  !
  ! Close and release resources.
  !
  DEALLOCATE(rdata)
  CALL h5aclose_f(attr , hdferr)
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5fclose_f(file , hdferr)
END PROGRAM main

