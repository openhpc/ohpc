!************************************************************
!
!  This example shows how to read and write enumerated
!  datatypes to an attribute.  The program first writes
!  enumerated values to an attribute with a dataspace of
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
  CHARACTER(LEN=21), PARAMETER :: filename  = "h5ex_t_enumatt_F03.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset   = "DS1"
  CHARACTER(LEN=2) , PARAMETER :: attribute = "A1"
  INTEGER          , PARAMETER :: dim0      = 4
  INTEGER          , PARAMETER :: dim1      = 7
  INTEGER(HID_T)               :: F_BASET  ! File base type
  INTEGER(HID_T)               :: M_BASET  ! Memory base type
  INTEGER(SIZE_T)  , PARAMETER :: NAME_BUF_SIZE = 16

! Enumerated type
  ENUM, BIND(C)
     ENUMERATOR :: SOLID = 0, LIQUID, GAS, PLASMA
  END ENUM

  INTEGER(HID_T) :: file, filetype, memtype, space, dset, attr ! Handles
  INTEGER :: hdferr

  INTEGER(hsize_t),   DIMENSION(1:2) :: dims = (/dim0, dim1/)
  INTEGER(kind(SOLID)), DIMENSION(1:dim0, 1:dim1), TARGET :: wdata ! Write buffer
  INTEGER(kind(SOLID)), DIMENSION(:,:), ALLOCATABLE, TARGET :: rdata ! Read buffer
  INTEGER(kind(SOLID)), TARGET :: val

  CHARACTER(LEN=6), DIMENSION(1:4) :: &
       names = (/"SOLID ", "LIQUID", "GAS   ", "PLASMA"/)

  CHARACTER(LEN=NAME_BUF_SIZE) :: name
  INTEGER(HSIZE_T), DIMENSION(1:2) :: maxdims
  INTEGER(kind(SOLID)) :: i, j
  TYPE(C_PTR) :: f_ptr
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Initialize DATA.
  !
  F_BASET   = H5T_STD_I16BE      ! File base type
  M_BASET   = h5kind_to_type(kind(SOLID), H5_INTEGER_KIND) ! Memory base type
  DO i = 1, dim0
     DO j = 1, dim1 
        wdata(i,j) = MOD( (j-1)*(i-1), PLASMA+1)
     ENDDO
  ENDDO
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create the enumerated datatypes for file and memory.  This
  ! process is simplified if native types are used for the file,
  ! as only one type must be defined.
  !
  CALL h5tenum_create_f(F_BASET, filetype, hdferr)
  CALL h5tenum_create_f(M_BASET, memtype, hdferr)

  DO i = SOLID, PLASMA
     !
     ! Insert enumerated value for memtype.
     !
     val = i
     CALL h5tenum_insert_f(memtype, TRIM(names(i+1)), val, hdferr)
     !
     ! Insert enumerated value for filetype.  We must first convert
     ! the numerical value val to the base type of the destination.
     !
     f_ptr = C_LOC(val)
     CALL h5tconvert_f(M_BASET, F_BASET, INT(1,SIZE_T), f_ptr, hdferr)
     CALL h5tenum_insert_f(filetype, TRIM(names(i+1)), val, hdferr)
  ENDDO
  !
  ! Create dataspace with a null dataspace.
  !
  CALL h5screate_f(H5S_NULL_F, space, hdferr)
  CALL h5dcreate_f(file, dataset, H5T_STD_I32LE, space, dset, hdferr)
  CALL h5sclose_f(space, hdferr)
  !
  ! Create dataspace.  Setting maximum size to be the current size.
  !
  CALL h5screate_simple_f(2, dims, space, hdferr)
  !
  ! Create the attribute and write the enumerated data to it.
  !
  CALL h5acreate_f(dset, attribute, filetype, space, attr, hdferr)
  f_ptr = C_LOC(wdata(1,1))
  CALL h5awrite_f(attr, memtype, f_ptr, hdferr)
  !
  ! Close and release resources.
  !
  CALL h5aclose_f(attr, hdferr)
  CALL h5dclose_f(dset, hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5tclose_f(filetype, hdferr)
  CALL h5fclose_f(file, hdferr)
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
  CALL h5aread_f( attr, memtype, f_ptr, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '(A,":")') attribute
  DO i=1, dims(1)
     WRITE(*,'(" [")', ADVANCE='NO')
     DO j = 1, dims(2)
        !
        ! Get the name of the enumeration member.
        !
        CALL h5tenum_nameof_f( memtype, rdata(i,j), NAME_BUF_SIZE, name, hdferr) 
        WRITE(*,'(" ",A6," ")', ADVANCE='NO') TRIM(NAME)
     ENDDO
     WRITE(*,'("]")')
  ENDDO
  !
  ! Close and release resources.
  !
  DEALLOCATE(rdata)
  CALL h5aclose_f(attr , hdferr)
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5tclose_f(memtype, hdferr)
  CALL h5fclose_f(file , hdferr)
END PROGRAM main
