!************************************************************
!
!  This example shows how to read and write array datatypes
!  to an attribute.  The program first writes integers arrays
!  of dimension ADIM0xADIM1 to an attribute with a dataspace
!  of DIM0, then closes the  file.  Next, it reopens the
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

  CHARACTER(LEN=22), PARAMETER :: filename  = "h5ex_t_arrayatt_F03.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset   = "DS1"
  CHARACTER(LEN=2) , PARAMETER :: attribute = "A1"
  INTEGER          , PARAMETER :: dim0      = 4
  INTEGER          , PARAMETER :: adim0     = 3
  INTEGER          , PARAMETER :: adim1     = 5

  INTEGER(HID_T)  :: file, filetype, memtype, space, dset, attr ! Handles
  INTEGER :: hdferr
  INTEGER(HSIZE_T), DIMENSION(1:1)   :: dims = (/dim0/)
  INTEGER(HSIZE_T), DIMENSION(1:2)   :: adims = (/adim0, adim1/)
  INTEGER(HSIZE_T), DIMENSION(1:2)   :: maxdims
  INTEGER, DIMENSION(1:dim0, 1:adim0, 1:adim1), TARGET :: wdata ! Write buffer 
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE, TARGET :: rdata    ! Read buffer
  INTEGER :: i, j, k
  TYPE(C_PTR) :: f_ptr
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Initialize data.  i is the element in the dataspace, j and k the
  ! elements within the array datatype.
  !
  DO i = 1, dim0
     DO j = 1, adim0
        DO k = 1, adim1
           wdata(i,j,k) = (i-1)*(j-1)-(j-1)*(k-1)+(i-1)*(k-1)
        ENDDO
     ENDDO
  ENDDO
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create array datatypes for file and memory.
  !
  CALL H5Tarray_create_f(INT(H5T_STD_I64LE, HID_T), 2, adims, filetype, hdferr)
  CALL H5Tarray_create_f(H5T_NATIVE_INTEGER, 2, adims, memtype, hdferr)
  !
  ! Create dataset with a null dataspace.
  !
  CALL H5Screate_f(H5S_NULL_F, space, hdferr)
  CALL h5dcreate_f(file, dataset, H5T_STD_I32LE, space, dset, hdferr)
  CALL H5Sclose_f(space, hdferr)
  !
  ! Create dataspace.  Setting maximum size to be the current size.
  !
  CALL h5screate_simple_f(1, dims, space, hdferr)
  !
  ! Create the attribute and write the array data to it.
  !
  CALL H5Acreate_f(dset, attribute, filetype, space, attr, hdferr)
  f_ptr = C_LOC(wdata)
  CALL H5Awrite_f(attr, memtype, f_ptr, hdferr)
  !
  ! Close and release resources.
  !
  CALL H5Aclose_f(attr, hdferr)
  CALL H5Dclose_f(dset, hdferr)
  CALL H5Sclose_f(space, hdferr)
  CALL H5Tclose_f(filetype, hdferr)
  CALL H5Tclose_f(memtype, hdferr)
  CALL H5Fclose_f(file, hdferr)
  !
  ! Now we begin the read section of this example. 
  !
  ! Open file, dataset, and attribute.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f(file, dataset, dset, hdferr)
  CALL h5aopen_f(dset, attribute, attr, hdferr)
  !
  ! Get the datatype and its dimensions.
  !
  CALL H5Aget_type_f(attr, filetype, hdferr)
  CALL H5Tget_array_dims_f(filetype, adims, hdferr)
  !
  ! Get dataspace and allocate memory for read buffer.  This is a
  ! three dimensional attribute when the array datatype is included.
  !
  CALL H5Aget_space_f(attr, space, hdferr)
  CALL H5Sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
  ALLOCATE(rdata(1:dims(1),1:adims(1),1:adims(2)))
  !
  ! Create the memory datatype.
  ! 
  CALL H5Tarray_create_f(H5T_NATIVE_INTEGER, 2, adims, memtype, hdferr)
  !
  ! Read the data.
  !
  f_ptr = C_LOC(rdata)
  CALL H5Aread_f(attr, memtype, f_ptr, hdferr)
  !
  ! Output the data to the screen.
  !
  DO i = 1, dims(1)
     WRITE(*,'(A,"[",i1,"]:")') attribute,i-1
     DO j=1, adim0
        WRITE(*,'(" [")', ADVANCE='NO')
        WRITE(*,'(80i3)', ADVANCE='NO') rdata(i,j,:)
        WRITE(*,'(" ]")')
     ENDDO
  ENDDO
  !
  ! Close and release resources.
  !
  DEALLOCATE(rdata)
  CALL H5Aclose_f(attr, hdferr)
  CALL H5Dclose_f(dset, hdferr)
  CALL H5Sclose_f(space, hdferr)
  CALL H5Tclose_f(filetype, hdferr)
  CALL H5Tclose_f(memtype, hdferr)
  CALL H5Fclose_f(file, hdferr)

END PROGRAM main
