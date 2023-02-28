!************************************************************
!
!  This example shows how to read and write variable-length
!  datatypes to a dataset.  The program first writes two
!  variable-length integer arrays to a dataset then closes
!  the file.  Next, it reopens the file, reads back the data,
!  and outputs it to the screen.
!
!  The data structure is a matrix which is has 2 rows
!  and the number of columns varies in each row, for 
!  this example row 1 has LEN0 columns and row 2 has LEN1 columns
!
!  This file is intended for use with HDF5 Library version 1.8
!  with --enable-fortran2003 
!
!************************************************************

PROGRAM main

  USE HDF5
  USE ISO_C_BINDING
  
  IMPLICIT NONE

  CHARACTER(LEN=18), PARAMETER :: filename  = "h5ex_t_vlen_F03.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset   = "DS1"
  INTEGER, PARAMETER :: LEN0 = 3
  INTEGER, PARAMETER :: LEN1 = 12

  INTEGER(HID_T)  :: file, filetype, memtype, space, dset ! Handles
  INTEGER :: hdferr
  INTEGER(HSIZE_T), DIMENSION(1:2)   :: maxdims
  INTEGER :: i, j

  ! vl data
  TYPE vl
     INTEGER, DIMENSION(:), POINTER :: data
  END TYPE vl
  TYPE(vl), DIMENSION(:), ALLOCATABLE :: ptr

  TYPE(hvl_t), DIMENSION(1:2), TARGET :: wdata ! Array of vlen structures
  TYPE(hvl_t), DIMENSION(1:2), TARGET :: rdata ! Pointer to vlen structures

  INTEGER(hsize_t), DIMENSION(1:1) :: dims = (/2/)
  INTEGER, DIMENSION(:), POINTER :: ptr_r 
  TYPE(C_PTR) :: f_ptr
  
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Initialize variable-length data.  wdata(1) is a countdown of
  ! length LEN0, wdata(2) is a Fibonacci sequence of length LEN1.
  !
  wdata(1)%len = LEN0
  wdata(2)%len = LEN1

  ALLOCATE( ptr(1:2) )
  ALLOCATE( ptr(1)%data(1:wdata(1)%len) )
  ALLOCATE( ptr(2)%data(1:wdata(2)%len) )

  DO i=1, wdata(1)%len
     ptr(1)%data(i) = wdata(1)%len - i + 1 ! 3 2 1
  ENDDO
  wdata(1)%p = C_LOC(ptr(1)%data(1))

  ptr(2)%data(1:2) = 1
  DO i = 3, wdata(2)%len
     ptr(2)%data(i) = ptr(2)%data(i-1) + ptr(2)%data(i-2) ! (1 1 2 3 5 8 etc.)
  ENDDO
  wdata(2)%p = C_LOC(ptr(2)%data(1))

  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create variable-length datatype for file and memory.
  !
  CALL h5tvlen_create_f(H5T_STD_I32LE, filetype, hdferr)
  CALL h5tvlen_create_f(H5T_NATIVE_INTEGER, memtype, hdferr)
  !
  ! Create dataspace.
  !
  CALL h5screate_simple_f(1, dims, space, hdferr)
  !
  ! Create the dataset and write the variable-length data to it.
  !
  CALL h5dcreate_f(file, dataset, filetype, space, dset, hdferr)
 
  f_ptr = C_LOC(wdata(1))
  CALL h5dwrite_f(dset, memtype, f_ptr, hdferr)

  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5tclose_f(filetype, hdferr)
  CALL h5tclose_f(memtype, hdferr)
  CALL h5fclose_f(file , hdferr)
  DEALLOCATE(ptr)

  !
  ! Now we begin the read section of this example.

  !
  ! Open file and dataset.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f(file, dataset, dset, hdferr)

  !
  ! Get dataspace and allocate memory for array of vlen structures.
  ! This does not actually allocate memory for the vlen data, that
  ! will be done by the library.
  !
  CALL h5dget_space_f(dset, space, hdferr)
  CALL h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr) 

  !
  ! Create the memory datatype.
  !
  CALL h5tvlen_create_f(H5T_NATIVE_INTEGER, memtype, hdferr)

  !
  ! Read the data.
  !
  f_ptr = C_LOC(rdata(1))
  CALL h5dread_f(dset, memtype, f_ptr, hdferr)
  !
  ! Output the variable-length data to the screen.
  !
  DO i = 1, dims(1)
     WRITE(*,'(A,"(",I0,"):",/,"{")', ADVANCE="no") dataset,i
     CALL c_f_pointer(rdata(i)%p, ptr_r, [rdata(i)%len] )
     DO j = 1, rdata(i)%len
        WRITE(*,'(1X,I0)', ADVANCE='no') ptr_r(j)
        IF ( j .LT. rdata(i)%len) WRITE(*,'(",")', ADVANCE='no')
     ENDDO
     WRITE(*,'( " }")')
  ENDDO

  !
  ! Close and release resources.  Note the use of H5Dvlen_reclaim
  ! removes the need to manually deallocate the previously allocated
  ! data.
  !
  CALL h5dvlen_reclaim_f(memtype, space, H5P_DEFAULT_F, f_ptr, hdferr)
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5tclose_f(memtype, hdferr)
  CALL h5fclose_f(file , hdferr)

END PROGRAM main
