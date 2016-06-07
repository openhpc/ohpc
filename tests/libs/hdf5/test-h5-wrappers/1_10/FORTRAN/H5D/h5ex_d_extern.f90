!************************************************************
!
!  This example shows how to read and write data to an
!  external dataset.  The program first writes integers to an
!  external dataset with dataspace dimensions of DIM0xDIM1,
!  then closes the file.  Next, it reopens the file, reads
!  back the data, and outputs the name of the external data
!  file and the data to the screen.
!
!  This file is intended for use with HDF5 Library verion 1.8
!
! ************************************************************/

PROGRAM main

  USE HDF5

  IMPLICIT NONE

  CHARACTER(LEN=16), PARAMETER :: filename     = "h5ex_d_extern.h5"
  CHARACTER(LEN=18), PARAMETER :: externalname = "h5ex_d_extern.data"
  CHARACTER(LEN=3) , PARAMETER :: dataset  = "DS1"
  INTEGER          , PARAMETER :: dim0     = 4
  INTEGER          , PARAMETER :: dim1     = 7
  INTEGER(SIZE_T)  , PARAMETER :: name_buf_size = 32

  INTEGER :: hdferr
  INTEGER :: layout
  INTEGER(HID_T)  :: file, space, dset, dcpl ! Handles
  INTEGER(HSIZE_T), DIMENSION(1:2)   :: dims = (/dim0, dim1/)
  INTEGER(HSIZE_T), DIMENSION(1:2)   :: start, stride, count, block

  INTEGER, DIMENSION(1:dim0, 1:dim1) :: wdata, & ! Write buffer 
                                        rdata    ! Read buffer
  CHARACTER(LEN=name_buf_size) :: name
  INTEGER :: i, j
  INTEGER(OFF_T) :: offset ! Offset, in bytes, from thebeginning of the file to the 
                           ! location in the file where the data starts.
  INTEGER(HSIZE_T) :: bytes ! Number of bytes reserved in the file for the data
  INTEGER(SIZE_T) :: int_size ! size of integer
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Initialize data.
  !
  DO i = 1, dim0
     DO j = 1, dim1
        wdata(i,j) = (i-1)*(j-1)-(j-1)
     ENDDO
  ENDDO
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create dataspace.  Setting maximum size to NULL sets the maximum
  ! size to be the current size.
  !
  CALL h5screate_simple_f(2, dims, space, hdferr)
  !
  ! Create the dataset creation property list, set the external
  ! file.
  !
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, hdferr)
  ! for HDF5 versions <= 1.8.2 use:
  CALL h5tget_size_f(H5T_NATIVE_INTEGER, int_size, hdferr)
  bytes = int_size*dim0*dim1
  ! else use:
  !   bytes = INT(H5F_UNLIMITED_F,HSIZE_T)
  CALL h5pset_external_f(dcpl, externalname, INT(0,OFF_T), bytes, hdferr)
  !
  ! Create the external dataset.
  !
  CALL h5dcreate_f(file, dataset, H5T_STD_I32LE, space, dset, hdferr, dcpl)
  !
  ! Write the data to the dataset.
  ! 
  CALL h5dwrite_f(dset, H5T_NATIVE_INTEGER, wdata, dims, hdferr)
  !
  ! Close and release resources.
  ! 
  CALL h5pclose_f(dcpl , hdferr)
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5fclose_f(file , hdferr)
  !
  ! Now we begin the read section of this example.
  !
  !
  ! Open file and dataset using the default properties.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f(file, dataset, dset, hdferr)
  !
  ! Retrieve dataset creation property list.
  !
  CALL H5Dget_create_plist_f(dset, dcpl, hdferr)
  !
  ! Retrieve and print the name of the external file.  Here we
  ! manually set the last field in name to null, in case the name of
  ! the file is longer than the buffer.
  !
  CALL H5Pget_external_f (dcpl, 0, name_buf_size, name, offset, bytes, hdferr)
  WRITE(*,'(A," is stored in file: ",A)')  dataset, TRIM(name)
  !
  ! Read the data using the default properties.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, dims, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '(A,":")') dataset
  DO i=1, dim0
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80i3)', ADVANCE='NO') rdata(i,:)
     WRITE(*,'(" ]")')
  ENDDO
  !
  ! Close and release resources.
  !
  CALL h5pclose_f(dcpl , hdferr)
  CALL h5dclose_f(dset , hdferr)
  CALL h5fclose_f(file , hdferr)

END PROGRAM main
