!************************************************************
!
!  This example shows how to create and extend an unlimited
!  dataset.  The program first writes integers to a dataset
!  with dataspace dimensions of DIM0xDIM1, then closes the
!  file.  Next, it reopens the file, reads back the data,
!  outputs it to the screen, extends the dataset, and writes
!  new data to the entire extended dataset.  Finally it
!  reopens the file again, reads back the data, and outputs it
!  to the screen.
!
!  This file is intended for use with HDF5 Library version 1.8
!
!************************************************************
PROGRAM main

  USE HDF5
  IMPLICIT NONE

  CHARACTER(LEN=18), PARAMETER :: filename = "h5ex_d_unlimmod.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset  = "DS1"
  INTEGER          , PARAMETER :: dim0     = 4
  INTEGER          , PARAMETER :: dim1     = 7
  INTEGER          , PARAMETER :: edim0    = 6
  INTEGER          , PARAMETER :: edim1    = 10
  INTEGER          , PARAMETER :: chunk0   = 4
  INTEGER          , PARAMETER :: chunk1   = 4

  INTEGER(HID_T)  :: file, space, dset, dcpl ! Handles
  INTEGER         :: hdferr
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dims = (/dim0, dim1/)
  INTEGER(HSIZE_T), DIMENSION(1:2) :: chunk =(/chunk0, chunk1/)
  INTEGER(HSIZE_T), DIMENSION(1:2) :: extdims =(/edim0, edim1/)
  INTEGER(HSIZE_T), DIMENSION(1:2) :: maxdims

  INTEGER, DIMENSION(1:dim0, 1:dim1), TARGET :: wdata ! Write buffer
  INTEGER, DIMENSION(1:edim0, 1:edim1), TARGET :: wdata2 ! Write buffer for extension
  INTEGER :: i, j
  INTEGER(HSIZE_T), DIMENSION(1:2) :: ndims
  INTEGER, ALLOCATABLE, DIMENSION(:,:), TARGET :: rdata    ! Read buffer
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Initialize data.
  !
  DO i = 1, dim0
     DO j = 1, dim1
        wdata(i,j) = (i-2)*(j-1)
     ENDDO
  ENDDO
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create dataspace with unlimited dimensions.
  !
  maxdims = (/H5S_UNLIMITED_F, H5S_UNLIMITED_F/)
  CALL h5screate_simple_f(2, dims, space, hdferr, maxdims)
  !
  ! Create the dataset creation property list, and set the chunk
  ! size.
  !
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, hdferr)
  CALL h5pset_chunk_f(dcpl, 2, chunk, hdferr)
  !
  ! Create the unlimited dataset.
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
  ! In this next section we read back the data, extend the dataset,
  ! and write new data to the entire dataset.
  !
  !
  ! Open file and dataset using the default properties.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file, hdferr)
  CALL h5dopen_f (file, dataset, dset, hdferr)
  !
  ! Get dataspace and allocate memory for read buffer.  This is a
  ! two dimensional dataset so the dynamic allocation must be done
  ! in steps.
  !
  CALL h5dget_space_f(dset, space, hdferr)
  CALL h5sget_simple_extent_dims_f(space, dims, ndims, hdferr)
  !
  ! Allocate buffer for reading
  !
  ALLOCATE(rdata(1:dims(1),1:dims(2)))
  !
  ! Read the data using the default properties.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, dims, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '(/,"Dataset before extension:")')
  DO i=1, dim0
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80i3)', ADVANCE='NO') rdata(i,:)
     WRITE(*,'(" ]")')
  ENDDO
  !
  ! Extend the dataset.
  !
  CALL h5dset_extent_f(dset, extdims, hdferr)
  !
  ! Initialize data for writing to the extended dataset.
  !
  DO i = 1, edim0
     DO j = 1, edim1
        wdata2(i,j) = j-1
     ENDDO
  ENDDO
  !
  ! Write the data to the extended dataset.
  !
  CALL h5dwrite_f(dset, H5T_NATIVE_INTEGER, wdata2, extdims, hdferr)
  !
  ! Close and release resources.
  !
  DEALLOCATE(rdata)
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5fclose_f(file , hdferr)
  !
  ! Now we simply read back the data and output it to the screen.
  !
  !
  ! Open file and dataset using the default properties.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f (file, dataset, dset, hdferr)
  !
  ! Get dataspace and allocate memory for the read buffer as before.
  !
  CALL h5dget_space_f(dset, space, hdferr)
  CALL h5sget_simple_extent_dims_f(space, dims, ndims, hdferr)
  
  ALLOCATE( rdata(1:extdims(1),1:extdims(2)) )
  !
  ! Read the data using the default properties.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, dims, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '(/,"Dataset after extension:")')
  DO i=1, dims(1)
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80i3)', ADVANCE='NO') rdata(i,:)
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
