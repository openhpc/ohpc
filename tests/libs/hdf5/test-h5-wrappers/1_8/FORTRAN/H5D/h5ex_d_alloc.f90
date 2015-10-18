! ***********************************************************
!
!  this example shows how to set the space allocation time
!  for a dataset.  the program first creates two datasets,
!  one with the default allocation time (late) and one with
!  early allocation time, and displays whether each has been
!  allocated and their allocation size.  next, it writes data
!  to the datasets, and again displays whether each has been
!  allocated and their allocation size.
!
!  this file is intended for use with hdf5 library verion 1.8
!
! ***********************************************************

PROGRAM main

  USE HDF5

  IMPLICIT NONE

  CHARACTER(LEN=15), PARAMETER :: filename = "h5ex_d_alloc.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset1 = "DS1"
  CHARACTER(LEN=3) , PARAMETER :: dataset2 = "DS2"
  INTEGER          , PARAMETER :: dim0     = 4
  INTEGER          , PARAMETER :: dim1     = 7
  INTEGER          , PARAMETER :: fillval  = 99
  INTEGER          , PARAMETER :: rank     = 2

  INTEGER :: space_status
  INTEGER :: i, j
  INTEGER :: hdferr
  INTEGER(HID_T)   :: file, space, dset1, dset2, dcpl ! handles
  INTEGER(HSIZE_T) :: storage_size
  INTEGER(HSIZE_T), DIMENSION(1:2)           :: dims = (/dim0, dim1/) ! size write buffer
  INTEGER         , DIMENSION(1:dim0,1:dim1) :: wdata ! write buffer
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Initialize data.
  !
  DO i = 1, dim0
     DO j = 1, dim1
        wdata(i,j) = i*j-j
     ENDDO
  ENDDO
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create dataspace. 
  !
  CALL h5screate_simple_f(rank, dims, space, hdferr)
  !
  ! Create the dataset creation property list, and set the chunk size.
  !
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, hdferr)
  !
  ! Set the allocation time to "early".  This way we can be sure
  ! that reading from the dataset immediately after creation will
  ! return the fill value.
  !
  CALL h5pset_alloc_time_f(dcpl, H5D_ALLOC_TIME_EARLY_F,hdferr)
  !
  WRITE(*,'(/,"Creating datasets...",/)') 
  WRITE(*,'(A," has allocation time H5D_ALLOC_TIME_LATE_F")') dataset1
  WRITE(*,'(A," has allocation time H5D_ALLOC_TIME_EARLY_F"/)') dataset2
  !
  ! Create the dataset using the dataset creation property list.
  !
  CALL h5dcreate_f(file, dataset1, H5T_STD_I32LE, space, dset1, hdferr)
  CALL h5dcreate_f(file, dataset2, H5T_STD_I32LE, space, dset2, hdferr, dcpl)
  ! 
  ! Retrieve and print space status and storage size for dset1.
  !
  storage_size = 0
  CALL h5dget_space_status_f(dset1, space_status, hdferr)
  CALL h5dget_storage_size_f(dset1, storage_size, hdferr)
  !
  IF(space_status.EQ.H5D_SPACE_STS_ALLOCATED_F)THEN
     WRITE(*,'("Space for ",A," has been allocated.")') dataset1
  ELSE
     WRITE(*,'("Space for ",A," has not been allocated.")') dataset1
  END IF
  WRITE(*,'("Storage size for ",A," is: ",I3," bytes.")') dataset1, storage_size
  !
  ! Retrieve and print space status and storage size for dset2.
  !
  CALL h5dget_space_status_f(dset2, space_status, hdferr)
  CALL h5dget_storage_size_f(dset2, storage_size, hdferr)
  !
  IF(space_status.EQ.H5D_SPACE_STS_ALLOCATED_F)THEN
     WRITE(*,'("Space for ",A," has been allocated.")') dataset2
  ELSE
     WRITE(*,'("Space for ",A," has not been allocated.")') dataset2
  END IF
  WRITE(*,'("Storage size for ",A," is: ",I3," bytes.")') dataset2, storage_size
  !
  WRITE(*,'(/,"Writing data...",/)')
  !
  ! Write the data to the datasets.
  !
  CALL h5dwrite_f(dset1, H5T_NATIVE_INTEGER, wdata, dims, hdferr)
  CALL h5dwrite_f(dset2, H5T_NATIVE_INTEGER, wdata, dims, hdferr)
  !
  ! Retrieve and print space status and storage size for dset1.
  !
  CALL h5dget_space_status_f(dset1, space_status, hdferr)
  CALL h5dget_storage_size_f(dset1, storage_size, hdferr)
  !
  IF(space_status.EQ.H5D_SPACE_STS_ALLOCATED_F)THEN
     WRITE(*,'("Space for ",A," has been allocated.")') dataset1
  ELSE
     WRITE(*,'("Space for ",A," has not been allocated.")') dataset1
  END IF
  WRITE(*,'("Storage size for ",A," is: ",I3," bytes.")') dataset1, storage_size
  !
  ! Retrieve and print space status and storage size for dset2.
  !
  CALL h5dget_space_status_f(dset2, space_status, hdferr)
  CALL h5dget_storage_size_f(dset2, storage_size, hdferr)
  !
  IF(space_status.EQ.H5D_SPACE_STS_ALLOCATED_F)THEN
     WRITE(*,'("Space for ",A," has been allocated.")') dataset2
  ELSE
     WRITE(*,'("Space for ",A," has not been allocated.")') dataset2
  END IF
  WRITE(*,'("Storage size for ",A," is: ",I3," bytes.",/)') dataset2, storage_size
  !
  ! Close and release resources.
  !
  CALL h5pclose_f(dcpl , hdferr)
  CALL h5dclose_f(dset1, hdferr)
  CALL h5dclose_f(dset2, hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5fclose_f(file , hdferr)
  !
END PROGRAM main
