!************************************************************
!
!  This example shows how to set the fill value for a
!  dataset.  The program first sets the fill value to
!  FILLVAL, creates a dataset with dimensions of DIM0xDIM1,
!  reads from the uninitialized dataset, and outputs the
!  contents to the screen.  Next, it writes integers to the
!  dataset, reads the data back, and outputs it to the
!  screen.  Finally it extends the dataset, reads from it,
!  and outputs the result to the screen.
!
!  This file is intended for use with HDF5 Library verion 1.8
!
! ************************************************************/
PROGRAM main

  USE HDF5

  IMPLICIT NONE

  CHARACTER(LEN=17), PARAMETER :: filename = "h5ex_d_fillval.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset  = "DS1"
  INTEGER          , PARAMETER :: dim0     = 4
  INTEGER          , PARAMETER :: dim1     = 7
  INTEGER          , PARAMETER :: edim0    = 6
  INTEGER          , PARAMETER :: edim1    = 10
  INTEGER          , PARAMETER :: chunk0   = 4
  INTEGER          , PARAMETER :: chunk1   = 4
  INTEGER          , PARAMETER :: fillval  = 99

  INTEGER :: hdferr
  INTEGER(HID_T)  :: file, space, dset, dcpl ! Handles
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dims    = (/dim0, dim1/)
  INTEGER(HSIZE_T), DIMENSION(1:2) :: extdims = (/edim0, edim1/)
  INTEGER(HSIZE_T), DIMENSION(1:2) :: maxdims
  INTEGER(HSIZE_T), DIMENSION(1:2) :: chunk   = (/chunk0, chunk1/)

  INTEGER, DIMENSION(1:dim0, 1:dim1)   :: wdata, & ! Write buffer 
                                          rdata    ! Read buffer
  INTEGER, DIMENSION(1:edim0, 1:edim1) :: rdata2   ! Read buffer for extension
  INTEGER :: i, j
  INTEGER :: fillvall
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Initialize data.
  !
  maxdims = (/H5S_UNLIMITED_F, H5S_UNLIMITED_F/)

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
  ! Create dataspace with unlimited dimensions.
  !
  CALL h5screate_simple_f(2, dims, space, hdferr, maxdims)
  !  
  ! Create the dataset creation property list, and set the chunk
  ! size.
  !
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, hdferr)
  CALL h5pset_chunk_f(dcpl, 2, chunk, hdferr)
  !
  ! Set the fill value for the dataset.
  !
  CALL h5pset_fill_value_f(dcpl, H5T_NATIVE_INTEGER, fillval, hdferr)
  !
  ! Set the allocation time to "early".  This way we can be sure
  ! that reading from the dataset immediately after creation will
  ! return the fill value.
  !
  CALL h5pset_alloc_time_f(dcpl, H5D_ALLOC_TIME_EARLY_F,hdferr)
  !
  ! Create the dataset using the dataset creation property list.
  !
  CALL h5dcreate_f(file, dataset, H5T_STD_I32LE, space, dset, hdferr, dcpl)
  !
  ! Read values from the dataset, which has not been written to yet.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, dims, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '("Dataset before being written to:")')
  DO i=1, dim0
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80i3)', ADVANCE='NO') rdata(i,:)
     WRITE(*,'(" ]")')
  ENDDO
  !
  ! Write the data to the dataset.
  !
  CALL h5dwrite_f(dset, H5T_NATIVE_INTEGER, wdata, dims, hdferr, file_space_id=space)
  !
  ! Read the data back.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, dims, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '("Dataset after being written to:")')
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
  ! Read from the extended dataset.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata2, extdims, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '("Dataset after extension:")')
  DO i=1, extdims(1)
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80i3)', ADVANCE='NO') rdata2(i,:)
     WRITE(*,'(" ]")')
  ENDDO
  !
  ! Close and release resources.
  !
  CALL h5pclose_f(dcpl , hdferr)
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5fclose_f(file , hdferr)

END PROGRAM main
