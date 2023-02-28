!************************************************************
!
!  This example shows how to read and write data to a dataset
!  using a data transform expression.  The program first
!  writes integers to a dataset using the transform
!  expression TRANSFORM, then closes the file.  Next, it
!  reopens the file, reads back the data without a transform,
!  and outputs the data to the screen.  Finally it reads the
!  data using the transform expression RTRANSFORM and outputs
!  the results to the screen.
!
!  This file is intended for use with HDF5 Library verion 1.8
!
!************************************************************
PROGRAM main

  USE HDF5

  IMPLICIT NONE

  CHARACTER(LEN=19), PARAMETER :: filename = "h5ex_d_transform.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset  = "DS1"
  INTEGER          , PARAMETER :: dim0     = 4
  INTEGER          , PARAMETER :: dim1     = 7
  CHARACTER(LEN=3) , PARAMETER :: transform = "x+1"
  CHARACTER(LEN=3) , PARAMETER :: rtransform = "x-1"

  INTEGER(HID_T)  :: file, space, dset, dxpl ! Handles
  INTEGER         :: hdferr
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dims = (/dim0, dim1/)
  INTEGER, DIMENSION(1:dim0, 1:dim1) :: wdata, & ! Write buffer 
                                        rdata    ! Read buffer
  INTEGER :: i, j
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
  ! Output the data to the screen.
  !
  WRITE(*, '("Original Data:")')
  DO i=1, dim0
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80i3)', ADVANCE='NO') wdata(i,:)
     WRITE(*,'(" ]")')
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
  ! Create the dataset transfer property list and define the
  ! transform expression.
  !
  CALL h5pcreate_f(H5P_DATASET_XFER_F, dxpl, hdferr)
  CALL h5pset_data_transform_f (dxpl, transform, hdferr)
  !
  ! Create the dataset using the default properties.  Unfortunately
  ! we must save as a native type or the transform operation will
  ! fail.
  !
  CALL h5dcreate_f(file, dataset, H5T_NATIVE_INTEGER, space, dset, hdferr)
  !
  ! Write the data to the dataset using the dataset transfer
  ! property list.
  !
  CALL h5dwrite_f(dset, H5T_NATIVE_INTEGER, wdata, dims, hdferr,xfer_prp=dxpl)
  !
  ! Close and release resources.
  !
  CALL h5pclose_f(dxpl , hdferr)
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5fclose_f(file , hdferr)
  !
  ! Now we begin the read section of this example.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f (file, dataset, dset, hdferr)
  !
  ! Read the data using the default properties.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, dims, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '(/,"Data as written with transform ",A,":")') transform
  DO i=1, dim0
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80i3)', ADVANCE='NO') rdata(i,:)
     WRITE(*,'(" ]")')
  ENDDO
  !
  ! Create the dataset transfer property list and define the
  ! transform expression.
  !
  CALL h5pcreate_f(H5P_DATASET_XFER_F, dxpl, hdferr)
  CALL h5pset_data_transform_f (dxpl, rtransform, hdferr)
  !
  ! Read the data using the dataset transfer property list.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, dims, hdferr,xfer_prp=dxpl)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '(/,"Data as written with transform ",A," and read with transform ",A,":")') &
       transform, rtransform
  DO i=1, dim0
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80i3)', ADVANCE='NO') rdata(i,:)
     WRITE(*,'(" ]")')
  ENDDO
  !
  ! Close and release resources.
  !
  CALL h5pclose_f(dxpl , hdferr)
  CALL h5dclose_f(dset , hdferr)
  CALL h5fclose_f(file , hdferr)

END PROGRAM main
