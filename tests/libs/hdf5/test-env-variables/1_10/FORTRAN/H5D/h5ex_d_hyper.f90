!************************************************************
!
!  This example shows how to read and write data to a
!  dataset by hyberslabs.  The program first writes integers
!  in a hyperslab selection to a dataset with dataspace
!  dimensions of DIM0xDIM1, then closes the file.  Next, it
!  reopens the file, reads back the data, and outputs it to
!  the screen.  Finally it reads the data again using a
!  different hyperslab selection, and outputs the result to
!  the screen.
!
!  This file is intended for use with HDF5 Library verion 1.8
!
! ************************************************************/
PROGRAM main

  USE HDF5

  IMPLICIT NONE

  CHARACTER(LEN=15), PARAMETER :: filename = "h5ex_d_hyper.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset  = "DS1"
  INTEGER          , PARAMETER :: dim0     = 6
  INTEGER          , PARAMETER :: dim1     = 8

  INTEGER(HID_T)  :: file, space, dset ! Handles
  INTEGER         :: hdferr
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dims = (/dim0, dim1/)
  INTEGER(HSIZE_T), DIMENSION(1:2)   :: start, stride, count, block

  INTEGER, DIMENSION(1:dim0, 1:dim1) :: wdata, & ! Write buffer 
                                        rdata    ! Read buffer
  INTEGER :: i, j

  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  ! Initialize data to "1", to make it easier to see the selections.
  !
  wdata = 1
  !
  ! Print the data to the screen.
  !
  WRITE(*, '(/,"Original Data:")')
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
  ! Create dataspace.  Setting maximum size to be the current size.
  !
  CALL h5screate_simple_f(2, dims, space, hdferr)
  ! Create the dataset.  We will use all default properties for this
  ! example.
  !
  CALL h5dcreate_f(file, dataset, H5T_STD_I32LE, space, dset, hdferr)
  !
  ! Define and select the first part of the hyperslab selection.
  !  
  start = 0
  stride = 3
  count(1:2) = (/2,3/)
  block = 2
  CALL h5sselect_hyperslab_f (space, H5S_SELECT_SET_F, start, count, &
       hdferr, stride, block)
  !
  ! Define and select the second part of the hyperslab selection,
  ! which is subtracted from the first selection by the use of
  ! H5S_SELECT_NOTB
  !
  block = 1
  CALL h5sselect_hyperslab_f (space, H5S_SELECT_NOTB_F, start, count, &
       hdferr, stride, block)
  !
  ! Write the data to the dataset.
  !
  CALL h5dwrite_f(dset, H5T_NATIVE_INTEGER, wdata, dims, hdferr, file_space_id=space)
  !
  ! Close and release resources.
  !
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
  CALL h5dopen_f (file, dataset, dset, hdferr)
  !
  ! Read the data using the default properties.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, dims, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '(/,"Data as written to disk by hyberslabs:")')
  DO i=1, dim0
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80i3)', ADVANCE='NO') rdata(i,:)
     WRITE(*,'(" ]")')
  ENDDO
  !
  ! Initialize the read array.
  !
  rdata = 0
  !
  ! Define and select the hyperslab to use for reading.
  !
  CALL h5dget_space_f(dset, space, hdferr)

  start(1:2)=(/0,1/)
  stride = 4
  count = 2
  BLOCK(1:2)=(/2,3/)

  CALL h5sselect_hyperslab_f (space, H5S_SELECT_SET_F, start, count, &
       hdferr, stride, block)
  !
  ! Read the data using the previously defined hyperslab.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, dims, hdferr, file_space_id=space)
  !
  ! Output the DATA to the screen.
  !
  WRITE(*, '(/,"Data as read from disk by hyberslabs:")')
  DO i=1, dim0
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80i3)', ADVANCE='NO') rdata(i,:)
     WRITE(*,'(" ]")')
  ENDDO
  WRITE(*,'(/)')
  !
  ! Close and release resources.
  !
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5fclose_f(file , hdferr)
  
END PROGRAM main
