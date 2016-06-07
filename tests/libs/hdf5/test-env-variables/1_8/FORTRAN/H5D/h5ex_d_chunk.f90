! ************************************************************
!
!  This example shows how to create a chunked dataset.  The
!  program first writes integers in a hyperslab selection to
!  a chunked dataset with dataspace dimensions of DIM0xDIM1
!  and chunk size of CHUNK0xCHUNK1, then closes the file.
!  Next, it reopens the file, reads back the data, and
!  outputs it to the screen.  Finally it reads the data again
!  using a different hyperslab selection, and outputs
!  the result to the screen.
!
!  This file is intended for use with HDF5 Library verion 1.8
!
! ************************************************************

PROGRAM main

  USE HDF5

  IMPLICIT NONE

  CHARACTER(LEN=18), PARAMETER :: filename = "h5ex_d_chunk.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset  = "DS1"
  INTEGER          , PARAMETER :: dim0     = 6
  INTEGER          , PARAMETER :: dim1     = 8
  INTEGER          , PARAMETER :: chunk0   = 4
  INTEGER          , PARAMETER :: chunk1   = 4

  INTEGER :: hdferr
  INTEGER :: layout
  INTEGER(HID_T)  :: file, space, dset, dcpl ! Handles
  INTEGER(HSIZE_T), DIMENSION(1:2)   :: dims = (/dim0, dim1/), chunk = (/chunk0,chunk1/)
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
  !
  ! Create the dataset creation property list, and set the chunk
  ! size.
  !
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, hdferr)
  CALL h5pset_chunk_f(dcpl, 2, chunk, hdferr)
  !
  ! Create the chunked dataset.
  !  
  CALL h5dcreate_f(file, dataset, H5T_STD_I32LE, space, dset, hdferr, dcpl)
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
  CALL h5dopen_f (file, dataset, dset, hdferr)
  ! 
  ! Retrieve the dataset creation property list, and print the
  ! storage layout.
  !
  CALL h5dget_create_plist_f(dset, dcpl, hdferr)
  CALL h5pget_layout_f(dcpl, layout, hdferr)
  WRITE(*,'(/,"Storage layout for ", A," is: ")', ADVANCE='NO') dataset
  IF(layout.EQ.H5D_COMPACT_F)THEN
     WRITE(*,'("H5D_COMPACT_F",/)')
  ELSE IF (layout.EQ.H5D_CONTIGUOUS_F)THEN
     WRITE(*,'("H5D_CONTIGUOUS_F",/)')
  ELSE IF (layout.EQ.H5D_CHUNKED_F)THEN
     WRITE(*,'("H5D_CHUNKED",/)')
  ENDIF
  !
  ! Read the data using the default properties.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, dims, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '("Data as written to disk by hyberslabs:")')
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
  start(1:2) = (/0,1/)
  stride = 4
  count = 2
  block(1:2) = (/2,3/)
 
  CALL h5sselect_hyperslab_f (space, H5S_SELECT_SET_F, start, count, &
       hdferr, stride, block)
  !
  ! Read the data using the previously defined hyperslab.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, dims, hdferr, file_space_id=space)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '(/,"Data as read from disk by hyperslab:")')
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
  CALL h5sclose_f(space, hdferr)
  CALL h5fclose_f(file , hdferr)

END PROGRAM main
