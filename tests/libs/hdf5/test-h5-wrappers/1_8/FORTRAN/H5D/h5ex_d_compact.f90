! ************************************************************
!
!  This example shows how to read and write data to a compact
!  dataset.  The program first writes integers to a compact
!  dataset with dataspace dimensions of DIM0xDIM1, then
!  closes the file.  Next, it reopens the file, reads back
!  the data, and outputs it to the screen.
!
!  This file is intended for use with HDF5 Library verion 1.8
!
! ************************************************************

PROGRAM main

  USE HDF5

  IMPLICIT NONE

  CHARACTER(LEN=17), PARAMETER :: filename = "h5ex_d_compact.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset  = "DS1"
  INTEGER          , PARAMETER :: dim0     = 4
  INTEGER          , PARAMETER :: dim1     = 7

  INTEGER :: hdferr
  INTEGER :: layout
  INTEGER(HID_T)  :: file, space, dset, dcpl ! Handles
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
  ! Create the dataset creation property list, set the layout to
  ! compact.
  !
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, hdferr)
  CALL h5pset_layout_f(dcpl, H5D_COMPACT_F, hdferr)
  !
  ! Create the dataset.  We will use all default properties for this
  ! example.
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
  ! Now we begin the READ section of this example.
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
