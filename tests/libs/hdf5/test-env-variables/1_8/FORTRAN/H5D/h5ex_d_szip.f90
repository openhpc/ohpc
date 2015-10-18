!************************************************************
!
!  This example shows how to read and write data to a dataset
!  using szip compression.    The program first checks if
!  szip compression is available, then if it is it writes
!  integers to a dataset using szip, then closes the file.
!  Next, it reopens the file, reads back the data, and
!  outputs the type of compression and the maximum value in
!  the dataset to the screen.
!
!  This file is intended for use with HDF5 Library verion 1.8
!
!************************************************************
PROGRAM main

  USE HDF5

  IMPLICIT NONE

  CHARACTER(LEN=14), PARAMETER :: filename = "h5ex_d_szip.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset  = "DS1"
  INTEGER          , PARAMETER :: dim0     = 32
  INTEGER          , PARAMETER :: dim1     = 64
  INTEGER          , PARAMETER :: chunk0   = 4
  INTEGER          , PARAMETER :: chunk1   = 8

  INTEGER :: hdferr
  LOGICAL         :: avail
  INTEGER(HID_T)  :: file, space, dset, dcpl ! Handles
  INTEGER, DIMENSION(1:4) :: cd_values
  INTEGER :: filter_id
  INTEGER :: filter_info_both
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dims = (/dim0, dim1/), chunk =(/chunk0,chunk1/)
  INTEGER(SIZE_T) :: nelmts
  INTEGER :: flags, filter_info
  INTEGER, DIMENSION(1:dim0, 1:dim1) :: wdata, & ! Write buffer 
                                        rdata    ! Read buffer
  INTEGER :: max, i, j
  INTEGER, PARAMETER :: MaxChrLen = 80
  CHARACTER(LEN=MaxChrLen) :: name
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  ! 
  !  Check if gzip compression is available and can be used for both
  !  compression and decompression.  Normally we do not perform error
  !  checking in these examples for the sake of clarity, but in this
  !  case we will make an exception because this filter is an
  !  optional part of the hdf5 library.
  !  
  CALL h5zfilter_avail_f(H5Z_FILTER_SZIP_F, avail, hdferr)

  IF (.NOT.avail) THEN
     WRITE(*,'("szip filter not available.",/)')
     STOP
  ENDIF
  CALL h5zget_filter_info_f(H5Z_FILTER_SZIP_F, filter_info, hdferr)

  filter_info_both=IOR(H5Z_FILTER_ENCODE_ENABLED_F,H5Z_FILTER_DECODE_ENABLED_F)
  IF (filter_info .NE. filter_info_both) THEN
     WRITE(*,'("szip filter not available for encoding and decoding.",/)')
     STOP
  ENDIF
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
  ! Create dataspace.  Setting size to be the current size.
  !
  CALL h5screate_simple_f(2, dims, space, hdferr)
  !
  ! Create the dataset creation property list, add the szip
  ! compression filter and set the chunk size.
  !
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, hdferr)
  CALL h5pset_szip_f(dcpl, H5_SZIP_NN_OM_F, 8, hdferr)
  CALL h5pset_chunk_f(dcpl, 2, chunk, hdferr)
  !
  ! Create the dataset.
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
  CALL h5dopen_f (file, dataset, dset, hdferr)
  !
  ! Retrieve dataset creation property list.
  !
  CALL h5dget_create_plist_f(dset, dcpl, hdferr)
  !
  ! Retrieve and print the filter type.  Here we only retrieve the
  ! first filter because we know that we only added one filter.
  !
  
  nelmts = 1
  CALL H5Pget_filter_f(dcpl, 0, flags, nelmts, cd_values, MaxChrLen, name, filter_id, hdferr)
  WRITE(*,'("Filter type is: ")', ADVANCE='NO')
  IF(filter_id.EQ.H5Z_FILTER_DEFLATE_F)THEN
     WRITE(*,'(T2,"H5Z_FILTER_DEFLATE_F")')
  ELSE IF(filter_id.EQ.H5Z_FILTER_SHUFFLE_F)THEN
     WRITE(*,'(T2,"H5Z_FILTER_SHUFFLE_F")')
  ELSE IF(filter_id.EQ.H5Z_FILTER_FLETCHER32_F)THEN
     WRITE(*,'(T2,"H5Z_FILTER_FLETCHER32_F")')
  ELSE IF(filter_id.EQ.H5Z_FILTER_SZIP_F)THEN
     WRITE(*,'(T2,"H5Z_FILTER_SZIP_F")')
! DEFINED ONLY IN F2003 hdf5 branch
!  ELSE IF(filter_id.EQ.H5Z_FILTER_NBIT_F)THEN
!     WRITE(*,'(T2,"H5Z_FILTER_NBIT_F")')
!  ELSE IF(filter_id.EQ.H5Z_FILTER_SCALEOFFSET_F)THEN
!     WRITE(*,'(T2,"H5Z_FILTER_SCALEOFFSET_F")')
  ENDIF
  !
  ! Read the data using the default properties.
  !
  CALL h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, dims, hdferr)
  !
  ! Find the maximum value in the dataset, to verify that it was
  ! read correctly.
  !
  max = MAXVAL(rdata)
  !
  ! Print the maximum value.
  !
  WRITE(*,'("Maximum value in ",A," is: ",i10)') dataset, max
  !
  ! Close and release resources.
  !
  CALL h5pclose_f(dcpl , hdferr)
  CALL h5dclose_f(dset , hdferr)
  CALL h5fclose_f(file , hdferr)

END PROGRAM main
