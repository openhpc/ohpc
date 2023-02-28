! ************************************************************
!
!  This example shows how to read and write real and integer
!  data where the precision is set by SELECTED_REAL_KIND and
!  SELECTED_INT_KIND.
!  
!  The program first writes integers
!  and reals to a dataset with dataspace dimensions of DIM0xDIM1, 
!  then closes the file.  Next, it reopens the file, reads back 
!  the data, and outputs it to the screen.
!
!  This file is intended for use with HDF5 Library verion 1.8
!  with --enable-fortran2003
!
! ************************************************************

PROGRAM main

  USE HDF5
  USE ISO_C_BINDING

  IMPLICIT NONE

! Set the precision for the real KINDs

  INTEGER, PARAMETER :: sp = KIND(1.0),              &
       dp = SELECTED_REAL_KIND(2*PRECISION(1.0_sp)), &
       qp = SELECTED_REAL_KIND(2*PRECISION(1.0_dp))

  !                                                        -10      10
  ! Find the INTEGER KIND that can represent values from 10   to 10
  !
  INTEGER, PARAMETER :: ip = SELECTED_INT_KIND(10)
  !                                           
  CHARACTER(LEN=23), PARAMETER :: filename = "h5ex_d_rdwr_kind_F03.h5"
  CHARACTER(LEN=4) , PARAMETER :: dataset_r = "DS_R"
  CHARACTER(LEN=4) , PARAMETER :: dataset_i = "DS_I"
  INTEGER          , PARAMETER :: dim0     = 4
  INTEGER          , PARAMETER :: dim1     = 7

  INTEGER :: hdferr
  INTEGER(HID_T) :: file, space, dset_r, dset_i ! Handles
  INTEGER(HSIZE_T), DIMENSION(1:2)           :: dims = (/dim0, dim1/) ! Size read/write buffer
  INTEGER(KIND=ip), DIMENSION(1:dim0,1:dim1), TARGET :: wdata_i, rdata_i  ! Write/Read buffers
  REAL(kIND=dp), DIMENSION(1:dim0,1:dim1), TARGET    :: wdata_r, rdata_r  ! Write/Read buffers
  INTEGER :: i, j
  TYPE(C_PTR) :: f_ptr

  INTEGER(HID_T) :: h5_kind_type_r, h5_kind_type_i ! HDF type corresponding to the specified KIND

  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Initialize data.
  !
  DO i = 1, dim0
     DO j = 1, dim1
        wdata_i(i,j) = (i-1)*(j-1)-(j-1)
        wdata_r(i,j) = (REAL(i,kind=ip)-1.0_ip)*(REAL(j,kind=ip)-1.0_ip)-(REAL(j,kind=ip)-1.0_ip)
     ENDDO
  ENDDO
  !
  ! Find the HDF type corresponding to the specified KIND
  !
  h5_kind_type_r = h5kind_to_type(dp,H5_REAL_KIND)
  h5_kind_type_i = h5kind_to_type(ip,H5_INTEGER_KIND)
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create dataspace.  Setting size to be the current size.
  !
  CALL h5screate_simple_f(2, dims, space, hdferr)
  !
  ! Create the dataset. 
  !
  CALL h5dcreate_f(file, dataset_i, h5_kind_type_i, space, dset_i, hdferr)
  CALL h5dcreate_f(file, dataset_r, h5_kind_type_r, space, dset_r, hdferr)
  !
  ! Write the data to the dataset.
  !
  f_ptr = C_LOC(wdata_i(1,1))
  CALL h5dwrite_f(dset_i, h5_kind_type_i, f_ptr, hdferr)
  f_ptr = C_LOC(wdata_r(1,1))
  CALL h5dwrite_f(dset_r, h5_kind_type_r, f_ptr, hdferr)
  !
  ! Close and release resources.
  !
  CALL h5dclose_f(dset_r, hdferr)
  CALL h5dclose_f(dset_i, hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5fclose_f(file , hdferr)
  !
  ! Now we begin the read section of this example.
  !
  !
  ! Open file and dataset using the default properties.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f(file, dataset_i, dset_i, hdferr)
  CALL h5dopen_f(file, dataset_r, dset_r, hdferr)
  !
  ! Read the data using the default properties.
  !
  f_ptr = C_LOC(rdata_i(1,1))
  CALL h5dread_f(dset_i, h5_kind_type_i, f_ptr, hdferr)
  f_ptr = C_LOC(rdata_r(1,1))
  CALL h5dread_f(dset_r, h5_kind_type_r, f_ptr, hdferr)
  !
  ! Output the data to the screen.
  !
  WRITE(*, '(/,A,":")') dataset_i
  DO i=1, dim0
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80i3)', ADVANCE='NO') rdata_i(i,:)
     WRITE(*,'(" ]")')
  ENDDO
  WRITE(*, '(/,A,":")') dataset_r
  DO i=1, dim0
     WRITE(*,'(" [")', ADVANCE='NO')
     WRITE(*,'(80f7.3)', ADVANCE='NO') rdata_r(i,:)
     WRITE(*,'(" ]")')
  ENDDO
  WRITE(*, '(/)')
  !
  ! Close and release resources.
  !
  CALL h5dclose_f(dset_i , hdferr)
  CALL h5dclose_f(dset_r , hdferr)
  CALL h5fclose_f(file , hdferr)

END PROGRAM main
