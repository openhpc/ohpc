!************************************************************
!
!  This example shows how to read and write compound
!  datatypes to an attribute.  The program first writes
!  compound structures to an attribute with a dataspace of
!  DIM0, then closes the file.  Next, it reopens the file,
!  reads back the data, and outputs it to the screen.
!
!  This file is intended for use with HDF5 Library version 1.8
!  with --enable-fortran2003
!
!************************************************************
PROGRAM main

  USE ISO_C_BINDING
  USE HDF5

  IMPLICIT NONE

  ! This should map to REAL*8 on most modern processors
  INTEGER, PARAMETER :: real_kind_15 = SELECTED_REAL_KIND(15,307)

  CHARACTER(LEN=21), PARAMETER :: filename     = "h5ex_t_cmpdatt_F03.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset      = "DS1"
  CHARACTER(LEN=2) , PARAMETER :: attribute    = "A1"
  INTEGER          , PARAMETER :: dim0         = 4
  INTEGER          , PARAMETER :: maxstringlen = 80

  TYPE sensor_t ! Compound data type
     INTEGER :: serial_no
     CHARACTER(LEN=maxstringlen) :: location
     REAL(real_kind_15) :: temperature
     REAL(real_kind_15) :: pressure
  END TYPE sensor_t

  TYPE(sensor_t), DIMENSION(1:dim0), TARGET ::  wdata ! Write buffer
  TYPE(sensor_t), DIMENSION(1:dim0), TARGET ::  rdata ! Read buffer
  INTEGER(HID_T)  :: file, filetype, memtype, space, dset, attr, strtype ! Handles
  INTEGER :: hdferr
  INTEGER(HSIZE_T), DIMENSION(1:1)   :: dims = (/dim0/), ndims
  TYPE(C_PTR) :: f_ptr
  INTEGER :: i
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Initialize data.
  !
  wdata(1)%serial_no = 1153
  wdata(1)%location = "Exterior (static)"
  wdata(1)%temperature = 53.23_real_kind_15
  wdata(1)%pressure = 24.57_real_kind_15
  wdata(2)%serial_no = 1184
  wdata(2)%location = "Intake"
  wdata(2)%temperature = 55.12_real_kind_15
  wdata(2)%pressure = 22.95_real_kind_15
  wdata(3)%serial_no = 1027
  wdata(3)%location = "Intake manifold"
  wdata(3)%temperature = 103.55_real_kind_15
  wdata(3)%pressure = 31.23_real_kind_15
  wdata(4)%serial_no = 1313
  wdata(4)%location = "Exhaust manifold"
  wdata(4)%temperature = 1252.89_real_kind_15
  wdata(4)%pressure = 84.11_real_kind_15
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create the compound datatype for memory.
  !
  CALL h5tcreate_f(H5T_COMPOUND_F, H5OFFSETOF(C_LOC(wdata(1)), C_LOC(wdata(2))), memtype, hdferr)
  
  CALL h5tinsert_f(memtype, "Serial number", &
       H5OFFSETOF(C_LOC(wdata(1)),C_LOC(wdata(1)%serial_no)), H5T_NATIVE_INTEGER, hdferr)
  !
  ! Create datatype for the String attribute.
  !
  CALL h5tcopy_f(H5T_NATIVE_CHARACTER, strtype, hdferr)
  CALL h5tset_size_f(strtype, INT(maxstringlen,size_t), hdferr)  

  CALL h5tinsert_f(memtype, "Location", &
       H5OFFSETOF(C_LOC(wdata(1)),C_LOC(wdata(1)%location)), strtype, hdferr)

  CALL h5tinsert_f(memtype, "Temperature (F)", &
       H5OFFSETOF(C_LOC(wdata(1)),C_LOC(wdata(1)%temperature)), &
       h5kind_to_type(real_kind_15,H5_REAL_KIND), hdferr)

  CALL h5tinsert_f(memtype, "Pressure (inHg)", &
       H5OFFSETOF(C_LOC(wdata(1)),C_LOC(wdata(1)%pressure)), &
       h5kind_to_type(real_kind_15,H5_REAL_KIND), hdferr)
  !
  ! Create the compound datatype for the file.  Because the standard
  ! types we are using for the file may have different sizes than
  ! the corresponding native types, we must manually calculate the
  ! offset of each member.
  !
  CALL h5tcreate_f(H5T_COMPOUND_F, INT(8 + maxstringlen + 8 + 8 , size_t), filetype, hdferr)
  
  CALL h5tinsert_f(filetype, "Serial number", 0_size_t, H5T_STD_I64BE, hdferr)

  CALL h5tinsert_f(filetype, "Location", 8_size_t, strtype, hdferr)

  CALL h5tinsert_f(filetype, "Temperature (F)", INT(8 + maxstringlen,size_t), &
       H5T_IEEE_F64BE, hdferr)

  CALL h5tinsert_f(filetype, "Pressure (inHg)", INT(8 + maxstringlen + 8, size_t), &
       H5T_IEEE_F64BE, hdferr)
  !
  ! Create dataset with a null dataspace.
  !
  CALL h5screate_f(H5S_NULL_F, space, hdferr)

  CALL h5dcreate_f(file,DATASET, H5T_STD_I32LE, space, dset, hdferr)

  CALL h5sclose_f(space, hdferr)
  !
  ! Create dataspace.  Set the size to be the current size.
  !
  CALL h5screate_simple_f(1, dims, space, hdferr)
  !
  ! Create the attribute and write the compound data to it.
  !
  CALL h5acreate_f(dset, attribute, filetype, space, attr, hdferr)

  f_ptr = C_LOC(wdata(1))
  CALL h5awrite_f(attr, memtype, f_ptr, hdferr)
  !
  ! Close and release resources.
  !
  CALL h5aclose_f(attr, hdferr)
  CALL h5dclose_f(dset, hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5tclose_f(filetype, hdferr)
  CALL h5fclose_f(file, hdferr)
  !
  ! Now we begin the read section of this example.
  !
  !
  ! Open file, dataset, and attribute.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f(file, dataset, dset, hdferr)
  CALL h5aopen_f(dset, attribute, attr, hdferr)
  !
  ! Get dataspace and allocate memory for read buffer.
  !
  CALL h5aget_space_f(attr,space, hdferr)
  CALL h5sget_simple_extent_dims_f(space, dims, ndims, hdferr)
  !
  ! Read the data.
  !
  f_ptr = C_LOC(rdata(1))
  CALL h5aread_f( attr, memtype, f_ptr, hdferr)
  !
  ! Output the data to the screen.
  !
  DO i = 1, ndims(1)
     WRITE(*,'(A,I1,":")') ATTRIBUTE, i
     WRITE(*,'("Serial number   : ", I6)') rdata(i)%serial_no
     WRITE(*,'("Location        : ", A)' ) TRIM(rdata(i)%location)
     WRITE(*,'("Temperature (F) : ", f8.2)') rdata(i)%temperature
     WRITE(*,'("Pressure (inHg) : ", f8.2)') rdata(i)%pressure
  END DO
  !
  ! Close and release resources
  !
  CALL h5aclose_f(attr, hdferr)
  CALL h5dclose_f(dset, hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5tclose_f(strtype, hdferr)
  CALL h5fclose_f(file, hdferr)

END PROGRAM main
