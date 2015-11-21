!************************************************************
!
!  This example shows how to create "compact-or-indexed"
!  format groups, new to 1.8.  This example also illustrates
!  the space savings of compact groups by creating 2 files
!  which are identical except for the group format, and
!  displaying the file size of each.  Both files have one
!  empty group in the root group.
!
!  This file is intended for use with HDF5 Library verion 1.8
!
!************************************************************

PROGRAM main

  USE HDF5
  IMPLICIT NONE

  CHARACTER(LEN=18), PARAMETER :: filename1 = "h5ex_g_compact1.h5"
  CHARACTER(LEN=18), PARAMETER :: filename2 = "h5ex_g_compact2.h5"
  CHARACTER(LEN=2) , PARAMETER :: groupname  = "G1"

  INTEGER(HID_T)   :: file, group, fapl ! handles
  INTEGER :: hdferr
  INTEGER :: storage_type ! Type of storage for links in group:
                          !   H5G_STORAGE_TYPE_COMPACT: Compact storage
                          !   H5G_STORAGE_TYPE_DENSE: Indexed storage
                          !   H5G_STORAGE_TYPE_SYMBOL_TABLE: Symbol tables
  INTEGER :: nlinks       ! Number of links in group
  INTEGER :: max_corder   ! Current maximum creation order value for group 
  INTEGER(HSIZE_T) :: size
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Create file 1.  This file will use original format groups.
  !
  CALL h5fcreate_f(filename1, H5F_ACC_TRUNC_F, file, hdferr)
  CALL h5gcreate_f(file, groupname, group, hdferr)
  !
  ! Obtain the group info and print the group storage type.
  !
  CALL H5Gget_info_f(group, storage_type, nlinks, max_corder, hdferr)

  WRITE(*,'("Group storage type for ",A," is: ")', ADVANCE='NO' ) filename1

  IF(storage_type.EQ.H5G_STORAGE_TYPE_COMPACT_F)THEN
     WRITE(*,'("H5G_STORAGE_TYPE_COMPACT_F")') ! New compact format
  ELSE IF(storage_type.EQ.H5G_STORAGE_TYPE_DENSE_F)THEN
     WRITE(*,'("H5G_STORAGE_TYPE_DENSE_F")') ! New dense (indexed) format
  ELSE IF(storage_type.EQ.H5G_STORAGE_TYPE_SYMBOL_TABLE_F)THEN
     WRITE(*,'("H5G_STORAGE_TYPE_SYMBOL_TABLE_F")') ! Original format
  ENDIF
  !
  ! Close and re-open file.  Needed to get the correct file size.
  !
  CALL h5gclose_f(group, hdferr)
  CALL h5fclose_f(file , hdferr)
  CALL h5fopen_f(filename1, H5F_ACC_RDONLY_F, file, hdferr)
  !
  ! Obtain and print the file size.
  !
  CALL h5fget_filesize_f(file, size, hdferr)
  WRITE(*,'("File size for ",A," is: ",i6," bytes",/)') filename1, size
  !
  ! Close filename1
  !
  CALL h5fclose_f(file, hdferr)
  !
  ! Set file access property list to allow the latest file format.
  ! This will allow the library to create new compact format groups.
  !
  CALL h5pcreate_f (H5P_FILE_ACCESS_F, fapl, hdferr)
  CALL h5pset_libver_bounds_f (fapl, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, hdferr)
  !
  ! Create file 2 using the new file access property list.
  !
  CALL h5fcreate_f(filename2, H5F_ACC_TRUNC_F, file, hdferr, access_prp=fapl)
  CALL h5gcreate_f(file, groupname, group, hdferr)
  !
  ! Obtain the group info and print the group storage type.
  !
  CALL H5Gget_info_f(group, storage_type, nlinks, max_corder, hdferr)

  WRITE(*,'("Group storage type for ",A," is: ")', ADVANCE='NO' ) filename2

  IF(storage_type.EQ.H5G_STORAGE_TYPE_COMPACT_F)THEN
     WRITE(*,'("H5G_STORAGE_TYPE_COMPACT_F")') ! New compact format
  ELSE IF(storage_type.EQ.H5G_STORAGE_TYPE_DENSE_F)THEN
     WRITE(*,'("H5G_STORAGE_TYPE_DENSE_F")') ! New dense (indexed) format
  ELSE IF(storage_type.EQ.H5G_STORAGE_TYPE_SYMBOL_TABLE_F)THEN
     WRITE(*,'("H5G_STORAGE_TYPE_SYMBOL_TABLE_F")') ! Original format
  ENDIF
  !
  ! Close and re-open file.  Needed to get the correct file size.
  !
  CALL h5gclose_f(group, hdferr)
  CALL h5fclose_f(file , hdferr)
  CALL h5fopen_f(filename2, H5F_ACC_RDONLY_F, file, hdferr, fapl)
  !
  ! Obtain and print the file size.
  !
  CALL h5fget_filesize_f(file, size, hdferr)
  WRITE(*,'("File size for ",A," is: ",i6,"bytes",/)') filename2, size
  !
  ! Close and release resources.
  !
  CALL h5pclose_f(fapl, hdferr)
  CALL h5fclose_f(file, hdferr)

END PROGRAM main
