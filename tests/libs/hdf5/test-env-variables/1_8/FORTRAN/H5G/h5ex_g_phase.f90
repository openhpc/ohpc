!************************************************************
!
!  This example shows how to set the conditions for
!  conversion between compact and dense (indexed) groups.
!
!  This file is intended for use with HDF5 Library verion 1.8
!
!************************************************************
PROGRAM main

  USE HDF5
  IMPLICIT NONE

  CHARACTER(LEN=15), PARAMETER :: filename  = "h5ex_g_phase.h5"
  INTEGER, PARAMETER :: MAX_GROUPS  = 7
  INTEGER, PARAMETER :: MAX_COMPACT = 5
  INTEGER, PARAMETER :: MIN_DENSE   = 3

  INTEGER(HID_T)   :: file, group, subgroup, fapl, gcpl ! handles
  INTEGER :: hdferr
  INTEGER :: storage_type ! Type of storage for links in group:
                          !   H5G_STORAGE_TYPE_COMPACT_F: Compact storage
                          !   H5G_STORAGE_TYPE_DENSE_F: Indexed storage
                          !   H5G_STORAGE_TYPE_SYMBOL_TABLE_F: Symbol tables
  INTEGER :: nlinks       ! Number of links in group
  INTEGER :: max_corder   ! Current maximum creation order value for group
  CHARACTER(LEN=2) :: name = "G0" ! Name of subgroup
  INTEGER :: i
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Set file access property list to allow the latest file format.
  ! This will allow the library to create new format groups.
  !
  CALL h5pcreate_f (H5P_FILE_ACCESS_F, fapl, hdferr)
  CALL h5pset_libver_bounds_f (fapl, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, hdferr)
  !
  ! Create group access property list and set the phase change
  ! conditions.  In this example we lowered the conversion threshold
  ! to simplify the output, though this may not be optimal.
  !
  CALL h5pcreate_f (H5P_GROUP_CREATE_F, gcpl, hdferr)
  CALL h5pset_link_phase_change_f (gcpl, MAX_COMPACT, MIN_DENSE, hdferr)
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr, access_prp=fapl )
  !
  ! Create primary group.
  !
  CALL h5gcreate_f(file, name, group, hdferr, gcpl_id=gcpl)
  !
  ! Add subgroups to "group" one at a time, print the storage type
  ! for "group" after each subgroup is created.
  !
  DO i = 1, MAX_GROUPS
     !
     ! Define the subgroup name and create the subgroup.
     !
     WRITE(name,'(A,I1)') "G",i ! G1, G2, G3 etc.
     
     CALL h5gcreate_f(group, name, subgroup, hdferr)
     CALL h5gclose_f(subgroup, hdferr)
     !
     ! Obtain the group info and print the group storage type
     !
     CALL H5Gget_info_f(group, storage_type, nlinks, max_corder, hdferr)
     WRITE(*,'(I1," Groups: Storage type is ")', ADVANCE='NO') nlinks
     IF(storage_type.EQ. H5G_STORAGE_TYPE_COMPACT_F)THEN
        WRITE(*,'("H5G_STORAGE_TYPE_COMPACT_F")')  ! New compact format
     ELSE IF(storage_type.EQ.H5G_STORAGE_TYPE_DENSE_F)THEN
        WRITE(*,'("H5G_STORAGE_TYPE_DENSE_F")') ! New dense (indexed) format
     ELSE IF(storage_type.EQ.H5G_STORAGE_TYPE_SYMBOL_TABLE_F)THEN
        WRITE(*,'("H5G_STORAGE_TYPE_SYMBOL_TABLE")') ! Original format
     END IF
  ENDDO
  WRITE(*,'()')
  !
  ! Delete subgroups one at a time, print the storage type for
  !"group" after each subgroup is deleted.
  !
  DO i = MAX_GROUPS,1, -1
     !
     ! Define the subgroup name and delete the subgroup.
     !
     WRITE(name,'(A,I1)') "G",i ! G1, G2, G3 etc.
     CALL h5ldelete_f(group, name, hdferr)
     !
     ! Obtain the group info and print the group storage type
     !
     CALL H5Gget_info_f(group, storage_type, nlinks, max_corder, hdferr)
     WRITE(*,'(I1," Groups: Storage type is ")', ADVANCE='NO') nlinks
     IF(storage_type.EQ. H5G_STORAGE_TYPE_COMPACT_F)THEN
        WRITE(*,'("H5G_STORAGE_TYPE_COMPACT_F")')  ! New compact format
     ELSE IF(storage_type.EQ.H5G_STORAGE_TYPE_DENSE_F)THEN
        WRITE(*,'("H5G_STORAGE_TYPE_DENSE_F")') ! New dense (indexed) format
     ELSE IF(storage_type.EQ.H5G_STORAGE_TYPE_SYMBOL_TABLE_F)THEN
        WRITE(*,'("H5G_STORAGE_TYPE_SYMBOL_TABLE")') ! Original format
     END IF
  ENDDO
  !
  ! Close and release resources.
  !  
  CALL h5pclose_f(fapl, hdferr)
  CALL h5pclose_f(gcpl, hdferr)
  CALL h5gclose_f(group, hdferr)
  CALL h5fclose_f(file, hdferr)

END PROGRAM main
