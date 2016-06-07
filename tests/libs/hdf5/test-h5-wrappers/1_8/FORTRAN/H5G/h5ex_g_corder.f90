!************************************************************
!
!  This example shows how to track links in a group by
!  creation order.  The program creates a series of groups,
!  then reads back their names: first in alphabetical order,
!  then in creation order.
!
!  This file is intended for use with HDF5 Library version 1.8
!
!************************************************************

PROGRAM main

  USE HDF5
  IMPLICIT NONE

  CHARACTER(LEN=16), PARAMETER :: filename   = "h5ex_g_corder.h5"

  INTEGER(HID_T) :: file, group, subgroup, gcpl ! handles
  INTEGER :: hdferr
  INTEGER :: storage_type ! Type of storage for links in group:
                          !   H5G_STORAGE_TYPE_COMPACT: Compact storage
                          !   H5G_STORAGE_TYPE_DENSE: Indexed storage
                          !   H5G_STORAGE_TYPE_SYMBOL_TABLE: Symbol tables
  INTEGER :: nlinks       ! Number of links in group
  INTEGER :: max_corder   ! Current maximum creation order value for group 
  INTEGER(SIZE_T) :: size  ! Size of name
  INTEGER(HSIZE_T) :: i     ! Index
  CHARACTER(LEN=80) :: name ! Output buffer
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create group creation property list and enable link creation
  ! order tracking.  Attempting to track by creation order in a
  ! group that does not have this property set will result in an
  ! error.
  !
  CALL h5pcreate_f(H5P_GROUP_CREATE_F, gcpl, hdferr)
  CALL h5pset_link_creation_order_f( gcpl, IOR(H5P_CRT_ORDER_TRACKED_F,H5P_CRT_ORDER_INDEXED_F),hdferr)
  !
  ! Create primary group using the property list.
  !
  CALL h5gcreate_f(file, "index_group", group, hdferr, gcpl_id=gcpl)
  !
  ! Create subgroups in the primary group.  These will be tracked
  ! by creation order.  Note that these groups do not have to have
  ! the creation order tracking property set.
  !
  CALL h5gcreate_f(group, "H", subgroup, hdferr)
  CALL h5gclose_f(subgroup,hdferr)
  CALL h5gcreate_f(group, "D", subgroup, hdferr)
  CALL h5gclose_f(subgroup,hdferr)
  CALL h5gcreate_f(group, "F", subgroup, hdferr)
  CALL h5gclose_f(subgroup,hdferr)
  CALL h5gcreate_f(group, "5", subgroup, hdferr)
  CALL h5gclose_f(subgroup,hdferr)
  !
  ! Get group info.
  !
  CALL H5Gget_info_f(group, storage_type, nlinks, max_corder, hdferr)
  !
  ! Traverse links in the primary group using alphabetical indices
  ! (H5_INDEX_NAME).
  !
  WRITE(*,'("Traversing group using alphabetical indices:")')
  DO i = 0, nlinks-1
     !
     ! Get name and size of name
     ! 
     CALL H5Lget_name_by_idx_f(group, ".", H5_INDEX_NAME_F, H5_ITER_INC_F, i, name, hdferr, size)
     WRITE(*,'("Index ",i2,": ",A)') INT(i), TRIM(name)
  ENDDO
  !
  !Traverse links in the primary group by creation order
  ! (H5_INDEX_CRT_ORDER).
  !
  WRITE(*,'(/,"Traversing group using creation order indices:")')
  DO i = 0, nlinks-1
     !
     ! Get name and size of name
     ! 
     CALL H5Lget_name_by_idx_f(group, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, i, name, hdferr, size)
     WRITE(*,'("Index ",i2,": ",A)') INT(i), TRIM(name)
  ENDDO
  !
  ! Close and release resources.
  !
  CALL h5pclose_f(gcpl, hdferr)
  CALL h5gclose_f(group, hdferr)
  CALL h5fclose_f(file, hdferr)
END PROGRAM main
