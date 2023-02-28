!************************************************************
!
!  This example shows how to read and write object references
!  to an attribute.  The program first creates objects in the
!  file and writes references to those objects to an
!  attribute with a dataspace of DIM0, then closes the file.
!  Next, it reopens the file, dereferences the references,
!  and outputs the names of their targets to the screen.
!
!  This file is intended for use with HDF5 Library version 1.8
!  with --enable-fortran2003
!
!************************************************************
PROGRAM main

  USE HDF5
  USE ISO_C_BINDING

  IMPLICIT NONE
  CHARACTER(LEN=23), PARAMETER :: filename  = "h5ex_t_objrefatt_F03.h5"
  CHARACTER(LEN=3) , PARAMETER :: dataset   = "DS1"
  CHARACTER(LEN=2) , PARAMETER :: attribute = "A1"
  INTEGER          , PARAMETER :: dim0      = 2

  INTEGER(HID_T)  :: file, space, dset, obj, attr ! Handles
  INTEGER :: hdferr
  INTEGER(hsize_t),   DIMENSION(1:1) :: dims = (/DIM0/)
  TYPE(hobj_ref_t_f), DIMENSION(1:dim0), TARGET :: wdata ! Write buffer
  TYPE(hobj_ref_t_f), DIMENSION(:), ALLOCATABLE, TARGET :: rdata ! Read buffer
  INTEGER :: objtype
  INTEGER(SIZE_T) :: name_size
  CHARACTER(LEN=80) :: name
  INTEGER(HSIZE_T), DIMENSION(1:1) :: maxdims
  INTEGER :: i
  TYPE(C_PTR) :: f_ptr
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create a dataset with a null dataspace.
  !
  CALL h5screate_f(H5S_NULL_F,space,hdferr)
  CALL h5dcreate_f(file, "DS2", H5T_STD_I32LE, space, obj, hdferr)
  !
  CALL h5dclose_f(obj  , hdferr)
  CALL h5sclose_f(space, hdferr)
  !
  ! Create a group.
  !
  CALL h5gcreate_f(file, "G1", obj, hdferr)
  CALL h5gclose_f(obj, hdferr)
  !
  ! Create references to the previously created objects. note, space_id
  ! is not needed for object references.
  !
  f_ptr = C_LOC(wdata(1))
  CALL H5Rcreate_f(file, "G1", H5R_OBJECT_F, f_ptr, hdferr)
  f_ptr = C_LOC(wdata(2))
  CALL H5Rcreate_f(file, "DS2", H5R_OBJECT_F, f_ptr, hdferr)
  !
  ! Create dataset with a null dataspace to serve as the parent for
  ! the attribute.
  !
  CALL h5screate_f(H5S_NULL_F, space, hdferr)
  CALL h5dcreate_f(file, dataset, H5T_STD_I32LE, space, dset, hdferr)
  CALL h5sclose_f(space, hdferr)
  !
  ! Create dataspace.  Setting maximum size to be the current size.
  !
  CALL h5screate_simple_f(1, dims, space, hdferr)
  !
  ! Create the attribute and write the object references to it.
  !
  CALL H5Acreate_f(dset, attribute, H5T_STD_REF_OBJ, space, attr, hdferr)
  f_ptr = C_LOC(wdata(1))
  CALL H5Awrite_f(attr, H5T_STD_REF_OBJ, f_ptr, hdferr)
  !
  ! Close and release resources.
  !
  CALL H5Aclose_f(attr, hdferr)
  CALL H5Dclose_f(dset, hdferr)
  CALL H5Sclose_f(space, hdferr)
  CALL H5Fclose_f(file, hdferr)
  !
  ! Now we begin the read section of this example.
  !
  ! Open file and dataset.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  CALL h5dopen_f(file, dataset, dset, hdferr)
  CALL h5aopen_f(dset, attribute, attr, hdferr)
  !
  ! Get dataspace and allocate memory for read buffer.
  !
  CALL h5aget_space_f(attr, space, hdferr)
  CALL h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
  ALLOCATE(rdata(1:maxdims(1)))
  !
  ! Read the data.
  !
  f_ptr = C_LOC(rdata(1))
  CALL h5aread_f( attr, H5T_STD_REF_OBJ, f_ptr, hdferr)
  !
  ! Output the data to the screen.
  !
  DO i = 1, maxdims(1)
     WRITE(*,'(A,"[",i1,"]:",/,2X,"->")', ADVANCE='NO') attribute, i-1
     !
     ! Open the referenced object, get its name and type.
     !
     f_ptr = C_LOC(rdata(i))
     CALL H5Rdereference_f(dset, H5R_OBJECT_F, f_ptr, obj, hdferr)
     CALL H5Rget_obj_type_f(dset, H5R_OBJECT_F, f_ptr, objtype, hdferr)
     !
     ! Get the length of the name and name
     !
     CALL H5Iget_name_f(obj, name, 80_size_t, name_size, hdferr)
     !
     ! Print the object type and close the object.
     !
     IF(objtype.EQ.H5G_GROUP_F)THEN
        WRITE(*,'("Group")', ADVANCE="NO")
     ELSE IF(objtype.EQ.H5G_DATASET_F)THEN
        WRITE(*,'("Dataset")', ADVANCE="NO")
     ELSE IF(objtype.EQ.H5G_TYPE_F)THEN
        WRITE(*,'("Named Datatype")', ADVANCE="NO")
     ELSE
        WRITE(*,'("Unknown")', ADVANCE="NO")
     ENDIF
     CALL h5oclose_f(obj, hdferr)
     !
     ! Print the name.
     !
     WRITE(*,'(": ",A)') name(1:name_size)
  END DO
  !
  ! Close and release resources.
  !
  DEALLOCATE(rdata)
  CALL h5aclose_f(attr , hdferr)
  CALL h5dclose_f(dset , hdferr)
  CALL h5sclose_f(space, hdferr)
  CALL h5fclose_f(file , hdferr)
  !
END PROGRAM main
