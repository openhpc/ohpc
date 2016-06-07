!************************************************************
!
!  This example shows a way to recursively traverse the file
!  using h5literate and h5literate_by_name_f.  The method shown 
!  here guarantees that
!  the recursion will not enter an infinite loop, but does
!  not prevent objects from being visited more than once.
!  The program prints the directory structure of the file
!  specified in filename.  The default file used by this example
!  implements the structure described in the User's Guide,
!  chapter 4, figure 26.
!
!  This file is intended for use with HDF5 Library version 1.8
!  with --enable-fortran2003
! ************************************************************

MODULE g_traverse

  USE HDF5
  USE ISO_C_BINDING
  IMPLICIT NONE

  CHARACTER(LEN=18) :: filename = "h5ex_g_traverse.h5"

  !
  ! Define operator data structure type for H5Literate callback.
  ! During recursive iteration, these structures will form a
  ! linked list that can be searched for duplicate groups,
  ! preventing infinite recursion.
  !
  TYPE :: opdata
     INTEGER :: recurs              ! Recursion level.  0=root
     TYPE(opdata), POINTER :: prev  ! Pointer to previous opdata 
     INTEGER(haddr_t) :: addr       ! Group address
  END TYPE opdata

CONTAINS

  !
  ! OPERATOR FUNCTION TO BE CALLED BY H5LITERATE_F
  !
  ! ************************************************************
  !
  !  Operator function.  This function prints the name and type
  !  of the object passed to it.  If the object is a group, it
  !  is first checked against other groups in its path using
  !  the group_check function, then if it is not a duplicate,
  !  H5Literate is called for that group.  This guarantees that
  !  the program will not enter infinite recursion due to a
  !  circular path in the file.
  !
  ! ************************************************************

  RECURSIVE INTEGER FUNCTION op_func(loc_id, name, info, operator_data) RESULT(ret_val) BIND(C)

    USE HDF5
    USE ISO_C_BINDING
    IMPLICIT NONE

    INTEGER(hid_t), VALUE :: loc_id
    CHARACTER(LEN=1), DIMENSION(1:10) :: name ! Must have LEN=1 for bind(C) strings
    TYPE(C_PTR), VALUE :: info
    TYPE(C_PTR), VALUE :: operator_data

    INTEGER :: status, return_val
    TYPE(h5o_info_t), TARGET :: infobuf 
    TYPE(C_PTR) :: ptr
    CHARACTER(LEN=10) :: name_string
    INTEGER :: i
    TYPE(opdata), POINTER :: od
    TYPE(opdata), TARGET :: nextod
    INTEGER(HSIZE_T) :: idx

    TYPE(C_PTR) :: ptr2
    TYPE(C_FUNPTR) :: funptr

    CHARACTER(LEN=10) :: space
    INTEGER :: spaces ! Number of whitespaces to prepend to output
    INTEGER :: len

    ret_val = 0
    
    name_string(1:10) = " "
    len = 0
    DO
       len = len + 1
       IF(name(len)(1:1).EQ.C_NULL_CHAR) EXIT
       name_string(len:len) = name(len)(1:1)
    ENDDO
    len = len - 1 ! subtract NULL character

    space(1:10) = " "

    CALL C_F_POINTER(operator_data, od)
    !
    ! Get type of the object and display its name and type.
    ! The name of the object is passed to this function by
    ! the Library.
    !
    CALL H5Oget_info_by_name_f(loc_id, name_string, infobuf, status)

    spaces = 2*(od%recurs+1)

    WRITE(*,'(A)', ADVANCE='NO') space(1:spaces) !  Format output


    IF(infobuf%type.EQ.H5O_TYPE_GROUP_F)THEN

       WRITE(*,'("Group: ",A," {")') name_string(1:len)

!            
!              Check group address against linked list of operator
!              data structures.  We will always run the check, as the
!              reference count cannot be relied upon if there are
!              symbolic links, and H5Oget_info_by_name always follows
!              symbolic links.  Alternatively we could use H5Lget_info
!              and never recurse on groups discovered by symbolic
!              links, however it could still fail if an object's
!              reference count was manually manipulated with
!              H5Odecr_refcount.
!        

       i = group_check(od, infobuf%addr)

       IF(i.EQ.1)THEN
          WRITE(*,'(A)') space(1:spaces)//"  Warning: Loop detected!"
       ELSE

          nextod%recurs = od%recurs + 1
          nextod%prev => od
          nextod%addr = infobuf%addr
          idx = 0
          ptr2 = C_LOC(nextod)
          funptr = C_FUNLOC(op_func)
          CALL h5literate_by_name_f(loc_id, name_string, H5_INDEX_NAME_F, H5_ITER_NATIVE_F, idx, &
               funptr, ptr2, ret_val, status)

       ENDIF
       WRITE(*,'(A)') space(1:spaces)//"}"
       RETURN
    ELSE IF(infobuf%type.EQ.H5O_TYPE_DATASET_F)THEN
       WRITE(*,'("Dataset: ",A)') name_string(1:len)
    ELSE IF(infobuf%type.EQ.H5O_TYPE_NAMED_DATATYPE_F)THEN
       WRITE(*,'("Datatype: ",A)') name_string(1:len)
    ELSE
       WRITE(*,'("Unknown: ",A)') name_string(1:len)
    ENDIF

END FUNCTION op_func

!************************************************************
!
!  This function recursively searches the linked list of
!  opdata structures for one whose address matches
!  target_addr.  Returns 1 if a match is found, and 0
!  otherwise.
!
! ************************************************************/

  INTEGER RECURSIVE FUNCTION group_check(od, target_addr) result(g_c)

    IMPLICIT NONE
    TYPE(opdata), POINTER :: od
    INTEGER(haddr_t) :: target_addr
 
    IF (od%addr .EQ. target_addr)THEN
       g_c = 1       ! Addresses match
    ELSE IF (od%recurs.EQ.0)THEN
       g_c = 0       ! Root group reached with no matches
    ELSE
       ! Recursively examine the next node
       g_c = group_check(od%prev, target_addr)
    END IF
  END FUNCTION group_check

END MODULE g_traverse

PROGRAM main

  USE HDF5
  USE ISO_C_BINDING
  
  USE g_traverse

  IMPLICIT NONE

  INTEGER(hid_t) :: file ! Handle
  INTEGER :: status
  TYPE(h5o_info_t) :: infobuf
  TYPE(opdata), TARGET :: od
  TYPE(C_PTR) :: ptr
  INTEGER(hsize_t) :: idx
  INTEGER :: ret_value
  TYPE(C_FUNPTR) :: funptr
  !
  ! Open file and initialize the operator data structure.
  !
  CALL H5Fopen_f(filename, H5F_ACC_RDONLY_F, file, status)

  CALL h5oget_info_by_name_f(file, "/", infobuf, status)

  od%recurs = 0
  od%prev => NULL()
  od%addr = infobuf%addr
  !
  ! Print the root group and formatting, begin iteration.
  !
  idx = 0
  funptr = C_FUNLOC(op_func)
  ptr    = C_LOC(od)

  WRITE(*,'(A)') "/ {"
  CALL  H5Literate_f(file, H5_INDEX_NAME_F,  H5_ITER_NATIVE_F, idx, funptr, ptr, ret_value, status) 
  WRITE(*,'(A)') "}"

  !
  ! Close and release resources.
  !
  CALL H5Fclose_f(file, status)

END PROGRAM main



