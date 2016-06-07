!
! Copyright (C) 2002-2004 FPMD & PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
MODULE io_global
  !----------------------------------------------------------------------------
  !
  IMPLICIT NONE
  !
  PRIVATE
  SAVE
  !
  PUBLIC :: io_global_start, meta_io_global_start, io_global_getionode, io_global_getmeta
  PUBLIC :: stdout, ionode, ionode_id, meta_ionode, meta_ionode_id
  PUBLIC :: xmlinputunit, xmloutputunit, xmltmpunit
  !
  INTEGER :: stdout = 6            ! unit connected to standard output
  INTEGER :: ionode_id = 0         ! index of the i/o node
  LOGICAL :: ionode = .TRUE.       ! identifies the i/o node
  INTEGER :: meta_ionode_id = 0    ! index of the i/o node for meta-codes
  LOGICAL :: meta_ionode = .TRUE.  ! identifies the i/o node for meta-codes
  LOGICAL :: first = .TRUE.
  INTEGER :: xmlinputunit          ! unit connected to the xml input
  INTEGER :: xmloutputunit = 51    ! unit connected to the xml output
  INTEGER :: xmltmpunit    = 52    ! unit connected to the temp xml output
  !    
  CONTAINS
     !
     !-----------------------------------------------------------------------
     SUBROUTINE io_global_start( mpime, ionode_set )
       !-----------------------------------------------------------------------
       !
       IMPLICIT NONE
       !
       INTEGER, INTENT(IN) :: mpime, ionode_set
       !
       !
       IF ( mpime == ionode_set ) THEN
          !
          ionode      = .TRUE.
          !
       ELSE
          !
          ionode      = .FALSE.
          !
       END IF
       !
       ionode_id      = ionode_set
       !
       first = .FALSE.
       !
       RETURN
       !
     END SUBROUTINE io_global_start
     !
     !-----------------------------------------------------------------------
     SUBROUTINE meta_io_global_start( mpime, ionode_set )
       !-----------------------------------------------------------------------
       !
       IMPLICIT NONE
       !
       INTEGER, INTENT(IN) :: mpime, ionode_set
       !
       !
       IF ( mpime == ionode_set ) THEN
          !
          meta_ionode      = .TRUE.
          !
       ELSE
          !
          meta_ionode      = .FALSE.
          !
       END IF
       !
       meta_ionode_id      = ionode_set
       !
       first = .FALSE.
       !
       RETURN
       !
     END SUBROUTINE meta_io_global_start
     !
     !
     !
     !-----------------------------------------------------------------------
     SUBROUTINE io_global_getionode( ionode_out, ionode_id_out )
       !-----------------------------------------------------------------------
       !
       IMPLICIT NONE
       !
       LOGICAL, INTENT(OUT) :: ionode_out
       INTEGER, INTENT(OUT) :: ionode_id_out
       !
       !
       IF ( first ) &
          CALL errore( ' io_global_getionode ', ' ionode not yet defined ', 1 )
       !
       ionode_out    = ionode
       ionode_id_out = ionode_id
       !
       RETURN
       !
     END SUBROUTINE io_global_getionode
     !  
     !  
     !-----------------------------------------------------------------------
     SUBROUTINE io_global_getmeta( myrank, root )
       !-----------------------------------------------------------------------
       !
       ! ... writes in module variables meta_ionode_id and meta_ionode
       !
       IMPLICIT NONE
       !
       INTEGER, INTENT(IN) :: myrank, root
       !
       !
       IF(myrank == root) THEN
       !
         meta_ionode   = .true.
       !
       ELSE
         meta_ionode = .false.
       !
       ENDIF 
       !
       meta_ionode_id = root
       !
       RETURN
       !
     END SUBROUTINE io_global_getmeta
     !
     !
END MODULE io_global
