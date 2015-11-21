!
! Copyright (C) 2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
MODULE buffers
  !----------------------------------------------------------------------------
  !
  USE kinds, ONLY : DP
  !
  PRIVATE
  PUBLIC :: open_buffer, init_buffer, get_buffer, save_buffer, close_buffer
  !
  SAVE
  !
  ! ... global variables
  !
  COMPLEX(DP), ALLOCATABLE :: buffer1(:,:)
  INTEGER :: nword_
  CHARACTER(LEN=80) :: extension_
  !
  INTEGER, EXTERNAL :: find_free_unit
  !
  CONTAINS
  !-----------------------------------------------------------------------
  SUBROUTINE open_buffer (unit, extension, nword, maxrec, exst)
  !-----------------------------------------------------------------------
  !
  !     unit > 6 : connect unit "unit" to a file "prefix"."extension" in
  !     tmp_dir for direct I/O access, record length nword complex numbers;
  !     maxrec is ignored, exst=T(F) if the file (does not) exists
  !
  !     unit =-1 : allocate a buffer for storing up to maxrec records
  !     of length nword complex numbers; extension is saved but ignored
  !     exst=T(F) if the buffer is already allocated
  !
  USE io_files,  ONLY : diropn
  !
  IMPLICIT NONE
  !
  CHARACTER(LEN=*), INTENT(IN) :: extension
  INTEGER, INTENT(IN) :: unit, nword, maxrec
  LOGICAL, INTENT(OUT) :: exst
  !
  INTEGER :: ierr
  !
  exst = .FALSE.
  IF ( unit == -1 ) THEN
     !
     exst =  ALLOCATED ( buffer1 )
     !
     IF ( exst ) THEN
        !
        CALL infomsg ('open_buffer', 'buffer already allocated')
        !
     ELSE
        !
        nword_ = nword
        extension_ = extension
        ALLOCATE ( buffer1 ( nword, maxrec ) )
        !
     END IF
     !
  ELSE IF ( unit > 6 ) THEN
     !
     CALL diropn (unit, extension, 2*nword, exst)
     !
  ELSE
     !
     CALL errore ('open_buffer', 'incorrect unit specified', ABS(unit))
     !
  END IF
  !
  RETURN
  !
END SUBROUTINE open_buffer
!
!----------------------------------------------------------------------------
SUBROUTINE save_buffer( vect, nword, unit, nrec )
  !----------------------------------------------------------------------------
  !
  ! ... copy vect(1:nword) into the "nrec"-th record of
  ! ... - a previously allocated buffer, if unit = -1
  ! ... - a previously opened direct-access file with unit > 6
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: nword, unit, nrec
  COMPLEX(DP), INTENT(IN) :: vect(nword)
  !
  IF ( unit == -1 ) THEN
     !
     IF ( ALLOCATED ( buffer1 ) ) THEN
        !
        IF ( nrec > SIZE ( buffer1, 2) )  &
           CALL errore ('save_buffer', 'too many records', ABS(nrec))
        !
        IF ( nword /= SIZE ( buffer1, 1) )  &
           CALL errore ('save_buffer', 'record length mismatch', ABS(nword))
        !
        buffer1(:,nrec) = vect(:)
        !
     ELSE
        !
        CALL errore ('save_buffer', 'buffer not allocated', ABS(unit))
        !
     END IF
     !
  ELSE IF ( unit > 6 ) THEN
     !
     CALL davcio ( vect, 2*nword, unit, nrec, +1 )
     !
  ELSE
     !
     CALL errore ('save_buffer', 'incorrect unit specified', ABS(unit))
     !
  END IF
  !
  RETURN
  !
END SUBROUTINE save_buffer
!
!----------------------------------------------------------------------------
SUBROUTINE get_buffer( vect, nword, unit, nrec )
  !----------------------------------------------------------------------------
  !
  ! ... copy vect(1:nword) from the "nrec"-th record of
  ! ... - a previously allocated buffer, if unit = -1
  ! ... - a previously opened direct-access file with unit > 6
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: nword, unit, nrec
  COMPLEX(DP), INTENT(OUT) :: vect(nword)
  !
  IF ( unit == -1 ) THEN
     !
     IF ( ALLOCATED ( buffer1 ) ) THEN
        !
        IF ( nrec > SIZE ( buffer1, 2) )  &
           CALL errore ('get_buffer', 'no such record', ABS(nrec))
        !
        IF ( nword /= SIZE ( buffer1, 1) )  &
           CALL errore ('get_buffer', 'record length mismatch', ABS(nword))
        !
        vect(:) = buffer1(:,nrec)
        !
     ELSE
        !
        CALL errore ('get_buffer', 'buffer not allocated', ABS(unit))
        !
     END IF
     !
  ELSE IF ( unit > 6 ) THEN
     !
     CALL davcio ( vect, 2*nword, unit, nrec, -1 )
     !
  ELSE
     !
     CALL errore ('get_buffer', 'incorrect unit specified', ABS(unit))
     !
  END IF
  !
  RETURN
  !
END SUBROUTINE get_buffer
!
SUBROUTINE close_buffer ( unit, status )
  !
  !     unit > 6 : close unit with status "status" ('keep' or 'delete')
  !     unit =-1 : deallocate buffer; if "status='keep'" save to file
  !                (using saved value of extension)
  !
  USE io_files, ONLY : diropn
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: unit
  CHARACTER(LEN=*), INTENT(IN) :: status
  !
  INTEGER :: unit_, i
  LOGICAL :: exst, opnd
  !
  IF ( unit == -1 ) THEN
     !
     IF ( ALLOCATED ( buffer1 ) ) THEN
        !
        IF ( TRIM(status) == 'KEEP' .OR. TRIM(status) == 'keep') THEN
           !
           unit_ = find_free_unit () 
           CALL diropn (unit_, extension_, 2*nword_, exst)
           DO i = 1, SIZE (buffer1, 2)
              CALL davcio ( buffer1(1,i), 2*nword_, unit_, i, +1 )
           END DO
           CLOSE( UNIT = unit_, STATUS = status )
           !
        END IF
        !
        DEALLOCATE (buffer1)
        !
     ELSE
        !
        CALL infomsg ('close_buffer', 'buffer not allocated')
        !
     END IF
     !
  ELSE IF ( unit > 6 ) THEN
     !
     INQUIRE( UNIT = unit, OPENED = opnd )
     !
     IF ( opnd ) CLOSE( UNIT = unit, STATUS = status )
     !
  ELSE
     !
     CALL infomsg ('get_buffer', 'incorrect unit specified')
     !
  END IF
  !
END SUBROUTINE close_buffer
!
SUBROUTINE init_buffer ( unit, exst, ierr )
  !
  !     unit > 6 : ignored
  !     unit =-1 : read into buffer the array previously saved to file
  !                when the buffer was closed (used in NEB calculations)
  !     exst     : T if the file where to read from is present
  !     ierr     : 0 if everything ok, 1 otherwise
  !
  USE io_files, ONLY : diropn
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: unit
  INTEGER, INTENT(OUT) :: ierr
  LOGICAL, INTENT(OUT) :: exst
  !
  INTEGER :: unit_, i
  !
  ierr = 1 
  !
  IF ( unit == -1 ) THEN
     !
     IF ( .NOT. ALLOCATED ( buffer1 ) ) THEN
        CALL infomsg ('init_buffer', 'buffer not allocated')
        RETURN
     END IF
     !
     unit_ = find_free_unit () 
     CALL diropn (unit_, extension_, 2*nword_, exst)
     IF ( .NOT. exst ) THEN
        CLOSE (UNIT = unit_ , STATUS = 'delete')
        RETURN
     END IF
     !
     DO i = 1, SIZE (buffer1, 2)
        CALL davcio ( buffer1(1,i), 2*nword_, unit_, i, -1 )
     END DO
     CLOSE( UNIT = unit_, STATUS = 'keep' )
     ierr = 0
     !
  ELSE
     !
     CALL infomsg ('init_buffer', 'incorrect unit specified')
     !
  END IF
  !
END SUBROUTINE init_buffer
!
END MODULE buffers
