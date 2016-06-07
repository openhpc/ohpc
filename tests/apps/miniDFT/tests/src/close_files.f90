!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
SUBROUTINE close_files(lflag)
  !----------------------------------------------------------------------------
  !
  ! ... Close all files and synchronize processes for a new scf calculation.
  !
  USE control_flags, ONLY : twfcollect, io_level
  USE fixed_occ,     ONLY : one_atom_occupations
  USE io_files,      ONLY : prefix, iunwfc, iunigk, iunat, iunsat, &
                            iunefield, iunefieldm, iunefieldp
  USE buffers,       ONLY : close_buffer
  USE mp_global,     ONLY : intra_image_comm
  USE mp,            ONLY : mp_barrier
  !
  IMPLICIT NONE
  !
  LOGICAL, intent(in) :: lflag
  !
  LOGICAL :: opnd
  !  ... close buffer/file containing wavefunctions: discard if
  !  ... wavefunctions are written in xml format, save otherwise
  !
  !BMA: eliminiate filesystem activity for miniDFT
  !IF ( lflag .AND. (twfcollect .OR. io_level < 0 )) THEN
  !   CALL close_buffer ( iunwfc, 'DELETE' )
  !ELSE
  !   CALL close_buffer ( iunwfc, 'KEEP' )
  !END IF
  !
  ! ... iunigk is kept open during the execution - close and remove
  !
  ! BMA: eliminating filesystem ops that arent used by mini-app
  !INQUIRE( UNIT = iunigk, OPENED = opnd )
  !IF ( opnd ) CLOSE( UNIT = iunigk, STATUS = 'DELETE' )
  !
  ! ... iunat  contains the (orthogonalized) atomic wfcs 
  ! ... iunsat contains the (orthogonalized) atomic wfcs * S
  !
  IF ( one_atom_occupations) THEN
     !
     INQUIRE( UNIT = iunat, OPENED = opnd )  
     IF ( opnd ) CLOSE( UNIT = iunat, STATUS = 'KEEP' )
     INQUIRE( UNIT = iunsat, OPENED = opnd )  
     IF ( opnd ) CLOSE( UNIT = iunsat, STATUS = 'KEEP' )
     !
  END IF
  !
  ! ... close unit for electric field if needed
  !
  !
  CALL mp_barrier( intra_image_comm )  
  !
  RETURN
  !
END SUBROUTINE close_files
