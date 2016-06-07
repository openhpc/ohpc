!
! Copyright (C) 2002-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
  !--------------------------------------------------------------------------
  FUNCTION find_free_unit()
    !--------------------------------------------------------------------------
    !
    IMPLICIT NONE
    !
    INTEGER :: find_free_unit
    INTEGER :: iunit
    LOGICAL :: opnd
    !
    !
    unit_loop: DO iunit = 99, 1, -1
       !
       INQUIRE( UNIT = iunit, OPENED = opnd )
       !
       IF ( .NOT. opnd ) THEN
          !
          find_free_unit = iunit
          !
          RETURN
          !
       END IF
       !
    END DO unit_loop
    !
    CALL errore( 'find_free_unit()', 'free unit not found ?!?', 1 )
    !
    RETURN
    !
  END FUNCTION find_free_unit
  !
