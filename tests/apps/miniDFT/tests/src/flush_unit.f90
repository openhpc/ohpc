!
! Copyright (C) 2005 PWSCF-FPMD-CPV groups
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!  
#if defined(__XLF) || defined(__ABSOFT)
   #define flush flush_
#endif
!
!----------------------------------------------------------------------------
SUBROUTINE flush_unit( unit_tobeflushed )
  !----------------------------------------------------------------------------
  !
  ! ... this is a wrapper to the standard flush routine
  !
  INTEGER, INTENT(IN) :: unit_tobeflushed
  LOGICAL             :: opnd
  !
  !
  INQUIRE( UNIT = unit_tobeflushed, OPENED = opnd )
  !
  IF ( opnd ) CALL flush( unit_tobeflushed )
  !
  RETURN
  !
END SUBROUTINE
