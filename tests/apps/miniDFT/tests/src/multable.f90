!
! Copyright (C) 2001-2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
SUBROUTINE multable (nsym, s, table)
  !-----------------------------------------------------------------------
  !
  !  Checks that {S} is a group and calculates multiplication table
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: nsym, s(3,3,nsym)
  ! nsym = number of symmetry operations
  ! s    = rotation matrix (in crystal axis, represented by integers)
  INTEGER, INTENT(OUT) :: table (48, 48)
  ! multiplication table:  S(n)*S(m) = S (table(n,m) )
  !
  INTEGER :: isym, jsym, ksym, ss (3, 3)
  LOGICAL :: found, smn
  !
  DO isym = 1, nsym
     DO jsym = 1, nsym
        ! 
        ss = MATMUL (s(:,:,jsym),s(:,:,isym))
        !
        !     here we check that the input matrices really form a group
        !     and we set the multiplication table
        !
        found = .false.
        DO ksym = 1, nsym
           smn =  ALL ( s(:,:,ksym) == ss(:,:) )
           IF (smn) THEN
              IF (found) CALL errore ('multable', 'Not a group', 1)
              found = .true.
              table (jsym, isym) = ksym
           END IF
        END DO
        IF ( .NOT.found) CALL errore ('multable', ' Not a group', 2)
     END DO
  END DO
  RETURN
  !
END SUBROUTINE multable
