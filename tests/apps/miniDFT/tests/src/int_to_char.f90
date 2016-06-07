!
! Copyright (C) 2009 Quantum ESPRESSO groups
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
  !-----------------------------------------------------------------------
  FUNCTION int_to_char( i )
    !-----------------------------------------------------------------------
    !
    ! ... converts an integer number of up to 6 figures
    ! ... into a left-justifed character variable 
    !
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: i
    CHARACTER (LEN=6)   :: int_to_char
    CHARACTER :: c
    INTEGER   :: n, j, nc
    LOGICAL   :: neg
    !   
    nc = 6
    !
    IF( i < 0 ) then
       nc  = nc - 1
       n   = -i
       neg = .true.
    ELSE
       n   = i
       neg = .false.
    END IF
    !
    j = 1
    DO WHILE( j <= nc ) 
       int_to_char(j:j) = CHAR( MOD( n, 10 ) + ICHAR( '0' ) )
       n = n / 10
       IF( n == 0 ) EXIT
       j = j + 1
    END DO
    !
    IF( j <= nc ) THEN
       DO n = 1, j/2
          c = int_to_char( n : n )
          int_to_char( n : n ) = int_to_char( j-n+1 : j-n+1 )
          int_to_char( j-n+1 : j-n+1 ) = c
       END DO
       IF( j < nc ) int_to_char(j+1:nc) = ' '
    ELSE
       int_to_char(:) = '*'
    END IF
    !
    IF( neg ) THEN
       DO n = nc+1, 2, -1
          int_to_char(n:n) = int_to_char(n-1:n-1)
       END DO
       int_to_char(1:1) = '-'
    END IF
    !
    RETURN
    !
  END FUNCTION int_to_char
