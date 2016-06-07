!
! Copyright (C) 2001-2004 Carlo Cavazzoni and PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
FUNCTION matches( string1, string2 )  
  !-----------------------------------------------------------------------
  !
  ! ... .TRUE. if string1 is contained in string2, .FALSE. otherwise
  !
  IMPLICIT NONE
  !
  CHARACTER (LEN=*), INTENT(IN) :: string1, string2
  LOGICAL                       :: matches
  INTEGER                       :: len1, len2, l  
  !
  !
  len1 = LEN_TRIM( string1 )  
  len2 = LEN_TRIM( string2 )  
  !
  DO l = 1, ( len2 - len1 + 1 )  
     !   
     IF ( string1(1:len1) == string2(l:(l+len1-1)) ) THEN  
        !
        matches = .TRUE.  
        !
        RETURN  
        !
     END IF
     !
  END DO
  !
  matches = .FALSE.
  ! 
  RETURN
  !
END FUNCTION matches
!
!-----------------------------------------------------------------------
FUNCTION imatches( string1, string2 )
  !-----------------------------------------------------------------------
  !
  ! ... .TRUE. if string1 is contained in string2, .FALSE. otherwise
  !   *** case insensitive ***
  !
  IMPLICIT NONE
  !
  CHARACTER (LEN=*), INTENT(IN) :: string1, string2
  CHARACTER(LEN=len(string1))   :: aux1
  CHARACTER(LEN=len(string2))   :: aux2
  CHARACTER(LEN=1)              :: lowercase
  LOGICAL                       :: imatches
  LOGICAL, EXTERNAL             :: matches
  INTEGER                       :: i
  !
  aux1 = string1
  aux2 = string2
  ! 
  do i=1,len(aux1)
     aux1(i:i)=lowercase(aux1(i:i))
  enddo
  do i=1,len(aux2)
     aux2(i:i)=lowercase(aux2(i:i))
  enddo
  !
  imatches = matches(aux1, aux2)
  !
  RETURN
  !
END FUNCTION imatches
!
!-----------------------------------------------------------------------
SUBROUTINE remove_comments_from_string( string )  
  !-----------------------------------------------------------------------
  !
  ! chop string removing everything after an esclamation mark (!)
  !
  IMPLICIT NONE
  !
  CHARACTER (LEN=*), INTENT(INOUT) :: string
  INTEGER                       :: len, l  
  !
  !
  len = LEN_TRIM( string )  
  !
  l=1
  DO WHILE ( string(l:l) /= "!" ) 
     l = l + 1
     if (l == len+1) EXIT 
  END DO
  len = l-1
  !
  string = string(1:len)
  ! 
  RETURN
  !
END SUBROUTINE remove_comments_from_string
!
