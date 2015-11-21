!
! Copyright (C) 2001-2004 Carlo Cavazzoni and PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
! ... SUBROUTINE field_count:   accepts two string (one of them is optional) 
!                               and one integer and count the number of fields
!                               in the string separated by a blank or a tab 
!                               character. If the optional string is specified
!                               (it has anyway len=1) it is assumed as the 
!                               separator character.
!                               Ignores any character following the exclamation
!                               mark (fortran comment)
!
! ... SUBROUTINE con_cam:       counts the number of fields in a string 
!                               separated by the optional character
!
! ... SUBROUTINE field_compare: accepts two strings and one integer. Counts the
!                               fields contained in the first string and 
!                               compares it with the integer. 
!                               If they are less than the integer calls the 
!                               routine error and show by the second string the
!                               name of the field where read-error occurred.
!
! ... SUBROUTINE version_parse: Determine the major, minor and patch numbers from 
!                               a version string with the fmt "i.j.k"
!
! ... FUNCTION version_compare: Compare two version strings; the result can be
!                               "newer", "equal", "older", ""
! 
!
!----------------------------------------------------------------------------
MODULE parser
  !----------------------------------------------------------------------------
  !
  USE io_global, ONLY : stdout
  USE kinds, ONLY : DP
  !
  PRIVATE
  !
  PUBLIC :: parse_unit, field_count, read_line, get_field
  PUBLIC :: version_parse, version_compare
  !
  INTEGER :: parse_unit = 5 ! normally 5, but can be set otherwise
  !
  CONTAINS
  !
  !
  !--------------------------------------------------------------------------
  PURE SUBROUTINE field_count( num, line, car )
    !--------------------------------------------------------------------------
    !
    IMPLICIT NONE
    !
    INTEGER,                    INTENT(OUT) :: num
    CHARACTER(LEN=*),           INTENT(IN)  :: line
    CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: car
#if defined (__XLF)
    ! ... with the IBM xlf compiler some combination of flags lead to
    ! ... variables being defined as static, hence giving a conflict
    ! ... with PURE function. We then force the variable to be AUTOMATIC
    CHARACTER(LEN=1), AUTOMATIC             :: sep1, sep2    
    INTEGER, AUTOMATIC                      :: j
#else
    CHARACTER(LEN=1)                        :: sep1, sep2    
    INTEGER                                 :: j
#endif
    !
    !
    num = 0
    !
    IF ( .NOT. present(car) ) THEN
       !
       sep1 = char(32)  ! ... blank character
       sep2 = char(9)   ! ... tab character
       !
       DO j = 2, MAX( LEN( line ), 256 )
          !
          IF ( line(j:j) == '!' .OR. line(j:j) == char(0) ) THEN
             !
             IF ( line(j-1:j-1) /= sep1 .AND. line(j-1:j-1) /= sep2 ) THEN
                !
                num = num + 1
                !
             END IF   
             !
             EXIT
             !
          END IF
          !
          IF ( ( line(j:j) == sep1 .OR. line(j:j) == sep2 ) .AND. &
               ( line(j-1:j-1) /= sep1 .AND. line(j-1:j-1) /= sep2 ) ) THEN
             !
             num = num + 1
             !
          END IF
          !
       END DO
       !
    ELSE
       !
       sep1 = car
       !
       DO j = 2, MAX( LEN( line ), 256 )
          ! 
          IF ( line(j:j) == '!' .OR. &
               line(j:j) == char(0) .OR. line(j:j) == char(32) ) THEN
             !
             IF ( line(j-1:j-1) /= sep1 ) num = num + 1
             !
             EXIT
             !
          END IF
          !
          IF ( line(j:j) == sep1 .AND. line(j-1:j-1) /= sep1 ) num = num + 1
          !
       END DO
       !
    END IF
    !
    RETURN
    !
  END SUBROUTINE field_count
  !
  !
  !--------------------------------------------------------------------------
  SUBROUTINE read_line( line, nfield, field, end_of_file, error )
    !--------------------------------------------------------------------------
    !
    USE mp,        ONLY : mp_bcast
    USE mp_global, ONLY : world_comm
    USE io_global, ONLY : ionode, ionode_id
    !
    IMPLICIT NONE
    !
    CHARACTER(LEN=*),           INTENT(OUT) :: line
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)  :: field
    INTEGER,          OPTIONAL, INTENT(IN)  :: nfield
    LOGICAL,          OPTIONAL, INTENT(OUT) :: end_of_file, error
    LOGICAL                                 :: tend, terr
    !
    !
    IF( LEN( line ) < 256 ) THEN
       CALL errore(' read_line ', ' input line too short ', MAX(LEN(line),1) )
    END IF
    !
    tend = .FALSE.
    terr = .FALSE.
    IF ( ionode ) THEN
30     READ (parse_unit, fmt='(A256)', ERR=15, END=10) line
       IF( line == ' ' .OR. line(1:1) == '#' ) GO TO 30
       GO TO 20
10     tend = .TRUE.
       GO TO 20
15     terr = .TRUE.
20     CONTINUE
    END IF
    !
    CALL mp_bcast( tend, ionode_id, world_comm )
    CALL mp_bcast( terr, ionode_id, world_comm )
    CALL mp_bcast( line, ionode_id, world_comm )
    !
    IF( PRESENT(end_of_file) ) THEN
       end_of_file = tend
    ELSE IF( tend ) THEN
       CALL infomsg(' read_line ', ' end of file ' )
    END IF
    IF( PRESENT(error) ) THEN
       error = terr
    ELSE IF( terr ) THEN
       CALL infomsg(' read_line ', ' read error ' )
    END IF
    IF( PRESENT(field) .and. .not.(tend.or.terr) ) &
     &CALL field_compare( line, nfield, field )
    !
  END SUBROUTINE read_line
  !
  !
  !--------------------------------------------------------------------------
  SUBROUTINE field_compare( str, nf, var )
    !--------------------------------------------------------------------------
    !
    IMPLICIT NONE
    !
    CHARACTER(LEN=*), INTENT(IN) :: var
    INTEGER,          INTENT(IN) :: nf
    CHARACTER(LEN=*), INTENT(IN) :: str
    INTEGER                      :: nc
    !
    CALL field_count( nc, str )
    !
    IF( nc < nf ) &
      CALL errore( ' field_compare ', &
                 & ' wrong number of fields: ' // TRIM( var ), 1 )
    !
    RETURN
    !
  END SUBROUTINE field_compare
  !
  !
  !--------------------------------------------------------------------------
  SUBROUTINE con_cam(num, line, car)
    !--------------------------------------------------------------------------
    CHARACTER(LEN=*) :: line
    CHARACTER(LEN=1) :: sep
    CHARACTER(LEN=1), OPTIONAL :: car
    INTEGER :: num, j

    num = 0
    IF (len(line) .GT. 256 ) THEN
       WRITE( stdout,*) 'riga ', line
       WRITE( stdout,*) 'lunga ', len(line)
       num = -1
       RETURN
    END IF

    WRITE( stdout,*) '1riga ', line
    WRITE( stdout,*) '1lunga ', len(line)
    IF ( .NOT. present(car) ) THEN
       sep=char(32)             !char(32) is the blank character
    ELSE
       sep=car
    END IF

    DO j=2, MAX(len(line),256)
       IF ( line(j:j) == '!' .OR. line(j:j) == char(0)) THEN
          RETURN
       END IF
       IF ( (line(j:j) .EQ. sep) .AND. &
            (line(j-1:j-1) .NE. sep) )  THEN
          num = num + 1
       END IF
    END DO
    RETURN
  END SUBROUTINE con_cam
  !
  !--------------------------------------------------------------------------
  SUBROUTINE version_parse(str, major, minor, patch, ierr)
    !--------------------------------------------------------------------------
    !   
    ! Determine the major, minor and patch numbers from 
    ! a version string with the fmt "i.j.k"
    ! 
    ! The ierr variable assumes the following values
    !
    ! ierr < 0     emtpy string
    ! ierr = 0     no problem
    ! ierr > 0     fatal error
    !   
    IMPLICIT NONE
    CHARACTER(*),     INTENT(in)    :: str 
    INTEGER,          INTENT(out)   :: major, minor, patch, ierr
    !
    INTEGER       :: i1, i2, length
    INTEGER       :: ierrtot
    CHARACTER(10) :: num(3)

    !
    major = 0
    minor = 0
    patch = 0

    length = LEN_TRIM( str )
    !
    IF ( length == 0 ) THEN
       !
       ierr = -1
       RETURN
       !
    ENDIF

    i1 = SCAN( str, ".")
    i2 = SCAN( str, ".", BACK=.TRUE.)
    !
    IF ( i1 == 0 .OR. i2 == 0 .OR. i1 == i2 ) THEN
       !
       ierr = 1
       RETURN
       !
    ENDIF
    !
    num(1) = str(    1 : i1-1 )
    num(2) = str( i1+1 : i2-1 )
    num(3) = str( i2+1 : )
    !
    ierrtot = 0
    !
    READ( num(1), *, IOSTAT=ierr ) major
    IF (ierr/=0) RETURN
    !
    READ( num(2), *, IOSTAT=ierr ) minor
    IF (ierr/=0) RETURN
    !
    READ( num(3), *, IOSTAT=ierr ) patch
    IF (ierr/=0) RETURN
    !
  END SUBROUTINE version_parse
  !
  !--------------------------------------------------------------------------
  FUNCTION version_compare(str1, str2)
    !--------------------------------------------------------------------------
    !   
    ! Compare two version strings; the result is
    ! 
    ! "newer":   str1 is newer that str2    
    ! "equal":   str1 is equal   to str2    
    ! "older":   str1 is older than str2    
    ! " ":       str1 or str2 has a wrong format
    !   
    IMPLICIT NONE
    CHARACTER(*)  :: str1, str2
    CHARACTER(10) :: version_compare
    !
    INTEGER   :: version1(3), version2(3)
    INTEGER   :: basis, icheck1, icheck2
    INTEGER   :: ierr
    !

    version_compare = " "
    !
    CALL version_parse( str1, version1(1), version1(2), version1(3), ierr) 
    IF ( ierr/=0 ) RETURN
    !
    CALL version_parse( str2, version2(1), version2(2), version2(3), ierr) 
    IF ( ierr/=0 ) RETURN
    !
    ! 
    basis = 1000
    !
    icheck1 = version1(1) * basis**2 + version1(2)* basis + version1(3) 
    icheck2 = version2(1) * basis**2 + version2(2)* basis + version2(3) 
    !
    IF ( icheck1 > icheck2 ) THEN
       !
       version_compare = 'newer'
       !
    ELSEIF( icheck1 == icheck2 ) THEN
       !
       version_compare = 'equal'
       !
    ELSE
       !
       version_compare = 'older'
       !
    ENDIF
    !
  END FUNCTION version_compare
  !
  !--------------------------------------------------------------------------
  SUBROUTINE get_field(n, field, str, sep)
    !--------------------------------------------------------------------------
    ! Extract whitespace-separated nth block from string
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: n
    CHARACTER(len=*),INTENT(OUT) :: field
    CHARACTER(len=*),INTENT(IN)  :: str
    CHARACTER(len=1),OPTIONAL,INTENT(IN) :: sep
    INTEGER :: i,j,z ! block start and end
    INTEGER :: k     ! block counter
    CHARACTER(len=1) :: sep1, sep2
    !print*, "------------- parser start -------------"
    !print '(3a)', "string: -->", str,"<--"
    IF(present(sep)) THEN
      sep1 = sep
      sep2 = sep ! redundant, but easy
    ELSE
      sep1 = char(32)  ! ... blank character
      sep2 = char(9)   ! ... tab char
    ENDIF
    !
    k = 1 ! counter for the required block
    !
    DO i = 1,len(str)
    ! look for the beginning of the required block
      z = MAX(i-1,1)
      !print '(2a1,3i4,2l)', str(i:i), str(z:z), i,z,k,n,&
      !       (str(i:i) == sep1 .or. str(i:i) == sep2), (str(z:z) /= sep1 .and. str(z:z) /= sep2)
      IF( k == n) EXIT
      IF( (str(i:i) == sep1 .or. str(i:i) == sep2) &
           .and. &
          (str(z:z) /= sep1 .and. str(z:z) /= sep2) &
        ) &
        k = k+1
    ENDDO
    !
    !print*, "i found: ",i
    DO j = i,len(str)
    ! look for the beginning of the next block
      z = MAX(j-1,1)
      IF( (str(j:j) == sep1 .or. str(j:j) == sep2) &
           .and. &
          (str(z:z) /= sep1 .and. str(z:z) /= sep2) &
        ) &
        k = k+1
      IF( k >n) EXIT
    ENDDO
    !print*, "j found: ",j
    !
    IF (j <= len(str)) THEN
      ! if we are here, the reqired block was followed by a separator
      ! and another field, we have to trash one char (a separator)
      field = TRIM(adjustl(str(i:j-1)))
      !print*, "taking: ",i,j-2
    ELSE
      ! if we are here, it was the last block in str, we have to take
      ! all the remaining chars
      field = TRIM(adjustl(str(i:len(str))))
      !print*, "taking from ",i
    ENDIF
    !print*, "------------- parser end -------------"

  END SUBROUTINE get_field

END MODULE parser
