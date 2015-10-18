!
! Copyright (C) 2004-2012 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!--------------------------------------------------------------------------
!
! NOTE: by default the following macro is DISABLED: the default version of the 
!       subroutines and functions is in the second half of the file.
!
! NOTE: the mkdir function is NOT called directly as it return error on directory
!       already existing, we are instead using a c wrapper (c_mkdir_safe)
#ifdef __ISO_C_BINDING
!
MODULE wrappers
  USE kinds, ONLY : DP
  USE io_global, ONLY : stdout
  USE ISO_C_BINDING
  IMPLICIT NONE
  !
  ! C std library functions fortran wrappers:
  PUBLIC  f_remove, f_link, rename, f_chdir, f_mkdir, f_rmdir, f_getcwd
  ! more stuff:
  PUBLIC  feval_infix, md5_from_file
  !
  ! HELP:
  ! integer f_remove(pathname)
  ! integer f_rename(oldfile, newfile)
  ! integer f_chdir(newdir)
  ! integer f_chmod(mode) i.e. mode=777
  ! integer f_mkdir(dirname, mode) mode is optional
  ! integer f_rmdir(dirname)
  ! subroutine f_getcwd(dirname) 
  ! All *name are fortran characters of any length characters,
  ! mode are integers, all functions return 0 if successful, -1 otherwise
  !
  ! real(dp) :: result = feval_infix(integer:: ierr, character(len=*) :: expression)
  ! subroutine md5_from_file(character(len=*) :: filename, character(len=32) ::md5)
  PRIVATE
  !
  SAVE
  !
  ! Interfaces to the C functions, these are kept private as Fortran
  ! characters have (?) to be converted explicitly to C character arrays.
  ! Use the f_* wrappers instead
  INTERFACE
    FUNCTION remove(pathname) BIND(C,name="remove") RESULT(r)
      USE iso_c_binding
      CHARACTER(kind=c_char),INTENT(in)  :: pathname(*)
      INTEGER(c_int)        :: r
    END FUNCTION
    FUNCTION rename(input,output) BIND(C,name="rename") RESULT(r)
      USE iso_c_binding
      CHARACTER(kind=c_char),INTENT(in) :: input(*)
      CHARACTER(kind=c_char),INTENT(in) :: output(*)
      INTEGER(c_int)        :: r
    END FUNCTION
    FUNCTION link(input,output) BIND(C,name="link") RESULT(r)
      USE iso_c_binding
      CHARACTER(kind=c_char),INTENT(in) :: input(*)
      CHARACTER(kind=c_char),INTENT(in) :: output(*)
      INTEGER(c_int)        :: r
    END FUNCTION
!    FUNCTION chmod(filename,mode) BIND(C,name="chmod") RESULT(r)
!      USE iso_c_binding
!      CHARACTER(kind=c_char),INTENT(in)  :: filename(*)
!      INTEGER(c_int),VALUE  ,INTENT(in)  :: mode
!      INTEGER(c_int)        :: r
!    END FUNCTION
    FUNCTION chdir(filename) BIND(C,name="chdir") RESULT(r)
      USE iso_c_binding
      CHARACTER(kind=c_char),INTENT(in)  :: filename(*)
      INTEGER(c_int)        :: r
    END FUNCTION
!    FUNCTION mkdir(dirname,mode) BIND(C,name="c_mkdir") RESULT(r)
!      USE iso_c_binding
!      CHARACTER(kind=c_char),INTENT(in)  :: dirname(*)
!      INTEGER(c_int),VALUE  ,INTENT(in)  :: mode
!      INTEGER(c_int)        :: r
!    END FUNCTION
    FUNCTION mkdir_safe(dirname) BIND(C,name="c_mkdir_safe") RESULT(r)
      USE iso_c_binding
      CHARACTER(kind=c_char),INTENT(in)  :: dirname(*)
      INTEGER(c_int)        :: r
    END FUNCTION
    FUNCTION rmdir(dirname) BIND(C,name="rmdir") RESULT(r)
      USE iso_c_binding
      CHARACTER(kind=c_char),INTENT(in)  :: dirname(*)
      INTEGER(c_int)        :: r
    END FUNCTION
    FUNCTION getcwd(buffer,size) BIND(C,name="getcwd") RESULT(r)
      USE iso_c_binding
      CHARACTER(kind=c_char) ,INTENT(out) :: buffer(*)
      INTEGER(c_size_t),VALUE,INTENT(in)  :: size
      TYPE(c_ptr)  :: r
    END FUNCTION
  END INTERFACE
  !
  ! ====================================================================
CONTAINS
  ! ====================================================================
  ! fortran wrappers functions that call the C functions after converting
  ! frotran characters to C character arrays
  FUNCTION f_remove(filename) RESULT(r)
    CHARACTER(*),INTENT(in)  :: filename
    INTEGER(c_int) :: r
    r= remove(TRIM(filename)//C_NULL_CHAR)
  END FUNCTION

  FUNCTION f_rename(input,output) RESULT(k)
    CHARACTER(*),INTENT(in)  :: input,output
    INTEGER :: k
    k= rename(TRIM(input)//C_NULL_CHAR,TRIM(output)//C_NULL_CHAR)
  END FUNCTION

  FUNCTION f_link(input,output) RESULT(k)
    CHARACTER(*),INTENT(in)  :: input,output
    INTEGER :: k
    k= link(TRIM(input)//C_NULL_CHAR,TRIM(output)//C_NULL_CHAR)
  END FUNCTION
  
  FUNCTION f_chdir(dirname) RESULT(r)
    CHARACTER(*),INTENT(in)  :: dirname
    INTEGER(c_int) :: r
    r= chdir(TRIM(dirname)//C_NULL_CHAR)
  END FUNCTION

  FUNCTION f_mkdir(dirname) RESULT(r)
    CHARACTER(*),INTENT(in)  :: dirname
    INTEGER(c_int) :: r
    r= mkdir_safe(TRIM(dirname)//C_NULL_CHAR)
  END FUNCTION
  
!  FUNCTION f_chmod(filename, mode) RESULT(r)
!    CHARACTER(*),INTENT(in)  :: filename
!    INTEGER,INTENT(in) :: mode
!    INTEGER(c_int) :: r
!    INTEGER(c_int) :: c_mode
!    c_mode = INT(mode, kind=c_int)
!    r= chmod(TRIM(filename)//C_NULL_CHAR, c_mode)
!  END FUNCTION

  FUNCTION f_rmdir(dirname) RESULT(r)
    CHARACTER(*),INTENT(in)  :: dirname
    INTEGER(c_int) :: r
    r= rmdir(TRIM(dirname)//C_NULL_CHAR)
  END FUNCTION
  
  SUBROUTINE f_getcwd(output)
    CHARACTER(kind=c_char,len=*),INTENT(out) :: output
    TYPE(c_ptr) :: buffer
    INTEGER(C_LONG) :: length,i
    length=LEN(output)
    buffer=getcwd(output,length)
    DO i=1,length
      IF(output(i:i) == C_NULL_CHAR) EXIT
    ENDDO
    output(i:)=' '
  END SUBROUTINE
  !
  ! ==================================================================== 
  ! Two more wrappers for eval_infix (simple algebric expression parser)
  ! and for get_md5 which computes the md5 sum of a file.
  !
  FUNCTION feval_infix(fierr, fstr)
    IMPLICIT NONE
    REAL(DP) :: feval_infix
    INTEGER :: fierr
    CHARACTER(len=*) :: fstr
    INTEGER :: filen
    !
    INTERFACE
    REAL(kind=c_double) FUNCTION ceval_infix(cierr, cstr, cilen) BIND(C, name="eval_infix")
    !  double eval_infix( int *ierr, const char *strExpression, int len )
      USE ISO_C_BINDING
      INTEGER(kind=c_int)    :: cierr
      CHARACTER(kind=c_char) :: cstr(*)
      INTEGER(kind=c_int),VALUE :: cilen
    END FUNCTION ceval_infix
    END INTERFACE
    !
    INTEGER(kind=c_int) :: cierr
    INTEGER(kind=c_int) :: cilen
    CHARACTER(len=len_trim(fstr)+1,kind=c_char) :: cstr
    !
    INTEGER :: i
    !
    filen = len_trim(fstr)
    cilen = INT(filen, kind=c_int)
    DO i = 1,filen
      cstr(i:i) = fstr(i:i)
    ENDDO
    cstr(filen+1:filen+1)=C_NULL_CHAR
    !
    feval_infix = REAL( ceval_infix(cierr, cstr, cilen), kind=DP)
    fierr = INT(cierr)
    RETURN
  END FUNCTION feval_infix
  !
  !
  SUBROUTINE md5_from_file (ffile, fmd5)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT (IN) :: ffile
    CHARACTER(len=32), INTENT (OUT) :: fmd5
    !
    INTERFACE
    SUBROUTINE cget_md5(cfile, cmd5, cierr) BIND(C, name="get_md5")
    ! void get_md5(const char *file, char *md5, int err)
      USE ISO_C_BINDING
      CHARACTER(kind=c_char) :: cfile(*)
      CHARACTER(kind=c_char) :: cmd5(*)
      INTEGER(kind=c_int)    :: cierr
    END SUBROUTINE cget_md5
    END INTERFACE
    !
    INTEGER,PARAMETER :: md5_length = 32
    INTEGER :: i
    !
    CHARACTER(len=len_trim(ffile)+1,kind=c_char) :: cfile!(*)
    CHARACTER(len=(md5_length+1),kind=c_char)    :: cmd5!(*)
    INTEGER(kind=c_int)    :: cierr
    !
    cfile = TRIM(ffile)//C_NULL_CHAR
    !
    CALL cget_md5(cfile, cmd5, cierr)
    !
    DO i = 1,md5_length
       fmd5(i:i) = cmd5(i:i)
    ENDDO
    !
  END SUBROUTINE 
END MODULE
! ==================================================================== 
#else 
! interfaces not using iso_c_binding follow
! ==================================================================== 
MODULE wrappers
  !
  ! these routines are used to pass fortran strings to C routines  in a
  ! safe way. Strings are converted to integer arrays here,  passed to 
  ! C wrappers, converted back to strings. Other ways to pass fortran
  ! strings to C turned out to be non portable and not safe
  !
   USE kinds, ONLY : DP
   USE io_global,  ONLY : stdout
   IMPLICIT NONE
   SAVE
CONTAINS
   !
   FUNCTION feval_infix( ierr, str )
     REAL(DP) :: feval_infix
     INTEGER :: ierr
     CHARACTER(LEN=*) :: str
     INTEGER :: i, ilen
     INTEGER, ALLOCATABLE :: istr(:)
     REAL(DP), EXTERNAL :: eval_infix_wrapper
     ALLOCATE( istr( LEN( str ) ) )
     DO i = 1, LEN( str )
        istr(i) = ICHAR( str(i:i) )
        IF( istr(i) < 0 .OR. istr(i) > 127 ) &
           CALL errore( ' feval_infix ', ' invalid character ', ABS( istr(i) ) )
     END DO 
     ilen = LEN( str )
     feval_infix = eval_infix_wrapper( ierr, istr, ilen )
     DEALLOCATE( istr )
     RETURN
   END FUNCTION
   !
   FUNCTION f_mkdir( dirname )
     INTEGER :: f_mkdir
     CHARACTER(LEN=*) :: dirname
     INTEGER :: i, ilen
     INTEGER, ALLOCATABLE :: istr(:)
     INTEGER, EXTERNAL :: c_mkdir_int
     ALLOCATE( istr( LEN_TRIM( dirname ) ) )
     DO i = 1, LEN_TRIM( dirname )
        istr(i) = ICHAR( dirname(i:i) )
        IF( istr(i) < 0 .OR. istr(i) > 127 ) &
           CALL errore( ' f_mkdir ', ' invalid character ', ABS( istr(i) ) )
     END DO 
     ilen = LEN_TRIM( dirname )
     f_mkdir = c_mkdir_int( istr, ilen )
     DEALLOCATE( istr )
     RETURN
   END FUNCTION
   !
   FUNCTION f_chdir( dirname )
     INTEGER :: f_chdir
     CHARACTER(LEN=*) :: dirname
     INTEGER :: i, ilen
     INTEGER, ALLOCATABLE :: istr(:)
     INTEGER, EXTERNAL :: c_chdir_int
     ALLOCATE( istr( LEN_TRIM( dirname ) ) )
     DO i = 1, LEN_TRIM( dirname )
        istr(i) = ICHAR( dirname(i:i) )
        IF( istr(i) < 0 .OR. istr(i) > 127 ) &
           CALL errore( ' f_chdir ', ' invalid character ', ABS( istr(i) ) )
     END DO
     ilen = LEN_TRIM( dirname )
     f_chdir = c_chdir_int( istr, ilen )
     DEALLOCATE( istr )
     RETURN
   END FUNCTION
   !
   FUNCTION f_rename( oldname, newname )
     INTEGER :: f_rename
     CHARACTER(LEN=*) :: oldname
     CHARACTER(LEN=*) :: newname
     INTEGER :: i, lold, lnew
     INTEGER, ALLOCATABLE :: iold(:)
     INTEGER, ALLOCATABLE :: inew(:)
     INTEGER, EXTERNAL :: c_rename_int
     lold = LEN( oldname )
     lnew = LEN( newname )
     ALLOCATE( iold( lold ) )
     ALLOCATE( inew( lnew ) )
     DO i = 1, lold
        iold(i) = ICHAR( oldname(i:i) )
        IF( iold(i) < 0 .OR. iold(i) > 127 ) &
           CALL errore( ' f_rename ', ' invalid character ', ABS( iold(i) ) )
     END DO
     DO i = 1, lnew
        inew(i) = ICHAR( newname(i:i) )
        IF( inew(i) < 0 .OR. inew(i) > 127 ) &
           CALL errore( ' f_rename ', ' invalid character ', ABS( inew(i) ) )
     END DO
     f_rename = c_rename_int( iold, lold, inew, lnew )
     DEALLOCATE( inew )
     DEALLOCATE( iold )
     RETURN
   END FUNCTION
   !
   FUNCTION f_link( oldname, newname )
     INTEGER :: f_link
     CHARACTER(LEN=*) :: oldname
     CHARACTER(LEN=*) :: newname
     INTEGER :: i, lold, lnew
     INTEGER, ALLOCATABLE :: iold(:)
     INTEGER, ALLOCATABLE :: inew(:)
     INTEGER, EXTERNAL :: c_link_int
     lold = LEN( oldname )
     lnew = LEN( newname )
     ALLOCATE( iold( lold ) )
     ALLOCATE( inew( lnew ) )
     DO i = 1, lold
        iold(i) = ICHAR( oldname(i:i) )
        IF( iold(i) < 0 .OR. iold(i) > 127 ) &
           CALL errore( ' f_link ', ' invalid character ', ABS( iold(i) ) )
     END DO
     DO i = 1, lnew
        inew(i) = ICHAR( newname(i:i) )
        IF( inew(i) < 0 .OR. inew(i) > 127 ) &
           CALL errore( ' f_link ', ' invalid character ', ABS( inew(i) ) )
     END DO
     f_link = c_link_int( iold, lold, inew, lnew )
     DEALLOCATE( inew )
     DEALLOCATE( iold )
     RETURN
   END FUNCTION
   !

   SUBROUTINE md5_from_file (filename, md5)
     
     CHARACTER(LEN=*), INTENT (IN) :: filename
     CHARACTER(len=32), INTENT (OUT) :: md5
     CHARACTER(LEN=len_trim(filename)) :: ftrim

     INTEGER , EXTERNAL :: file_md5
     INTEGER, ALLOCATABLE :: istr(:)
     INTEGER :: getter(32), retval, i, ilen

     ftrim = TRIM(filename)     
     ilen = LEN(ftrim )
     ALLOCATE( istr( ilen ) )
     DO i = 1, ilen
        istr(i) = ichar( ftrim(i:i) )
     ENDDO
     
     retval = file_md5( istr, ilen, getter )

     DO i = 1,32
        md5(i:i) = char(  getter(i) )
     ENDDO

     DEALLOCATE( istr )

  END SUBROUTINE 
  !
END MODULE
#endif
