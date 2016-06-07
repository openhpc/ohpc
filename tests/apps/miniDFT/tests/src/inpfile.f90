!
! Copyright (C) 2002-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
#if defined(__ABSOFT)
#  define getenv getenv_
#  define getarg getarg_
#  define iargc  iargc_
#endif
!
!----------------------------------------------------------------------------
SUBROUTINE input_from_file( ierr )
  !
  ! This subroutine checks program arguments and, if input file is present,
  ! attach input unit ( 5 ) to the specified file
  !
  !
  IMPLICIT NONE
  !
  INTEGER             :: stdin = 5, stderr = 6, &
                         ilen, iiarg, nargs, ierr
  ! do not define iargc as external: g95 does not like it
  INTEGER             :: iargc
  CHARACTER (LEN=256)  :: input_file
  !
  ! ... Input from file ?
  !
  nargs = iargc()
  !
  ierr = -1
  !
  DO iiarg = 1, ( nargs - 1 )
     !
     CALL getarg( iiarg, input_file )
     !
     IF ( TRIM( input_file ) == '-input' .OR. &
          TRIM( input_file ) == '-inp'   .OR. &
          TRIM( input_file ) == '-in' ) THEN
        !
        CALL getarg( ( iiarg + 1 ) , input_file )
        !
        WRITE(*, '(5x,a)') "Reading input from "//TRIM( input_file )

        OPEN ( UNIT = stdin, FILE = input_file, FORM = 'FORMATTED', &
               STATUS = 'OLD', IOSTAT = ierr )
        !
        ! TODO: return error code ierr (-1 no file, 0 file opened, > 1 error)
        ! do not call "errore" here: it may hang in parallel execution
        ! if this routine ois called by ionode only
        !
        IF ( ierr > 0 ) WRITE (stderr, &
                '(" *** input file ",A," not found ***")' ) TRIM( input_file )
        !
     END IF
     !
  END DO

END SUBROUTINE input_from_file
!
!----------------------------------------------------------------------------
!
SUBROUTINE get_file( input_file )
  !
  ! This subroutine reads, either from command line or from terminal,
  ! the name of a file to be opened
  ! TODO: return error code if an error occurs
  !
  IMPLICIT NONE
  !
  CHARACTER (LEN=*)  :: input_file
  !
  CHARACTER (LEN=256)  :: prgname
  ! do not define iargc as external: g95 does not like it
  INTEGER             :: nargs, iargc
  LOGICAL             :: exst
  !
  nargs = iargc()
  CALL getarg (0,prgname)
  !
  IF ( nargs == 0 ) THEN
10   PRINT  '("Input file > ",$)'
     READ (5,'(a)', end = 20, err=20) input_file
     IF ( input_file == ' ') GO TO 10
     INQUIRE ( FILE = input_file, EXIST = exst )
     IF ( .NOT. exst) THEN
        PRINT  '(A,": file not found")', TRIM(input_file)
        GO TO 10
     END IF
  ELSE IF ( nargs == 1 ) then
     CALL getarg (1,input_file)
  ELSE
     PRINT  '(A,": too many arguments ",i4)', TRIM(prgname), nargs
  END IF
  RETURN
20 PRINT  '(A,": reading file name ",A)', TRIM(prgname), TRIM(input_file)
  !
END SUBROUTINE get_file
!
!----------------------------------------------------------------------------
!
SUBROUTINE get_arg_npool( npool )
   !
   IMPLICIT NONE
   !
   INTEGER :: npool
   !
   INTEGER :: nargs, iiarg
   CHARACTER(LEN=10) :: np
   INTEGER :: iargc
   !
   npool = 1
   nargs = iargc()
   !
   DO iiarg = 1, ( nargs - 1 )
      !
      CALL getarg( iiarg, np )
      !
      IF ( TRIM( np ) == '-npool' .OR. TRIM( np ) == '-npools' ) THEN
         !
         CALL getarg( ( iiarg + 1 ), np )
         READ( np, * ) npool
         !
      END IF
      !
   END DO
   !
   RETURN
END SUBROUTINE get_arg_npool
!
!----------------------------------------------------------------------------
!
SUBROUTINE get_arg_nimage( nimage )
   !
   IMPLICIT NONE
   !
   INTEGER :: nimage
   !
   INTEGER :: nargs, iiarg
   CHARACTER(LEN=10) :: np
   INTEGER :: iargc
   !
   nimage = 1
   nargs = iargc()
   !
   DO iiarg = 1, ( nargs - 1 )
      !
      CALL getarg( iiarg, np )
      !
      IF ( TRIM( np ) == '-nimage' .OR. TRIM( np ) == '-nimages' ) THEN
         !
         CALL getarg( ( iiarg + 1 ), np )
         READ( np, * ) nimage
         !
      END IF
      !
   END DO
   !
   RETURN
END SUBROUTINE get_arg_nimage
!
!----------------------------------------------------------------------------
!
SUBROUTINE get_arg_ntg( ntask_groups )
   !
   IMPLICIT NONE
   !
   INTEGER :: ntask_groups
   !
   INTEGER :: nargs, iiarg
   CHARACTER(LEN=20) :: np
   INTEGER :: iargc
   !
   ntask_groups = 0
   nargs = iargc()
   !
   DO iiarg = 1, ( nargs - 1 )
      !
      CALL getarg( iiarg, np )
      !
      IF ( TRIM( np ) == '-ntg' .OR. TRIM( np ) == '-ntask_groups' ) THEN
         !
         CALL getarg( ( iiarg + 1 ), np )
         READ( np, * ) ntask_groups
         !
      END IF
      !
   END DO
   !
   RETURN
END SUBROUTINE get_arg_ntg
!
!----------------------------------------------------------------------------
!
SUBROUTINE get_arg_nbgrp( nbgrp )
   !
   IMPLICIT NONE
   !
   INTEGER :: nbgrp
   !
   INTEGER :: nargs, iiarg
   CHARACTER(LEN=20) :: np
   INTEGER :: iargc
   !
   nbgrp = 0
   nargs = iargc()
   !
   DO iiarg = 1, ( nargs - 1 )
      !
      CALL getarg( iiarg, np )
      !
      IF ( TRIM( np ) == '-nbgrp' .OR. TRIM( np ) == '-nband_group' .OR. &
           TRIM( np ) == '-nb' .OR. TRIM( np ) == '-nband' ) THEN
         !
         CALL getarg( ( iiarg + 1 ), np )
         READ( np, * ) nbgrp
         !
      END IF
      !
   END DO
   !
   RETURN
END SUBROUTINE get_arg_nbgrp
!
!----------------------------------------------------------------------------
!
SUBROUTINE get_arg_northo( nproc_ortho )
   !
   IMPLICIT NONE
   !
   INTEGER :: nproc_ortho
   !
   INTEGER :: nargs, iiarg
   CHARACTER(LEN=20) :: np
   INTEGER :: iargc
   !
   nproc_ortho = 0
   nargs = iargc()
   !
   DO iiarg = 1, ( nargs - 1 )
      !
      CALL getarg( iiarg, np )
      !
      IF ( TRIM( np ) == '-nproc_ortho' .OR. TRIM( np ) == '-nproc_diag' .OR. &
           TRIM( np ) == '-northo' .OR. TRIM( np ) == '-ndiag' ) THEN
         !
         CALL getarg( ( iiarg + 1 ), np )
         READ( np, * ) nproc_ortho
         !
      END IF
      !
   END DO
   !
   RETURN
END SUBROUTINE get_arg_northo

SUBROUTINE get_env ( variable_name, variable_value )
  !
  ! Wrapper for intrinsic getenv - all machine-dependent stuff here
  !
  CHARACTER (LEN=*)  :: variable_name, variable_value
  !
  CALL getenv ( variable_name, variable_value)
  !
END SUBROUTINE get_env
