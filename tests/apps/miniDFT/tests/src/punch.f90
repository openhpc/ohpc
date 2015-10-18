!
! Copyright (C) 2001-2006 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
SUBROUTINE punch( what )
  !----------------------------------------------------------------------------
  !
  ! ... This routine is called at the end of the run to save to a file
  ! ... the information needed for further processing (phonon etc.)
  !
  USE io_global,            ONLY : stdout
  USE io_files,             ONLY : prefix, iunpun
  USE control_flags,        ONLY : io_level
  USE a2F,                  ONLY : la2F, a2Fsave
  !
  IMPLICIT NONE
  !
  CHARACTER(LEN=*) :: what
  !
  !
  IF (io_level < 0 ) RETURN
  !
  WRITE( UNIT = stdout, FMT = '(/,5X,"Writing output data file ",A)' ) &
      TRIM( prefix ) // '.save'
  !
  iunpun = 4
  !
  write(*,*) "punch.f90:33 skipping pw_writefile, requires iotk"
  !CALL pw_writefile( TRIM( what ) )
  !
  IF ( la2F ) CALL a2Fsave()
  !
  RETURN
  !
END SUBROUTINE punch
