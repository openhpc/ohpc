!
! Copyright (C) 2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
MODULE read_input
   !---------------------------------------------------------------------------
   !
   USE kinds,     ONLY: DP
   !
   IMPLICIT NONE
   SAVE
   !
   PRIVATE
   PUBLIC :: read_input_file, has_been_read
   !
   LOGICAL :: has_been_read = .FALSE.
   !
   CONTAINS
   !
   !-------------------------------------------------------------------------
   SUBROUTINE read_input_file ( prog )
     !-------------------------------------------------------------------------
     !
     !use MQEoptions,            only : MQEo
     USE read_namelists_module, ONLY : read_namelists
     USE read_cards_module,     ONLY : read_cards
     USE io_global,             ONLY : stdout, ionode, ionode_id
     USE mp,                    ONLY : mp_bcast
     USE mp_global,             ONLY : intra_image_comm
     !
     IMPLICIT NONE
     !
     !borrowed from QE iotk_base.f90
     integer, parameter :: iotk_taglenx =  65535 ! (2**16-1)
     integer, parameter :: iotk_namlenx =  256
     integer, parameter :: iotk_attlenx =  iotk_taglenx - iotk_namlenx - 1 ! for space
     !
     CHARACTER(LEN=2), INTENT (IN) :: prog
     CHARACTER(LEN=iotk_attlenx) :: attr
     LOGICAL :: xmlinput
     INTEGER :: ierr
     !
     !
     if( ionode ) then
        call input_from_file( ierr )
        !WRITE(stdout, '(5x,a)') "Reading input from "//TRIM( MQEo%infile)
        !OPEN ( UNIT = 5, FILE = TRIM( MQEo%infile), FORM = 'FORMATTED', &
        !       STATUS = 'OLD', IOSTAT = ierr )
        !if( ierr > 0 ) ierr = 2
        xmlinput = .false.
     end if
     !
     CALL mp_bcast( ierr, ionode_id, intra_image_comm )
     IF ( ierr > 0 ) CALL errore('read_input', 'opening input file',ierr)
     CALL mp_bcast( xmlinput, ionode_id, intra_image_comm )
     CALL mp_bcast( attr, ionode_id, intra_image_comm )
     !
        !
        ! ... Read NAMELISTS 
        !
        CALL read_namelists( prog )
        !
        ! ... Read CARDS 
        !
        CALL read_cards ( prog )
        !
     if( ionode ) CLOSE( UNIT=5, STATUS='keep', IOSTAT=ierr )
     !
     has_been_read = .TRUE.
     !
     RETURN
     !
   END SUBROUTINE read_input_file
  !
END MODULE read_input
