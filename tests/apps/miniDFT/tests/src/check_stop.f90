!
! Copyright (C) 2002-2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
! ... This module contains functions to check if the code should
! ... be smoothly stopped.
! ... In particular the function check_stop_now returns .TRUE. if
! ... either the user has created a given file or if the
! ... elapsed time is larger than max_seconds
!
!------------------------------------------------------------------------------!
MODULE check_stop
!------------------------------------------------------------------------------!
  !
  USE kinds
  !
  IMPLICIT NONE
  !
  SAVE
  !
  REAL(DP) :: max_seconds = 1.E+7_DP
  !
  LOGICAL, PRIVATE :: tinit = .FALSE.
  !
  REAL(DP) :: init_second
  !
  CONTAINS
     !
     ! ... internal procedures
     !
     !-----------------------------------------------------------------------
     SUBROUTINE check_stop_init()
       !-----------------------------------------------------------------------
       !
       USE input_parameters, ONLY : max_seconds_ => max_seconds
       USE io_global,        ONLY : stdout
       USE io_files,         ONLY : prefix, exit_file
#if defined __TRAP_SIGUSR1
       USE set_signal,       ONLY : signal_trap_init
#endif

       !
       IMPLICIT NONE
       !
       REAL(DP), EXTERNAL :: cclock
       !
       IF ( tinit ) &
          WRITE( UNIT = stdout, &
                 FMT = '(/,5X,"WARNING: check_stop already initialized")' )
       !
       ! ... the exit_file name is set here
       !
       exit_file = TRIM( prefix ) // '.EXIT'
       !
       IF ( max_seconds_ > 0.0_DP ) max_seconds = max_seconds_
       !
       init_second = cclock()
       tinit   = .TRUE.
       !
#if defined __TRAP_SIGUSR1
       CALL signal_trap_init ( )
#endif
       !
       RETURN
       !
     END SUBROUTINE check_stop_init
     !
     !-----------------------------------------------------------------------
     FUNCTION check_stop_now( inunit )
       !-----------------------------------------------------------------------
       !
       USE mp,         ONLY : mp_bcast
       USE mp_global,  ONLY : intra_image_comm
       USE io_global,  ONLY : ionode, ionode_id, meta_ionode, stdout
       USE io_files,   ONLY : exit_file, iunexit
#if defined __TRAP_SIGUSR1
       USE set_signal, ONLY : signal_detected
#endif
       ! KNK_image
       ! USE mp_global, ONLY : mpime, root, world_comm
       !
       IMPLICIT NONE
       !
       INTEGER, OPTIONAL, INTENT(IN) :: inunit
       !
       INTEGER            :: unit
       LOGICAL            :: check_stop_now, tex
       LOGICAL            :: signaled
       REAL(DP)           :: seconds
       REAL(DP), EXTERNAL :: cclock
       !
       !
       ! ... cclock is a C function returning the elapsed solar
       ! ... time in seconds since the Epoch ( 00:00:00 1/1/1970 )
       !
       IF ( .NOT. tinit ) &
          CALL errore( 'check_stop_now', 'check_stop not initialized', 1 )
       !
       unit = stdout
       IF ( PRESENT( inunit ) ) unit = inunit
       !
       check_stop_now = .FALSE.
       !
       signaled = .FALSE.
       !
       ! KNK_image
       ! IF ( mpime == root ) THEN
       IF ( ionode ) THEN
          !
          INQUIRE( FILE = TRIM( exit_file ), EXIST = tex )
          !
          IF ( tex ) THEN
             !
             check_stop_now = .TRUE.
             !
             OPEN( UNIT = iunexit, FILE = TRIM( exit_file ) )
             CLOSE( UNIT = iunexit, STATUS = 'DELETE' )
             !
          ELSE
             !
             seconds = cclock() - init_second
             !
             check_stop_now = ( seconds  >  max_seconds )
             !
          END IF
          !
       END IF
       !
#if defined __TRAP_SIGUSR1
       signaled = signal_detected()
       check_stop_now = check_stop_now .OR. signaled
       tex = tex .OR. signaled
#endif
       !
       ! KNK_image
       ! CALL mp_bcast( check_stop_now, root, world_comm )
       CALL mp_bcast( check_stop_now, ionode_id, intra_image_comm )
       !
       IF ( check_stop_now .AND. meta_ionode ) THEN
          !
          IF ( tex ) THEN
             !
             WRITE( UNIT = unit, &
                    FMT = '(/,5X,"Program stopped by user request")' )
             !
          ELSE
             !
             WRITE( UNIT = unit, &
                    FMT = '(/,5X,"Maximum CPU time exceeded")' )
             WRITE( UNIT = unit, &
                    FMT = '(/,5X,"max_seconds     = ",F10.2)' ) max_seconds
             WRITE( UNIT = unit, &
                    FMT = '(5X,"elapsed seconds = ",F10.2)' ) seconds
             !
          END IF
          !
       END IF
       !
       RETURN
       !
     END FUNCTION check_stop_now
     !
END MODULE check_stop
