!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! ... Time-printing utilities - Contains the following subroutines:
!     init_clocks( go )    initialization - must be called first
!                          go = .TRUE. : up to "maxclock" clocks can be started
!                          go = .FALSE.: only clock #1 can be started
!     start_clock( label )   starts clock "label" (max 14 characters)
!                            if "label" has never been started, initializes it
!                            issues warning if "label" already started
!     stop_clock( label )    stops  clock "label"
!                            issues warning if "label" is either not running
!                            or has never been started
!     print_clock( label )   print cpu and wall time measured by clock "label"
!                            clock "label" may be running or stopped 
!                            and remains in the same state
!                            issues warning if "label" has never been started
! ... and the following function (real(kind=dp):
!     get_clock( label )     return wall time measured by clock "label"
!                            returns -1 if "label" has never been started
! ... All output and warnings are written to stdout
! ... Clocks should be started, read, stopped either on all processors, or 
! ... only on one, but not half and half! For parallel debugging, uncomment:
!#define __TRACE
! ... See also comments in subroutine print_this_clock about parallel case
!
!----------------------------------------------------------------------------
MODULE mytime
  !----------------------------------------------------------------------------
  !
  USE kinds, ONLY : DP
  !
  IMPLICIT NONE
  !
  SAVE
  !
  INTEGER,  PARAMETER :: maxclock = 100
  REAL(DP), PARAMETER :: notrunning = - 1.0_DP
  !
  REAL(DP)          :: cputime(maxclock), t0cpu(maxclock)
  REAL(DP)          :: walltime(maxclock), t0wall(maxclock)
  CHARACTER(len=14) :: clock_label(maxclock)
  INTEGER           :: called(maxclock)
  !
  INTEGER :: nclock = 0
  LOGICAL :: no
  INTEGER :: trace_depth = 0
  !
END MODULE mytime
!
!----------------------------------------------------------------------------
SUBROUTINE init_clocks( go )
  !----------------------------------------------------------------------------
  !
  ! ... go = .TRUE.  : clocks will run
  ! ... go = .FALSE. : only clock #1 will run
  !
  USE kinds,  ONLY : DP
  USE mytime, ONLY : called, t0cpu, cputime, no, notrunning, maxclock, &
       clock_label, walltime, t0wall
  !
  IMPLICIT NONE
  !
  LOGICAL :: go
  INTEGER :: n
  !
  !
  no = .not. go
  !
  DO n = 1, maxclock
     !
     called(n)      = 0
     cputime(n)     = 0.0_DP
     t0cpu(n)       = notrunning
     walltime(n)        = 0.0_DP
     t0wall(n)          = notrunning
     clock_label(n) = ' '
     !
  ENDDO
  !
  RETURN
  !
END SUBROUTINE init_clocks
!
!----------------------------------------------------------------------------
SUBROUTINE start_clock( label )
  !----------------------------------------------------------------------------
  !
  USE kinds,     ONLY : DP
  USE io_global, ONLY : stdout
#if defined (__TRACE)
  USE mp_global, ONLY : mpime
#endif
  USE mytime,    ONLY : nclock, clock_label, notrunning, no, maxclock, &
                        t0cpu, t0wall, trace_depth
  !
  IMPLICIT NONE
  !
  CHARACTER(len=*) :: label
  !
  CHARACTER(len=14) :: label_
  INTEGER          :: n
  REAL(DP), EXTERNAL :: scnds, cclock
  !
#if defined (__TRACE)
  WRITE( stdout, '("mpime = ",I2,", TRACE (depth=",I2,") Start: ",A14)') mpime, trace_depth, label
  trace_depth = trace_depth + 1
#endif
  !
  IF ( no .and. ( nclock == 1 ) ) RETURN
  !
  ! ... prevent trouble if label is longer than 14 characters
  !
  label_ = trim ( label )
  !
  DO n = 1, nclock
     !
     IF ( clock_label(n) == label_ ) THEN
        !
        ! ... found previously defined clock: check if not already started,
        ! ... store in t0cpu the starting time
        !
        IF ( t0cpu(n) /= notrunning ) THEN
!            WRITE( stdout, '("start_clock: clock # ",I2," for ",A14, &
!                           & " already started")' ) n, label_
        ELSE
           t0cpu(n) = scnds()
                   t0wall(n) = cclock()
        ENDIF
        !
        RETURN
        !
     ENDIF
     !
  ENDDO
  !
  ! ... clock not found : add new clock for given label
  !
  IF ( nclock == maxclock ) THEN
     !
     WRITE( stdout, '("start_clock: Too many clocks! call ignored")' )
     !
  ELSE
     !
     nclock                                     = nclock + 1
     clock_label(nclock)        = label_
     t0cpu(nclock)                      = scnds()
     t0wall(nclock)                     = cclock()
     !
  ENDIF
  !
  RETURN
  !
END SUBROUTINE start_clock
!
!----------------------------------------------------------------------------
SUBROUTINE stop_clock( label )
  !----------------------------------------------------------------------------
  !
  USE kinds,     ONLY : DP
  USE io_global, ONLY : stdout
#if defined (__TRACE)
  USE mp_global, ONLY : mpime
#endif
  USE mytime,    ONLY : no, nclock, clock_label, cputime, walltime, &
                        notrunning, called, t0cpu, t0wall, trace_depth
  !
  IMPLICIT NONE
  !
  CHARACTER(len=*) :: label
  !
  CHARACTER(len=14) :: label_
  INTEGER          :: n
  REAL(DP), EXTERNAL :: scnds, cclock
  !
#if defined (__TRACE)
  trace_depth = trace_depth - 1
  WRITE( *, '("mpime = ",I2,", TRACE (depth=",I2,") End: ",A14)') mpime, trace_depth, label
#endif
  !
  IF ( no ) RETURN
  !
  ! ... prevent trouble if label is longer than 14 characters
  !
  label_ = trim ( label )
  !
  DO n = 1, nclock
     !
     IF ( clock_label(n) == label_ ) THEN
        !
        ! ... found previously defined clock : check if properly initialised,
        ! ... add elapsed time, increase the counter of calls
        !
        IF ( t0cpu(n) == notrunning ) THEN
           !
           WRITE( stdout, '("stop_clock: clock # ",I2," for ",A14, &
                          & " not running")' ) n, label
           !
        ELSE
           !
           cputime(n)   = cputime(n) + scnds() - t0cpu(n)
           walltime(n)  = walltime(n) + cclock() - t0wall(n)
           t0cpu(n)             = notrunning
           t0wall(n)    = notrunning
           called(n)    = called(n) + 1
           !
        ENDIF
        !
        RETURN
        !
     ENDIF
     !
  ENDDO
  !
  ! ... clock not found
  !
  WRITE( stdout, '("stop_clock: no clock for ",A14," found !")' ) label
  !
  RETURN
  !
END SUBROUTINE stop_clock
!
!----------------------------------------------------------------------------
SUBROUTINE print_clock( label )
  !----------------------------------------------------------------------------
  !
  USE kinds,     ONLY : DP
  USE io_global, ONLY : stdout
  USE mytime,    ONLY : nclock, clock_label
  !
  IMPLICIT NONE
  !
  CHARACTER(len=*) :: label
  !
  CHARACTER(len=14) :: label_
  INTEGER          :: n
  !
  IF ( label == ' ' ) THEN
     !
     WRITE( stdout, * )
     !
     DO n = 1, nclock
        !
        CALL print_this_clock( n )
        !
     ENDDO
     !
  ELSE
     !
     ! ... prevent trouble if label is longer than 14 characters
     !
     label_ = trim ( label )
     !
     DO n = 1, nclock
        !
        IF ( clock_label(n) == label_ ) THEN
           !
           CALL print_this_clock( n )
           !
           exit
           !
        ENDIF
        !
     ENDDO
     !
  ENDIF
  !
  RETURN
  !
END SUBROUTINE print_clock
!
!----------------------------------------------------------------------------
SUBROUTINE print_this_clock( n )
  !----------------------------------------------------------------------------
  !
  USE kinds,     ONLY : DP
  USE io_global, ONLY : stdout
  USE mytime,    ONLY : no, nclock, clock_label, cputime, walltime, &
                        notrunning, called, t0cpu, t0wall
!
! ... See comments below about parallel case
!
!  USE mp,        ONLY : mp_max
!  USE mp_global, ONLY : intra_image_comm, my_image_id
  !
  IMPLICIT NONE
  !
  INTEGER  :: n
  REAL(DP) :: elapsed_cpu_time, elapsed_wall_time, nsec, msec
  INTEGER  :: nday, nhour, nmin, nmax, mday, mhour, mmin
  !
  REAL(DP), EXTERNAL :: scnds, cclock
  !
  !
  IF ( t0cpu(n) == notrunning ) THEN
     !
     ! ... clock stopped, print the stored value for the cpu time
     !
     elapsed_cpu_time = cputime(n)
     elapsed_wall_time= walltime(n)
     !
  ELSE
     !
     ! ... clock not stopped, print the current value of the cpu time
     !
     elapsed_cpu_time   = cputime(n) + scnds() - t0cpu(n)
     elapsed_wall_time  = walltime(n) + cclock() - t0wall(n)
     called(n)  = called(n) + 1
     !
  ENDIF
  !
  nmax = called(n)
  !
  ! ... In the parallel case there are several possible approaches
  ! ... The safest one is to leave each clock independent from the others
  ! ... Another possibility is to print the maximum across all processors
  ! ... This is done by uncommenting the following lines
  !
  ! CALL mp_max( elapsed_cpu_time, intra_image_comm )
  ! CALL mp_max( elapsed_wall_time, intra_image_comm )
  ! CALL mp_max( nmax, intra_image_comm )
  !
  ! ... In the last line we assume that the maximum cpu time
  ! ... is associated to the maximum number of calls
  ! ... NOTA BENE: by uncommenting the above lines you may run into
  ! ... serious trouble if clocks are not started on all nodes
  !
  IF ( n == 1 ) THEN
     !
     ! ... The first clock is written as days/hour/min/sec
     !
     nday  = elapsed_cpu_time / 86400
     nsec  = elapsed_cpu_time - 86400 * nday
     nhour = nsec / 3600
     nsec  = nsec - 3600 * nhour
     nmin  = nsec / 60
     nsec  = nsec - 60 * nmin
     !
     ! ... The first clock writes elapsed (wall) time as well
     !
     mday  = elapsed_wall_time / 86400
     msec  = elapsed_wall_time - 86400 * mday
     mhour = msec / 3600
     msec  = msec - 3600 * mhour
     mmin  = msec / 60
     msec  = msec - 60 * mmin
     !
     IF ( nday > 0 .or. mday > 0 ) THEN
        !
        WRITE( stdout, &
               '(5X,A14," : ",3X,I2,"d",3X,I2,"h",I2, "m CPU ", &
           &            "   ",3X,I2,"d",3X,I2,"h",I2, "m WALL"/)' ) &
             clock_label(n), nday, nhour, nmin, mday, mhour, mmin
        !
     ELSEIF ( nhour > 0 .or. mhour > 0 ) THEN
        !
        WRITE( stdout, &
               '(5X,A14," : ",3X,I2,"h",I2,"m CPU ", &
           &            "   ",3X,I2,"h",I2,"m WALL"/)' ) &
             clock_label(n), nhour, nmin, mhour, mmin
        !
     ELSEIF ( nmin > 0 .or. mmin > 0 ) THEN
        !
        WRITE( stdout, &
               '(5X,A14," : ",I2,"m",F5.2,"s CPU ", &
               &        "   ",I2,"m",F5.2,"s WALL"/)' ) &
             clock_label(n), nmin, nsec, mmin, msec
        !
     ELSE
        !
        WRITE( stdout, &
               '(5X,A14," : ",3X,F5.2,"s CPU ",7X,F5.2,"s WALL"/)' )&
             clock_label(n), nsec, msec
        !
     ENDIF
     !
  ELSEIF ( nmax == 1 .or. t0cpu(n) /= notrunning ) THEN
     !
     ! ... for clocks that have been called only once
     !
     WRITE( stdout, &
            '(5X,A14," : ",F9.2,"s CPU ",F9.2,"s WALL (",I8," calls)")' ) &
                clock_label(n), elapsed_cpu_time, elapsed_wall_time, nmax
     !
  ELSEIF ( nmax == 0 ) THEN
     !
     ! ... for clocks that have never been called
     !
     WRITE( stdout, &
            '("print_this: clock # ",I2," for ",A14," never called !"/)' ) &
                n, clock_label(n)
     !
  ELSE
     !
     ! ... for all other clocks
     !
     WRITE( stdout, &
        '(5X,A14," : ",F9.2,"s CPU ",F9.2,"s WALL (",I8," calls)")' ) &
        clock_label(n), elapsed_cpu_time, elapsed_wall_time, nmax
     !
  ENDIF
  !
  RETURN
  !
END SUBROUTINE print_this_clock
!
!----------------------------------------------------------------------------
FUNCTION get_clock( label )
  !----------------------------------------------------------------------------
  !
  USE kinds,     ONLY : DP
  USE io_global, ONLY : stdout
  USE mytime,    ONLY : no, nclock, clock_label, walltime, &
                        notrunning, called, t0wall, t0cpu
!
! ... See comments in subroutine print_this_clock about parallel case
!
!  USE mp,        ONLY : mp_max
!  USE mp_global, ONLY : intra_image_comm
  !
  IMPLICIT NONE
  !
  REAL(DP)         :: get_clock
  CHARACTER(len=*) :: label
  INTEGER          :: n
  !
  REAL(DP), EXTERNAL :: cclock
  !
  !
  IF ( no ) THEN
     !
     IF ( label == clock_label(1) ) THEN
        !
        get_clock = cclock()
        !
     ELSE
        !
        get_clock = notrunning
        !
     ENDIF
     !
     RETURN
     !
  ENDIF
  !
  DO n = 1, nclock
     !
     IF ( label == clock_label(n) ) THEN
        !
        IF ( t0cpu(n) == notrunning ) THEN
           !
           get_clock = walltime(n)
           !
        ELSE
           !
           get_clock = walltime(n) + cclock() - t0wall(n)
           !
        ENDIF
        !
        ! ... See comments in subroutine print_this_clock about parallel case
        !
        ! CALL mp_max( get_clock, intra_image_comm )
        !
        RETURN
        !
     ENDIF
     !
  ENDDO
  !
  ! ... clock not found
  !
  get_clock = notrunning
  !
  RETURN
  !
END FUNCTION get_clock
