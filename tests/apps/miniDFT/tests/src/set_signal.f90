MODULE set_signal
! This module is a Fortran 2003 interface to the customize_signals.c C file
! Compatible with Intel/PGI/Gcc(>=4.3) compilers

! This module is compiled only if the following preprocessing option
! is enabled
#if defined __TRAP_SIGUSR1

USE iso_c_binding
USE io_global, ONLY : stdout
USE mp_global, ONLY : root, world_comm, mp_bcast, mpime

IMPLICIT NONE

LOGICAL,VOLATILE::signal_trapped

INTERFACE 
   FUNCTION init_signal_USR1(new_handler) BIND(c, name = "init_signal_USR1")
     USE iso_c_binding
     TYPE(C_FUNPTR),VALUE,INTENT(IN):: new_handler
     INTEGER(C_INT)::init_signal_USR1
   END FUNCTION init_signal_USR1

   FUNCTION init_signal(signum, new_handler) BIND(c, name = "init_signal")
     USE iso_c_binding
     INTEGER(C_INT),VALUE :: signum
     TYPE(C_FUNPTR), VALUE,INTENT(IN) :: new_handler
     INTEGER(C_INT)::init_signal
   END FUNCTION init_signal

END INTERFACE

CONTAINS

SUBROUTINE set_signal_USR1(routine)
  USE iso_c_binding
  TYPE(C_FUNPTR),TARGET::ptr
  INTERFACE 
     SUBROUTINE routine(signal) bind(C)
       USE iso_c_binding
       INTEGER(C_INT),VALUE, INTENT(IN)::signal
     END SUBROUTINE routine

  END INTERFACE
       
  ptr = C_FUNLOC(routine)
  
  IF (init_signal_USR1(ptr) .NE. 0) THEN
     CALL errore("set_signal_USR1", "The association of signal USR1 failed!", 1)
  ENDIF
 
END SUBROUTINE set_signal_USR1

! Unused. Here for possible future developments
SUBROUTINE set_signal_action(signal, routine)
  USE iso_c_binding
  INTEGER::signal
  TYPE(C_FUNPTR),TARGET::ptr
  INTERFACE 
     SUBROUTINE routine(signal) bind(C)
       USE iso_c_binding
       INTEGER(C_INT),VALUE::signal
     END SUBROUTINE routine
  END INTERFACE

  ptr = C_FUNLOC(routine)
       
  IF (init_signal(signal, ptr) .NE. 0) THEN
     CALL errore("set_signal", "The association of the signal failed!", 1)
  ENDIF
END SUBROUTINE set_signal_action


! Sets the signal_trapped flag on all nodes/processors
! Only the master will use the signal, though
SUBROUTINE custom_handler(signum) BIND(c)
  USE iso_c_binding
  INTEGER(C_INT),VALUE,INTENT(IN):: signum
  WRITE(UNIT = stdout, FMT = *) "    **** Trapped signal", signum
  signal_trapped = .TRUE.
END SUBROUTINE custom_handler


! Set the signal handler for SIGUSR1 to 'custom_handler' 
! Every processor will trap the signal, howver only 0 will actually
! use the result (required since the default action for SIGUSR1 is
! exit)
SUBROUTINE signal_trap_init
  USE iso_c_binding
  WRITE(UNIT = stdout, FMT=*) "    signal trapping enabled: kill the code with -SIGUSR1 to stop cleanly the simulation "
  CALL set_signal_USR1(custom_handler)
END SUBROUTINE signal_trap_init


FUNCTION signal_detected()
  LOGICAL::signal_detected
  ! If the signal is trapped, set the exit status and broadcast it
  ! DO NOT broadcast the signal_trapped variable or you will be Very
  ! Sorry
  signal_detected = signal_trapped
     
  CALL mp_bcast(signal_detected, root, world_comm)

END FUNCTION signal_detected

#else

USE io_global, ONLY : stdout

CONTAINS

! Place holders to employ when the signal trapping feature is disabled
SUBROUTINE signal_trap_init
  WRITE(UNIT = stdout, FMT=*) "    signal trapping disabled: compile with "
  WRITE(UNIT = stdout, FMT=*) "    -D__TRAP_SIGUSR1 to enable this feature"
END SUBROUTINE signal_trap_init

FUNCTION signal_detected()
  LOGICAL::signal_detected
  signal_detected = .FALSE.
END FUNCTION signal_detected
  
#endif

END MODULE set_signal
