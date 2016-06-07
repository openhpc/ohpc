!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
SUBROUTINE errore( calling_routine, message, ierr )
  !----------------------------------------------------------------------------
  !
  ! ... This is a simple routine which writes an error message to output: 
  ! ... if ierr <= 0 it does nothing, 
  ! ... if ierr  > 0 it stops.
  !
  ! ...          **** Important note for parallel execution ***
  !
  ! ... in parallel execution unit 6 is written only by the first node;
  ! ... all other nodes have unit 6 redirected to nothing (/dev/null).
  ! ... As a consequence an error not occurring on the first node
  ! ... will be invisible. For T3E and ORIGIN machines, this problem
  ! ... is solved by writing an error message to unit * instead of 6.
  ! ... Whenever possible (IBM SP machines), we write to the standard
  ! ... error, unit 0 (the message will appear in the error files 
  ! ... produced by loadleveler).
  !
  USE io_global, ONLY : stdout
  USE io_files,  ONLY : crashunit, crash_file
  USE parallel_include
  !
  IMPLICIT NONE
  !
  CHARACTER(LEN=*), INTENT(IN) :: calling_routine, message
    ! the name of the calling calling_routinee
    ! the output messagee
  INTEGER,          INTENT(IN) :: ierr
    ! the error flag
  INTEGER                      :: mpime, mpierr
    ! the task id  
  !
  !
  IF ( ierr <= 0 ) RETURN
  !
  ! ... the error message is written un the "*" unit
  !
  WRITE( UNIT = *, FMT = '(/,1X,78("%"))' )
  WRITE( UNIT = *, &
         FMT = '(5X,"from ",A," : error #",I10)' ) calling_routine, ierr
  WRITE( UNIT = *, FMT = '(5X,A)' ) message
  WRITE( UNIT = *, FMT = '(1X,78("%"),/)' )
  !
  !
  WRITE( *, '("     stopping ...")' )
  !
  CALL flush_unit( stdout )
  !
  !
  mpime = 0
  !
  CALL MPI_COMM_RANK( MPI_COMM_WORLD, mpime, mpierr )
  !
  !  .. write the message to a file and close it before exiting
  !  .. this will prevent loss of information on systems that
  !  .. do not flush the open streams
  !  .. added by C.C.
  !
  OPEN( UNIT = crashunit, FILE = crash_file, &
        POSITION = 'APPEND', STATUS = 'UNKNOWN' )
  !      
  WRITE( UNIT = crashunit, FMT = '(/,1X,78("%"))' )
  WRITE( UNIT = crashunit, FMT = '(5X,"task #",I10)' ) mpime
  WRITE( UNIT = crashunit, &
         FMT = '(5X,"from ",A," : error #",I10)' ) calling_routine, ierr
  WRITE( UNIT = crashunit, FMT = '(5X,A)' ) message
  WRITE( UNIT = crashunit, FMT = '(1X,78("%"),/)' )
  !
  CLOSE( UNIT = crashunit )
  !
  ! ... try to exit in a smooth way
  !
  CALL MPI_ABORT( MPI_COMM_WORLD, mpierr )
  !
  CALL MPI_FINALIZE( mpierr )
  !
  !
  STOP 2
  !
  RETURN
  !
END SUBROUTINE errore
!
!----------------------------------------------------------------------
SUBROUTINE infomsg( routine, message )
  !----------------------------------------------------------------------
  !
  ! ... This is a simple routine which writes an info message 
  ! ... from a given routine to output. 
  !
  USE io_global,  ONLY : stdout, ionode
  !
  IMPLICIT NONE
  !
  CHARACTER (LEN=*) :: routine, message
  ! the name of the calling routine
  ! the output message
  !
  IF ( ionode ) THEN
     !
     WRITE( stdout , '(5X,"Message from routine ",A,":")' ) routine
     WRITE( stdout , '(5X,A)' ) message
     !
  END IF
  !
  RETURN
  !
END SUBROUTINE infomsg
!
module error_handler
  implicit none
  private

  public :: init_error, add_name, chop_name, error_mem, warning

  type chain
   character (len=35)   :: routine_name
   type(chain), pointer :: previous_link
  end type chain

  type(chain), pointer :: routine_chain

contains

  subroutine init_error(routine_name)
    implicit none
    character (len=*), intent(in) :: routine_name

    allocate(routine_chain)

    routine_chain%routine_name  =  routine_name
    nullify(routine_chain%previous_link)

    return
  end subroutine init_error

  subroutine add_name(routine_name)
    implicit none
    character (len=*), intent(in) :: routine_name
    type(chain), pointer          :: new_link

    allocate(new_link)
    new_link%routine_name  =  routine_name
    new_link%previous_link => routine_chain
    routine_chain          => new_link

    return
  end subroutine add_name

  subroutine chop_name
    implicit none
    type(chain), pointer :: chopped_chain

    chopped_chain => routine_chain%previous_link
    deallocate(routine_chain)
    routine_chain => chopped_chain

    return
  end subroutine chop_name

  recursive subroutine trace_back(error_code)

    implicit none
    integer :: error_code

    write(unit=*,fmt=*) "   Called by ", routine_chain%routine_name
    if (.not.associated(routine_chain%previous_link)) then
       write(unit=*,fmt=*) &
            " +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++"
       write(unit=*,fmt=*) " "
       if( error_code > 0 ) then
          stop
       else
          return
       end if
    end if

    routine_chain => routine_chain%previous_link
    call trace_back(error_code)

  end subroutine trace_back

  subroutine error_mem(message,error_code)
    character (len=*), intent(in) :: message
    integer, intent(in), optional :: error_code
    integer                       :: action_code
    type(chain), pointer          :: save_chain

    if (present(error_code)) then
       action_code = error_code
    else
       action_code = 1
    end if

    if( action_code /= 0 ) then
       write(unit=*,fmt=*) " "
       write(unit=*,fmt=*) &
            " +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++"

       if( action_code > 0 ) then
          write(unit=*,fmt=*) "   Fatal error in routine `", &
               trim(routine_chain%routine_name),"': ",message
       else
          write(unit=*,fmt=*) "   Warning from routine `", &
               trim(routine_chain%routine_name),"': ",message
          save_chain => routine_chain
       end if
       write(unit=*,fmt=*) &
            " +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++"
       routine_chain => routine_chain%previous_link
       call trace_back(action_code)
       routine_chain => save_chain
    end if

    return
  end subroutine error_mem

  subroutine warning(message)
    character (len=*), intent(in) :: message
    call error_mem(message,-1)
    return
  end subroutine warning

end module error_handler
