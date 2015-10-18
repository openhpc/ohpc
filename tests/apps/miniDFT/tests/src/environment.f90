!
! Copyright (C) 2002-2011 Quantum ESPRESSO groups
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!==-----------------------------------------------------------------------==!
MODULE environment
  !==-----------------------------------------------------------------------==!

  USE kinds, ONLY: DP
  USE io_files, ONLY: crash_file, crashunit, nd_nmbr
  USE io_global, ONLY: stdout, meta_ionode
  USE mp_global, ONLY: me_image, my_image_id, root_image, nimage, &
      nproc_image, nproc, npool, nproc_pool, nbgrp, get_ntask_groups
  USE global_version, ONLY: version_number

  IMPLICIT NONE

  ! ...  title of the simulation
  CHARACTER(LEN=75) :: title

  SAVE

  PRIVATE

  PUBLIC :: environment_start
  PUBLIC :: environment_end

  !==-----------------------------------------------------------------------==!
CONTAINS
  !==-----------------------------------------------------------------------==!

  SUBROUTINE environment_start( code )

    CHARACTER(LEN=*), INTENT(IN) :: code

    LOGICAL           :: exst, debug = .false.
    CHARACTER(LEN=80) :: code_version, uname
    CHARACTER(LEN=6), EXTERNAL :: int_to_char
    INTEGER :: iost

    ! ... Intel compilers v .ge.8 allocate a lot of stack space
    ! ... Stack limit is often small, thus causing SIGSEGV and crash
  
    CALL remove_stack_limit ( )

    ! ... use ".FALSE." to disable all clocks except the total cpu time clock
    ! ... use ".TRUE."  to enable clocks

    CALL init_clocks( .TRUE. )
    CALL start_clock( TRIM(code) )

    code_version = TRIM (code) // " v." // TRIM (version_number)

    ! ... for compatibility with PWSCF

    nd_nmbr = TRIM ( int_to_char( me_image+1 ))

    IF( meta_ionode ) THEN

       ! ...  search for file CRASH and delete it

       INQUIRE( FILE=TRIM(crash_file), EXIST=exst )
       IF( exst ) THEN
          OPEN(  UNIT=crashunit, FILE=TRIM(crash_file), STATUS='OLD',IOSTAT=iost )
          IF(iost==0) CLOSE( UNIT=crashunit, STATUS='DELETE', IOSTAT=iost )
          IF(iost/=0) WRITE(stdout,'(5x,"Remark: CRASH file could not ne deleted")')
       END IF

    ELSE
       ! ... one processor per image (other than meta_ionode)
       ! ... or, for debugging purposes, all processors,
       ! ... open their own standard output file
#if defined(DEBUG)
       debug = .true.
#endif
       IF (me_image == root_image .OR. debug ) THEN
          uname = 'out.' // trim(int_to_char( my_image_id )) // '_' // &
               trim(int_to_char( me_image))
          OPEN ( unit = stdout, file = TRIM(uname),status='unknown')
       ELSE
          OPEN ( unit = stdout, file='/dev/null', status='unknown' )
       END IF

    END IF
    !
    CALL opening_message( code_version )
    CALL parallel_info ( )
  END SUBROUTINE environment_start

  !==-----------------------------------------------------------------------==!

  SUBROUTINE environment_end( code )

    CHARACTER(LEN=*), INTENT(IN) :: code

    IF ( meta_ionode ) WRITE( stdout, * )

    CALL stop_clock(  TRIM(code) )
    CALL print_clock( TRIM(code) )

    CALL closing_message( )

    IF( meta_ionode ) THEN
       WRITE( stdout,'(A)')      '     JOB DONE.'
       call print_clock( 'Benchmark_Time' )
       WRITE( stdout,3335)
    END IF
3335 FORMAT('=',78('-'),'=')

    RETURN
  END SUBROUTINE environment_end

  !==-----------------------------------------------------------------------==!

  SUBROUTINE opening_message( code_version )

    CHARACTER(LEN=*), INTENT(IN) :: code_version
    CHARACTER(LEN=9)  :: cdate, ctime

    CALL date_and_tim( cdate, ctime )
    !
    WRITE( stdout, '(/5X,"Program ",A18," starts on ",A9," at ",A9)' ) &
         code_version, cdate, ctime
    !
    WRITE (stdout, &
         '(/5X,"This is mini-DFT, a mini-application for plane-wave density functional",&
         &/5X,"theory calculations. Mini-DFT was extracted from",&
         &/5X,"the open-source Quantum ESPRESSO suite by B. Austin (2013). ")')
    !
    WRITE( stdout, '(/5X,"To acknowledge Quantum ESPRESSO, please cite", &
         &/9X,"P. Giannozzi et al., J. Phys.:Condens. Matter 21 395502 (2009);", &
         &/9X,"URL http://www.quantum-espresso.org" )' )

    RETURN
  END SUBROUTINE opening_message

  !==-----------------------------------------------------------------------==!

  SUBROUTINE closing_message( )

    CHARACTER(LEN=9)  :: cdate, ctime
    CHARACTER(LEN=80) :: time_str

    CALL date_and_tim( cdate, ctime )

    time_str = 'This run was terminated on:  ' // ctime // ' ' // cdate

    IF( meta_ionode ) THEN
       WRITE( stdout,*)
       WRITE( stdout,3334) time_str
       WRITE( stdout,3335)
    END IF

3334 FORMAT(3X,A60,/)
3335 FORMAT('=',78('-'),'=')

    RETURN
  END SUBROUTINE closing_message

  !==-----------------------------------------------------------------------==!
  SUBROUTINE parallel_info ( )
    !
#if defined(__OPENMP) || defined(_OPENMP)
    INTEGER, EXTERNAL :: omp_get_max_threads
#endif
    !
#if defined(__OPENMP) || defined(_OPENMP)
    WRITE( stdout, '(/5X,"Parallel version (MPI & OpenMP), running on ",&
         &I8," processor cores")' ) nproc * omp_get_max_threads()
    !
    WRITE( stdout, '(5X,"Number of MPI processes:           ",I8)' ) nproc
    !
    WRITE( stdout, '(5X,"Threads/MPI process:               ",I6)' ) &
         omp_get_max_threads()
#else
    WRITE( stdout, '(/5X,"Parallel version (MPI), running on ",&
         &I8," processors")' ) nproc 
#endif
    !
    IF ( nimage > 1 ) WRITE( stdout, &
         '(5X,"path-images division:  nimage    = ",I8)' ) nimage
    IF ( nbgrp > 1 ) WRITE( stdout, &
         '(5X,"band groups division:  nbgrp     = ",I8)' ) nbgrp
    IF ( npool > 1 ) WRITE( stdout, &
         '(5X,"K-points division:     npool     = ",I8)' ) npool
    IF ( nproc_pool > 1 ) WRITE( stdout, &
         '(5X,"R & G space division:  proc/pool = ",I8)' ) nproc_pool
    IF ( get_ntask_groups() > 1 ) WRITE( stdout, &
         '(5X,"wavefunctions fft division:  fft/group = ",I8)' ) &
         get_ntask_groups()
    !
  END SUBROUTINE parallel_info

  !==-----------------------------------------------------------------------==!
END MODULE environment
!==-----------------------------------------------------------------------==!
