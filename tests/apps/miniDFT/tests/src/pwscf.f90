!
! Copyright (C) 2001-2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
PROGRAM pwscf
  !----------------------------------------------------------------------------
  !
  ! ... Plane Wave Self-Consistent Field code 
  !
  USE io_global,        ONLY : stdout, ionode, ionode_id
  USE parameters,       ONLY : ntypx, npk, lmaxx
  USE cell_base,        ONLY : fix_volume
  USE control_flags,    ONLY : conv_elec, lscf
  USE control_flags,    ONLY : conv_ions, istep, nstep, restart, lmd, lbfgs
  USE force_mod,        ONLY : lforce, lstres, sigma
  USE environment,      ONLY : environment_start
  USE check_stop,       ONLY : check_stop_init
  USE mp_global,        ONLY : mp_startup, mp_global_end, intra_image_comm
  USE mp_global,        ONLY : nimage, me_image, root_image, my_image_id
  USE io_files,           ONLY : tmp_dir
  USE image_io_routines,  ONLY : io_image_start
  USE read_input,         ONLY : read_input_file
  !
  IMPLICIT NONE
  !
#ifdef __OPENMP
  include 'omp_lib.h'
#endif
  !
  INTEGER :: ierr
  CHARACTER(len=256) :: dirname
  !
#ifdef __HPCTK
  call hpctoolkit_sampling_stop();
#endif
  !
  !
  CALL mp_startup ( )
  ! reset IO nodes
  ! (do this to make each "image head node" an ionode)
  ! Has to be used ONLY to run nimage copies of pwscf
  !
#ifdef __OPENMP
  call dfftw_init_threads( omp_get_max_threads() )
  call dfftw_plan_with_nthreads( omp_get_max_threads() )
#endif
  !
  IF ( nimage > 1 ) CALL io_image_start( )
  CALL environment_start ( 'MiniDFT' )
  !
  IF ( ionode ) WRITE( unit = stdout, FMT = 9010 ) &
         ntypx, npk, lmaxx
  !
  !
  ! ... open, read, close input file
  !call MQEoptions_read()
  CALL read_input_file ('PW')
  !
  ! ... convert to internal variables
  !
  CALL iosys()
  !
  !
  IF( nimage > 1 ) THEN
     !
     ! ... When nimage are used, open a directory for each one
     ! ...It has to be done here in order not to disturb NEB like calculations
     !
     WRITE( dirname, FMT = '( I5.5 )' ) my_image_id
     tmp_dir = TRIM( tmp_dir )//TRIM( dirname )//'/'
     !
  END IF
  !
  !
  CALL check_stop_init()
  !
  !
  CALL setup ()
  !
  !
  CALL init_run()
  !
  CALL MPI_Barrier( intra_image_comm, ierr )
  CALL start_clock( 'Benchmark_Time' )
#ifdef __HPCTK
  call hpctoolkit_sampling_start();
#endif
#ifdef __IPM
  call MPI_Pcontrol( 1, "Benchmark_Time"//char(0))
#endif
  !
  main_loop: DO
     !
     ! ... electronic self-consistentcy
     !
     CALL electrons()
     !
     IF ( .NOT. conv_elec ) THEN
       CALL punch( 'all' )
       CALL stop_run( conv_elec )
     ENDIF
     !
     ! ... ionic section starts here
     !
     CALL start_clock( 'ions' )
     conv_ions = .TRUE.
     !
     ! ... force calculation
     !
     IF ( lforce ) CALL forces()
     !
     ! ... stress calculation
     !
     IF ( lstres ) CALL stress ( sigma )
     !
     !
     CALL stop_clock( 'ions' )
     !
     ! ... exit condition (ionic convergence) is checked here
     !
     IF ( conv_ions ) EXIT main_loop
     !
     ! ... terms of the hamiltonian depending upon nuclear positions
     ! ... are reinitialized here
     !
     !
  END DO main_loop
  CALL MPI_Barrier( intra_image_comm, ierr )
  CALL stop_clock( 'Benchmark_Time' )
  !
#ifdef __IPM
  call MPI_Pcontrol( -1, "Benchmark_Time"//char(0))
#endif
#ifdef __HPCTK
  call hpctoolkit_sampling_stop();
#endif
  !
  ! ... save final data file
  !
  CALL punch('all')
  CALL stop_run( conv_ions )
#ifdef __OPENMP
  call dfftw_cleanup_threads()
#endif
  !
  !  END IF      
  !
  STOP
  !
9010 FORMAT( /,5X,'Current dimensions of program MiniDFT are:', &
           & /,5X,'Max number of different atomic species (ntypx) = ',I2,&
           & /,5X,'Max number of k-points (npk) = ',I6,&
           & /,5X,'Max angular momentum in pseudopotentials (lmaxx) = ',i2)
  !
END PROGRAM pwscf
