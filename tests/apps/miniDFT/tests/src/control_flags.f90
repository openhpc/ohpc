!
! Copyright (C) 2002-2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=----------------------------------------------------------------------------=!
MODULE control_flags
  !=--------------------------------------------------------------------------=!
  !
  ! ... this module contains all basic variables that controls the
  ! ... execution flow
  !----------------------------------------------
  !
  USE kinds
  USE parameters
  !
  IMPLICIT NONE
  !
  SAVE
  !
  PRIVATE
  !
  TYPE convergence_criteria
     !
     LOGICAL  :: active
     INTEGER  :: nstep
     REAL(DP) :: ekin
     REAL(DP) :: derho
     REAL(DP) :: force
     !
  END TYPE convergence_criteria
  !
  PUBLIC :: tbeg, nomore, nbeg, isave, iprint, tv0rd, nv0rd, tzeroc, tzerop, &
            tfor, tpre, tzeroe, tsde, tsdp, tsdc, taurdr,                    &
            ndr, ndw, tortho, ortho_eps, ortho_max, tstress, tprnfor,        &
            timing, memchk, tprnsfac, tcarpar,                               &
            trane,dt_old,ampre, tranp, amprp, tdipole, t_diis, t_diis_simple,&
            t_diis_rot, tnosee, tnosep, tnoseh, tcp, tcap, tdamp, tdampions, &
            tconvthrs, tolp, convergence_criteria, tionstep, nstepe,         &
            tsteepdesc, tatomicwfc, tscreen, gamma_only, force_pairing,      &
            lecrpa, tddfpt
  !
  PUBLIC :: fix_dependencies, check_flags
  PUBLIC :: tksw, trhor, thdyn, trhow
  PUBLIC :: twfcollect, printwfc
  PUBLIC :: lkpoint_dir
  PUBLIC :: program_name
  !
  ! ...   declare execution control variables
  !
  CHARACTER(LEN=4) :: program_name = ' '  !  used to control execution flow 
                                          !  inside module: 'PW' or 'CP'
  !
  LOGICAL :: trhor     = .FALSE. ! read rho from unit 47 (only cp, seldom used)
  LOGICAL :: trhow     = .FALSE. ! CP code, write rho to restart dir
  LOGICAL :: tksw      = .FALSE. ! CP: write Kohn-Sham states to restart dir
  !
  LOGICAL :: tsde          = .FALSE. ! electronic steepest descent
  LOGICAL :: tzeroe        = .FALSE. ! set to zero the electronic velocities
  LOGICAL :: tfor          = .FALSE. ! move the ions ( calculate forces )
  LOGICAL :: tsdp          = .FALSE. ! ionic steepest descent
  LOGICAL :: tzerop        = .FALSE. ! set to zero the ionic velocities
  LOGICAL :: tprnfor       = .FALSE. ! print forces to standard output
  LOGICAL :: taurdr        = .FALSE. ! read ionic position from standard input
  LOGICAL :: tv0rd         = .FALSE. ! read ionic velocities from standard input
  LOGICAL :: tpre          = .FALSE. ! calculate stress, and (in fpmd) variable cell dynamic
  LOGICAL :: thdyn         = .FALSE. ! variable-cell dynamics (only cp)
  LOGICAL :: tsdc          = .FALSE. ! cell geometry steepest descent
  LOGICAL :: tzeroc        = .FALSE. ! set to zero the cell geometry velocities
  LOGICAL :: tstress       = .FALSE. ! print stress to standard output
  LOGICAL :: tortho        = .FALSE. ! use iterative orthogonalization
  LOGICAL :: timing        = .FALSE. ! print out timing information
  LOGICAL :: memchk        = .FALSE. ! check for memory leakage
  LOGICAL :: tprnsfac      = .FALSE. ! print out structure factor
  LOGICAL :: tcarpar       = .FALSE. ! tcarpar is set TRUE for a "pure" Car Parrinello simulation
  LOGICAL :: tdamp         = .FALSE. ! Use damped dynamics for electrons
  LOGICAL :: tdampions     = .FALSE. ! Use damped dynamics for ions
  LOGICAL :: tatomicwfc    = .FALSE. ! Use atomic wavefunctions as starting guess for ch. density
  LOGICAL :: tscreen       = .FALSE. ! Use screened coulomb potentials for cluster calculations
  LOGICAL :: twfcollect    = .FALSE. ! Collect wave function in the restart file at the end of run.
  LOGICAL :: lkpoint_dir   = .TRUE.  ! save each k point in a different directory
  INTEGER :: printwfc      = -1      ! Print wave functions, temporarely used only by ensemble-dft
  LOGICAL :: force_pairing = .FALSE. ! Force pairing
  LOGICAL :: lecrpa        = .FALSE. ! RPA correlation energy request
  LOGICAL :: tddfpt        = .FALSE. ! use tddfpt specific tweaks to ph.x routines
  !
  TYPE (convergence_criteria) :: tconvthrs
                              !  thresholds used to check GS convergence
  !
  ! ... Ionic vs Electronic step frequency
  ! ... When "ion_nstep > 1" and "electron_dynamics = 'md' | 'sd' ", ions are
  ! ... propagated every "ion_nstep" electronic step only if the electronic
  ! ... "ekin" is lower than "ekin_conv_thr"
  !
  LOGICAL :: tionstep = .FALSE.
  INTEGER :: nstepe   = 1
                            !  parameters to control how many electronic steps
                            !  between ions move

  LOGICAL :: tsteepdesc = .FALSE.
                            !  parameters for electronic steepest desceent

  INTEGER :: nbeg   = 0 ! internal code for initialization ( -1, 0, 1, 2, .. )
  INTEGER :: ndw    = 0 !
  INTEGER :: ndr    = 0 !
  INTEGER :: nomore = 0 !
  INTEGER :: iprint =10 ! print output every iprint step
  INTEGER :: isave  = 0 ! write restart to ndr unit every isave step
  INTEGER :: nv0rd  = 0 !
  !
  ! ... .TRUE. if only gamma point is used
  !
  LOGICAL :: gamma_only = .TRUE.
  !
  ! This variable is used whenever a timestep change is requested
  !
  REAL(DP) :: dt_old = -1.0_DP
  !
  ! ... Wave function randomization
  !
  LOGICAL  :: trane = .FALSE.
  REAL(DP) :: ampre = 0.0_DP
  !
  ! ... Ionic position randomization
  !
  LOGICAL  :: tranp(nsx) = .FALSE.
  REAL(DP) :: amprp(nsx) = 0.0_DP
  !
  ! ... Read the cell from standard input
  !
  LOGICAL :: tbeg = .FALSE.
  !
  ! ... This flags control the calculation of the Dipole Moments
  !
  LOGICAL :: tdipole = .FALSE.
  !
  ! ... Flags that controls DIIS electronic minimization
  !
  LOGICAL :: t_diis        = .FALSE.
  LOGICAL :: t_diis_simple = .FALSE.
  LOGICAL :: t_diis_rot    = .FALSE.
  !
  ! ... Flag controlling the Nose thermostat for electrons
  !
  LOGICAL :: tnosee = .FALSE.
  !
  ! ... Flag controlling the Nose thermostat for the cell
  !
  LOGICAL :: tnoseh = .FALSE.
  !
  ! ... Flag controlling the Nose thermostat for ions
  !
  LOGICAL  :: tnosep = .FALSE.
  LOGICAL  :: tcap   = .FALSE.
  LOGICAL  :: tcp    = .FALSE.
  REAL(DP) :: tolp   = 0.0_DP   !  tolerance for temperature variation
  !
  REAL(DP), PUBLIC :: &
       ekin_conv_thr = 0.0_DP, &!  conv. threshold for fictitious e. kinetic energy
       etot_conv_thr = 0.0_DP, &!  conv. threshold for DFT energy
       forc_conv_thr = 0.0_DP   !  conv. threshold for atomic forces
  INTEGER, PUBLIC :: &
       ekin_maxiter = 100,   &!  max number of iter. for ekin convergence
       etot_maxiter = 100,   &!  max number of iter. for etot convergence
       forc_maxiter = 100     !  max number of iter. for atomic forces conv.
  !
  ! ... Several variables controlling the run ( used mainly in PW calculations )
  !
  ! ... logical flags controlling the execution
  !
  LOGICAL, PUBLIC :: &
    lfixatom=.FALSE., &! if .TRUE. some atom is kept fixed
    lscf    =.FALSE., &! if .TRUE. the calc. is selfconsistent
    lbfgs   =.FALSE., &! if .TRUE. the calc. is a relaxation based on BFGS
    lmd     =.FALSE., &! if .TRUE. the calc. is a dynamics
    llang   =.FALSE., &! if .TRUE. the calc. is Langevin dynamics
    lpath   =.FALSE., &! if .TRUE. the calc. is a path optimizations
    lneb    =.FALSE., &! if .TRUE. the calc. is NEB dynamics
    lsmd    =.FALSE., &! if .TRUE. the calc. is string dynamics
    lwf     =.FALSE., &! if .TRUE. the calc. is with wannier functions
    !=================================================================
    !  Lingzhu Kong 
    lwfnscf =.FALSE., &
    lwfpbe0 =.FALSE., &! if .TRUE. the calc. is with wannier functions and with PBE0 functional
    lwfpbe0nscf=.FALSE.,&
    !=================================================================
    lbands  =.FALSE., &! if .TRUE. the calc. is band structure
    lconstrain=.FALSE.,&! if .TRUE. the calc. is constraint
    ldamped =.FALSE., &! if .TRUE. the calc. is a damped dynamics
    llondon =.FALSE., & ! if .TRUE. compute semi-empirical dispersion correction
    restart =.FALSE.   ! if .TRUE. restart from results of a preceding run
  !
  ! ... pw self-consistency
  !
  INTEGER, PUBLIC :: &
    ngm0,             &! used in mix_rho
    niter,            &! the maximum number of iteration
    nmix,             &! the number of iteration kept in the history
    imix               ! the type of mixing (0=plain,1=TF,2=local-TF)
  REAL(DP), PUBLIC  :: &
    mixing_beta,      &! the mixing parameter
    tr2                ! the convergence threshold for potential

  LOGICAL, PUBLIC :: &
    conv_elec          ! if .TRUE. electron convergence has been reached
  ! next 3 variables used for EXX calculations
  LOGICAL, PUBLIC :: &
    adapt_thr       ! if .TRUE. an adaptive convergence threshold is used
                       ! for the scf cycle in an EXX calculation.
  REAL(DP), PUBLIC  :: &
    tr2_init,         &! initial value of tr2 for adaptive thresholds
    tr2_multi          ! the dexx multiplier for adaptive thresholds
                       ! tr2 = tr2_multi * dexx after each V_exx update 
  !
  ! ... pw diagonalization
  !
  REAL(DP), PUBLIC  :: &
    ethr               ! the convergence threshold for eigenvalues
  INTEGER, PUBLIC :: &
    david,            &! max dimension of subspace in Davidson diagonalization
    isolve,           &! Davidson or CG or DIIS diagonalization
    max_cg_iter,      &! maximum number of iterations in a CG di
    diis_buff,        &! dimension of the buffer in diis
    diis_ndim          ! dimension of reduced basis in DIIS
  LOGICAL, PUBLIC :: &
    diago_full_acc = .FALSE. ! if true,  empty eigenvalues have the same
                             ! accuracy of the occupied ones
  !
  ! ... wfc and rho extrapolation
  !
  REAL(DP), PUBLIC :: &
    alpha0,           &! the mixing parameters for the extrapolation
    beta0              ! of the starting potential
  INTEGER, PUBLIC :: &
    history,          &! number of old steps available for potential updating
    pot_order = 0,    &! type of potential updating ( see update_pot )
    wfc_order = 0      ! type of wavefunctions updating ( see update_pot )
  !
  ! ... ionic dynamics
  !
  INTEGER, PUBLIC :: &
    nstep = 1,       &! number of ionic steps
    istep = 0          ! current ionic step
  LOGICAL, PUBLIC :: &
    conv_ions          ! if .TRUE. ionic convergence has been reached
  REAL(DP), PUBLIC  :: &
    upscale            ! maximum reduction of convergence threshold
  !
  ! ... system's symmetries
  !
  LOGICAL, PUBLIC :: &
    noinv = .FALSE.    ! if .TRUE. q=>-q symmetry not used in k-point generation
  !
  ! ... phonon calculation
  !
  INTEGER, PUBLIC :: &
    modenum            ! for single mode phonon calculation
  !
  ! ... printout control
  !
  INTEGER, PUBLIC :: &
    io_level = 1       ! variable controlling the amount of I/O to file
  INTEGER, PUBLIC :: & ! variable controlling the amount of I/O to output
    iverbosity = 0     ! -2 minimal, -1 low, 0 medium, 1 high, 2 debug
  !
  ! ... miscellany
  !
  LOGICAL, PUBLIC :: &
    use_para_diag = .FALSE.  ! if .TRUE. a fully distributed memory iteration 
                             ! algorithm and parallel Householder algorithm are used
  !
  LOGICAL, PUBLIC :: &
    remove_rigid_rot = .FALSE. ! if .TRUE. the total torque acting on the atoms 
                               ! is removed
  LOGICAL, PUBLIC :: &
    do_makov_payne = .FALSE.   ! if .TRUE. makov-payne correction for isolated
                               ! system is used
  !
  INTEGER  :: ortho_max = 0      ! maximum number of iterations in routine ortho
  REAL(DP) :: ortho_eps = 0.0_DP ! threshold for convergence in routine ortho
  !
  ! ... Number of neighbouring cell to consider in ewald sum
  !
  INTEGER, PUBLIC :: iesr = 1
  !
  ! ... Real-sapce algorithms
  !
  LOGICAL,          PUBLIC :: tqr=.FALSE. ! if true the Q are in real space

  !LOGICAL,          PUBLIC :: real_space=.false. ! beta functions in real space
  !
  ! ... External Forces on Ions
  !
  LOGICAL,          PUBLIC :: textfor = .FALSE.

  !
  ! ...  end of module-scope declarations
  !
  !=--------------------------------------------------------------------------=!
  CONTAINS
  !=--------------------------------------------------------------------------=!
    !
    !------------------------------------------------------------------------
    SUBROUTINE fix_dependencies()
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      !
      ! ... Car Parrinello simulation
      !
      tcarpar = .TRUE.
      !
      IF ( t_diis .OR. tsteepdesc ) THEN
         !
         tcarpar = .FALSE.
         !
      END IF
      !
      ! ... if thdyn = .FALSE. set TSDC and TZEROC to .FALSE. too.
      !
      IF ( .NOT. thdyn ) THEN
         !
         tsdc   = .FALSE.
         tzeroc = .FALSE.
         !
      END IF
      !
      IF ( .NOT. tfor ) THEN
         !
         tzerop = .FALSE.
         tv0rd  = .FALSE.
         tsdp   = .FALSE.
         tcp    = .FALSE.
         tcap   = .FALSE.
         tnosep = .FALSE.
         !
      ELSE
         !
         IF ( tsdp ) THEN
            !
            tcp    = .FALSE.
            tcap   = .FALSE.
            tnosep = .FALSE.
            tv0rd  = .FALSE.
            !
         END IF
         !
         IF ( tv0rd ) tzerop = .TRUE.
         !
      END IF
      !
      IF ( tsde ) tnosee = .FALSE.
      !
      CALL check_flags()
      !
      RETURN
      !
    END SUBROUTINE fix_dependencies
    !
    !------------------------------------------------------------------------
    SUBROUTINE check_flags()
      !------------------------------------------------------------------------
      !
      ! ...  do some checks for consistency
      !
      IF ( tnosee .AND. t_diis ) &
         CALL errore( ' control_flags ', 'DIIS + ELECT. NOSE ? ', 0 )
      !
      !IF ( tortho .AND. t_diis ) &
      !   CALL errore(' control_flags ','DIIS, ORTHO NOT PERMITTED',0)
      !
      IF ( tnosep .AND. tcp ) &
         CALL errore( ' control_flags ', ' TCP AND TNOSEP BOTH TRUE', 0 )
      !
      IF ( tnosep .AND. tcap ) &
         CALL errore( ' control_flags ', ' TCAP AND TNOSEP BOTH TRUE', 0 )
      !
      IF ( tcp .AND. tcap ) &
         CALL errore( ' control_flags ', ' TCP AND TCAP BOTH TRUE', 0 )
      !
      IF ( tdipole .AND. thdyn ) &
         CALL errore( '  control_flags  ', ' DIPOLE WITH CELL DYNAMICS ', 0 )
      !
      IF ( tv0rd .AND. tsdp ) &
         CALL errore( ' control_flags ', &
                    & ' READING IONS VELOCITY WITH STEEPEST D.', 0 )
      !
      RETURN
      !
    END SUBROUTINE check_flags
    !
END MODULE control_flags

