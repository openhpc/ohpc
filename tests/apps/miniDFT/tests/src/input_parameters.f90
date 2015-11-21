!
! Copyright (C) 2002-2008 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!=----------------------------------------------------------------------------=!
!
MODULE input_parameters
!
!=----------------------------------------------------------------------------=!
!
!  this module contains
!  1) the definitions of all input parameters
!     (both those read from namelists and those read from cards)
!  2) the definitions of all namelists
!  3) routines that allocate data needed in input
!  Note that all values are initialized, but the default values should be
!  set in the appropriate routines contained in module "read_namelists"
!  The documentation of input variables can be found in Doc/INPUT_PW.*
!  (for pw.x) or in Doc/INPUT_CP (for cp.x)
!  Originally written by Carlo Cavazzoni for FPMD
!
!=----------------------------------------------------------------------------=!
  !
  USE kinds,      ONLY : DP
  USE parameters, ONLY : nsx, lqmax
  !
  IMPLICIT NONE
  !
  SAVE
  !
!=----------------------------------------------------------------------------=!
! BEGIN manual
!
!
! * DESCRIPTION OF THE INPUT FILE
!  (to be given as standard input)
!
!  The input file has the following layout:
!
!     &CONTROL
!       control_parameter_1,
!       control_parameter_2,
!       .......
!       control_parameter_Lastone
!     /
!     &SYSTEM
!       sistem_parameter_1,
!       sistem_parameter_2,
!       .......
!       sistem_parameter_Lastone
!     /
!     &ELECTRONS
!       electrons_parameter_1,
!       electrons_parameter_2,
!       .......
!       electrons_parameter_Lastone
!     /
!     &IONS
!       ions_parameter_1,
!       ions_parameter_2,
!       .......
!       ions_parameter_Lastone
!     /
!     &CELL
!       cell_parameter_1,
!       cell_parameter_2,
!       .......
!       cell_parameter_Lastone
!     /
!     ATOMIC_SPECIES
!      slabel_1 mass_1 pseudo_file_1
!      slabel_2 mass_2 pseudo_file_2
!      .....
!     ATOMIC_POSITIONS
!      alabel_1  px_1 py_1 pz_1
!      alabel_2  px_2 py_2 pz_2
!      .....
!     CARD_3
!     ....
!     CARD_N
!
!  -- end of input file --
!
!=----------------------------------------------------------------------------=!
!  CONTROL Namelist Input Parameters
!=----------------------------------------------------------------------------=!
!
        CHARACTER(len=80) :: title = ' '
          ! a string describing the current job

        CHARACTER(len=80) :: calculation = 'none'
          ! Specify the type of the simulation
          ! See below for allowed values
        CHARACTER(len=80) :: calculation_allowed(15)
        DATA calculation_allowed / 'scf', 'nscf', 'relax', 'md', 'cp', &
          'vc-relax', 'vc-md', 'vc-cp', 'bands', 'neb', 'smd', 'cp-wf', &
          'cp-wf-nscf','cp-wf-pbe0', 'pbe0-nscf'/   ! Lingzhu Kong
        CHARACTER(len=80) :: verbosity = 'default'
          ! define the verbosity of the code output
        CHARACTER(len=80) :: verbosity_allowed(6)
        DATA verbosity_allowed / 'debug', 'high', 'medium', 'default', &
                                 'low', 'minimal' /

        CHARACTER(len=80) :: restart_mode = 'restart'
          ! specify how to start/restart the simulation
        CHARACTER(len=80) :: restart_mode_allowed(3)
        DATA restart_mode_allowed / 'from_scratch', 'restart', 'reset_counters' /

        INTEGER :: nstep = 10
          ! number of simulation steps, see "restart_mode"

        INTEGER :: iprint = 10
          ! number of steps/scf iterations between successive writings
          ! of relevant physical quantities to standard output

        INTEGER :: isave = 100
          ! number of steps between successive savings of
          ! information needed to restart the run (see "ndr", "ndw")
          ! used only in CP

        LOGICAL :: tstress = .true.
          !  .TRUE.  calculate the stress tensor
          !  .FALSE. do not calculate the stress tensor

        LOGICAL :: tprnfor = .true.
          !  .TRUE.  calculate the atomic forces
          !  .FALSE. do not calculate the atomic forces

        REAL(DP) :: dt = 1.0_DP
          ! time step for molecular dynamics simulation, in atomic units
          ! CP: 1 a.u. of time = 2.4189 * 10^-17 s, PW: twice that much
          ! Typical values for CP simulations are between 1 and 10 a.u.
          ! For Born-Oppenheimer simulations, larger values can be used,
          ! since it mostly depends only upon the mass of ions.

        INTEGER :: ndr = 50
          ! Fortran unit from which the code reads the restart file

        INTEGER :: ndw = 50
          ! Fortran unit to which the code writes the restart file

        CHARACTER(len=256) :: outdir = './'
          ! specify the directory where the code opens output and restart
          ! files. When possible put this directory in the fastest available
          ! filesystem ( not NFS! )

        CHARACTER(len=256) :: prefix = 'prefix'
          ! specify the prefix for the output file, if not specified the
          ! files are opened as standard fortran units.

        CHARACTER(len=256) :: pseudo_dir = './'
          ! specify the directory containing the pseudopotentials

        REAL(DP) :: refg = 0.05_DP
          ! Accurancy of the interpolation table, interval between
          ! table values in Rydberg

        CHARACTER(len=256) :: wfcdir = 'undefined'
          ! scratch directory that is hopefully local to the node
          ! to store large, usually temporary files.

        REAL(DP) :: max_seconds = 1.0E+7_DP
          ! smoothly terminate program after the specified number of seconds
          ! this parameter is typically used to prevent an hard kill from
          ! the queuing system.

        REAL(DP) :: ekin_conv_thr = 1.0E-5_DP
          ! convergence criterion for electron minimization
          ! this criterion is met when "ekin < ekin_conv_thr"
          ! convergence is achieved when all criteria are met

        REAL(DP) :: etot_conv_thr = 1.0E-4_DP
          ! convergence criterion for ion minimization
          ! this criterion is met when "etot(n+1)-etot(n) < etot_conv_thr",
          ! where "n" is the step index, "etot" the DFT energy
          ! convergence is achieved when all criteria are met

        REAL(DP) :: forc_conv_thr = 1.0E-3_DP
          ! convergence criterion for ion minimization
          ! this criterion is met when "MAXVAL(fion) < forc_conv_thr",
          ! where fion are the atomic forces
          ! convergence is achieved when all criteria are met

        CHARACTER(len=80) :: disk_io = 'default'
          ! Specify the amount of I/O activities

        LOGICAL :: tefield  = .false.
          ! if .TRUE. a sawtooth potential simulating a finite electric field
          ! is added to the local potential = only used in PW

          LOGICAL :: tefield2  = .false.
          ! if .TRUE. a second finite electric field is added to the local potential
          ! only used in CP

        LOGICAL :: lelfield = .false.
          ! if .TRUE. a static homogeneous electric field is present
          ! via the modern theory of polarizability - differs from tefield!

        LOGICAL :: dipfield = .false.
          ! if .TRUE. the dipole field is subtracted
          ! only used in PW for surface calculations

        LOGICAL :: lberry = .false.
          ! if .TRUE., use modern theory of the polarization

        INTEGER :: gdir = 0
          ! G-vector for polarization calculation ( related to lberry )
          ! only used in PW

        INTEGER :: nppstr = 0
          ! number of k-points (parallel vector) ( related to lberry )
          ! only used in PW

        INTEGER  :: nberrycyc = 1
          !number of covergence cycles on electric field

        LOGICAL :: wf_collect = .false.
          ! This flag controls the way wavefunctions are stored to disk:
          !  .TRUE.  collect wavefunctions from all processors, store them
          !          into a single restart file on a single processors
          !  .FALSE. do not collect wavefunctions, store them into distributed
          !          files
          ! Only for PW and only in the parallel case

        INTEGER :: printwfc=1
          ! if <0 do nothing, if==0 print rho and fort.47, if == nband print band

        LOGICAL :: saverho = .true.
          ! This flag controls the saving of charge density in CP codes:
          !  .TRUE.  save charge density to restart dir
          !  .FALSE. do not save charge density

        LOGICAL :: tabps = .false. ! for ab-initio pressure and/or surface
                                   ! calculations

        LOGICAL :: lkpoint_dir = .true. ! opens a directory for each k point


        LOGICAL :: lecrpa = .FALSE.
          ! if true symmetry in scf run is neglected for RPA Ec calculation
          ! 

        CHARACTER(len=256) :: vdw_table_name = ' '

        NAMELIST / control / title, calculation, verbosity, restart_mode, &
          nstep, iprint, isave, tstress, tprnfor, dt, ndr, ndw, outdir,   &
          prefix, wfcdir, max_seconds, ekin_conv_thr, etot_conv_thr,      &
          forc_conv_thr, pseudo_dir, disk_io, tefield, dipfield, lberry,  &
          gdir, nppstr, wf_collect, printwfc, lelfield, nberrycyc, refg,  &
          tefield2, saverho, tabps, lkpoint_dir, lecrpa,     &
          vdw_table_name



!
!=----------------------------------------------------------------------------=!
!  SYSTEM Namelist Input Parameters
!=----------------------------------------------------------------------------=!
!
        INTEGER :: ibrav = 14
          ! index of the the Bravais lattice
          ! Note: in variable cell CP molecular dynamics, usually one does
          !       not want to put constraints on the cell symmetries, thus
          !       ibrav = 14 is used

        REAL(DP) :: celldm(6) = 0.0_DP
          ! dimensions of the cell (lattice parameters and angles)

        REAL(DP) :: a = 0.0_DP
        REAL(DP) :: c = 0.0_DP
        REAL(DP) :: b = 0.0_DP
        REAL(DP) :: cosab = 0.0_DP
        REAL(DP) :: cosac = 0.0_DP
        REAL(DP) :: cosbc = 0.0_DP
          ! Alternate definition of the cell - use either this or celldm

        INTEGER :: nat = 0
          ! total number of atoms

        INTEGER :: ntyp = 0
          ! number of atomic species

        INTEGER :: nbnd = 0
          ! number of electronic states, this parameter is MANDATORY in CP

        REAL(DP):: tot_charge = 0.0_DP
          ! total system charge

        REAL(DP) :: tot_magnetization = -1.0_DP
          ! majority - minority spin.
          ! A value < 0 means unspecified

        REAL(DP) :: ecutwfc = 0.0_DP
          ! energy cutoff for wave functions in k-space ( in Rydberg )
          ! this parameter is MANDATORY

        REAL(DP) :: ecutrho = 0.0_DP
          ! energy cutoff for charge density in k-space ( in Rydberg )
          ! by default its value is "4 * ecutwfc"

        INTEGER :: nr1 = 0
        INTEGER :: nr2 = 0
        INTEGER :: nr3 = 0
          ! dimensions of the real space grid for charge and potentials
          ! presently NOT used in CP

        INTEGER :: nr1s = 0
        INTEGER :: nr2s = 0
        INTEGER :: nr3s = 0
          ! dimensions of the real space grid for wavefunctions
          ! presently NOT used in CP

        INTEGER :: nr1b = 0
        INTEGER :: nr2b = 0
        INTEGER :: nr3b = 0
          ! dimensions of the "box" grid for Ultrasoft pseudopotentials

        CHARACTER(len=80) :: occupations = 'fixed'
          ! select the way electronic states are filled
          ! See card 'OCCUPATIONS' if ocupations='from_input'

        CHARACTER(len=80) :: smearing = 'gaussian'
          ! select the way electronic states are filled for metalic systems

        REAL(DP) :: degauss = 0.0_DP
          ! parameter for the smearing functions - NOT used in CP

        INTEGER :: nspin = 1
          ! number of spinors
          ! "nspin = 1" for LDA simulations
          ! "nspin = 2" for LSD simulations
          ! "nspin = 4" for NON COLLINEAR simulations


        LOGICAL :: nosym = .true., noinv = .false.
          ! (do not) use symmetry, q => -q symmetry in k-point generation
        LOGICAL :: nosym_evc = .false.
          ! if .true. use symmetry only to symmetrize k points
        LOGICAL :: force_symmorphic = .false.
          ! if .true. disable fractionary translations (nonsymmorphic groups)
        LOGICAL :: use_all_frac = .false.
          ! if .true. enable usage of all fractionary translations, 
          ! disabling check if they are commensurate with FFT grid

        REAL(DP) :: ecfixed = 0.0_DP, qcutz = 0.0_DP, q2sigma = 0.0_DP
          ! parameters for modified kinetic energy functional to be used
          ! in variable-cell constant cut-off simulations

        CHARACTER(len=80) :: input_dft = 'none'
          ! Variable used to overwrite dft definition contained in
          ! pseudopotential files; 'none' means DFT is read from pseudos.
          ! Only used in PW - allowed values: any legal DFT value

        REAL(DP) :: starting_magnetization( nsx ) = 0.0_DP
          ! ONLY PW

        LOGICAL :: lda_plus_u = .false.
          ! Use DFT+U method - following are the needed parameters
        INTEGER :: lda_plus_u_kind = 0
        INTEGER, PARAMETER :: nspinx=2
        REAL(DP) :: starting_ns_eigenvalue(lqmax,nspinx,nsx) = -1.0_DP
        REAL(DP) :: hubbard_u(nsx) = 0.0_DP
        REAL(DP) :: hubbard_j(3,nsx) = 0.0_DP
        REAL(DP) :: hubbard_alpha(nsx) = 0.0_DP
        CHARACTER(len=80) :: U_projection_type = 'atomic'

        LOGICAL :: la2F = .false.
          ! For electron-phonon calculations
          !
        LOGICAL :: step_pen=.false.
        REAL(DP) :: A_pen(10,nspinx) = 0.0_DP
        REAL(DP) :: sigma_pen(10) = 0.01_DP
        REAL(DP) :: alpha_pen(10) = 0.0_DP

          ! next group of variables PWSCF ONLY
          !
        INTEGER  :: nqx1 = 1, nqx2 = 1, nqx3=1
          !
        REAL(DP) :: exx_fraction = -1.0_DP      ! if negative, use defaults
        REAL(DP) :: screening_parameter = -1.0_DP
          !
        CHARACTER(len=80) :: exxdiv_treatment = 'gygi-baldereschi'
          ! define how ro cure the Coulomb divergence in EXX
          ! Allowed values are:
        CHARACTER(len=80) :: exxdiv_treatment_allowed(6)
        DATA exxdiv_treatment_allowed / 'gygi-baldereschi', 'gygi-bald', 'g-b',&
             'vcut_ws', 'vcut_spherical', 'none' /
          !
        LOGICAL  :: x_gamma_extrapolation = .true.
        REAL(DP) :: yukawa = 0.0_DP
        REAL(DP) :: ecutvcut = 0.0_DP
          ! auxiliary variables to define exxdiv treatment
        LOGICAL  :: adaptive_thr = .FALSE.
        REAL(DP) :: conv_thr_init = 0.001_DP
        REAL(DP) :: conv_thr_multi = 0.1_DP
        REAL(DP) :: ecutfock = -1.d0

          ! parameters for external electric field
        INTEGER  :: edir = 0
        REAL(DP) :: emaxpos = 0.0_DP
        REAL(DP) :: eopreg = 0.0_DP
        REAL(DP) :: eamp = 0.0_DP

          ! Various parameters for noncollinear calculations
        LOGICAL  :: noncolin = .false.
        LOGICAL  :: lspinorb = .false.
        LOGICAL  :: starting_spin_angle=.FALSE.
        REAL(DP) :: lambda = 1.0_DP
        REAL(DP) :: fixed_magnetization(3) = 0.0_DP
        REAL(DP) :: angle1(nsx) = 0.0_DP
        REAL(DP) :: angle2(nsx) = 0.0_DP
        INTEGER  :: report = 1
        LOGICAL  :: no_t_rev = .FALSE.

        CHARACTER(len=80) :: constrained_magnetization = 'none'
        REAL(DP) :: B_field(3) = 0.0_DP
          ! A fixed magnetic field defined by the vector B_field is added
          ! to the exchange and correlation magnetic field.

        CHARACTER(len=80) :: sic = 'none'
          ! CP only - SIC correction (D'avezac Mauri)
          ! Parameters for SIC calculation
        REAL(DP) :: sic_epsilon = 0.0_DP
        REAL(DP) :: sic_alpha   = 0.0_DP
        LOGICAL   :: force_pairing = .false.

        LOGICAL :: spline_ps = .false.
          ! use spline interpolation for pseudopotential
        LOGICAL :: one_atom_occupations=.false.

        CHARACTER(len=80) :: assume_isolated = 'none'
          ! used to define corrections for isolated systems
          ! other possibilities:
          !  'makov-payne', 'martyna-tuckerman`, `dcc`, 'esm'

        NAMELIST / system / ibrav, celldm, a, b, c, cosab, cosac, cosbc, nat, &
             ntyp, nbnd, ecutwfc, ecutrho, nr1, nr2, nr3, nr1s, nr2s,         &
             nr3s, nr1b, nr2b, nr3b, nosym, nosym_evc, noinv, use_all_frac,   &
             force_symmorphic, starting_magnetization,                        &
             occupations, degauss, nspin, ecfixed,                            &
             qcutz, q2sigma, lda_plus_U, lda_plus_u_kind,                     &
             Hubbard_U, Hubbard_J, Hubbard_alpha,                             &
             edir, emaxpos, eopreg, eamp, smearing, starting_ns_eigenvalue,   &
             U_projection_type, input_dft, la2F, assume_isolated,             &
             nqx1, nqx2, nqx3, ecutfock,                                      &
             exxdiv_treatment, x_gamma_extrapolation, yukawa, ecutvcut,       &
             exx_fraction, screening_parameter,                               &
             noncolin, lspinorb, starting_spin_angle, lambda, angle1, angle2, &
             report,              &
             constrained_magnetization, B_field, fixed_magnetization,         &
             sic, sic_epsilon, force_pairing, sic_alpha,                      &
             tot_charge, tot_magnetization,                                   &
             spline_ps, one_atom_occupations, &
             step_pen, A_pen, sigma_pen, alpha_pen, no_t_rev
!
!=----------------------------------------------------------------------------=!
!  EE Namelist Input Parameters
!=----------------------------------------------------------------------------=!
!
! kinetic energy cutoff for the coarse (MultiGrid) grid
        REAL(DP) :: ecutcoarse = 100.0d0
! amount of "new" correction introduced when mixing
        REAL(DP) :: mixing_charge_compensation = 1.0
! error tolerance for the multigrid solver
        REAL(DP) :: errtol = 1.d-22
! how early in scf itarations should the corrective pot start being calculated
        REAL(DP) :: comp_thr = 1.d-2
! nlev number of grid levels in the multigrid solver
        INTEGER :: nlev = 2
! itmax maximum number of iterations in the multigrid solver
        INTEGER :: itmax = 1000
! whichbc 0 if aperiodic
        INTEGER :: whichbc(3) = 0
! sets after how many scf cycles the corrective potential should be calculated
        INTEGER :: n_charge_compensation = 5
!
        INTEGER :: ncompx = 1
        INTEGER :: ncompy = 1
        INTEGER :: ncompz = 1
          ! ONLY PWSCF
!
        INTEGER :: mr1 = 0
        INTEGER :: mr2 = 0
        INTEGER :: mr3 = 0

        REAL(DP) :: cellmin( 3 ) = 0.D0
          ! ONLY PWSCF

        REAL(DP) :: cellmax( 3 ) = 1.D0

        NAMELIST / ee / comp_thr,    &
             ncompx,n_charge_compensation,              &
             ncompy, ncompz,mixing_charge_compensation, &
             mr1, mr2, mr3, ecutcoarse,                 &
             errtol, nlev, itmax, whichbc,              &
             cellmin, cellmax

!=----------------------------------------------------------------------------=!
!  ELECTRONS Namelist Input Parameters
!=----------------------------------------------------------------------------=!

        REAL(DP) :: emass = 0.0_DP
          ! effective electron mass in the CP Lagrangian,
          ! in atomic units ( 1 a.u. of mass = 1/1822.9 a.m.u. = 9.10939 * 10^-31 kg )
          ! Typical values in CP simulation are between 100. and 1000.

        REAL(DP) :: emass_cutoff = 0.0_DP
          ! mass cut-off (in Rydbergs) for the Fourier acceleration
          ! effective mass is rescaled for "G" vector components with kinetic
          ! energy above "emass_cutoff"
          ! Use a value grether than "ecutwfc" to disable Fourier acceleration.

        CHARACTER(len=80) :: orthogonalization = 'ortho'
          ! orthogonalization = 'Gram-Schmidt' | 'ortho'*
          ! selects the orthonormalization method for electronic wave functions
          !  'Gram-Schmidt'  use Gram-Schmidt algorithm
          !  'ortho'         use iterative algorithm

        REAL(DP) :: ortho_eps = 1.E-8_DP
          ! meaningful only if orthogonalization = 'ortho'
          ! tolerance for iterative orthonormalization,
          ! a value of 1.d-8 is usually sufficent

        INTEGER   :: ortho_max = 20
          ! meaningful only if orthogonalization = 'ortho'
          ! maximum number of iterations for orthonormalization
          ! usually between 15 and 30.

        INTEGER :: electron_maxstep = 1000
          ! maximum number of steps in electronic minimization
          ! This parameter apply only when using 'cg' electronic or
          ! ionic dynamics

        CHARACTER(len=80) :: electron_dynamics = 'none'
          ! set how electrons should be moved
        CHARACTER(len=80) :: electron_dynamics_allowed(6)
        DATA electron_dynamics_allowed &
          / 'default', 'sd', 'cg', 'damp', 'verlet', 'none' /

        REAL(DP) :: electron_damping = 0.0_DP
          ! meaningful only if " electron_dynamics = 'damp' "
          ! damping frequency times delta t, optimal values could be
          ! calculated with the formula
          !        sqrt(0.5*log((E1-E2)/(E2-E3)))
          ! where E1 E2 E3 are successive values of the DFT total energy
          ! in a steepest descent simulations

        CHARACTER(len=80) :: electron_velocities = 'default'
          ! electron_velocities = 'zero' | 'default'*
          ! 'zero'    restart setting electronic velocities to zero
          ! 'default' restart using electronic velocities of the previous run

        CHARACTER(len=80) :: electron_temperature = 'not_controlled'
          ! electron_temperature = 'nose' | 'not_controlled'* | 'rescaling'
          ! 'nose'           control electronic temperature using Nose thermostat
          !                  see parameter "fnosee" and "ekincw"
          ! 'rescaling'      control electronic temperature via velocities rescaling
          ! 'not_controlled' electronic temperature is not controlled

        REAL(DP) :: ekincw = 0.0_DP
          ! meaningful only with "electron_temperature /= 'not_controlled' "
          ! value of the average kinetic energy (in atomic units) forced
          ! by the temperature control

        REAL(DP) :: fnosee = 0.0_DP
          ! meaningful only with "electron_temperature = 'nose' "
          ! oscillation frequency of the nose thermostat (in terahertz)

        CHARACTER(len=80) :: startingwfc = 'random'
          ! startingwfc = 'atomic' | 'atomic+random' | 'random' | 'file'
          ! define how the code should initialize the wave function
          ! 'atomic'   start from superposition of atomic wave functions
          ! 'atomic+random' as above, plus randomization
          ! 'random'   start from random wave functions
          ! 'file'     read wavefunctions from file

        REAL(DP) :: ampre = 0.0_DP
          ! meaningful only if "startingwfc = 'random'", amplitude of the
          ! randomization ( allowed values: 0.0 - 1.0 )

        REAL(DP) :: grease = 0.0_DP
          ! a number <= 1, very close to 1: the damping in electronic
          ! damped dynamics is multiplied at each time step by "grease"
          ! (avoids overdamping close to convergence: Obsolete ?)
          ! grease = 1 : normal damped dynamics
          ! used only in CP

        INTEGER :: diis_size = 0
          ! meaningful only with " electron_dynamics = 'diis' "
          ! size of the matrix used for the inversion in the iterative subspace
          ! default is 4, allowed value 1-5

        INTEGER :: diis_nreset = 0
          ! meaningful only with " electron_dynamics = 'diis' "
          ! number of steepest descendent step after a reset of the diis
          ! iteration, default value is 3

        REAL(DP) :: diis_hcut = 0.0_DP
          ! meaningful only with " electron_dynamics = 'diis' "
          ! energy cutoff (a.u.), above which an approximate diagonal
          ! hamiltonian is used in finding the direction to the minimum
          ! default is "1.0"

        REAL(DP) :: diis_wthr = 1.E-4_DP
          ! meaningful only with " electron_dynamics = 'diis' "
          ! convergence threshold for wave function
          ! this criterion is satisfied when the maximum change
          ! in the wave functions component between two diis steps
          ! is less than this threshold
          ! default value is ekin_conv_thr

        REAL(DP) :: diis_delt = 1.0_DP
          ! meaningful only with " electron_dynamics = 'diis' "
          ! electronic time step used in the steepest descendent step
          ! default is "dt"

        INTEGER :: diis_maxstep = 100
          ! meaningful only with " electron_dynamics = 'diis' "
          ! maximum number of iteration in the diis minimization
          ! default is electron_maxstep

        LOGICAL :: diis_rot = .false.
          ! meaningful only with " electron_dynamics = 'diis' "
          ! if "diis_rot = .TRUE." enable diis with charge mixing and rotations
          ! default is "diis_rot = .FALSE."

        REAL(DP) :: diis_fthr = 1.E-3_DP
          ! meaningful only with "electron_dynamics='diis' " and "diis_rot=.TRUE."
          ! convergence threshold for ionic force
          ! this criterion is satisfied when the maximum change
          ! in the atomic force between two diis steps
          ! is less than this threshold
          ! default value is "0.0"

        REAL(DP) :: diis_temp = 0.0_DP
          ! meaningful only with "electron_dynamics='diis' " and "diis_rot=.TRUE."
          ! electronic temperature, significant only if ???

        REAL(DP) :: diis_achmix  = 0.0_DP
          ! meaningful only with "electron_dynamics='diis' " and "diis_rot=.TRUE."
          ! "A" parameter in the charge mixing formula
          ! chmix = A * G^2 / (G^2 + G0^2) , G represents reciprocal lattice vectors

        REAL(DP) :: diis_g0chmix  = 0.0_DP
          ! meaningful only with "electron_dynamics='diis' " and "diis_rot=.TRUE."
          ! "G0^2" parameter in the charge mixing formula

        INTEGER :: diis_nchmix = 0
          ! meaningful only with "electron_dynamics='diis' " and "diis_rot=.TRUE."
          ! dimension of the charge mixing

        REAL(DP) :: diis_g1chmix = 0.0_DP
          ! meaningful only with "electron_dynamics='diis' " and "diis_rot=.TRUE."
          ! "G1^2" parameter in the charge mixing formula
          ! metric = (G^2 + G1^2) / G^2 , G represents reciprocal lattice vectors

        INTEGER :: diis_nrot(3) = 0
          ! meaningful only with "electron_dynamics='diis' " and "diis_rot=.TRUE."
          ! start upgrading the charge density every "diis_nrot(1)" steps,
          ! then every "diis_nrot(2)", and at the end every "diis_nrot(3)",
          ! depending on "diis_rothr"

        REAL(DP) :: diis_rothr(3) = 1.E-4_DP
          ! meaningful only with "electron_dynamics='diis' " and "diis_rot=.TRUE."
          ! threshold on the charge difference between two diis step
          ! when max charge difference is less than "diis_rothr(1)", switch
          ! between the "diis_nrot(1)" upgrade frequency to "diis_nrot(2)",
          ! then when the max charge difference is less than "diis_rothr(2)",
          ! switch between "diis_nrot(2)" and "diis_nrot(3)", upgrade frequency,
          ! finally when the max charge difference is less than "diis_nrot(3)"
          ! convergence is achieved

        REAL(DP) :: diis_ethr = 1.E-4_DP
          ! meaningful only with "electron_dynamics='diis' " and "diis_rot=.TRUE."
          ! convergence threshold for energy
          ! this criterion is satisfied when the change
          ! in the energy between two diis steps
          ! is less than this threshold
          ! default value is etot_conv_thr

        LOGICAL :: diis_chguess = .false.
          ! meaningful only with "electron_dynamics='diis' " and "diis_rot=.TRUE."
          ! if "diis_chguess = .TRUE." enable charge density guess
          ! between two diis step, defaut value is "diis_chguess = .FALSE."

        CHARACTER(len=80) :: mixing_mode = 'default'
          ! type of mixing algorithm for charge self-consistency
          ! used only in PWscf

        REAL(DP) :: mixing_beta = 0.0_DP
          ! parameter for mixing algorithm
          ! used only in PWscf

        INTEGER :: mixing_ndim = 0
          ! dimension of mixing subspace
          ! used only in PWscf

        CHARACTER(len=80) :: diagonalization = 'david'
          ! diagonalization = 'david' or 'cg'
          ! algorithm used by PWscf for iterative diagonalization

        REAL(DP) :: diago_thr_init = 0.0_DP
          ! convergence threshold for the first iterative diagonalization.
          ! used only in PWscf

        INTEGER :: diago_cg_maxiter = 100
          ! max number of iterations for the first iterative diagonalization
          ! using conjugate-gradient algorithm - used only in PWscf

        INTEGER :: diago_david_ndim = 4
          ! dimension of the subspace used in Davidson diagonalization
          ! used only in PWscf

        LOGICAL :: diago_full_acc = .false.

        REAL(DP) :: conv_thr = 1.E-6_DP
          ! convergence threshold in electronic ONLY minimizations
          ! used only in PWscf

        INTEGER :: mixing_fixed_ns  = 0
          ! For DFT+U calculations, PWscf only

        CHARACTER(len=80) :: startingpot = 'potfile'
          ! specify the file containing the DFT potential of the system
          ! used only in PWscf

        INTEGER :: n_inner = 2
          ! number of inner loop per CG iteration.
          ! used only in CP

        INTEGER :: niter_cold_restart = 1
          !frequency of full cold smearing inner cycle (in iterations)

        REAL(DP) :: lambda_cold
         !step for not complete cold smearing inner cycle

        LOGICAL :: tgrand = .false.
          ! whether to do grand-canonical calculations.

        REAL(DP) :: fermi_energy = 0.0_DP
          ! chemical potential of the grand-canonical ensemble.

        CHARACTER(len=80) :: rotation_dynamics = "line-minimization"
          ! evolution the rotational degrees of freedom.

        CHARACTER(len=80) :: occupation_dynamics = "line-minimization"
          ! evolution of the occupational degrees of freedom.

        REAL(DP) :: rotmass = 0
          ! mass for the rotational degrees of freedom.

        REAL(DP) :: occmass = 0
          ! mass for the occupational degrees of freedom.

        REAL(DP) :: occupation_damping = 0
          ! damping for the rotational degrees of freedom.

        REAL(DP) :: rotation_damping = 0
          ! damping for the occupational degrees of freedom.

        LOGICAL :: tcg = .true.
          ! if true perform in cpv conjugate gradient minimization of electron energy

        INTEGER :: maxiter = 100
          ! max number of conjugate gradient iterations

        REAL(DP)  :: etresh =1.0E-7_DP
          ! treshhold on energy

        REAL(DP) :: passop =0.3_DP
          ! small step for parabolic interpolation

        INTEGER :: niter_cg_restart
          !frequency of restart for the conjugate gradient algorithm in iterations

        INTEGER  :: epol = 3
          ! electric field direction

        REAL(DP) :: efield =0.0_DP
          ! electric field intensity in atomic units

          ! real_space routines for US pps
          LOGICAL :: real_space = .false.


        REAL(DP) :: efield_cart(3)
          ! electric field vector in cartesian system of reference

       INTEGER  :: epol2 = 3
          ! electric field direction

        REAL(DP) :: efield2 =0.0_DP
          ! electric field intensity in atomic units

        LOGICAL :: tqr = .false.
          ! US contributions are added in real space

        LOGICAL :: occupation_constraints = .false.
          ! If true perform CP dynamics with constrained occupations
          ! to be used together with penalty functional ...

        NAMELIST / electrons / emass, emass_cutoff, orthogonalization, &
          electron_maxstep, ortho_eps, ortho_max, electron_dynamics,   &
          electron_damping, electron_velocities, electron_temperature, &
          ekincw, fnosee, ampre, grease,                               &
          diis_size, diis_nreset, diis_hcut,                           &
          diis_wthr, diis_delt, diis_maxstep, diis_rot, diis_fthr,     &
          diis_temp, diis_achmix, diis_g0chmix, diis_g1chmix,          &
          diis_nchmix, diis_nrot, diis_rothr, diis_ethr, diis_chguess, &
          mixing_mode, mixing_beta, mixing_ndim, mixing_fixed_ns,      &
          tqr, diago_cg_maxiter, diago_david_ndim, diagonalization ,   &
          startingpot, startingwfc , conv_thr,                         &
          adaptive_thr, conv_thr_init, conv_thr_multi,                 &
          diago_thr_init, n_inner, fermi_energy, rotmass, occmass,     &
          rotation_damping, occupation_damping, rotation_dynamics,     &
          occupation_dynamics, tcg, maxiter, etresh, passop, epol,     &
          efield, epol2, efield2, diago_full_acc,                      &
          occupation_constraints, niter_cg_restart,                    &
          niter_cold_restart, lambda_cold, efield_cart, real_space

!
!=----------------------------------------------------------------------------=!
!  IONS Namelist Input Parameters
!=----------------------------------------------------------------------------=!
!
        CHARACTER(len=80) :: phase_space = 'full'
          ! phase_space = 'full' | 'coarse-grained'
          ! 'full'             the full phase-space is used for the ionic
          !                    dynamics
          ! 'coarse-grained'   a coarse-grained phase-space, defined by a set
          !                    of constraints, is used for the ionic dynamics

!        CHARACTER(len=80) :: phase_space_allowed(2)
!        DATA phase_space_allowed / 'full', 'coarse-grained' /
        CHARACTER(len=80) :: phase_space_allowed(1)
        DATA phase_space_allowed / 'full' /

        CHARACTER(len=80) :: ion_dynamics = 'none'
          ! set how ions should be moved
        CHARACTER(len=80) :: ion_dynamics_allowed(8)
        DATA ion_dynamics_allowed / 'none', 'sd', 'cg', 'langevin', &
                                    'damp', 'verlet', 'bfgs', 'beeman' /

        REAL(DP) :: ion_radius(nsx) = 0.5_DP
          ! pseudo-atomic radius of the i-th atomic species
          ! (for Ewald summation), values between 0.5 and 2.0 are usually used.

        REAL(DP) :: ion_damping = 0.2_DP
          ! meaningful only if " ion_dynamics = 'damp' "
          ! damping frequency times delta t, optimal values could be
          ! calculated with the formula
          !        sqrt(0.5*log((E1-E2)/(E2-E3)))
          ! where E1 E2 E3 are successive values of the DFT total energy
          ! in a ionic steepest descent simulation

        CHARACTER(len=80) :: ion_positions = 'default'
          ! ion_positions = 'default'* | 'from_input'
          ! 'default'    restart the simulation with atomic positions read
          !              from the restart file
          ! 'from_input' restart the simulation with atomic positions read
          !              from standard input ( see the card 'ATOMIC_POSITIONS' )

        CHARACTER(len=80) :: ion_velocities = 'default'
          ! ion_velocities = 'zero' | 'default'* | 'random' | 'from_input'
          ! 'default'    restart the simulation with atomic velocities read
          !              from the restart file
          ! 'random'     start the simulation with random atomic velocities
          ! 'from_input' restart the simulation with atomic velocities read
          !              from standard input (see the card 'ATOMIC_VELOCITIES' )
          ! 'zero'       restart the simulation with atomic velocities set to zero

        CHARACTER(len=80) :: ion_temperature = 'not_controlled'
          ! ion_temperature = 'nose' | 'not_controlled'* | 'rescaling' |
          !    'berendsen' | 'andersen' | 'rescale-v' | 'rescale-T' | 'reduce-T'
          !
          ! 'nose'           control ionic temperature using Nose thermostat
          !                  see parameters "fnosep" and "tempw"
          ! 'rescaling'      control ionic temperature via velocity rescaling
          !                  see parameters "tempw" and "tolp"
          ! 'rescale-v'      control ionic temperature via velocity rescaling
          !                  see parameters "tempw" and "nraise"
          ! 'rescale-T'      control ionic temperature via velocity rescaling
          !                  see parameter "delta_t"
          ! 'reduce-T'       reduce ionic temperature
          !                  see parameters "nraise", delta_t"
          ! 'berendsen'      control ionic temperature using "soft" velocity
          !                  rescaling - see parameters "tempw" and "nraise"
          ! 'andersen'       control ionic temperature using Andersen thermostat
          !                  see parameters "tempw" and "nraise"
          ! 'not_controlled' ionic temperature is not controlled

        REAL(DP) :: tempw = 300.0_DP
          ! meaningful only with "ion_temperature /= 'not_controlled' "
          ! value of the ionic temperature (in Kelvin) forced
          ! by the temperature control

        INTEGER, PARAMETER :: nhclm   = 4
        REAL(DP) :: fnosep( nhclm )  = 50.0_DP
          ! meaningful only with "ion_temperature = 'nose' "
          ! oscillation frequency of the nose thermostat (in terahertz)
          ! nhclm is the max length for the chain; it can be easily increased
          ! since the restart file should be able to handle it
          ! perhaps better to align nhclm by 4

        INTEGER   ::  nhpcl = 0
          ! non-zero only with "ion_temperature = 'nose' "
          ! this defines the length of the Nose-Hoover chain

        INTEGER   :: nhptyp = 0
        ! this parameter set the nose hoover thermostat to more than one

        INTEGER   ::  nhgrp(nsx)=0
          ! this is the array to assign thermostats to atomic types
          ! allows to use various thermostat setups

        INTEGER   ::  ndega = 0
          ! this is the parameter to control active degrees of freedom
          ! used for temperature control and the Nose-Hoover chains

        REAL(DP) :: tolp = 50.0_DP
          ! meaningful only with "ion_temperature = 'rescaling' "
          ! tolerance (in Kelvin) of the rescaling. When ionic temperature
          ! differs from "tempw" more than "tolp" apply rescaling.

        REAL(DP)  ::  fnhscl(nsx)=-1.0_DP
        ! this is to scale the target energy, in case there are constraints
        ! the dimension is the same as nhgrp, meaning that atomic type
        ! i with a group nhgrp(i) is scaled by fnhscl(i)

        LOGICAL   :: tranp(nsx) = .false.
          ! tranp(i) control the randomization of the i-th atomic specie
          ! .TRUE.   randomize ionic positions ( see "amprp" )
          ! .FALSE.  do nothing

        REAL(DP) :: amprp(nsx) = 0.0_DP
          ! amprp(i) meaningful only if "tranp(i) = .TRUE.", amplitude of the
          ! randomization ( allowed values: 0.0 - 1.0 ) for the i-th atomic specie.
          ! Add to the positions a random displacements vector ( in bohr radius )
          ! defined as:  amprp( i ) * ( X, Y, Z )
          ! where X, Y, Z are pseudo random number in the interval [ -0.5 , 0.5 ]

        REAL(DP) :: greasp = 0.0_DP
          ! same as "grease", for ionic damped dynamics
          ! NOT used in FPMD

        INTEGER   :: ion_nstepe = 1
          ! number of electronic steps for each ionic step

        INTEGER   :: ion_maxstep = 1000
          ! maximum number of step in ionic minimization

        REAL(DP) :: upscale = 100.0_DP
          ! Max reduction allowed in scf threshold during optimization

        CHARACTER(len=80) :: pot_extrapolation = 'default', &
                             wfc_extrapolation = 'default'
          !  These variables are used only by PWSCF

        LOGICAL :: refold_pos
        LOGICAL :: remove_rigid_rot = .false.

        !
        ! ... delta_T, nraise, tolp are used to change temperature in PWscf
        !

        REAL(DP) :: delta_t = 1.0_DP

        INTEGER :: nraise = 1

        !
        ! ... variables added for new BFGS algorithm
        !

        INTEGER ::  bfgs_ndim = 1

        REAL(DP)  :: trust_radius_max = 0.8_DP
        REAL(DP)  :: trust_radius_min = 1.E-3_DP
        REAL(DP)  :: trust_radius_ini = 0.5_DP

        REAL(DP)  :: w_1 = 0.5E-1_DP
        REAL(DP)  :: w_2 = 0.5_DP

        REAL(DP)  :: sic_rloc = 0.0_DP

        !
        ! ... variable for meta-dynamics
        !
        INTEGER, PARAMETER :: max_nconstr = 100
        INTEGER  :: fe_nstep = 100
        INTEGER  :: sw_nstep = 10
        INTEGER  :: eq_nstep = 0
        REAL(DP) :: g_amplitude = 0.005_DP
        !
        REAL(DP) :: fe_step( max_nconstr ) = 0.4_DP
        !
        NAMELIST / ions / phase_space, ion_dynamics, ion_radius, ion_damping,  &
                          ion_positions, ion_velocities, ion_temperature,      &
                          tempw, fnosep, nhgrp, fnhscl, nhpcl, nhptyp, ndega, tranp,   &
                          amprp, greasp, tolp, ion_nstepe, ion_maxstep,        &
                          refold_pos, upscale, delta_t, pot_extrapolation,     &
                          wfc_extrapolation, nraise, remove_rigid_rot,         &
                          trust_radius_max, trust_radius_min,                  &
                          trust_radius_ini, w_1, w_2, bfgs_ndim, sic_rloc,     &
                          fe_step, fe_nstep, sw_nstep, eq_nstep, g_amplitude


!=----------------------------------------------------------------------------=!
!  CELL Namelist Input Parameters
!=----------------------------------------------------------------------------=!
!
        CHARACTER(len=80) :: cell_parameters = 'default'
          ! cell_parameters = 'default'* | 'from_input'
          ! 'default'    restart the simulation with cell parameters read
          !              from the restart file or "celldm" if
          !              "restart = 'from_scratch'"
          ! 'from_input' restart the simulation with cell parameters
          !              from standard input ( see the card 'CELL_PARAMETERS' )

        CHARACTER(len=80) :: cell_dynamics  = 'none'
          ! set how the cell should be moved
        CHARACTER(len=80) :: cell_dynamics_allowed(7)
        DATA cell_dynamics_allowed / 'sd', 'pr', 'none', 'w', 'damp-pr', &
                                     'damp-w', 'bfgs'  /

        CHARACTER(len=80) :: cell_velocities = 'default'
          ! cell_velocities = 'zero' | 'default'*
          ! 'zero'    restart setting cell velocitiy to zero
          ! 'default' restart using cell velocity of the previous run

        REAL(DP) :: press = 0.0_DP
          ! external pressure (in GPa, remember 1 kbar = 10^8 Pa)

        REAL(DP) :: wmass = 0.0_DP
          ! effective cell mass in the Parrinello-Rahman Lagrangian (in atomic units)
          ! of the order of magnitude of the total atomic mass
          ! (sum of the mass of the atoms) within the simulation cell.
          ! if you do not specify this parameters, the code will compute
          ! its value based on some physical consideration

        CHARACTER(len=80) :: cell_temperature  = 'not_controlled'
          ! cell_temperature = 'nose' | 'not_controlled'* | 'rescaling'
          ! 'nose'           control cell temperature using Nose thermostat
          !                  see parameters "fnoseh" and "temph"
          ! 'rescaling'      control cell temperature via velocities rescaling
          ! 'not_controlled' cell temperature is not controlled
          ! NOT used in FPMD

        REAL(DP) :: temph = 0.0_DP
          ! meaningful only with "cell_temperature /= 'not_controlled' "
          ! value of the cell temperature (in Kelvin) forced
          ! by the temperature control

        REAL(DP) :: fnoseh = 1.0_DP
          ! meaningful only with "cell_temperature = 'nose' "
          ! oscillation frequency of the nose thermostat (in terahertz)

        REAL(DP) :: greash = 0.0_DP
          ! same as "grease", for cell damped dynamics

        CHARACTER(len=80) :: cell_dofree = 'all'
          ! cell_dofree = 'all'* | 'volume' | 'x' | 'y' | 'z' | 'xy' | 'xz' | 'yz' | 'xyz'
          ! select which of the cell parameters should be moved
          ! 'all'    all axis and angles are propagated (default)
          ! 'volume' the cell is simply rescaled, without changing the shape
          ! 'x'      only the "x" axis is moved
          ! 'y'      only the "y" axis is moved
          ! 'z'      only the "z" axis is moved
          ! 'xy'     only the "x" and "y" axis are moved, angles are unchanged
          ! 'xz'     only the "x" and "z" axis are moved, angles are unchanged
          ! 'yz'     only the "y" and "z" axis are moved, angles are unchanged
          ! 'xyz'    "x", "y" and "z" axis are moved, angles are unchanged

        REAL(DP) :: cell_factor = 0.0_DP
          ! NOT used in FPMD

        INTEGER   :: cell_nstepe = 1
          ! number of electronic steps for each cell step

        REAL(DP) :: cell_damping = 0.0_DP
          ! meaningful only if " cell_dynamics = 'damp' "
          ! damping frequency times delta t, optimal values could be
          ! calculated with the formula
          !        sqrt(0.5*log((E1-E2)/(E2-E3)))
          ! where E1 E2 E3 are successive values of the DFT total energy
          ! in a ionic steepest descent simulation

        REAL(DP) :: press_conv_thr = 0.5_DP

        NAMELIST / cell / cell_parameters, cell_dynamics, cell_velocities, &
                          press, wmass, cell_temperature, temph, fnoseh,   &
                          cell_dofree, greash, cell_factor, cell_nstepe,   &
                          cell_damping, press_conv_thr

!
!=----------------------------------------------------------------------------=!!
! PRESS_AI Namelist Input Parameters
!=----------------------------------------------------------------------------=!
!
!
      LOGICAL  :: abivol = .false.
      LOGICAL  :: abisur = .false.
      LOGICAL  :: pvar   = .false.
      LOGICAL  :: fill_vac=.false.
      LOGICAL  :: scale_at=.false.
      LOGICAL  :: t_gauss =.false.
      LOGICAL  :: jellium= .false.
      LOGICAL  :: cntr(nsx)=.false.
      REAL(DP) :: P_ext = 0.0_DP
      REAL(DP) :: P_in  = 0.0_DP
      REAL(DP) :: P_fin = 0.0_DP
      REAL(DP) :: rho_thr = 0.0_DP
      REAL(DP) :: step_rad(nsx) = 0.0_DP
      REAL(DP) :: Surf_t = 0.0_DP
      REAL(DP) :: dthr = 0.0_DP
      REAL(DP) :: R_j = 0.0_DP
      REAL(DP) :: h_j = 0.0_DP
      REAL(DP) :: delta_eps = 0.0_DP
      REAL(DP) :: delta_sigma=0.0_DP
      INTEGER  :: n_cntr = 0
      INTEGER  :: axis = 0

      NAMELIST / press_ai / abivol, P_ext, pvar, P_in, P_fin, rho_thr,  &
     &                      step_rad, delta_eps, delta_sigma, n_cntr,   &
     &                      fill_vac, scale_at, t_gauss, abisur,        &
     &                      Surf_t, dthr, cntr, axis, jellium, R_j, h_j

!===============================================================================
!  END manual
! ----------------------------------------------------------------------


!  END manual
! ----------------------------------------------------------------------


! ----------------------------------------------------------------
! BEGIN manual
!
!=----------------------------------------------------------------------------=!
!  CARDS parameters
!=----------------------------------------------------------------------------=!
!
!  Note: See file read_cards.f90 for card syntax and usage
!
!    ATOMIC_SPECIES
!
        CHARACTER(len=3)  :: atom_label(nsx) = 'XX'   ! label of the atomic species being read
        CHARACTER(len=80) :: atom_pfile(nsx) = 'YY'   ! pseudopotential file name
        REAL(DP)          :: atom_mass(nsx)  = 0.0_DP ! atomic mass of the i-th atomic species
          ! in atomic mass units: 1 a.m.u. = 1822.9 a.u. = 1.6605 * 10^-27 kg
        LOGICAL   :: taspc = .false.
        LOGICAL   :: tkpoints = .false.
        LOGICAL   :: tforces = .false.
        LOGICAL   :: tocc = .false.
        LOGICAL   :: tcell = .false.
        LOGICAL   :: tdipole = .false.
        LOGICAL   :: tionvel = .false.
        LOGICAL   :: tconstr = .false.
        LOGICAL   :: tesr = .false.
        LOGICAL   :: tksout = .false.
        LOGICAL   :: ttemplate = .false.

!
!    ATOMIC_POSITIONS
!
        REAL(DP), ALLOCATABLE :: rd_pos(:,:)  ! unsorted positions from input
        INTEGER,  ALLOCATABLE :: sp_pos(:)
        INTEGER,  ALLOCATABLE :: if_pos(:,:)
        INTEGER,  ALLOCATABLE :: id_loc(:)
        INTEGER,  ALLOCATABLE :: na_inp(:)
        LOGICAL  :: tapos = .false.
        CHARACTER(len=80) :: atomic_positions = 'crystal'
          ! atomic_positions = 'bohr' | 'angstrong' | 'crystal' | 'alat'
          ! select the units for the atomic positions being read from stdin

        !
        ! ... variable added for NEB  ( C.S. 17/10/2003 )
        !
        !
!
!    ION_VELOCITIES
!
        REAL(DP), ALLOCATABLE :: rd_vel(:,:)   ! unsorted velocities from input
        INTEGER,  ALLOCATABLE :: sp_vel(:)
        LOGICAL  :: tavel          = .false.
!
!    ATOMIC_FORCES
!
        REAL(DP), ALLOCATABLE :: rd_for(:,:)  ! external forces applied to single atoms

!
!    KPOINTS
!
! ...   k-points inputs
        LOGICAL :: tk_inp = .false.
        REAL(DP), ALLOCATABLE :: xk(:,:), wk(:)
        INTEGER :: nkstot = 0, nk1 = 0, nk2 = 0, nk3 = 0, k1 = 0, k2 = 0, k3 = 0
        CHARACTER(len=80) :: k_points = 'gamma'
          ! k_points = 'automatic' | 'crystal' | 'tpiba' | 'gamma'*
          ! k_points = 'crystal_b' | 'tpiba_b'
          ! select the k points mesh
          ! 'automatic'  k points mesh is generated automatically
          !              with Monkhorst-Pack algorithm
          ! 'crystal'    k points mesh is given in stdin in scaled units
          ! 'tpiba'      k points mesh is given in stdin in units of ( 2 PI / alat )
          ! 'gamma'      only gamma point is used ( default in CPMD simulation )
          ! _b means that a band input is given. The weights is a integer
          !  number that gives the number of points between the present point
          !  and the next. The weight of the last point is not used.
!
!    OCCUPATIONS
!
        REAL(DP), ALLOCATABLE :: f_inp(:,:)
        LOGICAL   :: tf_inp = .false.

!
!    DIPOLE
!
        LOGICAL :: tdipole_card = .false.

!
!    ESR
!
       INTEGER :: iesr_inp = 1

!
!    CELL_PARAMETERS
!
       REAL(DP) :: rd_ht(3,3) = 0.0_DP
       CHARACTER(len=80) :: cell_units = 'alat'
       LOGICAL   :: trd_ht = .false.

!
!    CONSTRAINTS
!
      INTEGER :: nc_fields = 4   ! max number of fields that is allowed to
                                 ! define a constraint

      INTEGER  :: nconstr_inp    = 0
      REAL(DP) :: constr_tol_inp = 1.E-6_DP
      !
      CHARACTER(len=20), ALLOCATABLE :: constr_type_inp(:)
      REAL(DP),          ALLOCATABLE :: constr_inp(:,:)
      REAL(DP),          ALLOCATABLE :: constr_target_inp(:)
      LOGICAL,           ALLOCATABLE :: constr_target_set(:)

!
!    KOHN_SHAM
!
      INTEGER, ALLOCATABLE :: iprnks( :, : )
      INTEGER :: nprnks( nspinx ) = 0
        ! logical mask used to specify which kohn sham orbital should be
        ! written to files 'KS.'

!
!   CLIMBING_IMAGES
!
      !
      ! ... variable added for NEB  ( C.S. 20/11/2003 )
      !
      LOGICAL, ALLOCATABLE :: climbing( : )

!
!   PLOT_WANNIER
!


!  END manual
! ----------------------------------------------------------------------

      LOGICAL :: xmloutput = .false.
      ! if .true. PW produce an xml output
CONTAINS

  SUBROUTINE allocate_input_ions( ntyp, nat )
    !
    INTEGER, INTENT(in) :: ntyp, nat
    !
    IF ( allocated( rd_pos ) ) DEALLOCATE( rd_pos )
    IF ( allocated( sp_pos ) ) DEALLOCATE( sp_pos )
    IF ( allocated( if_pos ) ) DEALLOCATE( if_pos )
    IF ( allocated( id_loc ) ) DEALLOCATE( id_loc )
    IF ( allocated( na_inp ) ) DEALLOCATE( na_inp )
    IF ( allocated( rd_vel ) ) DEALLOCATE( rd_vel )
    IF ( allocated( sp_vel ) ) DEALLOCATE( sp_vel )
    IF ( allocated( rd_for ) ) DEALLOCATE( rd_for )
    !
    ALLOCATE( rd_pos( 3, nat ) )
    ALLOCATE( sp_pos( nat)   )
    ALLOCATE( if_pos( 3, nat ) )
    ALLOCATE( id_loc( nat)   )
    ALLOCATE( na_inp( ntyp)  )
    ALLOCATE( rd_vel( 3, nat ) )
    ALLOCATE( sp_vel( nat)   )
    ALLOCATE( rd_for( 3, nat ) )
    !
    rd_pos = 0.0_DP
    sp_pos = 0
    if_pos = 1
    id_loc = 0
    na_inp = 0
    rd_vel = 0.0_DP
    sp_vel = 0
    rd_for = 0.0_DP
    !
    RETURN
    !
  END SUBROUTINE allocate_input_ions

  SUBROUTINE allocate_input_constr()
    !
    IF ( allocated( constr_type_inp ) )   DEALLOCATE( constr_type_inp )
    IF ( allocated( constr_inp ) )        DEALLOCATE( constr_inp )
    IF ( allocated( constr_target_inp ) ) DEALLOCATE( constr_target_inp )
    IF ( allocated( constr_target_set ) ) DEALLOCATE( constr_target_set )
    !
    ALLOCATE( constr_type_inp(   nconstr_inp ) )
    ALLOCATE( constr_target_inp( nconstr_inp ) )
    ALLOCATE( constr_target_set( nconstr_inp ) )
    !
    ALLOCATE( constr_inp( nc_fields, nconstr_inp ) )
    !
    constr_type_inp   = ' '
    constr_inp        = 0.0_DP
    constr_target_inp = 0.0_DP
    constr_target_set = .false.
    !
    RETURN
    !
  END SUBROUTINE allocate_input_constr

  SUBROUTINE allocate_input_iprnks( nksx, nspin )
    !
    INTEGER, INTENT(in) :: nksx, nspin
    !
    IF( allocated( iprnks ) ) DEALLOCATE( iprnks )
    !
    ALLOCATE( iprnks( max( 1, nksx), nspin ) )
    !
    iprnks = 0
    !
    RETURN
    !
  END SUBROUTINE allocate_input_iprnks


  SUBROUTINE deallocate_input_parameters()
    !
    IF ( allocated( xk ) ) DEALLOCATE( xk )
    IF ( allocated( wk ) ) DEALLOCATE( wk )
    IF ( allocated( rd_pos ) ) DEALLOCATE( rd_pos )
    IF ( allocated( sp_pos ) ) DEALLOCATE( sp_pos )
    IF ( allocated( if_pos ) ) DEALLOCATE( if_pos )
    IF ( allocated( id_loc ) ) DEALLOCATE( id_loc )
    IF ( allocated( na_inp ) ) DEALLOCATE( na_inp )
    IF ( allocated( rd_vel ) ) DEALLOCATE( rd_vel )
    IF ( allocated( sp_vel ) ) DEALLOCATE( sp_vel )
    IF ( allocated( rd_for ) ) DEALLOCATE( rd_for )
    !
    !
    IF ( allocated( constr_type_inp ) )   DEALLOCATE( constr_type_inp )
    IF ( allocated( constr_inp ) )        DEALLOCATE( constr_inp )
    IF ( allocated( constr_target_inp ) ) DEALLOCATE( constr_target_inp )
    IF ( allocated( constr_target_set ) ) DEALLOCATE( constr_target_set )
    !
    IF ( allocated( iprnks ) )       DEALLOCATE( iprnks )
    !
    RETURN
    !
  END SUBROUTINE deallocate_input_parameters
  !
!=----------------------------------------------------------------------------=!
!
END MODULE input_parameters
!
!=----------------------------------------------------------------------------=!
