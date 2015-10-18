!
! Copyright (C) 2001-2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE setup()
  !----------------------------------------------------------------------------
  !
  ! ... This routine is called at the beginning of the calculation and
  ! ... 1) determines various parameters of the calculation:
  ! ...    zv        charge of each atomic type
  ! ...    nelec     total number of electrons (if not given in input)
  ! ...    nbnd      total number of bands (if not given in input)
  ! ...    nbndx     max number of bands used in iterative diagonalization
  ! ...    tpiba     2 pi / a (a = lattice parameter)
  ! ...    tpiba2    square of tpiba
  ! ...    gcutm     cut-off in g space for charge/potentials
  ! ...    gcutms    cut-off in g space for smooth charge
  ! ...    ethr      convergence threshold for iterative diagonalization
  ! ... 2) finds actual crystal symmetry:
  ! ...    s         symmetry matrices in the direct lattice vectors basis
  ! ...    nsym      number of crystal symmetry operations
  ! ...    nrot      number of lattice symmetry operations
  ! ...    ft        fractionary translations
  ! ...    irt       for each atom gives the corresponding symmetric
  ! ...    invsym    if true the system has inversion symmetry
  ! ... 3) generates k-points corresponding to the actual crystal symmetry
  ! ... 4) calculates various quantities used in magnetic, spin-orbit, PAW
  ! ...    electric-field, LDA+U calculations, and for parallelism
  !
  USE kinds,              ONLY : DP
  USE constants,          ONLY : eps8, rytoev 
  USE parameters,         ONLY : npk
  USE io_global,          ONLY : stdout
  USE io_files,           ONLY : tmp_dir, prefix, xmlpun, delete_if_present
  USE constants,          ONLY : pi, degspin
  USE cell_base,          ONLY : at, bg, alat, tpiba, tpiba2, ibrav, omega
  USE ions_base,          ONLY : nat, tau, ntyp => nsp, ityp, zv
  USE basis,              ONLY : starting_pot, natomwfc
  USE gvect,              ONLY : gcutm
  USE fft_base,           ONLY : dfftp
  USE fft_base,           ONLY : dffts
  USE grid_subroutines,   ONLY : realspace_grids_init
  USE gvecs,              ONLY : doublegrid, gcutms, dual
  USE klist,              ONLY : xk, wk, nks, nelec, degauss, lgauss, &
                                 lxkcry, nkstot, &
                                 nelup, neldw, two_fermi_energies, &
                                 tot_charge, tot_magnetization
  USE lsda_mod,           ONLY : lsda, nspin, current_spin, isk, &
                                 starting_magnetization
  USE ener,               ONLY : ef
  USE electrons_base,     ONLY : set_nelup_neldw
  USE start_k,            ONLY : nks_start, xk_start, wk_start, &
                                 nk1, nk2, nk3, k1, k2, k3
  USE ktetra,             ONLY : tetra, ntetra, ltetra
  USE symm_base,          ONLY : s, t_rev, irt, nrot, nsym, invsym, nosym, &
                                 d1,d2,d3, time_reversal, sname, set_sym_bl, &
                                 find_sym, inverse_s, no_t_rev
  USE wvfct,              ONLY : nbnd, nbndx, ecutwfc
  USE control_flags,      ONLY : tr2, ethr, lscf, lmd, david, lecrpa,  &
                                 isolve, niter, noinv, &
                                 lbands
  USE uspp_param,         ONLY : upf, n_atom_wfc
  USE fixed_occ,          ONLY : f_inp, tfixed_occ, one_atom_occupations
  USE funct,              ONLY : set_dft_from_name
  USE mp_global,          ONLY : kunit
  USE spin_orb,           ONLY : lspinorb, domag
  USE funct,              ONLY : dft_is_gradient
  !
  IMPLICIT NONE
  !
  integer, parameter :: nspin_lsda=1 !substitute for noncollin_module%nspin_lsda
  integer, parameter :: npol=1 !substitute for noncollin_module%npol
  real(dp),parameter :: m_loc(3,1)=0.D0 !dummy; substitute for noncollin%m_loc
  logical  :: use_para_diag !dummy; substitute for control_flags%use_para_diag
  INTEGER  :: na, nt, is, ierr, ibnd, ik
  LOGICAL  :: magnetic_sym, skip_equivalence=.FALSE.
  REAL(DP) :: iocc, ionic_charge, one
  !
  LOGICAL, EXTERNAL  :: check_para_diag
  INTEGER, EXTERNAL :: set_Hubbard_l
  !
  ! ... Compute the ionic charge for each atom type and the total ionic charge
  !
  zv(1:ntyp) = upf(1:ntyp)%zp
  !
     ionic_charge = SUM( zv(ityp(1:nat)) ) 
  !
  ! ... set the number of electrons 
  !
  nelec = ionic_charge - tot_charge
  !
  ! ... magnetism-related quantities
  !
  ! time reversal operation is set up to 0 by default
  t_rev = 0
     !
     ! ... wavefunctions are scalars
     !
     IF (lspinorb)  CALL errore( 'setup ',  &
         'spin orbit requires a non collinear calculation', 1 )
     !
     !
  !
  ! ... If the occupations are from input, check the consistency with the
  ! ... number of electrons
  !
  IF ( tfixed_occ ) THEN
     !
     iocc = 0
     !
     DO is = 1, nspin_lsda
        !
        iocc = iocc + SUM( f_inp(1:nbnd,is) )
        !
        DO ibnd = 1, nbnd
           if (f_inp(ibnd,is) > 2.d0/nspin_lsda .or. f_inp(ibnd,is) < 0.d0) &
              call errore('setup','wrong fixed occupations',is)
        END DO
     END DO
     !
     IF ( ABS( iocc - nelec ) > 1D-5 ) &
        CALL errore( 'setup', 'strange occupations: '//&
                     'number of electrons from occupations is wrong.', 1 )
     !
  END IF
  !
  ! ... Check: if there is an odd number of electrons, the crystal is a metal
  !
  IF ( lscf .AND. ABS( NINT( nelec / 2.D0 ) - nelec / 2.D0 ) > eps8 &
            .AND. .NOT. lgauss .AND. .NOT. ltetra .AND. .NOT. tfixed_occ ) &
      CALL infomsg( 'setup', 'the system is metallic, specify occupations' )
  !
  ! ... Check: spin-polarized calculations require either broadening or
  !             fixed occupation
  !
  IF ( lscf .AND. lsda &
            .AND. .NOT. lgauss .AND. .NOT. ltetra &
            .AND. .NOT. tfixed_occ .AND. .NOT. two_fermi_energies ) &
      CALL errore( 'setup', 'spin-polarized system, specify occupations', 1 )
  !
  ! ... setting nelup/neldw 
  !
  call set_nelup_neldw ( tot_magnetization, nelec, nelup, neldw )
  !
  ! ... Set the number of occupied bands if not given in input
  !
  IF ( nbnd == 0 ) THEN
     !
     IF (nat==0) CALL errore('setup','free electrons: nbnd required in input',1)
     !
     nbnd = MAX ( NINT( nelec / degspin ), NINT(nelup), NINT(neldw) )
     !
     IF ( lgauss .OR. ltetra ) THEN
        !
        ! ... metallic case: add 20% more bands, with a minimum of 4
        !
        nbnd = MAX( NINT( 1.2D0 * nelec / degspin ), &
                    NINT( 1.2D0 * nelup), NINT( 1.2d0 * neldw ), &
                    ( nbnd + 4 ) )
        !
     END IF
     !
     ! ... In the case of noncollinear magnetism, bands are NOT
     ! ... twofold degenerate :
     !
     !
  ELSE
     !
     IF ( nbnd < NINT( nelec / degspin ) .AND. lscf ) &
        CALL errore( 'setup', 'too few bands', 1 )
     !
     IF ( nbnd < NINT( nelup ) .AND. lscf ) &
        CALL errore( 'setup', 'too few spin up bands', 1 )
     IF ( nbnd < NINT( neldw ) .AND. lscf ) &
        CALL errore( 'setup', 'too few spin dw bands', 1 )
     !
     !
  END IF
  !
  ! ... Here we  set the precision of the diagonalization for the first scf
  ! ... iteration of for the first ionic step
  ! ... for subsequent steps ethr is automatically updated in electrons
  !
  IF ( nat==0 ) THEN
     ethr=1.0D-8
  ELSE IF ( .NOT. lscf ) THEN
     !
     IF ( ethr == 0.D0 ) ethr = 0.1D0 * MIN( 1.D-2, tr2 / nelec )
     !
  ELSE
     !
     IF ( ethr == 0.D0 ) THEN
        !
        IF ( starting_pot == 'file' ) THEN
           !
           ! ... if you think that the starting potential is good
           ! ... do not spoil it with a lousy first diagonalization :
           ! ... set a strict ethr in the input file (diago_thr_init)
           !
           ethr = 1.D-5
           !
        ELSE
           !
           ! ... starting atomic potential is probably far from scf
           ! ... do not waste iterations in the first diagonalizations
           !
           ethr = 1.0D-2
           !
        END IF
        !
     END IF
     !
  END IF
  !
  IF ( .NOT. lscf ) niter = 1
  !
  ! ... set number of atomic wavefunctions
  !
  natomwfc = n_atom_wfc( nat, ityp, .false. )
  !
  ! ... set the max number of bands used in iterative diagonalization
  !
  nbndx = nbnd
  IF ( isolve == 0 ) nbndx = david * nbnd
  !
  use_para_diag = check_para_diag( nbnd )
  ! ... Set the units in real and reciprocal space
  !
  tpiba  = 2.D0 * pi / alat
  tpiba2 = tpiba**2
  !
  ! ... Compute the cut-off of the G vectors
  !
  doublegrid = ( dual > 4.D0 )
  IF ( doublegrid ) &
     CALL infomsg ( 'setup', 'no reason to have ecutrho>4*ecutwfc' )
  gcutm = dual * ecutwfc / tpiba2
  !
  IF ( doublegrid ) THEN
     !
     gcutms = 4.D0 * ecutwfc / tpiba2
     !
  ELSE
     !
     gcutms = gcutm
     !
  END IF
  !
  ! ... Test that atoms do not overlap
  !
  call check_atoms ( nat, tau, bg )
  !
  ! ... calculate dimensions of the FFT grid
  !
  CALL realspace_grids_init ( dfftp, dffts, at, bg, gcutm, gcutms )
  !
  !  ... generate transformation matrices for the crystal point group
  !  ... First we generate all the symmetry matrices of the Bravais lattice
  !
  call set_sym_bl ( )
  !
  ! ... If lecrpa is true, nosym must be set to true also
  !
  IF ( lecrpa ) nosym = .TRUE.
  IF ( lecrpa ) skip_equivalence=.TRUE.
  !
  ! ... If nosym is true do not use any point-group symmetry
  !
  IF ( nosym ) nrot = 1
  !
  ! ... time_reversal = use q=>-q symmetry for k-point generation
  !
  magnetic_sym = .false. 
  time_reversal = .NOT. noinv .AND. .NOT. magnetic_sym
  !
  ! ... Automatic generation of k-points (if required)
  !
  IF ( nks_start == 0 ) THEN
     !
        !
        CALL kpoint_grid ( nrot, time_reversal, skip_equivalence, s, t_rev, bg,&
                           npk, k1,k2,k3, nk1,nk2,nk3, nkstot, xk, wk)
        !
     !
  ELSE 
     nkstot = nks_start
     xk(:,1:nkstot) = xk_start(:,1:nks_start)
     wk(1:nkstot) = wk_start(1:nks_start)
     !
  END IF
  !
  IF ( nat==0 ) THEN
     !
     nsym=nrot
     invsym=.true.
     CALL inverse_s ( ) 
     !
  ELSE
     !
     ! ... eliminate rotations that are not symmetry operations
     !
     CALL find_sym ( nat, tau, ityp, dfftp%nr1, dfftp%nr2, dfftp%nr3, &
                  magnetic_sym, m_loc )
     !
  END IF
  !
  ! ... Input k-points are assumed to be  given in the IBZ of the Bravais
  ! ... lattice, with the full point symmetry of the lattice.
  ! ... If some symmetries of the lattice are missing in the crystal,
  ! ... "irreducible_BZ" computes the missing k-points.
  !
  IF ( .NOT. lbands ) THEN
     CALL irreducible_BZ (nrot, s, nsym, time_reversal, &
                          magnetic_sym, at, bg, npk, nkstot, xk, wk, t_rev)
  ELSE
     one = SUM (wk(1:nkstot))
     IF ( one > 0.0_dp ) wk(1:nkstot) = wk(1:nkstot) / one
  END IF
  !
  ntetra = 0
  !
  IF ( lbands ) THEN
     !
     ! ... if calculating bands, we read the Fermi energy
     !
     write(*,*)"setup.f90:496 skipping pw_readfile, requires iotk"
     !CALL pw_readfile( 'reset', ierr )
     !CALL pw_readfile( 'ef',   ierr )
     CALL errore( 'setup ', 'problem reading ef from file ' // &
             & TRIM( tmp_dir ) // TRIM( prefix ) // '.save', ierr )

     !
  ELSE IF ( ltetra ) THEN
     !
     ! ... Calculate quantities used in tetrahedra method
     !
     ntetra = 6 * nk1 * nk2 * nk3
     !
     ALLOCATE( tetra( 4, ntetra ) )
     !
     CALL tetrahedra( nsym, s, time_reversal, t_rev, at, bg, npk, k1, k2, k3, &
          nk1, nk2, nk3, nkstot, xk, wk, ntetra, tetra )
     !
  END IF
  !
  !
  IF ( lsda ) THEN
     !
     ! ... LSDA case: two different spin polarizations,
     ! ...            each with its own kpoints
     !
     if (nspin /= 2) call errore ('setup','nspin should be 2; check iosys',1)
     !
     CALL set_kup_and_kdw( xk, wk, isk, nkstot, npk )
     !
  ELSE
     !
     ! ... LDA case: the two spin polarizations are identical
     !
     wk(1:nkstot)    = wk(1:nkstot) * degspin
     current_spin = 1
     isk(:) = 1
     !
     IF ( nspin /= 1 ) &
        CALL errore( 'setup', 'nspin should be 1; check iosys', 1 )
     !
  END IF
  !
  IF ( nkstot > npk ) CALL errore( 'setup', 'too many k points', nkstot )
  !
  !
  !
  ! ... distribute k-points (and their weights and spin indices)
  !
  kunit = 1
  CALL divide_et_impera( xk, wk, isk, lsda, nkstot, nks )
  !
  IF (one_atom_occupations) THEN
     DO ik=1,nkstot
        DO ibnd=natomwfc+1, nbnd
           IF (f_inp(ibnd,ik)> 0.0_DP) CALL errore('setup', &
               'no atomic wavefunction for some band',1)
        ENDDO
     ENDDO
  ENDIF

  !
  RETURN
  !
END SUBROUTINE setup
!
!----------------------------------------------------------------------------
LOGICAL FUNCTION check_para_diag( nbnd )
  !
  USE io_global,        ONLY : stdout, ionode, ionode_id
  USE mp_global,        ONLY : np_ortho

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: nbnd
  LOGICAL, SAVE :: first = .TRUE.

  IF( .NOT. first ) RETURN
  first = .FALSE.
  !
  IF( np_ortho(1) > nbnd ) &
     CALL errore ('check_para_diag', 'Too few bands for required ndiag',nbnd)
  !
  check_para_diag = .true.
  !
  IF ( ionode ) THEN
     !
     WRITE( stdout, '(/,5X,"Subspace diagonalization in iterative solution ",&
                     &     "of the eigenvalue problem:")' ) 
     IF ( check_para_diag ) THEN
        WRITE( stdout, '(5X,"scalapack distributed-memory algorithm ", &
              & "(size of sub-group: ", I2, "*", I3, " procs)",/)') &
               np_ortho(1), np_ortho(2)
     ELSE
        WRITE( stdout, '(5X,"a serial algorithm will be used",/)' )
     END IF
     !
  END IF
  !
  RETURN
END FUNCTION check_para_diag
