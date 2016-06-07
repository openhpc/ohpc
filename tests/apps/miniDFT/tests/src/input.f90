
! Copyright (C) 2002-2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE iosys()
  !-----------------------------------------------------------------------------
  !
  ! ...  this subroutine reads input data from standard input ( unit 5 )
  ! ...  Use "-input filename" to read input from file "filename":
  ! ...  may be useful if you have trouble reading from standard input
  ! ...  ---------------------------------------------------------------
  !
  ! ...  access the modules renaming the variables that have the same name
  ! ...  as the input parameters, this is required in order to use a code
  ! ...  independent input parser
  !
  !
  USE kinds,         ONLY : DP
  USE control_flags, ONLY: adapt_thr, tr2_init, tr2_multi
  USE constants,     ONLY : autoev, eV_to_kelvin, pi, rytoev, &
                            ry_kbar, amconv, bohr_radius_angs, eps8
  USE mp_global,     ONLY : npool, nproc_pool
  !
  USE io_global,     ONLY : stdout, ionode, ionode_id
  !
  !
  USE mp,            ONLY : mp_bcast
  !
  !
  USE cell_base,     ONLY : at, alat, omega, &
                            cell_base_init, init_dofree
  !
  USE ions_base,     ONLY : if_pos, ityp, tau, extfor, &
                            ntyp_ => nsp, &
                            nat_  => nat, &
                            amass, tau_format
  !
  USE basis,         ONLY : startingconfig, starting_wfc, starting_pot
  !
  USE run_info,      ONLY : title_ => title
  !
  !
  USE extfield,      ONLY : tefield_  => tefield, &
                            dipfield_ => dipfield, &
                            edir_     => edir, &
                            emaxpos_  => emaxpos, &
                            eopreg_   => eopreg, &
                            eamp_     => eamp, &
                            forcefield
  !
  USE io_files,      ONLY : input_drho, output_drho, &
                            psfile, tmp_dir, wfc_dir, &
                            prefix_     => prefix, &
                            pseudo_dir_ => pseudo_dir
  !
  USE force_mod,     ONLY : lforce, lstres, force
  !
  USE gvecs,         ONLY : dual
  USE gvect,         ONLY : ecutrho_ => ecutrho
  !
  USE fft_base, ONLY : dfftp
  USE fft_base, ONLY : dffts
  !
  USE klist,         ONLY : lgauss, ngauss, two_fermi_energies, &
                            smearing_          => smearing, &
                            degauss_           => degauss, &
                            tot_charge_        => tot_charge, &
                            tot_magnetization_ => tot_magnetization
  !
  USE ktetra,        ONLY : ltetra
  USE start_k,       ONLY : init_start_k
  !
  !
  USE a2F,           ONLY : la2F_ => la2F
  !
  !
  !
  USE lsda_mod,      ONLY : nspin_                  => nspin, &
                            starting_magnetization_ => starting_magnetization, &
                            lsda
  !
  USE relax,         ONLY : epse, epsf, epsp, starting_scf_threshold
  !
  USE control_flags, ONLY : isolve, max_cg_iter, david, tr2, imix, gamma_only,&
                            nmix, iverbosity, niter, pot_order, wfc_order, &
                            remove_rigid_rot_ => remove_rigid_rot, &
                            diago_full_acc_   => diago_full_acc, &
                            tolp_             => tolp, &
                            upscale_          => upscale, &
                            mixing_beta_      => mixing_beta, &
                            nstep_            => nstep, &
                            iprint_           => iprint, &
                            noinv_            => noinv, &
                            lkpoint_dir_      => lkpoint_dir, &
                            tqr_              => tqr, &
                            io_level, ethr, lscf, lbfgs, lmd, &
                            ldamped, lbands, llang,           &
                            lconstrain, restart, twfcollect, &
                            lecrpa_           => lecrpa
  !
  USE wvfct,         ONLY : nbnd_ => nbnd, &
                            ecutwfc_ => ecutwfc, &
                            ecfixed_ => ecfixed, &
                            qcutz_   => qcutz, &
                            q2sigma_ => q2sigma
  !
  USE fixed_occ,     ONLY : tfixed_occ, f_inp, &
                            one_atom_occupations_ => one_atom_occupations
  !
  !
  USE spin_orb, ONLY : lspinorb_ => lspinorb,  &
                       starting_spin_angle_ => starting_spin_angle

  !
  USE symm_base, ONLY : no_t_rev_ => no_t_rev, nofrac, allfrac, &
                        nosym_ => nosym, nosym_evc_=> nosym_evc
  !

  USE read_pseudo_mod,       ONLY : readpp

  !
  ! ... CONTROL namelist
  !
  USE input_parameters, ONLY : title, calculation, verbosity, restart_mode,    &
                               nstep, iprint, tstress, tprnfor, dt, outdir,    &
                               wfcdir, prefix, etot_conv_thr, forc_conv_thr,   &
                               pseudo_dir, disk_io, tefield, dipfield,         &
                               wf_collect,                                    &
                               lkpoint_dir, lecrpa 
  !
  ! ... SYSTEM namelist
  !
  USE input_parameters, ONLY : ibrav, celldm, a, b, c, cosab, cosac, cosbc, &
                               nat, ntyp, nbnd,tot_charge,tot_magnetization,&
                               ecutwfc, ecutrho, nr1, nr2, nr3, nr1s, nr2s, &
                               nr3s, noinv, nosym, nosym_evc, no_t_rev,     &
                               use_all_frac, force_symmorphic,              &
                               starting_magnetization,                      &
                               occupations, degauss, smearing, nspin,       &
                               ecfixed, qcutz, q2sigma, lda_plus_U,         &
                               lda_plus_U_kind, Hubbard_U, Hubbard_J,       &
                               Hubbard_alpha, input_dft, la2F,              &
                               starting_ns_eigenvalue, U_projection_type,   &
                               x_gamma_extrapolation, nqx1, nqx2, nqx3,     &
                               exxdiv_treatment, yukawa, ecutvcut,          &
                               exx_fraction, screening_parameter, ecutfock, &
                               edir, emaxpos, eopreg, eamp, &
                               constrained_magnetization,     &
                               B_field, fixed_magnetization, lspinorb,&
                               starting_spin_angle,                           &
                               assume_isolated, spline_ps,                    &
                               one_atom_occupations 
  !
  ! ... ELECTRONS namelist
  !
  USE input_parameters, ONLY : electron_maxstep, mixing_mode, mixing_beta, &
                               mixing_ndim, mixing_fixed_ns, conv_thr,     &
                               tqr, diago_thr_init, diago_cg_maxiter,      &
                               diago_david_ndim, diagonalization,          &
                               diago_full_acc, startingwfc, startingpot,   &
                               real_space
  USE input_parameters, ONLY : adaptive_thr, conv_thr_init, conv_thr_multi
  !
  ! ... IONS namelist
  !
  USE input_parameters, ONLY : phase_space, ion_dynamics, ion_positions, tolp, &
                               tempw, delta_t, nraise, ion_temperature,        &
                               refold_pos, remove_rigid_rot, upscale,          &
                               pot_extrapolation,  wfc_extrapolation,          &
                               w_1, w_2, trust_radius_max, trust_radius_min,   &
                               trust_radius_ini, bfgs_ndim
  !
  ! ... CELL namelist
  !
  USE input_parameters, ONLY : cell_parameters, cell_dynamics, press, wmass,  &
                               cell_temperature, cell_factor, press_conv_thr, &
                               cell_dofree
  !
  ! ... CARDS
  !
  USE input_parameters,   ONLY : k_points, xk, wk, nk1, nk2, nk3,  &
                                 k1, k2, k3, nkstot
  USE input_parameters, ONLY : nconstr_inp, trd_ht, rd_ht, cell_units
  !
  USE read_namelists_module, ONLY : read_namelists, sm_not_set
  USE us, ONLY : spline_ps_ => spline_ps
  !
  USE input_parameters,       ONLY : deallocate_input_parameters
  !
  IMPLICIT NONE
  !
  CHARACTER(LEN=256), EXTERNAL :: trimcheck
  !
  INTEGER  :: ia, image, nt, inlc
  REAL(DP) :: theta, phi
  !
  !
  ! ... various initializations of control variables
  !
  lforce    = tprnfor
  !
  SELECT CASE( trim( calculation ) )
  CASE( 'scf' )
     !
     lscf  = .true.
     nstep = 1
     !
  CASE( 'nscf' )
     !
     lforce = .false.
     nstep  = 1
     !
  CASE( 'bands' )
     !
     lforce = .false.
     lbands = .true.
     nstep  = 1
     !
  CASE( 'relax' )
     !
     lscf   = .true.
     lforce = .true.
     !
     epse = etot_conv_thr
     epsf = forc_conv_thr
     !
     SELECT CASE( trim( ion_dynamics ) )
     CASE( 'bfgs' )
        !
        lbfgs = .true.
        !
     CASE ( 'damp' )
        !
        lmd     = .true.
        ldamped = .true.
        !
        !
     CASE DEFAULT
        !
        CALL errore( 'iosys', 'calculation=' // trim( calculation ) // &
                   & ': ion_dynamics=' // trim( ion_dynamics ) // &
                   & ' not supported', 1 )
        !
     END SELECT
     !
     !
  CASE( 'vc-relax' )
     !
     lscf      = .true.
     lmd       = .true.
     lforce    = .true.
     ldamped   = .true.
     !
     epse =  etot_conv_thr
     epsf =  forc_conv_thr
     epsp = press_conv_thr
     !
     SELECT CASE( trim( cell_dynamics ) )
     CASE( 'none' )
        !
        !
     CASE( 'damp-pr' )
        !
        !
     CASE( 'damp-w' )
        !
        !
     CASE( 'bfgs' )
        !
        lbfgs = .true.
        lmd   = .false.
        ldamped = .false.
        !
     CASE DEFAULT
        !
        CALL errore( 'iosys', 'calculation=' // trim( calculation ) // &
                   & ': cell_dynamics=' // trim( cell_dynamics ) // &
                   & ' not supported', 1 )
        !
     END SELECT
     !
     IF ( .not. ldamped .and. .not. lbfgs) &
        CALL errore( 'iosys', 'calculation='// trim( calculation ) // &
                   & ': incompatible ion (' // trim( ion_dynamics )// &
                   & ') and cell dynamics ('// trim(cell_dynamics )// ')', 1 )
     !
  CASE( 'vc-md' )
     !
     lscf      = .true.
     lmd       = .true.
     lforce    = .true.
     !
     !
     SELECT CASE( trim( cell_dynamics ) )
     CASE( 'none' )
        !
        !
     CASE( 'pr' )
        !
        !
     CASE( 'w' )
        !
        !
     CASE DEFAULT
        !
        CALL errore( 'iosys', 'calculation=' // trim( calculation ) // &
                   & ': ion_dynamics=' // trim( ion_dynamics ) // &
                   & ' not supported', 1 )
        !
     END SELECT
     !
     IF ( trim( ion_dynamics ) /= 'beeman' ) &
        CALL errore( 'iosys', 'calculation=' // trim( calculation ) // &
                   & ': ion_dynamics=' // trim( ion_dynamics ) // &
                   & ' not supported', 1 )
     !
  CASE DEFAULT
     !
     CALL errore( 'iosys', 'calculation ' // &
                & trim( calculation ) // ' not implemented', 1 )
     !
  END SELECT
  !
  lstres = ( tstress .and. lscf )
  !
  IF ( tefield .and. ( .not. nosym ) ) THEN
     nosym = .true.
     WRITE( stdout, &
            '(5x,"Presently no symmetry can be used with electric field",/)' )
  ENDIF
  IF ( tefield .and. tstress ) THEN
     tstress = .false.
     WRITE( stdout, &
            '(5x,"Presently stress not available with electric field",/)' )
  ENDIF
  IF ( tefield .and. ( nspin > 2 ) ) THEN
     CALL errore( 'iosys', 'LSDA not available with electric field' , 1 )
  ENDIF
  !
  twfcollect = wf_collect
  !
  ! ... Set Values for electron and bands
  !
  tfixed_occ = .false.
  ltetra     = .false.
  lgauss     = .false.
  !
  SELECT CASE( trim( occupations ) )
  CASE( 'fixed' )
     !
     ngauss = 0
     IF ( degauss /= 0.D0 ) THEN
        CALL errore( ' iosys ', &
                   & ' fixed occupations, gauss. broadening ignored', -1 )
        degauss = 0.D0
     ENDIF
     !
  CASE( 'smearing' )
     !
     lgauss = ( degauss > 0.0_dp ) 
     IF ( .NOT. lgauss ) &
        CALL errore( ' iosys ', &
                   & ' smearing requires gaussian broadening', 1 )
     !
     SELECT CASE ( trim( smearing ) )
     CASE ( 'gaussian', 'gauss', 'Gaussian', 'Gauss' )
        ngauss = 0
        smearing_ = 'gaussian'
     CASE ( 'methfessel-paxton', 'm-p', 'mp', 'Methfessel-Paxton', 'M-P', 'MP' )
        ngauss = 1
        smearing_ = 'Methfessel-Paxton'
     CASE ( 'marzari-vanderbilt', 'cold', 'm-v', 'mv', 'Marzari-Vanderbilt', 'M-V', 'MV')
        ngauss = -1
        smearing_ = 'Marzari-Vanderbilt'
     CASE ( 'fermi-dirac', 'f-d', 'fd', 'Fermi-Dirac', 'F-D', 'FD')
        ngauss = -99
        smearing_ = 'Fermi-Dirac'
     CASE DEFAULT
        CALL errore( ' iosys ', ' smearing '//trim(smearing)//' unknown', 1 )
     END SELECT
     !
  CASE( 'tetrahedra' )
     !
     ! replace "errore" with "infomsg" in the next line if you really want
     ! to perform a calculation with forces using tetrahedra 
     !
     IF( lforce ) CALL errore( 'iosys', &
        'force calculation with tetrahedra not recommanded: use smearing',1)
     !
     ! as above, for stress
     !
     IF( lstres ) CALL errore( 'iosys', &
        'stress calculation with tetrahedra not recommanded: use smearing',1)
     ngauss = 0
     ltetra = .true.
     !
  CASE( 'from_input' )
     !
     ngauss     = 0
     tfixed_occ = .true.
     !
  CASE DEFAULT
     !
     CALL errore( 'iosys','occupations ' // trim( occupations ) // &
                & 'not implemented', 1 )
     !
  END SELECT
  !
  IF( nbnd < 1 ) &
     CALL errore( 'iosys', 'nbnd less than 1', nbnd )
  !
  SELECT CASE( nspin )
  CASE( 1 )
     !
     lsda = .false.
     !
  CASE( 2 )
     !
     lsda = .true.
     !
  CASE( 4 )
     !
     lsda = .false.
     !
  CASE DEFAULT
     !
     CALL errore( 'iosys', 'wrong input value for nspin', 1 )
     !
  END SELECT
  !
  two_fermi_energies = ( tot_magnetization /= -1._DP)
  IF ( two_fermi_energies .and. tot_magnetization < 0._DP) &
     CALL errore( 'iosys', 'tot_magnetization only takes positive values', 1 )
  IF ( two_fermi_energies .and. .not. lsda ) &
     CALL errore( 'iosys', 'tot_magnetization requires nspin=2', 1 )
  !
  IF ( occupations == 'fixed' .and. lsda  .and. lscf ) THEN
     !
     IF ( two_fermi_energies ) THEN
        !
        IF ( abs( nint(tot_magnetization ) - tot_magnetization ) > eps8 ) &
           CALL errore( 'iosys', &
                 & 'fixed occupations requires integer tot_magnetization', 1 )
        IF ( abs( nint(tot_charge ) - tot_charge ) > eps8 ) &
           CALL errore( 'iosys', &
                      & 'fixed occupations requires integer charge', 1 )
        !
     ELSE
        !
        CALL errore( 'iosys', &
                   & 'fixed occupations and lsda need tot_magnetization', 1 )
        !
     ENDIF
     !
  ENDIF
  !
  !
  SELECT CASE( trim( constrained_magnetization ) )
  CASE( 'none' )
     !
     ! ... starting_magnetization(nt) = sm_not_set means "not set"
     ! ... if no constraints are imposed on the magnetization, 
     ! ... starting_magnetization must be set for at least one atomic type
     !
     IF ( lscf .AND. lsda .AND. ( .NOT. tfixed_occ ) .AND. &
          ( .not. two_fermi_energies )  .AND. &
          ALL (starting_magnetization(1:ntyp) == sm_not_set) ) &
        CALL errore('iosys','some starting_magnetization MUST be set', 1 )
     !
     ! ... bring starting_magnetization between -1 and 1
     !
     DO nt = 1, ntyp
        !
        IF ( starting_magnetization(nt) == sm_not_set ) THEN
           starting_magnetization(nt) = 0.0_dp
        ELSEIF ( starting_magnetization(nt) > 1.0_dp ) THEN
          starting_magnetization(nt) = 1.0_dp
        ELSEIF ( starting_magnetization(nt) <-1.0_dp ) THEN
          starting_magnetization(nt) =-1.0_dp
        ENDIF
        !
     ENDDO
     !
     !
  CASE( 'atomic' )
     !
     IF ( nspin == 1 ) &
        CALL errore( 'iosys','constrained atomic magnetizations ' // &
                   & 'require nspin=2 or 4 ', 1 )
     IF ( ALL (starting_magnetization(1:ntyp) == sm_not_set) ) &
        CALL errore( 'iosys','constrained atomic magnetizations ' // &
                   & 'require that some starting_magnetization is set', 1 )
     !
     !
  CASE( 'atomic direction' )
     !
     IF ( nspin == 1 ) &
        CALL errore( 'iosys','constrained atomic magnetization ' // &
                   & 'directions require nspin=2 or 4 ', 1 )
     !
  CASE( 'total' )
     !
     IF ( nspin == 4 ) THEN
        !
        !
     ELSE
        !
        CALL errore( 'iosys','constrained total magnetization ' // &
                   & 'requires nspin= 4 ', 1 )
        !
     ENDIF
     !
  CASE( 'total direction' )
     !
  CASE DEFAULT
     !
     CALL errore( 'iosys','constrained magnetization ' // &
                & trim( constrained_magnetization ) // 'not implemented', 1 )
     !
  END SELECT
  !
  !
  IF ( ecutrho <= 0.D0 ) THEN
     !
     dual = 4.D0
     ecutrho = dual*ecutwfc
     !
  ELSE
     !
     dual = ecutrho / ecutwfc
     IF ( dual <= 1.D0 ) &
        CALL errore( 'iosys', 'invalid dual?', 1 )
     !
  ENDIF
  !
  SELECT CASE( trim( restart_mode ) )
  CASE( 'from_scratch' )
     !
     restart        = .false.
     IF ( lscf ) THEN
        startingconfig = 'input'
     ELSE
        startingconfig = 'file'
     ENDIF
     !
  CASE DEFAULT
     !
     CALL errore( 'iosys', &
                & 'unknown restart_mode ' // trim( restart_mode ), 1 )
     !
  END SELECT
  !
  SELECT CASE( trim( disk_io ) )
  CASE( 'high' )
     !
     io_level = 2
     !
  CASE ( 'low' )
     !
     io_level = 0
     restart  = .false.
     !
  CASE ( 'none' )
     !
     io_level = -1
     restart  = .false.
     IF ( twfcollect ) THEN
        CALL infomsg('iosys', 'minimal I/O required, wf_collect reset to FALSE')
        twfcollect= .false.
     ENDIF
     !
  CASE DEFAULT
     !
     io_level = 1
     !
     IF ( lscf ) restart  = .false.
     !
  END SELECT
  !
  Hubbard_U(:)    = Hubbard_U(:) / rytoev
  Hubbard_J(:,:)  = Hubbard_J(:,:) / rytoev
  Hubbard_alpha(:)= Hubbard_alpha(:) / rytoev
  !
  ethr = diago_thr_init
  !
  IF ( startingpot /= 'atomic' .and. startingpot /= 'file' ) THEN
     !
     CALL infomsg( 'iosys', 'wrong startingpot: use default (1)' )
     !
     IF ( lscf ) THEN
        startingpot = 'atomic'
     ELSE 
        startingpot = 'file'
     END IF
     !
  ENDIF
  !
  IF ( .not. lscf .and. startingpot /= 'file' ) THEN
     !
     CALL infomsg( 'iosys', 'wrong startingpot: use default (2)' )
     !
     startingpot = 'file'
     !
  ENDIF
  !
  IF (      startingwfc /= 'atomic' .and. &
            startingwfc /= 'random' .and. &
            startingwfc /= 'atomic+random' .and. &
            startingwfc /= 'file' ) THEN
     !
     CALL infomsg( 'iosys', 'wrong startingwfc: use default' )
     !
     startingwfc = 'atomic'
     !
  ENDIF
  !
  SELECT CASE( trim( diagonalization ) )
  CASE ( 'cg' )
     !
     isolve = 1
     max_cg_iter = diago_cg_maxiter
     !
  CASE ( 'david', 'davidson' )
     !
     isolve = 0
     david = diago_david_ndim
     !
  CASE DEFAULT
     !
     CALL errore( 'iosys', 'diagonalization ' // &
                & trim( diagonalization ) // ' not implemented', 1 )
     !
  END SELECT
  !
  tr2   = conv_thr
  niter = electron_maxstep
  adapt_thr = adaptive_thr
  tr2_init  = conv_thr_init
  tr2_multi = conv_thr_multi
  !
  pot_order = 1
  SELECT CASE( trim( pot_extrapolation ) )
  CASE( 'from_wfcs', 'from-wfcs' )
     ! not actually implemented
     pot_order =-1
     !
  CASE( 'none' )
     !
     pot_order = 0
     !
  CASE( 'first_order', 'first-order', 'first order' )
     !
     IF ( lmd  ) THEN
        pot_order = 2
     ELSE
        CALL infomsg('iosys', "pot_extrapolation='"//trim(pot_extrapolation)//&
                     "' not available, using 'atomic'")
     ENDIF
     !
  CASE( 'second_order', 'second-order', 'second order' )
     !
     IF ( lmd  ) THEN
        pot_order = 3
     ELSE
        CALL infomsg('iosys', "pot_extrapolation='"//trim(pot_extrapolation)//&
                     "' not available, using 'atomic'")
     ENDIF
     !
  CASE DEFAULT
     !
     pot_order = 1
     !
  END SELECT
  !
  wfc_order = 0
  SELECT CASE( trim( wfc_extrapolation ) )
     !
  CASE( 'first_order', 'first-order', 'first order' )
     !
     IF ( lmd  ) THEN
        wfc_order = 2
     ELSE
        CALL infomsg('iosys', "wfc_extrapolation='"//trim(pot_extrapolation)//&
                     "' not available, using 'atomic'")
     ENDIF
     !
  CASE( 'second_order', 'second-order', 'second order' )
     !
     IF ( lmd  ) THEN
        wfc_order = 3
     ELSE
        CALL infomsg('iosys', "wfc_extrapolation='"//trim(pot_extrapolation)//&
                     "' not available, using 'atomic'")
     ENDIF
     !
  END SELECT
  !
  SELECT CASE( trim( mixing_mode ) )
  CASE( 'plain' )
     !
     imix = 0
     !
  CASE( 'TF' )
     !
     imix = 1
     !
  CASE( 'local-TF' )
     !
     imix = 2
     !
  CASE( 'potential' )
     !
     CALL errore( 'iosys', 'potential mixing no longer implemented', 1 )
     !
  CASE DEFAULT
     !
     CALL errore( 'iosys', 'unknown mixing ' // trim( mixing_mode ), 1 )
     !
  END SELECT
  !
  starting_scf_threshold = tr2
  nmix = mixing_ndim
  !
  IF ( ion_dynamics == ' bfgs' .and. epse <= 20.D0 * ( tr2 / upscale ) ) &
       CALL errore( 'iosys', 'required etot_conv_thr is too small:' // &
                     & ' conv_thr must be reduced', 1 )
  !
  SELECT CASE( trim( verbosity ) )
  CASE( 'debug', 'high', 'medium' )
     !
     iverbosity = 1
     !
  CASE( 'low', 'default', 'minimal' )
     !
     iverbosity = 0 
     !
  CASE DEFAULT
     !
     iverbosity = 0
     !
  END SELECT
  !
  tmp_dir = trimcheck ( outdir )
  !

  !
  ! ... Copy values from input module to PW internals
  !
  tqr_        = tqr
  !
  title_      = title
  lkpoint_dir_=lkpoint_dir
  tefield_    = tefield
  dipfield_   = dipfield
  prefix_     = trim( prefix )
  pseudo_dir_ = trimcheck( pseudo_dir )
  nstep_      = nstep
  iprint_     = iprint
  lecrpa_     = lecrpa
  !
  nat_     = nat
  ntyp_    = ntyp
  edir_    = edir
  emaxpos_ = emaxpos
  eopreg_  = eopreg
  eamp_    = eamp
  dfftp%nr1     = nr1
  dfftp%nr2     = nr2
  dfftp%nr3     = nr3
  ecutrho_ = ecutrho
  ecutwfc_ = ecutwfc
  ecfixed_ = ecfixed
  qcutz_   = qcutz
  q2sigma_ = q2sigma
  dffts%nr1    = nr1s
  dffts%nr2    = nr2s
  dffts%nr3    = nr3s
  degauss_ = degauss
  !
  tot_charge_        = tot_charge
  tot_magnetization_ = tot_magnetization
  !
  lspinorb_ = lspinorb
  starting_spin_angle_ = starting_spin_angle
  one_atom_occupations_ = one_atom_occupations
  !
  no_t_rev_ = no_t_rev
  allfrac   = use_all_frac
  !
  spline_ps_ = spline_ps
  !
  la2F_                   = la2F
  nspin_                  = nspin
  starting_magnetization_ = starting_magnetization
  noinv_                  = noinv
  nosym_                  = nosym
  nosym_evc_              = nosym_evc
  nofrac                  = force_symmorphic
  nbnd_                   = nbnd
  !
  !
  !
  diago_full_acc_ = diago_full_acc
  starting_wfc    = startingwfc
  starting_pot    = startingpot
  mixing_beta_    = mixing_beta
  !
  remove_rigid_rot_ = remove_rigid_rot
  upscale_          = upscale
  !
  IF (trim(occupations) /= 'from_input') one_atom_occupations_=.false.
  !
  !
  SELECT CASE( trim( assume_isolated ) )
      !
    CASE( 'none' )
      !
      !
    CASE DEFAULT
      !
      call errore ('iosys','unrecognized value for assume_isolated',1)
  END SELECT
  !
  ! ... read following cards
  !
  ALLOCATE( ityp( nat_ ) )
  ALLOCATE( tau(    3, nat_ ) )
  ALLOCATE( force(  3, nat_ ) )
  ALLOCATE( if_pos( 3, nat_ ) )
  ALLOCATE( extfor( 3, nat_ ) )
  IF ( tfixed_occ ) THEN
     IF ( nspin_ == 4 ) THEN
        ALLOCATE( f_inp( nbnd_, 1 ) )
     ELSE
        ALLOCATE( f_inp( nbnd_, nspin_ ) )
     ENDIF
  ENDIF
  !
  IF ( tefield ) ALLOCATE( forcefield( 3, nat_ ) )
  !
  ! ... note that read_cards_pw no longer reads cards!
  !
  CALL read_cards_pw ( psfile, tau_format )
  !
  ! ... set up atomic positions and crystal lattice
  !
  call cell_base_init ( ibrav, celldm, a, b, c, cosab, cosac, cosbc, &
                        trd_ht, rd_ht, cell_units )
  !
  ! ... set up k-points
  !
  CALL init_start_k ( nk1, nk2, nk3, k1, k2, k3, k_points, nkstot, xk, wk )
  gamma_only = ( k_points == 'gamma' )
  if( gamma_only ) call errore('iosys','kpoints=gamma calculations not available for mini_DFT',1)
  !
  !
  CALL convert_tau ( tau_format, nat_, tau)
  !
  IF ( wmass == 0.D0 ) THEN
     !
     ! ... set default value of wmass
     !
#if defined __PGI
     DO ia = 1, nat_
        wmass = wmass + amass( ityp(ia) )
     ENDDO
#else
     wmass = sum( amass(ityp(:)) )
#endif
     !
     wmass = wmass * amconv
     !
  ELSE
     !
     ! ... wmass is given in amu, Renata's dynamics uses masses in atomic units
     !
     !
  ENDIF
  !
  ! ... unit conversion for pressure
  !
  !
  ! ... set constraints for cell dynamics/optimization
  !
  CALL init_dofree ( cell_dofree )
  !
  ! ... read pseudopotentials (also sets DFT)
  !
  CALL readpp ( input_dft )
  !
  !
  ! ... variables for constrained dynamics are set here
  !
  lconstrain = ( nconstr_inp > 0 )
  !
  ! ... Files
  !
  input_drho  = ' '
  output_drho = ' '
  !
  IF (real_space ) THEN
        CALL errore ('iosys', 'Real space only with Gamma point', 1)
  ENDIF
  !
  ! Deallocation of temp input arrays
  !
  CALL deallocate_input_parameters ()  
  !
  RETURN
  !
END SUBROUTINE iosys
!
!----------------------------------------------------------------------------
SUBROUTINE read_cards_pw ( psfile, tau_format )
  !----------------------------------------------------------------------------
  !
  USE kinds,              ONLY : DP
  USE input_parameters,   ONLY : atom_label, atom_pfile, atom_mass, taspc, &
                                 tapos, rd_pos, atomic_positions, if_pos,  &
                                 sp_pos, f_inp, rd_for, tavel, sp_vel, rd_vel
  USE cell_base,          ONLY : at, ibrav
  USE ions_base,          ONLY : nat, ntyp => nsp, ityp, tau, atm, extfor
  USE fixed_occ,          ONLY : tfixed_occ, f_inp_ => f_inp
  USE ions_base,          ONLY : if_pos_ =>  if_pos, amass, fixatom
  USE control_flags,      ONLY : lfixatom, textfor
  !
  IMPLICIT NONE
  !
  CHARACTER (len=256) :: psfile(ntyp)
  CHARACTER (len=80)  :: tau_format
  INTEGER, EXTERNAL :: atomic_number
  REAL(DP), EXTERNAL :: atom_weight
  !
  INTEGER :: is, ia
  !
  !
  amass = 0
  !
  IF ( .not. taspc ) &
     CALL errore( 'read_cards_pw', 'atomic species info missing', 1 )
  IF ( .not. tapos ) &
     CALL errore( 'read_cards_pw', 'atomic position info missing', 1 )
  !
  DO is = 1, ntyp
     !
     amass(is)  = atom_mass(is)
     psfile(is) = atom_pfile(is)
     atm(is)    = atom_label(is)
     !
     IF ( amass(is) <= 0.0_DP ) amass(is)= &
              atom_weight(atomic_number(trim(atm(is))))

     IF ( amass(is) <= 0.D0 ) CALL errore( 'read_cards_pw', 'invalid  mass', is )
     !
  ENDDO
  !
  textfor = .false.
  IF( any( rd_for /= 0.0_DP ) ) textfor = .true.
  !
  DO ia = 1, nat
     !
     tau(:,ia) = rd_pos(:,ia)
     ityp(ia)  = sp_pos(ia)
     extfor(:,ia) = rd_for(:,ia)
     !
  ENDDO
  !
  ! ... The constrain on fixed coordinates is implemented using the array
  ! ... if_pos whose value is 0 when the coordinate is to be kept fixed, 1
  ! ... otherwise. 
  !
  if_pos_(:,:) = if_pos(:,1:nat)
  fixatom = COUNT( if_pos_(1,:)==0 .AND. if_pos_(2,:)==0 .AND. if_pos_(3,:)==0 )
  lfixatom = ANY ( if_pos_ == 0 )
  !
  tau_format = trim( atomic_positions )
  !
  IF ( tfixed_occ ) THEN
     !
     f_inp_ = f_inp
     !
     DEALLOCATE ( f_inp )
     !
  ENDIF
  !
  RETURN
  !
END SUBROUTINE read_cards_pw
!
!-----------------------------------------------------------------------
SUBROUTINE convert_tau (tau_format, nat_, tau)
!-----------------------------------------------------------------------
  !
  ! ... convert input atomic positions to internally used format:
  ! ... tau in a0 units
  !
  USE kinds,         ONLY : DP
  USE constants,     ONLY : bohr_radius_angs
  USE cell_base,     ONLY : at, alat
  IMPLICIT NONE
  CHARACTER (len=*), INTENT(in)  :: tau_format
  INTEGER, INTENT(in)  :: nat_
  REAL (DP), INTENT(inout) :: tau(3,nat_)
  !
  SELECT CASE( tau_format )
  CASE( 'alat' )
     !
     ! ... input atomic positions are divided by a0: do nothing
     !
  CASE( 'bohr' )
     !
     ! ... input atomic positions are in a.u.: divide by alat
     !
     tau = tau / alat
     !
  CASE( 'crystal' )
     !
     ! ... input atomic positions are in crystal axis
     !
     CALL cryst_to_cart( nat_, tau, at, 1 )
     !
  CASE( 'angstrom' )
     !
     ! ... atomic positions in A: convert to a.u. and divide by alat
     !
     tau = tau / bohr_radius_angs / alat
     !
  CASE DEFAULT
     !
     CALL errore( 'iosys','tau_format=' // &
                & trim( tau_format ) // ' not implemented', 1 )
     !
  END SELECT
  !
END SUBROUTINE convert_tau
