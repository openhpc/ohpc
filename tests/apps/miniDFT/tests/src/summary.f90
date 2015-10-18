!
! Copyright (C) 2001-2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
SUBROUTINE summary()
  !-----------------------------------------------------------------------
  !
  !    This routine writes on output all the information obtained from
  !    the input file and from the setup routine, before starting the
  !    self-consistent calculation.
  !
  !    if iverbosity < 1 only a partial summary is done.
  !
  USE io_global,       ONLY : stdout
  USE kinds,           ONLY : DP
  USE run_info,        ONLY: title
  USE constants,       ONLY : amconv, rytoev
  USE cell_base,       ONLY : alat, ibrav, omega, at, bg, celldm
  USE ions_base,       ONLY : nat, atm, zv, tau, ntyp => nsp, ityp
  USE ions_base,       ONLY : amass
  USE gvect,           ONLY : ecutrho, ngm, ngm_g, gcutm
  USE gvecs,         ONLY : doublegrid, ngms, gcutms
  USE fft_base,        ONLY : dfftp
  USE fft_base,        ONLY : dffts
  USE lsda_mod,        ONLY : lsda, starting_magnetization
  USE klist,           ONLY : degauss, smearing, lgauss, nkstot, xk, wk, &
                              nelec, nelup, neldw, two_fermi_energies
  USE ktetra,          ONLY : ltetra
  USE control_flags,   ONLY : imix, nmix, mixing_beta, nstep, lscf, &
                              tr2, isolve, lmd, lbfgs, lpath, iverbosity
  USE spin_orb,        ONLY : domag, lspinorb
  USE funct,           ONLY : write_dft_name
  USE fixed_occ,       ONLY : f_inp, tfixed_occ
  USE uspp_param,      ONLY : upf
  USE wvfct,           ONLY : nbnd, ecutwfc, qcutz, ecfixed, q2sigma
  USE lsda_mod,        ONLY : nspin
  USE mp_global,       ONLY : intra_bgrp_comm
  USE mp,              ONLY : mp_sum
  !
  IMPLICIT NONE
  !
  ! ... declaration of the local variables
  !
  INTEGER :: i, ipol, apol, na, isym, ik, nt, ibnd, ngmtot
    ! counter on the celldm elements
    ! counter on polarizations
    ! counter on direct or reciprocal lattice vect
    ! counter on atoms
    ! counter on symmetries
    ! counter on k points
    ! counter on beta functions
    ! counter on types
    ! counter on bands
    ! total number of G-vectors (parallel execution)
    !
  REAL(DP), ALLOCATABLE :: xau(:,:)
    ! atomic coordinate referred to the crystal axes
  REAL(DP) :: xkg(3)
    ! coordinates of the k point in crystal axes
  CHARACTER :: mixing_style * 9
  REAL(DP) :: xp
    ! fraction contributing to a given atom type (obsolescent)
  !
  ! ... we start with a general description of the run
  !
  IF ( imix ==  0 ) mixing_style = 'plain'
  IF ( imix ==  1 ) mixing_style = 'TF'
  IF ( imix ==  2 ) mixing_style = 'local-TF'
  !
  IF ( title /= ' ') WRITE( stdout, "(/,5X,'Title: ',/,5X,A75)" ) title
  !
  WRITE( stdout, 100) ibrav, alat, omega, nat, ntyp
  IF ( two_fermi_energies ) THEN
     WRITE( stdout, 101) nelec, nelup, neldw
  ELSE
     WRITE( stdout, 102) nelec
  END IF
  WRITE( stdout, 103) nbnd, ecutwfc, ecutrho
  IF ( lscf) WRITE( stdout, 104) tr2, mixing_beta, nmix, mixing_style
  !
100 FORMAT( /,/,5X, &
       &     'bravais-lattice index     = ',I12,/,5X, &
       &     'lattice parameter (alat)  = ',F12.4,'  a.u.',/,5X, &
       &     'unit-cell volume          = ',F12.4,' (a.u.)^3',/,5X, &
       &     'number of atoms/cell      = ',I12,/,5X, &
       &     'number of atomic types    = ',I12)
101 FORMAT(5X, &
       &     'number of electrons       = ',F12.2,' (up:',f7.2,', down:',f7.2,')')
102 FORMAT(5X, &
       &     'number of electrons       = ',f12.2)
103 FORMAT(5X, &
       &     'number of Kohn-Sham states= ',I12,/,5X, &
       &     'kinetic-energy cutoff     = ',F12.4,'  Ry',/,5X, &
       &     'charge density cutoff     = ',F12.4,'  Ry')
104 FORMAT(5X, &
       &     'convergence threshold     = ',1PE12.1,/,5X, &
       &     'mixing beta               = ',0PF12.4,/,5X, &
       &     'number of iterations used = ',I12,2X,A,' mixing')
  !
  call write_dft_name ( ) 
  !
  IF ( lmd .OR. lbfgs .OR. lpath ) &
     WRITE( stdout, '(5X,"nstep                     = ",I12,/)' ) nstep
  !
  !
  IF ( qcutz > 0.D0 ) THEN
     !
     WRITE( stdout, 110 ) ecfixed, qcutz, q2sigma
     !
110  FORMAT( 5X,'A smooth kinetic-energy cutoff is imposed at ', &
          &  F12.4,' Ry',/5X,'height of the smooth ', &
          &  'step-function =',F21.4,' Ry',/5X, &
          &  'width of the smooth step-function  =',F21.4,' Ry',/ )
     !
  END IF
 
#ifdef __ENVIRON
  IF ( do_environ ) CALL environ_summary()
#endif
! DCC
!  IF ( do_comp )  CALL write_ee_summary()
  !
  ! ... ESM
  !
  !
  ! ... and here more detailed information. Description of the unit cell
  !
  WRITE( stdout, '(/2(3X,3(2X,"celldm(",I1,")=",F11.6),/))' ) &
       ( i, celldm(i), i = 1, 6 )
  !
  WRITE( stdout, '(5X, &
       &     "crystal axes: (cart. coord. in units of alat)",/, &
       &       3(15x,"a(",i1,") = (",3f11.6," )  ",/ ) )')  (apol,  &
       (at (ipol, apol) , ipol = 1, 3) , apol = 1, 3)
  !
  WRITE( stdout, '(5x, &
       &   "reciprocal axes: (cart. coord. in units 2 pi/alat)",/, &
       &            3(15x,"b(",i1,") = (",3f10.6," )  ",/ ) )')  (apol,&
       &  (bg (ipol, apol) , ipol = 1, 3) , apol = 1, 3)
  !
  CALL print_ps_info ( )
  !
  !
  ! ... print the vdw table information if needed
  !
  WRITE( stdout, '(/5x, "atomic species   valence    mass     pseudopotential")')
  xp = 1.d0
  DO nt = 1, ntyp
     WRITE( stdout, '(5x,a6,6x,f10.2,2x,f10.5,5x,5 (a2,"(",f5.2,")"))') &
                   atm(nt), zv(nt), amass(nt), upf(nt)%psd, xp
  ENDDO


  IF (lsda) THEN
     WRITE( stdout, '(/5x,"Starting magnetic structure ", &
          &      /5x,"atomic species   magnetization")')
     DO nt = 1, ntyp
        WRITE( stdout, '(5x,a6,9x,f6.3)') atm(nt), starting_magnetization(nt)
     ENDDO
  ENDIF
  !
  !   description of symmetries
  !
  CALL  print_symmetries ( iverbosity, .false., domag )
  !
  !    description of the atoms inside the unit cell
  !
  WRITE( stdout, '(/,3x,"Cartesian axes")')
  WRITE( stdout, '(/,5x,"site n.     atom                  positions (alat units)")')

  WRITE( stdout, '(6x,i4,8x,a6," tau(",i4,") = (",3f12.7,"  )")') &
             (na, atm(ityp(na)), na, (tau(ipol,na), ipol=1,3), na=1,nat)
  !
  !  output of starting magnetization
  !
  IF (iverbosity > 0) THEN
     !
     !   allocate work space
     !
     ALLOCATE (xau(3,nat))
     !
     !     Compute the coordinates of each atom in the basis of the
     !     direct lattice vectors
     !
     DO na = 1, nat
        DO ipol = 1, 3
           xau(ipol,na) = bg(1,ipol)*tau(1,na) + bg(2,ipol)*tau(2,na) + &
                          bg(3,ipol)*tau(3,na)
        ENDDO
     ENDDO
     !
     !   description of the atoms inside the unit cell
     !   (in crystallographic coordinates)
     !
     WRITE( stdout, '(/,3x,"Crystallographic axes")')
     WRITE( stdout, '(/,5x,"site n.     atom        ", &
          &             "          positions (cryst. coord.)")')

     WRITE( stdout, '(6x,i4,8x,a6," tau(",i4,") = (",3f11.7,"  )")') &
           (na, atm(ityp(na)), na,  (xau(ipol,na), ipol=1,3), na=1,nat)
     !
     !   deallocate work space
     !
     DEALLOCATE(xau)
  ENDIF

  IF (lgauss) THEN
     WRITE( stdout, '(/5x,"number of k points=", i6, 2x, &
          &             a," smearing, width (Ry)=",f8.4)') &
          &             nkstot, TRIM(smearing), degauss
  ELSE IF (ltetra) THEN
     WRITE( stdout,'(/5x,"number of k points=",i6, &
          &        " (tetrahedron method)")') nkstot
  ELSE
     WRITE( stdout, '(/5x,"number of k points=",i6)') nkstot

  ENDIF
  IF ( iverbosity > 0 .OR. nkstot < 100 ) THEN
     WRITE( stdout, '(23x,"cart. coord. in units 2pi/alat")')
     DO ik = 1, nkstot
        WRITE( stdout, '(8x,"k(",i5,") = (",3f12.7,"), wk =",f12.7)') ik, &
             (xk (ipol, ik) , ipol = 1, 3) , wk (ik)
     ENDDO
  ELSE
     WRITE( stdout, '(/5x,a)') &
     "Number of k-points >= 100: set verbosity='high' to print them."
  ENDIF
  IF ( iverbosity > 0 ) THEN
     WRITE( stdout, '(/23x,"cryst. coord.")')
     DO ik = 1, nkstot
        DO ipol = 1, 3
           xkg(ipol) = at(1,ipol)*xk(1,ik) + at(2,ipol)*xk(2,ik) + &
                       at(3,ipol)*xk(3,ik)
           ! xkg are the component in the crystal RL basis
        ENDDO
        WRITE( stdout, '(8x,"k(",i5,") = (",3f12.7,"), wk =",f12.7)') &
             ik, (xkg (ipol) , ipol = 1, 3) , wk (ik)
     ENDDO
  ENDIF
  WRITE( stdout, '(/5x,"Dense  grid: ",i8," G-vectors", 5x, &
       &               "FFT dimensions: (",i4,",",i4,",",i4,")")') &
       &         ngm_g, dfftp%nr1, dfftp%nr2, dfftp%nr3
  IF (doublegrid) THEN
     !
     ngmtot = ngms
     CALL mp_sum (ngmtot, intra_bgrp_comm)
     !
     WRITE( stdout, '(/5x,"Smooth grid: ",i8," G-vectors", 5x, &
       &               "FFT dimensions: (",i4,",",i4,",",i4,")")') &
       &         ngmtot, dffts%nr1, dffts%nr2, dffts%nr3
  ENDIF

! DCC
!  IF (do_coarse .OR. do_mltgrid ) THEN
!    WRITE( stdout, '(5x,"G cutoff =",f10.4,"  (", &
!          &    i7," G-vectors)","  coarse grid: (",i3, &
!          &    ",",i3,",",i3,")")') gcutmc, ngmc, mr1, mr2, mr3
!  END IF

  IF (tfixed_occ) THEN
     WRITE( stdout, '(/,5X,"Occupations read from input ")' ) 
     IF (nspin==2) THEN
        WRITE(stdout, '(/,5X," Spin-up")' ) 
        WRITE(stdout, '(/,(5X,8f9.4))') (f_inp(ibnd,1),ibnd=1,nbnd)
        WRITE(stdout, '(/,5X," Spin-down")' ) 
        WRITE(stdout, '(/,(5X,8f9.4))') (f_inp(ibnd,2),ibnd=1,nbnd)
     ELSE
        WRITE(stdout, '(/,(5X,8f9.4))') (f_inp(ibnd,1), ibnd=1,nbnd)
     END IF
  END IF

  !
  CALL flush_unit( stdout )
  !
  RETURN
  !
END SUBROUTINE summary
!
!-----------------------------------------------------------------------
SUBROUTINE print_ps_info
  !-----------------------------------------------------------------------
  !
  USE io_global,       ONLY : stdout
  USE io_files,        ONLY : pseudo_dir, psfile
  USE ions_base,       ONLY : ntyp => nsp
  USE atom,            ONLY : rgrid
  USE uspp_param,      ONLY : upf
  USE funct,           ONLY : dft_is_gradient

  !
  INTEGER :: nt, lmax
  CHARACTER :: ps*35
  !
  DO nt = 1, ntyp
     !
     IF ( upf(nt)%tpawp ) THEN
        ! Note: for PAW pseudo also tvanp is .true.
        ps="Projector augmented-wave"
     ELSE
        ps='Norm-conserving'
     END IF
     !
     IF ( upf(nt)%nlcc ) ps = TRIM(ps) // ' + core correction'
     !
     WRITE( stdout, '(/5x,"PseudoPot. #",i2," for ",a2," read from file:", &
           & /5x,a)') nt, upf(nt)%psd, TRIM(pseudo_dir)//TRIM (psfile(nt))
     WRITE( stdout, '(5x,"MD5 check sum: ", a )') upf(nt)%md5_cksum
     !
     WRITE( stdout, '( 5x,"Pseudo is ",a,", Zval =",f5.1)') &
            TRIM (ps), upf(nt)%zp
     !
     WRITE( stdout, '(5x,A)') TRIM(upf(nt)%generated)
     !
     IF(upf(nt)%tpawp) &
        WRITE( stdout, '(5x,a,a)') &
               "Shape of augmentation charge: ", TRIM(upf(nt)%paw%augshape)
     !
     ! info added for 1/r pseudos (AF)
     IF(upf(nt)%tcoulombp ) &
        WRITE( stdout, '(5x,a,a)') "1/r Coulomb pseudo"
     !
     WRITE( stdout, '(5x,"Using radial grid of ", i4, " points, ", &
         &i2," beta functions with: ")') rgrid(nt)%mesh, upf(nt)%nbeta
     DO ib = 1, upf(nt)%nbeta
        IF (ib < 10 ) THEN
           WRITE( stdout, '(15x," l(",i1,") = ",i3)') ib, upf(nt)%lll(ib)
        ELSE 
           WRITE( stdout, '(14x," l(",i2,") = ",i3)') ib, upf(nt)%lll(ib)
        ENDIF
     END DO


  ENDDO
END SUBROUTINE print_ps_info
!
SUBROUTINE print_symmetries ( iverbosity, noncolin, domag )
  !-----------------------------------------------------------------------
  !
  USE kinds,           ONLY : dp
  USE io_global,       ONLY : stdout 
  USE symm_base,       ONLY : nsym, nsym_ns, nsym_na, invsym, s, sr, &
                              t_rev, ftau, sname
  USE rap_point_group, ONLY : code_group, nclass, nelem, elem, &
       which_irr, char_mat, name_rap, name_class, gname, ir_ram
  USE rap_point_group_so, ONLY : nrap, nelem_so, elem_so, has_e, &
       which_irr_so, char_mat_so, name_rap_so, name_class_so, d_spin, &
       name_class_so1
  USE rap_point_group_is, ONLY : nsym_is, sr_is, ftau_is, d_spin_is, &
       gname_is, sname_is, code_group_is
  USE cell_base,       ONLY : at
  USE fft_base, ONLY : dfftp
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: iverbosity
  LOGICAL, INTENT(IN) :: noncolin, domag
  !
  INTEGER :: nclass_ref   ! The number of classes of the point group
  INTEGER :: isym, ipol
  REAL (dp) :: ft1, ft2, ft3
  !
  !
  IF (nsym <= 1) THEN
     WRITE( stdout, '(/5x,"No symmetry found")')
  ELSE
     IF (invsym) THEN
        IF ( nsym_ns > 0 ) THEN
           WRITE( stdout, '(/5x,i2," Sym. Ops., with inversion, found ", &
                    &  "(",i2," have fractional translation)")' ) nsym, nsym_ns
        ELSE 
           WRITE( stdout, '(/5x,i2," Sym. Ops., with inversion, found")' )&
                         nsym
        END IF
     ELSE
        IF ( nsym_ns > 0 ) THEN
           WRITE( stdout, '(/5x,i2," Sym. Ops. (no inversion) found ",&
                    &  "(",i2," have fractional translation)")' ) nsym, nsym_ns
        ELSE
           WRITE( stdout,'(/5x,i2," Sym. Ops. (no inversion) found")' ) nsym
        END IF
     ENDIF
  ENDIF
  IF ( nsym_na > 0 ) THEN 
      WRITE( stdout, '(10x,"(note: ",i2," additional sym.ops. were found ", &
                   &   "but ignored",/,10x," their fractional transations ", &
                   &   "are incommensurate with FFT grid)",/)') nsym_na
  ELSE
      WRITE( stdout, '(/)' )
  END IF

END SUBROUTINE print_symmetries
