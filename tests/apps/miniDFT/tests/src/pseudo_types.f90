!
! Copyright (C) 2002-2008 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
MODULE pseudo_types

  !  this module contains the definitions of several TYPE structures,
  !  together with their allocation/deallocation routines

  USE kinds, ONLY: DP
  use radial_grids, ONLY: radial_grid_type

  IMPLICIT NONE
  SAVE
  !
  ! Additional data to make a PAW setup out of an US pseudo,
  ! they are all stored on a radial grid:
  TYPE paw_in_upf
     REAL(DP),POINTER :: ae_rho_atc(:) ! AE core charge (pseudo ccharge
     ! is already included in upf)
     REAL(DP),POINTER :: pfunc(:,:,:),&! Psi_i(r)*Psi_j(r)
          pfunc_rel(:,:,:), & ! Psi_i(r)*Psi_j(r) small component
          ptfunc(:,:,:), & ! as above, but for pseudo
          aewfc_rel(:,:) ! as above, but for pseudo
     REAL(DP),POINTER :: ae_vloc(:)    ! AE local potential (pseudo vloc
     ! is already included in upf)
     REAL(DP),POINTER :: oc(:)         ! starting occupation used to init becsum
     ! they differ from US ones because they
     ! are indexed on BETA functions, non on WFC
     REAL(DP),POINTER :: augmom(:,:,:) ! multipole AE-pseudo (i,j,l=0:2*lmax)
     REAL(DP)         :: raug          ! augfunction max radius
     INTEGER          :: iraug         ! index on rgrid closer to, and >, raug
     INTEGER          :: lmax_aug      ! max angmom of augmentation functions, it is ==
     ! to 2* max{l of pseudized wavefunctions}
     ! note that nqlc of upf also includes the angmom of
     ! empty virtual channel used to generate local potential
     REAL(DP)         :: core_energy   ! constant to add in order to get all-electron energy
     CHARACTER(len=12):: augshape      ! shape of augmentation charge
  END TYPE paw_in_upf


  TYPE pseudo_upf
     CHARACTER(LEN=80):: generated=' '! generator software
     CHARACTER(LEN=80):: author=' '   ! pseudopotential's author
     CHARACTER(LEN=80):: date=' '     ! generation date
     CHARACTER(LEN=80):: comment=' '  ! author's comment
     CHARACTER(LEN=2) :: psd=' '      ! Element label
     CHARACTER(LEN=20) :: typ=' '     ! Pseudo type ( NC or US or PAW)
     CHARACTER(len=6) :: rel=' '      ! relativistic: {no|scalar|full}
     LOGICAL :: tvanp              ! .true. if Ultrasoft
     LOGICAL :: tcoulombp          ! .true. if Coulomb 1/r potential
     LOGICAL :: nlcc               ! Non linear core corrections
     CHARACTER(LEN=25) :: dft      ! Exch-Corr type
     REAL(DP) :: zp                ! z valence
     REAL(DP) :: etotps            ! total energy
     REAL(DP) :: ecutwfc           ! suggested cut-off for wfc
     REAL(DP) :: ecutrho           ! suggested cut-off for rho
     !
     CHARACTER(len=11) :: nv       ! UPF file three-digit version i.e. 2.0.0
     INTEGER :: lmax               ! maximum l component in beta
     INTEGER :: lmax_rho           ! max l component in charge (should be 2*lmax)
     REAL(DP), POINTER :: vnl(:,:,:) ! vnl(i,l,s) = V(r_i)_{ls}
     ! only for single-channel NC PP
     ! Wavefunctions and projectors
     INTEGER :: nwfc               ! number of atomic wavefunctions
     INTEGER :: nbeta              ! number of projectors
     INTEGER,  POINTER :: kbeta(:) ! kbeta(nbeta) see below
     INTEGER :: kkbeta             ! kkbeta=max(kbeta(:))
     !  kbeta<=mesh is the number of grid points for each beta function
     !              beta(r,nb) = 0 for r > r(kbeta(nb))
     ! kkbeta<=mesh is the largest of such number so that for all beta
     !              beta(r,nb) = 0 for r > r(kkbeta)
     !
     INTEGER,  POINTER :: lll(:)     ! lll(nbeta) l of each projector
     REAL(DP), POINTER :: beta(:,:)  ! beta(mesh,nbeta) projectors
     !
     CHARACTER(LEN=2), POINTER :: els(:)  ! els(nwfc) label of wfc
     CHARACTER(LEN=2), POINTER :: els_beta(:)  ! els(nbeta) label of beta
     INTEGER, POINTER  :: nchi(:)    ! lchi(nwfc) value of pseudo-n for wavefcts
     INTEGER, POINTER  :: lchi(:)    ! lchi(nwfc) value of l for wavefcts
     REAL(DP), POINTER :: oc(:)      ! oc(nwfc) occupancies for wavefcts
     REAL(DP), POINTER :: epseu(:)   ! pseudo one-particle energy (nwfc)
     REAL(DP), POINTER :: rcut_chi(:)! rcut_chi(nwfc) cutoff inner radius
     REAL(DP), POINTER :: rcutus_chi(:)! rcutus_chi(nwfc) ultrasoft outer radius
     ! Chi and rho_at are only used for initial density and initial wfcs:
     REAL(DP), POINTER :: chi(:,:)   ! chi(mesh,nwfc) atomic wavefcts
     REAL(DP), POINTER :: rho_at(:)  ! rho_at(mesh) atomic charge
     ! Minimal radial grid:
     INTEGER :: mesh               ! number of points in the radial mesh
     REAL(DP) :: xmin              ! the minimum x of the linear mesh
     REAL(DP) :: rmax              ! the maximum radius of the mesh
     REAL(DP) :: zmesh             ! the nuclear charge used for mesh
     REAL(DP) :: dx                ! the deltax of the linear mesh
     REAL(DP), POINTER :: r(:)     ! r(mesh)  radial grid
     REAL(DP), POINTER :: rab(:)   ! rab(mesh) dr(x)/dx (x=linear grid)
     ! Pseudized core charge
     REAL(DP), POINTER :: rho_atc(:) ! rho_atc(mesh) atomic core charge
     ! Local potential
     INTEGER :: lloc                 ! L of channel used to generate local potential
     ! (if < 0 it was generated by smoothing AE potential)
     REAL(DP) :: rcloc               ! vloc = v_ae for r > rcloc
     REAL(DP), POINTER :: vloc(:)    ! vloc(mesh) local atomic potential
     !
     REAL(DP), POINTER :: dion(:,:)  ! dion(nbeta,nbeta) atomic D_{mu,nu}
     ! Augmentation
     LOGICAL :: q_with_l              ! if .true. qfunc is pseudized in
     ! different ways for different l
     INTEGER :: nqf                  ! number of Q coefficients
     INTEGER :: nqlc                 ! number of angular momenta in Q
     REAL(DP):: qqq_eps              ! qfunc is null if its norm is .lt. qqq_eps
     REAL(DP), POINTER :: rinner(:)  ! rinner(0:2*lmax) r_L
     REAL(DP), POINTER :: qqq(:,:)   ! qqq(nbeta,nbeta) q_{mu,nu}
     ! Augmentation without L dependecy
     REAL(DP), POINTER :: qfunc(:,:) ! qfunc(mesh,nbeta*(nbeta+1)/2)
     ! Q_{mu,nu}(|r|) function for |r|> r_L
     ! Augmentation depending on L (optional, compulsory for PAW)
     REAL(DP), POINTER :: qfuncl(:,:,:)!  qfuncl(mesh,nbeta*(nbeta+1)/2,l)
     ! Q_{mu,nu}(|r|) function for |r|> r_L
     ! Analitycal coeffs cor small r expansion of qfunc (Vanderbilt's code)
     REAL(DP), POINTER :: qfcoef(:,:,:,:) ! qfcoef(nqf,0:2*lmax,nbeta,nbeta)
     ! coefficients for Q for |r|<r_L
     ! All electron and pseudo wavefunction, pswfc differ from chi as they are
     ! one for each beta, not just some choosen for initial conditions
     LOGICAL           :: has_wfc    ! if true, UPF contain AE and PS wfc for each beta
     REAL(DP), POINTER :: aewfc(:,:) ! wfc(mesh,nbeta) all-electron wfc
     REAL(DP), POINTER :: pswfc(:,:) ! wfc(mesh,nbeta) pseudo wfc

     LOGICAL :: has_so             ! if .true. includes spin-orbit
     INTEGER, POINTER :: nn(:)     ! nn(nwfc) quantum number of wfc
     REAL(DP), POINTER :: rcut(:)  ! cut-off radius(nbeta)
     REAL(DP), POINTER :: rcutus(:)! ultrasoft cut-off radius (nbeta)
     REAL(DP), POINTER :: jchi(:)  ! jchi(nwfc) j=l+1/2 or l-1/2 of wfc
     REAL(DP), POINTER :: jjj(:)   ! jjj(nbeta) j=l+1/2 or l-1/2 of beta

     ! PAW:
     INTEGER :: paw_data_format      ! The version of the format
     LOGICAL  :: tpawp               ! true if atom is PAW, PAW data must be present
     TYPE(paw_in_upf) :: paw         ! additional data for PAW (see above)
     TYPE(radial_grid_type),POINTER :: grid ! pointer to the corresponding grid
     ! in radial_grids module

     ! GIPAW:
     LOGICAL  :: has_gipaw           ! Whether GIPAW data is included
     LOGICAL  :: paw_as_gipaw        !EMINE
     INTEGER  :: gipaw_data_format   ! The version of the format
     INTEGER  :: gipaw_ncore_orbitals
     REAL(DP), POINTER :: gipaw_core_orbital_n(:)
     REAL(DP), POINTER :: gipaw_core_orbital_l(:)
     CHARACTER(LEN=2), POINTER :: gipaw_core_orbital_el(:)
     REAL(DP), POINTER :: gipaw_core_orbital(:,:)
     REAL(DP), POINTER :: gipaw_vlocal_ae(:)
     REAL(DP), POINTER :: gipaw_vlocal_ps(:)
     INTEGER :: gipaw_wfs_nchannels
     CHARACTER(LEN=2), POINTER :: gipaw_wfs_el(:)
     INTEGER, POINTER :: gipaw_wfs_ll(:)
     REAL(DP), POINTER :: gipaw_wfs_ae(:,:)
     REAL(DP), POINTER :: gipaw_wfs_rcut(:)
     REAL(DP), POINTER :: gipaw_wfs_rcutus(:)
     REAL(DP), POINTER :: gipaw_wfs_ps(:,:)
     !
     !  MD5 checksum ... used to verify integrity of the information contained
     !  in the pseudopotential file w.r.t previous run
     !
     CHARACTER(len=32) :: MD5_cksum = 'NOT SET'

  END TYPE pseudo_upf

CONTAINS

  SUBROUTINE nullify_paw_in_upf( paw )
    TYPE( paw_in_upf ), INTENT(INOUT) :: paw
    NULLIFY( paw%ae_rho_atc )
    NULLIFY( paw%aewfc_rel )
    NULLIFY( paw%pfunc )
    NULLIFY( paw%pfunc_rel )
    NULLIFY( paw%ptfunc )
    NULLIFY( paw%ae_vloc )
    NULLIFY( paw%augmom )
    NULLIFY( paw%oc )
  END SUBROUTINE nullify_paw_in_upf

  SUBROUTINE deallocate_paw_in_upf( paw )
    TYPE( paw_in_upf ), INTENT(INOUT) :: paw
    IF( ASSOCIATED( paw%ae_rho_atc ) ) DEALLOCATE ( paw%ae_rho_atc )
    IF( ASSOCIATED( paw%aewfc_rel ) )  DEALLOCATE (paw%aewfc_rel )
    IF( ASSOCIATED( paw%pfunc ) )      DEALLOCATE ( paw%pfunc )
    IF( ASSOCIATED( paw%pfunc_rel ) )  DEALLOCATE ( paw%pfunc_rel )
    IF( ASSOCIATED( paw%ptfunc ) )     DEALLOCATE ( paw%ptfunc )
    IF( ASSOCIATED( paw%ae_vloc )  )   DEALLOCATE ( paw%ae_vloc )
    IF( ASSOCIATED( paw%augmom ) )     DEALLOCATE ( paw%augmom )
    IF( ASSOCIATED( paw%oc ) )         DEALLOCATE ( paw%oc )
  END SUBROUTINE deallocate_paw_in_upf
  !
  !
  SUBROUTINE nullify_pseudo_upf( upf )
    TYPE( pseudo_upf ), INTENT(INOUT) :: upf
    CALL nullify_paw_in_upf( upf%paw )
    NULLIFY( upf%grid ) 
    NULLIFY( upf%els, upf%lchi, upf%nchi, upf%jchi, upf%oc )
    NULLIFY( upf%r, upf%rab )
    NULLIFY( upf%rho_atc, upf%vloc )
    NULLIFY( upf%nn)
    NULLIFY( upf%els_beta)
    NULLIFY( upf%rcut, upf%rcutus, upf%rcut_chi, upf%rcutus_chi )
    NULLIFY( upf%epseu)
    NULLIFY( upf%vnl)
    NULLIFY( upf%lll, upf%jjj, upf%kbeta, upf%beta, upf%dion )
    NULLIFY( upf%aewfc, upf%pswfc )
    NULLIFY( upf%rinner, upf%qqq, upf%qfunc, upf%qfuncl, upf%qfcoef )
    NULLIFY( upf%chi )
    NULLIFY( upf%rho_at )
    NULLIFY ( upf%gipaw_core_orbital_n )
    NULLIFY ( upf%gipaw_core_orbital_l )
    NULLIFY ( upf%gipaw_core_orbital_el )
    NULLIFY ( upf%gipaw_core_orbital )
    NULLIFY ( upf%gipaw_vlocal_ae )
    NULLIFY ( upf%gipaw_vlocal_ps )
    NULLIFY ( upf%gipaw_wfs_el )
    NULLIFY ( upf%gipaw_wfs_ll )
    NULLIFY ( upf%gipaw_wfs_ae )
    NULLIFY ( upf%gipaw_wfs_rcut )
    NULLIFY ( upf%gipaw_wfs_rcutus )
    NULLIFY ( upf%gipaw_wfs_ps )
    RETURN
  END SUBROUTINE nullify_pseudo_upf

  SUBROUTINE deallocate_pseudo_upf( upf )
    TYPE( pseudo_upf ), INTENT(INOUT) :: upf
    CALL deallocate_paw_in_upf( upf%paw )
    IF( ASSOCIATED( upf%els ) )     DEALLOCATE( upf%els )
    IF( ASSOCIATED( upf%lchi ) )    DEALLOCATE( upf%lchi )
    IF( ASSOCIATED( upf%nchi ) )    DEALLOCATE( upf%nchi )
    IF( ASSOCIATED( upf%jchi ) )    DEALLOCATE( upf%jchi )
    IF( ASSOCIATED( upf%oc ) )      DEALLOCATE( upf%oc )
    !
    IF(ASSOCIATED(upf%grid)) THEN
       IF( ASSOCIATED( upf%r ) ) NULLIFY( upf%r )
       IF( ASSOCIATED( upf%rab ) ) NULLIFY( upf%rab )
       NULLIFY(upf%grid)
    ELSE
       IF( ASSOCIATED( upf%r ) ) DEALLOCATE( upf%r )
       IF( ASSOCIATED( upf%rab ) ) DEALLOCATE( upf%rab )
    ENDIF
    !
    IF( ASSOCIATED( upf%nn ) )      DEALLOCATE( upf%nn )
    IF( ASSOCIATED( upf%els_beta ) )DEALLOCATE( upf%els_beta )
    IF( ASSOCIATED( upf%rcut_chi ) )  DEALLOCATE( upf%rcut_chi )
    IF( ASSOCIATED( upf%rcutus_chi ) )DEALLOCATE( upf%rcutus_chi )
    IF( ASSOCIATED( upf%rcut ) )    DEALLOCATE( upf%rcut )
    IF( ASSOCIATED( upf%rcutus ) )  DEALLOCATE( upf%rcutus )
    IF( ASSOCIATED( upf%epseu ) )   DEALLOCATE( upf%epseu )
    IF( ASSOCIATED( upf%rho_atc ) ) DEALLOCATE( upf%rho_atc )
    IF( ASSOCIATED( upf%vloc ) )    DEALLOCATE( upf%vloc )
    IF( ASSOCIATED( upf%lll ) )     DEALLOCATE( upf%lll )
    IF( ASSOCIATED( upf%jjj ) )     DEALLOCATE( upf%jjj )
    IF( ASSOCIATED( upf%kbeta ) )   DEALLOCATE( upf%kbeta )
    IF( ASSOCIATED( upf%beta ) )    DEALLOCATE( upf%beta )
    IF( ASSOCIATED( upf%vnl ) )     DEALLOCATE( upf%vnl )
    IF( ASSOCIATED( upf%aewfc ) )   DEALLOCATE( upf%aewfc )
    IF( ASSOCIATED( upf%pswfc ) )   DEALLOCATE( upf%pswfc )
    IF( ASSOCIATED( upf%dion ) )    DEALLOCATE( upf%dion )
    IF( ASSOCIATED( upf%rinner ) )  DEALLOCATE( upf%rinner )
    IF( ASSOCIATED( upf%qqq ) )     DEALLOCATE( upf%qqq )
    IF( ASSOCIATED( upf%qfunc ) )   DEALLOCATE( upf%qfunc )
    IF( ASSOCIATED( upf%qfuncl ) )  DEALLOCATE( upf%qfuncl )
    IF( ASSOCIATED( upf%qfcoef ) )  DEALLOCATE( upf%qfcoef )
    IF( ASSOCIATED( upf%chi ) )     DEALLOCATE( upf%chi )
    IF( ASSOCIATED( upf%rho_at ) )  DEALLOCATE( upf%rho_at )
    IF ( ASSOCIATED ( upf%gipaw_core_orbital_n ) ) &
         DEALLOCATE ( upf%gipaw_core_orbital_n )
    IF ( ASSOCIATED ( upf%gipaw_core_orbital_l ) ) &
         DEALLOCATE ( upf%gipaw_core_orbital_l )
    IF ( ASSOCIATED ( upf%gipaw_core_orbital_el ) ) &
         DEALLOCATE ( upf%gipaw_core_orbital_el )
    IF ( ASSOCIATED ( upf%gipaw_core_orbital ) ) &
         DEALLOCATE ( upf%gipaw_core_orbital )
    IF ( ASSOCIATED ( upf%gipaw_vlocal_ae ) ) &
         DEALLOCATE ( upf%gipaw_vlocal_ae )
    IF ( ASSOCIATED ( upf%gipaw_vlocal_ps ) ) &
         DEALLOCATE ( upf%gipaw_vlocal_ps )
    IF ( ASSOCIATED ( upf%gipaw_wfs_el ) ) &
         DEALLOCATE ( upf%gipaw_wfs_el )
    IF ( ASSOCIATED ( upf%gipaw_wfs_ll ) ) &
         DEALLOCATE ( upf%gipaw_wfs_ll )
    IF ( ASSOCIATED ( upf%gipaw_wfs_ae ) ) &
         DEALLOCATE ( upf%gipaw_wfs_ae )
    IF ( ASSOCIATED ( upf%gipaw_wfs_rcut ) ) &
         DEALLOCATE ( upf%gipaw_wfs_rcut )
    IF ( ASSOCIATED ( upf%gipaw_wfs_rcutus ) ) &
         DEALLOCATE ( upf%gipaw_wfs_rcutus )
    IF ( ASSOCIATED ( upf%gipaw_wfs_ps ) ) &
         DEALLOCATE ( upf%gipaw_wfs_ps )
    RETURN
  END SUBROUTINE deallocate_pseudo_upf

END MODULE pseudo_types
