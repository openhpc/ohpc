!
! Copyright (C) 2001-2012 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!--------------------------------------------------------------------------
!
MODULE basis
  !
  ! ... The variables needed to describe the atoms in the unit cell
  !
  SAVE
  !
  INTEGER :: &
       natomwfc            ! number of starting wavefunctions
  CHARACTER(len=30) ::    &!
       starting_wfc,      &! 'random' or 'atomic' or 'atomic+randm' or 'file'
       starting_pot,      &! 'atomic' or 'file'
       startingconfig      ! 'input' or 'file'
  !
END MODULE basis
!
!
MODULE klist
  !
  ! ... The variables for the k-points
  !
  USE kinds,      ONLY : DP
  USE parameters, ONLY : npk
  !
  SAVE
  !
  CHARACTER (len=32) :: &
       smearing            ! smearing type
  REAL(DP) :: &
       xk(3,npk),         &! coordinates of k points
       wk(npk),           &! weight of k points
       xqq(3),            &! coordinates of q point (used in the ACFDT part)
       degauss,           &! smearing parameter
       nelec,             &! number of electrons
       nelup=0.0_dp,      &! number of spin-up electrons (if two_fermi_energies=t)
       neldw=0.0_dp,      &! number of spin-dw electrons (if two_fermi_energies=t)
       tot_magnetization, &! nelup-neldw >= 0 (negative value means unspecified)
       tot_charge
  REAL(DP) :: &
       qnorm= 0.0_dp      ! |q|, used in phonon+US calculations only
  INTEGER, ALLOCATABLE :: &
       ngk(:)              ! number of plane waves for each k point
  INTEGER :: &
       nks,               &! number of k points in this pool
       nkstot,            &! total number of k points
       ngauss              ! type of smearing technique
  LOGICAL :: &
       lgauss,         &! if .TRUE.: use gaussian broadening
       lxkcry=.false., &! if .TRUE.:k-pnts in cryst. basis accepted in input
       two_fermi_energies ! if .TRUE.: nelup and neldw set ef_up and ef_dw
                          ! separately
  !
END MODULE klist
!
!
MODULE lsda_mod
  !
  ! ... The variables needed for the lsda calculation
  !
  USE kinds,      ONLY : DP
  USE parameters, ONLY : ntypx, npk
  !
  SAVE
  !
  LOGICAL :: &
       lsda
  REAL(DP) :: &
       magtot,                       &! total magnetization
       absmag,                       &! total absolute magnetization
       starting_magnetization(ntypx)  ! the magnetization used to start with
  INTEGER :: &
       nspin,           &! number of spin polarization: 2 if lsda, 1 other
       current_spin,    &! spin of the current kpoint
       isk(npk)          ! for each k-point: 1=spin up, 2=spin down
  !
END MODULE lsda_mod
!
!
MODULE ktetra
  !
  ! ... The variables for the tetrahedron method
  !
  SAVE
  !
  INTEGER :: &
       ntetra            ! number of tetrahedra
  INTEGER, ALLOCATABLE :: &
       tetra(:,:)        ! index of k-points in a given tetrahedron
                         ! shape (4,ntetra)
  LOGICAL :: &
       ltetra            ! if .TRUE.: use tetrahedron method
  !
END MODULE ktetra
!
!
MODULE rap_point_group
   !
   USE kinds,      ONLY : DP
   !
   INTEGER :: &
          code_group,  &   ! The code of the point group
          nclass,  &       ! The number of classes of the point group
          nelem(12),   &   ! The elements of each class
          elem(8,12),  &   ! Which elements in the smat list for each class
          which_irr(12)    ! For each class gives its position in the
                           ! character table.
   !
   COMPLEX(DP) :: char_mat(12,12)       ! the character tables: rap,class

   CHARACTER(len=15) :: name_rap(12)  ! the name of the representation
   CHARACTER(len=3)  :: ir_ram(12)    ! a string I, R or I+R for infrared,
                                      !  Raman, or infrared+raman modes.
   CHARACTER(len=11) :: gname         ! the name of the group
   CHARACTER(len=5) :: name_class(12) ! the name of the class
   !
END MODULE rap_point_group

MODULE rap_point_group_so
   !
   USE kinds,      ONLY : DP
   !
   INTEGER :: &
          nrap,    &       ! The number of classes of the point group
          nelem_so(24),   &! The elements of each class
          elem_so(12,24),  &! Which elements in the smat list for each class
          has_e(12,24),  & ! if -1 the smat is multiplied by -E
          which_irr_so(24) ! For each class gives its position in the
                           ! character table.
   !
   COMPLEX(DP) :: char_mat_so(12,24),  &   ! the character tables
                  d_spin(2,2,48)           ! the rotation in spin space

   CHARACTER(len=15) :: name_rap_so(12)  ! the name of the representation
   CHARACTER(len=5) :: name_class_so(24), &  ! the name of the class
                       name_class_so1(24)  ! the name of the class
   !
END MODULE rap_point_group_so
!
MODULE rap_point_group_is
   !
   USE kinds,      ONLY : DP
   !
   INTEGER :: &
          ftau_is(3,48), & ! The fractional transl. of the invariant subgroup
          nsym_is,       & ! The number of operations of the invariant subgroup
          code_group_is    ! The code of the point invariant subgroup

   REAL(DP) :: &
          sr_is(3,3,48)    ! The matrices of the invariant subgroup

   COMPLEX(DP) :: &
                d_spin_is(2,2,48)      ! the rotation in spin space

   CHARACTER(len=45) :: sname_is(48)   ! name of the symmetries
   CHARACTER(len=11) :: gname_is       ! the name of the invariant group
   !
END MODULE rap_point_group_is
!
MODULE vlocal
  !
  ! ... The variables needed for the local potential in reciprocal space
  !
  USE kinds, ONLY : DP
  !
  SAVE
  !
  COMPLEX(DP), ALLOCATABLE :: &
       strf(:,:)              ! the structure factor
  REAL(DP), ALLOCATABLE :: &
       vloc(:,:)              ! the local potential for each atom type
  !
END MODULE vlocal
!
!
MODULE wvfct
  !
  ! ... The variables needed to compute the band structure
  !
  USE kinds, ONLY : DP
  !
  SAVE
  !
  INTEGER ::  &
       npwx,             &! maximum number of PW for wavefunctions
       nbndx,            &! max number of bands use in iterative diag
       nbnd,             &! number of bands
       npw,              &! the number of plane waves
       current_k          ! the index of k-point under consideration
  INTEGER, ALLOCATABLE, TARGET :: &
       igk(:)             ! index of G corresponding to a given index of k+G
  REAL(DP) :: &
       ecutwfc,       &! energy cut-off
       ecfixed,       &!
       qcutz = 0.0_DP,&! For the modified Ekin functional
       q2sigma         !
  REAL(DP), ALLOCATABLE :: &
       et(:,:),          &! eigenvalues of the hamiltonian
       wg(:,:),          &! the weight of each k point and band
       g2kin(:)           ! kinetic energy
  INTEGER, ALLOCATABLE :: &
       btype(:,:)         ! one if the corresponding state has to be
                          ! converged to full accuracy, zero otherwise
  !
END MODULE wvfct
!
!
MODULE ener
  !
  ! ... The variables needed to compute the energies
  !
  USE kinds, ONLY : DP
  !
  SAVE
  !
  REAL(DP) :: &
       etot,           &! the total Kohn-Sham energy of the solid
       hwf_energy,     &! this is the Harris-Weinert-Foulkes energy
       eband,          &! the band energy
       deband,         &! scf correction to have variational energy
       ehart,          &! the hartree energy
       etxc,           &! the exchange and correlation energy
       vtxc,           &! another exchange-correlation energy
       etxcc,          &! the nlcc exchange and correlation
       ewld,           &! the ewald energy
       elondon,        &! the semi-empirical dispersion energy
       demet,          &! variational correction ("-TS") for metals
       epaw,           &! sum of one-center paw contributions
       ef, ef_up, ef_dw ! the fermi energy (up and dw if two_fermi_energies=.T.)
  !
END MODULE ener
!
!
MODULE force_mod
  !
  ! ... The variables for the first derivative of the energy
  !
  USE kinds, ONLY : DP
  !
  SAVE
  !
  REAL(DP), ALLOCATABLE :: &
       force(:,:)       ! the force on each atom
  REAL(DP) :: &
       sigma(3,3)       ! the stress acting on the system
  LOGICAL :: &
       lforce,         &! if .TRUE. compute the forces
       lstres           ! if .TRUE. compute the stress
  !
END MODULE force_mod
!
MODULE relax
  !
  ! ... The variables used to control ionic relaxations
  !
  USE kinds, ONLY : DP
  !
  SAVE
  !
  REAL(DP) :: &
       epse,                    &! threshold on total energy
       epsf,                    &! threshold on forces
       epsp,                    &! threshold on pressure
       starting_scf_threshold    ! self-explanatory
  !
END MODULE relax
!
!
!
!
MODULE us
  !
  ! ... These parameters are needed with the US pseudopotentials
  !
  USE kinds,      ONLY : DP
  !
  SAVE
  !
  INTEGER :: &
       nqxq,             &! size of interpolation table
       nqx                ! number of interpolation points
  REAL(DP), PARAMETER:: &
       dq = 0.01D0           ! space between points in the pseudopotential tab.
  REAL(DP), ALLOCATABLE :: &
       qrad(:,:,:,:),         &! radial FT of Q functions
       tab(:,:,:),            &! interpolation table for PPs
       tab_at(:,:,:)           ! interpolation table for atomic wfc
  LOGICAL :: spline_ps = .false.
  REAL(DP), ALLOCATABLE :: &
       tab_d2y(:,:,:)            ! for cubic splines
  !
END MODULE us
!
!
!!$MODULE ldaU
!!$  !
!!$  ! ... The quantities needed in lda+U calculations
!!$  !
!!$  USE kinds,      ONLY : DP
!!$  USE parameters, ONLY : lqmax, ntypx
!!$  !
!!$  SAVE
!!$  !
!!$  INTEGER, PARAMETER :: nspinx=2
!!$  COMPLEX(DP), ALLOCATABLE :: &
!!$       swfcatom(:,:),         &! orthogonalized atomic wfcs
!!$       d_spin_ldau(:,:,:)      ! the rotations in spin space for all the symmetries
!!$  REAL(DP) :: &
!!$       eth,                  &! the Hubbard contribution to the energy
!!$       Hubbard_U(ntypx),     &! the Hubbard U
!!$       Hubbard_J(3,ntypx),   &! extra Hubbard parameters:  
!!$                              !     p: J(1) = J
!!$                              !     d: J(1) = J, J(2) =  B 
!!$                              !     f: J(1) = J, J(2) = E2, J(3) = E3 
!!$       Hubbard_alpha(ntypx), &! the Hubbard alpha (used to calculate U)
!!$       starting_ns(lqmax,nspinx,ntypx) !
!!$  INTEGER :: &
!!$       niter_with_fixed_ns,  &! no. of iterations with fixed ns
!!$       lda_plus_u_kind,      &! 1/0 --> full/simplified(old) LDA+U calculation  
!!$       Hubbard_l(ntypx),     &! the angular momentum of Hubbard states
!!$       Hubbard_lmax = 0       ! maximum angular momentum of Hubbard states
!!$  LOGICAL :: &
!!$       lda_plus_u,           &! .TRUE. if lda+u calculation is performed
!!$       conv_ns                ! .TRUE. if ns are converged
!!$  CHARACTER(len=30) :: &      ! 'atomic', 'ortho-atomic', 'file'
!!$       U_projection           ! specifies how input coordinates are given
!!$  INTEGER, ALLOCATABLE :: &
!!$       oatwfc(:)              ! offset of atomic wfcs used for projections
!!$  !
!!$END MODULE ldaU
!
!
MODULE extfield
  !
  ! ... The quantities needed in calculations with external field
  !
  USE kinds, ONLY : DP
  !
  SAVE
  !
  LOGICAL :: &
       tefield,      &! if .TRUE. a finite electric field is added to the
                      ! local potential
       dipfield       ! if .TRUE. the dipole field is subtracted
  INTEGER :: &
       edir           ! direction of the field
  REAL(DP) :: &
      emaxpos,       &! position of the maximum of the field (0<emaxpos<1)
      eopreg,        &! amplitude of the inverse region (0<eopreg<1)
      eamp,          &! field amplitude (in a.u.) (1 a.u. = 51.44 10^11 V/m)
      etotefield      ! energy correction due to the field
  REAL(DP), ALLOCATABLE :: &
      forcefield(:,:)
  !
END MODULE extfield
!
!
!
MODULE fixed_occ
  !
  ! ... The quantities needed in calculations with fixed occupations
  !
  USE kinds,      ONLY : DP
  !
  SAVE
  !
  REAL(DP), ALLOCATABLE :: &
       f_inp(:,:)             ! the occupations for each spin
  LOGICAL :: &
       tfixed_occ, &          ! if .TRUE. the occupations are fixed.
       one_atom_occupations   ! if .TRUE. the occupations are decided
                              !  according to the projections of the
                              !  wavefunctions on the initial atomic
                              !  wavefunctions (to be used only
                              !  for an isolated atom)
  !
END MODULE fixed_occ

MODULE spin_orb

  USE kinds, ONLY: DP
  USE parameters, ONLY : lmaxx

  SAVE

  LOGICAL :: &
      lspinorb,  &       ! if .TRUE. this is a spin-orbit calculation
      starting_spin_angle, & ! if .TRUE. the initial wavefunctions are 
                             ! spin-angle functions. 
      domag              ! if .TRUE. magnetization is computed


  COMPLEX (DP) :: rot_ylm(2*lmaxx+1,2*lmaxx+1)  ! transform real
                         ! spherical harmonics into complex ones
  COMPLEX (DP), ALLOCATABLE :: fcoef(:,:,:,:,:) ! function needed to
                         ! account for spinors.
END MODULE spin_orb
!
MODULE pwcom
  !
  USE constants, ONLY : e2, rytoev, amconv, pi, tpi, fpi
  USE cell_base, ONLY : celldm, at, bg, alat, omega, tpiba, tpiba2, ibrav
  USE basis
  USE gvect
  USE gvecs
  USE klist
  USE lsda_mod
  USE vlocal
  USE wvfct
  USE ener
  USE force_mod
  USE relax
!!$  USE cellmd
  USE us
!!$  USE ldaU
  USE extfield
  USE fixed_occ
  USE spin_orb
  !
END MODULE pwcom
