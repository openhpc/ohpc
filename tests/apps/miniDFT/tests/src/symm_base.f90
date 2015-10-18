!
! Copyright (C) 2010-2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!--------------------------------------------------------------------------
!
MODULE symm_base
  
  USE kinds,      ONLY : DP
  USE cell_base,  ONLY : at, bg
  !
  ! ... The variables needed to describe the symmetry properties
  ! ... and the routines to find crystal symmetries
  !  
  ! ... these are acceptance criteria
  !
  REAL(DP), parameter :: eps1 = 1.0d-6, eps2 = 1.0d-5
  !
  SAVE
  !
  PRIVATE
  !
  ! ... Exported variables
  !
  PUBLIC :: s, sr, sname, ft, ftau, nrot, nsym, nsym_ns, nsym_na, t_rev, &
            no_t_rev, time_reversal, irt, invs, invsym, d1, d2, d3, &
            allfrac, nofrac, nosym, nosym_evc
  INTEGER :: &
       s(3,3,48),            &! symmetry matrices, in crystal axis
       invs(48),             &! index of inverse operation: S^{-1}_i=S(invs(i))
       ftau(3,48),           &! fractional translations, in FFT coordinates
       nrot,                 &! number of bravais lattice symmetries 
       nsym = 1,             &! total number of crystal symmetries
       nsym_ns = 0,          &! nonsymmorphic (fractional translation) symms
       nsym_na = 0            ! excluded nonsymmorphic symmetries because
                              ! fract. transl. is noncommensurate with FFT grid
  REAL (DP) :: &
       ft (3,48),            &! fractional translations, in crystal axis
       sr (3,3,48)            ! symmetry matrices, in cartesian axis
  !
  ! ... note: ftau are used for symmetrization in real space (phonon, exx)
  ! ... in which case they must be commensurated with the FFT grid
  !
  CHARACTER(LEN=45) ::  sname(48)   ! name of the symmetries
  INTEGER :: &
       t_rev(48) = 0          ! time reversal flag, for noncolinear magnetism
  INTEGER, ALLOCATABLE :: &
       irt(:,:)               ! symmetric atom for each atom and sym.op.
  LOGICAL :: &
       time_reversal=.true., &! if .TRUE. the system has time reversal symmetry
       invsym,               &! if .TRUE. the system has inversion symmetry
       nofrac= .FALSE.,      &! if .TRUE. fract. translations are not allowed
       allfrac= .FALSE.,     &! if .TRUE. all fractionary transations allowed,
                              ! even those not commensurate with FFT grid
       nosym = .FALSE.,      &! if .TRUE. no symmetry is used
       nosym_evc = .FALSE.,  &! if .TRUE. symmetry is used only to symmetrize
                              ! k points
       no_t_rev=.FALSE.       ! if .TRUE. remove the symmetries that 
                              ! require time reversal               
  REAL(DP),TARGET :: &
       d1(3,3,48),           &! matrices for rotating spherical
       d2(5,5,48),           &! harmonics (d1 for l=1, ...)
       d3(7,7,48)             !
  !
  ! ... Exported routines
  !
  PUBLIC ::  find_sym, inverse_s, copy_sym, checkallsym, &
             s_axis_to_cart, set_sym, set_sym_bl
  !
CONTAINS
   !
   SUBROUTINE inverse_s ( )
     !-----------------------------------------------------------------------
     !
     ! Locate index of S^{-1}
     !
     IMPLICIT NONE
     !
     INTEGER :: isym, jsym, ss (3, 3)
     LOGICAL :: found
     !
     DO isym = 1, nsym
        found = .FALSE.
        DO jsym = 1, nsym
           !
           ss = MATMUL (s(:,:,jsym),s(:,:,isym))
           ! s(:,:,1) is the identity
           IF ( ALL ( s(:,:,1) == ss(:,:) ) ) THEN
              invs (isym) = jsym
              found = .TRUE.
           END IF
        END DO
        IF ( .NOT.found) CALL errore ('inverse_s', ' Not a group', 1)
     END DO
     !
   END SUBROUTINE inverse_s 
   !
!-----------------------------------------------------------------------
subroutine set_sym_bl ( )
  !-----------------------------------------------------------------------
  !
  ! Provides symmetry operations for all bravais lattices
  ! Tests first the 24 proper rotations for the cubic lattice; 
  ! then the 8 rotations specific for the hexagonal axis (special axis c);
  ! then inversion is added
  !
  implicit none
  !
  ! sin3 = sin(pi/3), cos3 = cos(pi/3), msin3 = -sin(pi/3), mcos3 = -cos(pi/3)
  !
  real(DP), parameter :: sin3 = 0.866025403784438597d0, cos3 = 0.5d0, &
                             msin3 =-0.866025403784438597d0, mcos3 = -0.5d0
  real(DP) :: s0(3, 3, 32), overlap (3, 3), rat (3), rot (3, 3), value
  ! s0: the s matrices in cartesian axis
  ! overlap: inverse overlap matrix between direct lattice
  ! rat: the rotated of a direct vector ( cartesian )
  ! rot: the rotated of a direct vector ( crystal axis )
  ! value: component of the s matrix in axis basis
  integer :: jpol, kpol, mpol, irot
  ! counters over the polarizations and the rotations

  character :: s0name (64) * 45
  ! full name of the rotational part of each symmetry operation

  data s0/ 1.d0,  0.d0,  0.d0,  0.d0,  1.d0,  0.d0,  0.d0,  0.d0,  1.d0, &
          -1.d0,  0.d0,  0.d0,  0.d0, -1.d0,  0.d0,  0.d0,  0.d0,  1.d0, &
          -1.d0,  0.d0,  0.d0,  0.d0,  1.d0,  0.d0,  0.d0,  0.d0, -1.d0, &
           1.d0,  0.d0,  0.d0,  0.d0, -1.d0,  0.d0,  0.d0,  0.d0, -1.d0, &
           0.d0,  1.d0,  0.d0,  1.d0,  0.d0,  0.d0,  0.d0,  0.d0, -1.d0, &
           0.d0, -1.d0,  0.d0, -1.d0,  0.d0,  0.d0,  0.d0,  0.d0, -1.d0, &
           0.d0, -1.d0,  0.d0,  1.d0,  0.d0,  0.d0,  0.d0,  0.d0,  1.d0, &
           0.d0,  1.d0,  0.d0, -1.d0,  0.d0,  0.d0,  0.d0,  0.d0,  1.d0, &
           0.d0,  0.d0,  1.d0,  0.d0, -1.d0,  0.d0,  1.d0,  0.d0,  0.d0, &
           0.d0,  0.d0, -1.d0,  0.d0, -1.d0,  0.d0, -1.d0,  0.d0,  0.d0, &
           0.d0,  0.d0, -1.d0,  0.d0,  1.d0,  0.d0,  1.d0,  0.d0,  0.d0, &
           0.d0,  0.d0,  1.d0,  0.d0,  1.d0,  0.d0, -1.d0,  0.d0,  0.d0, &
          -1.d0,  0.d0,  0.d0,  0.d0,  0.d0,  1.d0,  0.d0,  1.d0,  0.d0, &
          -1.d0,  0.d0,  0.d0,  0.d0,  0.d0, -1.d0,  0.d0, -1.d0,  0.d0, &
           1.d0,  0.d0,  0.d0,  0.d0,  0.d0, -1.d0,  0.d0,  1.d0,  0.d0, &
           1.d0,  0.d0,  0.d0,  0.d0,  0.d0,  1.d0,  0.d0, -1.d0,  0.d0, &
           0.d0,  0.d0,  1.d0,  1.d0,  0.d0,  0.d0,  0.d0,  1.d0,  0.d0, &
           0.d0,  0.d0, -1.d0, -1.d0,  0.d0,  0.d0,  0.d0,  1.d0,  0.d0, &
           0.d0,  0.d0, -1.d0,  1.d0,  0.d0,  0.d0,  0.d0, -1.d0,  0.d0, &
           0.d0,  0.d0,  1.d0, -1.d0,  0.d0,  0.d0,  0.d0, -1.d0,  0.d0, &
           0.d0,  1.d0,  0.d0,  0.d0,  0.d0,  1.d0,  1.d0,  0.d0,  0.d0, &
           0.d0, -1.d0,  0.d0,  0.d0,  0.d0, -1.d0,  1.d0,  0.d0,  0.d0, &
           0.d0, -1.d0,  0.d0,  0.d0,  0.d0,  1.d0, -1.d0,  0.d0,  0.d0, &
           0.d0,  1.d0,  0.d0,  0.d0,  0.d0, -1.d0, -1.d0,  0.d0,  0.d0, &
           cos3,  sin3, 0.d0, msin3,  cos3, 0.d0, 0.d0, 0.d0,  1.d0, &
           cos3, msin3, 0.d0,  sin3,  cos3, 0.d0, 0.d0, 0.d0,  1.d0, &
          mcos3,  sin3, 0.d0, msin3, mcos3, 0.d0, 0.d0, 0.d0,  1.d0, &
          mcos3, msin3, 0.d0,  sin3, mcos3, 0.d0, 0.d0, 0.d0,  1.d0, &
           cos3, msin3, 0.d0, msin3, mcos3, 0.d0, 0.d0, 0.d0, -1.d0, &
           cos3,  sin3, 0.d0,  sin3, mcos3, 0.d0, 0.d0, 0.d0, -1.d0, &
          mcos3, msin3, 0.d0, msin3,  cos3, 0.d0, 0.d0, 0.d0, -1.d0, &
          mcos3,  sin3, 0.d0,  sin3,  cos3, 0.d0, 0.d0, 0.d0, -1.d0 /

  data s0name/  'identity                                     ',&
                '180 deg rotation - cart. axis [0,0,1]        ',&
                '180 deg rotation - cart. axis [0,1,0]        ',&
                '180 deg rotation - cart. axis [1,0,0]        ',&
                '180 deg rotation - cart. axis [1,1,0]        ',&
                '180 deg rotation - cart. axis [1,-1,0]       ',&
                ' 90 deg rotation - cart. axis [0,0,-1]       ',&
                ' 90 deg rotation - cart. axis [0,0,1]        ',&
                '180 deg rotation - cart. axis [1,0,1]        ',&
                '180 deg rotation - cart. axis [-1,0,1]       ',&
                ' 90 deg rotation - cart. axis [0,1,0]        ',&
                ' 90 deg rotation - cart. axis [0,-1,0]       ',&
                '180 deg rotation - cart. axis [0,1,1]        ',&
                '180 deg rotation - cart. axis [0,1,-1]       ',&
                ' 90 deg rotation - cart. axis [-1,0,0]       ',&
                ' 90 deg rotation - cart. axis [1,0,0]        ',&
                '120 deg rotation - cart. axis [-1,-1,-1]     ',&
                '120 deg rotation - cart. axis [-1,1,1]       ',&
                '120 deg rotation - cart. axis [1,1,-1]       ',&
                '120 deg rotation - cart. axis [1,-1,1]       ',&
                '120 deg rotation - cart. axis [1,1,1]        ',&
                '120 deg rotation - cart. axis [-1,1,-1]      ',&
                '120 deg rotation - cart. axis [1,-1,-1]      ',&
                '120 deg rotation - cart. axis [-1,-1,1]      ',&
                ' 60 deg rotation - cryst. axis [0,0,1]       ',&
                ' 60 deg rotation - cryst. axis [0,0,-1]      ',&
                '120 deg rotation - cryst. axis [0,0,1]       ',&
                '120 deg rotation - cryst. axis [0,0,-1]      ',&
                '180 deg rotation - cryst. axis [1,-1,0]      ',&
                '180 deg rotation - cryst. axis [2,1,0]       ',&
                '180 deg rotation - cryst. axis [0,1,0]       ',&
                '180 deg rotation - cryst. axis [1,1,0]       ',&
                'inversion                                    ',&
                'inv. 180 deg rotation - cart. axis [0,0,1]   ',&
                'inv. 180 deg rotation - cart. axis [0,1,0]   ',&
                'inv. 180 deg rotation - cart. axis [1,0,0]   ',&
                'inv. 180 deg rotation - cart. axis [1,1,0]   ',&
                'inv. 180 deg rotation - cart. axis [1,-1,0]  ',&
                'inv.  90 deg rotation - cart. axis [0,0,-1]  ',&
                'inv.  90 deg rotation - cart. axis [0,0,1]   ',&
                'inv. 180 deg rotation - cart. axis [1,0,1]   ',&
                'inv. 180 deg rotation - cart. axis [-1,0,1]  ',&
                'inv.  90 deg rotation - cart. axis [0,1,0]   ',&
                'inv.  90 deg rotation - cart. axis [0,-1,0]  ',&
                'inv. 180 deg rotation - cart. axis [0,1,1]   ',&
                'inv. 180 deg rotation - cart. axis [0,1,-1]  ',&
                'inv.  90 deg rotation - cart. axis [-1,0,0]  ',&
                'inv.  90 deg rotation - cart. axis [1,0,0]   ',&
                'inv. 120 deg rotation - cart. axis [-1,-1,-1]',&
                'inv. 120 deg rotation - cart. axis [-1,1,1]  ',&
                'inv. 120 deg rotation - cart. axis [1,1,-1]  ',&
                'inv. 120 deg rotation - cart. axis [1,-1,1]  ',&
                'inv. 120 deg rotation - cart. axis [1,1,1]   ',&
                'inv. 120 deg rotation - cart. axis [-1,1,-1] ',&
                'inv. 120 deg rotation - cart. axis [1,-1,-1] ',&
                'inv. 120 deg rotation - cart. axis [-1,-1,1] ',& 
                'inv.  60 deg rotation - cryst. axis [0,0,1]  ',&
                'inv.  60 deg rotation - cryst. axis [0,0,-1] ',&
                'inv. 120 deg rotation - cryst. axis [0,0,1]  ',&
                'inv. 120 deg rotation - cryst. axis [0,0,-1] ',&
                'inv. 180 deg rotation - cryst. axis [1,-1,0] ',&
                'inv. 180 deg rotation - cryst. axis [2,1,0]  ',&
                'inv. 180 deg rotation - cryst. axis [0,1,0]  ',&
                'inv. 180 deg rotation - cryst. axis [1,1,0]  ' /

  !    compute the overlap matrix for crystal axis

  do jpol = 1,3
     do kpol = 1,3
        rot(kpol,jpol) = at(1,kpol)*at(1,jpol) +&
                         at(2,kpol)*at(2,jpol) +&
                         at(3,kpol)*at(3,jpol)
     enddo
  enddo
  !
  !    then its inverse (rot is used as work space)
  !
  call invmat (3, rot, overlap, value)

  nrot = 1
  do irot = 1,32
     !
     !   for each possible symmetry
     !
     do jpol = 1,3
        do mpol = 1,3
           !
           !   compute, in cartesian coordinates the rotated vector
           !
           rat(mpol) = s0(mpol,1,irot)*at(1,jpol) +&
                       s0(mpol,2,irot)*at(2,jpol) +&
                       s0(mpol,3,irot)*at(3,jpol)
        enddo

        do kpol = 1,3
           !
           !   the rotated vector is projected on the direct lattice
           !
           rot(kpol,jpol) = at(1,kpol)*rat(1) +&
                            at(2,kpol)*rat(2) +&
                            at(3,kpol)*rat(3)
        enddo
     enddo
     !
     !  and the inverse of the overlap matrix is applied
     !
     do jpol = 1,3
        do kpol = 1,3
           value = overlap(jpol,1)*rot(1,kpol) +&
           &       overlap(jpol,2)*rot(2,kpol) +&
           &       overlap(jpol,3)*rot(3,kpol)
           if ( abs(DBLE(nint(value))-value) > eps1 ) then
              !
              ! if a noninteger is obtained, this implies that this operation
              ! is not a symmetry operation for the given lattice
              !
              go to 10
           end if
           s(kpol,jpol,nrot) = nint(value)
           sname(nrot)=s0name(irot)
        enddo
     enddo
     nrot = nrot+1
10   continue
  enddo
  nrot = nrot-1
  !
  !     set the inversion symmetry ( Bravais lattices have always inversion
  !     symmetry )
  !
  do irot = 1, nrot
     do kpol = 1,3
        do jpol = 1,3
           s(kpol,jpol,irot+nrot) = -s(kpol,jpol,irot)
           sname(irot+nrot) = s0name(irot+32)
        end do
     end do
  end do

  nrot = 2*nrot

  return
  !
end subroutine set_sym_bl
!
!-----------------------------------------------------------------------
SUBROUTINE find_sym ( nat, tau, ityp, nr1, nr2, nr3, magnetic_sym, m_loc )
  !-----------------------------------------------------------------------
  !
  !     This routine finds the point group of the crystal, by eliminating
  !     the symmetries of the Bravais lattice which are not allowed
  !     by the atomic positions (or by the magnetization if present)
  !
  implicit none
  !
  integer, intent(in) :: nat, ityp (nat), nr1, nr2, nr3  
  real(DP), intent(in) :: tau (3,nat), m_loc(3,1)
  logical, intent(in) :: magnetic_sym
  !
  logical :: sym (48)
  ! if true the corresponding operation is a symmetry operation
  !
  IF ( .NOT. ALLOCATED(irt) ) ALLOCATE( irt( 48, nat ) )
  irt( :, : ) = 0
  !
  !    Here we find the true symmetries of the crystal
  !
  CALL sgam_at ( nat, tau, ityp, nr1, nr2, nr3, sym )
  !
  !  If nosym_evc is true from now on we do not use the symmetry any more
  !
  IF (nosym_evc) THEN
     sym=.false.
     sym(1)=.true.
  ENDIF
  !
  !    Here we re-order all rotations in such a way that true sym.ops 
  !    are the first nsym; rotations that are not sym.ops. follow
  !
  nsym = copy_sym ( nrot, sym )
  !
  IF ( .not. is_group ( ) ) THEN
     CALL infomsg ('find_sym', 'Not a group! symmetry disabled')
     nsym = 1
  END IF
  !
  ! check if inversion (I) is a symmetry.
  ! If so, it should be the (nsym/2+1)-th operation of the group
  !
  invsym = ALL ( s(:,:,nsym/2+1) == -s(:,:,1) )
  !
  CALL inverse_s ( ) 
  !
  CALL s_axis_to_cart ( ) 
  !
  return
  !
END SUBROUTINE find_sym
!
!-----------------------------------------------------------------------
subroutine sgam_at ( nat, tau, ityp, nr1, nr2, nr3, sym )
  !-----------------------------------------------------------------------
  !
  !     Given the point group of the Bravais lattice, this routine finds 
  !     the subgroup which is the point group of the considered crystal.
  !     Non symmorphic groups are allowed, provided that fractional
  !     translations are allowed (nofrac=.false), that the unit cell is
  !     not a supercell, and that they are commensurate with the FFT grid
  !
  !     On output, the array sym is set to .true.. for each operation
  !     of the original point group that is also a symmetry operation 
  !     of the crystal symmetry point group
  !
  USE io_global,  ONLY : stdout
  USE kinds
  implicit none
  !
  integer, intent(in) :: nat, ityp (nat), nr1, nr2, nr3
  ! nat  : number of atoms in the unit cell
  ! ityp : species of each atom in the unit cell
  ! nr*  : dimensions of the FFT mesh
  !
  real(DP), intent(in) :: tau (3, nat)
  !
  ! tau  : cartesian coordinates of the atoms
  !
  !     output variables
  !
  logical, intent(out) :: sym (48)
  ! sym(isym)    : flag indicating if sym.op. isym in the parent group
  !                is a true symmetry operation of the crystal
  !
  integer :: na, kpol, nb, irot, i, j
  ! counters
  real(DP) , allocatable :: xau (:,:), rau (:,:)
  ! atomic coordinates in crystal axis
  logical :: fractional_translations
  real(DP) :: ft_(3), ft1, ft2, ft3
  !
  allocate(xau(3,nat))
  allocate(rau(3,nat))
  !
  !     Compute the coordinates of each atom in the basis of
  !     the direct lattice vectors
  !
  do na = 1, nat
     xau(:,na) = bg(1,:) * tau(1,na) + bg(2,:) * tau(2,na) + bg(3,:) * tau(3,na)
  enddo
  !
  !      check if the identity has fractional translations
  !      (this means that the cell is actually a supercell).
  !      When this happens, fractional translations are disabled,
  !      because there is no guarantee that the generated sym.ops.
  !      form a group
  !
  nb = 1
  irot = 1
  !
  fractional_translations = .not. nofrac
  do na = 2, nat
     if ( fractional_translations ) then
        if (ityp (nb) == ityp (na) ) then
           ft_(:) = xau(:,na) - xau(:,nb) - nint( xau(:,na) - xau(:,nb) )
           !
           sym(irot) = checksym ( irot, nat, ityp, xau, xau, ft_ )
           !
           if ( sym (irot) .and. &
               (abs (ft_(1) **2 + ft_(2) **2 + ft_(3) **2) < 1.d-8) ) &
               call errore ('sgam_at', 'overlapping atoms', na)
           if (sym (irot) ) then
              fractional_translations = .false.
              WRITE( stdout, '(5x,"Found symmetry operation: I + (",&
             &   3f8.4, ")",/,5x,"This is a supercell,", &
             &   " fractional translations are disabled")') ft_
           endif
        endif
     end if
  enddo
  !
  nsym_ns = 0 
  do irot = 1, nrot
     !
     ! check that the grid is compatible with the S rotation
     !
     if ( mod (s (2, 1, irot) * nr1, nr2) /= 0 .or. &
          mod (s (3, 1, irot) * nr1, nr3) /= 0 .or. &
          mod (s (1, 2, irot) * nr2, nr1) /= 0 .or. &
          mod (s (3, 2, irot) * nr2, nr3) /= 0 .or. &
          mod (s (1, 3, irot) * nr3, nr1) /= 0 .or. &
          mod (s (2, 3, irot) * nr3, nr2) /= 0 ) then
        sym (irot) = .false.
        WRITE( stdout, '(5x,"warning: symmetry operation # ",i2, &
             &         " not compatible with FFT grid. ")') irot
        WRITE( stdout, '(3i4)') ( (s (i, j, irot) , j = 1, 3) , i = 1, 3)
        goto 100
     endif

     do na = 1, nat
        ! rau = rotated atom coordinates
        rau (:, na) = s (1,:, irot) * xau (1, na) + &
                      s (2,:, irot) * xau (2, na) + &
                      s (3,:, irot) * xau (3, na)
     enddo
     !
     !      first attempt: no fractional translation
     !
     ftau (:, irot) = 0
     ft (:, irot) = 0
     ft_(:) = 0.d0
     !
     sym(irot) = checksym ( irot, nat, ityp, xau, rau, ft_ )
     !
     if (.not.sym (irot) .and. fractional_translations) then
        nb = 1
        do na = 1, nat
           if (ityp (nb) == ityp (na) ) then
              !
              !      second attempt: check all possible fractional translations
              !
              ft_ (:) = rau(:,na) - xau(:,nb) - nint( rau(:,na) - xau(:,nb) )
              !
              sym(irot) = checksym ( irot, nat, ityp, xau, rau, ft_ )
              !
              if (sym (irot) ) then
                 nsym_ns = nsym_ns + 1
                 ft (:,irot) = ft_(:)
                 go to 100
              end if
           endif
        enddo

     endif
100  continue
  enddo
  !
  ! convert ft to FFT coordinates, check if compatible with FFT grid
  ! for real-space symmetrization (if done: currently, exx, phonon)
  ! 
  nsym_na = 0
  do irot =1, nrot
     if ( sym(irot) .AND. .NOT. allfrac ) then
        ft1 = ft(1,irot) * nr1
        ft2 = ft(2,irot) * nr2
        ft3 = ft(3,irot) * nr3
        ! check if the fractional translations are commensurate
        ! with the FFT grid, discard sym.op. if not
        ! (needed because ph.x symmetrizes in real space)
        if (abs (ft1 - nint (ft1) ) / nr1 > eps2 .or. &
            abs (ft2 - nint (ft2) ) / nr2 > eps2 .or. &
            abs (ft3 - nint (ft3) ) / nr3 > eps2 ) then
            !     WRITE( stdout, '(5x,"warning: symmetry operation", &
            !          &     " # ",i2," not allowed.   fractional ", &
            !          &     "translation:"/5x,3f11.7,"  in crystal", &
            !          &     " coordinates")') irot, ft_
            sym (irot) = .false.
            nsym_na = nsym_na + 1
            nsym_ns = nsym_ns - 1
         endif
         ftau (1, irot) = nint (ft1)
         ftau (2, irot) = nint (ft2)
         ftau (3, irot) = nint (ft3)
      end if
  end do
  !
  !   deallocate work space
  !
  deallocate (rau)
  deallocate (xau)
  !
  return
end subroutine sgam_at
!
!-----------------------------------------------------------------------
subroutine sgam_at_mag ( nat, m_loc, sym )
  !-----------------------------------------------------------------------
  !
  !   Find magnetic symmetries, i.e. point-group symmetries that are
  !   also symmetries of the local magnetization - including
  !   rotation + time reversal operations
  !
  implicit none
  !
  integer, intent(in) :: nat
  real(DP), intent(in) :: m_loc(3, nat)
  !
  ! m_loc: local magnetization, must be invariant under the sym.op.
  !
  logical, intent(inout) :: sym (48)
  !
  ! sym(isym) = .true. if rotation isym is a sym.op. of the crystal
  !                    (i.e. not of the bravais lattice only)
  !
  integer :: na, nb, irot
  logical :: t1, t2
  real(DP) , allocatable ::  mxau(:,:), mrau(:,:)
  ! magnetization and rotated magnetization in crystal axis
  !
  allocate ( mxau(3,nat), mrau(3,nat) )
  !
  !     Compute the local magnetization of each atom in the basis of
  !     the direct lattice vectors
  !
  do na = 1, nat
     mxau (:, na)= bg (1, :) * m_loc (1, na) + &
                   bg (2, :) * m_loc (2, na) + &
                   bg (3, :) * m_loc (3, na)
  enddo
  !
  do irot = 1, nrot
     !
     t_rev(irot) = 0
     !
     if ( sym (irot) ) then
        !
        ! mrau = rotated local magnetization
        !
        do na = 1, nat
            mrau(:,na) = s(1,:,irot) * mxau(1,na) + &
                         s(2,:,irot) * mxau(2,na) + &
                         s(3,:,irot) * mxau(3,na)  
        enddo
        if (sname(irot)(1:3)=='inv') mrau = -mrau
        !
        ! check if this a magnetic symmetry
        !
        t1 = .true.
        t2 = .true.
        do na = 1, nat
           !
           nb = irt (irot,na)
           if ( nb < 1 .or. nb > nat ) call errore ('check_mag_sym', &
               'internal error: out-of-bound atomic index', na) 
           !
           t1 = ( abs(mrau(1,na) - mxau(1,nb)) +       &
                  abs(mrau(2,na) - mxau(2,nb)) +       &
                  abs(mrau(3,na) - mxau(3,nb)) < eps2 ) .and. t1
           t2 = ( abs(mrau(1,na) + mxau(1,nb))+       &
                  abs(mrau(2,na) + mxau(2,nb))+       &
                  abs(mrau(3,na) + mxau(3,nb)) < eps2 ) .and. t2
           !
        enddo
        !
        if ( .not.t1 .and. .not.t2 ) then
           ! not a magnetic symmetry
           sym(irot) = .false.
        else if( t2 .and. .not. t1 ) then
           ! magnetic symmetry with time reversal, if allowed
           IF (no_t_rev) THEN
              sym(irot) = .false.
           ELSE
              t_rev(irot) = 1
           ENDIF
        end if
        !
     end if
     !
  enddo
  !
  !   deallocate work space
  !
  deallocate ( mrau, mxau )
  !
  return
END SUBROUTINE sgam_at_mag
!
SUBROUTINE set_sym(nat, tau, ityp, nspin_mag, m_loc, nr1, nr2, nr3)
  !
  ! This routine receives as input atomic types and positions, if there
  ! is noncollinear magnetism and the initial magnetic moments, the fft
  ! dimensions nr1, nr2, nr3; it sets the symmetry elements of this module.
  ! Note that at and bg are those in cell_base. It sets nrot, nsym, s,
  ! sname, sr, invs, ftau, irt, t_rev,  time_reversal, and invsym
  ! 
  !-----------------------------------------------------------------------
  !
  IMPLICIT NONE
  ! input 
  INTEGER, INTENT(IN)  :: nat, ityp(nat), nspin_mag, nr1, nr2, nr3
  REAL(DP), INTENT(IN) :: tau(3,nat)
  REAL(DP), INTENT(IN) :: m_loc(3,1) !dummy variable
  !
  time_reversal = (nspin_mag /= 4)
  t_rev(:) = 0
  CALL set_sym_bl ( )
  CALL find_sym ( nat, tau, ityp, nr1, nr2, nr3, .not.time_reversal, m_loc )
  !
  RETURN
  END SUBROUTINE set_sym
!
!-----------------------------------------------------------------------
INTEGER FUNCTION copy_sym ( nrot_, sym ) 
!-----------------------------------------------------------------------
  !
  implicit none
  integer, intent(in) :: nrot_
  logical, intent(inout) :: sym(48)
  !
  integer :: stemp(3,3), ftemp(3), ttemp, irot, jrot
  REAL(dp) :: ft_(3)
  integer, allocatable :: irtemp(:)
  character(len=45) :: nametemp
  !
  ! copy symm. operations in sequential order so that
  ! s(i,j,irot) , irot <= nsym          are the sym.ops. of the crystal
  !               nsym+1 < irot <= nrot are the sym.ops. of the lattice
  ! on exit copy_sym returns nsym
  !
  allocate ( irtemp( size(irt,2) ) )
  jrot = 0
  do irot = 1, nrot_
     if (sym (irot) ) then
        jrot = jrot + 1
        if ( irot > jrot ) then 
           stemp = s(:,:,jrot)
           s (:,:, jrot) = s (:,:, irot)
           s (:,:, irot) = stemp
           ftemp(:) = ftau(:,jrot)
           ftau (:, jrot) = ftau (:, irot)
           ftau (:, irot) = ftemp(:)
           ft_(:) = ft(:,jrot)
           ft (:, jrot) = ft (:, irot)
           ft (:, irot) = ft_(:)
           irtemp (:) = irt (jrot,:)
           irt (jrot,:) = irt (irot,:)
           irt (irot,:) = irtemp (:)
           nametemp = sname (jrot)
           sname (jrot) = sname (irot)
           sname (irot) = nametemp
           ttemp = t_rev(jrot)
           t_rev(jrot) = t_rev(irot)
           t_rev(irot) = ttemp
        endif
     endif
  enddo
  sym (1:jrot) = .true.
  sym (jrot+1:nrot_) = .false.
  deallocate ( irtemp )
  !
  copy_sym = jrot
  return
  !
END FUNCTION copy_sym

!
!-----------------------------------------------------------------------
LOGICAL FUNCTION is_group ( )
  !-----------------------------------------------------------------------
  !
  !  Checks that {S} is a group 
  !
  IMPLICIT NONE
  !
  INTEGER :: isym, jsym, ksym, ss (3, 3)
  REAL (DP) :: st(3), dt(3)
  LOGICAL :: found
  !
  DO isym = 1, nsym
     DO jsym = 1, nsym
        ! 
        ss = MATMUL (s(:,:,isym),s(:,:,jsym))
        st(:)= ft(:,jsym) + s(1,:,jsym)*ft(1,isym) + &
                            s(2,:,jsym)*ft(2,isym) + &
                            s(3,:,jsym)*ft(3,isym) 
        !
        !     here we check that the input matrices really form a group:
        !        S(k)   = S(i)*S(j)
        !        ftau_k = S(j)*ftau_i+ftau_j (modulo a lattice vector)
        !
        found = .false.
        DO ksym = 1, nsym
           dt(:) = ft(:,ksym) - st(:) - NINT( ft(:,ksym) - st(:) ) 
           IF ( ALL( s(:,:,ksym) == ss(:,:) ) .AND. &
                ( ABS ( dt(1) ) < eps2 ) .AND. &
                ( ABS ( dt(2) ) < eps2 ) .AND. &
                ( ABS ( dt(3) ) < eps2 ) ) THEN
              IF (found) THEN
                 is_group = .false.
                 RETURN
              END IF
              found = .true.
           END IF
        END DO
        IF ( .NOT.found) then
           is_group = .false.
           RETURN
        END IF
     END DO
  END DO
  is_group=.true.
  RETURN
  !
END FUNCTION is_group
!
!-----------------------------------------------------------------------
logical function checksym ( irot, nat, ityp, xau, rau, ft_ )
  !-----------------------------------------------------------------------
  !
  !   This function receives as input all the atomic positions xau,
  !   and the rotated rau by the symmetry operation ir. It returns
  !   true if for each atom na, it is possible to find an atom nb
  !   which is of the same type of na, and coincide with it after the
  !   symmetry operation. Fractional translations are allowed.
  !
  implicit none
  !
  integer, intent(in) :: nat, ityp (nat), irot
  ! nat : number of atoms
  ! ityp: the type of each atom
  real(DP), intent(in) :: xau (3, nat), rau (3, nat), ft_(3)
  ! xau: the initial vectors (in crystal coordinates)
  ! rau: the rotated vectors (as above)
  ! ft_: fractionary translation (as above)
  !
  integer :: na, nb
  logical, external :: eqvect
  ! the testing function
  !
  do na = 1, nat
     do nb = 1, nat
        checksym = ( ityp (na) == ityp (nb) .and. &
                     eqvect (rau (1, na), xau (1, nb), ft_) )
        if ( checksym ) then
           !
           ! the rotated atom does coincide with one of the like atoms
           ! keep track of which atom the rotated atom coincides with
           !
           irt (irot, na) = nb
           goto 10
        endif
     enddo
     !
     ! the rotated atom does not coincide with any of the like atoms
     ! s(ir) + ft is not a symmetry operation
     !
     return 
10   continue
  enddo
  !
  ! s(ir) + ft is a symmetry operation
  !
  return 
end function checksym
!
!-----------------------------------------------------------------------
subroutine checkallsym ( nat, tau, ityp, nr1, nr2, nr3 ) 
  !-----------------------------------------------------------------------
  !     given a crystal group this routine checks that the actual
  !     atomic positions and bravais lattice vectors are compatible with
  !     it. Used in relaxation/MD runs to check that atomic motion is
  !     consistent with assumed symmetry.
  !
  implicit none
  !
  integer, intent(in) :: nat, ityp (nat), nr1, nr2, nr3
  real(DP), intent(in) :: tau (3, nat)
  !
  integer :: na, kpol, isym, i, j, k, l
  logical :: loksym (48)
  real(DP) :: sx (3, 3), sy(3,3)
  real(DP) , allocatable :: xau(:,:), rau(:,:)
  !
  allocate (xau( 3 , nat))    
  allocate (rau( 3 , nat))    
  !
  !     check that s(i,j, isym) is an orthogonal operation
  !
  do isym = 1, nsym
     sx = DBLE( s(:,:,isym) )
     sy = matmul ( bg, sx )
     sx = matmul ( sy, transpose(at) )
     ! sx is s in cartesian axis
     sy = matmul ( transpose ( sx ), sx )
     ! sy = s*transpose(s) = I
     do i = 1, 3
        sy (i,i) = sy (i,i) - 1.0_dp
     end do
     if (any (abs (sy) > eps1 ) ) &
         call errore ('checkallsym', 'not orthogonal operation', isym)
  enddo
  !
  !     Compute the coordinates of each atom in the basis of the lattice
  !
  do na = 1, nat
     do kpol = 1, 3
        xau (kpol, na) = bg (1, kpol) * tau (1, na) + &
                         bg (2, kpol) * tau (2, na) + &
                         bg (3, kpol) * tau (3, na)
     enddo
  enddo
  !
  !     generate the coordinates of the rotated atoms
  !
  do isym = 1, nsym
     do na = 1, nat
        do kpol = 1, 3
           rau (kpol, na) = s (1, kpol, isym) * xau (1, na) + &
                            s (2, kpol, isym) * xau (2, na) + &
                            s (3, kpol, isym) * xau (3, na)
        enddo
     enddo
     !
     loksym(isym) =  checksym ( isym, nat, ityp, xau, rau, ft(1,isym) )
     !
  enddo
  !
  !   deallocate work space
  !
  deallocate(rau)
  deallocate(xau)
  !
  do isym = 1,nsym
     if (.not.loksym (isym) ) call errore ('checkallsym', &
          'the following symmetry operation is not satisfied  ', -isym)
  end do
  if (ANY (.not.loksym (1:nsym) ) ) then
      !call symmetrize_at (nsym, s, invs, ft, irt, nat, tau, at, bg, &
      !                    alat, omega)
      call errore ('checkallsym', &
           'some of the original symmetry operations not satisfied ',1)
  end if
  !
  return
end subroutine checkallsym

!----------------------------------------------------------------------
subroutine s_axis_to_cart ( )
  !----------------------------------------------------------------------
  !
  !     This routine transforms symmetry matrices expressed in the
  !     basis of the crystal axis into rotations in cartesian axis
  !
  USE kinds
  implicit none
  !
  integer :: isym
  real(dp):: sa(3,3), sb(3,3)
  !
  do isym = 1,nsym
     sa (:,:) = DBLE ( s(:,:,isym) )
     sb = MATMUL ( bg, sa )
     sr (:,:, isym) = MATMUL ( at, TRANSPOSE (sb) )
  enddo
  !
 end subroutine s_axis_to_cart

END MODULE symm_base
