!
! Copyright (C) 2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=----------------------------------------------------------------------------=!
   MODULE gvect
!=----------------------------------------------------------------------------=!

     ! ... variables describing the reciprocal lattice vectors
     ! ... G vectors with |G|^2 < ecutrho, cut-off for charge density
     ! ... With gamma tricks, G-vectors are divided into two half-spheres,
     ! ... G> and G<, containing G and -G (G=0 is in G>)
     ! ... This is referred to as the "dense" (or "hard", or "thick") grid

     USE kinds, ONLY: DP

     IMPLICIT NONE
     SAVE

     INTEGER :: ngm  = 0  ! local  number of G vectors (on this processor)
                          ! with gamma tricks, only vectors in G>
     INTEGER :: ngm_g= 0  ! global number of G vectors (summed on all procs)
                          ! in serial execution, ngm_g = ngm
     INTEGER :: ngl = 0   ! number of G-vector shells
     INTEGER :: ngmx = 0  ! local number of G vectors, maximum across all procs

     REAL(DP) :: ecutrho = 0.0_DP ! energy cut-off for charge density 
     REAL(DP) :: gcutm = 0.0_DP   ! ecutrho/(2 pi/a)^2, cut-off for |G|^2

     ! nl  = fft index for G-vectors (with gamma tricks, only for G>)
     ! nlm = as above, for G< (used only with gamma tricks)

     INTEGER, ALLOCATABLE :: nl(:), nlm(:)

     INTEGER :: gstart = 2 ! index of the first G vector whose module is > 0
                           ! Needed in parallel execution: gstart=2 for the
                           ! proc that holds G=0, gstart=1 for all others

     !     G^2 in increasing order (in units of tpiba2=(2pi/a)^2)
     !
     REAL(DP), ALLOCATABLE, TARGET :: gg(:) 

     !     gl(i) = i-th shell of G^2 (in units of tpiba2)
     !     igtongl(n) = shell index for n-th G-vector
     !
     REAL(DP), POINTER :: gl(:) 
     INTEGER, ALLOCATABLE, TARGET :: igtongl(:) 
     !
     !     G-vectors cartesian components ( in units tpiba =(2pi/a)  )
     !
     REAL(DP), ALLOCATABLE, TARGET :: g(:,:) 

     !     mill = miller index of G vectors (local to each processor)
     !            G(:) = mill(1)*bg(:,1)+mill(2)*bg(:,2)+mill(3)*bg(:,3) 
     !            where bg are the reciprocal lattice basis vectors 
     !
     INTEGER, ALLOCATABLE, TARGET :: mill(:,:)
     
     !     ig_l2g  = converts a local G-vector index into the global index
     !               ("l2g" means local to global): ig_l2g(i) = index of i-th
     !               local G-vector in the global array of G-vectors
     !
     INTEGER, ALLOCATABLE, TARGET :: ig_l2g(:)
     !
     !     sortedig_l2g = array obtained by sorting ig_l2g
     !
     INTEGER, ALLOCATABLE, TARGET :: sortedig_l2g(:)
     !
     !     mill_g  = miller index of all G vectors
     !
     INTEGER, ALLOCATABLE, TARGET :: mill_g(:,:)
     !
     ! the phases e^{-iG*tau_s} used to calculate structure factors
     !
     COMPLEX(DP), ALLOCATABLE :: eigts1(:,:), eigts2(:,:), eigts3(:,:)
     !
   CONTAINS

     SUBROUTINE gvect_init( ngm_ , comm )
       !
       ! Set local and global dimensions, allocate arrays
       !
       USE mp, ONLY: mp_max, mp_sum
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: ngm_
       INTEGER, INTENT(IN) :: comm  ! communicator of the group on which g-vecs are distributed
       !
       ngm = ngm_
       !
       !  calculate maximum over all processors
       !
       ngmx = ngm
       CALL mp_max( ngmx, comm )
       !
       !  calculate sum over all processors
       !
       ngm_g = ngm
       CALL mp_sum( ngm_g, comm )
       !
       !  allocate arrays - only those that are always kept until the end
       !
       ALLOCATE( gg(ngm) )
       ALLOCATE( g(3, ngm) )
       ALLOCATE( mill(3, ngm) )
       ALLOCATE( nl (ngm) )
       ALLOCATE( nlm(ngm) )
       ALLOCATE( ig_l2g(ngm) )
       ALLOCATE( igtongl(ngm) )
       !
       RETURN 
       !
     END SUBROUTINE gvect_init

     SUBROUTINE deallocate_gvect()
       IF( ALLOCATED( gg ) ) DEALLOCATE( gg )
       IF( ASSOCIATED( gl ) ) DEALLOCATE( gl )
       IF( ALLOCATED( g ) )  DEALLOCATE( g )
       IF( ALLOCATED( mill_g ) ) DEALLOCATE( mill_g )
       IF( ALLOCATED( mill ) ) DEALLOCATE( mill )
       IF( ALLOCATED( igtongl ) ) DEALLOCATE( igtongl )
       IF( ALLOCATED( ig_l2g ) ) DEALLOCATE( ig_l2g )
       IF( ALLOCATED( sortedig_l2g ) ) DEALLOCATE( sortedig_l2g )
       IF( ALLOCATED( eigts1 ) ) DEALLOCATE( eigts1 )
       IF( ALLOCATED( eigts2 ) ) DEALLOCATE( eigts2 )
       IF( ALLOCATED( eigts3 ) ) DEALLOCATE( eigts3 )
       IF( ALLOCATED( nl ) ) DEALLOCATE( nl )
       IF( ALLOCATED( nlm ) ) DEALLOCATE( nlm )
     END SUBROUTINE deallocate_gvect

!=----------------------------------------------------------------------------=!
   END MODULE gvect
!=----------------------------------------------------------------------------=!

!=----------------------------------------------------------------------------=!
   MODULE gvecs
!=----------------------------------------------------------------------------=!
     USE kinds, ONLY: DP

     IMPLICIT NONE
     SAVE

     ! ... G vectors with |G|^2 < 4*ecutwfc, cut-off for wavefunctions
     ! ... ("smooth" grid). Gamma tricks and units as for the "dense" grid
     !
     INTEGER :: ngms = 0  ! local  number of smooth vectors (on this processor)
     INTEGER :: ngms_g=0  ! global number of smooth vectors (summed on procs) 
                          ! in serial execution this is equal to ngms
     INTEGER :: ngsx = 0  ! local number of smooth vectors, max across procs

     ! nl  = fft index for smooth vectors (with gamma tricks, only for G>)
     ! nlm = as above, for G< (used only with gamma tricks)

     INTEGER, ALLOCATABLE :: nls(:), nlsm(:)

     REAL(DP) :: ecuts = 0.0_DP   ! energy cut-off = 4*ecutwfc
     REAL(DP) :: gcutms= 0.0_DP   ! ecuts/(2 pi/a)^2, cut-off for |G|^2

     REAL(DP) :: dual = 0.0_DP    ! ecutrho=dual*ecutwfc
     LOGICAL  :: doublegrid = .FALSE. ! true if smooth and dense grid differ
                                      ! doublegrid = (dual > 4)

   CONTAINS

     SUBROUTINE gvecs_init( ngs_ , comm )
       USE mp, ONLY: mp_max, mp_sum
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: ngs_
       INTEGER, INTENT(IN) :: comm  ! communicator of the group on which g-vecs are distributed
       !
       ngms = ngs_
       !
       !  calculate maximum over all processors
       !
       ngsx = ngms
       CALL mp_max( ngsx, comm )
       !
       !  calculate sum over all processors
       !
       ngms_g = ngms
       CALL mp_sum( ngms_g, comm )
       !
       !  allocate arrays 
       !
       ALLOCATE( nls (ngms) )
       ALLOCATE( nlsm(ngms) )
       !
       RETURN 
       !
     END SUBROUTINE gvecs_init

     SUBROUTINE deallocate_gvecs()
       IF( ALLOCATED( nls ) ) DEALLOCATE( nls )
       IF( ALLOCATED( nlsm ) ) DEALLOCATE( nlsm )
     END SUBROUTINE deallocate_gvecs

!=----------------------------------------------------------------------------=!
   END MODULE gvecs
!=----------------------------------------------------------------------------=!

