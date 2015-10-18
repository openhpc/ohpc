!
! Copyright (C) 2008-2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!--------------------------------------------------------------------------
!
MODULE symme
  
  USE kinds,      ONLY : DP
  USE cell_base,  ONLY : at, bg
  USE symm_base,  ONLY : s, sname, ft, nrot, nsym, t_rev, time_reversal, &
                         irt, invs, invsym
  !
  ! ... Routines used for symmetrization 
  ! 
  SAVE
  PRIVATE
  !
  ! General-purpose symmetrizaton routines
  !
  PUBLIC ::  symscalar, symvector, symtensor, symmatrix, symv, &
             symtensor3, symmatrix3, crys_to_cart, cart_to_crys
  !
  ! For symmetrization in reciprocal space (all variables are private)
  !
  PUBLIC :: sym_rho_init, sym_rho, sym_rho_deallocate
  !
  LOGICAL :: &
       no_rho_sym=.true.      ! do not perform symetrization of charge density
  INTEGER :: ngs              ! number of symmetry-related G-vector shells
  TYPE shell_type
     INTEGER, POINTER :: vect(:)
  END TYPE shell_type
  ! shell contains a list of symmetry-related G-vectors for each shell
  TYPE(shell_type), ALLOCATABLE :: shell(:)
  ! Arrays used for parallel symmetrization
  INTEGER, ALLOCATABLE :: sendcnt(:), recvcnt(:), sdispls(:), rdispls(:)
  !
CONTAINS
   !
   LOGICAL FUNCTION rho_sym_needed ( )
      !-----------------------------------------------------------------------
      rho_sym_needed = .NOT. no_rho_sym
   END FUNCTION rho_sym_needed
   !
   SUBROUTINE symscalar (nat, scalar)
     !-----------------------------------------------------------------------
     ! Symmetrize a function f(na), na=atom index
     !
     IMPLICIT NONE
     !
     INTEGER, INTENT(IN) :: nat
     REAL(DP), intent(INOUT) :: scalar(nat)
     !
     INTEGER :: isym
     REAL(DP), ALLOCATABLE :: work (:)

     IF (nsym == 1) RETURN

     ALLOCATE (work(nat))
     work(:) = 0.0_dp
     DO isym = 1, nsym
        work (:) = work (:) +  scalar(irt(isym,:))
     END DO
     scalar(:) = work(:) / DBLE(nsym)
     DEALLOCATE (work)
   
   END SUBROUTINE symscalar
   !
   SUBROUTINE symvector (nat, vect)
     !-----------------------------------------------------------------------
     ! Symmetrize a function f(i,na), i=cartesian component, na=atom index
     ! e.g. : forces (in cartesian axis) 
     !
     IMPLICIT NONE
     !
     INTEGER, INTENT(IN) :: nat
     REAL(DP), intent(INOUT) :: vect(3,nat)
     !
     INTEGER :: na, isym, nar
     REAL(DP), ALLOCATABLE :: work (:,:)
     !
     IF (nsym == 1) RETURN
     !
     ALLOCATE (work(3,nat))
     !
     ! bring vector to crystal axis
     !
     DO na = 1, nat
        work(:,na) = vect(1,na)*at(1,:) + &
                     vect(2,na)*at(2,:) + &
                     vect(3,na)*at(3,:)
     END DO
     !
     ! symmetrize in crystal axis
     !
     vect (:,:) = 0.0_dp
     DO na = 1, nat
        DO isym = 1, nsym
           nar = irt (isym, na)
           vect (:, na) = vect (:, na) + &
                          s (:, 1, isym) * work (1, nar) + &
                          s (:, 2, isym) * work (2, nar) + &
                          s (:, 3, isym) * work (3, nar)
        END DO
     END DO
     work (:,:) = vect (:,:) / DBLE(nsym)
     !
     ! bring vector back to cartesian axis
     !
     DO na = 1, nat
        vect(:,na) = work(1,na)*bg(:,1) + &
                     work(2,na)*bg(:,2) + &
                     work(3,na)*bg(:,3)
     END DO
     !
     DEALLOCATE (work)
     !
   END SUBROUTINE symvector
   !
   SUBROUTINE symtensor (nat, tens)
     !-----------------------------------------------------------------------
     ! Symmetrize a function f(i,j,na), i,j=cartesian components, na=atom index
     ! e.g. : effective charges (in cartesian axis) 
     !
     IMPLICIT NONE
     !
     INTEGER, INTENT(IN) :: nat
     REAL(DP), intent(INOUT) :: tens(3,3,nat)
     !
     INTEGER :: na, isym, nar, i,j,k,l
     REAL(DP), ALLOCATABLE :: work (:,:,:)
     !
     IF (nsym == 1) RETURN
     !
     ! bring tensor to crystal axis
     !
     DO na=1,nat
        CALL cart_to_crys ( tens (:,:,na) )
     END DO
     !
     ! symmetrize in crystal axis
     !
     ALLOCATE (work(3,3,nat))
     work (:,:,:) = 0.0_dp
     DO na = 1, nat
        DO isym = 1, nsym
           nar = irt (isym, na)
           DO i = 1, 3
              DO j = 1, 3
                 DO k = 1, 3
                    DO l = 1, 3
                       work (i,j,na) = work (i,j,na) + &
                          s (i,k,isym) * s (j,l,isym) * tens (k,l,nar)
                    END DO
                 END DO
              END DO
           END DO
        END DO
     END DO
     tens (:,:,:) = work (:,:,:) / DBLE(nsym)
     DEALLOCATE (work)
     !
     ! bring tensor back to cartesian axis
     !
     DO na=1,nat
        CALL crys_to_cart ( tens (:,:,na) )
     END DO
     !
     !
   END SUBROUTINE symtensor
   !
   !-----------------------------------------------------------------------
   SUBROUTINE symv ( vect)
   !--------------------------------------------------------------------
     !
     ! Symmetrize a vector f(i), i=cartesian components
     ! The vector is supposed to be axial: inversion does not change it. 
     ! Time reversal changes its sign. Note that only groups compatible with 
     ! a finite magnetization give a nonzero output vector. 
     !
     IMPLICIT NONE
     !
     REAL (DP), INTENT(inout) :: vect(3)  ! the vector to rotate
     !
     integer :: isym 
     real(DP) :: work (3), segno
     !
     IF (nsym == 1) RETURN
     !
     ! bring vector to crystal axis
     !
     work(:) = vect(1)*at(1,:) + vect(2)*at(2,:) + vect(3)*at(3,:)
     vect = work
     work=0.0_DP
     do isym = 1, nsym
        segno=1.0_DP
        IF (sname(isym)(1:3)=='inv') segno=-1.0_DP
        IF (t_rev(isym)==1) segno=-1.0_DP*segno
        work (:) = work (:) + segno * ( &
                       s (:, 1, isym) * vect (1) + &
                       s (:, 2, isym) * vect (2) + &
                       s (:, 3, isym) * vect (3) )
     enddo
     work=work/nsym
   !
   !  And back in cartesian coordinates.
   !
   vect(:) = work(1) * bg(:,1) + work(2) * bg(:,2) + work(3) * bg(:,3)
   !
   end subroutine symv
   !
   SUBROUTINE symmatrix ( matr )
     !-----------------------------------------------------------------------
     ! Symmetrize a function f(i,j), i,j=cartesian components
     ! e.g. : stress, dielectric tensor (in cartesian axis) 
     !
     IMPLICIT NONE
     !
     REAL(DP), intent(INOUT) :: matr(3,3)
     !
     INTEGER :: isym, i,j,k,l
     REAL(DP) :: work (3,3)
     !
     IF (nsym == 1) RETURN
     !
     ! bring matrix to crystal axis
     !
     CALL cart_to_crys ( matr )
     !
     ! symmetrize in crystal axis
     !
     work (:,:) = 0.0_dp
     DO isym = 1, nsym
        DO i = 1, 3
           DO j = 1, 3
              DO k = 1, 3
                 DO l = 1, 3
                    work (i,j) = work (i,j) + &
                       s (i,k,isym) * s (j,l,isym) * matr (k,l)
                 END DO
              END DO
           END DO
        END DO
     END DO
     matr (:,:) = work (:,:) / DBLE(nsym)
     !
     ! bring matrix back to cartesian axis
     !
     CALL crys_to_cart ( matr )
     !
   END SUBROUTINE symmatrix
   !
   SUBROUTINE symmatrix3 ( mat3 )
     !-----------------------------------------------------------------------
     !
     ! Symmetrize a function f(i,j,k), i,j,k=cartesian components
     ! e.g. : nonlinear susceptibility
     ! BEWARE: input in crystal axis, output in cartesian axis
     !
     IMPLICIT NONE
     !
     REAL(DP), intent(INOUT) :: mat3(3,3,3)
     !
     INTEGER :: isym, i,j,k,l,m,n
     REAL(DP) :: work (3,3,3)
     !
     IF (nsym == 1) RETURN
     !
     work (:,:,:) = 0.0_dp
     DO isym = 1, nsym
        DO i = 1, 3
           DO j = 1, 3
              DO k = 1, 3
                 DO l = 1, 3
                    DO m = 1, 3
                       DO n = 1, 3
                          work (i, j, k) = work (i, j, k) + &
                               s (i, l, isym) * s (j, m, isym) * &
                               s (k, n, isym) * mat3 (l, m, n)
                       END DO
                    END DO
                 END DO
              END DO
           END DO
        END DO
     END DO
     mat3 = work/ DBLE(nsym)
     !
     ! Bring to cartesian axis
     !
     CALL crys_to_cart_mat3 ( mat3 ) 
     !
   END SUBROUTINE symmatrix3
   !
   !
   SUBROUTINE symtensor3 (nat, tens3 )
     !-----------------------------------------------------------------------
     ! Symmetrize a function f(i,j,k, na), i,j,k=cartesian, na=atom index
     ! e.g. : raman tensor
     ! BEWARE: input in crystal axis, output in cartesian axis
     !
     IMPLICIT NONE
     !
     INTEGER, INTENT(IN) :: nat
     REAL(DP), intent(INOUT) :: tens3(3,3,3,nat)
     !
     INTEGER :: na, isym, nar, i,j,k,l,n,m
     REAL(DP), ALLOCATABLE :: work (:,:,:,:)
     !
     IF (nsym == 1) RETURN
     !
     ! symmetrize in crystal axis
     !
     ALLOCATE (work(3,3,3,nat))
     work (:,:,:,:) = 0.0_dp
     DO na = 1, nat
        DO isym = 1, nsym
           nar = irt (isym, na)
           DO i = 1, 3
              DO j = 1, 3
                 DO k = 1, 3
                    DO l = 1, 3
                       DO m =1, 3
                          DO n =1, 3
                             work (i, j, k, na) = work (i, j, k, na) + &
                                  s (i, l, isym) * s (j, m, isym) *    &
                                  s (k, n, isym) * tens3 (l, m, n, nar)
                          END DO
                       END DO
                    END DO
                 END DO
              END DO
           END DO
        END DO
     END DO
     tens3 (:,:,:,:) =   work(:,:,:,:) / DBLE (nsym)
     DEALLOCATE (work)
     !
     ! Bring to cartesian axis
     !
     DO na = 1, nat
        CALL crys_to_cart_mat3 ( tens3(:,:,:,na) ) 
     END DO
     !
   END SUBROUTINE symtensor3
   !
   ! Routines for crystal to cartesian axis conversion
   !
   !INTERFACE cart_to_crys
   !  MODULE PROCEDURE cart_to_crys_mat, cart_to_crys_mat3
   !END INTERFACE
   !INTERFACE crys_to_cart
   !  MODULE PROCEDURE crys_to_cart
   !END INTERFACE
   !
   SUBROUTINE cart_to_crys ( matr )
     !-----------------------------------------------------------------------
     !     
     IMPLICIT NONE
     !
     REAL(DP), intent(INOUT) :: matr(3,3)
     !
     REAL(DP) :: work(3,3)
     INTEGER :: i,j,k,l
     !
     work(:,:) = 0.0_dp
     DO i = 1, 3
        DO j = 1, 3
           DO k = 1, 3
              DO l = 1, 3
                 work(i,j) = work(i,j) + matr(k,l) * at(k,i) * at(l,j)
              END DO
           END DO
        END DO
     END DO
     !
     matr(:,:) = work(:,:)
     !
   END SUBROUTINE cart_to_crys
   !
   SUBROUTINE crys_to_cart ( matr )
     !-----------------------------------------------------------------------
     !
     IMPLICIT NONE
     !
     REAL(DP), intent(INOUT) :: matr(3,3)
     !
     REAL(DP) :: work(3,3)
     INTEGER :: i,j,k,l
     !
     work(:,:) = 0.0_dp
     DO i = 1, 3
        DO j = 1, 3
           DO k = 1, 3
              DO l = 1, 3
                 work(i,j) = work(i,j) + &
                             matr(k,l) * bg(i,k) * bg(j,l)
              END DO
           END DO
        END DO
     END DO
     matr(:,:) = work(:,:)
     !
   END SUBROUTINE crys_to_cart
   !
   SUBROUTINE crys_to_cart_mat3 ( mat3 )
     !-----------------------------------------------------------------------
     !
     IMPLICIT NONE
     !
     REAL(DP), intent(INOUT) :: mat3(3,3,3)
     !
     REAL(DP) :: work(3,3,3)
     INTEGER :: i,j,k,l,m,n
     !
     work(:,:,:) = 0.0_dp
     DO i = 1, 3
        DO j = 1, 3
           DO k = 1, 3
              DO l = 1, 3
                 DO m = 1, 3
                    DO n = 1, 3
                       work (i, j, k) = work (i, j, k) +  &
                          mat3 (l, m, n) * bg (i, l) * bg (j, m) * bg (k, n)
                    END DO
                 END DO
              END DO
           END DO
        END DO
     END DO
     mat3(:,:,:) = work (:,:,:)
     !
   END SUBROUTINE crys_to_cart_mat3
   !
   ! G-space symmetrization
   !
   SUBROUTINE sym_rho_init ( gamma_only )
    !-----------------------------------------------------------------------
    !
    !  Initialize arrays needed for symmetrization in reciprocal space
    ! 
    USE gvect, ONLY : ngm, g
    !
    LOGICAL, INTENT(IN) :: gamma_only
    !
    no_rho_sym = gamma_only .OR. (nsym==1)
    IF (no_rho_sym) RETURN
    CALL sym_rho_init_para ( )
    !
  END SUBROUTINE sym_rho_init
   !
  !
  SUBROUTINE sym_rho_init_para ( )
    !-----------------------------------------------------------------------
    !
    !  Initialize arrays needed for parallel symmetrization
    ! 
    USE mp_global, ONLY : nproc_bgrp, me_bgrp, intra_bgrp_comm
    USE parallel_include
    USE gvect, ONLY : ngm, gcutm, g, gg
    !
    IMPLICIT NONE
    !
    REAL(DP), PARAMETER :: twothirds = 0.6666666666666666_dp
    REAL(DP), ALLOCATABLE :: gcut_(:), g_(:,:)
    INTEGER :: np, ig, ngloc, ngpos, ierr, ngm_
    !
    ALLOCATE ( sendcnt(nproc_bgrp), recvcnt(nproc_bgrp), &
               sdispls(nproc_bgrp), rdispls(nproc_bgrp) )
    ALLOCATE ( gcut_(nproc_bgrp) )
    !
    ! the gcut_ cutoffs are estimated in such a way that there is an similar
    ! number of G-vectors in each shell gcut_(i) < G^2 < gcut_(i+1)
    !
    DO np = 1, nproc_bgrp
       gcut_(np) = gcutm * np**twothirds/nproc_bgrp**twothirds
    END DO
    !
    ! find the number of G-vectors in each shell (defined as above)
    ! beware: will work only if G-vectors are in order of increasing |G|
    !
    ngpos=0
    DO np = 1, nproc_bgrp
       sdispls(np) = ngpos
       ngloc=0
       DO ig=ngpos+1,ngm
          IF ( gg(ig) > gcut_(np) ) EXIT
          ngloc = ngloc+1
       END DO
       ! BMA: I'm fairly convinced this is not an error if triggered by extreme strong-scaling
       !      Furthermore, this test assumes the sticks are not load balanced
       !IF ( ngloc < 1 ) CALL infomsg('sym_rho_init', &
       !     'likely internal error: no G-vectors found')
       sendcnt(np) = ngloc
       ngpos = ngpos + ngloc
       IF ( ngpos > ngm ) &
            CALL errore('sym_rho','internal error: too many G-vectors', ngpos)
    END DO
    IF ( ngpos /= ngm .OR. ngpos /= SUM (sendcnt)) &
         CALL errore('sym_rho_init', &
         'internal error: inconsistent number of G-vectors', ngpos)
    DEALLOCATE ( gcut_ )
  !
  ! sendcnt(i) = n_j(i) = number of G-vectors in shell i for processor j (this)
  ! sdispls(i) = \sum_{k=1}^i n_j(k) = starting position of shell i for proc j
  ! we need the number and positions of G-vector shells for other processors
  !
    CALL mpi_alltoall( sendcnt, 1, MPI_INTEGER, recvcnt, 1, MPI_INTEGER, &
         intra_bgrp_comm, ierr)
    !
    rdispls(1) = 0
    DO np = 2, nproc_bgrp
       rdispls(np) = rdispls(np-1)+ recvcnt(np-1)
    END DO
    !
    ! recvcnt(i) = n_i(j) = number of G-vectors in shell j for processor i
    ! rdispls(i) = \sum_{k=1}^i n_k(j) = start.pos. of shell j for proc i
    !
    ! now collect G-vector shells on each processor
    !
    ngm_ = SUM(recvcnt)
    ALLOCATE (g_(3,ngm_))
    ! remember that G-vectors have 3 components
    sendcnt(:) = 3*sendcnt(:)
    recvcnt(:) = 3*recvcnt(:)
    sdispls(:) = 3*sdispls(:)
    rdispls(:) = 3*rdispls(:)
    CALL mpi_alltoallv ( g , sendcnt, sdispls, MPI_DOUBLE_PRECISION, &
                         g_, recvcnt, rdispls, MPI_DOUBLE_PRECISION, &
                         intra_bgrp_comm, ierr)
    sendcnt(:) = sendcnt(:)/3
    recvcnt(:) = recvcnt(:)/3
    sdispls(:) = sdispls(:)/3
    rdispls(:) = rdispls(:)/3
    !
    ! find shells of symmetry-related G-vectors
    !
    CALL sym_rho_init_shells( ngm_, g_ )
    !
    DEALLOCATE (g_)
    !
  END SUBROUTINE sym_rho_init_para
  !
  !
  SUBROUTINE sym_rho_init_shells ( ngm_, g_ )
    !-----------------------------------------------------------------------
    !
    !  Initialize G-vector shells needed for symmetrization
    ! 
    USE constants, ONLY : eps8
    USE mp_global, ONLY : nproc_bgrp
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: ngm_
    REAL(DP), INTENT(IN) :: g_(3,ngm_)
    !
    LOGICAL, ALLOCATABLE :: done(:)
    INTEGER, ALLOCATABLE :: n(:,:), igsort(:)
    REAL(DP), ALLOCATABLE :: g2sort_g(:)
    INTEGER :: i,j,is,ig, iig, jg, ng, sn(3), gshell(3,48)
    LOGICAL :: found
    !
    ngs = 0
    ! shell should be allocated to the number of symmetry shells
    ! since this is unknown, we use the number of all G-vectors
    ALLOCATE ( shell(ngm_) )
    ALLOCATE ( done(ngm_), n(3,ngm_) )
    ALLOCATE ( igsort (ngm_))
    DO ig=1,ngm_
       !
       done(ig) = .false.
       ! G-vectors are stored as integer indices in crystallographic axis:
       !    G = n(1)*at(1) + n(2)*at(2) + n(3)*at(3)
       n(:,ig) = nint ( at(1,:)*g_(1,ig) + at(2,:)*g_(2,ig) + at(3,:)*g_(3,ig) )
       !
       NULLIFY(shell(ig)%vect)
       !
    END DO
!
!   The following algorithm can become very slow if ngm_ is large and
!   g vectors are not ordered in increasing order. This happens 
!   in the parallel case.
!
    IF (nproc_bgrp > 1 .AND. ngm_ > 20000) THEN
       ALLOCATE ( g2sort_g(ngm_))
       g2sort_g(:)=g_(1,:)*g_(1,:)+g_(2,:)*g_(2,:)+g_(3,:)*g_(3,:)
       igsort(1) = 0
       CALL hpsort_eps( ngm_, g2sort_g, igsort, eps8 )
       DEALLOCATE( g2sort_g)
    ELSE
       DO ig=1,ngm_
          igsort(ig)=ig
       ENDDO
    ENDIF
    !
    DO iig=1,ngm_
       !
       ig=igsort(iig)
       IF ( done(ig) ) CYCLE
       !
       ! we start a new shell of symmetry-equivalent G-vectors
       ngs = ngs+1
       ! ng: counter on G-vectors in this shell
       ng  = 0
       DO is=1,nsym
          ! integer indices for rotated G-vector
          sn(:)=s(:,1,is)*n(1,ig)+s(:,2,is)*n(2,ig)+s(:,3,is)*n(3,ig)
          found = .false.
          ! check if this rotated G-vector is equivalent to any other
          ! vector already present in this shell
shelloop: DO i=1,ng
             found = ( sn(1)==gshell(1,i) .and. &
                       sn(2)==gshell(2,i) .and. &
                       sn(3)==gshell(3,i) )
             if (found) exit shelloop
          END DO shelloop
          IF ( .not. found ) THEN
             ! add rotated G-vector to this shell
             ng = ng + 1
             IF (ng > 48) CALL errore('sym_rho_init_shell','internal error',48)
             gshell(:,ng) = sn(:)
          END IF
       END DO
       ! there are ng vectors gshell in shell ngs
       ! now we have to locate them in the list of G-vectors
       ALLOCATE ( shell(ngs)%vect(ng))
       DO i=1,ng
gloop:    DO jg=iig,ngm_
             j=igsort(jg)
             IF (done(j)) CYCLE gloop
                found = ( gshell(1,i)==n(1,j) .and. &
                          gshell(2,i)==n(2,j) .and. &
                          gshell(3,i)==n(3,j) )
             IF ( found ) THEN
                done(j)=.true.
                shell(ngs)%vect(i) = j
                EXIT gloop
             END IF
          END DO gloop
          IF (.not. found) CALL errore('sym_rho_init_shell','lone vector',i)
       END DO
       !
    END DO
    DEALLOCATE ( n, done ) 
    DEALLOCATE( igsort)

  END SUBROUTINE sym_rho_init_shells
  !
  !-----------------------------------------------------------------------
  SUBROUTINE sym_rho (nspin, rhog)
    !-----------------------------------------------------------------------
    !
    !     Symmetrize the charge density rho in reciprocal space
    !     Distributed parallel algorithm: collects entire shells of G-vectors
    !     and corresponding rho(G), calls sym_rho_serial to perform the
    !     symmetrization, re-distributed rho(G) into original ordering
    !     rhog(ngm,nspin) components of rho: rhog(ig) = rho(G(:,ig))
    !                     unsymmetrized on input, symmetrized on output
    !     nspin=1,2,4     unpolarized, LSDA, non-colinear magnetism     
    !
    USE constants,            ONLY : eps8, eps6
    USE gvect,                ONLY : ngm, g
    USE parallel_include
    USE mp_global,            ONLY : intra_bgrp_comm
    !
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: nspin
    COMPLEX(DP), INTENT(INOUT) :: rhog(ngm,nspin)
    !
    REAL(DP), allocatable :: g0(:,:), g_(:,:), gg_(:) 
    REAL(DP) :: gg0_, gg1_
    COMPLEX(DP), allocatable :: rhog_(:,:)
    INTEGER :: is, ig, igl, np, ierr, ngm_
    !
    IF ( no_rho_sym) RETURN
    !
    ! we transpose the matrix of G-vectors and their coefficients
    !
    ngm_ = SUM(recvcnt)
    ALLOCATE (rhog_(ngm_,nspin),g_(3,ngm_))
    DO is=1,nspin
       CALL mpi_alltoallv (rhog (1,is) , sendcnt, sdispls, MPI_DOUBLE_COMPLEX,&
            rhog_(1,is), recvcnt, rdispls, MPI_DOUBLE_COMPLEX, &
            intra_bgrp_comm, ierr)
    END DO
    ! remember that G-vectors have 3 components
    sendcnt(:) = 3*sendcnt(:)
    recvcnt(:) = 3*recvcnt(:)
    sdispls(:) = 3*sdispls(:)
    rdispls(:) = 3*rdispls(:)
    CALL mpi_alltoallv ( g , sendcnt, sdispls, MPI_DOUBLE_PRECISION, &
         g_, recvcnt, rdispls, MPI_DOUBLE_PRECISION, &
         intra_bgrp_comm, ierr)
    !
    !   Now symmetrize
    !
    CALL sym_rho_serial ( ngm_, g_, nspin, rhog_ )
    !
    DEALLOCATE ( g_ )
    !
    ! bring symmetrized rho(G) back to original distributed form
    !
    sendcnt(:) = sendcnt(:)/3
    recvcnt(:) = recvcnt(:)/3
    sdispls(:) = sdispls(:)/3
    rdispls(:) = rdispls(:)/3
    DO is = 1, nspin
       CALL mpi_alltoallv (rhog_(1,is), recvcnt, rdispls, MPI_DOUBLE_COMPLEX, &
            rhog (1,is), sendcnt, sdispls, MPI_DOUBLE_COMPLEX, &
            intra_bgrp_comm, ierr)
    END DO
    DEALLOCATE ( rhog_ )
    !
    RETURN
  END SUBROUTINE sym_rho
  !
  !-----------------------------------------------------------------------
  SUBROUTINE sym_rho_serial ( ngm_, g_, nspin_, rhog_ )
    !-----------------------------------------------------------------------
    !
    !     symmetrize the charge density rho in reciprocal space 
    !     Serial algorithm - requires in input: 
    !     g_(3,ngm_)      list of G-vectors
    !     nspin_          number of spin components to be symmetrized
    !     rhog_(ngm_,nspin_) rho in reciprocal space: rhog_(ig) = rho(G(:,ig))
    !                      unsymmetrized on input, symmetrized on output
    !
    USE kinds
    USE constants,            ONLY : tpi
    !
    IMPLICIT NONE
    !
    INTEGER, INTENT (IN) :: ngm_, nspin_
    REAL(DP) , INTENT (IN) :: g_( 3, ngm_ )
    COMPLEX(DP) , INTENT (INOUT) :: rhog_( ngm_, nspin_ )
    !
    REAL(DP), ALLOCATABLE :: g0(:,:)
    REAL(DP) :: sg(3), ft_(3,48), arg
    COMPLEX(DP) :: fact, rhosum(2), mag(3), magrot(3), magsum(3)
    INTEGER :: irot(48), ig, isg, igl, ng, ns, nspin_lsda, is
    LOGICAL, ALLOCATABLE :: done(:)
    LOGICAL :: non_symmorphic(48)
    !
    ! convert fractional translations to cartesian, in a0 units
    !
    DO ns=1,nsym
       non_symmorphic(ns) = ( ft(1,ns) /= 0.0_dp .OR. &
                              ft(2,ns) /= 0.0_dp .OR. &
                              ft(3,ns) /= 0.0_dp )
       IF ( non_symmorphic(ns) ) ft_(:,ns) = at(:,1)*ft(1,ns) + &
                                             at(:,2)*ft(2,ns) + &
                                             at(:,3)*ft(3,ns)
    END DO
    !
    IF ( nspin_ == 4 ) THEN
       nspin_lsda = 1
       !
    ELSE IF ( nspin_ == 1 .OR. nspin_ == 2 ) THEN
       nspin_lsda = nspin_
    ELSE
       CALL errore('sym_rho_serial','incorrect value of nspin',nspin_)
    END IF
    !
    ! scan shells of G-vectors
    !
    DO igl=1, ngs
       !
       ! symmetrize: \rho_sym(G) = \sum_S rho(SG) for all G-vectors in the star
       !
       ng = SIZE ( shell(igl)%vect )
       allocate ( g0(3,ng), done(ng) )
       IF ( ng < 1 ) CALL errore('sym_rho_serial','internal error',1)
       !
       !  bring G-vectors to crystal axis
       !
       DO ig=1,ng
          g0(:,ig) = g_(:,shell(igl)%vect(ig) )
       END DO
       CALL cryst_to_cart (ng, g0, at,-1)
       !
       !  rotate G-vectors
       !
       done(1:ng) = .false.
       DO ig=1,ng
          IF ( .NOT. done(ig)) THEN
             rhosum(:) = (0.0_dp, 0.0_dp)
             magsum(:) = (0.0_dp, 0.0_dp)
             ! S^{-1} are needed here
             DO ns=1,nsym

                sg(:) = s(:,1,invs(ns)) * g0(1,ig) + &
                        s(:,2,invs(ns)) * g0(2,ig) + &
                        s(:,3,invs(ns)) * g0(3,ig)
                irot(ns) = 0
                DO isg=1,ng
                   IF ( ABS ( sg(1)-g0(1,isg) ) < 1.0D-5 .AND. &
                        ABS ( sg(2)-g0(2,isg) ) < 1.0D-5 .AND. &
                        ABS ( sg(3)-g0(3,isg) ) < 1.0D-5 ) THEN
                      irot(ns) = isg
                      EXIT
                   END IF
                END DO
                IF ( irot(ns) < 1 .OR. irot(ns) > ng ) &
                     CALL errore('sym_rho_serial','internal error',2)
                ! isg is the index of rotated G-vector
                isg = shell(igl)%vect(irot(ns))
                !
                ! non-spin-polarized case: component 1 is the charge
                ! LSDA case: components 1,2 are spin-up and spin-down charge
                ! non colinear case: component  1 is the charge density,
                !                    components 2,3,4 are the magnetization
                ! non colinear case: components 2,3,4 are the magnetization
                !
                IF ( nspin_ == 4 ) THEN
                   ! bring magnetization to crystal axis
                   mag(:) = rhog_(isg, 2) * bg(1,:) + &
                            rhog_(isg, 3) * bg(2,:) + &
                            rhog_(isg, 4) * bg(3,:)
                   ! rotate and add magnetization
                   magrot(:) = s(1,:,invs(ns)) * mag(1) + &
                               s(2,:,invs(ns)) * mag(2) + &
                               s(3,:,invs(ns)) * mag(3)
                   IF (sname(invs(ns))(1:3)=='inv') magrot(:)=-magrot(:)
                   IF (t_rev(invs(ns)) == 1)        magrot(:)=-magrot(:)
                END IF
                IF ( non_symmorphic (ns) )  THEN
                   arg = tpi * ( g_(1,isg) * ft_(1,ns) + &
                                 g_(2,isg) * ft_(2,ns) + &
                                 g_(3,isg) * ft_(3,ns) )
                   fact = CMPLX ( COS(arg), -SIN(arg), KIND=dp )
                   DO is=1,nspin_lsda
                      rhosum(is) = rhosum(is) + rhog_(isg, is) * fact
                   END DO
                   IF ( nspin_ == 4 ) &
                        magsum(:) = magsum(:) + magrot(:) * fact
                ELSE
                   DO is=1,nspin_lsda
                      rhosum(is) = rhosum(is) + rhog_(isg, is)
                   END DO
                   IF ( nspin_ == 4 ) &
                        magsum(:) = magsum(:) + magrot(:)
                END IF
             END DO
             !
             DO is=1,nspin_lsda
                rhosum(is) = rhosum(is) / nsym
             END DO
             IF ( nspin_ == 4 ) magsum(:) = magsum(:) / nsym
             !
             !  now fill the shell of G-vectors with the symmetrized value
             !
             DO ns=1,nsym
                isg = shell(igl)%vect(irot(ns))
                IF ( nspin_ == 4 ) THEN
                   ! rotate magnetization
                   magrot(:) = s(1,:,ns) * magsum(1) + &
                               s(2,:,ns) * magsum(2) + &
                               s(3,:,ns) * magsum(3)
                   IF (sname(ns)(1:3)=='inv') magrot(:)=-magrot(:)
                   IF (t_rev(ns) == 1)        magrot(:)=-magrot(:)
                   ! back to cartesian coordinates
                   mag(:) = magrot(1) * at(:,1) + &
                            magrot(2) * at(:,2) + &
                            magrot(3) * at(:,3)
                END IF
                IF ( non_symmorphic (ns) )  THEN
                   arg = tpi * ( g_(1,isg) * ft_(1,ns) + &
                                 g_(2,isg) * ft_(2,ns) + &
                                 g_(3,isg) * ft_(3,ns) )
                   fact = CMPLX ( COS(arg), SIN(arg), KIND=dp )
                   DO is=1,nspin_lsda
                      rhog_(isg,is) = rhosum(is) * fact
                   END DO
                   IF ( nspin_ == 4 ) THEN
                      DO is=2,nspin_
                         rhog_(isg, is) = mag(is-1)*fact
                      END DO
                   END IF
                ELSE
                   DO is=1,nspin_lsda
                      rhog_(isg,is) = rhosum(is)
                   END DO
                   IF ( nspin_ == 4 ) THEN
                      DO is=2,nspin_
                         rhog_(isg, is) = mag(is-1)
                      END DO
                   END IF
                END IF
                done(irot(ns)) =.true.
             END DO
          END IF
       END DO
       DEALLOCATE ( done, g0 )
    END DO
    !
    RETURN
  END SUBROUTINE sym_rho_serial

  SUBROUTINE sym_rho_deallocate ( )
    !
    IF ( ALLOCATED (rdispls) ) DEALLOCATE (rdispls) 
    IF ( ALLOCATED (recvcnt) ) DEALLOCATE (recvcnt) 
    IF ( ALLOCATED (sdispls) ) DEALLOCATE (sdispls) 
    IF ( ALLOCATED (sendcnt) ) DEALLOCATE (sendcnt) 
    IF ( ALLOCATED (shell) ) THEN
       DO i=1,SIZE(shell)
          IF ( ASSOCIATED(shell(i)%vect) ) DEALLOCATE (shell(i)%vect)
       END DO
       DEALLOCATE (shell)
    END IF
    !
  END SUBROUTINE sym_rho_deallocate
  !
END MODULE symme
