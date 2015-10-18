!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
!
!
!----------------------------------------------------------------------------
SUBROUTINE protate_wfc_k( npwx, npw, nstart, nbnd, npol, psi, overlap, evc, e )
  !----------------------------------------------------------------------------
  !
  ! ... Parallel version of rotate_wfc for colinear, k-point calculations
  ! ... Subroutine with distributed matrices, written by Carlo Cavazzoni
  !
  USE kinds,            ONLY : DP
  USE mp_global,        ONLY : intra_bgrp_comm, &
                               nbgrp, nproc_bgrp, me_bgrp, root_bgrp, &
                               ortho_comm, np_ortho, me_ortho, ortho_comm_id,&
                               leg_ortho
  USE descriptors,      ONLY : descla_init , la_descriptor
  USE parallel_toolkit, ONLY : zsqmred, zsqmher
  USE mp,               ONLY : mp_bcast, mp_root_sum, mp_sum, mp_barrier
  !
  IMPLICIT NONE
  !
  ! ... I/O variables
  !
  INTEGER :: npw, npwx, nstart, nbnd, npol
    ! dimension of the matrix to be diagonalized
    ! leading dimension of matrix psi, as declared in the calling pgm unit
    ! input number of states
    ! output number of states
    ! number of spin polarizations
  LOGICAL :: overlap
    ! if .FALSE. : S|psi> not needed
  COMPLEX(DP) :: psi(npwx*npol,nstart), evc(npwx*npol,nbnd)
    ! input and output eigenvectors (may overlap)
  REAL(DP) :: e(nbnd)
    ! eigenvalues
  !
  ! ... local variables
  !
  INTEGER :: kdim, kdmx
  COMPLEX(DP), ALLOCATABLE :: aux(:,:), hc(:,:), sc(:,:), vc(:,:)
  REAL(DP),    ALLOCATABLE :: en(:)
  !
  TYPE(la_descriptor) :: desc
    ! matrix distribution descriptors
  INTEGER :: nx
    ! maximum local block dimension
  LOGICAL :: la_proc
    ! flag to distinguish procs involved in linear algebra
  TYPE(la_descriptor), ALLOCATABLE :: desc_ip( :, : )
  INTEGER, ALLOCATABLE :: rank_ip( :, : )
  !
  ALLOCATE( desc_ip( np_ortho(1), np_ortho(2) ) )
  ALLOCATE( rank_ip( np_ortho(1), np_ortho(2) ) )
  !
  CALL desc_init( nstart, desc, desc_ip )
  !
  IF ( npol == 1 ) THEN
     !
     kdim = npw
     kdmx = npwx
     !
  ELSE
     !
     kdim = npwx*npol
     kdmx = npwx*npol
     !
  END IF
  !
  ALLOCATE( aux(kdmx, nstart ) )    
  ALLOCATE( hc( nx, nx) )    
  ALLOCATE( sc( nx, nx) )    
  ALLOCATE( vc( nx, nx) )    
  ALLOCATE( en( nstart ) )

  aux=(0.0_DP,0.0_DP)
  !
  ! ... Set up the Hamiltonian and Overlap matrix on the subspace :
  !
  ! ...      H_ij = <psi_i| H |psi_j>     S_ij = <psi_i| S |psi_j>
  !
  CALL h_psi( npwx, npw, nstart, psi, aux )
  !
  CALL compute_distmat( hc, psi, aux ) 
  !            
  IF ( overlap ) THEN
     !
     CALL s_psi( npwx, npw, nstart, psi, aux )
     !
     CALL compute_distmat( sc, psi, aux )
     !
  ELSE
     !
     CALL compute_distmat( sc, psi, psi )
     !  
  END IF
  !
  ! ... Diagonalize
  !
  CALL pcdiaghg( nstart, hc, sc, nx, en, vc, desc )
  !
  e(:) = en(1:nbnd)
  !
  ! ...  update the basis set
  !  
  CALL refresh_evc()
  !     
  evc(:,:) = aux(:,1:nbnd)
  !
  DEALLOCATE( en )
  DEALLOCATE( vc )
  DEALLOCATE( sc )
  DEALLOCATE( hc )
  DEALLOCATE( aux )
  !
  DEALLOCATE( desc_ip )
  DEALLOCATE( rank_ip )
  !
  RETURN
  !
  !
CONTAINS
  !
  SUBROUTINE desc_init( nsiz, desc, desc_ip )
     !
     INTEGER, INTENT(IN)  :: nsiz
     TYPE(la_descriptor), INTENT(OUT) :: desc
     TYPE(la_descriptor), INTENT(OUT) :: desc_ip(:,:)
     INTEGER :: i, j, rank
     INTEGER :: coor_ip( 2 )
     !
     CALL descla_init( desc, nsiz, nsiz, np_ortho, me_ortho, ortho_comm, ortho_comm_id )
     !
     nx = desc%nrcx
     !
     DO j = 0, desc%npc - 1
        DO i = 0, desc%npr - 1
           coor_ip( 1 ) = i
           coor_ip( 2 ) = j
           CALL descla_init( desc_ip(i+1,j+1), desc%n, desc%nx, &
                             np_ortho, coor_ip, ortho_comm, 1 )
           CALL GRID2D_RANK( 'R', desc%npr, desc%npc, i, j, rank )
           rank_ip( i+1, j+1 ) = rank * leg_ortho
        END DO
     END DO
     !
     la_proc = .FALSE.
     IF( desc%active_node > 0 ) la_proc = .TRUE.
     !
     RETURN
  END SUBROUTINE desc_init
  !
  !
  SUBROUTINE compute_distmat( dm, v, w )
     !
     !  This subroutine compute <vi|wj> and store the
     !  result in distributed matrix dm 
     !
     INTEGER :: ipc, ipr
     INTEGER :: nr, nc, ir, ic, root
     COMPLEX(DP), INTENT(OUT) :: dm( :, : )
     COMPLEX(DP) :: v(:,:), w(:,:)
     COMPLEX(DP), ALLOCATABLE :: work( :, : )
     !
     ALLOCATE( work( nx, nx ) )
     !
     work = ( 0.0_DP, 0.0_DP )
     !
     DO ipc = 1, desc%npc !  loop on column procs 
        !
        nc = desc_ip( 1, ipc )%nc
        ic = desc_ip( 1, ipc )%ic
        !
        DO ipr = 1, ipc ! desc%npr ! ipc ! use symmetry for the loop on row procs
           !
           nr = desc_ip( ipr, ipc )%nr
           ir = desc_ip( ipr, ipc )%ir
           !
           !  rank of the processor for which this block (ipr,ipc) is destinated
           !
           root = rank_ip( ipr, ipc )

           ! use blas subs. on the matrix block

           CALL ZGEMM( 'C', 'N', nr, nc, kdim, ( 1.D0, 0.D0 ) , &
                       v(1,ir), kdmx, w(1,ic), kdmx, ( 0.D0, 0.D0 ), work, nx )

           ! accumulate result on dm of root proc.
           CALL mp_root_sum( work, dm, root, intra_bgrp_comm )

        END DO
        !
     END DO
     !
     CALL zsqmher( nstart, dm, nx, desc )
     !
     DEALLOCATE( work )
     !
     RETURN
  END SUBROUTINE compute_distmat


  SUBROUTINE refresh_evc( )
     !
     INTEGER :: ipc, ipr
     INTEGER :: nr, nc, ir, ic, root
     COMPLEX(DP), ALLOCATABLE :: vtmp( :, : )
     COMPLEX(DP) :: beta

     ALLOCATE( vtmp( nx, nx ) )
     !
     DO ipc = 1, desc%npc
        !
        nc = desc_ip( 1, ipc )%nc
        ic = desc_ip( 1, ipc )%ic
        !
        IF( ic <= nbnd ) THEN
           !
           nc = min( nc, nbnd - ic + 1 )
           !
           beta = ( 0.D0, 0.D0 )

           DO ipr = 1, desc%npr
              !
              nr = desc_ip( ipr, ipc )%nr
              ir = desc_ip( ipr, ipc )%ir
              !
              root = rank_ip( ipr, ipc )

              IF( ipr-1 == desc%myr .AND. ipc-1 == desc%myc .AND. la_proc ) THEN
                 !
                 !  this proc sends his block
                 ! 
                 CALL mp_bcast( vc(:,1:nc), root, intra_bgrp_comm )
                 CALL ZGEMM( 'N', 'N', kdim, nc, nr, ( 1.D0, 0.D0 ), &
                          psi(1,ir), kdmx, vc, nx, beta, aux(1,ic), kdmx )
              ELSE
                 !
                 !  all other procs receive
                 ! 
                 CALL mp_bcast( vtmp(:,1:nc), root, intra_bgrp_comm )
                 CALL ZGEMM( 'N', 'N', kdim, nc, nr, ( 1.D0, 0.D0 ), &
                          psi(1,ir), kdmx, vtmp, nx, beta, aux(1,ic), kdmx )
              END IF
              ! 

              beta = ( 1.D0, 0.D0 )

           END DO
           !
        END IF
        !
     END DO
     !
     DEALLOCATE( vtmp )

     RETURN
  END SUBROUTINE refresh_evc

END SUBROUTINE protate_wfc_k
