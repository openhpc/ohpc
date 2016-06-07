!
! Copyright (C) 2001-2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
SUBROUTINE gk_sort( k, ngm, g, ecut, ngk, igk, gk )
   !----------------------------------------------------------------------------
   !
   ! ... sorts k+g in order of increasing magnitude, up to ecut
   ! ... NB: this version should yield the same ordering for different ecut
   ! ...     and the same ordering in all machines
   !
   USE kinds,     ONLY : DP
   USE constants, ONLY : eps8
   USE wvfct,     ONLY : npwx
   !
   IMPLICIT NONE
   !
   REAL(DP), INTENT(in) :: k(3)      ! the k point
   INTEGER, INTENT(in) :: ngm        ! the number of g vectors
   REAL(DP), INTENT(in) :: g(3,ngm)  ! the coordinates of G vectors
   REAL(DP), INTENT(in) :: ecut      ! the cut-off energy
   INTEGER, INTENT(out) :: ngk       ! the number of k+G vectors inside the "ecut sphere"
   INTEGER, INTENT(out) :: igk(npwx) ! the correspondence k+G <-> G
   REAL(DP), INTENT(out) :: gk(npwx) ! the moduli of k+G
   !
   INTEGER :: ng   ! counter on   G vectors
   INTEGER :: nk   ! counter on k+G vectors
   REAL(DP) :: q   ! |k+G|^2
   REAL(DP) :: q2x ! upper bound for |G|
   !
   ! ... first we count the number of k+G vectors inside the cut-off sphere
   !
   q2x = ( sqrt( sum(k(:)**2) ) + sqrt( ecut ) )**2
   !
   ngk = 0
   igk(:) = 0
   gk (:) = 0.0_dp
   !
   DO ng = 1, ngm
      q = sum( ( k(:) + g(:,ng) )**2 )
      IF(q<=eps8) q=0.d0
      !
      ! ... here if |k+G|^2 <= Ecut
      !
      IF ( q <= ecut ) THEN
         ngk = ngk + 1
         IF ( ngk > npwx ) &
            CALL errore( 'gk_sort', 'array gk out-of-bounds', 1 )
         !
         gk(ngk) = q
         !
         ! set the initial value of index array
         igk(ngk) = ng
      ELSE
         ! if |G| > |k| + SQRT( Ecut )  stop search and order vectors
         IF ( sum( g(:,ng)**2 ) > ( q2x + eps8 ) ) exit
      ENDIF
   ENDDO
   !
   IF ( ng > ngm ) &
      CALL infomsg( 'gk_sort', 'unexpected exit from do-loop')
   !
   ! ... order vector gk keeping initial position in index
   !
   CALL hpsort_eps( ngk, gk, igk, eps8 )
   !
   ! ... now order true |k+G|
   !
   DO nk = 1, ngk
      gk(nk) = sum( (k(:) + g(:,igk(nk)) )**2 )
   ENDDO
   !
END SUBROUTINE gk_sort
