!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
#define TEST_NEW_PRECONDITIONING
!
!-----------------------------------------------------------------------
subroutine g_psi (lda, n, m, npol, psi, e)
  !-----------------------------------------------------------------------
  !
  !    This routine computes an estimate of the inverse Hamiltonian
  !    and applies it to m wavefunctions
  !
  USE kinds
  USE g_psi_mod
  implicit none
  integer :: lda, n, m, npol, ipol
  ! input: the leading dimension of psi
  ! input: the real dimension of psi
  ! input: the number of bands
  ! input: the number of coordinates of psi
  ! local variable: counter of coordinates of psi
  real(DP) :: e (m)
  ! input: the eigenvectors
  complex(DP) :: psi (lda, npol, m)
  ! inp/out: the psi vector
  !
  !    Local variables
  !
  real(DP), parameter :: eps = 1.0d-4
  ! a small number
  real(DP) :: x, scala, denm
  integer :: k, i
  ! counter on psi functions
  ! counter on G vectors
  !
  call start_clock ('g_psi')
  !
#ifdef TEST_NEW_PRECONDITIONING
  scala = 1.d0
  do ipol=1,npol
     do k = 1, m
        do i = 1, n
           x = (h_diag(i,ipol) - e(k)*s_diag(i,ipol))*scala
           denm = (1.d0+x+sqrt(1.d0+(x-1)*(x-1.d0)))/scala
        !         denm = 1.d0 + 16*x*x*x*x/(27.d0+18*x+12*x*x+8*x*x*x)
           psi (i, ipol, k) = psi (i, ipol, k) / denm
        enddo
     enddo
  enddo
#else
  do ipol=1,npol
     do k = 1, m
        do i = 1, n
           denm = h_diag (i,ipol) - e (k) * s_diag (i,ipol)
        !
        ! denm = g2+v(g=0) - e(k)
        !
           if (abs (denm) < eps) denm = sign (eps, denm)
        !
        ! denm = sign( max( abs(denm),eps ), denm )
        !
           psi (i, ipol, k) = psi (i, ipol, k) / denm
        enddo
     enddo
  enddo
#endif

  call stop_clock ('g_psi')
  return
end subroutine g_psi
