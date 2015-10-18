!
! Copyright (C) 2004 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
subroutine invmat (n, a, a_inv, da)
  !-----------------------------------------------------------------------
  ! computes the inverse "a_inv" of matrix "a", both dimensioned (n,n)
  ! if the matrix is dimensioned 3x3, it also computes determinant "da"
  ! matrix "a" is unchanged on output - LAPACK
  !
  USE kinds, ONLY : DP
  implicit none
  integer :: n
  real(DP), DIMENSION (n,n) :: a, a_inv
  real(DP) :: da
  !
  integer :: info, lda, lwork, ipiv (n)
  ! info=0: inversion was successful
  ! lda   : leading dimension (the same as n)
  ! ipiv  : work space for pivoting (assumed of length lwork=n)
  real(DP) :: work (n) 
  ! more work space
  !
  lda = n
  lwork=n
  !
  a_inv(:,:) = a(:,:)
  !
  call dgetrf (n, n, a_inv, lda, ipiv, info)
  call errore ('invmat', 'error in DGETRF', abs (info) )
  call dgetri (n, a_inv, lda, ipiv, work, lwork, info)
  call errore ('invmat', 'error in DGETRI', abs (info) )
  !
  if (n == 3) then
     da = a(1,1)*(a(2,2)*a(3,3)-a(2,3)*a(3,2)) + &
          a(1,2)*(a(2,3)*a(3,1)-a(2,1)*a(3,3)) + &
          a(1,3)*(a(2,1)*a(3,2)-a(3,1)*a(2,2))
     IF (ABS(da) < 1.d-10) CALL errore(' invmat ',' singular matrix ', 1)
  else
     da = 0.d0
  end if

  return
end subroutine invmat

