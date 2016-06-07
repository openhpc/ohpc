!
! Copyright (C) 2001-2008 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
logical function eqvect (x, y, f)
  !-----------------------------------------------------------------------
  !
  !   This function test if the difference x-y-f is an integer.
  !   x, y = 3d vectors in crystal axis, f = fractionary translation
  !
  USE kinds
  implicit none
  real(DP), intent(in) :: x (3), y (3), f (3)
  !
  real(DP), parameter :: accep = 1.0d-5 ! acceptance parameter
  !
  eqvect = abs( x(1)-y(1)-f(1) - nint(x(1)-y(1)-f(1)) ) < accep .and. &
           abs( x(2)-y(2)-f(2) - nint(x(2)-y(2)-f(2)) ) < accep .and. &
           abs( x(3)-y(3)-f(3) - nint(x(3)-y(3)-f(3)) ) < accep
  !
  return
end function eqvect
