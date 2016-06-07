!
! Copyright (C) 2002-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!---------------------------------------------------------------------
function qe_erf (x)  
  !---------------------------------------------------------------------
  !
  !     Error function - computed from the rational approximations of
  !     W. J. Cody, Math. Comp. 22 (1969), pages 631-637.
  !
  !     for abs(x) le 0.47 erf is calculated directly
  !     for abs(x) gt 0.47 erf is calculated via erf(x)=1-erfc(x)
  !
  use kinds, only : DP
  implicit none  
  real(DP), intent(in) :: x
  real(DP) :: x2, p1 (4), q1 (4)
  real(DP), external :: qe_erfc  
  real(DP) :: qe_erf
  data p1 / 2.426679552305318E2_DP, 2.197926161829415E1_DP, &
            6.996383488619136_DP,  -3.560984370181538E-2_DP /
  data q1 / 2.150588758698612E2_DP, 9.116490540451490E1_DP, &
            1.508279763040779E1_DP, 1.000000000000000_DP /
  !
  if (abs (x) > 6.0_DP) then  
     !
     !  erf(6)=1-10^(-17) cannot be distinguished from 1
     !
     qe_erf = sign (1.0_DP, x)  
  else  
     if (abs (x)  <= 0.47_DP) then  
        x2 = x**2  
        qe_erf=x *(p1 (1) + x2 * (p1 (2) + x2 * (p1 (3) + x2 * p1 (4) ) ) ) &
                / (q1 (1) + x2 * (q1 (2) + x2 * (q1 (3) + x2 * q1 (4) ) ) )
     else  
        qe_erf = 1.0_DP - qe_erfc (x)  
     endif
  endif
  !
  return  
end function qe_erf
!
!---------------------------------------------------------------------
function qe_erfc (x)  
  !---------------------------------------------------------------------
  !
  !     erfc(x) = 1-erf(x)  - See comments in erf
  !
  use kinds, only : DP
  implicit none  
  real(DP),intent(in) :: x
  real(DP)            :: qe_erfc
  real(DP) :: ax, x2, xm2, p2 (8), q2 (8), p3 (5), q3 (5), pim1
  real(DP), external :: qe_erf  
  data p2 / 3.004592610201616E2_DP,  4.519189537118719E2_DP, &
            3.393208167343437E2_DP,  1.529892850469404E2_DP, &
            4.316222722205674E1_DP,  7.211758250883094_DP,   &
            5.641955174789740E-1_DP,-1.368648573827167E-7_DP /
  data q2 / 3.004592609569833E2_DP,  7.909509253278980E2_DP, &
            9.313540948506096E2_DP,  6.389802644656312E2_DP, &
            2.775854447439876E2_DP,  7.700015293522947E1_DP, &
            1.278272731962942E1_DP,  1.000000000000000_DP /
  data p3 /-2.996107077035422E-3_DP,-4.947309106232507E-2_DP, &
           -2.269565935396869E-1_DP,-2.786613086096478E-1_DP, &
           -2.231924597341847E-2_DP /
  data q3 / 1.062092305284679E-2_DP, 1.913089261078298E-1_DP, &
            1.051675107067932_DP,    1.987332018171353_DP,    &
            1.000000000000000_DP /

  data pim1 / 0.56418958354775629_DP /  
  !        ( pim1= sqrt(1/pi) )
  ax = abs (x)  
  if (ax > 26.0_DP) then  
     !
     !  erfc(26.0)=10^(-296); erfc( 9.0)=10^(-37);
     !
     qe_erfc = 0.0_DP  
  elseif (ax > 4.0_DP) then  
     x2 = x**2  
     xm2 = (1.0_DP / ax) **2  
     qe_erfc = (1.0_DP / ax) * exp ( - x2) * (pim1 + xm2 * (p3 (1) &
          + xm2 * (p3 (2) + xm2 * (p3 (3) + xm2 * (p3 (4) + xm2 * p3 (5) &
          ) ) ) ) / (q3 (1) + xm2 * (q3 (2) + xm2 * (q3 (3) + xm2 * &
          (q3 (4) + xm2 * q3 (5) ) ) ) ) )
  elseif (ax > 0.47_DP) then  
     x2 = x**2  
     qe_erfc = exp ( - x2) * (p2 (1) + ax * (p2 (2) + ax * (p2 (3) &
          + ax * (p2 (4) + ax * (p2 (5) + ax * (p2 (6) + ax * (p2 (7) &
          + ax * p2 (8) ) ) ) ) ) ) ) / (q2 (1) + ax * (q2 (2) + ax * &
          (q2 (3) + ax * (q2 (4) + ax * (q2 (5) + ax * (q2 (6) + ax * &
          (q2 (7) + ax * q2 (8) ) ) ) ) ) ) )
  else  
     qe_erfc = 1.0_DP - qe_erf (ax)  
  endif
  !
  ! erf(-x)=-erf(x)  =>  erfc(-x) = 2-erfc(x)
  !
  if (x < 0.0_DP) qe_erfc = 2.0_DP - qe_erfc  
  !
  return  
end function qe_erfc
!
!---------------------------------------------------------------------
function gauss_freq (x)
  !---------------------------------------------------------------------
  !
  !     gauss_freq(x) = (1+erf(x/sqrt(2)))/2 = erfc(-x/sqrt(2))/2
  !             - See comments in erf
  !
  use kinds, only : DP
  implicit none
  real(DP),intent(in) :: x
  real(DP)            :: gauss_freq
  real(DP), parameter :: c =  0.7071067811865475_DP
  !        ( c= sqrt(1/2) )
  real(DP), external :: qe_erfc
  !
  gauss_freq = 0.5_DP * qe_erfc ( - x * c)
  !
  return
end function gauss_freq


