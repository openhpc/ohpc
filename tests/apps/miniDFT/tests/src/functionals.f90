!
! Copyright (C) 2001-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
subroutine slater (rs, ex, vx)
  !-----------------------------------------------------------------------
  !        Slater exchange with alpha=2/3
  !
  USE kinds, ONLY : DP
  implicit none
  real(DP) :: rs, ex, vx
  real(DP), parameter  :: f= -0.687247939924714d0, alpha = 2.0d0/3.0d0
  ! f = -9/8*(3/2pi)^(2/3)
  !
  ex = f * alpha / rs
  vx = 4.d0 / 3.d0 * f * alpha / rs
  !
  return
end subroutine slater
subroutine pz (rs, iflag, ec, vc)
  !-----------------------------------------------------------------------
  !     LDA parameterization from Monte Carlo data
  !     iflag=1: J.P. Perdew and A. Zunger, PRB 23, 5048 (1981)
  !     iflag=2: G. Ortiz and P. Ballone, PRB 50, 1391 (1994)
  !
  USE kinds, ONLY : DP
  implicit none
  real(DP) :: rs, ec, vc
  integer :: iflag
  !
  real(DP) :: a (2), b (2), c (2), d (2), gc (2), b1 (2), b2 (2)
  real(DP) :: lnrs, rs12, ox, dox
  !
  data a / 0.0311d0, 0.031091d0 /, b / -0.048d0, -0.046644d0 /, &
       c / 0.0020d0, 0.00419d0 /, d / -0.0116d0, -0.00983d0 /
  data gc / -0.1423d0, -0.103756d0 /, b1 / 1.0529d0, 0.56371d0 /, &
       b2 / 0.3334d0, 0.27358d0 /
  !
  if (rs.lt.1.0d0) then
     ! high density formula
     lnrs = log (rs)
     ec = a (iflag) * lnrs + b (iflag) + c (iflag) * rs * lnrs + d ( &
          iflag) * rs
     vc = a (iflag) * lnrs + (b (iflag) - a (iflag) / 3.d0) + 2.d0 / &
          3.d0 * c (iflag) * rs * lnrs + (2.d0 * d (iflag) - c (iflag) ) &
          / 3.d0 * rs
  else
     ! interpolation formula
     rs12 = sqrt (rs)
     ox = 1.d0 + b1 (iflag) * rs12 + b2 (iflag) * rs
     dox = 1.d0 + 7.d0 / 6.d0 * b1 (iflag) * rs12 + 4.d0 / 3.d0 * &
          b2 (iflag) * rs
     ec = gc (iflag) / ox
     vc = ec * dox / ox
  endif
  !
  return
end subroutine pz
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
subroutine pw (rs, iflag, ec, vc)
  !-----------------------------------------------------------------------
  !     iflag=1: J.P. Perdew and Y. Wang, PRB 45, 13244 (1992)
  !     iflag=2: G. Ortiz and P. Ballone, PRB 50, 1391 (1994)
  !
  USE kinds, ONLY : DP
  implicit none
  real(DP) :: rs, ec, vc
  integer :: iflag
  !
  real(DP) :: a, b1, b2, c0, c1, c2, c3, d0, d1
  parameter (a = 0.031091d0, b1 = 7.5957d0, b2 = 3.5876d0, c0 = a, &
       c1 = 0.046644d0, c2 = 0.00664d0, c3 = 0.01043d0, d0 = 0.4335d0, &
       d1 = 1.4408d0)
  real(DP) :: lnrs, rs12, rs32, rs2, om, dom, olog
  real(DP) :: a1 (2), b3 (2), b4 (2)
  data a1 / 0.21370d0, 0.026481d0 /, b3 / 1.6382d0, -0.46647d0 /, &
       b4 / 0.49294d0, 0.13354d0 /
  !
  ! high- and low-density formulae implemented but not used in PW case
  ! (reason: inconsistencies in PBE/PW91 functionals)
  !
  if (rs.lt.1d0.and.iflag.eq.2) then
     ! high density formula
     lnrs = log (rs)
     ec = c0 * lnrs - c1 + c2 * rs * lnrs - c3 * rs
     vc = c0 * lnrs - (c1 + c0 / 3.d0) + 2.d0 / 3.d0 * c2 * rs * &
          lnrs - (2.d0 * c3 + c2) / 3.d0 * rs
  elseif (rs.gt.100.d0.and.iflag.eq.2) then
     ! low density formula
     ec = - d0 / rs + d1 / rs**1.5d0
     vc = - 4.d0 / 3.d0 * d0 / rs + 1.5d0 * d1 / rs**1.5d0
  else
     ! interpolation formula
     rs12 = sqrt (rs)
     rs32 = rs * rs12
     rs2 = rs**2
     om = 2.d0 * a * (b1 * rs12 + b2 * rs + b3 (iflag) * rs32 + b4 ( &
          iflag) * rs2)
     dom = 2.d0 * a * (0.5d0 * b1 * rs12 + b2 * rs + 1.5d0 * b3 ( &
          iflag) * rs32 + 2.d0 * b4 (iflag) * rs2)
     olog = log (1.d0 + 1.0d0 / om)
     ec = - 2.d0 * a * (1.d0 + a1 (iflag) * rs) * olog
     vc = - 2.d0 * a * (1.d0 + 2.d0 / 3.d0 * a1 (iflag) * rs) &
          * olog - 2.d0 / 3.d0 * a * (1.d0 + a1 (iflag) * rs) * dom / &
          (om * (om + 1.d0) )
  endif
  !
  return
end subroutine pw
!
!---------------------------------------------------------------
subroutine pbex (rho, grho, iflag, sx, v1x, v2x)
  !---------------------------------------------------------------
  !
  ! PBE exchange (without Slater exchange):
  ! iflag=1  J.P.Perdew, K.Burke, M.Ernzerhof, PRL 77, 3865 (1996)
  ! iflag=2  "revised' PBE: Y. Zhang et al., PRL 80, 890 (1998)
  ! iflag=3  PBEsol: J.P.Perdew et al., PRL 100, 136406 (2008)
  !
  USE kinds, ONLY : DP
  USE constants, ONLY : pi
  implicit none
  real(DP) :: rho, grho, sx, v1x, v2x
  ! input: charge and squared gradient
  ! output: energy
  ! output: potential
  integer :: iflag
  ! local variables
  real(DP) :: kf, agrho, s1, s2, ds, dsg, exunif, fx
  ! (3*pi2*|rho|)^(1/3)
  ! |grho|
  ! |grho|/(2*kf*|rho|)
  ! s^2
  ! n*ds/dn
  ! n*ds/d(gn)
  ! exchange energy LDA part
  ! exchange energy gradient part
  real(DP) :: dxunif, dfx, f1, f2, f3, dfx1
  ! numerical coefficients (NB: c2=(3 pi^2)^(1/3) )
  real(DP) :: third, c1, c2, c5
  parameter (third = 1.d0 / 3.d0, c1 = 0.75d0 / pi , &
       c2 = 3.093667726280136d0, c5 = 4.d0 * third)
  ! parameters of the functional
  real(DP) :: k (3), mu(3)
  data k / 0.804d0, 1.2450D0, 0.804d0 /, &
       mu/ 0.21951d0, 0.21951d0, 0.12345679012345679012d0  /
  !
  agrho = sqrt (grho)
  kf = c2 * rho**third
  dsg = 0.5d0 / kf
  s1 = agrho * dsg / rho
  s2 = s1 * s1
  ds = - c5 * s1
  !
  !   Energy
  !
  f1 = s2 * mu(iflag) / k (iflag)
  f2 = 1.d0 + f1
  f3 = k (iflag) / f2
  fx = k (iflag) - f3
  exunif = - c1 * kf
  sx = exunif * fx
  !
  !   Potential
  !
  dxunif = exunif * third
  dfx1 = f2 * f2
  dfx = 2.d0 * mu(iflag) * s1 / dfx1
  v1x = sx + dxunif * fx + exunif * dfx * ds
  v2x = exunif * dfx * dsg / agrho

  sx = sx * rho
  return
end subroutine pbex
!
!---------------------------------------------------------------
subroutine pbec (rho, grho, iflag, sc, v1c, v2c)
  !---------------------------------------------------------------
  !
  ! PBE correlation (without LDA part)
  ! iflag=1: J.P.Perdew, K.Burke, M.Ernzerhof, PRL 77, 3865 (1996).
  ! iflag=2: J.P.Perdew et al., PRL 100, 136406 (2008).
  !
  USE kinds, ONLY : DP
  implicit none
  integer, intent(in) :: iflag
  real(DP) :: rho, grho, sc, v1c, v2c
  real(DP) :: ga, be (2)
  parameter (ga = 0.031091d0)
  data be / 0.066725d0, 0.046d0 /
  real(DP) :: third, pi34, xkf, xks
  parameter (third = 1.d0 / 3.d0, pi34 = 0.6203504908994d0)
  parameter (xkf = 1.919158292677513d0, xks = 1.128379167095513d0)
  ! pi34=(3/4pi)^(1/3), xkf=(9 pi/4)^(1/3), xks= sqrt(4/pi)
  real(DP) :: kf, ks, rs, ec, vc, t, expe, af, bf, y, xy, qy
  real(DP) :: s1, h0, dh0, ddh0
  !
  rs = pi34 / rho**third
  call pw (rs, 1, ec, vc)
  kf = xkf / rs
  ks = xks * sqrt (kf)
  t = sqrt (grho) / (2.d0 * ks * rho)
  expe = exp ( - ec / ga)
  af = be(iflag) / ga * (1.d0 / (expe-1.d0) )
  bf = expe * (vc - ec)
  y = af * t * t
  xy = (1.d0 + y) / (1.d0 + y + y * y)
  qy = y * y * (2.d0 + y) / (1.d0 + y + y * y) **2
  s1 = 1.d0 + be(iflag) / ga * t * t * xy
  h0 = ga * log (s1)
  dh0 = be(iflag) * t * t / s1 * ( - 7.d0 / 3.d0 * xy - qy * (af * bf / &
       be(iflag)-7.d0 / 3.d0) )
  ddh0 = be(iflag) / (2.d0 * ks * ks * rho) * (xy - qy) / s1
  sc = rho * h0
  v1c = h0 + dh0
  v2c = ddh0
  !
  return
end subroutine pbec
!
!-----------------------------------------------------------------------
function dpz (rs, iflg)
  !-----------------------------------------------------------------------
  !  derivative of the correlation potential with respect to local density
  !  Perdew and Zunger parameterization of the Ceperley-Alder functional
  !
  use kinds, only: DP
  USE constants, ONLY: pi, fpi
  !
  implicit none
  !
  real(DP), intent (in) :: rs
  integer, intent(in) :: iflg
  real(DP) :: dpz
  !
  !  local variables
  !  a,b,c,d,gc,b1,b2 are the parameters defining the functional
  !
  real(DP), parameter :: a = 0.0311d0, b = -0.048d0, c = 0.0020d0, &
       d = -0.0116d0, gc = -0.1423d0, b1 = 1.0529d0, b2 = 0.3334d0,&
       a1 = 7.0d0 * b1 / 6.d0, a2 = 4.d0 * b2 / 3.d0
  real(DP) :: x, den, dmx, dmrs
  !
  !
  if (iflg == 1) then
     dmrs = a / rs + 2.d0 / 3.d0 * c * (log (rs) + 1.d0) + &
          (2.d0 * d-c) / 3.d0
  else
     x = sqrt (rs)
     den = 1.d0 + x * (b1 + x * b2)
     dmx = gc * ( (a1 + 2.d0 * a2 * x) * den - 2.d0 * (b1 + 2.d0 * &
          b2 * x) * (1.d0 + x * (a1 + x * a2) ) ) / den**3
     dmrs = 0.5d0 * dmx / x
  endif
  !
  dpz = - fpi * rs**4.d0 / 9.d0 * dmrs
  return
  !
end function dpz
