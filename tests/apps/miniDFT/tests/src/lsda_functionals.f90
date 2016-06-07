!
! Copyright (C) 2001-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
subroutine pz_polarized (rs, ec, vc)
  !-----------------------------------------------------------------------
  !     J.P. Perdew and A. Zunger, PRB 23, 5048 (1981)
  !     spin-polarized energy and potential
  !
  USE kinds, ONLY : DP
  implicit none
  real(DP) :: rs, ec, vc
  real(DP) :: a, b, c, d, gc, b1, b2
  parameter (a = 0.01555d0, b = - 0.0269d0, c = 0.0007d0, d = &
       - 0.0048d0, gc = - 0.0843d0, b1 = 1.3981d0, b2 = 0.2611d0)
  real(DP) :: lnrs, rs12, ox, dox
  REAL(DP), PARAMETER :: xcprefact = 0.022575584d0, pi34 = 0.6203504908994d0 
  ! REAL(DP) :: betha, etha, csi, prefact
  !
  if (rs.lt.1.0d0) then
     ! high density formula
     lnrs = log (rs)
     ec = a * lnrs + b + c * rs * lnrs + d * rs
     vc = a * lnrs + (b - a / 3.d0) + 2.d0 / 3.d0 * c * rs * lnrs + &
          (2.d0 * d-c) / 3.d0 * rs
  else
     ! interpolation formula
     rs12 = sqrt (rs)
     ox = 1.d0 + b1 * rs12 + b2 * rs
     dox = 1.d0 + 7.d0 / 6.d0 * b1 * rs12 + 4.d0 / 3.d0 * b2 * rs
     ec = gc / ox
     vc = ec * dox / ox
  endif
  !
!  IF ( lxc_rel ) THEN
!     betha = prefact * pi34 / rs
!     etha = DSQRT( 1 + betha**2 )
!     csi = betha + etha
!     prefact = 1.0D0 - (3.0D0/2.0D0) * ( (betha*etha - log(csi))/betha**2 )**2
!     ec = ec * prefact
!     vc = vc * prefact
!  ENDIF
  return
end subroutine pz_polarized
!
!-----------------------------------------------------------------------
subroutine pz_spin (rs, zeta, ec, vcup, vcdw)
  !-----------------------------------------------------------------------
  !     J.P. Perdew and Y. Wang, PRB 45, 13244 (1992)
  !
  USE kinds, ONLY : DP
  implicit none
  real(DP) :: rs, zeta, ec, vcup, vcdw
  !
  real(DP) :: ecu, vcu, ecp, vcp, fz, dfz
  real(DP) :: p43, third
  parameter (p43 = 4.0d0 / 3.d0, third = 1.d0 / 3.d0)
  !
  ! unpolarized part (Perdew-Zunger formula)
  call pz (rs, 1, ecu, vcu)
  ! polarization contribution
  call pz_polarized (rs, ecp, vcp)
  !
  fz = ( (1.0d0 + zeta) **p43 + (1.d0 - zeta) **p43 - 2.d0) / &
       (2.d0**p43 - 2.d0)
  dfz = p43 * ( (1.0d0 + zeta) **third- (1.d0 - zeta) **third) &
       / (2.d0**p43 - 2.d0)
  !
  ec = ecu + fz * (ecp - ecu)
  vcup = vcu + fz * (vcp - vcu) + (ecp - ecu) * dfz * (1.d0 - zeta)
  vcdw = vcu + fz * (vcp - vcu) + (ecp - ecu) * dfz * ( - 1.d0 - &
       zeta)
  !
  return
end subroutine pz_spin
!
!---------

!-----------------------------------------------------------------------
subroutine pw_spin (rs, zeta, ec, vcup, vcdw)
  !-----------------------------------------------------------------------
  !     J.P. Perdew and Y. Wang, PRB 45, 13244 (1992)
  !
  USE kinds, ONLY : DP
  implicit none
  real(DP) :: rs, zeta, ec, vcup, vcdw
  ! xc parameters, unpolarised
  real(DP) :: a, a1, b1, b2, b3, b4, c0, c1, c2, c3, d0, d1
  parameter (a = 0.031091d0, a1 = 0.21370d0, b1 = 7.5957d0, b2 = &
       3.5876d0, b3 = 1.6382d0, b4 = 0.49294d0, c0 = a, c1 = 0.046644d0, &
       c2 = 0.00664d0, c3 = 0.01043d0, d0 = 0.4335d0, d1 = 1.4408d0)
  ! xc parameters, polarised
  real(DP) :: ap, a1p, b1p, b2p, b3p, b4p, c0p, c1p, c2p, c3p, d0p, &
       d1p
  parameter (ap = 0.015545d0, a1p = 0.20548d0, b1p = 14.1189d0, b2p &
       = 6.1977d0, b3p = 3.3662d0, b4p = 0.62517d0, c0p = ap, c1p = &
       0.025599d0, c2p = 0.00319d0, c3p = 0.00384d0, d0p = 0.3287d0, d1p &
       = 1.7697d0)
  ! xc parameters, antiferro
  real(DP) :: aa, a1a, b1a, b2a, b3a, b4a, c0a, c1a, c2a, c3a, d0a, &
       d1a
  parameter (aa = 0.016887d0, a1a = 0.11125d0, b1a = 10.357d0, b2a = &
       3.6231d0, b3a = 0.88026d0, b4a = 0.49671d0, c0a = aa, c1a = &
       0.035475d0, c2a = 0.00188d0, c3a = 0.00521d0, d0a = 0.2240d0, d1a &
       = 0.3969d0)
  real(DP) :: fz0
  parameter (fz0 = 1.709921d0)
  real(DP) :: rs12, rs32, rs2, zeta2, zeta3, zeta4, fz, dfz
  real(DP) :: om, dom, olog, epwc, vpwc
  real(DP) :: omp, domp, ologp, epwcp, vpwcp
  real(DP) :: oma, doma, ologa, alpha, vpwca
  !
  !     if(rs.lt.0.5d0) then
  ! high density formula (not implemented)
  !
  !     else if(rs.gt.100.d0) then
  ! low density formula  (not implemented)
  !
  !     else
  ! interpolation formula
  zeta2 = zeta * zeta
  zeta3 = zeta2 * zeta
  zeta4 = zeta3 * zeta
  rs12 = sqrt (rs)
  rs32 = rs * rs12
  rs2 = rs**2
  ! unpolarised
  om = 2.d0 * a * (b1 * rs12 + b2 * rs + b3 * rs32 + b4 * rs2)
  dom = 2.d0 * a * (0.5d0 * b1 * rs12 + b2 * rs + 1.5d0 * b3 * rs32 &
       + 2.d0 * b4 * rs2)
  olog = log (1.d0 + 1.0d0 / om)
  epwc = - 2.d0 * a * (1.d0 + a1 * rs) * olog
  vpwc = - 2.d0 * a * (1.d0 + 2.d0 / 3.d0 * a1 * rs) * olog - 2.d0 / &
       3.d0 * a * (1.d0 + a1 * rs) * dom / (om * (om + 1.d0) )
  ! polarized
  omp = 2.d0 * ap * (b1p * rs12 + b2p * rs + b3p * rs32 + b4p * rs2)
  domp = 2.d0 * ap * (0.5d0 * b1p * rs12 + b2p * rs + 1.5d0 * b3p * &
       rs32 + 2.d0 * b4p * rs2)
  ologp = log (1.d0 + 1.0d0 / omp)
  epwcp = - 2.d0 * ap * (1.d0 + a1p * rs) * ologp
  vpwcp = - 2.d0 * ap * (1.d0 + 2.d0 / 3.d0 * a1p * rs) * ologp - &
       2.d0 / 3.d0 * ap * (1.d0 + a1p * rs) * domp / (omp * (omp + 1.d0) &
       )
  ! antiferro
  oma = 2.d0 * aa * (b1a * rs12 + b2a * rs + b3a * rs32 + b4a * rs2)
  doma = 2.d0 * aa * (0.5d0 * b1a * rs12 + b2a * rs + 1.5d0 * b3a * &
       rs32 + 2.d0 * b4a * rs2)
  ologa = log (1.d0 + 1.0d0 / oma)
  alpha = 2.d0 * aa * (1.d0 + a1a * rs) * ologa
  vpwca = + 2.d0 * aa * (1.d0 + 2.d0 / 3.d0 * a1a * rs) * ologa + &
       2.d0 / 3.d0 * aa * (1.d0 + a1a * rs) * doma / (oma * (oma + 1.d0) &
       )
  !
  fz = ( (1.d0 + zeta) ** (4.d0 / 3.d0) + (1.d0 - zeta) ** (4.d0 / &
       3.d0) - 2.d0) / (2.d0** (4.d0 / 3.d0) - 2.d0)
  dfz = ( (1.d0 + zeta) ** (1.d0 / 3.d0) - (1.d0 - zeta) ** (1.d0 / &
       3.d0) ) * 4.d0 / (3.d0 * (2.d0** (4.d0 / 3.d0) - 2.d0) )
  !
  ec = epwc + alpha * fz * (1.d0 - zeta4) / fz0 + (epwcp - epwc) &
       * fz * zeta4
  !
  vcup = vpwc + vpwca * fz * (1.d0 - zeta4) / fz0 + (vpwcp - vpwc) &
       * fz * zeta4 + (alpha / fz0 * (dfz * (1.d0 - zeta4) - 4.d0 * fz * &
       zeta3) + (epwcp - epwc) * (dfz * zeta4 + 4.d0 * fz * zeta3) ) &
       * (1.d0 - zeta)

  vcdw = vpwc + vpwca * fz * (1.d0 - zeta4) / fz0 + (vpwcp - vpwc) &
       * fz * zeta4 - (alpha / fz0 * (dfz * (1.d0 - zeta4) - 4.d0 * fz * &
       zeta3) + (epwcp - epwc) * (dfz * zeta4 + 4.d0 * fz * zeta3) ) &
       * (1.d0 + zeta)
  !      endif
  !
  return
end subroutine pw_spin
!
!---------------------------------------------------------------
subroutine pbec_spin (rho, zeta, grho, iflag, sc, v1cup, v1cdw, v2c)
  !---------------------------------------------------------------
  !
  ! PBE correlation (without LDA part) - spin-polarized
  ! iflag = 1: J.P.Perdew, K.Burke, M.Ernzerhof, PRL 77, 3865 (1996).
  ! iflag = 2: J.P.Perdew et al., PRL 100, 136406 (2008)
  !
  USE kinds, ONLY : DP
  implicit none
  integer, intent(in) :: iflag
  real(DP) :: rho, zeta, grho, sc, v1cup, v1cdw, v2c
  real(DP) :: ga, be(2)
  parameter (ga = 0.031091d0)
  data be / 0.066725d0 ,  0.046d0 /
  real(DP) :: third, pi34, xkf, xks
  parameter (third = 1.d0 / 3.d0, pi34 = 0.6203504908994d0)
  parameter (xkf = 1.919158292677513d0, xks = 1.128379167095513d0)
  ! pi34=(3/4pi)^(1/3), xkf=(9 pi/4)^(1/3), xks= sqrt(4/pi)
  real(DP) :: kf, ks, rs, ec, vcup, vcdw, t, expe, af, y, xy, qy, &
       s1, h0, ddh0
  real(DP) :: fz, fz2, fz3, fz4, dfz, bfup, bfdw, dh0up, dh0dw, &
       dh0zup, dh0zdw
  !
  rs = pi34 / rho**third
  call pw_spin (rs, zeta, ec, vcup, vcdw)
  kf = xkf / rs
  ks = xks * sqrt (kf)
  fz = 0.5d0 * ( (1.d0 + zeta) ** (2.d0 / 3.d0) + (1.d0 - zeta) ** ( &
       2.d0 / 3.d0) )
  fz2 = fz * fz
  fz3 = fz2 * fz
  fz4 = fz3 * fz
  dfz = ( (1.d0 + zeta) ** ( - 1.d0 / 3.d0) - (1.d0 - zeta) ** ( - &
       1.d0 / 3.d0) ) / 3.d0
  t = sqrt (grho) / (2.d0 * fz * ks * rho)
  expe = exp ( - ec / (fz3 * ga) )
  af = be(iflag) / ga * (1.d0 / (expe-1.d0) )
  bfup = expe * (vcup - ec) / fz3
  bfdw = expe * (vcdw - ec) / fz3
  y = af * t * t
  xy = (1.d0 + y) / (1.d0 + y + y * y)
  qy = y * y * (2.d0 + y) / (1.d0 + y + y * y) **2
  s1 = 1.d0 + be(iflag) / ga * t * t * xy
  h0 = fz3 * ga * log (s1)
  dh0up = be(iflag) * t * t * fz3 / s1 * ( - 7.d0 / 3.d0 * xy - qy * &
       (af * bfup / be(iflag)-7.d0 / 3.d0) )
  dh0dw = be(iflag) * t * t * fz3 / s1 * ( - 7.d0 / 3.d0 * xy - qy * &
       (af * bfdw / be(iflag)-7.d0 / 3.d0) )
  dh0zup = (3.d0 * h0 / fz - be(iflag) * t * t * fz2 / s1 * (2.d0 * xy - &
  qy * (3.d0 * af * expe * ec / fz3 / be(iflag)+2.d0) ) ) * dfz * (1.d0 - zeta)
  dh0zdw = - (3.d0 * h0 / fz - be(iflag) * t * t * fz2 / s1 * (2.d0 * xy - &
  qy * (3.d0 * af * expe * ec / fz3 / be(iflag)+2.d0) ) ) * dfz * (1.d0 + zeta)

  ddh0 = be(iflag) * fz / (2.d0 * ks * ks * rho) * (xy - qy) / s1
  sc = rho * h0
  v1cup = h0 + dh0up + dh0zup
  v1cdw = h0 + dh0dw + dh0zdw
  v2c = ddh0
  return
end subroutine pbec_spin
!
!-----------------------------------------------------------------------
subroutine slater_spin (rho, zeta, ex, vxup, vxdw)
  !-----------------------------------------------------------------------
  !     Slater exchange with alpha=2/3, spin-polarized case
  !
  USE kinds, ONLY : DP
  implicit none
  real(DP) :: rho, zeta, ex, vxup, vxdw
  real(DP) :: f, alpha, third, p43
  parameter (f = - 1.10783814957303361d0, alpha = 2.0d0 / 3.0d0)
  ! f = -9/8*(3/pi)^(1/3)
  parameter (third = 1.d0 / 3.d0, p43 = 4.d0 / 3.d0)
  real(DP) :: exup, exdw, rho13
  !
  rho13 = ( (1.d0 + zeta) * rho) **third
  exup = f * alpha * rho13
  vxup = p43 * f * alpha * rho13
  rho13 = ( (1.d0 - zeta) * rho) **third
  exdw = f * alpha * rho13
  vxdw = p43 * f * alpha * rho13
  ex = 0.5d0 * ( (1.d0 + zeta) * exup + (1.d0 - zeta) * exdw)
  !
  return
end subroutine slater_spin

!
!-----------------------------------------------------------------------
function dpz_polarized (rs, iflg)
  !-----------------------------------------------------------------------
  !  derivative of the correlation potential with respect to local density
  !  Perdew and Zunger parameterization of the Ceperley-Alder functional
  !  spin-polarized case
  !
  USE kinds, only : DP
  USE constants, ONLY : pi, fpi
  !
  implicit none
  !
  real(DP), intent (in) :: rs
  integer, intent(in) :: iflg
  real(DP) :: dpz_polarized
  !
  !  local variables
  !  a,b,c,d,gc,b1,b2 are the parameters defining the functional
  !
  real(DP), parameter :: a = 0.01555d0, b = -0.0269d0, c = 0.0007d0, &
       d = -0.0048d0, gc = -0.0843d0, b1 = 1.3981d0, b2 = 0.2611d0,&
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
  dpz_polarized = - fpi * rs**4.d0 / 9.d0 * dmrs
  return
  !
end function dpz_polarized
