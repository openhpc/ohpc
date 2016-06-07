
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------
subroutine init_us_1
  !----------------------------------------------------------------------
  !
  !   This routine performs the following tasks:
  !   a) For each non vanderbilt pseudopotential it computes the D and
  !      the betar in the same form of the Vanderbilt pseudopotential.
  !   b) It computes the indices indv which establish the correspondence
  !      nh <-> beta in the atom
  !   c) It computes the indices nhtol which establish the correspondence
  !      nh <-> angular momentum of the beta function
  !   d) It computes the indices nhtolm which establish the correspondence
  !      nh <-> combined (l,m) index for the beta function.
  !   e) It computes the coefficients c_{LM}^{nm} which relates the
  !      spherical harmonics in the Q expansion
  !   f) It computes the radial fourier transform of the Q function on
  !      all the g vectors
  !   g) It computes the q terms which define the S matrix.
  !   h) It fills the interpolation table for the beta functions
  !
  USE kinds,        ONLY : DP
  USE parameters,   ONLY : lmaxx
  USE constants,    ONLY : fpi, sqrt2
  USE atom,         ONLY : rgrid
  USE ions_base,    ONLY : ntyp => nsp
  USE cell_base,    ONLY : omega, tpiba
  USE gvect,        ONLY : g, gg
  USE lsda_mod,     ONLY : nspin
  USE us,           ONLY : nqxq, dq, nqx, tab, tab_d2y, qrad, spline_ps
  USE splinelib
  USE uspp,         ONLY : nhtol, nhtoj, nhtolm, ijtoh, dvan, qq, indv,&
                           ap, aainit, qq_so, dvan_so, okvan
  USE uspp_param,   ONLY : upf, lmaxq, nbetam, nh, nhm, lmaxkb
  USE spin_orb,     ONLY : lspinorb, rot_ylm, fcoef
  USE mp_global,    ONLY : intra_bgrp_comm
  USE mp,           ONLY : mp_sum
  !
  implicit none
  !
  !     here a few local variables
  !
  integer :: nt, ih, jh, nb, mb, ijv, l, m, ir, iq, is, startq, &
       lastq, ilast, ndm
  ! various counters
  real(DP), allocatable :: aux (:), aux1 (:), besr (:), qtot (:,:)
  ! various work space
  real(DP) :: prefr, pref, q, qi
  ! the prefactor of the q functions
  ! the prefactor of the beta functions
  ! the modulus of g for each shell
  ! q-point grid for interpolation
  real(DP), allocatable :: ylmk0 (:)
  ! the spherical harmonics
  real(DP) ::  vqint, j
  ! interpolated value
  ! J=L+S (noninteger!)
  integer :: n1, m0, m1, n, li, mi, vi, vj, ijs, is1, is2, &
             lk, mk, vk, kh, lh
  integer, external :: sph_ind
  complex(DP) :: coeff, qgm(1)
  real(DP) :: spinor, ji, jk
  !
  real(DP), allocatable :: xdata(:)
  real(DP) :: d1
  !
  call start_clock ('init_us_1')
  !
  !    Initialization of the variables
  !
  ndm = MAXVAL ( upf(:)%kkbeta )
  allocate (aux ( ndm))    
  allocate (aux1( ndm))    
  allocate (besr( ndm))    
  allocate (qtot( ndm , nbetam*(nbetam+1)/2 ))    
  allocate (ylmk0( lmaxq * lmaxq))    
  ap (:,:,:)   = 0.d0
  if (lmaxq > 0) qrad(:,:,:,:)= 0.d0
  !
  ! the following prevents an out-of-bound error: upf(nt)%nqlc=2*lmax+1
  ! but in some versions of the PP files lmax is not set to the maximum
  ! l of the beta functions but includes the l of the local potential
  !
  do nt=1,ntyp
     upf(nt)%nqlc = MIN ( upf(nt)%nqlc, lmaxq )
     IF ( upf(nt)%nqlc < 0 )  upf(nt)%nqlc = 0
  end do

  prefr = fpi / omega
  if (lspinorb) then
!
!  In the spin-orbit case we need the unitary matrix u which rotates the
!  real spherical harmonics and yields the complex ones.
!
     rot_ylm=(0.d0,0.d0)
     l=lmaxx
     rot_ylm(l+1,1)=(1.d0,0.d0)
     do n1=2,2*l+1,2
       m=n1/2
       n=l+1-m
       rot_ylm(n,n1)=CMPLX((-1.d0)**m/sqrt2,0.0_dp,kind=DP)
       rot_ylm(n,n1+1)=CMPLX(0.d0,-(-1.d0)**m/sqrt2,kind=DP)
       n=l+1+m
       rot_ylm(n,n1)=CMPLX(1.0_dp/sqrt2,0.d0,kind=DP)
       rot_ylm(n,n1+1)=CMPLX(0.d0, 1.0_dp/sqrt2,kind=DP)
     enddo
     fcoef=(0.d0,0.d0)
     dvan_so = (0.d0,0.d0)
     qq_so=(0.d0,0.d0)
     qq  = 0.d0
  else
     qq  = 0.d0
     dvan = 0.d0
  endif
  !
  !   For each pseudopotential we initialize the indices nhtol, nhtolm,
  !   nhtoj, indv, and if the pseudopotential is of KB type we initialize the
  !   atomic D terms
  !
  do nt = 1, ntyp
     ih = 1
     do nb = 1, upf(nt)%nbeta
        l = upf(nt)%lll (nb)
        do m = 1, 2 * l + 1
           nhtol (ih, nt) = l
           nhtolm(ih, nt) = l*l+m
           indv  (ih, nt) = nb
           ih = ih + 1
        enddo
     enddo
     ! ijtoh map augmentation channel indexes ih and jh to composite
     ! "triangular" index ijh
     ijtoh(:,:,nt) = -1
     ijv = 0
     do ih = 1,nh(nt)
         do jh = ih,nh(nt)
             ijv = ijv+1
             ijtoh(ih,jh,nt) = ijv
             ijtoh(jh,ih,nt) = ijv
         enddo
     enddo
     !
     !    From now on the only difference between KB and US pseudopotentials
     !    is in the presence of the q and Q functions.
     !
     !    Here we initialize the D of the solid
     !
        do ih = 1, nh (nt)
          do jh = 1, nh (nt)
            if (nhtol (ih, nt) == nhtol (jh, nt) .and. &
              nhtolm(ih, nt) == nhtolm(jh, nt) ) then
              ir = indv (ih, nt)
              is = indv (jh, nt)
              if (lspinorb) then
                 dvan_so (ih, jh, 1, nt) = upf(nt)%dion (ir, is)
                 dvan_so (ih, jh, 4, nt) = upf(nt)%dion (ir, is)
              else
                 dvan (ih, jh, nt) = upf(nt)%dion (ir, is)
              endif
            endif
          enddo
        enddo
  enddo
  !
  !   here for the US types we compute the Fourier transform of the
  !   Q functions.
  !   
  call divide (intra_bgrp_comm, nqxq, startq, lastq)
  !
  do nt = 1, ntyp
     ! ntyp
  enddo
  !
  !   and finally we compute the qq coefficients by integrating the Q.
  !   q are the g=0 components of Q.
  !
  if (gg (1) > 1.0d-8) goto 100
  call ylmr2 (lmaxq * lmaxq, 1, g, gg, ylmk0)
100 continue
    call mp_sum(  qq , intra_bgrp_comm )
  !
  !     fill the interpolation table tab
  !
  pref = fpi / sqrt (omega)
  call divide (intra_bgrp_comm, nqx, startq, lastq)
  tab (:,:,:) = 0.d0
  do nt = 1, ntyp
     do nb = 1, upf(nt)%nbeta
        l = upf(nt)%lll (nb)
        do iq = startq, lastq
           qi = (iq - 1) * dq
           call sph_bes (upf(nt)%kkbeta, rgrid(nt)%r, qi, l, besr)
           do ir = 1, upf(nt)%kkbeta
              aux (ir) = upf(nt)%beta (ir, nb) * besr (ir) * rgrid(nt)%r(ir)
           enddo
           call simpson (upf(nt)%kkbeta, aux, rgrid(nt)%rab, vqint)
           tab (iq, nb, nt) = vqint * pref
        enddo
     enddo
  enddo

  call mp_sum(  tab, intra_bgrp_comm )

  ! initialize spline interpolation
  if (spline_ps) then
     allocate( xdata(nqx) )
     do iq = 1, nqx
        xdata(iq) = (iq - 1) * dq
     enddo
     do nt = 1, ntyp
        do nb = 1, upf(nt)%nbeta 
           d1 = (tab(2,nb,nt) - tab(1,nb,nt)) / dq
           call spline(xdata, tab(:,nb,nt), 0.d0, d1, tab_d2y(:,nb,nt))
        enddo
     enddo
     deallocate(xdata)
  endif

  deallocate (ylmk0)
  deallocate (qtot)
  deallocate (besr)
  deallocate (aux1)
  deallocate (aux)

  call stop_clock ('init_us_1')
  return
end subroutine init_us_1

