!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
subroutine dylmr2 (nylm, ngy, g, gg, dylm, ipol)
  !-----------------------------------------------------------------------
  !
  !     compute \partial Y_lm(G) \over \partial (G)_ipol
  !     using simple numerical derivation (SdG)
  !     The spherical harmonics are calculated in ylmr2
  !
  USE kinds, ONLY : DP
  implicit none
  !
  !    here the I/O variables
  !
  integer :: nylm, ngy, ipol
  ! input: number of spherical harmonics
  ! input: the number of g vectors to compute
  ! input: desired polarization
  real(DP) :: g (3, ngy), gg (ngy), dylm (ngy, nylm)
  ! input: the coordinates of g vectors
  ! input: the moduli of g vectors
  ! output: the spherical harmonics derivatives
  !
  !    and here the local variables
  !
  integer :: ig, lm
  ! counter on g vectors
  ! counter on l,m component

  real(DP), parameter :: delta = 1.d-6
  real(DP), allocatable :: dg (:), dgi (:), gx (:,:), ggx (:), ylmaux (:,:)
  ! dg is the finite increment for numerical derivation:
  ! dg = delta |G| = delta * sqrt(gg)
  ! dgi= 1 /(delta * sqrt(gg))
  ! gx = g +/- dg
  ! ggx = gx^2
  !
  allocate ( gx(3,ngy), ggx(ngy), dg(ngy), dgi(ngy), ylmaux(ngy,nylm) )

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(ig)
  do ig = 1, ngy
     dg (ig) = delta * sqrt (gg (ig) )
     if (gg (ig) .gt. 1.d-9) then
        dgi (ig) = 1.d0 / dg (ig)
     else
        dgi (ig) = 0.d0
     endif
  enddo
!$OMP END PARALLEL DO

  call dcopy (3 * ngy, g, 1, gx, 1)

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(ig)
  do ig = 1, ngy
     gx (ipol, ig) = g (ipol, ig) + dg (ig)
     ggx (ig) = gx (1, ig) * gx (1, ig) + &
                gx (2, ig) * gx (2, ig) + &
                gx (3, ig) * gx (3, ig)
  enddo
!$OMP END PARALLEL DO

  call ylmr2 (nylm, ngy, gx, ggx, dylm)

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(ig)
  do ig = 1, ngy
     gx (ipol, ig) = g (ipol, ig) - dg (ig)
     ggx (ig) = gx (1, ig) * gx (1, ig) + &
                gx (2, ig) * gx (2, ig) + &
                gx (3, ig) * gx (3, ig)
  enddo
!$OMP END PARALLEL DO

  call ylmr2 (nylm, ngy, gx, ggx, ylmaux)

  call daxpy (ngy * nylm, - 1.d0, ylmaux, 1, dylm, 1)

  do lm = 1, nylm
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(ig)
     do ig = 1, ngy
        dylm (ig, lm) = dylm (ig, lm) * 0.5d0 * dgi (ig)
     enddo
!$OMP END PARALLEL DO
  enddo

  deallocate ( gx, ggx, dg, dgi, ylmaux )

  return
end subroutine dylmr2

