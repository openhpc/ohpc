!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
subroutine interpolate (v, vs, iflag)
  !
  !     This subroutine interpolates :
  !     from the smooth mesh (vs) to a  thicker mesh (v)  (iflag>0)
  !        vs is unchanged on output
  !     from the  thick mesh (v ) to a smoother mesh (vs) (iflag<=0)
  !        v  is unchanged on output
  !     V and Vs are real and in real space . V and Vs may coincide
  !
  USE kinds, ONLY: DP
  USE gvect,  ONLY: nl, nlm
  USE gvecs,ONLY: ngms, nls, nlsm, doublegrid
  USE fft_base,      ONLY : dfftp, dffts
  USE fft_interfaces,ONLY : fwfft, invfft
  !
  implicit none
  real(DP) :: v (dfftp%nnr), vs (dffts%nnr)
  ! function on thick mesh
  ! function on smooth mesh

  complex(DP), allocatable :: aux (:), auxs (:)
  ! work array on thick mesh
  ! work array on smooth mesh

  integer :: iflag
  ! gives the direction of the interpolation

  integer :: ig, ir

  call start_clock ('interpolate')
  if (iflag <= 0) then
     !
     !    from thick to smooth
     !
     if (doublegrid) then
        allocate (aux( dfftp%nnr))    
        allocate (auxs(dffts%nnr))    
        aux (:) = v (:)
        CALL fwfft ('Dense', aux, dfftp)
        auxs (:) = (0.d0, 0.d0)
        do ig = 1, ngms
           auxs (nls (ig) ) = aux (nl (ig) )
        enddo
        CALL invfft ('Smooth', auxs, dffts)
        vs (:) = auxs (:)
        deallocate (auxs)
        deallocate (aux)
     else
        do ir = 1, dfftp%nnr
           vs (ir) = v (ir)
        enddo
     endif
  else
     !
     !   from smooth to thick
     !
     if (doublegrid) then
        allocate (aux( dfftp%nnr))    
        allocate (auxs(dffts%nnr))    
        auxs (:) = vs (:)
        CALL fwfft ('Smooth', auxs, dffts)
        aux (:) = (0.d0, 0.d0)
        do ig = 1, ngms
           aux (nl (ig) ) = auxs (nls (ig) )
        enddo
        CALL invfft ('Dense', aux, dfftp)
        v (:) = aux (:)
        deallocate (auxs)
        deallocate (aux)
     else
        do ir = 1, dfftp%nnr
           v (ir) = vs (ir)
        enddo
     endif
  endif
  call stop_clock ('interpolate')
  return
end subroutine interpolate
!
subroutine cinterpolate (v, vs, iflag)
  !
  !     This subroutine interpolates :
  !     from the smooth mesh (vs) to a  thicker mesh (v)  (iflag>0)
  !        vs is unchanged on output
  !     from the  thick mesh (v ) to a smoother mesh (vs) (iflag<=0)
  !        v  is unchanged on output
  !     V and Vs are complex and in real space . V and Vs may coincide
  !
  USE kinds, ONLY: DP
  USE gvect,  ONLY: nl, nlm
  USE gvecs,ONLY: ngms, nls, nlsm, doublegrid
!!$  USE control_flags, ONLY: gamma_only
  USE fft_base,      ONLY : dfftp, dffts
  USE fft_interfaces,ONLY : fwfft, invfft
  !
  IMPLICIT NONE
  complex(DP) :: v (dfftp%nnr), vs (dffts%nnr)
  ! function on thick mesh
  ! function on smooth mesh

  integer :: iflag
  ! gives the direction of the interpolation

  complex(DP), allocatable :: aux (:), auxs (:)
  ! work array on thick mesh
  ! work array on smooth mesh

  integer :: ig

!!$  if (gamma_only) call errore ('cinterpolate','not allowed', 1)
  call start_clock ('cinterpolate')
  if (iflag <= 0) then
     !
     !    from thick to smooth
     !
     if (doublegrid) then
        allocate (aux ( dfftp%nnr))    
        aux (:) = v(:) 
        CALL fwfft ('Dense', aux, dfftp)
        vs (:) = (0.d0, 0.d0)
        do ig = 1, ngms
           vs (nls (ig) ) = aux (nl (ig) )
        enddo
        CALL invfft ('Smooth', vs, dffts)
        deallocate (aux)
     else
        call zcopy (dfftp%nnr, v, 1, vs, 1)
     endif
  else
     !
     !   from smooth to thick
     !
     if (doublegrid) then
        allocate (auxs (dffts%nnr))    
        auxs (:) = vs(:)
        CALL fwfft ('Smooth', auxs, dffts)
        v (:) = (0.d0, 0.d0)
        do ig = 1, ngms
           v (nl (ig) ) = auxs (nls (ig) )
        enddo
        CALL invfft ('Dense', v, dfftp)
        deallocate (auxs)
     else
        call zcopy (dfftp%nnr, vs, 1, v, 1)
     endif
  endif
  call stop_clock ('cinterpolate')
  return
end subroutine cinterpolate
