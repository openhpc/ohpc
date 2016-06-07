!
! Copyright (C) 2001-2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
SUBROUTINE rgen ( dtau, rmax, mxr, at, bg, r, r2, nrm)
  !-----------------------------------------------------------------------
  !
  !   generates neighbours shells (cartesian, in units of lattice parameter)
  !   with length < rmax,and returns them in order of increasing length:
  !      r(:) = i*a1(:) + j*a2(:) + k*a3(:) - dtau(:),   r2 = r^2
  !   where a1, a2, a3 are primitive lattice vectors. Other input variables:
  !     mxr = maximum number of vectors
  !     at  = lattice vectors ( a1=at(:,1), a2=at(:,2), a3=at(:,3) )
  !     bg  = reciprocal lattice vectors ( b1=bg(:,1), b2=bg(:,2), b3=bg(:,3) )
  !   Other output variables:
  !     nrm = the number of vectors with r^2 < rmax^2
  !
  USE kinds, ONLY : DP
  !
  IMPLICIT NONE
  INTEGER, INTENT(in) :: mxr
  INTEGER, INTENT(out):: nrm
  REAL(DP), INTENT(in) :: at(3,3), bg(3,3), dtau(3), rmax
  REAL(DP), INTENT(out):: r(3,mxr), r2(mxr)
  !
  !    and here the local variables
  !
  INTEGER, ALLOCATABLE :: irr (:)
  INTEGER ::  nm1, nm2, nm3, i, j, k, ipol, ir, indsw, iswap
  real(DP) :: ds(3), dtau0(3)
  real(DP) :: t (3), tt, swap
  real(DP), EXTERNAL :: dnrm2
  !
  !
  nrm = 0
  IF (rmax==0.d0) RETURN

  ! bring dtau into the unit cell centered on the origin - prevents trouble
  ! if atomic positions are not centered around the origin but displaced
  ! far away (remember that translational invariance allows this!)
  !
  ds(:) = matmul( dtau(:), bg(:,:) )
  ds(:) = ds(:) - anint(ds(:))
  dtau0(:) = matmul( at(:,:), ds(:) )
  !
  ALLOCATE (irr( mxr))
  !
  ! these are estimates of the maximum values of needed integer indices
  !
  nm1 = int (dnrm2 (3, bg (1, 1), 1) * rmax) + 2
  nm2 = int (dnrm2 (3, bg (1, 2), 1) * rmax) + 2
  nm3 = int (dnrm2 (3, bg (1, 3), 1) * rmax) + 2
  !
  DO i = -nm1, nm1
     DO j = -nm2, nm2
        DO k = -nm3, nm3
           tt = 0.d0
           DO ipol = 1, 3
              t (ipol) = i*at (ipol, 1) + j*at (ipol, 2) + k*at (ipol, 3) &
                       - dtau0(ipol)
              tt = tt + t (ipol) * t (ipol)
           ENDDO
           IF (tt<=rmax**2.and.abs (tt) >1.d-10) THEN
              nrm = nrm + 1
              IF (nrm>mxr) CALL errore ('rgen', 'too many r-vectors', nrm)
              DO ipol = 1, 3
                 r (ipol, nrm) = t (ipol)
              ENDDO
              r2 (nrm) = tt
           ENDIF
        ENDDO
     ENDDO
  ENDDO
  !
  !   reorder the vectors in order of increasing magnitude
  !
  !   initialize the index inside sorting routine
  !
  irr (1) = 0
  IF (nrm>1) CALL hpsort (nrm, r2, irr)
  DO ir = 1, nrm - 1
20   indsw = irr (ir)
     IF (indsw/=ir) THEN
        DO ipol = 1, 3
           swap = r (ipol, indsw)
           r (ipol, indsw) = r (ipol, irr (indsw) )
           r (ipol, irr (indsw) ) = swap
        ENDDO
        iswap = irr (ir)
        irr (ir) = irr (indsw)
        irr (indsw) = iswap
        GOTO 20
     ENDIF

  ENDDO
  DEALLOCATE(irr)
  !
  RETURN
END SUBROUTINE rgen

