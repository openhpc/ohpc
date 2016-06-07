!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
subroutine simpson (mesh, func, rab, asum)
  !-----------------------------------------------------------------------
  !
  !     simpson's rule integration. On input:
  !       mesh = mhe number of grid points (should be odd)
  !       func(i)= function to be integrated
  !       rab(i) = r(i) * dr(i)/di * di
  !     For the logarithmic grid not including r=0 :
  !       r(i) = r_0*exp((i-1)*dx) ==> rab(i)=r(i)*dx
  !     For the logarithmic grid including r=0 :
  !       r(i) = a(exp((i-1)*dx)-1) ==> rab(i)=(r(i)+a)*dx
  !     Output in asum = \sum_i c_i f(i)*rab(i) = \int_0^\infty f(r) dr 
  !     where c_i are alternativaly 2/3, 4/3 except c_1 = c_mesh = 1/3
  !
  use kinds, ONLY: DP
  implicit none
  integer, intent(in) :: mesh
  real(DP), intent(in) :: rab (mesh), func (mesh)
  real(DP), intent(out):: asum
  !
  real(DP) :: f1, f2, f3, r12
  integer :: i
  !
  !     routine assumes that mesh is an odd number so run check
  !     if ( mesh+1 - ( (mesh+1) / 2 ) * 2 .ne. 1 ) then
  !       write(*,*) '***error in subroutine radlg'
  !       write(*,*) 'routine assumes mesh is odd but mesh =',mesh+1
  !       stop
  !     endif
  asum = 0.0d0
  r12 = 1.0d0 / 12.0d0
  f3 = func (1) * rab (1) * r12

  do i = 2, mesh - 1, 2
     f1 = f3
     f2 = func (i) * rab (i) * r12
     f3 = func (i + 1) * rab (i + 1) * r12
     asum = asum + 4.0d0 * f1 + 16.0d0 * f2 + 4.0d0 * f3
  enddo

  return
end subroutine simpson

!=-----------------------------------------------------------------------
subroutine simpson_cp90( mesh, func, rab, asum )
  !-----------------------------------------------------------------------
  !
  !    This routine computes the integral of a function defined on a
  !    logaritmic mesh, by using the open simpson formula given on 
  !    pag. 109 of Numerical Recipes. In principle it is used to
  !    perform integrals from zero to infinity. The first point of
  !    the function should be the closest to zero but not the value
  !    in zero. The formula used here automatically includes the 
  !    contribution from the zero point and no correction is required.
  !
  !    Input as "simpson". At least 8 integrating points are required.
  !
  !    last revised 12 May 1995 by Andrea Dal Corso
  !
  use kinds, ONLY: DP
  implicit none
  integer, intent(in) :: mesh
  real(DP), intent(in) :: rab (mesh), func (mesh)
  real(DP), intent(out):: asum
  !
  real(DP) :: c(4)
  integer ::i
  !
  if ( mesh < 8 ) call errore ('simpson_cp90','few mesh points',8)

  c(1) = 109.0d0 / 48.d0
  c(2) = -5.d0 / 48.d0
  c(3) = 63.d0 / 48.d0
  c(4) = 49.d0 / 48.d0

  asum = ( func(1)*rab(1) + func(mesh  )*rab(mesh  ) )*c(1) &
       + ( func(2)*rab(2) + func(mesh-1)*rab(mesh-1) )*c(2) &
       + ( func(3)*rab(3) + func(mesh-2)*rab(mesh-2) )*c(3) &
       + ( func(4)*rab(4) + func(mesh-3)*rab(mesh-3) )*c(4)
  do i=5,mesh-4
     asum = asum + func(i)*rab(i)
  end do

  return
end subroutine simpson_cp90
!
!-----------------------------------------------------------------------
SUBROUTINE herman_skillman_int(mesh,func,rab,asum)
!-----------------------------------------------------------------------
  !     simpson rule integration for herman skillman mesh (obsolescent)
  !     Input as in "simpson". BEWARE: "func" is overwritten!!!
  !
  use kinds, ONLY: DP
  IMPLICIT NONE
  integer, intent(in) :: mesh
  real(DP), intent(in) :: rab (mesh)
  real(DP), intent(inout) :: func (mesh)
  real(DP), intent(out):: asum
  !
  INTEGER :: i, j, k, i1, nblock
  REAL(DP) :: a1, a2e, a2o, a2es
  !
  a1=0.0d0
  a2e=0.0d0
  asum=0.0d0
  nblock=mesh/40
  i=1
  func(1)=0.0d0
  DO j=1,nblock
     DO k=1,20
        i=i+2
        i1=i-1
        a2es=a2e
        a2o=func(i1)/12.0d0
        a2e=func(i)/12.0d0
        a1=a1+5.0d0*a2es+8.0d0*a2o-a2e
        func(i1)=asum+a1*rab(i1)
        a1=a1-a2es+8.0d0*a2o+5.0d0*a2e
        func(i)=asum+a1*rab(i)
     END DO
     asum=func(i)
     a1=0.0d0
  END DO
  !
  RETURN
END SUBROUTINE herman_skillman_int
