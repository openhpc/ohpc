!
! Copyright (C) 2001-2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-------------------------------------------------------------------------
subroutine latgen(ibrav,celldm,a1,a2,a3,omega)
  !-----------------------------------------------------------------------
  !     sets up the crystallographic vectors a1, a2, and a3.
  !
  !     ibrav is the structure index:
  !       1  cubic P (sc)                8  orthorhombic P
  !       2  cubic F (fcc)               9  1-face (base) centered orthorhombic
  !       3  cubic I (bcc)              10  all face centered orthorhombic
  !       4  hexagonal and trigonal P   11  body centered orthorhombic
  !       5  trigonal R, 3-fold axis c  12  monoclinic P (unique axis: c)
  !       6  tetragonal P (st)          13  one face (base) centered monoclinic
  !       7  tetragonal I (bct)         14  triclinic P
  !     Also accepted:
  !       0  "free" structure          -12  monoclinic P (unique axis: b)
  !      -5  trigonal R, threefold axis along (111) 
  !      -9  alternate description for base centered orthorhombic
  !
  !     celldm are parameters which fix the shape of the unit cell
  !     omega is the unit-cell volume
  !
  !     NOTA BENE: all axis sets are right-handed
  !     Boxes for US PPs do not work properly with left-handed axis
  !
  use kinds, only: DP
  implicit none
  integer, intent(in) :: ibrav
  real(DP), intent(inout) :: celldm(6)
  real(DP), intent(inout) :: a1(3), a2(3), a3(3)
  real(DP), intent(out) :: omega
  !
  real(DP), parameter:: sr2 = 1.414213562373d0, &
                        sr3 = 1.732050807569d0
  integer :: i,j,k,l,iperm,ir
  real(DP) :: term, cbya, s, term1, term2, singam, sen
  !
  !  user-supplied lattice vectors
  !
  if (ibrav == 0) then
     if (SQRT( a1(1)**2 + a1(2)**2 + a1(3)**2 ) == 0 )  &
         call errore ('latgen', 'wrong at for ibrav=0', 1)
     if (SQRT( a2(1)**2 + a2(2)**2 + a2(3)**2 ) == 0 )  &
         call errore ('latgen', 'wrong at for ibrav=0', 2)
     if (SQRT( a3(1)**2 + a3(2)**2 + a3(3)**2 ) == 0 )  &
         call errore ('latgen', 'wrong at for ibrav=0', 3)

     if ( celldm(1) /= 0.D0 ) then
     !
     ! ... input at are in units of alat => convert them to a.u.
     !
         a1(:) = a1(:) * celldm(1)
         a2(:) = a2(:) * celldm(1)
         a3(:) = a3(:) * celldm(1)
     else
     !
     ! ... input at are in atomic units: define celldm(1) from a1
     !
         celldm(1) = SQRT( a1(1)**2 + a1(2)**2 + a1(3)**2 )
     end if
     !
  else
     a1(:) = 0.d0
     a2(:) = 0.d0
     a3(:) = 0.d0
  end if
  !
  if (celldm (1) <= 0.d0) call errore ('latgen', 'wrong celldm(1)', ibrav)
  !
  !  index of bravais lattice supplied
  !
  if (ibrav == 1) then
     !
     !     simple cubic lattice
     !
     a1(1)=celldm(1)
     a2(2)=celldm(1)
     a3(3)=celldm(1)
     !
  else if (ibrav == 2) then
     !
     !     fcc lattice
     !
     term=celldm(1)/2.d0
     a1(1)=-term
     a1(3)=term
     a2(2)=term
     a2(3)=term
     a3(1)=-term
     a3(2)=term
     !
  else if (ibrav == 3) then
     !
     !     bcc lattice
     !
     term=celldm(1)/2.d0
     do ir=1,3
        a1(ir)=term
        a2(ir)=term
        a3(ir)=term
     end do
     a2(1)=-term
     a3(1)=-term
     a3(2)=-term
     !
  else if (ibrav == 4) then
     !
     !     hexagonal lattice
     !
     if (celldm (3) <= 0.d0) call errore ('latgen', 'wrong celldm(3)', ibrav)
     !
     cbya=celldm(3)
     a1(1)=celldm(1)
     a2(1)=-celldm(1)/2.d0
     a2(2)=celldm(1)*sr3/2.d0
     a3(3)=celldm(1)*cbya
     !
  else if (ABS(ibrav) == 5) then
     !
     !     trigonal lattice
     !
     if (celldm (4) <= -0.5_dp .or. celldm (4) >= 1.0_dp) &
          call errore ('latgen', 'wrong celldm(4)', ibrav)
     !
     term1=sqrt(1.0_dp + 2.0_dp*celldm(4))
     term2=sqrt(1.0_dp - celldm(4))
     !
     IF ( ibrav == 5) THEN
        !     threefold axis along c (001)
        a2(2)=sr2*celldm(1)*term2/sr3
        a2(3)=celldm(1)*term1/sr3
        a1(1)=celldm(1)*term2/sr2
        a1(2)=-a1(1)/sr3
        a1(3)= a2(3)
        a3(1)=-a1(1)
        a3(2)= a1(2)
        a3(3)= a2(3)
     ELSE IF ( ibrav == -5) THEN
        !     threefold axis along (111)
        a1(1) = celldm(1)*(term1-2.0_dp*term2)/3.0_dp
        a1(2) = celldm(1)*(term1+term2)/3.0_dp
        a1(3) = a1(2)
        a2(1) = a1(3)
        a2(2) = a1(1)
        a2(3) = a1(2)
        a3(1) = a1(2)
        a3(2) = a1(3)
        a3(3) = a1(1)
     END IF
  else if (ibrav == 6) then
     !
     !     tetragonal lattice
     !
     if (celldm (3) <= 0.d0) call errore ('latgen', 'wrong celldm(3)', ibrav)
     !
     cbya=celldm(3)
     a1(1)=celldm(1)
     a2(2)=celldm(1)
     a3(3)=celldm(1)*cbya
     !
  else if (ibrav == 7) then
     !
     !     body centered tetragonal lattice
     !
     if (celldm (3) <= 0.d0) call errore ('latgen', 'wrong celldm(3)', ibrav)
     !
     cbya=celldm(3)
     a2(1)=celldm(1)/2.d0
     a2(2)=a2(1)
     a2(3)=cbya*celldm(1)/2.d0
     a1(1)= a2(1)
     a1(2)=-a2(1)
     a1(3)= a2(3)
     a3(1)=-a2(1)
     a3(2)=-a2(1)
     a3(3)= a2(3)
     !
  else if (ibrav == 8) then
     !
     !     Simple orthorhombic lattice
     !
     if (celldm (2) <= 0.d0) call errore ('latgen', 'wrong celldm(2)', ibrav)
     if (celldm (3) <= 0.d0) call errore ('latgen', 'wrong celldm(3)', ibrav)
     !
     a1(1)=celldm(1)
     a2(2)=celldm(1)*celldm(2)
     a3(3)=celldm(1)*celldm(3)
     !
  else if ( ABS(ibrav) == 9) then
     !
     !     One face (base) centered orthorhombic lattice
     !
     if (celldm (2) <= 0.d0) call errore ('latgen', 'wrong celldm(2)', ibrav)
     if (celldm (3) <= 0.d0) call errore ('latgen', 'wrong celldm(3)', ibrav)
     !
     IF ( ibrav == 9 ) THEN
        !   old PWscf description
        a1(1) = 0.5d0 * celldm(1)
        a1(2) = a1(1) * celldm(2)
        a2(1) = - a1(1)
        a2(2) = a1(2)
     ELSE
        !   alternate description
        a1(1) = 0.5d0 * celldm(1)
        a1(2) =-a1(1) * celldm(2)
        a2(1) = a1(1)
        a2(2) =-a1(2)
     END IF
     a3(3) = celldm(1) * celldm(3)
     !
  else if (ibrav == 10) then
     !
     !     All face centered orthorhombic lattice
     !
     if (celldm (2) <= 0.d0) call errore ('latgen', 'wrong celldm(2)', ibrav)
     if (celldm (3) <= 0.d0) call errore ('latgen', 'wrong celldm(3)', ibrav)
     !
     a2(1) = 0.5d0 * celldm(1)
     a2(2) = a2(1) * celldm(2)
     a1(1) = a2(1)
     a1(3) = a2(1) * celldm(3)
     a3(2) = a2(1) * celldm(2)
     a3(3) = a1(3)
     !
  else if (ibrav == 11) then
     !
     !     Body centered orthorhombic lattice
     !
     if (celldm (2) <= 0.d0) call errore ('latgen', 'wrong celldm(2)', ibrav)
     if (celldm (3) <= 0.d0) call errore ('latgen', 'wrong celldm(3)', ibrav)
     !
     a1(1) = 0.5d0 * celldm(1)
     a1(2) = a1(1) * celldm(2)
     a1(3) = a1(1) * celldm(3)
     a2(1) = - a1(1)
     a2(2) = a1(2)
     a2(3) = a1(3)
     a3(1) = - a1(1)
     a3(2) = - a1(2)
     a3(3) = a1(3)
     !
  else if (ibrav == 12) then
     !
     !     Simple monoclinic lattice, unique (i.e. orthogonal to a) axis: c
     !
     if (celldm (2) <= 0.d0) call errore ('latgen', 'wrong celldm(2)', ibrav)
     if (celldm (3) <= 0.d0) call errore ('latgen', 'wrong celldm(3)', ibrav)
     if (abs(celldm(4))>=1.d0) call errore ('latgen', 'wrong celldm(4)', ibrav)
     !
     sen=sqrt(1.d0-celldm(4)**2)
     a1(1)=celldm(1)
     a2(1)=celldm(1)*celldm(2)*celldm(4)
     a2(2)=celldm(1)*celldm(2)*sen
     a3(3)=celldm(1)*celldm(3)
     !
  else if (ibrav ==-12) then
     !
     !     Simple monoclinic lattice, unique axis: b (more common)
     !
     if (celldm (2) <= 0.d0) call errore ('latgen', 'wrong celldm(2)', ibrav)
     if (celldm (3) <= 0.d0) call errore ('latgen', 'wrong celldm(3)', ibrav)
     if (abs(celldm(5))>=1.d0) call errore ('latgen', 'wrong celldm(5)', ibrav)
     !
     sen=sqrt(1.d0-celldm(5)**2)
     a1(1)=celldm(1)
     a2(2)=celldm(1)*celldm(2)
     a3(1)=celldm(1)*celldm(3)*celldm(5)
     a3(3)=celldm(1)*celldm(3)*sen
     !
  else if (ibrav == 13) then
     !
     !     One face centered monoclinic lattice
     !
     if (celldm (2) <= 0.d0) call errore ('latgen', 'wrong celldm(2)', ibrav)
     if (celldm (3) <= 0.d0) call errore ('latgen', 'wrong celldm(3)', ibrav)
     if (abs(celldm(4))>=1.d0) call errore ('latgen', 'wrong celldm(4)', ibrav)
     !
     sen = sqrt( 1.d0 - celldm(4) ** 2 )
     a1(1) = 0.5d0 * celldm(1) 
     a1(3) =-a1(1) * celldm(3)
     a2(1) = celldm(1) * celldm(2) * celldm(4)
     a2(2) = celldm(1) * celldm(2) * sen
     a3(1) = a1(1)
     a3(3) =-a1(3)
     !
  else if (ibrav == 14) then
     !
     !     Triclinic lattice
     !
     if (celldm (2) <= 0.d0) call errore ('latgen', 'wrong celldm(2)', ibrav)
     if (celldm (3) <= 0.d0) call errore ('latgen', 'wrong celldm(3)', ibrav)
     if (abs(celldm(4))>=1.d0) call errore ('latgen', 'wrong celldm(4)', ibrav)
     if (abs(celldm(5))>=1.d0) call errore ('latgen', 'wrong celldm(5)', ibrav)
     if (abs(celldm(6))>=1.d0) call errore ('latgen', 'wrong celldm(6)', ibrav)
     !
     singam=sqrt(1.d0-celldm(6)**2)
     term= (1.d0+2.d0*celldm(4)*celldm(5)*celldm(6)             &
          -celldm(4)**2-celldm(5)**2-celldm(6)**2)
     if (term < 0.d0) call errore &
        ('latgen', 'celldm do not make sense, check your data', ibrav)
     term= sqrt(term/(1.d0-celldm(6)**2))
     a1(1)=celldm(1)
     a2(1)=celldm(1)*celldm(2)*celldm(6)
     a2(2)=celldm(1)*celldm(2)*singam
     a3(1)=celldm(1)*celldm(3)*celldm(5)
     a3(2)=celldm(1)*celldm(3)*(celldm(4)-celldm(5)*celldm(6))/singam
     a3(3)=celldm(1)*celldm(3)*term
     !
  else
     !
     call errore('latgen',' nonexistent bravais lattice',ibrav)
     !
  end if
  !
  !  calculate unit-cell volume omega
  !
100 omega=0.d0
  s=1.d0
  i=1
  j=2
  k=3
  !
101 do iperm=1,3
     omega=omega+s*a1(i)*a2(j)*a3(k)
     l=i
     i=j
     j=k
     k=l
  end do
!
  i=2
  j=1
  k=3
  s=-s
  if(s < 0.d0) go to 101
  omega=abs(omega)
  return
!
end subroutine latgen
