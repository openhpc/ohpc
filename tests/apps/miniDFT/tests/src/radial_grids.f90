!
! Copyright (C) 2004 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
MODULE radial_grids
  !============================================================================
  !
  !   Module containing type definitions and auxiliary routines to deal with
  !   data on logarithmic radial grids.
  !   Should contain low level routines and no reference to other modules
  !   (with the possible exception of kinds and parameters) so as to be 
  !   call-able from any other module.
  !
  ! content:
  !
  ! - ndmx     : parameter definition max grid dimension
  !
  ! - radial_grid_type : derived type definition for radial grids
  !
  ! - do_mesh  : a routine to build the radial mesh
  !
  ! - check_mesh  : a routine to check if grid is consistently set 
  !
  ! - hartree  : a routine that solve the Poisson's equation on radial grid
  !
  ! - series   : a simple routine returning the coefficient of the polynomial 
  !              describing the leading behavior of a function f at small r.
  !
  ! - write_grid_on_file, read_grid_from_file : I/O routines 
  !
  !============================================================================
  !
  USE kinds, ONLY: dp
  !
  IMPLICIT NONE
  !
  integer, parameter :: &
       ndmx=3500     ! the maximum mesh size 

TYPE radial_grid_type

  INTEGER :: &
       mesh          ! the actual number of mesh points
  REAL(DP),POINTER :: &
       r(:),    & ! the radial mesh
       r2(:),   & ! the square of the radial mesh
       rab(:),  & ! d r(x) / d x where x is the linear grid
       sqr(:),  & ! the square root of the radial mesh
       rm1(:),  & ! 1 / r
       rm2(:),  & ! 1 / r**2
       rm3(:)     ! 1 / r**3
  REAL(DP) :: &
       xmin,       & ! the minimum x
       rmax,       & ! the maximum radial point
       zmesh,      & ! the ionic charge used for the mesh
       dx            ! the deltax of the linear mesh
END TYPE radial_grid_type

  PRIVATE
  PUBLIC :: ndmx, radial_grid_type, &
            do_mesh, check_mesh, hartree, series, &
            write_grid_on_file, read_grid_from_file, &
            allocate_radial_grid,&
            deallocate_radial_grid,&
            nullify_radial_grid,&
            radial_grid_COPY

      interface deallocate_radial_grid
         module procedure &
            deallocate_radial_grid_s,& ! only one
            deallocate_radial_grid_v   ! an array
      end interface

  !============================================================================
  !
 CONTAINS
!
!   Build the radial (logarithmic) grid 
!
!   r(i) = exp ( xmin + (i-1) dx ) / zmesh  i=1,mesh
!   r2(i) is r(i) square, sqr(i) is sqrt(r(i)) and 
!   rab(i) is the integration element = r(i)*dx
!  
!   more general grid definitions are possible but currently not implemented
!   (example: Vanderbilt's grid, same as above but starting at r=0)
!   r(i) = exp ( xmin ) * ( exp( (i-1)*dx ) - 1.0_dp ) / zmesh
!   rab(i) = ( r(i) + exp(xmin)/zmesh ) * dx
!
!---------------------------------------------------------------
      subroutine radial_grid_COPY(X,Y)
!---------------------------------------------------------------
      type(radial_grid_type),intent(in)    :: X
      type(radial_grid_type),intent(inout) :: Y
      !
      call deallocate_radial_grid(Y)
      call allocate_radial_grid(Y, X%mesh)
      !
         Y%r(1:X%mesh)   = X%r(1:X%mesh)
         Y%r2(1:X%mesh)  = X%r2(1:X%mesh)
         Y%rab(1:X%mesh) = X%rab(1:X%mesh)
         Y%sqr(1:X%mesh) = X%sqr(1:X%mesh)
         Y%rm1(1:X%mesh) = X%rm1(1:X%mesh)
         Y%rm2(1:X%mesh) = X%rm2(1:X%mesh)
         Y%rm3(1:X%mesh) = X%rm3(1:X%mesh)
         !
         Y%xmin  = X%xmin
         Y%rmax  = X%rmax
         Y%zmesh = X%zmesh
         Y%dx    = X%dx
      end subroutine radial_grid_COPY
!
!---------------------------------------------------------------
      subroutine allocate_radial_grid(grid,mesh)
!---------------------------------------------------------------
      type(radial_grid_type),intent(inout) :: grid
      integer,intent(in) :: mesh
      if(mesh>ndmx) &
         call errore('allocate_radial_grid', 'mesh>ndmx',1)
      allocate(           &
         grid%r(mesh),    &
         grid%r2(mesh),   & ! the square of the radial mesh
         grid%rab(mesh),  & ! d r(x) / d x where x is the linear grid
         grid%sqr(mesh),  & ! the square root of the radial mesh
         grid%rm1(mesh),  & ! 1 / r
         grid%rm2(mesh),  & ! 1 / r**2
         grid%rm3(mesh)   ) ! 1 / r**3
      grid%mesh = mesh
      end subroutine allocate_radial_grid
!
!---------------------------------------------------------------
      subroutine deallocate_radial_grid_s(grid)
!---------------------------------------------------------------
      type(radial_grid_type),intent(inout) :: grid
       if (associated(grid%r))   deallocate(grid%r)
       if (associated(grid%r2))  deallocate(grid%r2)
       if (associated(grid%rab)) deallocate(grid%rab)
       if (associated(grid%sqr)) deallocate(grid%sqr)
       if (associated(grid%rm1)) deallocate(grid%rm1)
       if (associated(grid%rm2)) deallocate(grid%rm2)
       if (associated(grid%rm3)) deallocate(grid%rm3)
       grid%mesh = 0
       call nullify_radial_grid(grid)
      end subroutine deallocate_radial_grid_s
!---------------------------------------------------------------
      subroutine deallocate_radial_grid_v(grid)
!---------------------------------------------------------------
      type(radial_grid_type),intent(inout) :: grid(:)
      integer :: n
       do n = 1,size(grid)
         if (associated(grid(n)%r))   deallocate(grid(n)%r)
         if (associated(grid(n)%r2))  deallocate(grid(n)%r2)
         if (associated(grid(n)%rab)) deallocate(grid(n)%rab)
         if (associated(grid(n)%sqr)) deallocate(grid(n)%sqr)
         if (associated(grid(n)%rm1)) deallocate(grid(n)%rm1)
         if (associated(grid(n)%rm2)) deallocate(grid(n)%rm2)
         if (associated(grid(n)%rm3)) deallocate(grid(n)%rm3)
         grid(n)%mesh = 0
       enddo
       !deallocate(grid)
      end subroutine deallocate_radial_grid_v

!---------------------------------------------------------------
      subroutine nullify_radial_grid(grid)
!---------------------------------------------------------------
      type(radial_grid_type),intent(inout) :: grid
      nullify(           &
         grid%r,    &
         grid%r2,   & ! the square of the radial mesh
         grid%rab,  & ! d r(x) / d x where x is the linear grid
         grid%sqr,  & ! the square root of the radial mesh
         grid%rm1,  & ! 1 / r
         grid%rm2,  & ! 1 / r**2
         grid%rm3   ) ! 1 / r**3
      grid%mesh = -1
      end subroutine nullify_radial_grid
!
!---------------------------------------------------------------
      subroutine do_mesh(rmax,zmesh,xmin,dx,ibound,grid)
!---------------------------------------------------------------
!
      use kinds, only : DP
      implicit none
      type(radial_grid_type),intent(out) :: grid

      integer, intent(in)   :: ibound
      real(DP),intent(in)   :: rmax, zmesh, dx
      real(DP),intent(inout):: xmin

      real(DP) :: xmax, x
      integer  :: mesh, i
      !
      xmax=log(rmax*zmesh)
      mesh=(xmax-xmin)/dx+1
      !
      !  mesh must be odd for simpson integration.
      !
      mesh=2*(mesh/2)+1
      if(mesh+1 > ndmx) call errore('do_mesh','ndmx is too small',1)
      if(ibound == 1) xmin=xmax-dx*(mesh-1)
      !
      call deallocate_radial_grid(grid)
      call allocate_radial_grid(grid,mesh)
      !
      do i=1,mesh
         x=xmin+DBLE(i-1)*dx
         grid%r(i)   = exp(x)/zmesh
         grid%r2(i)  = grid%r(i)*grid%r(i)
         grid%rab(i) = grid%r(i)*dx
         grid%sqr(i) = sqrt(grid%r(i))
         grid%rm1(i) = 1._dp/grid%r(i)
         grid%rm2(i) = 1._dp/grid%r(i)**2
         grid%rm3(i) = 1._dp/grid%r(i)**3
      end do
      !
      grid%mesh = mesh
      grid%dx   = dx
      grid%xmin = xmin
      grid%rmax = rmax
      grid%zmesh = zmesh

      return
      end subroutine do_mesh
!
!  check that logarithmic grid is consistently set
!---------------------------------------------------------------
   subroutine check_mesh(grid)
!---------------------------------------------------------------
!
   use kinds, only : DP
   use constants, only : eps8
   implicit none
   type(radial_grid_type),intent(in) :: grid
   integer :: i

   if (grid%mesh < 0 ) call errore('check_mesh','grid%mesh < 0 ',1)
   do i=1,grid%mesh
      if (abs(grid%r2(i)/grid%r(i)**2-1.d0) > eps8 ) &
         call errore('check_mesh',' r2(i) is different ',i)
      if (abs(grid%sqr(i)/sqrt(grid%r(i))-1.d0) > eps8 ) &
         call errore('check_mesh',' sqr(i) is different ',i)
      if (abs(grid%rab(i)/(grid%r(i)*grid%dx)-1.d0) > eps8 ) &
         call errore('check_mesh',' rab(i) is different ',i)
   end do

   return
   end subroutine check_mesh
!
! Solution of the Poisson's equation on a radial (logarithmic) grid
!---------------------------------------------------------------
subroutine hartree(k,nst,mesh,grid,f,vh)
  !---------------------------------------------------------------
  !
  use kinds, only : DP
!  use radial_grids, only: radial_grid_type
  implicit none
  integer,intent(in)::       & 
       k,   & ! input: the k of the equation
       nst, & ! input: at low r, f goes as r**nst
       mesh   ! input: the dimension of the mesh

  type(radial_grid_type), intent(in) :: &
       grid   ! input: the radial grid
  real(DP), intent(in)::        &
       f(mesh)  ! input: the 4\pi r2 \rho function
  real(DP), intent(out)::       &
       vh(mesh) ! output: the required solution
  !
  ! local variables
  !
  integer ::        &
       k21,  &   ! 2k+1
       nk1,  &   ! nst-k-1
       ierr, &   ! integer variable for allocation control
       i         ! counter

  real(DP)::        &
       c0,c2,c3, & ! coefficients of the polynomial expansion close to r=0
       ch,       & ! dx squared / 12.0
       xkh2,     & ! ch * f
       ei, di,   & ! auxiliary variables for the diagonal and 
                   ! off diagonal elements of the matrix
       f1, fn,   & ! variables used for the boundary condition
       vhim1, vhi  ! variables for the right hand side

  real(DP), allocatable:: &
       d(:), &       ! the diagonal elements of 
                     ! the tridiagonal sys.
       e(:)          ! the off diagonal elements 
                     ! of the trid. sys.
  !
  ! Allocate space for the diagonal and off diagonal elements
  !
  if (mesh.ne.grid%mesh) call errore('hartree',' grid dimension mismatch',1) 
  allocate(d(mesh),stat=ierr)
  allocate(e(mesh),stat=ierr)

  if (ierr.ne.0) call errore('hartree',' error allocating d or e',1)
  !
  ! Find the series expansion of the solution close to r=0
  !
  k21=2*k+1
  nk1=nst-k-1
  if(nk1.le.0) then
     write(6,100) k,nst
100  format(5x,'stop in "hartree": k=',i3,'  nst=',i3)
     stop
  !else if(nk1.ge.4) then
  ! not sure whether the following is really correct, but the above wasn't
  else if(nk1.ge.3) then
     c2=0.0_dp
     c3=0.0_dp
  else
     e(1)=0.0_dp
     do i=1,4
        d(i)=-k21*f(i)/grid%r(i)**nst
     end do
     call series(d,grid%r,grid%r2,e(nk1))
     c2=e(1)/(4.0_dp*k+6.0_dp)
     c3=e(2)/(6.0_dp*k+12.0_dp)
  end if
  !
  ! Set the main auxiliary parameters
  !
  ch=grid%dx*grid%dx/12.0_dp
  xkh2=ch*(DBLE(k)+0.5_dp)**2
  ei=1.0_dp-xkh2
  di=-(2.0_dp+10.0_dp*xkh2)
  !
  ! Set the diagonal and the off diagonal elements of the 
  ! linear system, compute a part of the right hand side 
  !
  do i=2,mesh
     d(i)=-di
     e(i)=-ei
     vh(i)=k21*ch*grid%sqr(i)*f(i)
  end do
  !
  ! Use the boundary condition to eliminate the value of the 
  ! solution in the first point from the first equation. This 
  ! part for the diagonal element
  !
  f1=(grid%sqr(1)/grid%sqr(2))**k21
  d(2)=d(2)-ei*f1
  !
  ! Use the boundary condition to eliminate the value of the 
  ! solution in the last point from the last equation
  !
  fn=(grid%sqr(mesh-1)/grid%sqr(mesh))**k21
  d(mesh-1)=d(mesh-1)-ei*fn
  !
  ! In the first point vh(1) has the same definition as in the other points
  !
  vhim1=k21*ch*grid%sqr(1)*f(1)
  !
  ! Compute the right hand side using the auxiliary quantity vh(i).
  !
  do i=2,mesh-1
     vhi=vh(i)
     vh(i)=vhim1+10.0_dp*vhi+vh(i+1)
     vhim1=vhi
  end do
  !
  ! Use the boundary condition to eliminate the value of the solution in the 
  ! first point from the first equation. This part for the right hand side.
  !
  vh(2)=vh(2)-ei*grid%sqr(1)**k21*(c2*(grid%r2(2)-grid%r2(1)) &
       +c3*(grid%r(2)**3-grid%r(1)**3))
  !
  ! solve the linear system with lapack routine dptsv
  !
  call dptsv(mesh-2,1,d(2),e(2),vh(2),mesh-2,ierr)
  if (ierr.ne.0) call errore('hartree', 'error in lapack', ierr)
  !
  ! Set the value of the solution at the first and last point
  ! First, find c0 from the solution in the second point
  !
  c0=vh(2)/grid%sqr(2)**k21-c2*grid%r2(2)-c3*grid%r(2)*grid%r2(2)
  !
  ! and then use the series expansion at the first point
  !
  vh(1)=grid%sqr(1)**k21*(c0+c2*grid%r2(1)+c3*grid%r(1)**3)
  !
  ! the solution at the last point is given  by the boundary 
  ! condition
  !
  vh(mesh)=vh(mesh-1)*fn
  !
  ! The solution must be divided by r (from the equation) 
  ! and multiplied by the square root of r (from the log 
  ! mesh transformation)
  !
  do i=1,mesh
     vh(i)= vh(i) / grid%sqr(i)
  end do

  deallocate(e)
  deallocate(d)

  return
end subroutine hartree
!
! simple routine returning the coefficient of the polynomial 
! describing the leading behavior of a function f at small r.
!---------------------------------------------------------------
subroutine series(f,r,r2,b)
  !---------------------------------------------------------------
  !
  use kinds, only : DP
  implicit none
  real(DP):: dr21,dr31,dr32,dr41,dr42,dr43,df21,df32,df43, &
       ddf42,ddf31 
  real(DP):: f(4),r(4),r2(4),b(0:3)
  dr21=r(2)-r(1)
  dr31=r(3)-r(1)
  dr32=r(3)-r(2)
  dr41=r(4)-r(1)
  dr42=r(4)-r(2)
  dr43=r(4)-r(3)
  df21=(f(2)-f(1))/dr21
  df32=(f(3)-f(2))/dr32
  df43=(f(4)-f(3))/dr43
  ddf42=(df43-df32)/dr42
  ddf31=(df32-df21)/dr31
  b(3)=(ddf42-ddf31)/dr41
  b(2)=ddf31-b(3)*(r(1)+r(2)+r(3))
  b(1)=df21-b(2)*(r(2)+r(1))-b(3)*(r2(1)+r2(2)+r(1)*r(2))
  b(0)=f(1)-r(1)*(b(1)+r(1)*(b(2)+r(1)*b(3)))
  return
end subroutine series
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
! I/O routines 
!
!----------------------------------------------------------------------
subroutine write_grid_on_file(iunit,grid)
!  use radial_grids, only: radial_grid_type
  implicit none
  type(radial_grid_type), intent(in) :: grid
  integer, intent(in) :: iunit
  integer :: n
!
  WRITE(iunit,'(i8)') grid%mesh
  WRITE(iunit,'(e20.10)') grid%dx
  WRITE(iunit,'(e20.10)') grid%xmin
  WRITE(iunit,'(e20.10)') grid%zmesh
  WRITE(iunit,'(e20.10)') (grid%r(n), n=1,grid%mesh)
  WRITE(iunit,'(e20.10)') (grid%r2(n), n=1,grid%mesh)
  WRITE(iunit,'(e20.10)') (grid%sqr(n), n=1,grid%mesh)
!  WRITE(iunit,'(e20.10)') (grid%rab(n), n=1,grid%mesh)
  return
end subroutine write_grid_on_file

subroutine read_grid_from_file(iunit,grid)
!  use radial_grids, only: radial_grid_type
  implicit none
  type(radial_grid_type), intent(out) :: grid
  integer, intent(in) :: iunit
  integer :: n
!
  READ(iunit,'(i8)')     grid%mesh
  READ(iunit,'(e20.10)') grid%dx
  READ(iunit,'(e20.10)') grid%xmin
  READ(iunit,'(e20.10)') grid%zmesh
  READ(iunit,'(e20.10)') (grid%r(n), n=1,grid%mesh)
  READ(iunit,'(e20.10)') (grid%r2(n), n=1,grid%mesh)
  READ(iunit,'(e20.10)') (grid%sqr(n), n=1,grid%mesh)
!  READ(iunit,'(e20.10)') (grid%rab(n), n=1,grid%mesh)
  grid%rab(1:grid%mesh) = grid%r(1:grid%mesh) * grid%dx
  grid%rm1(1:grid%mesh) = 1._dp/grid%r(1:grid%mesh)
  grid%rm2(1:grid%mesh) = 1._dp/grid%r2(1:grid%mesh)
  grid%rm3(1:grid%mesh) = 1._dp/grid%r(1:grid%mesh)**3

  return
end subroutine read_grid_from_file
 
!----------------------------------------------------------------------
END MODULE radial_grids
