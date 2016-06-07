!
! Copyright (C) 2004-2006 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!---------------------------------------------------------------------------
MODULE splinelib
  !---------------------------------------------------------------------------
  !
  USE kinds, ONLY : DP
  !
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC :: dosplineint, spline, splint, splint_deriv
  !
  INTERFACE dosplineint
     !
     MODULE PROCEDURE dosplineint_1D, dosplineint_2D
     !
  END INTERFACE
  !
  CONTAINS
    !
    !------------------------------------------------------------------------
    SUBROUTINE spline( xdata, ydata, startu, startd, d2y )
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      !
      REAL(DP), INTENT(IN)  :: xdata(:), ydata(:), startu, startd 
      REAL(DP), INTENT(OUT) :: d2y(:)
      !
      INTEGER               :: i, k, ydim
      REAL(DP)              :: p, sig
      REAL(DP), ALLOCATABLE :: u(:)
      !
      !
      ydim = SIZE( ydata )
      !
      ALLOCATE( u( ydim ) )
      !
      u(1)   = startu
      d2y(1) = startd
      !
      DO  i = 2, ydim - 1
         !
         sig    = ( xdata(i) - xdata(i-1) ) / ( xdata(i+1) - xdata(i-1) ) 
         p      = sig * d2y(i- 1) + 2.0_DP 
         d2y(i) = ( sig - 1.0_DP ) / p 
         u(i)   = ( 6.0_DP * ( ( ydata(i+1) - ydata(i) ) / &
                    ( xdata(i+1) - xdata(i) ) - ( ydata(i) - ydata(i-1) ) / &
                    ( xdata(i) - xdata(i-1) ) ) / &
                    ( xdata(i+1) - xdata(i-1) ) - sig * u(i-1) ) / p 
         !       
      END DO
      !
      d2y(ydim) = 0  
      !
      DO  k = ydim - 1, 1, -1 
         !
         d2y(k) = d2y(k) * d2y(k+1) + u(k) 
         !
      END DO
      !
      DEALLOCATE( u )
      !
    END SUBROUTINE spline
    !
    !------------------------------------------------------------------------
    FUNCTION splint( xdata, ydata, d2y, x )
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      !
      REAL(DP), INTENT(IN) :: xdata(:), ydata(:), d2y(:)
      REAL(DP), INTENT(IN) :: x
      !
      REAL(DP) :: splint
      INTEGER  :: khi, klo, xdim
      REAL(DP) :: a, b, h
      !
      !
      xdim = SIZE( xdata )
      !
      klo = 1
      khi = xdim
      !
      klo = MAX( MIN( locate( xdata, x ), ( xdim - 1 ) ), 1 )
      !
      khi = klo + 1
      !
      h = xdata(khi) - xdata(klo)
      !
      a = ( xdata(khi) - x ) / h
      b = ( x - xdata(klo) ) / h
      !
      splint = a * ydata(klo) + b * ydata(khi) + &
               ( ( a**3 - a ) * d2y(klo) + ( b**3 - b ) * d2y(khi) ) * &
               ( h**2 ) / 6.0_DP

      END FUNCTION splint


    !------------------------------------------------------------------------
    FUNCTION splint_deriv( xdata, ydata, d2y, x )
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      !
      REAL(DP), INTENT(IN) :: xdata(:), ydata(:), d2y(:)
      REAL(DP), INTENT(IN) :: x
      !
      REAL(DP) :: splint_deriv
      INTEGER  :: khi, klo, xdim
      REAL(DP) :: a, b, da, db, h
      !
      !
      xdim = SIZE( xdata )
      !
      klo = 1
      khi = xdim
      !
      klo = MAX( MIN( locate( xdata, x ), ( xdim - 1 ) ), 1 )
      !
      khi = klo + 1
      !
      h = xdata(khi) - xdata(klo)
      !
      a = ( xdata(khi) - x ) / h
      b = ( x - xdata(klo) ) / h
      da = -1.0_DP / h
      db = 1.0_DP / h
      !
      splint_deriv = da * ydata(klo) + db * ydata(khi) + &
               ( ( 3.0_DP*a**2 - 1.0_DP ) * da * d2y(klo) + &
                 ( 3.0_DP*b**2 - 1.0_DP ) * db * d2y(khi) ) * &
               ( h**2 ) / 6.0_DP

      END FUNCTION splint_deriv

         !-------------------------------------------------------------------
         FUNCTION locate( xx, x )
           !-------------------------------------------------------------------
           !
           IMPLICIT NONE
           !
           REAL(DP), INTENT(IN) :: xx(:)
           REAL(DP), INTENT(IN) :: x
           !
           INTEGER :: locate
           INTEGER :: n, jl, jm, ju
           LOGICAL :: ascnd
           !
           !
           n     = SIZE( xx )
           ascnd = ( xx(n) >= xx(1) )
           jl    = 0
           ju    = n + 1
           !
           main_loop: DO
              !
              IF ( ( ju - jl ) <= 1 ) EXIT main_loop
              ! 
              jm = ( ju + jl ) / 2
              !
              IF ( ascnd .EQV. ( x >= xx(jm) ) ) THEN
                 !
                 jl = jm
                 !
              ELSE
                 !
                 ju = jm
                 !
              END IF
              !
           END DO main_loop
           !
           IF ( x == xx(1) ) THEN
              !
              locate = 1
              !
           ELSE IF ( x == xx(n) ) THEN
              !
              locate = n - 1
              !
           ELSE 
              !
              locate = jl
              !
           END IF
           !
         END FUNCTION locate      
         !
    !
    !------------------------------------------------------------------------
    SUBROUTINE dosplineint_1D( old_mesh, old_vec, new_mesh, new_vec )
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      !
      REAL (DP), INTENT(IN)  :: old_mesh(:), new_mesh(:)
      REAL (DP), INTENT(IN)  :: old_vec(:)
      REAL (DP), INTENT(OUT) :: new_vec(:)
      !
      REAL (DP), ALLOCATABLE :: d2y(:)
      INTEGER                :: i
      INTEGER                :: old_dim, new_dim
      !
      !
      old_dim = SIZE( old_vec )
      new_dim = SIZE( new_vec )
      !
      IF ( old_dim /= SIZE( old_mesh ) ) &
         CALL errore( 'dosplineint', &
                      'dimensions of old_mesh and old_vec do not match', 1 )         
      !
      IF ( new_dim /= SIZE( new_mesh ) ) &
         CALL errore( 'dosplineint', &
                      'dimensions of new_mesh and new_vec do not match', 1 ) 
      !
      ALLOCATE( d2y( old_dim ) )
      !
      d2y = 0
      !
      CALL spline( old_mesh , old_vec(:), 0.0_DP, 0.0_DP, d2y  ) 
      !
      DO i = 1, new_dim
         !
         new_vec(i) = splint( old_mesh, old_vec(:), d2y, new_mesh(i) )
         !
      END DO
      !
      DEALLOCATE( d2y )
      !
    END SUBROUTINE dosplineint_1D
    !    
    !------------------------------------------------------------------------
    SUBROUTINE dosplineint_2D( old_mesh, old_vec, new_mesh, new_vec )
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      !
      REAL (DP), INTENT(IN)  :: old_mesh(:), new_mesh(:)
      REAL (DP), INTENT(IN)  :: old_vec(:,:)
      REAL (DP), INTENT(OUT) :: new_vec(:,:)
      !
      REAL (DP), ALLOCATABLE :: d2y(:)
      INTEGER                :: dim, i, j
      INTEGER                :: old_dim, new_dim
      !
      !
      dim = SIZE( old_vec, 1 )
      !
      IF( dim /= SIZE( new_vec, 1 ) ) &
         CALL errore( 'dosplineint', &
                      'dimensions of old_vec and new_vec do not match', 1 )
      !
      old_dim = SIZE( old_vec, 2 )
      new_dim = SIZE( new_vec, 2 )
      !
      IF ( old_dim /= SIZE( old_mesh, 1 ) ) &
         CALL errore( 'dosplineint', &
                      'dimensions of old_mesh and old_vec do not match', 1 )         
      !
      IF ( new_dim /= SIZE( new_mesh, 1 ) ) &
         CALL errore( 'dosplineint', &
                      'dimensions of new_mesh and new_vec do not match', 1 ) 
      !
      ALLOCATE( d2y( old_dim ) )
      !
      DO i = 1, dim
         ! 
         d2y = 0
         !
         CALL spline( old_mesh , old_vec(i,:), 0.0_DP, 0.0_DP, d2y  ) 
         !
         DO j = 1, new_dim
            !
            new_vec(i,j) = splint( old_mesh, old_vec(i,:), d2y, new_mesh(j) )
            !
         END DO
         !
      END DO
      !
      DEALLOCATE( d2y )
      !
    END SUBROUTINE dosplineint_2D
    !
END MODULE splinelib
