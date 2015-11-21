!
! Copyright (C) 2001-2008 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
MODULE random_numbers
  !----------------------------------------------------------------------------
  !
  USE kinds, ONLY : DP
  !
  IMPLICIT NONE
  !
  INTERFACE gauss_dist
     !
     MODULE PROCEDURE gauss_dist_scal, gauss_dist_vect
     !
  END INTERFACE
  !
  CONTAINS
    !
    !------------------------------------------------------------------------
    FUNCTION randy ( irand )
      !------------------------------------------------------------------------
      !
      ! x=rand(n) : reseed with initial seed idum=n
      !             if randy is not explicitly initialized, it will be
      !             initialized with seed idum=0 the first time it is called
      ! x=rand( ) : generate uniform real(DP) numbers x in [0,1]
      !
      REAL(DP) :: randy
      INTEGER, optional    :: irand
      !
      INTEGER , PARAMETER  :: m    = 714025, &
                              ia   = 1366, &
                              ic   = 150889, &
                              ntab = 97
      REAL(DP), PARAMETER  :: rm = 1.0_DP / m
      INTEGER              :: j
      INTEGER, SAVE        :: ir(ntab), iy, idum=0
      LOGICAL, SAVE        :: first=.true.
      !
      IF ( present(irand) ) THEN
         idum = irand
         first=.true.
      END IF

      IF ( first ) THEN
         !
         first = .false.
         idum = MOD( ic - idum, m )
         !
         DO j=1,ntab
            idum=mod(ia*idum+ic,m)
            ir(j)=idum
         END DO
         idum=mod(ia*idum+ic,m)
         iy=idum
      END IF
      j=1+(ntab*iy)/m
      IF( j > ntab .OR. j <  1 ) call errore('randy','j out of range',j)
      iy=ir(j)
      randy=iy*rm
      idum=mod(ia*idum+ic,m)
      ir(j)=idum
      !
      RETURN
      !
    END FUNCTION randy
    !
    !------------------------------------------------------------------------
    SUBROUTINE set_random_seed ( )
      !------------------------------------------------------------------------
      !
      ! poor-man random seed for randy
      !
      INTEGER, DIMENSION (8) :: itime
      INTEGER :: iseed, irand
      !
      CALL date_and_time ( values = itime ) 
      ! itime contains: year, month, day, time difference in minutes, hours,
      !                 minutes, seconds and milliseconds. 
      iseed = ( itime(8) + itime(6) ) * ( itime(7) + itime(4) )
      irand = randy ( iseed )
      !
    END SUBROUTINE set_random_seed
    !
    !-----------------------------------------------------------------------
    FUNCTION gauss_dist_scal( mu, sigma )
      !-----------------------------------------------------------------------
      !
      ! ... this function generates a number taken from a normal
      ! ... distribution of mean value \mu and variance \sigma
      !
      IMPLICIT NONE
      !
      REAL(DP), INTENT(IN) :: mu
      REAL(DP), INTENT(IN) :: sigma
      REAL(DP)             :: gauss_dist_scal
      !
      REAL(DP) :: x1, x2, w
      !
      !
      gaussian_loop: DO
         !
         x1 = 2.0_DP * randy() - 1.0_DP
         x2 = 2.0_DP * randy() - 1.0_DP
         !
         w = x1 * x1 + x2 * x2
         !
         IF ( w < 1.0_DP ) EXIT gaussian_loop
         !
      END DO gaussian_loop
      !
      w = SQRT( ( - 2.0_DP * LOG( w ) ) / w )
      !
      gauss_dist_scal = x1 * w * sigma + mu
      !
      RETURN
      !
    END FUNCTION gauss_dist_scal
    !    
    !-----------------------------------------------------------------------
    FUNCTION gauss_dist_vect( mu, sigma, dim )
      !-----------------------------------------------------------------------
      !
      ! ... this function generates an array of numbers taken from a normal
      ! ... distribution of mean value \mu and variance \sigma
      !
      IMPLICIT NONE
      !
      REAL(DP), INTENT(IN) :: mu
      REAL(DP), INTENT(IN) :: sigma
      INTEGER,  INTENT(IN) :: dim
      REAL(DP)             :: gauss_dist_vect( dim )
      !
      REAL(DP) :: x1, x2, w
      INTEGER  :: i
      !
      !
      DO i = 1, dim, 2
         !
         gaussian_loop: DO
            !
            x1 = 2.0_DP * randy() - 1.0_DP
            x2 = 2.0_DP * randy() - 1.0_DP
            !
            w = x1 * x1 + x2 * x2
            !
            IF ( w < 1.0_DP ) EXIT gaussian_loop
            !
         END DO gaussian_loop
         !
         w = SQRT( ( - 2.0_DP * LOG( w ) ) / w )
         !
         gauss_dist_vect(i) = x1 * w * sigma
         !
         IF ( i >= dim ) EXIT
         !
         gauss_dist_vect(i+1) = x2 * w * sigma
         !
      END DO
      !
      gauss_dist_vect(:) = gauss_dist_vect(:) + mu
      !
      RETURN
      !
    END FUNCTION gauss_dist_vect
    !
END MODULE random_numbers
