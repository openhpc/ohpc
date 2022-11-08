!>
!> @file
!>
!>  PLASMA is a software package provided by:
!>  University of Tennessee, US,
!>  University of Manchester, UK.
!>
!> @precisions normal z -> s d c
!>
!>    @brief Tests PLASMA matrix inversion based on Cholesky
!!           factorization

      program test_zpotri

      use, intrinsic :: iso_fortran_env
      use iso_c_binding
      use omp_lib
      use plasma

      implicit none

      integer, parameter :: sp = c_float
      integer, parameter :: dp = c_double

      ! set working precision, this value is rewritten for different precisions
      integer, parameter :: wp = dp

      integer,     parameter   :: n = 2000
      complex(wp), parameter   :: zmone = -1.0
      complex(wp), allocatable :: A(:,:), Aref(:,:)
      real(wp),    allocatable :: work(:)
      integer                  :: seed(4) = [0, 0, 0, 1]
      real(wp)                 :: Anorm, error, tol
      character                :: uploLapack ='L'
      integer                  :: uploPlasma = PlasmaLower
      !character(len=32)        :: frmt ="(10(3X,F7.3,SP,F7.3,'i'))"
      integer                  :: lda, infoPlasma, infoLapack, i
      logical                  :: success = .false.

      ! Performance variables
      real(dp) :: tstart, tstop, telapsed

      ! External functions
      real(wp), external :: dlamch, zlanhe, zlange

      tol = 50.0 * dlamch('E')
      print *, "tol:", tol

      lda = max(1,n)

      ! Allocate matrices A, Re, Im
      allocate(A(lda,n), stat=infoPlasma)

      ! Generate random Hermitian positive definite matrix A
      call zlarnv(1, seed, lda*n, A)
      A = A * conjg(transpose(A))
      do i = 1, n
        A(i,i) = A(i,i) + n
      end do

      !print *, "Random Hermitian positive definite matrix A:"
      !do i = 1, n
      !  print frmt, A(i,:)
      !end do
      !print *, ""

      ! Factorize matrix A using Cholesky
      call zpotrf(uploLapack, n, A, lda, infoLapack)
      print *, "zpotrf:", infoLapack

      !print *, "Factor " // uploLapack // " of Chol(A):"
      !do i = 1, n
      !  print frmt, A(i,:)
      !end do
      !print *, ""

      allocate(Aref(lda,n), stat=infoPlasma)
      Aref = A

      !===================
      ! Initialize PLASMA.
      !===================
      call plasma_init(infoPlasma)

      !===========================================
      ! Invert matrix A using L or U from Chol(A).
      !===========================================
      tstart = omp_get_wtime()
      call plasma_zpotri(uploPlasma, n, A, lda, infoPlasma)
      tstop  = omp_get_wtime()
      telapsed = tstop-tstart

      !=================
      ! Finalise PLASMA.
      !=================
      call plasma_finalize(infoPlasma)

      !print *, "A^{-1}:"
      !do i = 1, n
      !  print frmt, A(i,:)
      !end do
      !print *, ""

      print *, "Time:", telapsed
      print *, ""

      ! Check Cholesky-based matrix inverse is correct

      ! Invert matrix Aref using L or U from Chol(Aref)
      call zpotri(uploLapack, n, Aref, lda, infoLapack)
      print *, "zpotri:", infoLapack

      if (infoLapack == 0) then

        !print *, "Aref^{-1}:"
        !do i = 1, n
        !  print frmt, Aref(i,:)
        !end do
        !print *, ""

        ! Calculate difference A := -1*Aref+A, A := A-Aref
        ! A = A-Aref
        call zaxpy(lda*n, zmone, Aref, 1, A, 1)

        !print *, "A := A-Aref"
        !do i = 1, n
        !  print frmt, Aref(i,:)
        !end do
        !print *, ""

        ! Calculate norms |Aref^{-1}|_F, |A^{-1} - Aref^{-1}|_F,
        Anorm = zlanhe('F', uploLapack, n, Aref, lda, work)
        print *, "|Aref^{-1}|_F:", Anorm

        error = zlange('F', n, n, A, lda, work)
        print *, "|A^{-1} - Aref^{-1}|_F:", error

        ! Calculate error |A^{-1} - Aref^{-1}|_F / |Aref^{-1}|_F
        error = error/Anorm

        if (error < tol)  success = .true.

      else

        if (infoPlasma == infoLapack) then
          error   = 0.0
          success =.true.
        else
          error = -1.0
          success =.false.
        end if

      end if

      print *, "|A^{-1} - Aref^{-1}|_F / |Aref^{-1}|_F:", error
      print *, "success:                               ", success

      if (success) then
         write(*,'(a)') "  The result is CORRECT."
      else
         write(*,'(a)') "  The result is WRONG!"
         stop 1
      end if
      write(*,'(a)') ""

      ! Deallocate matrix Aref
      deallocate(Aref, stat=infoPlasma)

      ! Deallocate matrix A
      deallocate(A, stat=infoPlasma)

      end program test_zpotri
