!
!  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  SLEPc - Scalable Library for Eigenvalue Problem Computations
!  Copyright (c) 2002-2021, Universitat Politecnica de Valencia, Spain
!
!  This file is part of SLEPc.
!  SLEPc is distributed under a 2-clause BSD license (see LICENSE).
!  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!  Program usage: mpiexec -n <np> ./test4f [-help] [-n <n>] [-m <m>] [all SLEPc options]
!
!  Description: Singular value decomposition of a bidiagonal matrix.
!
!               |  1  2                     |
!               |     1  2                  |
!               |        1  2               |
!           A = |          .  .             |
!               |             .  .          |
!               |                1  2       |
!               |                   1  2    |
!
!  The command line options are:
!    -m <m>, where <m> = matrix rows.
!    -n <n>, where <n> = matrix columns (defaults to m+2).
!
! ----------------------------------------------------------------------
!
      program main
#include <slepc/finclude/slepcsvd.h>
      use slepcsvd
      implicit none

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Declarations
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      Mat                A, B
      SVD                svd
      SVDConv            conv;
      SVDStop            stp;
      SVDWhich           which;
      SVDConvergedReason reason;
      PetscInt           m, n, i, Istart
      PetscInt           col(2), its, Iend
      PetscScalar        val(2)
      SVDProblemType     ptype
      PetscMPIInt        rank
      PetscErrorCode     ierr
      PetscBool          flg, tmode
      PetscViewerAndFormat vf

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Beginning of program
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      call SlepcInitialize(PETSC_NULL_CHARACTER,ierr)
      call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)
      m = 20
      call PetscOptionsGetInt(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,  &
     &                        '-m',m,flg,ierr)
      call PetscOptionsGetInt(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,  &
     &                        '-n',n,flg,ierr)
      if (.not. flg) n = m+2

      if (rank .eq. 0) then
        write(*,100) m, n
      endif
 100  format (/'Bidiagonal matrix, m =',I3,', n=',I3,' (Fortran)')

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Build the Lauchli matrix
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      call MatCreate(PETSC_COMM_WORLD,A,ierr)
      call MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,m,n,ierr)
      call MatSetFromOptions(A,ierr)
      call MatSetUp(A,ierr)

      call MatGetOwnershipRange(A,Istart,Iend,ierr)
      val(1) = 1.0
      val(2) = 2.0
      do i=Istart,Iend-1
        col(1) = i
        col(2) = i+1
        if (i .le. n-1) then
          call MatSetValue(A,i,col(1),val(1),INSERT_VALUES,ierr)
        end if
        if (i .lt. n-1) then
          call MatSetValue(A,i,col(2),val(2),INSERT_VALUES,ierr)
        end if
      enddo

      call MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
      call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Compute singular values
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      call SVDCreate(PETSC_COMM_WORLD,svd,ierr)
      call SVDSetOperators(svd,A,PETSC_NULL_MAT,ierr)

!     ** test some interface functions
      call SVDGetOperators(svd,B,PETSC_NULL_MAT,ierr)
      call MatView(B,PETSC_VIEWER_STDOUT_WORLD,ierr)
      call SVDSetConvergenceTest(svd,SVD_CONV_ABS,ierr)
      call SVDSetStoppingTest(svd,SVD_STOP_BASIC,ierr)

!     ** query properties and print them
      call SVDGetProblemType(svd,ptype,ierr)
      if (rank .eq. 0) then
        write(*,105) ptype
      endif
 105  format (/' Problem type = ',I2)
      call SVDIsGeneralized(svd,flg,ierr)
      if (flg .and. rank .eq. 0) then
        write(*,*) 'generalized'
      endif
      call SVDGetImplicitTranspose(svd,tmode,ierr)
      if (rank .eq. 0) then
        if (tmode) then
          write(*,110) 'implicit'
        else
          write(*,110) 'explicit'
        endif
      endif
 110  format (' Transpose mode is',A9)
      call SVDGetConvergenceTest(svd,conv,ierr)
      if (rank .eq. 0) then
        write(*,120) conv
      endif
 120  format (' Convergence test is',I2)
      call SVDGetStoppingTest(svd,stp,ierr)
      if (rank .eq. 0) then
        write(*,130) stp
      endif
 130  format (' Stopping test is',I2)
      call SVDGetWhichSingularTriplets(svd,which,ierr)
      if (rank .eq. 0) then
        if (which .eq. SVD_LARGEST) then
          write(*,140) 'largest'
        else
          write(*,140) 'smallest'
        endif
      endif
 140  format (' Which =',A9)

      call PetscViewerAndFormatCreate(PETSC_VIEWER_STDOUT_WORLD,        &
     &                   PETSC_VIEWER_DEFAULT,vf,ierr)
      call SVDMonitorSet(svd,SVDMONITORFIRST,vf,                        &
     &                   PetscViewerAndFormatDestroy,ierr)
      call SVDMonitorConvergedCreate(PETSC_VIEWER_STDOUT_WORLD,         &
     &                   PETSC_VIEWER_DEFAULT,PETSC_NULL_VEC,vf,ierr)
      call SVDMonitorSet(svd,SVDMONITORCONVERGED,vf,                    &
     &                   SVDMonitorConvergedDestroy,ierr)
      call SVDMonitorCancel(svd,ierr)

!     ** call the solver
      call SVDSetFromOptions(svd,ierr)
      call SVDSolve(svd,ierr)
      call SVDGetConvergedReason(svd,reason,ierr)
      if (rank .eq. 0) then
        write(*,150) reason
      endif
 150  format (' Converged reason:',I2)
      call SVDGetIterationNumber(svd,its,ierr)
!     if (rank .eq. 0) then
!       write(*,160) its
!     endif
!160  format (' Number of iterations of the method:',I4)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Display solution and clean up
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      call SVDErrorView(svd,SVD_ERROR_RELATIVE,PETSC_NULL_VIEWER,ierr)
      call SVDDestroy(svd,ierr)
      call MatDestroy(A,ierr)

      call SlepcFinalize(ierr)
      end

!/*TEST
!
!   test:
!      suffix: 1
!      args: -svd_type {{lanczos trlanczos cross cyclic randomized}}
!      filter: sed -e 's/2.99255/2.99254/'
!
!TEST*/
