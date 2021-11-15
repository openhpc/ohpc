!
!
!   This program demonstrates use of PETSc dense matrices.
!
      program main
#include <petsc/finclude/petscsys.h>
      use petscsys
      implicit none

      PetscErrorCode ierr

      call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
      if (ierr .ne. 0) then
        print*,'Unable to initialize PETSc'
        stop
      endif

!  Demo of PETSc-allocated dense matrix storage
      call Demo1()

!  Demo of user-allocated dense matrix storage
      call Demo2()

      call PetscFinalize(ierr)
      end

! -----------------------------------------------------------------
!
!  Demo1 -  This subroutine demonstrates the use of PETSc-allocated dense
!  matrix storage.  Here MatDenseGetArray() is used for direct access to the
!  array that stores the dense matrix.  The user declares an array (aa(1))
!  and index variable (ia), which are then used together to manipulate
!  the array contents.
!
!  Note the use of PETSC_NULL_SCALAR in MatCreateSeqDense() to indicate that no
!  storage is being provided by the user. (Do NOT pass a zero in that
!  location.)
!
      subroutine Demo1()
#include <petsc/finclude/petscmat.h>
      use petscmat
      implicit none

      Mat         A
      PetscInt   n,m
      PetscErrorCode ierr
      PetscScalar aa(1)
      PetscOffset ia

      n = 4
      m = 5

!  Create matrix

!      call MatCreateSeqDense(PETSC_COMM_SELF,m,n,PETSC_NULL_SCALAR,     &
!     &     A,ierr)

!  Using MatCreate() instead of  MatCreateSeqDense() as above to avoid Nag F90 errors
!  However both cases are equivalent

      call MatCreate(PETSC_COMM_SELF,A,ierr)
      call MatSetSizes(A,m,n,m,n,ierr)
      call MatSetType(A,MATSEQDENSE,ierr)
      call MatSetUp(A,ierr)

!  Access array storage
      call MatDenseGetArray(A,aa,ia,ierr)

!  Set matrix values directly
      call FillUpMatrix(m,n,aa(ia+1))

      call MatDenseRestoreArray(A,aa,ia,ierr)

!  Finalize matrix assembly
      call MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
      call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)

!  View matrix
      call MatView(A,PETSC_VIEWER_STDOUT_SELF,ierr)

!  Clean up
      call MatDestroy(A,ierr)
      return
      end

! -----------------------------------------------------------------
!
!  Demo2 -  This subroutine demonstrates the use of user-allocated dense
!  matrix storage.
!
      subroutine Demo2()
#include <petsc/finclude/petscmat.h>
      use petscmat
      implicit none

      PetscInt   n,m
      PetscErrorCode ierr
      parameter (m=5,n=4)
      Mat       A
      PetscScalar    aa(m,n)

!  Create matrix
      call MatCreateSeqDense(PETSC_COMM_SELF,m,n,aa,A,ierr)

!  Set matrix values directly
      call FillUpMatrix(m,n,aa)

!  Finalize matrix assembly
      call MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
      call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)

!  View matrix
      call MatView(A,PETSC_VIEWER_STDOUT_SELF,ierr)

!  Clean up
      call MatDestroy(A,ierr)
      return
      end

! -----------------------------------------------------------------

      subroutine FillUpMatrix(m,n,X)
      PetscInt          m,n,i,j
      PetscScalar      X(m,n)

      do 10, j=1,n
        do 20, i=1,m
          X(i,j) = 1.0/real(i+j-1)
 20     continue
 10   continue
      return
      end
