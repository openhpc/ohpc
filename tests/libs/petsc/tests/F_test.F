!
!
!   This program tests storage of PETSc Dense matrix.
!   It Creates a Dense matrix, and stores it into a file,
!   and then reads it back in as a SeqDense and MPIDense
!   matrix, and prints out the contents.
!
      program main
#include <petsc/finclude/petscmat.h>
      use petscmat
      implicit none

      PetscErrorCode ierr
      PetscInt row,col,ten
      PetscMPIInt rank
      PetscScalar  v
      Mat     A
      PetscViewer  view

      call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
      if (ierr .ne. 0) then
        print*,'Unable to initialize PETSc'
        stop
      endif

      call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)
!
!     Proc-0 Create a seq-dense matrix and write it to a file
!
      if (rank .eq. 0) then
         ten = 10
         call MatCreateSeqDense(PETSC_COMM_SELF,ten,ten,                  &
     &        PETSC_NULL_SCALAR,A,ierr)
         v = 1.0
         do row=0,9
            do col=0,9
               call MatSetValue(A,row,col,v,INSERT_VALUES,ierr)
               v = v + 1.0
            enddo
         enddo

         call MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
         call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)

         call PetscObjectSetName(A,'Original Matrix',ierr)
         call MatView(A,PETSC_VIEWER_STDOUT_SELF,ierr)
!
!        Now Write this matrix to a binary file
!
         call PetscViewerBinaryOpen(PETSC_COMM_SELF,'dense.mat',           &
     &        FILE_MODE_WRITE,view,ierr)
         call MatView(A,view,ierr)
         call PetscViewerDestroy(view,ierr)
         call MatDestroy(A,ierr)
!
!        Read this matrix into a SeqDense matrix

         call PetscViewerBinaryOpen(PETSC_COMM_SELF,'dense.mat',            &
     &        FILE_MODE_READ,view,ierr)
         call MatCreate(PETSC_COMM_SELF,A,ierr)
         call MatSetType(A, MATSEQDENSE,ierr)
         call MatLoad(A,view,ierr)

         call PetscObjectSetName(A,'SeqDense Matrix read in from file',     &
     &        ierr)
         call MatView(A,PETSC_VIEWER_STDOUT_SELF,ierr)
         call MatDestroy(A,ierr)
         call PetscViewerDestroy(view,ierr)
      endif

!
!     Use a barrier, so that the procs do not try opening the file before
!     it is created.
!
      call MPI_Barrier(PETSC_COMM_WORLD,ierr)

      call PetscViewerBinaryOpen(PETSC_COMM_WORLD,'dense.mat',               &
     &     FILE_MODE_READ,view,ierr)
      call MatCreate(PETSC_COMM_WORLD,A,ierr)
      call MatSetType(A, MATMPIDENSE,ierr)
      call MatLoad(A,view,ierr)

      call PetscObjectSetName(A, 'MPIDense Matrix read in from file',        &
     &     ierr)
      call MatView(A,PETSC_VIEWER_STDOUT_WORLD,ierr)
      call MatDestroy(A,ierr)
      call PetscViewerDestroy(view,ierr)
      call PetscFinalize(ierr)
      end

!/*TEST
!
!   test:
!      nsize: 2
!      output_file: output/ex63_1.out
!
!TEST*/
