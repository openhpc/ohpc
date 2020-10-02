
! Block Jacobi preconditioner for solving a linear system in parallel with KSP
! The code indicates the procedures for setting the particular block sizes and 
! for using different linear solvers on the individual blocks

! This example focuses on ways to customize the block Jacobi preconditioner. 
! See ex1.c and ex2.c for more detailed comments on the basic usage of KSP 
! (including working with matrices and vectors)

! Recall: The block Jacobi method is equivalent to the ASM preconditioner with zero overlap.

!/*T
!   Concepts: KSP^customizing the block Jacobi preconditioner
!   Processors: n
!T*/


program main
#include <petsc/finclude/petscksp.h>
      use petscksp
      
      implicit none
      Vec             :: x,b,u      ! approx solution, RHS, exact solution 
      Mat             :: A            ! linear system matrix 
      KSP             :: ksp         ! KSP context 
      PC              :: myPc           ! PC context 
      PC              :: subpc        ! PC context for subdomain 
      PetscReal       :: norm         ! norm of solution error 
      PetscReal,parameter :: tol = 1.e-6
      PetscErrorCode  :: ierr
      PetscInt        :: i,j,Ii,JJ,n
      PetscInt, parameter :: m = 64
      PetscMPIInt     :: myRank,mySize
      PetscInt        :: its,nlocal,first,Istart,Iend
      PetscScalar     :: v
      PetscScalar, parameter :: &
        myNone = -1.0, &
        sone   = 1.0
      PetscBool       :: isbjacobi,flg
      KSP,allocatable,dimension(:) ::   subksp     ! array of local KSP contexts on this processor 
      PetscInt,allocatable,dimension(:) :: blks
      character(len=80)    :: outputString
      PetscInt,parameter :: one = 1, five = 5
   
      call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
      if (ierr /= 0) then
        write(6,*)'Unable to initialize PETSc'
        stop
      endif
      
      call PetscOptionsGetInt(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,'-m',m,flg,ierr)
      CHKERRA(ierr)
      call MPI_Comm_rank(PETSC_COMM_WORLD,myRank,ierr)
      CHKERRA(ierr)
      call MPI_Comm_size(PETSC_COMM_WORLD,mySize,ierr)
      CHKERRA(ierr)
      n=m+2
      
      !-------------------------------------------------------------------
      ! Compute the matrix and right-hand-side vector that define
      ! the linear system, Ax = b.
      !--------------------------------------------------------------- 

      ! Create and assemble parallel matrix
      
      call  MatCreate(PETSC_COMM_WORLD,A,ierr)
      call  MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,m*n,m*n,ierr)
      call  MatSetFromOptions(A,ierr)
      call  MatMPIAIJSetPreallocation(A,five,PETSC_NULL_INTEGER,five,PETSC_NULL_INTEGER,ierr)
      call  MatSeqAIJSetPreallocation(A,five,PETSC_NULL_INTEGER,ierr)
      call  MatGetOwnershipRange(A,Istart,Iend,ierr)
      
      do Ii=Istart,Iend-1
          v =-1.0; i = Ii/n; j = Ii - i*n
          if (i>0) then
            JJ = Ii - n
            call MatSetValues(A,one,Ii,one,JJ,v,ADD_VALUES,ierr)
            CHKERRA(ierr)
          endif
          
          if (i<m-1) then
            JJ = Ii + n
            call MatSetValues(A,one,Ii,one,JJ,v,ADD_VALUES,ierr)
            CHKERRA(ierr)
          endif
      
          if (j>0) then
            JJ = Ii - 1
            call MatSetValues(A,one,Ii,one,JJ,v,ADD_VALUES,ierr)
            CHKERRA(ierr)
          endif
      
          if (j<n-1) then
            JJ = Ii + 1
            call MatSetValues(A,one,Ii,one,JJ,v,ADD_VALUES,ierr)
            CHKERRA(ierr)
          endif
          
          v=4.0
          call MatSetValues(A,one,Ii,one,Ii,v,ADD_VALUES,ierr)
          CHKERRA(ierr)
          
        enddo
     
        call MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
        CHKERRA(ierr)
        call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)
        CHKERRA(ierr)

      ! Create parallel vectors

      call  VecCreate(PETSC_COMM_WORLD,u,ierr)
      CHKERRA(ierr)
      call  VecSetSizes(u,PETSC_DECIDE,m*n,ierr)
      CHKERRA(ierr)
      call  VecSetFromOptions(u,ierr)
      CHKERRA(ierr)
      call  VecDuplicate(u,b,ierr)
      call  VecDuplicate(b,x,ierr)
      
      ! Set exact solution; then compute right-hand-side vector.
      
      call Vecset(u,sone,ierr)
      CHKERRA(ierr)
      call MatMult(A,u,b,ierr)
      CHKERRA(ierr)
      
      ! Create linear solver context
      
      call KSPCreate(PETSC_COMM_WORLD,ksp,ierr)
      CHKERRA(ierr)
      
      ! Set operators. Here the matrix that defines the linear system
      ! also serves as the preconditioning matrix.
      
      call KSPSetOperators(ksp,A,A,ierr)
      CHKERRA(ierr)
      
      ! Set default preconditioner for this program to be block Jacobi.
      ! This choice can be overridden at runtime with the option
      ! -pc_type <type>
      
      call KSPGetPC(ksp,myPc,ierr)
      CHKERRA(ierr)
      call PCSetType(myPc,PCBJACOBI,ierr)
      CHKERRA(ierr)

      ! -----------------------------------------------------------------
      !            Define the problem decomposition
      !------------------------------------------------------------------- 

      ! Call PCBJacobiSetTotalBlocks() to set individually the size of
      ! each block in the preconditioner.  This could also be done with
      ! the runtime option -pc_bjacobi_blocks <blocks>
      ! Also, see the command PCBJacobiSetLocalBlocks() to set the
      ! local blocks.
      
      ! Note: The default decomposition is 1 block per processor.
      
      allocate(blks(m),source = n)
      
      call PCBJacobiSetTotalBlocks(myPc,m,blks,ierr)
      CHKERRA(ierr)
      deallocate(blks)
       
      !-------------------------------------------------------------------
      !       Set the linear solvers for the subblocks
      !------------------------------------------------------------------- 
      
      !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ! Basic method, should be sufficient for the needs of most users.
      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ! By default, the block Jacobi method uses the same solver on each
      ! block of the problem.  To set the same solver options on all blocks,
      ! use the prefix -sub before the usual PC and KSP options, e.g.,
      ! -sub_pc_type <pc> -sub_ksp_type <ksp> -sub_ksp_rtol 1.e-4
      
      !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !  Advanced method, setting different solvers for various blocks.
      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      
      ! Note that each block's KSP context is completely independent of
      ! the others, and the full range of uniprocessor KSP options is
      ! available for each block. The following section of code is intended
      ! to be a simple illustration of setting different linear solvers for
      ! the individual blocks.  These choices are obviously not recommended
      ! for solving this particular problem.

      call PetscObjectTypeCompare(myPc,PCBJACOBI,isbjacobi,ierr)
      
      
      if (isbjacobi) then
      
        ! Call KSPSetUp() to set the block Jacobi data structures (including
        ! creation of an internal KSP context for each block).
        ! Note: KSPSetUp() MUST be called before PCBJacobiGetSubKSP()

        call KSPSetUp(ksp,ierr)
        
        ! Extract the array of KSP contexts for the local blocks

        call PCBJacobiGetSubKSP(myPc,nlocal,first,PETSC_NULL_KSP,ierr)

        call PCBJacobiGetSubKSP(myPc,nlocal,first,PETSC_NULL_KSP,ierr)
        allocate(subksp(nlocal))
        call PCBJacobiGetSubKSP(myPc,nlocal,first,subksp,ierr)
      

        ! Loop over the local blocks, setting various KSP options for each block
        
        do i=0,nlocal-1
          
          call KSPGetPC(subksp(i+1),subpc,ierr)
          
          if (myRank>0) then
            
            if (mod(i,2)==1) then 
               call PCSetType(subpc,PCILU,ierr)
               CHKERRA(ierr)
              
            else
              call PCSetType(subpc,PCNONE,ierr)
              CHKERRA(ierr)
              call KSPSetType(subksp(i+1),KSPBCGS,ierr)
              CHKERRA(ierr)
              call KSPSetTolerances(subksp(i+1),tol,PETSC_DEFAULT_REAL,PETSC_DEFAULT_REAL,PETSC_DEFAULT_INTEGER,ierr)
              CHKERRA(ierr)
            endif
            
          else 
             call PCSetType(subpc,PCJACOBI,ierr)
             CHKERRA(ierr)
             call KSPSetType(subksp(i+1),KSPGMRES,ierr)
             CHKERRA(ierr)
             call KSPSetTolerances(subksp(i+1),tol,PETSC_DEFAULT_REAL,PETSC_DEFAULT_REAL,PETSC_DEFAULT_INTEGER,ierr)
             CHKERRA(ierr)
          endif
          
        end do
        
      endif

      !----------------------------------------------------------------
      !                Solve the linear system
      !-----------------------------------------------------------------
      
      ! Set runtime options
      
      call KSPSetFromOptions(ksp,ierr); CHKERRA(ierr)
      
      ! Solve the linear system
     
      call KSPSolve(ksp,b,x,ierr); CHKERRA(ierr)
      
      !  -----------------------------------------------------------------
      !               Check solution and clean up
      !------------------------------------------------------------------- 
      
 
      !  -----------------------------------------------------------------
      ! Check the error
      !  -----------------------------------------------------------------
      
      !call VecView(x,PETSC_VIEWER_STDOUT_WORLD,ierr)

      call VecAXPY(x,myNone,u,ierr)
      
      !call VecView(x,PETSC_VIEWER_STDOUT_WORLD,ierr)


      call VecNorm(x,NORM_2,norm,ierr); CHKERRA(ierr)
      call KSPGetIterationNumber(ksp,its,ierr)
      CHKERRA(ierr)
      write(outputString,*)'Norm of error',real(norm),'Iterations',its,'\n'         ! PETScScalar might be of complex type
      call PetscPrintf(PETSC_COMM_WORLD,outputString,ierr)
      CHKERRA(ierr)
      
      ! Free work space.  All PETSc objects should be destroyed when they
      ! are no longer needed.
      deallocate(subksp)
      call KSPDestroy(ksp,ierr)
      CHKERRA(ierr)
      call VecDestroy(u,ierr)
      CHKERRA(ierr)
      call VecDestroy(b,ierr)
      CHKERRA(ierr)
      call MatDestroy(A,ierr)
      CHKERRA(ierr)
      call VecDestroy(x,ierr)
      CHKERRA(ierr)
      call PetscFinalize(ierr)
      CHKERRA(ierr)
      
end program main

!/*TEST
!
!   test:
!      nsize: 2
!      args: -ksp_monitor_short -ksp_gmres_cgs_refinement_type refine_always> ex7_1.tmp 2>&1
!
!   test:
!      suffix: 2
!      nsize: 2
!      args: -ksp_view
!
!TEST*/
