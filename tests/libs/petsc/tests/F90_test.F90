!
!
      program main
#include <petsc/finclude/petscvec.h>
      use petscvec
      implicit none
!
!  This example demonstrates basic use of the PETSc Fortran interface
!  to vectors.
!
       PetscInt  n
       PetscErrorCode ierr
       PetscBool  flg
       PetscScalar      one,two,three,dot
       PetscReal        norm,rdot
       Vec              x,y,w
       PetscOptions     options

       n     = 20
       one   = 1.0
       two   = 2.0
       three = 3.0

       PetscCallA(PetscInitialize(ierr))
       PetscCallA(PetscOptionsCreate(options,ierr))
       PetscCallA(PetscOptionsGetInt(options,PETSC_NULL_CHARACTER,'-n',n,flg,ierr))
       PetscCallA(PetscOptionsDestroy(options,ierr))

! Create a vector, then duplicate it
       PetscCallA(VecCreate(PETSC_COMM_WORLD,x,ierr))
       PetscCallA(VecSetSizes(x,PETSC_DECIDE,n,ierr))
       PetscCallA(VecSetFromOptions(x,ierr))
       PetscCallA(VecDuplicate(x,y,ierr))
       PetscCallA(VecDuplicate(x,w,ierr))

       PetscCallA(VecSet(x,one,ierr))
       PetscCallA(VecSet(y,two,ierr))

       PetscCallA(VecDot(x,y,dot,ierr))
       rdot = PetscRealPart(dot)
       write(6,100) rdot
  100  format('Result of inner product ',f10.4)

       PetscCallA(VecScale(x,two,ierr))
       PetscCallA(VecNorm(x,NORM_2,norm,ierr))
       write(6,110) norm
  110  format('Result of scaling ',f10.4)

       PetscCallA(VecCopy(x,w,ierr))
       PetscCallA(VecNorm(w,NORM_2,norm,ierr))
       write(6,120) norm
  120  format('Result of copy ',f10.4)

       PetscCallA(VecAXPY(y,three,x,ierr))
       PetscCallA(VecNorm(y,NORM_2,norm,ierr))
       write(6,130) norm
  130  format('Result of axpy ',f10.4)

       PetscCallA(VecDestroy(x,ierr))
       PetscCallA(VecDestroy(y,ierr))
       PetscCallA(VecDestroy(w,ierr))
       PetscCallA(PetscFinalize(ierr))
       end
