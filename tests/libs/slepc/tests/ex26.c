/*
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   SLEPc - Scalable Library for Eigenvalue Problem Computations
   Copyright (c) 2002-2016, Universitat Politecnica de Valencia, Spain

   This file is part of SLEPc.

   SLEPc is free software: you can redistribute it and/or modify it under  the
   terms of version 3 of the GNU Lesser General Public License as published by
   the Free Software Foundation.

   SLEPc  is  distributed in the hope that it will be useful, but WITHOUT  ANY
   WARRANTY;  without even the implied warranty of MERCHANTABILITY or  FITNESS
   FOR  A  PARTICULAR PURPOSE. See the GNU Lesser General Public  License  for
   more details.

   You  should have received a copy of the GNU Lesser General  Public  License
   along with SLEPc. If not, see <http://www.gnu.org/licenses/>.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

static char help[] = "Computes the action of the square root of the 2-D Laplacian.\n\n"
  "The command line options are:\n"
  "  -n <n>, where <n> = number of grid subdivisions in x dimension.\n"
  "  -m <m>, where <m> = number of grid subdivisions in y dimension.\n\n";

#include <slepcmfn.h>

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **argv)
{
  Mat                A;           /* problem matrix */
  MFN                mfn;
  FN                 f;
  PetscReal          norm;
  Vec                v,y,z;
  PetscInt           N,n=10,m,Istart,Iend,i,j,II;
  PetscErrorCode     ierr;
  PetscBool          flag,draw_sol;
  MFNConvergedReason reason;

  SlepcInitialize(&argc,&argv,(char*)0,help);

  ierr = PetscOptionsGetInt(NULL,NULL,"-n",&n,NULL);CHKERRQ(ierr);
  ierr = PetscOptionsGetInt(NULL,NULL,"-m",&m,&flag);CHKERRQ(ierr);
  if (!flag) m=n;
  N = n*m;
  ierr = PetscPrintf(PETSC_COMM_WORLD,"\nSquare root of Laplacian y=sqrt(A)*e_1, N=%D (%Dx%D grid)\n\n",N,n,m);CHKERRQ(ierr);

  ierr = PetscOptionsHasName(NULL,NULL,"-draw_sol",&draw_sol);CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                 Compute the discrete 2-D Laplacian, A
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = MatCreate(PETSC_COMM_WORLD,&A);CHKERRQ(ierr);
  ierr = MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,N,N);CHKERRQ(ierr);
  ierr = MatSetFromOptions(A);CHKERRQ(ierr);
  ierr = MatSetUp(A);CHKERRQ(ierr);

  ierr = MatGetOwnershipRange(A,&Istart,&Iend);CHKERRQ(ierr);
  for (II=Istart;II<Iend;II++) {
    i = II/n; j = II-i*n;
    if (i>0) { ierr = MatSetValue(A,II,II-n,-1.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (i<m-1) { ierr = MatSetValue(A,II,II+n,-1.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (j>0) { ierr = MatSetValue(A,II,II-1,-1.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (j<n-1) { ierr = MatSetValue(A,II,II+1,-1.0,INSERT_VALUES);CHKERRQ(ierr); }
    ierr = MatSetValue(A,II,II,4.0,INSERT_VALUES);CHKERRQ(ierr);
  }

  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  /* set symmetry flag so that solver can exploit it */
  ierr = MatSetOption(A,MAT_HERMITIAN,PETSC_TRUE);CHKERRQ(ierr);

  /* set v = e_1 */
  ierr = MatCreateVecs(A,NULL,&v);CHKERRQ(ierr);
  ierr = VecSetValue(v,0,1.0,INSERT_VALUES);CHKERRQ(ierr);
  ierr = VecAssemblyBegin(v);CHKERRQ(ierr);
  ierr = VecAssemblyEnd(v);CHKERRQ(ierr);
  ierr = VecDuplicate(v,&y);CHKERRQ(ierr);
  ierr = VecDuplicate(v,&z);CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
             Create the solver, set the matrix and the function
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  ierr = MFNCreate(PETSC_COMM_WORLD,&mfn);CHKERRQ(ierr);
  ierr = MFNSetOperator(mfn,A);CHKERRQ(ierr);
  ierr = MFNGetFN(mfn,&f);CHKERRQ(ierr);
  ierr = FNSetType(f,FNSQRT);CHKERRQ(ierr);
  ierr = MFNSetFromOptions(mfn);CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                      First solve: y=sqrt(A)*v
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = MFNSolve(mfn,v,y);CHKERRQ(ierr);
  ierr = MFNGetConvergedReason(mfn,&reason);CHKERRQ(ierr);
  if (reason<0) SETERRQ(PETSC_COMM_WORLD,1,"Solver did not converge");
  ierr = VecNorm(y,NORM_2,&norm);CHKERRQ(ierr);
  
  ierr = PetscPrintf(PETSC_COMM_WORLD," Intermediate vector has norm %g\n",(double)norm);CHKERRQ(ierr);
  if (draw_sol) {
    ierr = PetscViewerDrawSetPause(PETSC_VIEWER_DRAW_WORLD,-1);CHKERRQ(ierr);
    ierr = VecView(y,PETSC_VIEWER_DRAW_WORLD);CHKERRQ(ierr);
  }

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
             Second solve: z=sqrt(A)*y and compare against A*v
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = MFNSolve(mfn,y,z);CHKERRQ(ierr);
  ierr = MFNGetConvergedReason(mfn,&reason);CHKERRQ(ierr);
  if (reason<0) SETERRQ(PETSC_COMM_WORLD,1,"Solver did not converge");

  ierr = MatMult(A,v,y);CHKERRQ(ierr);   /* overwrite y */
  ierr = VecAXPY(y,-1.0,z);CHKERRQ(ierr);
  ierr = VecNorm(y,NORM_2,&norm);CHKERRQ(ierr);
  
  if (norm<100*PETSC_MACHINE_EPSILON) {
    ierr = PetscPrintf(PETSC_COMM_WORLD," Error norm is less than 100*epsilon\n\n");CHKERRQ(ierr);
  } else {
    ierr = PetscPrintf(PETSC_COMM_WORLD," Error norm %3.1e\n\n",(double)norm);CHKERRQ(ierr);
  }
  if (draw_sol) {
    ierr = PetscViewerDrawSetPause(PETSC_VIEWER_DRAW_WORLD,-1);CHKERRQ(ierr);
    ierr = VecView(z,PETSC_VIEWER_DRAW_WORLD);CHKERRQ(ierr);
  }

  /* 
     Free work space
  */
  ierr = MFNDestroy(&mfn);CHKERRQ(ierr);
  ierr = MatDestroy(&A);CHKERRQ(ierr);
  ierr = VecDestroy(&v);CHKERRQ(ierr);
  ierr = VecDestroy(&y);CHKERRQ(ierr);
  ierr = VecDestroy(&z);CHKERRQ(ierr);
  ierr = SlepcFinalize();
  return ierr;
}

