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

static char help[] = "Singular value decomposition of the Lauchli matrix.\n"
  "The command line options are:\n"
  "  -n <n>, where <n> = matrix dimension.\n"
  "  -mu <mu>, where <mu> = subdiagonal value.\n\n";

#include <slepcsvd.h>

int main(int argc,char **argv)
{
  Mat            A;               /* operator matrix */
  Vec            u,v;             /* left and right singular vectors */
  SVD            svd;             /* singular value problem solver context */
  SVDType        type;
  PetscReal      error,tol,sigma,mu=PETSC_SQRT_MACHINE_EPSILON;
  PetscInt       n=100,i,j,Istart,Iend,nsv,maxit,its,nconv;

  PetscCall(SlepcInitialize(&argc,&argv,(char*)0,help));

  PetscCall(PetscOptionsGetInt(NULL,NULL,"-n",&n,NULL));
  PetscCall(PetscOptionsGetReal(NULL,NULL,"-mu",&mu,NULL));
  PetscCall(PetscPrintf(PETSC_COMM_WORLD,"\nLauchli singular value decomposition, (%" PetscInt_FMT " x %" PetscInt_FMT ") mu=%g\n\n",n+1,n,(double)mu));

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                          Build the Lauchli matrix
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  PetscCall(MatCreate(PETSC_COMM_WORLD,&A));
  PetscCall(MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,n+1,n));
  PetscCall(MatSetFromOptions(A));
  PetscCall(MatSetUp(A));

  PetscCall(MatGetOwnershipRange(A,&Istart,&Iend));
  for (i=Istart;i<Iend;i++) {
    if (i == 0) {
      for (j=0;j<n;j++) {
        PetscCall(MatSetValue(A,0,j,1.0,INSERT_VALUES));
      }
    } else {
      PetscCall(MatSetValue(A,i,i-1,mu,INSERT_VALUES));
    }
  }

  PetscCall(MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY));
  PetscCall(MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY));
  PetscCall(MatCreateVecs(A,&v,&u));

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          Create the singular value solver and set various options
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  /*
     Create singular value solver context
  */
  PetscCall(SVDCreate(PETSC_COMM_WORLD,&svd));

  /*
     Set operators
  */
  PetscCall(SVDSetOperators(svd,A,NULL));

  /*
     Use thick-restart Lanczos as default solver
  */
  PetscCall(SVDSetType(svd,SVDTRLANCZOS));

  /*
     Set solver parameters at runtime
  */
  PetscCall(SVDSetFromOptions(svd));

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                      Solve the singular value system
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  PetscCall(SVDSolve(svd));
  PetscCall(SVDGetIterationNumber(svd,&its));
  PetscCall(PetscPrintf(PETSC_COMM_WORLD," Number of iterations of the method: %" PetscInt_FMT "\n",its));

  /*
     Optional: Get some information from the solver and display it
  */
  PetscCall(SVDGetType(svd,&type));
  PetscCall(PetscPrintf(PETSC_COMM_WORLD," Solution method: %s\n\n",type));
  PetscCall(SVDGetDimensions(svd,&nsv,NULL,NULL));
  PetscCall(PetscPrintf(PETSC_COMM_WORLD," Number of requested singular values: %" PetscInt_FMT "\n",nsv));
  PetscCall(SVDGetTolerances(svd,&tol,&maxit));
  PetscCall(PetscPrintf(PETSC_COMM_WORLD," Stopping condition: tol=%.4g, maxit=%" PetscInt_FMT "\n",(double)tol,maxit));

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                    Display solution and clean up
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  /*
     Get number of converged singular triplets
  */
  PetscCall(SVDGetConverged(svd,&nconv));
  PetscCall(PetscPrintf(PETSC_COMM_WORLD," Number of converged approximate singular triplets: %" PetscInt_FMT "\n\n",nconv));

  if (nconv>0) {
    /*
       Display singular values and relative errors
    */
    PetscCall(PetscPrintf(PETSC_COMM_WORLD,
         "          sigma           relative error\n"
         "  --------------------- ------------------\n"));
    for (i=0;i<nconv;i++) {
      /*
         Get converged singular triplets: i-th singular value is stored in sigma
      */
      PetscCall(SVDGetSingularTriplet(svd,i,&sigma,u,v));

      /*
         Compute the error associated to each singular triplet
      */
      PetscCall(SVDComputeError(svd,i,SVD_ERROR_RELATIVE,&error));

      PetscCall(PetscPrintf(PETSC_COMM_WORLD,"       % 6f      ",(double)sigma));
      PetscCall(PetscPrintf(PETSC_COMM_WORLD," % 12g\n",(double)error));
    }
    PetscCall(PetscPrintf(PETSC_COMM_WORLD,"\n"));
  }

  /*
     Free work space
  */
  PetscCall(SVDDestroy(&svd));
  PetscCall(MatDestroy(&A));
  PetscCall(VecDestroy(&u));
  PetscCall(VecDestroy(&v));
  PetscCall(SlepcFinalize());
  return 0;
}
