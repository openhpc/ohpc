/*
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   SLEPc - Scalable Library for Eigenvalue Problem Computations
   Copyright (c) 2002-2020, Universitat Politecnica de Valencia, Spain

   This file is part of SLEPc.
   SLEPc is distributed under a 2-clause BSD license (see LICENSE).
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

static char help[] = "Solves the same problem as in ex5, with a user-defined stopping test."
  "It is a standard nonsymmetric eigenproblem with real eigenvalues and the rightmost eigenvalue is known to be 1.\n"
  "This example illustrates how the user can set a custom stopping test function.\n\n"
  "The command line options are:\n"
  "  -m <m>, where <m> = number of grid subdivisions in each dimension.\n"
  "  -seconds <s>, where <s> = maximum time in seconds allowed for computation.\n\n";

#include <slepceps.h>
#include <petsctime.h>

/*
   User-defined routines
*/

PetscErrorCode MyStoppingTest(EPS,PetscInt,PetscInt,PetscInt,PetscInt,EPSConvergedReason*,void*);
PetscErrorCode MatMarkovModel(PetscInt,Mat);

int main(int argc,char **argv)
{
  Mat                A;               /* operator matrix */
  EPS                eps;             /* eigenproblem solver context */
  PetscReal          seconds=2.5;     /* maximum time allowed for computation */
  PetscLogDouble     deadline;        /* time to abort computation */
  PetscInt           N,m=15,nconv;
  PetscBool          terse;
  PetscViewer        viewer;
  EPSConvergedReason reason;
  PetscCall(SlepcInitialize(&argc,&argv,(char*)0,help));

  PetscCall(PetscOptionsGetInt(NULL,NULL,"-m",&m,NULL));
  N = m*(m+1)/2;
  PetscCall(PetscPrintf(PETSC_COMM_WORLD,"\nMarkov Model, N=%" PetscInt_FMT " (m=%" PetscInt_FMT ")\n",N,m));
  PetscCall(PetscOptionsGetReal(NULL,NULL,"-seconds",&seconds,NULL));
  PetscCall(PetscPrintf(PETSC_COMM_WORLD,"Maximum time for computation is set to %g seconds.\n\n",(double)seconds));
  deadline = seconds;
  PetscCall(PetscTimeAdd(&deadline));

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     Compute the operator matrix that defines the eigensystem, Ax=kx
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  PetscCall(MatCreate(PETSC_COMM_WORLD,&A));
  PetscCall(MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,N,N));
  PetscCall(MatSetFromOptions(A));
  PetscCall(MatSetUp(A));
  PetscCall(MatMarkovModel(m,A));

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                Create the eigensolver and set various options
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  PetscCall(EPSCreate(PETSC_COMM_WORLD,&eps));
  PetscCall(EPSSetOperators(eps,A,NULL));
  PetscCall(EPSSetProblemType(eps,EPS_NHEP));
  PetscCall(EPSSetStoppingTestFunction(eps,MyStoppingTest,&deadline,NULL));
  PetscCall(EPSSetFromOptions(eps));

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                      Solve the eigensystem
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  PetscCall(EPSSolve(eps));

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                    Display solution and clean up
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  /* show detailed info unless -terse option is given by user */
  PetscCall(PetscOptionsHasName(NULL,NULL,"-terse",&terse));
  if (terse) {
    PetscCall(EPSErrorView(eps,EPS_ERROR_RELATIVE,NULL));
  } else {
    PetscCall(PetscViewerASCIIGetStdout(PETSC_COMM_WORLD,&viewer));
    PetscCall(PetscViewerPushFormat(viewer,PETSC_VIEWER_ASCII_INFO_DETAIL));
    PetscCall(EPSGetConvergedReason(eps,&reason));
    if (reason!=EPS_CONVERGED_USER) {
      PetscCall(EPSConvergedReasonView(eps,viewer));
      PetscCall(EPSErrorView(eps,EPS_ERROR_RELATIVE,viewer));
    } else {
      PetscCall(EPSGetConverged(eps,&nconv));
      PetscCall(PetscViewerASCIIPrintf(viewer,"Eigensolve finished with %" PetscInt_FMT " converged eigenpairs; reason=%s\n",nconv,EPSConvergedReasons[reason]));
    }
    PetscCall(PetscViewerPopFormat(viewer));
  }
  PetscCall(EPSDestroy(&eps));
  PetscCall(MatDestroy(&A));
  PetscCall(SlepcFinalize());
  return 0;
}

/*
    Matrix generator for a Markov model of a random walk on a triangular grid.

    This subroutine generates a test matrix that models a random walk on a
    triangular grid. This test example was used by G. W. Stewart ["{SRRIT} - a
    FORTRAN subroutine to calculate the dominant invariant subspaces of a real
    matrix", Tech. report. TR-514, University of Maryland (1978).] and in a few
    papers on eigenvalue problems by Y. Saad [see e.g. LAA, vol. 34, pp. 269-295
    (1980) ]. These matrices provide reasonably easy test problems for eigenvalue
    algorithms. The transpose of the matrix  is stochastic and so it is known
    that one is an exact eigenvalue. One seeks the eigenvector of the transpose
    associated with the eigenvalue unity. The problem is to calculate the steady
    state probability distribution of the system, which is the eigevector
    associated with the eigenvalue one and scaled in such a way that the sum all
    the components is equal to one.

    Note: the code will actually compute the transpose of the stochastic matrix
    that contains the transition probabilities.
*/
PetscErrorCode MatMarkovModel(PetscInt m,Mat A)
{
  const PetscReal cst = 0.5/(PetscReal)(m-1);
  PetscReal       pd,pu;
  PetscInt        Istart,Iend,i,j,jmax,ix=0;

  PetscFunctionBeginUser;
  PetscCall(MatGetOwnershipRange(A,&Istart,&Iend));
  for (i=1;i<=m;i++) {
    jmax = m-i+1;
    for (j=1;j<=jmax;j++) {
      ix = ix + 1;
      if (ix-1<Istart || ix>Iend) continue;  /* compute only owned rows */
      if (j!=jmax) {
        pd = cst*(PetscReal)(i+j-1);
        /* north */
        if (i==1) {
          PetscCall(MatSetValue(A,ix-1,ix,2*pd,INSERT_VALUES));
        } else {
          PetscCall(MatSetValue(A,ix-1,ix,pd,INSERT_VALUES));
        }
        /* east */
        if (j==1) {
          PetscCall(MatSetValue(A,ix-1,ix+jmax-1,2*pd,INSERT_VALUES));
        } else {
          PetscCall(MatSetValue(A,ix-1,ix+jmax-1,pd,INSERT_VALUES));
        }
      }
      /* south */
      pu = 0.5 - cst*(PetscReal)(i+j-3);
      if (j>1) {
        PetscCall(MatSetValue(A,ix-1,ix-2,pu,INSERT_VALUES));
      }
      /* west */
      if (i>1) {
        PetscCall(MatSetValue(A,ix-1,ix-jmax-2,pu,INSERT_VALUES));
      }
    }
  }
  PetscCall(MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY));
  PetscCall(MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY));
  PetscFunctionReturn(PETSC_SUCCESS);
}

/*
    Function for user-defined stopping test.

    Checks that the computing time has not exceeded the deadline.
*/
PetscErrorCode MyStoppingTest(EPS eps,PetscInt its,PetscInt max_it,PetscInt nconv,PetscInt nev,EPSConvergedReason *reason,void *ctx)
{
  PetscLogDouble now,deadline = *(PetscLogDouble*)ctx;

  PetscFunctionBeginUser;
  /* check if usual termination conditions are met */
  PetscCall(EPSStoppingBasic(eps,its,max_it,nconv,nev,reason,NULL));
  if (*reason==EPS_CONVERGED_ITERATING) {
    /* check if deadline has expired */
    PetscCall(PetscTime(&now));
    if (now>deadline) *reason = EPS_CONVERGED_USER;
  }
  PetscFunctionReturn(PETSC_SUCCESS);
}

/*TEST

   test:
      suffix: 1
      args: -m 350 -seconds 0.6

TEST*/
