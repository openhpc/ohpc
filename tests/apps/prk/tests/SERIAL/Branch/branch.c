/*
Copyright (c) 2013, Intel Corporation

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions 
are met:

* Redistributions of source code must retain the above copyright 
      notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above 
      copyright notice, this list of conditions and the following 
      disclaimer in the documentation and/or other materials provided 
      with the distribution.
* Neither the name of Intel Corporation nor the names of its 
      contributors may be used to endorse or promote products 
      derived from this software without specific prior written 
      permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN 
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.
*/

/*******************************************************************

NAME:    Branch

PURPOSE: This program tests the effect of inner-loop branches on
         application performance. We investigate four cases. The first
         three all concern light-weight loops, i.e. loops that have
         very few instructions associated with them.
         1) branches inside vectorizable loops where the branch does
            not necessarily inhibit vectorization: vector_go
         2) branches inside vectorizable loops where the branch does
            inhibit vectorization: vector_stop
         3) branches inside non-vectorizable loops: no_vector
         4) branches inside non-vectorizable loops in which each branch
            corresponds to a sizeable and different set of instructions:
            ins-heavy

CONSTRAINTS:
         - the code should be continuously scalable, i.e. the user should
           be able to specify the amount of work to be done.
         - the code should be verifiable.
         - the code should be executable with and without branches, with 
           otherwise identical amounts of work, to assess the impact of the 
           branches.
         - the performance of the code should be dominated by the work in
           the loops, not by memory bandwidth required to fetch data. This 
           means that arrays should fit in cache, and any loop over arrays 
           should be executed many times to amortize the initial memory load 
           costs and to remove noise from the timings.
         - any arrays used should be initialized only once, to avoid confusing
           performance impact of initialization with that of the branches. 
           Because the base loop over the array is short, it completes very
           quickly, leading to very noisy results if it were timed separately.
           Hence, we must time the ensemble of all iterations over the base
           loop, which would include reinitializations if present.
         - the branches should be "unpredictable," meaning that if the compiler
           guesses them to be always taken or to be always not taken, it will
           be wrong often. Otherwise the cost of a mispredicted branch may
           not show up in the performance results.
         - the amount of work in the codes containing the three different
           types of light-weight loops should be the same to allow fair
           comparisions.
         - the code should not not produce overflow or underflow. 
         - the actual cost of computing the branch condition should be small,
           so that we can assess the cost of the occurrence of the branch as
           it disrupts vectorization and the hardware pipelines). If the
           condition were expensive to compute and we run the code with and
           without the branch, the performance difference would be exaggerated.
         - Note: Casts from integer to float or double are not always vectorizable. 

APPROACH:
         - to avoid casts and keep conditionals inexpensive and exact, we use 
           only integer operations. 
         - we make sure that the numerical results of the codes for the
           different branch structures and for the different paths following
           the branch are identical.
         - conditionals are simple comparisons to zero of quantities that
           are computed anyway.
         - initialization produces a saw-tooth pattern with frequent sign
           crossings to defeat speculative branch execution.
         - successive iterations over a relatively short array result simply
           in a change of sign of all array elements, so that the results are
           bounded, and verification values are easily computable.

USAGE:   The program takes as input the length of the
         vector loop, the number of repetitions of the loop, and the type of 
         branching

         <progname> <# iterations> <vector length> <branch_type>
  
         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.

FUNCTIONS CALLED:

         Other than standard C functions, the following 
         functions are used in this program:

         wtime()
         fill_vec()
         func*()

HISTORY: Written by Rob Van der Wijngaart, February 2009.
  
**********************************************************************************/

#include <par-res-kern_general.h>

/* the following values are only used as labels                                  */
#define VECTOR_STOP       66
#define VECTOR_GO         77
#define NO_VECTOR         88
#define INS_HEAVY         99
#define WITH_BRANCHES      1
#define WITHOUT_BRANCHES   0

extern int fill_vec(int *vector, int vector_length, int iterations, int branch, 
                    int *nfunc, int *rank);

int main(int argc, char ** argv)
{
  long     vector_length;   /* length of vector loop containing the branch       */
  int      nfunc;           /* number of functions used in INS_HEAVY option      */
  int      rank;            /* matrix rank used in INS_HEAVY option              */
  double   branch_time,     /* timing parameters                                 */
           no_branch_time;
  double   ops;             /* double precision representation of integer ops    */
  int      iterations;      /* number of times the branch loop is carried out    */
  int      i, iter, aux;    /* dummies                                           */
  char     *branch_type;    /* string defining branching type                    */
  int      btype;           /* integer encoding branching type                   */
  int      total=0, 
           total_ref;       /* computed and stored verification values           */
  int * RESTRICT vector; 
  int * RESTRICT index;
  int factor = -1;

/**********************************************************************************
** process and test input parameters    
**********************************************************************************/

  if (argc != 4){
    printf("Usage:     %s <# iterations> <vector length>", *argv);
    printf("<branching type>\n");
    printf("branching type: vector_go, vector_stop, no_vector, ins_heavy\n");
    exit(EXIT_FAILURE);
  }

  iterations = atoi(*++argv);
  if (iterations < 1 || iterations%2==1){
     printf("ERROR: Iterations must be positive and even : %d \n", iterations);
     exit(EXIT_FAILURE);
  }

  vector_length  = atol(*++argv);
  if (vector_length < 1){
     printf("ERROR: loop length must be >= 1 : %d \n",vector_length);
     exit(EXIT_FAILURE);
  }

  branch_type = *++argv;
  if      (!strcmp(branch_type,"vector_stop")) btype = VECTOR_STOP;
  else if (!strcmp(branch_type,"vector_go"  )) btype = VECTOR_GO;
  else if (!strcmp(branch_type,"no_vector"  )) btype = NO_VECTOR;
  else if (!strcmp(branch_type,"ins_heavy"  )) btype = INS_HEAVY;
  else  {
    printf("Wrong branch type: %s; choose vector_stop, vector_go, ", branch_type);
    printf("no_vector, or ins_heavy\n");
    exit(EXIT_FAILURE);
  }

  printf("Serial Branching Bonanza\n");
  printf("Vector length              = %ld\n", vector_length);
  printf("Number of iterations       = %d\n", iterations);
  printf("Branching type             = %s\n", branch_type);

  vector = malloc(vector_length*2*sizeof(int));
  if (!vector) {
    printf("ERROR: Failed to allocate space for vector\n");
    exit(EXIT_FAILURE);
  }

  /* grab the second half of vector to store index array                         */
  index   = vector + vector_length;

  /* initialize the array with entries with varying signs; array "index" is only 
     used to obfuscate the compiler (i.e. it won't vectorize a loop containing
     indirect referencing). It functions as the identity operator.               */
  for (i=0; i<vector_length; i++) { 
    vector[i]  = 3 - (i&7);
    index[i]   = i;
  }

  branch_time = wtime();

  /* do actual branching */

  switch (btype) {

    case VECTOR_STOP:
      /* condition vector[index[i]]>0 inhibits vectorization                     */
      for (iter=0; iter<iterations; iter+=2) {
        #pragma vector always
        for (i=0; i<vector_length; i++) { 
          aux = -(3 - (i&7));
          if (vector[index[i]]>0) vector[i] -= 2*vector[i];
          else                    vector[i] -= 2*aux;
        }
        #pragma vector always
        for (i=0; i<vector_length; i++) { 
          aux = (3 - (i&7));
          if (vector[index[i]]>0) vector[i] -= 2*vector[i];
          else                    vector[i] -= 2*aux;
        }
      }
      break;

    case VECTOR_GO:
      /* condition aux>0 allows vectorization                                    */
      for (iter=0; iter<iterations; iter+=2) {
        #pragma vector always
        for (i=0; i<vector_length; i++) {
          aux = -(3 - (i&7));
          if (aux>0) vector[i] -= 2*vector[i];
          else       vector[i] -= 2*aux;
        }
        #pragma vector always
        for (i=0; i<vector_length; i++) {
          aux = (3 - (i&7));
          if (aux>0) vector[i] -= 2*vector[i];
          else       vector[i] -= 2*aux;
        }
      }
      break;

    case NO_VECTOR:
      /* condition aux>0 allows vectorization, but indirect indexing inbibits it */
      for (iter=0; iter<iterations; iter+=2) {
        #pragma vector always
        for (i=0; i<vector_length; i++) {
          aux = -(3 - (i&7));
          if (aux>0) vector[i] -= 2*vector[index[i]];
          else       vector[i] -= 2*aux;
        }
        #pragma vector always
        for (i=0; i<vector_length; i++) {
          aux = (3 - (i&7));
          if (aux>0) vector[i] -= 2*vector[index[i]];
          else       vector[i] -= 2*aux;
        }
      }
      break;

    case INS_HEAVY:
      fill_vec(vector, vector_length, iterations, WITH_BRANCHES, &nfunc, &rank);
    }

    branch_time = wtime() - branch_time;
    if (btype == INS_HEAVY) {
      printf("Number of matrix functions = %d\n", nfunc);
      printf("Matrix order               = %d\n", rank);
    }

    /* do the whole thing once more, but now without branches                    */

    no_branch_time = wtime();

    /* do actual branching */

    switch (btype) {

    case VECTOR_STOP:
    case VECTOR_GO:
      for (iter=0; iter<iterations; iter+=2) {
        #pragma vector always
        for (i=0; i<vector_length; i++) { 
          aux = -(3-(i&7)); 
          vector[i] -= (vector[i] + aux);
        }
        for (i=0; i<vector_length; i++) {
          aux = (3-(i&7)); 
          vector[i] -= (vector[i] + aux);
        }
      }
      break;

    case NO_VECTOR:
      for (iter=0; iter<iterations; iter+=2) {
        #pragma vector always
        for (i=0; i<vector_length; i++) {
          aux = -(3-(i&7));
          vector[i] -= (vector[index[i]]+aux); 
        }
        #pragma vector always
        for (i=0; i<vector_length; i++) {
          aux = (3-(i&7));
          vector[i] -= (vector[index[i]]+aux); 
        }
      }
      break;

    case INS_HEAVY:
      fill_vec(vector, vector_length, iterations, WITHOUT_BRANCHES, &nfunc, &rank);
    }

    no_branch_time = wtime() - no_branch_time;
    ops = (double)vector_length * (double)iterations;
    if (btype == INS_HEAVY) ops *= rank*(rank*19 + 6);
    else                    ops *= 4;

    for (total = 0, i=0; i<vector_length; i++) total += vector[i];

  /* compute verification values                                                 */
  total_ref = ((vector_length%8)*(vector_length%8-8) + vector_length)/2;

  if (total == total_ref) {
    printf("Solution validates\n");
    printf("Rate (Mops/s) with branches:    %lf time (s): %lf\n", 
           ops/(branch_time*1.e6), branch_time);
    printf("Rate (Mops/s) without branches: %lf time (s): %lf\n", 
           ops/(no_branch_time*1.e6), no_branch_time);
#ifdef VERBOSE
    printf("Array sum = %d, reference value = %d\n", total, total_ref);
#endif     
  }
  else {
    printf("ERROR: array sum = %d, reference value = %d\n", total, total_ref);
  }

  exit(EXIT_SUCCESS);
}
