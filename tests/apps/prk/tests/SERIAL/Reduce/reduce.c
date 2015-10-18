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

NAME:    reduce

PURPOSE: This program tests the efficiency with which a pair of 
         vectors can be added in elementwise fashion. 
  
USAGE:   The program takes as input the length of the vectors and the 
         number of times the reduction is repeated, 

         <progname> <# iterations> <vector length> [<algorithm>]
  
         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.

FUNCTIONS CALLED:

         Other than standard C functions, the following 
         functions are used in this program:

         wtime()

HISTORY: Written by Rob Van der Wijngaart, February 2009.
  
*******************************************************************/

#include <par-res-kern_general.h>

int main(int argc, char ** argv)
{
  long   vector_length;    /* length of vectors to be aggregated             */
  double reduce_time,      /* timing parameters                              */
         avgtime;
  double epsilon=1.e-8;    /* error tolerance                                */
  int    i, iter;          /* dummies                                        */
  double element_value;    /* reference element value for final vector       */
  int    iterations;       /* number of times the reduction is carried out   */
  double * RESTRICT vector;/* vector to be reduced                           */
  double * RESTRICT ones;  /* vector to be reduced                           */

/*****************************************************************************
** process and test input parameters    
******************************************************************************/

  if (argc != 3){
    printf("Usage:     %s <# iterations> <vector length>\n", *argv);
    return(EXIT_FAILURE);
  }

  iterations = atoi(*++argv);
  if (iterations < 1){
    printf("ERROR: Iterations must be positive : %d\n", iterations);
    exit(EXIT_FAILURE);
  }

  vector_length  = atol(*++argv);
  if (vector_length < 1){
    printf("ERROR: vector length must be >= 1 : %ld\n",vector_length);
    exit(EXIT_FAILURE);
  }

  printf("Serial Vector Reduction\n");
  printf("Vector length                  = %ld\n", vector_length);
  printf("Number of iterations           = %d\n", iterations);

  vector= (double *) malloc(2*vector_length*sizeof(double)); 
  if (vector==NULL) {
    printf("ERROR: Could not allocate space for vector: %ld\n",
           2*vector_length*sizeof(double));
    exit(EXIT_FAILURE);
  }

  ones = vector + vector_length;

  /* initialize the arrays                                                    */
  for (i=0; i<vector_length; i++) {
    vector[i]  = (double)1;
    ones[i]    = (double)1;
  }

  for (iter=0; iter<=iterations; iter++) {

    /* start timer after a warmup iteration */
    if (iter == 1) { 
      reduce_time = wtime();
    }

    /* do the reduction                                                      */
    for (i=0; i<vector_length; i++) {
      vector[i] += ones[i];
    }

  } /* end of iter loop                                                      */

    reduce_time = wtime() - reduce_time;

  /* verify correctness */
  element_value = (double)(iterations + 2.0);

  for (i=0; i<vector_length; i++) {
    if (ABS(vector[i] - element_value) >= epsilon) {
       printf("First error at i=%d; value: %lf; reference value: %lf\n",
              i, vector[i], element_value);
       exit(EXIT_FAILURE);
    }
  }

  printf("Solution validates\n");
#ifdef VERBOSE
  printf("Element verification value: %lf\n", element_value);
#endif
  avgtime = reduce_time/(double)iterations;
  printf("Rate (MFlops/s): %lf  Avg time (s): %lf\n",
         1.0E-06 * (2.0-1.0)*vector_length/avgtime, avgtime);

  exit(EXIT_SUCCESS);
}
