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
/* Copyright 1991-2013: John D. McCalpin                                 */
/*-----------------------------------------------------------------------*/
/* License:                                                              */
/*  1. You are free to use this program and/or to redistribute           */
/*     this program.                                                     */
/*  2. You are free to modify this program for your own use,             */
/*     including commercial use, subject to the publication              */
/*     restrictions in item 3.                                           */
/*  3. You are free to publish results obtained from running this        */
/*     program, or from works that you derive from this program,         */
/*     with the following limitations:                                   */
/*     3a. In order to be referred to as "STREAM benchmark results",     */
/*         published results must be in conformance to the STREAM        */
/*         Run Rules, (briefly reviewed below) published at              */
/*         http://www.cs.virginia.edu/stream/ref.html                    */
/*         and incorporated herein by reference.                         */
/*         As the copyright holder, John McCalpin retains the            */
/*         right to determine conformity with the Run Rules.             */
/*     3b. Results based on modified source code or on runs not in       */
/*         accordance with the STREAM Run Rules must be clearly          */
/*         labelled whenever they are published.  Examples of            */
/*         proper labelling include:                                     */
/*           "tuned STREAM benchmark results"                            */
/*           "based on a variant of the STREAM benchmark code"           */
/*         Other comparable, clear, and reasonable labelling is          */
/*         acceptable.                                                   */
/*     3c. Submission of results to the STREAM benchmark web site        */
/*         is encouraged, but not required.                              */
/*  4. Use of this program or creation of derived works based on this    */
/*     program constitutes acceptance of these licensing restrictions.   */
/*  5. Absolutely no warranty is expressed or implied.                   */
/*-----------------------------------------------------------------------*/

/**********************************************************************
 
NAME:      nstream
 
PURPOSE:   To compute memory bandwidth when adding a vector of a given
           number of double precision values to the scalar multiple of 
           another vector of the same length, and storing the result in
           a third vector. 
 
USAGE:     The program takes as input the number of threads, the number 
           of iterations to loop over the triad vectors, the length of the
           vectors, and the offset between vectors
 
           <progname> <# threads> <# iterations> <vector length> <offset>
 
           The output consists of diagnostics to make sure the 
           algorithm worked, and of timing statistics.
 
FUNCTIONS CALLED:
 
           Other than OpenMP or standard C functions, the following 
           external functions are used in this program:
 
           wtime()
           bail_out()
           checkTRIADresults()
 
NOTES:     Bandwidth is determined as the number of words read, plus the 
           number of words written, times the size of the words, divided 
           by the execution time. For a vector length of N, the total 
           number of words read and written is 4*N*sizeof(double).
 
HISTORY:   This code is loosely based on the Stream benchmark by John
           McCalpin, but does not follow all the Stream rules. Hence,
           reported results should not be associated with Stream in
           external publications
REVISION:  Modified by Tim Mattson to handle OpenMP correctly         
REVISION:  Modified by Rob Van der Wijngaart, December 2005, to     
           parameterize vector size and offsets through compiler flags.
           Also removed all Stream cases except TRIAD.                 
REVISION:  Modified by Rob Van der Wijngaart, May 2006, to introduce
           dependence between successive triad operations. This is
           necessary to avoid dead code elimination
**********************************************************************/
 
#include <par-res-kern_general.h>
#include <par-res-kern_omp.h>
 
#define DEFAULTMAXLENGTH 2000000
#ifdef MAXLENGTH
  #if MAXLENGTH > 0
    #define N   MAXLENGTH
  #else
    #define N   DEFAULTMAXLENGTH
  #endif
#else
  #define N   DEFAULTMAXLENGTH
#endif
 
#ifdef STATIC_ALLOCATION
  /* use static to make sure it goes on the heap, not the stack          */
  static double a[N];
#else
  static double * RESTRICT a;
#endif

static double * RESTRICT b;
static double * RESTRICT c;
 
#define SCALAR  3.0
 
static int checkTRIADresults(int, long int);
 
int main(int argc, char **argv) 
{
  long     j, iter;       /* dummies                                     */
  double   scalar;        /* constant used in Triad operation            */
  int      iterations;    /* number of times vector loop gets repeated   */
  long int length,        /* total vector length                         */
           offset;        /* offset between vectors a and b, and b and c */
  double   bytes;         /* memory IO size                              */
  size_t   space;         /* memory used for a single vector             */
  double   nstream_time,  /* timing parameters                           */
           avgtime;
  int      nthread_input; /* thread parameters                           */
  int      nthread; 
  int      num_error=0;     /* flag that signals that requested and 
                              obtained numbers of threads are the same   */
 
/**********************************************************************************
* process and test input parameters    
***********************************************************************************/
 
  if (argc != 5){
     printf("Usage:  %s <# threads> <# iterations> <vector length> <offset>\n", *argv);
     exit(EXIT_FAILURE);
  }
 
  nthread_input = atoi(*++argv);
  iterations    = atoi(*++argv);
  length        = atol(*++argv);
  offset        = atol(*++argv);

  if ((nthread_input < 1) || (nthread_input > MAX_THREADS)) {
    printf("ERROR: Invalid number of threads: %d\n", nthread_input);
    exit(EXIT_FAILURE);
  }
 
  if ((iterations < 1)) {
    printf("ERROR: Invalid number of iterations: %d\n", iterations);
    exit(EXIT_FAILURE);
  }
 
  if (length < 0) {
    printf("ERROR: Invalid vector length: %ld\n", length);
    exit(EXIT_FAILURE);
  }

  if (offset < 0) {
    printf("ERROR: Incvalid array offset: %ld\n", offset);
    exit(EXIT_FAILURE);
  }

#ifdef STATIC_ALLOCATION 
  if ((3*length + 2*offset) > N) {
    printf("ERROR: vector length/offset %ld/%ld too ", length, offset);
    printf("large; increase MAXLENGTH in Makefile or decrease vector length\n");
    exit(EXIT_FAILURE);
  }
#endif
 
  omp_set_num_threads(nthread_input);
 
#ifndef STATIC_ALLOCATION
  space = (3*length + 2*offset)*sizeof(double);
  a = (double *) malloc(space);
  if (!a) {
    printf("ERROR: Could not allocate %ld words for vectors\n", 
           3*length+2*offset);
    exit(EXIT_FAILURE);
  }
#endif
  b = a + length + offset;
  c = b + length + offset;
 
  #pragma omp parallel private(j,iter) 
  {
  #pragma omp master
  {
  nthread = omp_get_num_threads();
 
  printf("OpenMP stream triad: A = B + scalar*C\n");
  if (nthread != nthread_input) {
    num_error = 1;
    printf("ERROR: number of requested threads %d does not equal ",
           nthread_input);
    printf("number of spawned threads %d\n", nthread);
  } 
  else {
    printf("Number of threads    = %i;\n",nthread_input);
    printf("Vector length        = %ld\n", length);
    printf("Offset               = %ld\n", offset);
    printf("Number of iterations = %d\n", iterations);
  }
  }
  bail_out(num_error); 

  #pragma omp for
  #pragma vector always
  for (j=0; j<length; j++) {
    a[j] = 0.0;
    b[j] = 2.0;
    c[j] = 2.0;
  }
    
  /* --- MAIN LOOP --- repeat Triad iterations times --- */
 
  scalar = SCALAR;
 
  for (iter=0; iter<=iterations; iter++) {
 
    if (iter==1) {
      #pragma omp barrier
      #pragma omp master
      {
        nstream_time = wtime();
      }
    }
 
    #pragma omp for
    #pragma vector always
    for (j=0; j<length; j++) a[j] += b[j]+scalar*c[j];
 
  } /* end of iterations                                              */

  #pragma omp barrier
  #pragma omp master
  {
    nstream_time = wtime() - nstream_time;
  }

  }  /* end of OpenMP parallel region */
 
  /*********************************************************************
  ** Analyze and output results.
  *********************************************************************/
 
  bytes   = 4.0 * sizeof(double) * length;
  if (checkTRIADresults(iterations, length)) {
    avgtime = nstream_time/iterations;
    printf("Rate (MB/s): %lf Avg time (s): %lf\n",
           1.0E-06 * bytes/avgtime, avgtime);
   }
  else exit(EXIT_FAILURE);
 
  return 0;
}
 
 
int checkTRIADresults (int iterations, long int length) {
  double aj, bj, cj, scalar, asum;
  double epsilon = 1.e-8;
  int j,iter;
 
  /* reproduce initialization */
  aj = 0.0;
  bj = 2.0;
  cj = 2.0;
 
  /* now execute timing loop */
  scalar = SCALAR;
  for (iter=0; iter<=iterations; iter++) {
    aj += bj+scalar*cj;
  }
 
  aj = aj * (double) (length);
 
  asum = 0.0;
  for (j=0; j<length; j++) asum += a[j];
 
#ifdef VERBOSE
  printf ("Results Comparison: \n");
  printf ("        Expected checksum: %f\n",aj);
  printf ("        Observed checksum: %f\n",asum);
#endif
 
  if (ABS(aj-asum)/asum > epsilon) {
    printf ("Failed Validation on output array\n");
#ifndef VERBOSE
    printf ("        Expected checksum: %f \n",aj);
    printf ("        Observed checksum: %f \n",asum);
#endif
    return (0);
  }
  else {
    printf ("Solution validates\n");
    return (1);
  }
}
