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

PURPOSE: This program tests the efficiency with which a collection of 
         vectors that are distributed among the threads can be added in
         elementwise fashion. The number of vectors per thread is two, 
         so that a reduction will take place even if the code runs on
         just a single thread.
  
USAGE:   The program takes as input the number of threads, the length 
         of the vectors, the number of times the reduction is repeated, 
         plus, optionally, the type of reduction algorithm . The default 
         algorithm is binary tree reduction with point-to-point 
         synchronization.
         Note that vector reduction is not currently available in C
         in the OpenMP standard (version 2.5).

         <progname>  <# threads> <# iterations> <vector length> [<algorithm>]
  
         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.

FUNCTIONS CALLED:

         Other than OpenMP or standard C functions, the following 
         functions are used in this program:

         wtime()
         bail_out()

NOTES:   The long-optimal algorithm is based on a distributed memory
         algorithm decribed in:
         Collective Communication; Theory, Practice, and Experience by
         Chan, Heimlich, Purkayastha, Van de Geijn (to Appear). This
         is a two-phase, multi-stage algorithm. In the first phase,
         partial sums of the vectors are built by each thread locally.
         In the second phase the partial sums are collected on the 
         master thread. In the distributed-memory algorithm the second
         phase also has multiple stages. In the shared-memory algorithm
         it is more efficient to let each thread write its contribution
         into the master thread vector in a single stage.

HISTORY: Written by Rob Van der Wijngaart, March 2006.
  
*******************************************************************/

#include <par-res-kern_general.h>
#include <par-res-kern_omp.h>

#define LINEAR            11
#define BINARY_BARRIER    12
#define BINARY_P2P        13
#define LONG_OPTIMAL      14
#define NONE              15
#define LOCAL             16
#define VEC0(id,i)        vector[(id        )*(vector_length)+i]
#define VEC1(id,i)        vector[(id+nthread)*(vector_length)+i]
/* define shorthand for flag with cache line padding                             */
#define LINEWORDS         16
#define flag(i)           flag[(i)*LINEWORDS]

int main(int argc, char ** argv)
{
  int    my_ID;           /* Thread ID                                       */
  long   vector_length;   /* length of vectors to be aggregated              */
  long   total_length;    /* bytes needed to store reduction vectors         */
  double reduce_time,     /* timing parameters                               */
         avgtime;
  double epsilon=1.e-8;   /* error tolerance                                 */
  int    group_size,      /* size of aggregating half of thread pool         */
         old_size,        /* group size in previous binary tree iteration    */
         i, id, iter, stage; /* dummies                                      */
  double element_value;   /* reference element value for final vector        */
  char   *algorithm;      /* reduction algorithm selector                    */
  int    intalgorithm;    /* integer encoding of algorithm selector          */
  int    iterations;      /* number of times the reduction is carried out    */
  int    flag[MAX_THREADS*LINEWORDS]; /* used for pairwise synchronizations  */
  int    start[MAX_THREADS],
         end[MAX_THREADS];/* segments of vectors for bucket algorithm        */
  long   segment_size;
  int    my_donor, my_segment;
  int    nthread_input,   /* thread parameters                               */
         nthread;   
  double RESTRICT *vector;/* vector pair to be reduced                       */
  int    num_error=0;     /* flag that signals that requested and obtained
                             numbers of threads are the same                 */

/*****************************************************************************
** process and test input parameters    
******************************************************************************/

  if (argc != 4 && argc != 5){
    printf("Usage:     %s <# threads> <# iterations> <vector length> ", *argv);
    printf("[<alghorithm>]\n");
    printf("Algorithm: linear, binary-barrier, binary-p2p, or long-optimal\n");
    return(EXIT_FAILURE);
  }

  /* Take number of threads to request from command line                     */
  nthread_input = atoi(*++argv); 

  if ((nthread_input < 1) || (nthread_input > MAX_THREADS)) {
    printf("ERROR: Invalid number of threads: %d\n", nthread_input);
    exit(EXIT_FAILURE);
  }

  omp_set_num_threads(nthread_input);

  iterations = atoi(*++argv);
  if (iterations < 1){
    printf("ERROR: Iterations must be positive : %d \n", iterations);
    exit(EXIT_FAILURE);
  }

  vector_length  = atol(*++argv);
  if (vector_length < 1){
    printf("ERROR: vector length must be >= 1 : %d \n",vector_length);
    exit(EXIT_FAILURE);
  }

  total_length = 2*nthread_input*sizeof(double)*vector_length;
  vector = (double *) malloc(total_length);
  if (!vector) {
    printf("Could not allocate space for vectors\n");
    exit(EXIT_FAILURE);
  }

  algorithm = "binary-p2p";
  if (argc == 5) algorithm = *++argv;

  intalgorithm = NONE;
  if (!strcmp(algorithm,"linear"        )) intalgorithm = LINEAR;
  if (!strcmp(algorithm,"binary-barrier")) intalgorithm = BINARY_BARRIER;
  if (!strcmp(algorithm,"binary-p2p"    )) intalgorithm = BINARY_P2P;
  if (!strcmp(algorithm,"long-optimal"  )) intalgorithm = LONG_OPTIMAL;
  if (intalgorithm == NONE) {
    printf("Wrong algorithm: %s; choose linear, binary-barrier, ", algorithm);
    printf("binary-p2p, or long-optimal\n");
    exit(EXIT_FAILURE);
  }
  else {
    if (nthread_input == 1) intalgorithm = LOCAL;
  }

  #pragma omp parallel private(i, old_size, group_size, my_ID, iter, start, end, \
                               segment_size, stage, id, my_donor, my_segment) 
  {

  my_ID = omp_get_thread_num();

  #pragma omp master 
  {
  nthread = omp_get_num_threads();

  printf("OpenMP Vector Reduction\n");
  if (nthread != nthread_input) {
    num_error = 1;
    printf("ERROR: number of requested threads %d does not equal ",
           nthread_input);
    printf("number of spawned threads %d\n", nthread);
  } 
  else {
    printf("Number of threads              = %d\n",nthread_input);
    printf("Vector length                  = %d\n", vector_length);
    printf("Reduction algorithm            = %s\n", algorithm);
    printf("Number of iterations           = %d\n", iterations);
  }
  }
  bail_out(num_error);

  for (iter=0; iter<=iterations; iter++) {

    /* start timer after a warmup iteration                                        */
    if (iter == 1) { 
      #pragma omp barrier
      #pragma omp master
      {
        reduce_time = wtime();
      }
    }

    /* in case of the long-optimal algorithm we need a barrier before the
       reinitialization to make sure that we don't overwrite parts of the
       vector before other threads are done with those parts                 */
    if (intalgorithm == LONG_OPTIMAL) {
      #pragma omp barrier
    }

    /* initialize the arrays, assuming first-touch memory placement          */
    for (i=0; i<vector_length; i++) {
      VEC0(my_ID,i) = (double)(my_ID+1);
      VEC1(my_ID,i) = (double)(my_ID+1+nthread);
    }
   
    if (intalgorithm == BINARY_P2P) {
      /* we need a barrier before setting all flags to zero, to avoid 
         zeroing some that are still in use in a previous iteration          */
      #pragma omp barrier
      flag(my_ID) = 0;

      /* we also need a barrier after setting the flags, to make each is
         visible to all threads, and to synchronize before the timer starts  */
      #pragma omp barrier
    }    

    /* do actual reduction                                                   */

    /* first do the "local" part, which is the same for all algorithms       */
    for (i=0; i<vector_length; i++) {
      VEC0(my_ID,i) += VEC1(my_ID,i);
    }

    /* now do the "non-local" part                                           */

    switch (intalgorithm) {

    case LOCAL:  
       break;

    case LINEAR:

       #pragma omp barrier
       #pragma omp master
       {
       for (id=1; id<nthread; id++) {
         for (i=0; i<vector_length; i++) {
           VEC0(0,i) += VEC0(id,i);
         }
       }
       }
       break;

    case BINARY_BARRIER:

      group_size = nthread;

      while (group_size >1) {
        /* barrier to make sure threads have completed their updates before
            the results are being read                                       */
        #pragma omp barrier
        old_size = group_size;
        group_size = (group_size+1)/2;

        /* Threads in "first half" of group aggregate data from threads in 
           second half; must make sure the counterpart is within old group. 
           If group size is odd, the last thread in the group does not have 
            a counterpart.                                                   */
        if (my_ID < group_size && my_ID+group_size<old_size) {
          for (i=0; i<vector_length; i++) {
            VEC0(my_ID,i) += VEC0(my_ID+group_size,i);
          }
        }
      }
      break;

    case BINARY_P2P:

      group_size = nthread;

      while (group_size >1) {

        old_size = group_size;
        group_size = (group_size+1)/2;

        /* synchronize between each pair of threads that collaborate to 
           aggregate a new subresult, to make sure the donor of the pair has 
           updated its vector in the previous round before it is being read  */
        if (my_ID < group_size && my_ID+group_size<old_size) {
          while (flag(my_ID+group_size) == 0) {
            #pragma omp flush
          }
          /* make sure I read the latest version of vector from memory       */
          #pragma omp flush 
          for (i=0; i<vector_length; i++) {
            VEC0(my_ID,i) += VEC0(my_ID+group_size,i);
          }
        }
        else {
          if (my_ID < old_size) {
            /* I am a producer of data in this iteration; make sure my 
               updated version can be seen by all threads                    */
            flag(my_ID) = 1;
            #pragma omp flush
          }
        }
      }
      break;

    case LONG_OPTIMAL:

      /* compute starts and ends of subvectors to be passed among threads    */
      segment_size = (vector_length+nthread-1)/nthread;
      for (id=0; id<nthread; id++) {
        start[id] = segment_size*id;
        end[id]   = MIN(vector_length,segment_size*(id+1));
      }

      /* first do the Bucket Reduce Scatter in nthread-1 stages              */
      my_donor   = (my_ID-1+nthread)%nthread;
      for (stage=1; stage<nthread; stage++) {
        #pragma omp barrier
        my_segment = (my_ID-stage+nthread)%nthread;
        for (i=start[my_segment]; i<end[my_segment]; i++) {
          VEC0(my_ID,i) += VEC0(my_donor,i);
        }
      }
      /* next, each thread pushes its contribution into the master thread 
         vector; no need to synchronize, because of the push model           */
      my_segment = (my_ID+1)%nthread;
      if (my_ID != 0)
        for (i=start[my_segment]; i<end[my_segment]; i++) {
          VEC0(0,i) = VEC0(my_ID,i);
      }
      break;

    } /* end of algorithm switch statement                                   */

  } /* end of iter loop                                                      */

  #pragma omp barrier
  #pragma omp master
  {
    reduce_time = wtime() - reduce_time;
  }


  } /* end of OpenMP parallel region                                         */

  /* verify correctness */
  element_value = (double)nthread*(2.0*(double)nthread+1.0);

  for (i=0; i<vector_length; i++) {
    if (ABS(VEC0(0,i) - element_value) >= epsilon) {
       printf("First error at i=%d; value: %lf; reference value: %lf\n",
              i, VEC0(0,i), element_value);
       exit(EXIT_FAILURE);
    }
  }

  printf("Solution validates\n");
#ifdef VERBOSE
  printf("Element verification value: %lf\n", element_value);
#endif
  avgtime = reduce_time/iterations;
  printf("Rate (MFlops/s): %lf  Avg time (s): %lf\n",
         1.0E-06 * (2.0*nthread-1.0)*vector_length/avgtime, avgtime);

  exit(EXIT_SUCCESS);
}
