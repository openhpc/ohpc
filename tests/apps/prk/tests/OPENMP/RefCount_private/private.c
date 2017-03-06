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

NAME:    RefCount

PURPOSE: This program tests the efficiency of exclusive access to a
         pair of non-adjacent private reference counters
  
USAGE:   The program takes as input the total number of times each
         thread updates its the reference counters, and the number of 
         threads involved.

               <progname>  <# threads><# iterations>
  
         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.

FUNCTIONS CALLED:

         Other than OpenMP or standard C functions, the following 
         functions are used in this program:

         wtime()
         bail_out()

HISTORY: Written by Rob Van der Wijngaart, January 2006.
  
*******************************************************************/

#include <par-res-kern_general.h>
#include <par-res-kern_omp.h>

/* choose a default cache line size (bytes); not that if we do not undefine 
   before redefining the variable, we get a warning                              */
#if !defined(LINE_LENGTH) || LINE_LENGTH < 1
  #undef LINE_LENGTH
  #define LINE_LENGTH  128
#endif

int main(int argc, char ** argv)
{
  int        iterations;      /* number of reference counter updates per thread */
  int        iter, thread;
  int        line_fit;        /* indicates that elements fit on different lines */
  s64Int     alloc_unit;      /* amount of space reserved per element           */
  s64Int     **pcounter1,
             **pcounter2;     /* pointers to pointers to space for counters     */
  s64Int     *counter_space;  /* pointer to space reserved for counters         */
  omp_lock_t **counter_lock;   
  omp_lock_t *lock_space;     /* pointer to space reserved for lock handles     */
  double     refcount_time;   /* timing parameter                               */
  int        nthread_input,
             nthread;         /* number of threads requested and used           */
  s64Int     global_counter1,
             global_counter2;
  int        num_error=0;     /* flag that signals that requested and obtained
                                 numbers of threads are the same                */

/*********************************************************************
** process and test input parameters    
*********************************************************************/

  if (argc != 3){
    printf("Usage: %s <# threads> <# counter pair updates>\n", *argv);
    exit(EXIT_FAILURE);
  }

  nthread_input = atoi(*++argv);
  if ((nthread_input < 1) || (nthread_input > MAX_THREADS)) {
    printf("ERROR: Invalid number of threads: %d\n", nthread_input);
    exit(EXIT_FAILURE);
  }

  omp_set_num_threads(nthread_input);

  iterations  = atoi(*++argv);
  if (iterations < 1){
    printf("ERROR: iterations must be >= 1 : %d \n",iterations);
    exit(EXIT_FAILURE);
  }

  /* allocate the locks on which we will be pounding; we
     put them on different cache lines, if possible                    */

  counter_lock = (omp_lock_t **) malloc(nthread_input*sizeof(omp_lock_t *));
  if (!counter_lock) {
    printf("Not able to allocate memory for counter lock handles\n");
    exit(EXIT_FAILURE);
  }

  /* we try to put counter locks on different cache lines              */
  line_fit = 1;
  if (sizeof(omp_lock_t) > LINE_LENGTH)
    alloc_unit = (sizeof(omp_lock_t)/LINE_LENGTH+1)*LINE_LENGTH;
  else
    alloc_unit = LINE_LENGTH;

  lock_space = (omp_lock_t *) malloc(2*nthread_input*alloc_unit);
  while (!lock_space && alloc_unit >= 2*sizeof(s64Int)) {
    line_fit = 0;
    alloc_unit/=2;
    lock_space = (omp_lock_t *)malloc(2*nthread_input*alloc_unit);
  }
  if (!lock_space) {
    printf("Not able to allocate memory for lock handles\n");
    exit(EXIT_FAILURE);
  }
 
#ifdef VERBOSE
  if (!line_fit) printf("Lock handles do not fit on separate cache lines\n");      
  else           printf("Lock handles fit on separate cache lines\n");      
#endif
      
  /* we try to put private counters on different cache lines           */
  pcounter1 = (s64Int **) malloc(nthread_input*sizeof(s64Int *));
  pcounter2 = (s64Int **) malloc(nthread_input*sizeof(s64Int *));
  if (!pcounter1 || !pcounter2) {
    printf("Not able to allocate memory for reference counter pointers\n");
    exit(EXIT_FAILURE);
  }

  line_fit = 1;
  if (sizeof(s64Int) > LINE_LENGTH)
    alloc_unit = (sizeof(s64Int)/LINE_LENGTH+1)*LINE_LENGTH;
  else
    alloc_unit = LINE_LENGTH;
  counter_space = malloc(2*nthread_input*alloc_unit);
  while (!counter_space && alloc_unit >= 2*sizeof(s64Int)) {
    line_fit = 0;
    alloc_unit/=2;
    counter_space = malloc(2*nthread_input*alloc_unit);
  }
  if (!counter_space) {
    printf("Not able to allocate memory for reference counters\n");
    exit(EXIT_FAILURE);
  }
#ifdef VERBOSE
  printf("Cache line size used: %d bytes\n", LINE_LENGTH);
  if (!line_fit) printf("Counters do not fit on separate cache lines\n");      
  else           printf("Counters fit on separate cache lines\n");      
#endif

  #pragma omp parallel 
  {

  int my_ID;      /* Thread ID                                          */
  int iter;       /* dummy                                              */

  my_ID = omp_get_thread_num();

  /* for maximum performance we rely on first touch and parallel initialization */

  /* each thread initializes its own counters */
  pcounter1[my_ID] = (s64Int *)((char *)counter_space + my_ID*2*alloc_unit);
  pcounter2[my_ID] = (s64Int *)((char *)counter_space + (my_ID*2+1)*alloc_unit);
  *(pcounter1[my_ID]) = 0;
  *(pcounter2[my_ID]) = 0;

  /* each thread initializes its own lock  */
  counter_lock[my_ID] = (omp_lock_t *)((char *)lock_space + my_ID*alloc_unit);
  omp_init_lock(counter_lock[my_ID]);

  #pragma omp master
  {
  nthread = omp_get_num_threads();
  printf("OpenMP exclusive access test RefCount, private counters\n");
  if (nthread != nthread_input) {
    num_error = 1;
    printf("ERROR: number of requested threads %d does not equal ",
           nthread_input);
    printf("number of spawned threads %d\n", nthread);
  } 
  else {
    printf("Number of threads              = %i;\n",nthread_input);
    printf("Number of counter pair updates = %i\n", iterations);
#ifndef LOCK
    printf("Not ");
#endif
    printf("Using (uncontended) locks\n");
  }
  }
  bail_out(num_error);

  #pragma omp master
  {
  refcount_time = wtime();
  }

  #pragma omp for 
  for (iter=0; iter<iterations; iter++) { 
#ifdef LOCK
    omp_set_lock(counter_lock[my_ID]);
#endif
    (*(pcounter1[my_ID]))++;
    (*(pcounter2[my_ID]))++;
#ifdef LOCK
    omp_unset_lock(counter_lock[my_ID]);
#endif
  }

  #pragma omp master
  {
  refcount_time = (wtime() - refcount_time);
  }     

  } /* end of OpenMP parallel region */

  global_counter1 = global_counter2 = 0;
  for (thread=0; thread<nthread; thread++) {
    global_counter1 += *(pcounter1[thread]);
    global_counter2 += *(pcounter2[thread]);
  }

  if (global_counter1 != iterations || global_counter1 != global_counter2 ) {
    printf("Incorrect or inconsistent counter sum values: "FSTR64U", "FSTR64U";",
           global_counter1, global_counter2);
    printf(" should be %d\n", iterations);
    exit(EXIT_FAILURE);
  }
  else {
#ifdef VERBOSE
    printf("Solution validates; Correct counter value sum of "FSTR64"\n", 
           global_counter1);
#else 
    printf("Solution validates\n");
#endif
    printf("Rate (MCPUPs/s): %lf, time (s): %lf\n",
           iterations/refcount_time*1.e-6, refcount_time);
  }
  exit(EXIT_SUCCESS);
}
