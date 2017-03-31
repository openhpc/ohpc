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

/*****************************************************************************

NAME:    StopNGo

PURPOSE: This program tests the efficiency of a global synchronization
         on the target system. 
  
USAGE:   The program takes as input the number of times the test of
         string manipulation involving a global synchronization is
         carried out, as well as an input string and the number of
         threads involved.

         <progname>  <# threads> <# iterations> <length of numerical string>
  
         The user can avoid false sharing if the string, disregarding
         the string terminator, fits within an exact multiple of a cache 
         line.

         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.

         Compile with VERBOSE defined if you want lots of output.

FUNCTIONS CALLED:

         Other than OpenMP or standard C functions, the following 
         functions are used in this program:

         wtime()
         bail_out()
         chartoi()

HISTORY: Written by Rob Van der Wijngaart, December 2005.
  
*****************************************************************************/

#include <par-res-kern_general.h>
#include <par-res-kern_omp.h>

#define EOS '\0'

static int chartoi(char c) {
  /* define short string; second character contains string terminator       */
  char letter[2]="0";
  letter[0]=c;
  return (atoi(letter));
}


int main(int argc, char ** argv)
{
  int    my_ID;         /* Thread ID                                        */
  int    iterations;    /* number of times to rehash strings                */
  int    i, iter;       /* dummy                                            */
  int    checksum;      /* computed checksum of final aggregate string      */
  char   *scramble = "27638472638746283742712311207892";
  char   *basestring;   /* initial string to be copied to private strings   */
  char   *iterstring;   /* private copy of string                           */
  char   *catstring;    /* concatenated, scrambled string                   */
  long   length;        /* length of scramble string                        */
  long   thread_length; /* string length per thread                         */
  int    basesum;       /* checksum of base string                          */
  double stopngo_time;  /* timing parameter                                 */
  int    nthread_input, /* thread parameters                                */
         nthread; 
  int    num_error=0;   /* flag that signals that requested and obtained
                             numbers of threads are the same                */

  /**************************************************************************
  ** process, and test input parameter
  ***************************************************************************/

  if (argc != 4){
     printf("Usage: %s <# threads> <# iterations> <scramble string length>\n", *argv);
     exit(EXIT_FAILURE);
  }

  /* Take number of threads to request from command line                    */
  nthread_input = atoi(*++argv); 

  if ((nthread_input < 1) || (nthread_input > MAX_THREADS)) {
    printf("ERROR: Invalid number of threads: %d\n", nthread_input);
    exit(EXIT_FAILURE);
  }

  omp_set_num_threads(nthread_input);

  iterations = atoi(*++argv);
  if(iterations < 1){
     printf("ERROR: iterations must be >= 1 : %d \n",iterations);
     exit(EXIT_FAILURE);
  }

  length = atol(*++argv);
  if (length <nthread_input || length%nthread_input !=0) {
    printf("ERROR: length of string %d must be multiple of # threads: %d\n", 
           length, nthread_input);
    exit(EXIT_FAILURE);
  }
  thread_length = length/nthread_input;

  basestring = malloc((thread_length+1)*sizeof(char));
  if (basestring==NULL) {
    printf("ERROR: Could not allocate space for scramble string\n");
    exit(EXIT_FAILURE);
  }

  /* fill the base string with copies (truncated) of scrable string         */
  for (i=0; i<thread_length; i++) basestring[i]=scramble[i%32];
  basestring[thread_length] = EOS;

  catstring=(char *) malloc((length+1)*sizeof(char));
  if (catstring==NULL) {
    printf("ERROR: Could not allocate space for concatenation string: %d\n",
           length+1);
    exit(EXIT_FAILURE);
  }

  /* initialize concatenation string with nonsense                          */
  for (i=0; i<length; i++) catstring[i]='9';
  catstring[length]=EOS;

  #pragma omp parallel private(iterstring, my_ID, i, iter)
  {

  my_ID = omp_get_thread_num();

  /* everybody receives a private copy of the base string                   */
  iterstring = (char *) malloc((thread_length+1)*sizeof(char));
  if (!iterstring) {
    printf("ERROR: Thread %d could not allocate space for private string\n", 
           my_ID);
    num_error = 1;
  }
  bail_out(num_error);

  strcpy(iterstring, basestring);

  #pragma omp master
  {
  nthread = omp_get_num_threads();

  printf("OpenMP global synchronization test\n");
  if (nthread != nthread_input) {
    num_error = 1;
    printf("ERROR: number of requested threads %d does not equal ",
           nthread_input);
    printf("number of spawned threads %d\n", nthread);
  } 
  else {
    printf("Number of threads              = %d;\n",nthread_input);
    printf("Length of scramble string      = %ld\n", length);
    printf("Number of iterations           = %d\n", iterations);
  }
  }
  bail_out(num_error);

  #pragma omp master 
  {
  stopngo_time = wtime();
  }

  for (iter=0; iter<iterations; iter++) { 

    /* we need a barrier to avoid reading catstring before it is complete   */
    #pragma omp barrier
    /* glue all private strings together                                    */
    strncpy(catstring+my_ID*thread_length,iterstring,(size_t) thread_length);

    /* synchronize so we can read the consistent concatenated string        */
    #pragma omp barrier
    /* now all threads select different, nonoverlapping substring           */
    for (i=0; i<thread_length; i++) iterstring[i]=catstring[my_ID+i*nthread];

#ifdef VERBOSE
    #pragma omp master
    {
    checksum=0;
    for (i=0; i<strlen(catstring);i++) checksum+= chartoi(catstring[i]);
    printf("Iteration %d, cat string %s, checksum equals: %d\n", 
            iter, catstring, checksum);
    }
#endif
  }

  #pragma omp master
  {
  stopngo_time = wtime() - stopngo_time;
  }
  } /* end of parallel region                                               */

  /* compute checksum on obtained result, adding all digits in the string   */
  basesum=0;
  for (i=0; i<thread_length; i++) basesum += chartoi(basestring[i]);
  checksum=0;
  for (i=0; i<strlen(catstring);i++) checksum += chartoi(catstring[i]);
  if (checksum != basesum*nthread) {
    printf("Incorrect checksum: %d instead of %d\n", checksum, basesum*nthread);
    exit(EXIT_FAILURE);
  }
  else {
#ifdef VERBOSE
    printf("Solution validates; Correct checksum of %d\n", checksum);
#else
    printf("Solution validates\n");
#endif
  }
  printf("Rate (synch/s): %e, time (s): %lf\n", 
         (((double)iterations)/stopngo_time), stopngo_time);

  exit(EXIT_SUCCESS);
}  /* end of main */

