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

/*********************************************************************************

NAME:    dgemm

PURPOSE: This program tests the efficiency with which a dense matrix
         dense multiplication is carried out
  
USAGE:   The program takes as input the number of threads, the matrix
         order, the number of times the matrix-matrix multiplication 
         is carried out, and, optionally, a tile size for matrix
         blocking

         <progname> <# threads> <# iterations> <matrix order> [<tile size>]
  
         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.

FUNCTIONS CALLED:

         Other than OpenMP or standard C functions, the following 
         functions are used in this program:

         wtime()
         bail_out()

HISTORY: Written by Rob Van der Wijngaart, September 2006.
         Made array dimensioning dynamic, October 2007
         Allowed arbitrary block size, November 2007
         Removed reverse-engineered MKL source code option, November 2007
         Changed from row- to column-major storage order, November 2007
         Stored blocks of B in transpose form, November 2007
  
***********************************************************************************/

#include <par-res-kern_general.h>
#include <par-res-kern_omp.h>

#ifdef MKL
  #include <mkl_cblas.h>
#endif

#ifndef DEFAULTBLOCK
  #define DEFAULTBLOCK 32
#endif

#ifndef BOFFSET
  #define BOFFSET 12
#endif
#define AA_arr(i,j) AA[(i)+(block+BOFFSET)*(j)]
#define BB_arr(i,j) BB[(i)+(block+BOFFSET)*(j)]
#define CC_arr(i,j) CC[(i)+(block+BOFFSET)*(j)]
#define  A_arr(i,j)  A[(i)+(order)*(j)]
#define  B_arr(i,j)  B[(i)+(order)*(j)]
#define  C_arr(i,j)  C[(i)+(order)*(j)]

#define forder (1.0*order)

main(int argc, char **argv){

  int     iter, i,ii,j,jj,k,kk,ig,jg,kg; /* dummies                               */
  int     iterations;           /* number of times the multiplication is done     */
  double  dgemm_time,           /* timing parameters                              */
          avgtime;
  double  checksum = 0.0,       /* checksum of result                             */
          ref_checksum;
  double  epsilon = 1.e-8;      /* error tolerance                                */
  int     nthread_input,        /* thread parameters                              */
          nthread;   
  int     num_error=0;          /* flag that signals that requested and 
                                   obtained numbers of threads are the same       */
  static  
  double *A, *B, *C;            /* input (A,B) and output (C) matrices            */
  int     order;                /* number of rows and columns of matrices         */
  int     block;                /* tile size of matrices                          */

#ifndef MKL  
  if (argc != 4 && argc != 5) {
    printf("Usage: %s <# threads> <# iterations> <matrix order> [tile size]\n",*argv);
#else
  if (argc != 4) {
    printf("Usage: %s <# threads> <# iterations> <matrix order>\n",*argv);
#endif
    exit(EXIT_FAILURE);
  }

  /* Take number of threads to request from command line                          */
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

  order = atoi(*++argv);
  if (order < 1) {
    printf("ERROR: Matrix order must be positive: %d\n", order);
    exit(EXIT_FAILURE);
  }
  A = (double *) malloc(order*order*sizeof(double));
  B = (double *) malloc(order*order*sizeof(double));
  C = (double *) malloc(order*order*sizeof(double));
  if (!A || !B || !C) {
    printf("ERROR: Could not allocate space for global matrices\n");
    exit(EXIT_FAILURE);
  }

  ref_checksum = (0.25*forder*forder*forder*(forder-1.0)*(forder-1.0));

  #pragma omp parallel for private(i,j) 
  for(j = 0; j < order; j++) for(i = 0; i < order; i++) {
    A_arr(i,j) = B_arr(i,j) = (double) j; 
    C_arr(i,j) = 0.0;
  }

  printf("OpenMP Dense matrix-matrix multiplication\n");

#ifndef MKL
  if (argc == 5) {
         block = atoi(*++argv);
  } else block = DEFAULTBLOCK;

  #pragma omp parallel private (i,j,k,ii,jj,kk,ig,jg,kg,iter)
  {
  double *AA, *BB, *CC;

  if (block > 0) {
    /* matrix blocks for local temporary copies                                     */
    AA = (double *) malloc(block*(block+BOFFSET)*3*sizeof(double));
    if (!AA) {
      num_error = 1;
      printf("Could not allocate space for matrix tiles on thread %d\n", 
             omp_get_thread_num());
    }
    bail_out(num_error);
    BB = AA + block*(block+BOFFSET);
    CC = BB + block*(block+BOFFSET);
  } 

  #pragma omp master 
  {
  nthread = omp_get_num_threads();

  if (nthread != nthread_input) {
    num_error = 1;
    printf("ERROR: number of requested threads %d does not equal ",
           nthread_input);
    printf("number of spawned threads %d\n", nthread);
  } 
  else {
    printf("Matrix order          = %d\n", order);
    printf("Number of threads     = %d\n", nthread_input);
    if (block>0)
      printf("Blocking factor       = %d\n", block);
    else
      printf("No blocking\n");
    printf("Number of iterations  = %d\n", iterations);
  }
  }
  bail_out(num_error); 

  for (iter=0; iter<=iterations; iter++) {

    if (iter==1) {
      #pragma omp barrier
      #pragma omp master
      {
        dgemm_time = wtime();
      }
    }


    if (block > 0) {
  
      #pragma omp for 
      for(jj = 0; jj < order; jj+=block){
        for(kk = 0; kk < order; kk+=block) {
  
          for (jg=jj,j=0; jg<MIN(jj+block,order); j++,jg++) 
          for (kg=kk,k=0; kg<MIN(kk+block,order); k++,kg++) 
            BB_arr(j,k) =  B_arr(kg,jg);
  
          for(ii = 0; ii < order; ii+=block){
  
            for (kg=kk,k=0; kg<MIN(kk+block,order); k++,kg++)
            for (ig=ii,i=0; ig<MIN(ii+block,order); i++,ig++)
              AA_arr(i,k) = A_arr(ig,kg);
  
            for (jg=jj,j=0; jg<MIN(jj+block,order); j++,jg++) 
            for (ig=ii,i=0; ig<MIN(ii+block,order); i++,ig++)
              CC_arr(i,j) = 0.0;
         
            for (kg=kk,k=0; kg<MIN(kk+block,order); k++,kg++)
            for (jg=jj,j=0; jg<MIN(jj+block,order); j++,jg++) 
            for (ig=ii,i=0; ig<MIN(ii+block,order); i++,ig++)
              CC_arr(i,j) += AA_arr(i,k)*BB_arr(j,k);
  
            for (jg=jj,j=0; jg<MIN(jj+block,order); j++,jg++) 
            for (ig=ii,i=0; ig<MIN(ii+block,order); i++,ig++)
              C_arr(ig,jg) += CC_arr(i,j);
  
          }
        }  
      }
    }
    else {
      #pragma omp for 
      for (jg=0; jg<order; jg++) 
      for (kg=0; kg<order; kg++) 
      for (ig=0; ig<order; ig++) 
        C_arr(ig,jg) += A_arr(ig,kg)*B_arr(kg,jg);
    }

  } /* end of iterations                                                          */

  #pragma omp barrier
  #pragma omp master
  {
    dgemm_time = wtime() - dgemm_time;
  }

  } /* end of parallel region                                                     */

#else

  printf("Matrix size           = %dx%d\n", order, order);
  printf("Number of threads     = %d\n", nthread_input);
  printf("Using Math Kernel Library\n");
  printf("Number of iterations  = %d\n", iterations);

  for (iter=0; iter<=iterations; iter++) {

    if (iter==1) dgemm_time = wtime();

    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, order, order, 
                order, 1.0, &(A_arr(0,0)), order, &(B_arr(0,0)), order, 
                1.0, &(C_arr(0,0)), order);
  }
  dgemm_time = wtime()-dgemm_time;
#endif

  for(checksum=0.0,j = 0; j < order; j++) for(i = 0; i < order; i++)
    checksum += C_arr(i,j);

  /* verification test                                                            */
  ref_checksum *= (iterations+1);

  if (ABS((checksum - ref_checksum)/ref_checksum) > epsilon) {
    printf("ERROR: Checksum = %lf, Reference checksum = %lf\n",
           checksum, ref_checksum);
    exit(EXIT_FAILURE);
  }
  else {
    printf("Solution validates\n");
#ifdef VERBOSE
    printf("Reference checksum = %lf, checksum = %lf\n", 
           ref_checksum, checksum);
#endif
  }

  double nflops = 2.0*forder*forder*forder;
  avgtime = dgemm_time/iterations;
  printf("Rate (MFlops/s): %lf  Avg time (s): %lf\n",
         1.0E-06 *nflops/avgtime, avgtime);

  exit(EXIT_SUCCESS);

}
