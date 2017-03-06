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

NAME:    Stencil

PURPOSE: This program tests the efficiency with which a space-invariant,
         linear, symmetric filter (stencil) can be applied to a square
         grid or image.
  
USAGE:   The program takes as input the number of threads, the linear
         dimension of the grid, and the number of iterations on the grid

               <progname> <# threads> <iterations> <grid size> 
  
         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.

FUNCTIONS CALLED:

         Other than OpenMP or standard C functions, the following 
         functions are used in this program:

         wtime()
         bail_out()

HISTORY: - Written by Rob Van der Wijngaart, November 2006.
         - RvdW: Removed unrolling pragmas for clarity;
           added constant to array "in" at end of each iteration to force 
           refreshing of neighbor data in parallel versions; August 2013
  
*******************************************************************/

#include <par-res-kern_general.h>
#include <par-res-kern_omp.h>

#ifndef RADIUS
  #define RADIUS 2
#endif

#ifdef DOUBLE
  #define DTYPE   double
  #define EPSILON 1.e-8
  #define COEFX   1.0
  #define COEFY   1.0
  #define FSTR    "%lf"
#else
  #define DTYPE   float
  #define EPSILON 0.0001f
  #define COEFX   1.0f
  #define COEFY   1.0f
  #define FSTR    "%f"
#endif

/* define shorthand for indexing a multi-dimensional array                       */
#define IN(i,j)       in[i+(j)*(n)]
#define OUT(i,j)      out[i+(j)*(n)]
#define WEIGHT(ii,jj) weight[ii+RADIUS][jj+RADIUS]

int main(int argc, char ** argv) {

  int    n;               /* linear grid dimension                               */
  int    i, j, ii, jj, it, jt, iter;  /* dummies                                 */
  DTYPE  norm,            /* L1 norm of solution                                 */
         reference_norm;
  DTYPE  f_active_points; /* interior of grid with respect to stencil            */
  DTYPE  flops;           /* floating point ops per iteration                    */
  int    iterations;      /* number of times to run the algorithm                */
  double stencil_time,    /* timing parameters                                   */
         avgtime;
  int    stencil_size;    /* number of points in stencil                         */
  int    nthread_input,   /* thread parameters                                   */
         nthread; 
  DTYPE  * RESTRICT in;   /* input grid values                                   */
  DTYPE  * RESTRICT out;  /* output grid values                                  */
  long   total_length;    /* total required length to store grid values          */
  int    num_error=0;     /* flag that signals that requested and obtained
                             numbers of threads are the same                     */
  DTYPE  weight[2*RADIUS+1][2*RADIUS+1]; /* weights of points in the stencil     */

  /*******************************************************************************
  ** process and test input parameters    
  ********************************************************************************/

  if (argc != 4){
    printf("Usage: %s <# threads> <# iterations> <array dimension>\n", 
           *argv);
    return(EXIT_FAILURE);
  }

  /* Take number of threads to request from command line */
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

  n  = atoi(*++argv);

  if (n < 1){
    printf("ERROR: grid dimension must be positive: %d\n", n);
    exit(EXIT_FAILURE);
  }

  if (RADIUS < 1) {
    printf("ERROR: Stencil radius %d should be positive\n", RADIUS);
    exit(EXIT_FAILURE);
  }

  if (2*RADIUS +1 > n) {
    printf("ERROR: Stencil radius %d exceeds grid size %d\n", RADIUS, n);
    exit(EXIT_FAILURE);
  }

  /*  make sure the vector space can be represented                             */
  total_length = n*n*sizeof(DTYPE);

  in  = (DTYPE *) malloc(total_length);
  out = (DTYPE *) malloc(total_length);
  if (!in || !out) {
    printf("ERROR: could not allocate space for input or output array\n");
    exit(EXIT_FAILURE);
  }

  /* fill the stencil weights to reflect a discrete divergence operator         */
  for (jj=-RADIUS; jj<=RADIUS; jj++) for (ii=-RADIUS; ii<=RADIUS; ii++)
    WEIGHT(ii,jj) = (DTYPE) 0.0;
#ifdef STAR
  stencil_size = 4*RADIUS+1;
  for (ii=1; ii<=RADIUS; ii++) {
    WEIGHT(0, ii) = WEIGHT( ii,0) =  (DTYPE) (1.0/(2.0*ii*RADIUS));
    WEIGHT(0,-ii) = WEIGHT(-ii,0) = -(DTYPE) (1.0/(2.0*ii*RADIUS));
  }
#else
  stencil_size = (2*RADIUS+1)*(2*RADIUS+1);
  for (jj=1; jj<=RADIUS; jj++) {
    for (ii=-jj+1; ii<jj; ii++) {
      WEIGHT(ii,jj)  =  (DTYPE) (1.0/(4.0*jj*(2.0*jj-1)*RADIUS));
      WEIGHT(ii,-jj) = -(DTYPE) (1.0/(4.0*jj*(2.0*jj-1)*RADIUS));
      WEIGHT(jj,ii)  =  (DTYPE) (1.0/(4.0*jj*(2.0*jj-1)*RADIUS));
      WEIGHT(-jj,ii) = -(DTYPE) (1.0/(4.0*jj*(2.0*jj-1)*RADIUS));      
    }
    WEIGHT(jj,jj)    =  (DTYPE) (1.0/(4.0*jj*RADIUS));
    WEIGHT(-jj,-jj)  = -(DTYPE) (1.0/(4.0*jj*RADIUS));
  }
#endif  

  norm = (DTYPE) 0.0;
  f_active_points = (DTYPE) (n-2*RADIUS)*(DTYPE) (n-2*RADIUS);

  #pragma omp parallel private(i, j, ii, jj, it, jt, iter) 
  {

  #pragma omp master
  {
  nthread = omp_get_num_threads();

  printf("OpenMP stencil execution on 2D grid\n");
  if (nthread != nthread_input) {
    num_error = 1;
    printf("ERROR: number of requested threads %d does not equal ",
           nthread_input);
    printf("number of spawned threads %d\n", nthread);
  } 
  else {
    printf("Number of threads    = %d\n",nthread_input);
    printf("Grid size            = %d\n", n);
    printf("Radius of stencil    = %d\n", RADIUS);
    printf("Number of iterations = %d\n", iterations);
#ifdef STAR
    printf("Type of stencil      = star\n");
#else
    printf("Type of stencil      = compact\n");
#endif
#ifdef DOUBLE
    printf("Data type            = double precision\n");
#else
    printf("Data type            = single precision\n");
#endif
#ifndef PARALLELFOR
    printf("Parallel regions     = fused (omp for)\n");
#else
    printf("Parallel regions     = split (omp parallel for)\n");
#endif
  }
  }
  bail_out(num_error);

#ifdef PARALLELFOR
} 
#endif

  /* intialize the input and output arrays                                     */
#ifdef PARALLELFOR
  #pragma omp parallel for private(i)
#else
  #pragma omp for
#endif
  for (j=0; j<n; j++) for (i=0; i<n; i++) 
    IN(i,j) = COEFX*i+COEFY*j;
#ifdef PARALLELFOR
  #pragma omp parallel for private(i)
#else
  #pragma omp for
#endif
  for (j=RADIUS; j<n-RADIUS; j++) for (i=RADIUS; i<n-RADIUS; i++) 
    OUT(i,j) = (DTYPE)0.0;

  for (iter = 0; iter<=iterations; iter++){

    /* start timer after a warmup iteration                                        */
    if (iter == 1) { 
#ifndef PARALLELFOR
      #pragma omp barrier
      #pragma omp master
#endif
      {   
        stencil_time = wtime();
      }
    }

#ifdef PARALLELFOR
    #pragma omp parallel for private(i, ii, jj)
#else
    #pragma omp for
#endif
    for (j=RADIUS; j<n-RADIUS; j++) {
      for (i=RADIUS; i<n-RADIUS; i++) {
#ifdef STAR
        for (jj=-RADIUS; jj<=RADIUS; jj++)  OUT(i,j) += WEIGHT(0,jj)*IN(i,j+jj);
        for (ii=-RADIUS; ii<0; ii++)        OUT(i,j) += WEIGHT(ii,0)*IN(i+ii,j);
        for (ii=1; ii<=RADIUS; ii++)        OUT(i,j) += WEIGHT(ii,0)*IN(i+ii,j);
#else
        /* would like to be able to unroll this loop, but compiler will ignore  */
        for (jj=-RADIUS; jj<=RADIUS; jj++) 
        for (ii=-RADIUS; ii<=RADIUS; ii++)  OUT(i,j) += WEIGHT(ii,jj)*IN(i+ii,j+jj);
#endif
      }
    }

    /* add constant to solution to force refresh of neighbor data, if any       */
#ifdef PARALLELFOR
    #pragma omp parallel for private(i)
#else
    #pragma omp for
#endif
    for (j=0; j<n; j++) for (i=0; i<n; i++) IN(i,j)+= 1.0;
  } /* end of iterations                                                        */

#ifndef PARALLELFOR
  #pragma omp barrier
  #pragma omp master
#endif
  {
    stencil_time = wtime() - stencil_time;
  }

  /* compute L1 norm in parallel                                                */
#ifdef PARALLELFOR
  #pragma omp parallel for reduction(+:norm), private (i)
#else
  #pragma omp for reduction(+:norm)
#endif
  for (j=RADIUS; j<n-RADIUS; j++) for (i=RADIUS; i<n-RADIUS; i++) {
    norm += (DTYPE)ABS(OUT(i,j));
  }
#ifndef PARALLELFOR
  } /* end of OPENMP parallel region                                             */
#endif

  norm /= f_active_points;

  /*******************************************************************************
  ** Analyze and output results.
  ********************************************************************************/

/* verify correctness                                                            */
  reference_norm = (DTYPE) (iterations+1) * (COEFX + COEFY);
  if (ABS(norm-reference_norm) > EPSILON) {
    printf("ERROR: L1 norm = "FSTR", Reference L1 norm = "FSTR"\n",
           norm, reference_norm);
    exit(EXIT_FAILURE);
  }
  else {
    printf("Solution validates\n");
#ifdef VERBOSE
    printf("Reference L1 norm = "FSTR", L1 norm = "FSTR"\n", 
           reference_norm, norm);
#endif
  }

  flops = (DTYPE) (2*stencil_size+1) * f_active_points;
  avgtime = stencil_time/iterations;
  printf("Rate (MFlops/s): "FSTR"  Avg time (s): %lf\n",
         1.0E-06 * flops/avgtime, avgtime);

  exit(EXIT_SUCCESS);
}
