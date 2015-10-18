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

NAME:    sparse

PURPOSE: This program tests the efficiency with which a sparse matrix
         vector multiplication is carried out
  
USAGE:   The program takes as input the number of threads, the 2log of the linear
         size of the 2D grid (equalling the 2log of the square root of the order
         of the sparse matrix), the radius of the difference stencil, and the number 
         of times the matrix-vector multiplication is carried out.

         <progname> <# threads> <# iterations> <2log root-of-matrix-order> <radius> 
  
         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.

FUNCTIONS CALLED:

         Other than OpenMP or standard C functions, the following 
         functions are used in this program:

         wtime()
         bail_out()
         reverse()

NOTES:   

HISTORY: Written by Rob Van der Wijngaart, August 2006.
         Updated by RvdW to parallelize matrix generation, March 2007.
         Updated by RvdW to fix verification bug, February 2013
         Updated by RvdW to sort matrix elements to reflect traditional CSR storage,
         August 2013
  
***********************************************************************************/

#include <par-res-kern_general.h>
#include <par-res-kern_omp.h>

/* linearize the grid index                                                       */
#define LIN(i,j) (i+((j)<<lsize))

/* if the scramble flag is set, convert all (linearized) grid indices by 
   reversing their bits; if not, leave the grid indices alone                     */
#ifdef SCRAMBLE
  #define REVERSE(a,b)  reverse((a),(b))
#else
  #define REVERSE(a,b) (a)
#endif

#define BITS_IN_BYTE 8

static u64Int reverse(register u64Int, int);
static int compare(const void *el1, const void *el2);

int main(int argc, char **argv){

  int               iter, r;    /* dummies                                        */
  int               lsize;      /* logarithmic linear size of grid                */
  int               lsize2;     /* logarithmic size of grid                       */
  int               size;       /* linear size of grid                            */
  s64Int            size2;      /* matrix order (=total # points in grid)         */
  int               radius,     /* stencil parameters                             */
                    stencil_size; 
  s64Int            row, col, first, last; /* dummies                             */
  s64Int            i, j;       /* dummies                                        */
  int               iterations; /* number of times the multiplication is done     */
  s64Int            elm;        /* sequence number of matrix nonzero              */
  s64Int            nent;       /* number of nonzero entries                      */
  double            sparsity;   /* fraction of non-zeroes in matrix               */
  double            sparse_time,/* timing parameters                              */
                    avgtime;
  double * RESTRICT matrix;     /* sparse matrix entries                          */
  double * RESTRICT vector;     /* vector multiplying the sparse matrix           */
  double * RESTRICT result;     /* computed matrix-vector product                 */
  double            temp;       /* temporary scalar storing reduction data        */
  double            vector_sum; /* checksum of result                             */
  double            reference_sum; /* checksum of "rhs"                           */
  double            epsilon = 1.e-8; /* error tolerance                           */
  s64Int * RESTRICT colIndex;   /* column indices of sparse matrix entries        */
  int               nthread_input,  /* thread parameters                          */
                    nthread;   
  int               num_error=0; /* flag that signals that requested and 
                                   obtained numbers of threads are the same       */
  size_t            vector_space, /* variables used to hold malloc sizes          */
                    matrix_space,
                    index_space;

  if (argc != 5) {
    printf("Usage: %s <# threads> <# iterations> <2log grid size> <stencil radius>\n",*argv);
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

  lsize = atoi(*++argv);
  lsize2 = 2*lsize;
  size = 1<<lsize;
  if (lsize <0) {
    printf("ERROR: Log of grid size must be greater than or equal to zero: %d\n", 
           (int) lsize);
    exit(EXIT_FAILURE);
  }
  /* compute number of points in the grid                                         */
  size2 = size*size;

  radius = atoi(*++argv);
  if (radius <0) {
    printf("ERROR: Stencil radius must be non-negative: %d\n", (int) size);
    exit(EXIT_FAILURE);
  }

  /* emit error if (periodic) stencil overlaps with itself                        */
  if (size <2*radius+1) {
    printf("ERROR: Grid extent %d smaller than stencil diameter 2*%d+1= %d\n",
           size, radius, radius*2+1);
    exit(EXIT_FAILURE);
  }
 
  /* compute total size of star stencil in 2D                                     */
  stencil_size = 4*radius+1;
  /* sparsity follows from number of non-zeroes per row                           */
  sparsity = (double)(4*radius+1)/(double)size2;

  /* compute total number of non-zeroes                                           */
  nent = size2*stencil_size;

  matrix_space = nent*sizeof(double);
  if (matrix_space/sizeof(double) != nent) {
    printf("ERROR: Cannot represent space for matrix: %ul\n", matrix_space);
    exit(EXIT_FAILURE);
  } 

  matrix = (double *) malloc(matrix_space);
  if (!matrix) {
    printf("ERROR: Could not allocate space for sparse matrix: "FSTR64U"\n", nent);
    exit(EXIT_FAILURE);
  } 

  vector_space = 2*size2*sizeof(double);
  if (vector_space/sizeof(double) != 2*size2) {
    printf("ERROR: Cannot represent space for vectors: %ul\n", vector_space);
    exit(EXIT_FAILURE);
  } 

  vector = (double *) malloc(vector_space);
  if (!vector) {
    printf("ERROR: Could not allocate space for vectors: %d\n", (int)(2*size2));
    exit(EXIT_FAILURE);
  }
  result = vector + size2;

  index_space = nent*sizeof(s64Int);
  if (index_space/sizeof(s64Int) != nent) {
    printf("ERROR: Cannot represent space for column indices: %ul\n", index_space);
    exit(EXIT_FAILURE);
  } 
  colIndex = (s64Int *) malloc(index_space);
  if (!colIndex) {
    printf("ERROR: Could not allocate space for column indices: "FSTR64U"\n",
           nent*sizeof(s64Int));
    exit(EXIT_FAILURE);
  } 

  #pragma omp parallel private (row, col, elm, first, last, iter)
  {

  #pragma omp master 
  {
  nthread = omp_get_num_threads();

  printf("OpenMP Sparse matrix-vector multiplication\n");
  if (nthread != nthread_input) {
    num_error = 1;
    printf("ERROR: number of requested threads %d does not equal ",
           nthread_input);
    printf("number of spawned threads %d\n", nthread);
  } 
  else {
    printf("Number of threads     = %16d\n",nthread_input);
    printf("Matrix order          = "FSTR64U"\n", size2);
    printf("Stencil diameter      = %16d\n", 2*radius+1);
    printf("Sparsity              = %16.10lf\n", sparsity);
    printf("Number of iterations  = %16d\n", iterations);
#ifdef SCRAMBLE
    printf("Using scrambled indexing\n");
#else
    printf("Using canonical indexing\n");
#endif
  }
  }
  bail_out(num_error);

  /* initialize the input and result vectors                                      */
  #pragma omp for
  for (row=0; row<size2; row++) result[row] = vector[row] = 0.0;

  /* fill matrix with nonzeroes corresponding to difference stencil. We use the 
     scrambling for reordering the points in the grid.                            */

  #pragma omp for private (i,j,r)
  for (row=0; row<size2; row++) {
    j = row/size; i=row%size;
    elm = row*stencil_size;
    colIndex[elm] = REVERSE(LIN(i,j),lsize2);
    for (r=1; r<=radius; r++, elm+=4) {
      colIndex[elm+1] = REVERSE(LIN((i+r)%size,j),lsize2);
      colIndex[elm+2] = REVERSE(LIN((i-r+size)%size,j),lsize2);
      colIndex[elm+3] = REVERSE(LIN(i,(j+r)%size),lsize2);
      colIndex[elm+4] = REVERSE(LIN(i,(j-r+size)%size),lsize2);
    }
    /* sort colIndex to make sure the compressed row accesses
       vector elements in increasing order                                         */
    qsort(&(colIndex[row*stencil_size]), stencil_size, sizeof(s64Int), compare);
    for (elm=row*stencil_size; elm<(row+1)*stencil_size; elm++)
      matrix[elm] = 1.0/(double)(colIndex[elm]+1);
  }

  for (iter=0; iter<=iterations; iter++) {

    /* start timer after a warmup iteration                                        */
    if (iter == 1) { 
      #pragma omp barrier
      #pragma omp master
      {   
        sparse_time = wtime();
      }
    }

    /* fill vector                                                                */
    #pragma omp for 
    for (row=0; row<size2; row++) vector[row] += (double) (row+1);

    /* do the actual matrix-vector multiplication                                 */
    #pragma omp for
    for (row=0; row<size2; row++) {
      first = stencil_size*row; last = first+stencil_size-1;
      #pragma simd reduction(+:temp) 
      for (temp=0.0,col=first; col<=last; col++) {
        temp += matrix[col]*vector[colIndex[col]];
      }
      result[row] += temp;
    }
  } /* end of iterations                                                          */

  #pragma omp barrier
  #pragma omp master
  {
    sparse_time = wtime() - sparse_time;
  }

  } /* end of parallel region                                                     */

  /* verification test                                                            */
  reference_sum = 0.5 * (double) nent * (double) (iterations+1) * 
                        (double) (iterations +2);

  vector_sum = 0.0;
  for (row=0; row<size2; row++) vector_sum += result[row];
  if (ABS(vector_sum-reference_sum) > epsilon) {
    printf("ERROR: Vector sum = %lf, Reference vector sum = %lf\n",
           vector_sum, reference_sum);
    exit(EXIT_FAILURE);
  }
  else {
    printf("Solution validates\n");
#ifdef VERBOSE
    printf("Reference sum = %lf, vector sum = %lf\n", 
           reference_sum, vector_sum);
#endif
  }

  avgtime = sparse_time/iterations;
  printf("Rate (MFlops/s): %lf  Avg time (s): %lf\n",
         1.0E-06 * (2.0*nent)/avgtime, avgtime);

  exit(EXIT_SUCCESS);
}

/* Code below reverses bits in unsigned integer stored in a 64-bit word.
   Bit reversal is with respect to the largest integer that is going to be
   processed for the particular run of the code, to make sure the reversal
   constitutes a true permutation. Hence, the final result needs to be shifted 
   to the right.
   Example: if largest integer being processed is 0x000000ff = 255 = 
   0000...0011111111 (binary), then the unshifted reversal of 0x00000006 = 6 =
   0000...0000000110 (binary) would be 011000000...0000 = 3*2^61, which is 
   outside the range of the original sequence 0-255. Setting shift_in_bits to
   2log(256) = 8, the final result is shifted the the right by 64-8=56 bits,
   so we get 000...0001100000 (binary) = 96, which is within the proper range */
u64Int reverse(register u64Int x, int shift_in_bits){ 
  x = ((x >> 1)  & 0x5555555555555555) | ((x << 1)  & 0xaaaaaaaaaaaaaaaa);
  x = ((x >> 2)  & 0x3333333333333333) | ((x << 2)  & 0xcccccccccccccccc);
  x = ((x >> 4)  & 0x0f0f0f0f0f0f0f0f) | ((x << 4)  & 0xf0f0f0f0f0f0f0f0);
  x = ((x >> 8)  & 0x00ff00ff00ff00ff) | ((x << 8)  & 0xff00ff00ff00ff00);
  x = ((x >> 16) & 0x0000ffff0000ffff) | ((x << 16) & 0xffff0000ffff0000);
  x = ((x >> 32) & 0x00000000ffffffff) | ((x << 32) & 0xffffffff00000000);
  return (x>>((sizeof(u64Int)*BITS_IN_BYTE-shift_in_bits)));
}

int compare(const void *el1, const void *el2) {
  s64Int v1 = *(s64Int *)el1;  
  s64Int v2 = *(s64Int *)el2;
  return (v1<v2) ? -1 : (v1>v2) ? 1 : 0;
}
