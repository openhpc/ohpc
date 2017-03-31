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
  
USAGE:   The program takes as input the 2log of the linear size of the 2D grid 
         (equalling the 2log of the square root of the order of the sparse matrix), 
         the radius of the difference stencil, and the number of times the 
         matrix-vector multiplication is carried out.

         <progname> <# iterations> <2log root-of-matrix-order> <radius>
  
         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.

FUNCTIONS CALLED:

         Other than MPI or standard C functions, the following 
         functions are used in this program:

         wtime()
         bail_out()
         reverse()
         qsort()
         compare

NOTES:   

HISTORY: Written by Rob Van der Wijngaart, October 2006.
         Updated by RvdW to fix verification bug, February 2013
         Updated by RvdW to sort matrix elements to reflect traditional CSR storage,
         August 2013
  
***********************************************************************************/

#include <par-res-kern_general.h>
#include <par-res-kern_mpi.h>

/* linearize the grid index                                                       */
#define LIN(i,j) (i+((j)<<lsize))

#ifdef TESTDENSE
#define DENSE(i,j) dense[LIN(i,j)]
#endif

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

  int               Num_procs;  /* Number of ranks                                */
  int               my_ID;      /* MPI rank                                       */
  int               root=0;
  int               iter, r;    /* dummies                                        */
  int               lsize;      /* logarithmic linear size of grid                */
  int               lsize2;     /* logarithmic size of grid                       */
  int               size;       /* linear size of grid                            */
  s64Int            size2;      /* matrix order (=total # points in grid)         */
  int               radius,     /* stencil parameters                             */
                    stencil_size; 
  s64Int            row, col, first, last; /* dummies                             */
  u64Int            i, j;       /* dummies                                        */
  int               iterations; /* number of times the multiplication is done     */

  s64Int            elm;        /* sequence number of matrix nonzero              */
  s64Int            elm_start;  /* auxiliary variable                             */
  int               jstart,     /* active grid rows parameters                    */
                    jend,
                    nrows,
                    row_offset;
  s64Int            nent;       /* number of nonzero entries                      */
  double            sparsity;   /* fraction of non-zeroes in matrix               */
  double            local_sparse_time,/* timing parameters                        */
                    sparse_time, 
                    avgtime;
  double * RESTRICT matrix;     /* sparse matrix entries                          */
  double * RESTRICT vector;     /* vector multiplying the sparse matrix           */
  double * RESTRICT result;     /* computed matrix-vector product                 */
  double            temp;       /* temporary scalar storing reduction data        */
#ifdef TESTDENSE
  double * RESTRICT rhs;        /* known matrix-vector product                    */
  double * RESTRICT dense;      /* dense matrix equivalent of "matrix"            */
#endif
  double            vector_sum; /* checksum of result                             */
  double            reference_sum, /* local checksum of "rhs"                     */
                    check_sum;  /* aggregate checksum of "rhs"                    */
  double            epsilon = 1.e-8; /* error tolerance                           */
  s64Int * RESTRICT colIndex;   /* column indices of sparse matrix entries        */
  int               error=0;    /* error flag                                     */
  size_t            vector_space, /* variables used to hold malloc sizes          */
                    matrix_space,
                    index_space;

/*********************************************************************
** Initialize the MPI environment
*********************************************************************/
  MPI_Init(&argc,&argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_ID);
  MPI_Comm_size(MPI_COMM_WORLD, &Num_procs);

/*********************************************************************
** process, test and broadcast input parameters
*********************************************************************/

  if (my_ID == root){
    printf("MPI sparse matrix-vector multiplication\n");
    if (argc != 4){
      printf("Usage: %s <# iterations> <2log grid size> <stencil radius>\n",*argv);
      error = 1;
      goto ENDOFTESTS;
    }

    iterations = atoi(*++argv);
    if (iterations < 1){
      printf("ERROR: Iterations must be positive : %d \n", iterations);
      error = 1;
      goto ENDOFTESTS;
    }

    lsize = atoi(*++argv);
    if (lsize <0) {
      printf("ERROR: Log of grid size must be non-negative: %d\n", 
           (int) lsize);
      error = 1;
      goto ENDOFTESTS;
    }
    lsize2 = 2*lsize;
    size = 1<<lsize;
    if (size < Num_procs) {
      printf("ERROR: Grid size %d must be at least equal to # procs %d\n",
             (int) size, Num_procs);
      error = 1;
      goto ENDOFTESTS;
    }

    if ((int)(size%Num_procs)) {
      printf("ERROR: Grid size %d must be multiple of # procs %d\n", 
             (int) size, Num_procs);
      error = 1;
      goto ENDOFTESTS;
    } 

    /* compute number of points in the grid                                         */
    size2 = size*size;

    radius = atoi(*++argv);
    if (radius <0) {
      printf("ERROR: Stencil radius must be non-negative: %d\n", radius);
      error = 1;
      goto ENDOFTESTS;
    }

    /* emit error if (periodic) stencil overlaps with itself                        */
    if (size <2*radius+1) {
      printf("ERROR: Grid extent %d smaller than stencil diameter 2*%d+1= %d\n",
             size, radius, radius*2+1);
      error = 1;
      goto ENDOFTESTS;
    }
 
    /* sparsity follows from number of non-zeroes per row                           */
    sparsity = (double)(4*radius+1)/(double)size2;

    printf("Number of ranks       = %16d\n",Num_procs);
    printf("Matrix order          = "FSTR64U"\n", size2);
    printf("Stencil diameter      = %16d\n", 2*radius+1);
    printf("Sparsity              = %16.10lf\n", sparsity);
    printf("Number of iterations  = %16d\n", iterations);
#ifdef SCRAMBLE
    printf("Using scrambled indexing\n");
#else
    printf("Using canonical indexing\n");
#endif

    ENDOFTESTS:;
  }
  bail_out(error);

  /* Broadcast benchmark data to all ranks */
  MPI_Bcast(&lsize,      1, MPI_INT,           root, MPI_COMM_WORLD);
  MPI_Bcast(&lsize2,     1, MPI_INT,           root, MPI_COMM_WORLD);
  MPI_Bcast(&size,       1, MPI_LONG_LONG_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&size2,      1, MPI_LONG_LONG_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&radius,     1, MPI_INT,           root, MPI_COMM_WORLD);
  MPI_Bcast(&iterations, 1, MPI_INT,           root, MPI_COMM_WORLD);

  /* compute total size of star stencil in 2D                                     */
  stencil_size = 4*radius+1;
  /* compute number of rows owned by each rank                                    */
  nrows = size2/Num_procs;

  /* compute total number of non-zeroes for this rank                             */
  nent = nrows*stencil_size;

  matrix_space = nent*sizeof(double);
  if (matrix_space/sizeof(double) != nent) {
    printf("ERROR: rank %d cannot represent space for matrix: %ul\n", 
           my_ID, matrix_space);
    error = 1;
  } 
  bail_out(error);

  matrix = (double *) malloc(matrix_space);
  if (!matrix) {
    printf("ERROR: rank %d could not allocate space for sparse matrix: "FSTR64U"\n", 
           my_ID, matrix_space);
    error = 1;
  } 
  bail_out(error);

  vector_space = (size2 + nrows)*sizeof(double);
  if (vector_space/sizeof(double) != (size2+nrows)) {
    printf("ERROR: rank %d Cannot represent space for vectors: %ul\n", 
           my_ID, vector_space);
    error = 1; 
  } 
  bail_out(error);

  vector = (double *) malloc(vector_space);
  if (!vector) {
    printf("ERROR: rank %d could not allocate space for vectors: %d\n", 
           my_ID, (int)(2*nrows));
    error = 1;
  }
  bail_out(error);
  result = vector + size2;

  index_space = nent*sizeof(s64Int);
  if (index_space/sizeof(s64Int) != nent) {
    printf("ERROR: rank %d cannot represent space for column indices: %ul\n", 
           my_ID, index_space);
    error = 1;
  } 
  bail_out(error);

  colIndex = (s64Int *) malloc(index_space);
  if (!colIndex) {
    printf("ERROR: rank %d Could not allocate space for column indices: "FSTR64U"\n",
           my_ID, nent*sizeof(s64Int));
    error = 1;
  } 
  bail_out(error);

  /* fill matrix with nonzeroes corresponding to difference stencil. We use the 
     scrambling for reordering the points in the grid.                            */

  jstart = (size/Num_procs)*my_ID;
  jend   = (size/Num_procs)*(my_ID+1);

  for (j=jstart; j<jend; j++) for (i=0; i<size; i++) {
    elm_start = (i+(j-jstart)*size)*stencil_size;
    elm = elm_start;
    colIndex[elm] = REVERSE(LIN(i,j),lsize2);
    for (r=1; r<=radius; r++, elm+=4) {
      colIndex[elm+1] = REVERSE(LIN((i+r)%size,j),lsize2);
      colIndex[elm+2] = REVERSE(LIN((i-r+size)%size,j),lsize2);
      colIndex[elm+3] = REVERSE(LIN(i,(j+r)%size),lsize2);
      colIndex[elm+4] = REVERSE(LIN(i,(j-r+size)%size),lsize2);
    }
    /* sort colIndex to make sure the compressed row accesses
       vector elements in increasing order                                        */
    qsort(&(colIndex[elm_start]), stencil_size, sizeof(s64Int), compare);
    for (elm=elm_start; elm<elm_start+stencil_size; elm++) 
      matrix[elm] = 1.0/(double)(colIndex[elm]+1);   
  }

#if defined(TESTDENSE) && defined(VERBOSE)
  /* fill dense matrix to test                                                    */
  matrix_space = size2*size2/Num_procs*sizeof(double);
  if (matrix_space/sizeof(double) != size2*size2/Num_procs) {
    printf("ERROR: Cannot represent space for matrix: %ul\n", matrix_space);
    exit(EXIT_FAILURE);
  } 
  dense = (double *) malloc(matrix_space);
  if (!dense) {
    printf("ERROR: Could not allocate space for dense matrix of order: %d\n",
           (int) size2);
    exit(EXIT_FAILURE);
  }
  rhs   = (double *) malloc(vector_space);
  if (!rhs) {
    printf("ERROR: Could not allocate space for rhs: %d\n", (int) size2);
    exit(EXIT_FAILURE);
  }
  for (row=0; row<nrows; row++) {
    for (col=0; col<size2; col++) DENSE(col,row) = 0.0;
    first = row*stencil_size; last = first+stencil_size-1;
    rhs[row] = (double) (last-first+1) * (double) iterations;
    for (elm=first; elm<=last; elm++) DENSE(colIndex[elm],row) = matrix[elm];
  }
#endif

  /* initialize the input and result vectors                                      */
  for (row=0; row<nrows; row++) result[row] = vector[row] = 0.0;

  for (iter=0; iter<=iterations; iter++) {

    /* start timer after a warmup iteration */
    if (iter == 1) { 
      MPI_Barrier(MPI_COMM_WORLD);
      local_sparse_time = wtime();
    }

    /* fill vector                                                                */
    row_offset = nrows*my_ID;
    for (row=row_offset; row<nrows+row_offset; row++) vector[row] += (double) (row+1);

    /* replicate vector on all ranks                                              */
    MPI_Allgather(MPI_IN_PLACE, nrows, MPI_DOUBLE, vector, nrows, MPI_DOUBLE,
                  MPI_COMM_WORLD);

    /* do the actual matrix multiplication                                        */
    for (row=0; row<nrows; row++) {
      first = stencil_size*row; last = first+stencil_size-1;
      #pragma simd reduction(+:temp) 
      for (temp=0.0,col=first; col<=last; col++) {
        temp += matrix[col]*vector[colIndex[col]];
      }
      result[row] += temp;
    }
  } /* end of iterations                                                          */

  local_sparse_time = wtime() - local_sparse_time;
  MPI_Reduce(&local_sparse_time, &sparse_time, 1, MPI_DOUBLE, MPI_MAX, root,
             MPI_COMM_WORLD);


#if defined(TESTDENSE) && defined(VERBOSE)
  /* print matrix, vector, rhs, plus computed solution                            */
  for (row=0; row<nrows; row++) {
    printf("( ");
    for (col=0; col<size2; col++) printf("%1.3lf  ", DENSE(col,row));
    printf(" ) ( %1.3lf ) = ( %1.3lf ) | ( %1.3lf )\n", 
           vector[row], result[row], rhs[row]);
  }
#endif

  /* verification test                                                            */
  reference_sum = 0.5 * (double) size2 * (double) stencil_size * 
    (double) (iterations+1) * (double) (iterations + 2);

  vector_sum = 0.0;
  for (row=0; row<nrows; row++) vector_sum += result[row];

  MPI_Reduce(&vector_sum, &check_sum, 1, MPI_DOUBLE, MPI_SUM, root, MPI_COMM_WORLD);

  if (my_ID == root) {
    if (ABS(check_sum-reference_sum) > epsilon) {
      printf("ERROR: Vector sum = %lf, Reference vector sum = %lf, my_ID = %d\n",
             check_sum, reference_sum, my_ID);
      error = 1;
    }
    else {
      printf("Solution validates\n");
#ifdef VERBOSE
      printf("Reference sum = %lf, check sum = %lf\n", 
             reference_sum, check_sum);
#endif
    }
    avgtime = sparse_time/iterations;
    printf("Rate (MFlops/s): %lf  Avg time (s): %lf\n",
           1.0E-06 * (2.0*nent*Num_procs)/avgtime, avgtime);
  }

  bail_out(error);

  MPI_Finalize();
  exit(EXIT_SUCCESS);
}

/* Code below reverses bits in unsigned integer stored in a 64-bit word.
   Bit reversal is with respect to the largest integer that is going to be
   ranked for the particular run of the code, to make sure the reversal
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

