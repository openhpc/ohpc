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
  
USAGE:   The program takes as input the linear dimension of the grid, 
         and the number of iterations on the grid
 
               <progname> <# iterations> <grid size>
  
         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.
 
FUNCTIONS CALLED:
 
         Other than MPI or standard C functions, the following 
         functions are used in this program:
 
         wtime()
         bail_out()
 
HISTORY: - Written by Rob Van der Wijngaart, November 2006.
         - RvdW, August 2013: Removed unrolling pragmas for clarity;
           fixed bug in compuation of width of strip assigned to 
           each rank;
         - RvdW, August 2013: added constant to array "in" at end of 
           each iteration to force refreshing of neighbor data in 
           parallel versions
         - RvdW, October 2014: introduced 2D domain decomposition
         - RvdW, October 2014: removed barrier at start of each iteration
         - RvdW, October 2014: replaced single rank/single iteration timing
           with global timing of all iterations across all ranks
  
*********************************************************************************/
 
#include <par-res-kern_general.h>
#include <par-res-kern_mpi.h>
 
#ifndef RADIUS
  #define RADIUS 2
#endif
 
#ifdef DOUBLE
  #define DTYPE     double
  #define MPI_DTYPE MPI_DOUBLE
  #define EPSILON   1.e-8
  #define COEFX     1.0
  #define COEFY     1.0
  #define FSTR      "%lf"
#else
  #define DTYPE     float
  #define MPI_DTYPE MPI_FLOAT
  #define EPSILON   0.0001f
  #define COEFX     1.0f
  #define COEFY     1.0f
  #define FSTR      "%f"
#endif
 
/* define shorthand for indexing multi-dimensional arrays with offsets           */
#define INDEXIN(i,j)  (i+RADIUS+(j+RADIUS)*(width+2*RADIUS))
/* need to add offset of RADIUS to j to account for ghost points                 */
#define IN(i,j)       in[INDEXIN(i-istart,j-jstart)]
#define INDEXOUT(i,j) (i+(j)*(width))
#define OUT(i,j)      out[INDEXOUT(i-istart,j-jstart)]
#define WEIGHT(ii,jj) weight[ii+RADIUS][jj+RADIUS]
 
int main(int argc, char ** argv) {
 
  int    Num_procs;       /* number of ranks                                     */
  int    Num_procsx, Num_procsy; /* number of ranks in each coord direction      */
  int    my_ID;           /* MPI rank                                            */
  int    my_IDx, my_IDy;  /* coordinates of rank in rank grid                    */
  int    right_nbr;       /* global rank of right neighboring tile               */
  int    left_nbr;        /* global rank of left neighboring tile                */
  int    top_nbr;         /* global rank of top neighboring tile                 */
  int    bottom_nbr;      /* global rank of bottom neighboring tile              */
  DTYPE *top_buf_out;     /* communication buffer                                */
  DTYPE *top_buf_in;      /*       "         "                                   */
  DTYPE *bottom_buf_out;  /*       "         "                                   */
  DTYPE *bottom_buf_in;   /*       "         "                                   */
  DTYPE *right_buf_out;   /*       "         "                                   */
  DTYPE *right_buf_in;    /*       "         "                                   */
  DTYPE *left_buf_out;    /*       "         "                                   */
  DTYPE *left_buf_in;     /*       "         "                                   */
  int    root = 0;
  int    n, width, height;/* linear global and local grid dimension              */
  int    i, j, ii, jj, kk, it, jt, iter, leftover;  /* dummies                   */
  int    istart, iend;    /* bounds of grid tile assigned to calling rank        */
  int    jstart, jend;    /* bounds of grid tile assigned to calling rank        */
  DTYPE  norm,            /* L1 norm of solution                                 */
         local_norm,      /* contribution of calling rank to L1 norm             */
         reference_norm;
  DTYPE  f_active_points; /* interior of grid with respect to stencil            */
  DTYPE  flops;           /* floating point ops per iteration                    */
  int    iterations;      /* number of times to run the algorithm                */
  double local_stencil_time,/* timing parameters                                 */
         stencil_time,
         avgtime; 
  int    stencil_size;    /* number of points in stencil                         */
  DTYPE  * RESTRICT in;   /* input grid values                                   */
  DTYPE  * RESTRICT out;  /* output grid values                                  */
  long   total_length_in; /* total required length to store input array          */
  long   total_length_out;/* total required length to store output array         */
  int    error=0;         /* error flag                                          */
  DTYPE  weight[2*RADIUS+1][2*RADIUS+1]; /* weights of points in the stencil     */
  MPI_Request request[8];
  MPI_Status  status[8];
  MPI_Win rma_winx;       /* RMA window object x-direction */
  MPI_Win rma_winy;       /* RMA window object y-direction */
  MPI_Info rma_winfo;     /* info for window */

  /*******************************************************************************
  ** Initialize the MPI environment
  ********************************************************************************/
  MPI_Init(&argc,&argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_ID);
  MPI_Comm_size(MPI_COMM_WORLD, &Num_procs);
 
  /*******************************************************************************
  ** process, test, and broadcast input parameters    
  ********************************************************************************/
 
  if (my_ID == root) {
#ifndef STAR
      printf("ERROR: Compact stencil not supported\n");
      error = 1;
      goto ENDOFTESTS;
#endif
    
    if (argc != 3){
      printf("Usage: %s <# iterations> <array dimension> \n", 
             *argv);
      error = 1;
      goto ENDOFTESTS;
    }
 
    iterations  = atoi(*++argv); 
    if (iterations < 1){
      printf("ERROR: iterations must be >= 1 : %d \n",iterations);
      error = 1;
      goto ENDOFTESTS;  
    }
 
    n  = atoi(*++argv);
    long nsquare = n * n;
    if (nsquare < Num_procs){ 
      printf("ERROR: grid size must be at least # ranks: %ld\n", nsquare);
      error = 1;
      goto ENDOFTESTS;
    }
 
    if (RADIUS < 0) {
      printf("ERROR: Stencil radius %d should be non-negative\n", RADIUS);
      error = 1;
      goto ENDOFTESTS;  
    }
 
    if (2*RADIUS +1 > n) {
      printf("ERROR: Stencil radius %d exceeds grid size %d\n", RADIUS, n);
      error = 1;
      goto ENDOFTESTS;  
    }
 
    ENDOFTESTS:;  
  }
  bail_out(error);
 
  /* determine best way to create a 2D grid of ranks (closest to square, for 
     best surface/volume ratio); we do this brute force for now
  */
  for (Num_procsx=(int) (sqrt(Num_procs+1)); Num_procsx>0; Num_procsx--) {
    if (!(Num_procs%Num_procsx)) {
      Num_procsy = Num_procs/Num_procsx;
      break;
    }
  }      
  my_IDx = my_ID%Num_procsx;
  my_IDy = my_ID/Num_procsx;
  /* compute neighbors; don't worry about dropping off the edges of the grid */
  right_nbr  = my_ID+1;
  left_nbr   = my_ID-1;
  top_nbr    = my_ID+Num_procsx;
  bottom_nbr = my_ID-Num_procsx;
 
  if (my_ID == root) {
    printf("MPIRMA stencil execution on 2D grid\n");
    printf("Number of ranks        = %d\n", Num_procs);
    printf("Grid size              = %d\n", n);
    printf("Radius of stencil      = %d\n", RADIUS);
    printf("Tiles in x/y-direction = %d/%d\n", Num_procsx, Num_procsy);
    printf("Type of stencil        = star\n");
#ifdef DOUBLE
    printf("Data type              = double precision\n");
#else
    printf("Data type              = single precision\n");
#endif
    printf("Number of iterations   = %d\n", iterations);
  }
 
  MPI_Bcast(&n,          1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&iterations, 1, MPI_INT, root, MPI_COMM_WORLD);
 
  /* compute amount of space required for input and solution arrays             */
  
  width = n/Num_procsx;
  leftover = n%Num_procsx;
  if (my_IDx<leftover) {
    istart = (width+1) * my_IDx; 
    iend = istart + width + 1;
  }
  else {
    istart = (width+1) * leftover + width * (my_IDx-leftover);
    iend = istart + width;
  }
  
  width = iend - istart + 1;
  if (width == 0) {
    printf("ERROR: rank %d has no work to do\n", my_ID);
    error = 1;
  }
  bail_out(error);
 
  height = n/Num_procsy;
  leftover = n%Num_procsy;
  if (my_IDy<leftover) {
    jstart = (height+1) * my_IDy; 
    jend = jstart + height + 1;
  }
  else {
    jstart = (height+1) * leftover + height * (my_IDy-leftover);
    jend = jstart + height;
  }
  
  height = jend - jstart + 1;
  if (height == 0) {
    printf("ERROR: rank %d has no work to do\n", my_ID);
    error = 1;
  }
  bail_out(error);
 
  if (width < RADIUS || height < RADIUS) {
    printf("ERROR: rank %d has work tile smaller then stencil radius\n",
           my_ID);
    error = 1;
  }
  bail_out(error);
 
  total_length_in = (width+2*RADIUS)*(height+2*RADIUS)*sizeof(DTYPE);
  total_length_out = width*height*sizeof(DTYPE);
 
  in  = (DTYPE *) malloc(total_length_in);
  out = (DTYPE *) malloc(total_length_out);
  if (!in || !out) {
    printf("ERROR: rank %d could not allocate space for input/output array\n",
            my_ID);
    error = 1;
  }
  bail_out(error);
 
  /* fill the stencil weights to reflect a discrete divergence operator         */
  for (jj=-RADIUS; jj<=RADIUS; jj++) for (ii=-RADIUS; ii<=RADIUS; ii++)
    WEIGHT(ii,jj) = (DTYPE) 0.0;
  stencil_size = 4*RADIUS+1;
  for (ii=1; ii<=RADIUS; ii++) {
    WEIGHT(0, ii) = WEIGHT( ii,0) =  (DTYPE) (1.0/(2.0*ii*RADIUS));
    WEIGHT(0,-ii) = WEIGHT(-ii,0) = -(DTYPE) (1.0/(2.0*ii*RADIUS));
  }
 
  norm = (DTYPE) 0.0;
  f_active_points = (DTYPE) (n-2*RADIUS)*(DTYPE) (n-2*RADIUS);
  /* intialize the input and output arrays                                     */
  for (j=jstart; j<jend; j++) for (i=istart; i<iend; i++) {
    IN(i,j)  = COEFX*i+COEFY*j;
    OUT(i,j) = (DTYPE)0.0;
  }
 
  /* RMA win info */
  MPI_Info_create(&rma_winfo);
  /* This key indicates that passive target RMA will not be used.
   * It is the one info key that MPICH actually uses for optimization. */
  MPI_Info_set(rma_winfo, "no_locks", "true");

  /* allocate communication buffers for halo values                            */
  MPI_Win_allocate(4*sizeof(DTYPE)*RADIUS*width, sizeof(DTYPE), rma_winfo, MPI_COMM_WORLD, (void *) &top_buf_out, &rma_winy);
  if (!top_buf_out) {
    printf("ERROR: Rank %d could not allocated comm buffers for y-direction\n", my_ID);
    error = 1;
  }
  bail_out(error);
  top_buf_in     = top_buf_out +   RADIUS*width;
  bottom_buf_out = top_buf_out + 2*RADIUS*width;
  bottom_buf_in  = top_buf_out + 3*RADIUS*width;
 
  MPI_Win_allocate(4*sizeof(DTYPE)*RADIUS*height, sizeof(DTYPE), rma_winfo, MPI_COMM_WORLD, (void *) &right_buf_out, &rma_winx);
  if (!right_buf_out) {
    printf("ERROR: Rank %d could not allocated comm buffers for x-direction\n", my_ID);
    error = 1;
  }
  bail_out(error);
  right_buf_in   = right_buf_out +   RADIUS*height;
  left_buf_out   = right_buf_out + 2*RADIUS*height;
  left_buf_in    = right_buf_out + 3*RADIUS*height;

  for (iter = 0; iter<=iterations; iter++){

    /* start timer after a warmup iteration */
    if (iter == 1) { 
      MPI_Barrier(MPI_COMM_WORLD);
      local_stencil_time = wtime();
    }

    /* need to fetch ghost point data from neighbors in y-direction                 */
    MPI_Win_fence(0, rma_winy);
    if (my_IDy < Num_procsy-1) {
      for (kk=0,j=jend-RADIUS; j<=jend-1; j++) for (i=istart; i<=iend; i++) {
          top_buf_out[kk++]= IN(i,j);
      }
      MPI_Put(top_buf_out, RADIUS*width, MPI_DTYPE, top_nbr,
	      3*RADIUS*width, RADIUS*width, MPI_DTYPE, rma_winy);
    }
    if (my_IDy > 0) {
      for (kk=0,j=jstart; j<=jstart+RADIUS-1; j++) for (i=istart; i<=iend; i++) {
          bottom_buf_out[kk++]= IN(i,j);
      }
      MPI_Put(bottom_buf_out, RADIUS*width, MPI_DTYPE, bottom_nbr,
	      RADIUS*width, RADIUS*width, MPI_DTYPE, rma_winy);
    }
    MPI_Win_fence(0, rma_winy);
    if (my_IDy < Num_procsy-1) {
      for (kk=0,j=jend; j<=jend+RADIUS-1; j++) for (i=istart; i<=iend; i++) {
          IN(i,j) = top_buf_in[kk++];
      }      
    }
    if (my_IDy > 0) {
      for (kk=0,j=jstart-RADIUS; j<=jstart-1; j++) for (i=istart; i<=iend; i++) {
          IN(i,j) = bottom_buf_in[kk++];
      }      
    }

    /* need to fetch ghost point data from neighbors in x-direction                 */
    MPI_Win_fence(0, rma_winx);
    if (my_IDx < Num_procsx-1) {
      for (kk=0,j=jstart; j<=jend; j++) for (i=iend-RADIUS; i<=iend-1; i++) {
          right_buf_out[kk++]= IN(i,j);
      }
      MPI_Put(right_buf_out, RADIUS*height, MPI_DTYPE, right_nbr,
	      3*RADIUS*height, RADIUS*height, MPI_DTYPE, rma_winx);
    }
    if (my_IDx > 0) {
      for (kk=0,j=jstart; j<=jend; j++) for (i=istart; i<=istart+RADIUS-1; i++) {
          left_buf_out[kk++]= IN(i,j);
      }
      MPI_Put(left_buf_out, RADIUS*height, MPI_DTYPE, left_nbr,
	      RADIUS*height, RADIUS*height, MPI_DTYPE, rma_winx);
    }
    MPI_Win_fence(0, rma_winx);
    if (my_IDx < Num_procsx-1) {
      for (kk=0,j=jstart; j<=jend; j++) for (i=iend; i<=iend+RADIUS-1; i++) {
          IN(i,j) = right_buf_in[kk++];
      }      
    }
    if (my_IDx > 0) {
      for (kk=0,j=jstart; j<=jend; j++) for (i=istart-RADIUS; i<=istart-1; i++) {
          IN(i,j) = left_buf_in[kk++];
      }      
    }
 
    /* Apply the stencil operator */
    for (j=MAX(jstart,RADIUS); j<MIN(n-RADIUS,jend); j++) {
      for (i=MAX(istart,RADIUS); i<MIN(n-RADIUS,iend); i++) {
        for (jj=-RADIUS; jj<=RADIUS; jj++) {
          OUT(i,j) += WEIGHT(0,jj)*IN(i,j+jj);
        }
        for (ii=-RADIUS; ii<0; ii++) {
          OUT(i,j) += WEIGHT(ii,0)*IN(i+ii,j);
        }
        for (ii=1; ii<=RADIUS; ii++) {
          OUT(i,j) += WEIGHT(ii,0)*IN(i+ii,j);
 
        }
      }
    }
 
    /* add constant to solution to force refresh of neighbor data, if any */
    for (j=jstart; j<jend; j++) for (i=istart; i<iend; i++) IN(i,j)+= 1.0;
 
  }
 
  local_stencil_time = wtime() - local_stencil_time;
  MPI_Reduce(&local_stencil_time, &stencil_time, 1, MPI_DOUBLE, MPI_MAX, root,
             MPI_COMM_WORLD);
  
  /* compute L1 norm in parallel                                                */
  local_norm = (DTYPE) 0.0;
  for (j=MAX(jstart,RADIUS); j<MIN(n-RADIUS,jend); j++) {
    for (i=MAX(istart,RADIUS); i<MIN(n-RADIUS,iend); i++) {
      local_norm += (DTYPE)ABS(OUT(i,j));
    }
  }
 
  MPI_Reduce(&local_norm, &norm, 1, MPI_DTYPE, MPI_SUM, root, MPI_COMM_WORLD);
 
  /*******************************************************************************
  ** Analyze and output results.
  ********************************************************************************/
 
/* verify correctness                                                            */
  if (my_ID == root) {
    norm /= f_active_points;
    if (RADIUS > 0) {
      reference_norm = (DTYPE) (iterations+1) * (COEFX + COEFY);
    }
    else {
      reference_norm = (DTYPE) 0.0;
    }
    if (ABS(norm-reference_norm) > EPSILON) {
      printf("ERROR: L1 norm = "FSTR", Reference L1 norm = "FSTR"\n",
             norm, reference_norm);
      error = 1;
    }
    else {
      printf("Solution validates\n");
#ifdef VERBOSE
      printf("Reference L1 norm = "FSTR", L1 norm = "FSTR"\n", 
             reference_norm, norm);
#endif
    }
  }
  bail_out(error);
 
  if (my_ID == root) {
    /* flops/stencil: 2 flops (fma) for each point in the stencil, 
       plus one flop for the update of the input of the array        */
    flops = (DTYPE) (2*stencil_size+1) * f_active_points;
    avgtime = stencil_time/iterations;
    printf("Rate (MFlops/s): "FSTR"  Avg time (s): %lf\n",
           1.0E-06 * flops/avgtime, avgtime);
  }
 
  MPI_Win_free(&rma_winx);
  MPI_Win_free(&rma_winy);

  MPI_Info_free(&rma_winfo);

  MPI_Finalize();
  exit(EXIT_SUCCESS);
}
