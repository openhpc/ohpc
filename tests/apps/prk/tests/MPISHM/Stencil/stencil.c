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

/**********************************************************************************
 Strategy for hierarchical decomposition of grid.
 Give a total number of Num_procs ranks and coherence domains of group_size ranks. 
 The grid is divided into Num_procs/group_size contiguous groups of group_size 
 tiles each (a "block"). 
 Each tile is assigned to a rank. Ranks within a block are contiguous within 
 MPI_COMM_WORLD, so that contiguous groups of ranks span a single coherence 
 domain.
 Given Num_procs ranks, factor Num_procs into two integers Num_procsx and 
 Num_procsy that are as close together as possible. That is the division of tiles 
 within the grid. The tiles are now assigned to groups that lie within the same 
 coherence domain. Let group_size be factored into two integers group_sizex and 
 group_sizey. The number of groups Num_groups equals Num_procs/group_size. 
 Nomenclature:
 group: sequence number of block within the grid (ordered lexicographically)
 Num_groupsx: number of blocks in the x-direction
 Num_groupsy: number of blocks in the y-direction
 groupx: x-coordinate of block
 groupy: y-coordinate of block
 my_local_ID: sequence number of tile within block (ordered lexicographically)
 my_local_IDx: x-coordinate of tile within block
 my_local_IDy: y-coordinate of tile within block
 my_global_IDx: x-coordinate of tile within overall grid
 my_global_IDy: y-coordinate of tile within overall grid
 
 Example: Num_procs=24, group_size=4
 Num_groups = 6, Num_procsx=4, Num_procsy=6, group_sizex=2, group_sizey=2, 
 Num_groupsx=2, Num_groupsy=3
 Ranks (double lines indicate block boundaries):

 ------------------------------------
 |        |        ||        |       |
 |   18   |   19   ||   22   |   23  |
 |        |        ||        |       |
 ------------------------------------ 
 |        |        ||        |       |
 |   16   |   17   ||   20   |   21  |
 |        |        ||        |       |
 ------------------------------------ 
 ------------------------------------ 
 |        |        ||        |       |
 |   10   |   11   ||   14   |   15  |
 |        |        ||        |       |
 ------------------------------------ 
 |        |        ||        |       |
 |   8    |   9    ||   12   |   13  |
 |        |        ||        |       |
 ------------------------------------ 
 ------------------------------------ 
 |        |        ||        |       |
 |   2    |   3    ||   6    |   7   |
 |        |        ||        |       |
 ------------------------------------ 
 |        |        ||        |       |
 |   0    |   1    ||   4    |   5   |
 |        |        ||        |       |
 ------------------------------------ 
 
*********************************************************************************/
 
#ifndef RADIUS
  #define RADIUS 2
#endif
 
#ifdef DOUBLE
  #define DTYPE     double
  #define MPI_DTYPE MPI_DOUBLE
  #define EPSILON   1.e-8
  #define COEFX     1.0
  #define COEFY     1.0
  #define FSTR      "%15.10lf"
#else
  #define DTYPE     float
  #define MPI_DTYPE MPI_FLOAT
  #define EPSILON   0.0001f
  #define COEFX     1.0f
  #define COEFY     1.0f
  #define FSTR      "%15.10f"
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
  int    Num_procsx, 
         Num_procsy;      /* number of ranks in each coord direction             */
  int    Num_groupsx, 
         Num_groupsy;     /* number of blocks in each coord direction            */
  int    my_group;        /* sequence number of shared memory block              */
  int    my_group_IDx,
         my_group_IDy;    /* coordinates of block within block grid              */
  int    group_size;      /* number of ranks in shared memory group              */
  int    group_sizex,
         group_sizey;     /* number of ranks in block in each coord direction    */
  int    my_ID;           /* MPI rank                                            */
  int    my_global_IDx, 
         my_global_IDy;   /* coordinates of rank in overall rank grid            */
  int    my_local_IDx, 
         my_local_IDy;    /* coordinates of rank within shared memory block      */
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
  int    n, width, height;/* linear global and block grid dimension              */
  int    width_rank, 
         height_rank;     /* linear local dimension                              */
  int    i, j, ii, jj, kk, it, jt, iter, leftover;  /* dummies                   */
  int    istart_rank, 
         iend_rank;       /* bounds of grid tile assigned to calling rank        */
  int    jstart_rank, 
         jend_rank;       /* bounds of grid tile assigned to calling rank        */
  int    istart, iend;    /* bounds of grid block containing tile                */
  int    jstart, jend;    /* bounds of grid block containing tile                */
  DTYPE  norm,            /* L1 norm of solution                                 */
         local_norm,      /* contribution of calling rank to L1 norm             */
         reference_norm;  /* value to be matched by computed norm                */
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
  MPI_Request request[8]; /* requests for sends & receives in 4 coord directions */
  MPI_Status  status[8];  /* corresponding statuses                              */
  MPI_Win shm_win_in;     /* shared memory window object for IN array            */
  MPI_Win shm_win_out;    /* shared memory window object for OUT array           */
  MPI_Comm shm_comm_prep; /* preparatory shared memory communicator              */
  MPI_Comm shm_comm;      /* Shared Memory Communicator                          */
  int shm_procs;          /* # of rankes in shared domain                        */
  int shm_ID;             /* MPI rank in shared memory domain                    */
  MPI_Aint size_in;       /* size of the IN array in shared memory window        */
  MPI_Aint size_out;      /* size of the OUT array in shared memory window       */
  int size_mul;           /* one for shm_comm root, zero for the other ranks     */
  int disp_unit;          /* ignored                                             */
 
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
    
    if (argc != 4){
      printf("Usage: %s  <#ranks per coherence domain><# iterations> <array dimension> \n", 
             *argv);
      error = 1;
      goto ENDOFTESTS;
    }
 
    group_size = atoi(*++argv);
    if (group_size < 1) {
      printf("ERROR: # ranks per coherence domain must be >= 1 : %d \n",group_size);
      error = 1;
      goto ENDOFTESTS;
    } 
    if (Num_procs%group_size) {
      printf("ERROR: total # %d ranks not divisible by ranks per coherence domain %d\n",
	     Num_procs, group_size);
      error = 1;
      goto ENDOFTESTS;
    } 

    iterations  = atoi(*++argv); 
    if (iterations < 0){
      printf("ERROR: iterations must be >= 0 : %d \n",iterations);
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

  MPI_Bcast(&n,          1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&iterations, 1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&group_size, 1, MPI_INT, root, MPI_COMM_WORLD);
 
  /* determine best way to create a 2D grid of ranks (closest to square, for 
     best surface/volume ratio); we do this brute force for now. The 
     decomposition needs to be such that shared memory groups can evenly
     tessellate the rank grid
  */
  for (Num_procsx=(int) (sqrt(Num_procs+1)); Num_procsx>0; Num_procsx--) {
    if (!(Num_procs%Num_procsx)) {
      Num_procsy = Num_procs/Num_procsx;
      for (group_sizex=(int)(sqrt(group_size+1)); group_sizex>0; group_sizex--) {
        if (!(group_size%group_sizex) && !(Num_procsx%group_sizex)) {
          group_sizey=group_size/group_sizex;
          break;
        }
      }
      if (!(Num_procsy%group_sizey)) break;
    }
  }      


  if (my_ID == root) {
    printf("MPI+SHM stencil execution on 2D grid\n");
    printf("Number of ranks                 = %d\n", Num_procs);
    printf("Grid size                       = %d\n", n);
    printf("Radius of stencil               = %d\n", RADIUS);
    printf("Tiles in x/y-direction          = %d/%d\n", Num_procsx, Num_procsy);
    printf("Tiles per shared memory domain  = %d\n", group_size);
    printf("Tiles in x/y-direction in group = %d/%d\n", group_sizex,  group_sizey);
    printf("Type of stencil                 = star\n");
#ifdef DOUBLE
    printf("Data type                       = double precision\n");
#else
    printf("Data type                       = single precision\n");
#endif
    printf("Number of iterations            = %d\n", iterations);
  }

  /* Setup for Shared memory regions */

  /* first divide WORLD in groups of size group_size */
  MPI_Comm_split(MPI_COMM_WORLD, my_ID/group_size, my_ID%group_size, &shm_comm_prep);
  /* derive from that a SHM communicator */
  MPI_Comm_split_type(shm_comm_prep, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, &shm_comm);
  MPI_Comm_rank(shm_comm, &shm_ID);
  MPI_Comm_size(shm_comm, &shm_procs);
  /* do sanity check, making sure groups did not shrink in second comm split */
  if (shm_procs != group_size) MPI_Abort(MPI_COMM_WORLD, 666);
  
  Num_groupsx = Num_procsx/group_sizex;
  Num_groupsy = Num_procsy/group_sizey;

  my_group = my_ID/group_size;
  my_group_IDx = my_group%Num_groupsx;
  my_group_IDy = my_group/Num_groupsx;
  my_local_IDx = my_ID%group_sizex;
  my_local_IDy = (my_ID%group_size)/group_sizex;
  my_global_IDx = my_group_IDx*group_sizex+my_local_IDx;
  my_global_IDy = my_group_IDy*group_sizey+my_local_IDy;

  /* set all neighboring ranks to -1 (no communication with other coherence domain) */
  left_nbr = right_nbr = top_nbr = bottom_nbr = -1;

  if (my_local_IDx == group_sizex-1 && my_group_IDx != (Num_groupsx-1)) {
    right_nbr = (my_group+1)*group_size+shm_ID-group_sizex+1;
  }

  if (my_local_IDx == 0 && my_group_IDx != 0) {
    left_nbr = (my_group-1)*group_size+shm_ID+group_sizex-1;
  }

  if (my_local_IDy == group_sizey-1 && my_group_IDy != (Num_groupsy-1)) {
    top_nbr = (my_group+Num_groupsx)*group_size + my_local_IDx;
  }

  if (my_local_IDy == 0 && my_group_IDy != 0) {
    bottom_nbr = (my_group-Num_groupsx)*group_size + group_sizex*(group_sizey-1)+my_local_IDx;
  }

  /* compute amount of space required for input and solution arrays for the block,
     and also compute index sets                                                  */
  
  width = n/Num_groupsx;
  leftover = n%Num_groupsx;
  if (my_group_IDx<leftover) {
    istart = (width+1) * my_group_IDx; 
    iend = istart + width;
  }
  else {
    istart = (width+1) * leftover + width * (my_group_IDx-leftover);
    iend = istart + width - 1;
  }
  
  width = iend - istart + 1;
  if (width == 0) {
    printf("ERROR: rank %d has no work to do\n", my_ID);
    error = 1;
  }
  bail_out(error);
 
  height = n/Num_groupsy;
  leftover = n%Num_groupsy;
  if (my_group_IDy<leftover) {
    jstart = (height+1) * my_group_IDy; 
    jend = jstart + height;
  }
  else {
    jstart = (height+1) * leftover + height * (my_group_IDy-leftover);
    jend = jstart + height - 1;
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

  /* only the root of each SHM domain specifies window of nonzero size */
  size_mul = (shm_ID==0);  
  size_in= total_length_in*size_mul; 
  MPI_Win_allocate_shared(size_in, sizeof(double), MPI_INFO_NULL, shm_comm, 
                          (void *) &in, &shm_win_in);
  MPI_Win_shared_query(shm_win_in, MPI_PROC_NULL, &size_in, &disp_unit, (void *)&in);
  if (in == NULL){
    printf(" Error allocating space for original matrix on node %d\n",my_ID);
    error = 1;
  }
  bail_out(error);

  size_out= total_length_out*size_mul;
  MPI_Win_allocate_shared(size_out, sizeof(double), MPI_INFO_NULL, shm_comm, 
                          (void *) &out, &shm_win_out);
  MPI_Win_shared_query(shm_win_out, MPI_PROC_NULL, &size_out, &disp_unit, (void *)&out);
  if (out == NULL){
    printf(" Error allocating space for transposed matrix by group %d\n", my_group);
    error = 1;
  }
  bail_out(error);

  /* determine index set assigned to each rank                         */

  width_rank = n/Num_procsx;
  leftover = n%Num_procsx;
  if (my_global_IDx<leftover) {
    istart_rank = (width_rank+1) * my_global_IDx; 
    iend_rank = istart_rank + width_rank;
  }
  else {
    istart_rank = (width_rank+1) * leftover + width_rank * (my_global_IDx-leftover);
    iend_rank = istart_rank + width_rank - 1;
  }

  width_rank = iend_rank - istart_rank + 1;   

  height_rank = n/Num_procsy;
  leftover = n%Num_procsy;
  if (my_global_IDy<leftover) {
    jstart_rank = (height_rank+1) * my_global_IDy; 
    jend_rank = jstart_rank + height_rank;
  }
  else {
    jstart_rank = (height_rank+1) * leftover + height_rank * (my_global_IDy-leftover);
    jend_rank = jstart_rank + height_rank - 1;
  }
  
  height_rank = jend_rank - jstart_rank + 1;

  /* allocate communication buffers for halo values                            */
  top_buf_out = (DTYPE *) malloc(4*sizeof(DTYPE)*RADIUS*width_rank);
  if (!top_buf_out) {
    printf("ERROR: Rank %d could not allocated comm buffers for y-direction\n", my_ID);
    error = 1;
  }
  bail_out(error);
  top_buf_in     = top_buf_out +   RADIUS*width_rank;
  bottom_buf_out = top_buf_out + 2*RADIUS*width_rank;
  bottom_buf_in  = top_buf_out + 3*RADIUS*width_rank;
 
  right_buf_out = (DTYPE *) malloc(4*sizeof(DTYPE)*RADIUS*height_rank);
  if (!right_buf_out) { 
    printf("ERROR: Rank %d could not allocated comm buffers for x-direction\n", my_ID);
    error = 1;
  }
  bail_out(error);
  right_buf_in   = right_buf_out +   RADIUS*height_rank;
  left_buf_out   = right_buf_out + 2*RADIUS*height_rank;
  left_buf_in    = right_buf_out + 3*RADIUS*height_rank;

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
  for (j=jstart_rank; j<=jend_rank; j++) for (i=istart_rank; i<=iend_rank; i++) {
    IN(i,j)  = COEFX*i+COEFY*j;
    OUT(i,j) = (DTYPE)0.0;
  }

  MPI_Barrier(shm_comm); 

  for (iter = 0; iter<=iterations; iter++){

    /* start timer after a warmup iteration */
    if (iter == 1) { 
      MPI_Barrier(MPI_COMM_WORLD);
      local_stencil_time = wtime();
    }

    /* need to fetch ghost point data from neighbors in y-direction                 */
    if (top_nbr != -1) {
      MPI_Irecv(top_buf_in, RADIUS*width_rank, MPI_DTYPE, top_nbr, 101,
                MPI_COMM_WORLD, &(request[1]));
      for (kk=0,j=jend_rank-RADIUS+1; j<=jend_rank; j++) 
      for (i=istart_rank; i<=iend_rank; i++) {
        top_buf_out[kk++]= IN(i,j);
      }
      MPI_Isend(top_buf_out, RADIUS*width_rank,MPI_DTYPE, top_nbr, 99, 
                MPI_COMM_WORLD, &(request[0]));
    }

    if (bottom_nbr != -1) {
      MPI_Irecv(bottom_buf_in,RADIUS*width_rank, MPI_DTYPE, bottom_nbr, 99, 
                MPI_COMM_WORLD, &(request[3]));
      for (kk=0,j=jstart_rank; j<=jstart_rank+RADIUS-1; j++) 
      for (i=istart_rank; i<=iend_rank; i++) {
        bottom_buf_out[kk++]= IN(i,j);
      }
      MPI_Isend(bottom_buf_out, RADIUS*width_rank,MPI_DTYPE, bottom_nbr, 101,
 	  MPI_COMM_WORLD, &(request[2]));
      }

    if (top_nbr != -1) {
      MPI_Wait(&(request[0]), &(status[0]));
      MPI_Wait(&(request[1]), &(status[1]));
      for (kk=0,j=jend_rank+1; j<=jend_rank+RADIUS; j++) 
      for (i=istart_rank; i<=iend_rank; i++) {
        IN(i,j) = top_buf_in[kk++];
      }
    }

    if (bottom_nbr != -1) {    
      MPI_Wait(&(request[2]), &(status[2]));
      MPI_Wait(&(request[3]), &(status[3]));
      for (kk=0,j=jstart_rank-RADIUS; j<=jstart_rank-1; j++) 
      for (i=istart_rank; i<=iend_rank; i++) {
        IN(i,j) = bottom_buf_in[kk++];
      }
    }

    /* need to fetch ghost point data from neighbors in x-direction                 */
    if (right_nbr != -1) {
      MPI_Irecv(right_buf_in, RADIUS*height_rank, MPI_DTYPE, right_nbr, 1010,
                MPI_COMM_WORLD, &(request[1+4]));
      for (kk=0,j=jstart_rank; j<=jend_rank; j++) 
      for (i=iend_rank-RADIUS+1; i<=iend_rank; i++) {
        right_buf_out[kk++]= IN(i,j);
      }
      MPI_Isend(right_buf_out, RADIUS*height_rank, MPI_DTYPE, right_nbr, 990, 
                MPI_COMM_WORLD, &(request[0+4]));
    }

    if (left_nbr != -1) {
      MPI_Irecv(left_buf_in, RADIUS*height_rank, MPI_DTYPE, left_nbr, 990, 
                MPI_COMM_WORLD, &(request[3+4]));
      for (kk=0,j=jstart_rank; j<=jend_rank; j++) 
      for (i=istart_rank; i<=istart_rank+RADIUS-1; i++) {
        left_buf_out[kk++]= IN(i,j);
      }
      MPI_Isend(left_buf_out, RADIUS*height_rank, MPI_DTYPE, left_nbr, 1010,
                MPI_COMM_WORLD, &(request[2+4]));
    }

    if (right_nbr != -1) {
      MPI_Wait(&(request[0+4]), &(status[0+4]));
      MPI_Wait(&(request[1+4]), &(status[1+4]));
      for (kk=0,j=jstart_rank; j<=jend_rank; j++) 
      for (i=iend_rank+1; i<=iend_rank+RADIUS; i++) {
        IN(i,j) = right_buf_in[kk++];
      }
    }

    if (left_nbr != -1) {
      MPI_Wait(&(request[2+4]), &(status[2+4]));
      MPI_Wait(&(request[3+4]), &(status[3+4]));
      for (kk=0,j=jstart_rank; j<=jend_rank; j++) 
      for (i=istart_rank-RADIUS; i<=istart_rank-1; i++) {
        IN(i,j) = left_buf_in[kk++];
      }
    }

    /* Apply the stencil operator */
    for (j=MAX(jstart_rank,RADIUS); j<=MIN(n-RADIUS-1,jend_rank); j++) {
      for (i=MAX(istart_rank,RADIUS); i<=MIN(n-RADIUS-1,iend_rank); i++) {
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
 
    MPI_Barrier(shm_comm); // needed to avoid writing IN while other ranks are reading it

    /* add constant to solution to force refresh of neighbor data, if any */
    for (j=jstart_rank; j<=jend_rank; j++) 
    for (i=istart_rank; i<=iend_rank; i++) IN(i,j)+= 1.0;

    MPI_Barrier(shm_comm); // needed to avoid reading IN while other ranks are writing it
 
  } /* end of iterations                                                   */
 
  local_stencil_time = wtime() - local_stencil_time;
  MPI_Reduce(&local_stencil_time, &stencil_time, 1, MPI_DOUBLE, MPI_MAX, root,
             MPI_COMM_WORLD);
  
  /* compute L1 norm in parallel                                                */
  local_norm = (DTYPE) 0.0;
  for (j=MAX(jstart_rank,RADIUS); j<=MIN(n-RADIUS-1,jend_rank); j++) {
    for (i=MAX(istart_rank,RADIUS); i<=MIN(n-RADIUS-1,iend_rank); i++) {
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
 
  MPI_Finalize();
  exit(EXIT_SUCCESS);
}
