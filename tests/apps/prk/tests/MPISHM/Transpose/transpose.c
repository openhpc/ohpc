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

NAME:    transpose

PURPOSE: This program tests the efficiency with which a square matrix
         can be transposed and stored in another matrix. The matrices
         are distributed identically.
  
USAGE:   Program inputs are the matrix order, the number of times to 
         repeat the operation, and the communication mode

         transpose <#ranks per coherence domain><# iterations><matrix size> [tile size]

         An optional parameter specifies the tile size used to divide the 
         individual matrix into blocks for improved cache and TLB performance. 
  
         The output consists of diagnostics to make sure the 
         transpose worked and timing statistics.

FUNCTIONS CALLED:

         Other than MPI or standard C functions, the following 
         functions are used in this program:

          wtime()           Portable wall-timer interface.
          bail_out()        Determine global error and exit if nonzero.

HISTORY: Written by Tim Mattson, April 1999.  
         Updated by Rob Van der Wijngaart, December 2005.
         Updated by Rob Van der Wijngaart, October 2006.
         Updated by Rob Van der Wijngaart, November 2014::
         - made variable names more consistent 
         - put timing around entire iterative loop of transposes
         - fixed incorrect matrix block access; no separate function
           for local transpose of matrix block
         - reordered initialization and verification loops to
           produce unit stride
         - changed initialization values, such that the input matrix
           elements are: A(i,j) = i+order*j
         
**********************************************************************************/

#include <par-res-kern_general.h>
#include <par-res-kern_mpi.h>

#define A(i,j)        A_p[(i+istart)+order*(j)]
#define B(i,j)        B_p[(i+istart)+order*(j)]
#define Work_in(i,j)  Work_in_p[i+Block_order*(j)]
#define Work_out(i,j) Work_out_p[i+Block_order*(j)]

int main(int argc, char ** argv)
{
  int Block_order;
  size_t Block_size;
  size_t Colblock_size;
  int Tile_order=32;
  int tiling;
  int Num_procs;     /* Number of ranks                                          */
  int order;         /* overall matrix order                                     */
  int send_to, recv_from; /* communicating ranks                                 */
  size_t bytes;      /* total amount of data to be moved                         */
  int my_ID;         /* rank                                                     */
  int root=0;        /* root rank of a communicator                              */
  int iterations;    /* number of times to run the pipeline algorithm            */
  int i, j, it, jt, ID;/* dummies                                                */
  int iter;          /* index of iteration                                       */
  int phase;         /* phase in the staged communication                        */
  size_t colstart;   /* sequence number of first column owned by calling rank    */
  int error=0;       /* error flag                                               */
  double *A_p;       /* original matrix column block                             */
  double *B_p;       /* transposed matrix column block                           */
  double *Work_in_p; /* workspace for the transpose function                     */
  double *Work_out_p;/* workspace for the transpose function                     */
  double abserr, abserr_tot; /* computed error                                   */
  double epsilon = 1.e-8; /* error tolerance                                     */
  double local_trans_time, /* timing parameters                                  */
         trans_time,
         avgtime;
  MPI_Status status; /* completion status of message                             */
  MPI_Win shm_win_A; /* Shared Memory window object                              */
  MPI_Win shm_win_B; /* Shared Memory window object                              */
  MPI_Win shm_win_Work_in; /* Shared Memory window object                        */
  MPI_Win shm_win_Work_out; /* Shared Memory window object                       */
  MPI_Info rma_winfo;/* info for window                                          */
  MPI_Comm shm_comm_prep;/* Shared Memory prep Communicator                      */
  MPI_Comm shm_comm; /* Shared Memory Communicator                               */
  int shm_procs;     /* # of ranks in shared domain                              */
  int shm_ID;        /* MPI rank within coherence domain                         */
  int group_size;    /* number of ranks per shared memory group                  */
  int Num_groups;    /* number of shared memory group                            */
  int group_ID;      /* sequence number of shared memory group                   */
  int size_mul;      /* size multiplier; 0 for non-root ranks in coherence domain*/
  int istart;
  MPI_Request send_req, recv_req;

/*********************************************************************************
** Initialize the MPI environment
**********************************************************************************/
  MPI_Init(&argc,&argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_ID);
  MPI_Comm_size(MPI_COMM_WORLD, &Num_procs);

  root = 0;

/*********************************************************************
** process, test and broadcast input parameter
*********************************************************************/

  if (my_ID == root){
    if (argc != 4 && argc !=5){
      printf("Usage: %s  <#ranks per coherence domain> <# iterations> <matrix order> [tile size]\n", 
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
      printf("ERROR: toal # %d ranks not divisible by ranks per coherence domain %d\n",
	     Num_procs, group_size);
      error = 1;
      goto ENDOFTESTS;
    } 

    iterations = atoi(*++argv);
    if (iterations < 1){
      printf("ERROR: iterations must be >= 1 : %d \n",iterations);
      error = 1;
      goto ENDOFTESTS;
    } 

    order = atoi(*++argv);
    if (order < Num_procs) {
      printf("ERROR: matrix order %d should at least # procs %d\n", 
             order, Num_procs);
      error = 1; goto ENDOFTESTS;
    }

    if (order%Num_procs) {
      printf("ERROR: matrix order %d should be divisible by # procs %d\n",
             order, Num_procs);
      error = 1; goto ENDOFTESTS;
    }

    if (argc == 5) Tile_order = atoi(*++argv);

    ENDOFTESTS:;
  }
  bail_out(error); 

  /*  Broadcast input data to all ranks */
  MPI_Bcast(&order,      1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&iterations, 1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&Tile_order, 1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&group_size, 1, MPI_INT, root, MPI_COMM_WORLD);

  if (my_ID == root) {
    printf("MPI+SHM Matrix transpose: B = A^T\n");
    printf("Number of ranks      = %d\n", Num_procs);
    printf("Rank group size      = %d\n", group_size);
    printf("Matrix order         = %d\n", order);
    printf("Number of iterations = %d\n", iterations);
    if ((Tile_order > 0) && (Tile_order < order))
       printf("Tile size            = %d\n", Tile_order);
    else  printf("Untiled\n");
#ifndef SYNCHRONOUS
    printf("Non-");
#endif
    printf("Blocking messages\n");
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

  /* a non-positive tile size means no tiling of the local transpose */
  tiling = (Tile_order > 0) && (Tile_order < order);
  bytes = 2 * sizeof(double) * order * order;

/*********************************************************************
** The matrix is broken up into column blocks that are mapped one to a 
** rank.  Each column block is made up of Num_procs smaller square 
** blocks of order block_order.
*********************************************************************/

  Num_groups     = Num_procs/group_size;
  Block_order    = order/Num_groups;

  group_ID       = my_ID/group_size;
  colstart       = Block_order * group_ID;
  Colblock_size  = order * Block_order;
  Block_size     = Block_order * Block_order;

/*********************************************************************
** Create the column block of the test matrix, the column block of the 
** transposed matrix, and workspace (workspace only if #procs>1)
*********************************************************************/

  /* RMA win info */
  MPI_Info_create(&rma_winfo);
  /* This key indicates that passive target RMA will not be used.
   * It is the one info key that MPICH actually uses for optimization. */
  MPI_Info_set(rma_winfo, "no_locks", "true");

  /* only the root of each SHM domain specifies window of nonzero size */
  size_mul = (shm_ID==0);  
  int offset = 32;
  MPI_Aint size= (Colblock_size+offset)*sizeof(double)*size_mul; int disp_unit;
  MPI_Win_allocate_shared(size, sizeof(double), rma_winfo, shm_comm, 
                          (void *) &A_p, &shm_win_A);
  MPI_Win_shared_query(shm_win_A, MPI_PROC_NULL, &size, &disp_unit, (void *)&A_p);
  if (A_p == NULL){
    printf(" Error allocating space for original matrix on node %d\n",my_ID);
    error = 1;
  }
  bail_out(error);
  A_p += offset;

  MPI_Win_allocate_shared(size, sizeof(double), rma_winfo, shm_comm, 
                          (void *) &B_p, &shm_win_B);
  MPI_Win_shared_query(shm_win_B, MPI_PROC_NULL, &size, &disp_unit, (void *)&B_p);
  if (B_p == NULL){
    printf(" Error allocating space for transposed matrix by group %d\n",group_ID);
    error = 1;
  }
  bail_out(error);
  B_p += offset;

  if (Num_groups>1) {

    size = Block_size*sizeof(double)*size_mul;
    MPI_Win_allocate_shared(size, sizeof(double),rma_winfo, shm_comm, 
                           (void *) &Work_in_p, &shm_win_Work_in);
    MPI_Win_shared_query(shm_win_Work_in, MPI_PROC_NULL, &size, &disp_unit, 
                         (void *)&Work_in_p);
    if (Work_in_p == NULL){
      printf(" Error allocating space for in block by group %d\n",group_ID);
      error = 1;
    }
    bail_out(error);

    MPI_Win_allocate_shared(size, sizeof(double), rma_winfo, 
                            shm_comm, (void *) &Work_out_p, &shm_win_Work_out);
    MPI_Win_shared_query(shm_win_Work_out, MPI_PROC_NULL, &size, &disp_unit, 
                         (void *)&Work_out_p);
    if (Work_out_p == NULL){
      printf(" Error allocating space for out block by group %d\n",group_ID);
      error = 1;
    }
    bail_out(error);
  }

  /* Fill the original column matrix                                             */
  istart = 0;  
  int chunk_size = Block_order/group_size;
  if (tiling) {
      for (j=shm_ID*chunk_size;j<(shm_ID+1)*chunk_size;j+=Tile_order) {
      for (i=0;i<order; i+=Tile_order) 
        for (jt=j; jt<MIN((shm_ID+1)*chunk_size,j+Tile_order); jt++)
          for (it=i; it<MIN(order,i+Tile_order); it++) {
            A(it,jt) = (double) (order*(jt+colstart) + it);
            B(it,jt) = -1.0;
          }
    }
  }
  else {
    for (j=shm_ID*chunk_size;j<(shm_ID+1)*chunk_size;j++) 
      for (i=0;i<order; i++) {
        A(i,j) = (double) (order*(j+colstart) + i);
        B(i,j) = -1.0;
      }
  }
  /* NEED A STORE FENCE HERE                                                     */
  MPI_Barrier(shm_comm);

  for (iter=0; iter<=iterations; iter++) {

    /* start timer after a warmup iteration */
    if (iter == 1) { 
      MPI_Barrier(MPI_COMM_WORLD);
      local_trans_time = wtime();
    }

    /* do the local transpose                                                    */
    istart = colstart; 
    if (!tiling) {
      for (i=shm_ID*chunk_size; i<(shm_ID+1)*chunk_size; i++) {
        for (j=0; j<Block_order; j++) 
              B(j,i) = A(i,j);
	}
    }
    else {
      for (i=shm_ID*chunk_size; i<(shm_ID+1)*chunk_size; i+=Tile_order) {
        for (j=0; j<Block_order; j+=Tile_order) 
          for (it=i; it<MIN(Block_order,i+Tile_order); it++)
            for (jt=j; jt<MIN(Block_order,j+Tile_order);jt++) {
              B(jt,it) = A(it,jt); 
	    }
      }
    }

    for (phase=1; phase<Num_groups; phase++){
      recv_from = ((group_ID + phase             )%Num_groups);
      send_to   = ((group_ID - phase + Num_groups)%Num_groups);

#ifndef SYNCHRONOUS
      if (shm_ID==0) {
         MPI_Irecv(Work_in_p, Block_size, MPI_DOUBLE, 
                   recv_from*group_size, phase, MPI_COMM_WORLD, &recv_req);  
      }
#endif

      istart = send_to*Block_order; 
      if (!tiling) {
        for (i=shm_ID*chunk_size; i<(shm_ID+1)*chunk_size; i++) 
          for (j=0; j<Block_order; j++){
	    Work_out(j,i) = A(i,j);
	  }
      }
      else {
        for (i=shm_ID*chunk_size; i<(shm_ID+1)*chunk_size; i+=Tile_order)
          for (j=0; j<Block_order; j+=Tile_order) 
            for (it=i; it<MIN(Block_order,i+Tile_order); it++)
              for (jt=j; jt<MIN(Block_order,j+Tile_order);jt++) {
                Work_out(jt,it) = A(it,jt); 
	      }
      }

      /* NEED A LOAD/STORE FENCE HERE                                            */
      MPI_Barrier(shm_comm);
      if (shm_ID==0) {
#ifndef SYNCHRONOUS  
        MPI_Isend(Work_out_p, Block_size, MPI_DOUBLE, send_to*group_size,
                  phase, MPI_COMM_WORLD, &send_req);
        MPI_Wait(&recv_req, &status);
        MPI_Wait(&send_req, &status);
#else
        MPI_Sendrecv(Work_out_p, Block_size, MPI_DOUBLE, send_to*group_size, 
                     phase, Work_in_p, Block_size, MPI_DOUBLE, 
  	             recv_from*group_size, phase, MPI_COMM_WORLD, &status);
#endif
      }
      /* NEED A LOAD FENCE HERE                                                  */ 
      MPI_Barrier(shm_comm);

      istart = recv_from*Block_order; 
      /* scatter received block to transposed matrix; no need to tile */
      for (j=shm_ID*chunk_size; j<(shm_ID+1)*chunk_size; j++)
        for (i=0; i<Block_order; i++) 
          B(i,j) = Work_in(i,j);

    }  /* end of phase loop  */
  } /* end of iterations */

  local_trans_time = wtime() - local_trans_time;
  MPI_Reduce(&local_trans_time, &trans_time, 1, MPI_DOUBLE, MPI_MAX, root,
             MPI_COMM_WORLD);

  abserr = 0.0;
  istart = 0;
  /*  for (j=shm_ID;j<Block_order;j+=group_size) for (i=0;i<order; i++) { */
  for (j=shm_ID*chunk_size; j<(shm_ID+1)*chunk_size; j++)
    for (i=0;i<order; i++) { 
      abserr += ABS(B(i,j) - (double)(order*i + j+colstart));
    }

  MPI_Reduce(&abserr, &abserr_tot, 1, MPI_DOUBLE, MPI_SUM, root, MPI_COMM_WORLD);

  if (my_ID == root) {
    if (abserr_tot < epsilon) {
      printf("Solution validates\n");
      avgtime = trans_time/(double)iterations;
      printf("Rate (MB/s): %lf Avg time (s): %lf\n",1.0E-06*bytes/avgtime, avgtime);
#ifdef VERBOSE
      printf("Summed errors: %f \n", abserr_tot);
#endif
    }
    else {
      printf("ERROR: Aggregate squared error %e exceeds threshold %e\n", abserr_tot, epsilon);
      error = 1;
    }
  }

  bail_out(error);

  MPI_Win_free(&shm_win_A);
  MPI_Win_free(&shm_win_B);
  if (Num_groups>1) {
      MPI_Win_free(&shm_win_Work_in);
      MPI_Win_free(&shm_win_Work_out);
  }

  MPI_Info_free(&rma_winfo);

  MPI_Finalize();
  exit(EXIT_SUCCESS);

}  /* end of main */

