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
 
NAME:    Pipeline
 
PURPOSE: This program tests the efficiency with which point-to-point
         synchronization can be carried out. It does so by executing 
         a pipelined algorithm on an m*n grid. The first array dimension
         is distributed among the ranks (stripwise decomposition).
  
USAGE:   The program takes as input the dimensions of the grid, and the
         number of times we loop over the grid
 
               p2p <#threads> <# iterations> <m> <n>
  
         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.
 
FUNCTIONS CALLED:
 
         Other than MPI or standard C functions, the following 
         functions are used in this program:
 
         wtime()
         bail_out()
 
HISTORY: - Written by Rob Van der Wijngaart, March 2006.
         - modified by Rob Van der Wijngaart, August 2006:
            * changed boundary conditions and stencil computation to avoid 
              overflow
            * introduced multiple iterations over grid and dependency between
              iterations
  
**********************************************************************************/
 
#include <par-res-kern_general.h>
#include <par-res-kern_mpiomp.h>
 
 
/* define shorthand for flag with cache line padding                             */ 
#define LINEWORDS  16 
#define flag(TID,j)    flag[((TID)+(j)*nthread)*LINEWORDS] 
#define ARRAY(i,j) vector[i+1+(j)*(segment_size+1)]
double *vector; /* array holding grid values                                     */
 
int main(int argc, char ** argv)
{
  int    my_ID;         /* rank                                                  */
  int    TID;           /* thread ID                                             */
  int    root;
  int    m, n;          /* grid dimensions                                       */
  double local_pipeline_time, /* timing parameters                               */
         pipeline_time,
         avgtime;
  double epsilon = 1.e-8; /* error tolerance                                     */
  double corner_val;    /* verification value at top right corner of grid        */
  int    i, j, iter, ID;/* dummies                                               */
  int    iterations;    /* number of times to run the pipeline algorithm         */
  int    start, end;    /* start and end of grid slice owned by calling rank     */
  int    segment_size;
  int    *flag;         /* used for pairwise synchronizations                    */
  int    *tstart, *tend;/* starts and ends of grid slices for respective threads */
  int    *tsegment_size;
  int    nthread;       /* number of threads                                     */
  int    error=0;       /* error flag                                            */
  int    Num_procs;     /* Number of ranks                                       */
  char  *name;          /* MPI threading mode suffix name                        */
  long   total_length;  /* total required length to store grid values            */
  MPI_Status status;    /* completion status of message                          */
  int    provided;      /* MPI level of thread support                           */
  int    true, false;   /* toggled booleans used for synchronization             */
 
/*********************************************************************************
** Initialize the MPI environment
**********************************************************************************/
  MPI_Init_thread(&argc,&argv, MPI_THREAD_MULTIPLE, &provided);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_ID);
  switch (provided) {
    case  MPI_THREAD_SERIALIZED: error=1; name="SERIALIZED"; break;
    case  MPI_THREAD_FUNNELED:   error=1; name="FUNNELED";   break;
    case  MPI_THREAD_SINGLE:     error=1; name="SINGLE";     break;
    case  MPI_THREAD_MULTIPLE:   error=0;                    break;
    default:                     error=1; name="UNKNOWN";    break;
  }
  if (error) {
    if (my_ID==0) printf("ERROR: need MPI_THREAD_MULTIPLE but gets MPI_THREAD_%s\n",
                         name);
  }
  bail_out(error);
 
  MPI_Comm_size(MPI_COMM_WORLD, &Num_procs);
 
/* we set root equal to highest rank, because this is also the rank 
   that reports on the verification value                           */
  root = Num_procs-1;
 
/*********************************************************************
** process, test and broadcast input parameter
*********************************************************************/
 
  if (my_ID == root){
    if (argc != 5){
      printf("Usage: %s <#threads> <#iterations> <1st array dimension> <2nd array dimension>\n", 
             *argv);
      error = 1;
      goto ENDOFTESTS;
    }
 
    /* Take number of threads to request from command line */
    nthread = atoi(*++argv); 
    if ((nthread < 1) || (nthread > MAX_THREADS)) {
      printf("ERROR: Invalid number of threads: %d\n", nthread);
      error = 1; 
      goto ENDOFTESTS; 
    }
 
    iterations = atoi(*++argv);
    if (iterations < 1){
      printf("ERROR: iterations must be >= 1 : %d \n",iterations);
      error = 1;
      goto ENDOFTESTS;
    } 
 
    m = atoi(*++argv);
    n = atoi(*++argv);
    if (m < 1 || n < 1){
      printf("ERROR: grid dimensions must be positive: %d, %d \n", m, n);
      error = 1;
      goto ENDOFTESTS;
    }
 
    if (m<Num_procs) {
      printf("ERROR: First grid dimension %d smaller than number of ranks %d\n", 
             m, Num_procs);
      error = 1;
      goto ENDOFTESTS;
    }
 
    ENDOFTESTS:;
  }
  bail_out(error); 
 
  /* Broadcast benchmark data to all ranks */
  MPI_Bcast(&m,          1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&n,          1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&iterations, 1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&nthread,    1, MPI_INT, root, MPI_COMM_WORLD);
 
  omp_set_num_threads(nthread);
 
  if (my_ID == root) {
    printf("MPI+OpenMP pipeline execution on 2D grid\n");
    printf("Number of ranks                = %i\n",Num_procs);
    printf("Number of threads              = %d\n", omp_get_max_threads());
    printf("Grid sizes                     = %d, %d\n", m, n);
    printf("Number of iterations           = %d\n", iterations);
#ifdef SYNCHRONOUS
    printf("Handshake between neighbor threads\n");
#else
    printf("No handshake between neighbor threads\n");
#endif
  }
 
  int leftover;
  segment_size = m/Num_procs;
  leftover     = m%Num_procs;
  if (my_ID < leftover) {
    start = (segment_size+1)* my_ID;
    end   = start + segment_size;
  }
  else {
    start = (segment_size+1) * leftover + segment_size * (my_ID-leftover);
    end   = start + segment_size -1;
  }
 
  /* now set segment_size to the value needed by the calling rank               */
  segment_size = end-start+1;
 
  /* total_length takes into account one ghost cell on left side of segment     */
  total_length = ((end-start+1)+1)*n;
  vector = (double *) malloc(sizeof(double)*total_length);
  if (vector == NULL) {
    printf("Could not allocate space for grid slice of %d by %d points", 
           segment_size, n);
    printf(" on rank %d\n", my_ID);
    error = 1;
  }
  bail_out(error);
 
  /* now divide the rank's grid slice among the threads                          */
  tstart = (int *) malloc(3*nthread*sizeof(int));
  if (!tstart) {
    printf("ERROR: Could not allocate space for array of slice boundaries\n");
    exit(EXIT_FAILURE);
  }
  tend = tstart + nthread;
  tsegment_size = tend + nthread;
 
  tstart[0] = start;
  for (TID=0; TID<nthread; TID++) {
    tsegment_size[TID] = segment_size/nthread;
    if (TID < (segment_size%nthread)) tsegment_size[TID]++;
    if (TID>0) tstart[TID] = tend[TID-1]+1;
    tend[TID] = tstart[TID]+tsegment_size[TID]-1;
  }
 
  flag = (int *) malloc(sizeof(int)*nthread*LINEWORDS*n);
  if (!flag) {
    printf("ERROR: Could not allocate space for synchronization flags\n");
    exit(EXIT_FAILURE);
  }  
 
#pragma omp parallel private(i, j, iter, true, false)
  {
  int TID = omp_get_thread_num();
 
  /* clear the array                                                             */
  for (j=0; j<n; j++) 
  for (i=tstart[TID]-1; i<=tend[TID]; i++) {
    ARRAY(i-start,j) = 0.0;
  }
  /* set boundary values (bottom and left side of grid                           */
  if (my_ID==0 && TID==0) for (j=0; j<n; j++) ARRAY(tstart[TID]-start,j) = (double) j;
  for (i=tstart[TID]-1; i<=tend[TID]; i++) {
    ARRAY(i-start,0) = (double) i;
  }
 
  #pragma omp barrier
 
  if (TID==0) {
    /* redefine start and end for calling rank to reflect local indices           */
    if (my_ID==0) start = 1; 
    else          start = 0;
    end = segment_size-1;
  
    /* redefine tstart and tend for calling thread to reflect local indices       */
    tstart[0] = start;
    tend[0] = tsegment_size[0]-1;
    for (ID=1; ID<nthread; ID++) {
      tstart[ID] = tend[ID-1]+1;
      tend[ID]   = tstart[ID]+tsegment_size[ID]-1;
    }
  }

  /* set flags to zero to indicate no data is available yet                       */
  true = 1; false = !true;
  for (j=0; j<n; j++) flag(TID,j) = 0;

  /* need barrier after setting flags, to make sure each is visible to all threads 
     and to synchronize before iterations start                                   */
  #pragma omp barrier  
 
  for (iter=0; iter<=iterations; iter++) {
 
#ifndef SYNCHRONOUS
    /* true and false toggle each iteration                                      */
    true = (iter+1)%2; false = !true;
#endif

    /* start timer after a warmup iteration */
    if (iter == 1) { 
      #pragma omp barrier
      if (TID==0) {
        MPI_Barrier(MPI_COMM_WORLD);
        local_pipeline_time = wtime();
      }
    }

    if ((Num_procs==1) && (TID==0)) { /* first thread waits for corner value       */
      while (flag(0,0) == true) {
        #pragma omp flush
      }
#ifdef SYNCHRONOUS
      flag(0,0)= true;
      #pragma omp flush
#endif      
    }

    /* execute pipeline algorithm for grid lines 1 through n-1 (skip bottom line) */
    for (j=1; j<n; j++) {
 
      /* if I am not at the left boundary, I need to wait for my left neighbor to
         send data                                                                */
      if (TID==0){
        if (my_ID > 0) {
          MPI_Recv(&(ARRAY(start-1,j)), 1, MPI_DOUBLE, my_ID-1, j, 
                   MPI_COMM_WORLD, &status);
        }
      }
      else {
	while (flag(TID-1,j) == false) {
           #pragma omp flush
        }
#ifdef SYNCHRONOUS
        flag(TID-1,j)= false;
        #pragma omp flush
#endif      
      }
 
      for (i=tstart[TID]; i<= tend[TID]; i++) {
        ARRAY(i,j) = ARRAY(i-1,j) + ARRAY(i,j-1) - ARRAY(i-1,j-1);
      }
 
      /* if not on right boundary, signal right neighbor it has new data */
      if (TID < nthread-1) {
#ifdef SYNCHRONOUS 
        while (flag(TID,j) == true) {
          #pragma omp flush
        }
#endif 
        flag(TID,j) = true;
        #pragma omp flush
      }
      else { /* if not on the right boundary, send data to my right neighbor      */  
        if (my_ID < Num_procs-1) {
          MPI_Send(&(ARRAY(end,j)), 1, MPI_DOUBLE, my_ID+1, j, MPI_COMM_WORLD);
        }
      }
    }
 
    /* copy top right corner value to bottom left corner to create dependency     */
    if (Num_procs>1) {
      if (TID==nthread-1 && my_ID==root) {
        corner_val = -ARRAY(end,n-1);
        MPI_Send(&corner_val,1,MPI_DOUBLE,0,888,MPI_COMM_WORLD);
      }
      if (TID==0  && my_ID==0) {
        MPI_Recv(&(ARRAY(0,0)),1,MPI_DOUBLE,root,888,MPI_COMM_WORLD,&status);
      }
    }
    else {
      if (TID==nthread-1) { /* if on right boundary, copy top right corner value 
                to bottom left corner to create dependency and signal completion  */
        ARRAY(0,0) = -ARRAY(m-1,n-1);
#ifdef SYNCHRONOUS
        while (flag(0,0) == false) {
          #pragma omp flush
        }
        flag(0,0) = false;
#else
        #pragma omp flush
        flag(0,0) = true;
#endif
        #pragma omp flush
      }
    }
 
  } /* end of iterations */
  } /* end of parallel section */
 
  local_pipeline_time = wtime() - local_pipeline_time;
  MPI_Reduce(&local_pipeline_time, &pipeline_time, 1, MPI_DOUBLE, MPI_MAX, root,
             MPI_COMM_WORLD);
 
  /*******************************************************************************
  ** Analyze and output results.
  ********************************************************************************/
 
  /* verify correctness, using top right value                                     */
  corner_val = (double) ((iterations+1)*(m+n-2));
  if (my_ID == root) {
    if (abs(ARRAY(end,n-1)-corner_val)/corner_val >= epsilon) {
      printf("ERROR: checksum %lf does not match verification value %lf\n",
             ARRAY(end,n-1), corner_val);
      error = 1;
    }
  }
  bail_out(error);
 
  if (my_ID == root) {
    avgtime = pipeline_time/iterations;
#ifdef VERBOSE   
    printf("Solution validates; verification value = %lf\n", corner_val);
    printf("Point-to-point synchronizations/s: %lf\n",
           ((float)((n-1)*(Num_procs-1)))/(avgtime));
#else
    printf("Solution validates\n");
#endif
    printf("Rate (MFlops/s): %lf Avg time (s): %lf\n",
           1.0E-06 * 2 * ((double)((m-1)*(n-1)))/avgtime, avgtime);
  }
 
  MPI_Finalize();
  exit(EXIT_SUCCESS);
 
}  /* end of main */
