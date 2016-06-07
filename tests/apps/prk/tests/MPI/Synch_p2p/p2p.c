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

               <progname> <# iterations> <m> <n>
  
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
#include <par-res-kern_mpi.h>

#define ARRAY(i,j) vector[i+1+(j)*(segment_size+1)]

int main(int argc, char ** argv)
{
  int    my_ID;         /* MPI rank                                              */
  int    root;          /* ID of root rank                                       */
  int    m, n;          /* grid dimensions                                       */
  double local_pipeline_time, /* timing parameters                               */
         pipeline_time,
         avgtime;
  double epsilon = 1.e-8; /* error tolerance                                     */
  double corner_val;    /* verification value at top right corner of grid        */
  int    i, j, jj, iter, ID;/* dummies                                           */
  int    iterations;    /* number of times to run the pipeline algorithm         */
  int    start, end;    /* start and end of grid slice owned by calling rank     */
  int    segment_size;  /* size of x-dimension of grid owned by calling rank     */
  int    error=0;       /* error flag                                            */
  int    Num_procs;     /* Number of ranks                                       */
  int    grp;           /* grid line aggregation factor                          */
  int    jjsize;        /* actual line group size                                */
  double *vector;       /* array holding grid values                             */
  double *inbuf, *outbuf; /* communication buffers used when aggregating         */
  long   total_length;  /* total required length to store grid values            */
  MPI_Status status;    /* completion status of message                          */

/*********************************************************************************
** Initialize the MPI environment
**********************************************************************************/
  MPI_Init(&argc,&argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_ID);
  MPI_Comm_size(MPI_COMM_WORLD, &Num_procs);

/* we set root equal to the highest rank, because this is also
   the rank that reports on the verification value                               */
  root = Num_procs-1;

/*********************************************************************
** process, test and broadcast input parameter
*********************************************************************/

  if (my_ID == root){
    if (argc != 4 && argc != 5){
      printf("Usage: %s  <#iterations> <1st array dimension> <2nd array dimension> [group factor]\n", 
             *argv);
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

    if (m<=Num_procs) {
      printf("ERROR: First grid dimension %d must be >= number of ranks %d\n", 
             m, Num_procs);
      error = 1;
      goto ENDOFTESTS;
    }

    if (argc==5) {
      grp = atoi(*++argv);
      if (grp < 1) grp = 1;
      else if (grp >= n) grp = n-1;
    }
    else grp = 1;

    ENDOFTESTS:;
  }
  bail_out(error); 

  if (my_ID == root) {
    printf("MPI pipeline execution on 2D grid\n");
    printf("Number of ranks                = %d\n",Num_procs);
    printf("Grid sizes                     = %d, %d\n", m, n);
    printf("Number of iterations           = %d\n", iterations);
    if (grp > 1)
    printf("Group factor                   = %d (cheating!)\n", grp);
  }
  
  /* Broadcast benchmark data to all rankes */
  MPI_Bcast(&m,          1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&n,          1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&grp,        1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&iterations, 1, MPI_INT, root, MPI_COMM_WORLD);

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
  segment_size = end - start + 1;

  /* total_length takes into account one ghost cell on left side of segment     */
  total_length = ((end-start+1)+1)*n;
  vector = (double *) malloc(total_length*sizeof(double));
  if (vector == NULL) {
    printf("Could not allocate space for grid slice of %d by %d points", 
           segment_size, n);
    printf(" on rank %d\n", my_ID);
    error = 1;
  }
  bail_out(error);
  
  /* reserve space for in and out buffers                                        */
  inbuf = (double *) malloc(2*sizeof(double)*(grp));
  if (inbuf == NULL) {
    printf("Could not allocate space for %d words of communication buffers", 
            2*grp);
    printf(" on rank %d\n", my_ID);
    error = 1;
  }
  bail_out(error);
  outbuf = inbuf + grp;
   
  /* clear the array                                                             */
  for (j=0; j<n; j++) for (i=start-1; i<=end; i++) {
    ARRAY(i-start,j) = 0.0;
  }
  /* set boundary values (bottom and left side of grid */
  if (my_ID==0) for (j=0; j<n; j++) ARRAY(0,j) = (double) j;
  for (i=start-1; i<=end; i++)      ARRAY(i-start,0) = (double) i;

  /* redefine start and end for calling rank to reflect local indices            */
  if (my_ID==0) start = 1; 
  else          start = 0;
  end = segment_size-1;

  for (iter=0; iter<=iterations; iter++) {

    /* start timer after a warmup iteration */
    if (iter == 1) { 
      MPI_Barrier(MPI_COMM_WORLD);
      local_pipeline_time = wtime();
    }

    /* execute pipeline algorithm for grid lines 1 through n-1 (skip bottom line) */
    if (grp==1) for (j=1; j<n; j++) { /* special case for no grouping             */

      /* if I am not at the left boundary, I need to wait for my left neighbor to
         send data                                                                */
      if (my_ID > 0) {
        MPI_Recv(&(ARRAY(start-1,j)), 1, MPI_DOUBLE, my_ID-1, j, 
                                  MPI_COMM_WORLD, &status);
      }

      for (i=start; i<= end; i++) {
        ARRAY(i,j) = ARRAY(i-1,j) + ARRAY(i,j-1) - ARRAY(i-1,j-1);
      }

      /* if I am not on the right boundary, send data to my right neighbor        */  
      if (my_ID < Num_procs-1) {
        MPI_Send(&(ARRAY(end,j)), 1, MPI_DOUBLE, my_ID+1, j, MPI_COMM_WORLD);
      }
    }
    else for (j=1; j<n; j+=grp) { /* apply grouping                               */

      jjsize = MIN(grp, n-j);
      /* if I am not at the left boundary, I need to wait for my left neighbor to
         send data                                                                */
      if (my_ID > 0) {
        MPI_Recv(inbuf, jjsize, MPI_DOUBLE, my_ID-1, j, MPI_COMM_WORLD, &status);
        for (jj=0; jj<jjsize; jj++) {
          ARRAY(start-1,jj+j) = inbuf[jj];
	}
      }

      for (jj=0; jj<jjsize; jj++) for (i=start; i<= end; i++) {
        ARRAY(i,jj+j) = ARRAY(i-1,jj+j) + ARRAY(i,jj+j-1) - ARRAY(i-1,jj+j-1);
      }

      /* if I am not on the right boundary, send data to my right neighbor        */  
      if (my_ID < Num_procs-1) {
        for (jj=0; jj<jjsize; jj++) {
          outbuf[jj] = ARRAY(end,jj+j);
        }
        MPI_Send(outbuf, jjsize, MPI_DOUBLE, my_ID+1, j, MPI_COMM_WORLD);
      }

    }

    /* copy top right corner value to bottom left corner to create dependency     */
    if (Num_procs >1) {
      if (my_ID==root) {
        corner_val = -ARRAY(end,n-1);
        MPI_Send(&corner_val,1,MPI_DOUBLE,0,888,MPI_COMM_WORLD);
      }
      if (my_ID==0) {
        MPI_Recv(&(ARRAY(0,0)),1,MPI_DOUBLE,root,888,MPI_COMM_WORLD,&status);
      }
    }
    else ARRAY(0,0)= -ARRAY(end,n-1);

  }

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
    /* flip the sign of the execution time to indicate cheating                    */
    if (grp>1) avgtime *= -1.0;
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

