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
  
USAGE:   The program takes as input the matrix order, the number of times 
         the matrix-matrix multiplication is carried out, the outer level
         block size, and a flag determining whether to use square tiles
         for the local dgemm.

         <progname> <# iterations> <matrix order> <block size> [<tile flag>]
  
         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.

FUNCTIONS CALLED:

         Other than OpenMP or standard C functions, the following 
         functions are used in this program:

         wtime()
         bail_out()

NOTES:   Derived frmo SUMMA implementation provided by Robert Van de Geijn,
         U. Texas at Austion.
         Algorithm first published by Elmroth, Gustavson, Jonsson & Kagstrom

HISTORY: Written by Rob Van der Wijngaart, December 2007
  
***********************************************************************************/

#include "par-res-kern_general.h"
#include "par-res-kern_fg-mpi.h"

#define A(i,j) (a[(j)*lda+i])
#define B(i,j) (b[(j)*ldb+i])
#define C(i,j) (c[(j)*ldc+i])
#ifndef BOFFSET
  #define BOFFSET 12
#endif
#define AA(i,j) (aa[(j)*ldaa+i])
#define BB(i,j) (bb[(j)*ldbb+i])
#define CC(i,j) (cc[(j)*ldcc+i])

#define epsilon  0.00001

void RING_Bcast(double *, int, MPI_Datatype, int, MPI_Comm);
void dlacpy(int, int, double *, int, double *, int);
void dgemm_local(int, int, int, double *, int, double *, int, 
                 double *, int, int, int);
void dgemm(int, int, int, double *, int, double *, int, double *, int,
                int *, int *, MPI_Comm, MPI_Comm, double *, double *);


int main(int argc, char *argv[])
{
  int my_ID, myrow, mycol, /* my index and row and column index    */
      root=0,           /* ID of root rank                         */
      Num_procs,        /* number of ranks                         */
      nprow, npcol,     /* row, column dimensions of rank grid     */
      order,            /* matrix order                            */
      mynrows, myfrow,  /* my number of rows and index of first row*/
      mylrow,           /* and last row                            */
      myncols, myfcol,  /* my number of cols and index of first row*/
      mylcol,           /* and last row                            */
      *mm,              /* arrays that hold m_i's and n_j's        */
      *nn,
      nb,               /* block factor for SUMMA                  */
      inner_block_flag, /* flag to select local DGEMM blocking     */
      error=0,          /* error flag                              */
      *ranks,           /* work array for row and column ranks     */
      lda, ldb, ldc,    /* leading array dimensions of a, b, and c */
      i, j, ii, jj,     /* dummy variables                         */
      iter, iterations;
  double *a, *b, *c,    /* arrays that hold local a, b, c          */
      *work1, *work2,   /* work arrays to pass to dpmmmult         */
      local_dgemm_time, /* timing parameters                       */
      dgemm_time,
      avgtime; 
  double
      forder, nflops,   /* float matrix order + total flops        */
      checksum,         /* array checksum for verification test    */
      checksum_local=0.0,
      ref_checksum;     /* reference checkcum for verification     */
  MPI_Group world_group, 
      temp_group;
  MPI_Comm comm_row,    /* communicators for row and column ranks  */
      comm_col;         /* of rank grid                            */
  int procsize;         /* number of ranks per OS process          */

  /* initialize                                                    */
  MPI_Init(&argc,&argv);
  MPI_Comm_rank( MPI_COMM_WORLD, &my_ID );
  MPI_Comm_size( MPI_COMM_WORLD, &Num_procs );

/*********************************************************************
** process, test and broadcast input parameters
*********************************************************************/

  if (my_ID == root) {
    if (argc != 5) {
      printf("Usage: %s <# iterations> <matrix order> <outer block size> ",
                                                               *argv);
      printf("<local block flag (non-zero=yes, zero=no)>\n");
      error = 1;
      goto ENDOFTESTS;
    }

    iterations  = atoi(*++argv);
    if(iterations < 1){
      printf("ERROR: iterations must be positive: %d \n",iterations);
      error = 1;
      goto ENDOFTESTS;
    }

    order = atoi(*++argv);
    if (order < Num_procs) {
      printf("ERROR: matrix order too small: %d\n", order);
      error = 1;
      goto ENDOFTESTS;
    }

    nb = atoi(*++argv);
    /* a non-positive tile size means no outer level tiling        */

    inner_block_flag = atoi(*++argv);
    
    ENDOFTESTS:;
  }
  bail_out(error);

  MPI_Bcast(&order,  1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&iterations, 1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&nb, 1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&inner_block_flag, 1, MPI_INT, root, MPI_COMM_WORLD);

  /* compute rank grid to most closely match a square; to do so,
     compute largest divisor of Num_procs, using hare-brained method. 
     The small term epsilon is used to guard against roundoff errors 
     in case Num_procs is a perfect square                         */
  nprow = (int) (sqrt((double) Num_procs + epsilon));
  while (Num_procs%nprow) nprow--;
  npcol = Num_procs/nprow;

  if (my_ID == root) {
    MPIX_Get_collocated_size(&procsize);
    printf("FG_MPI Dense matrix-matrix multiplication: C = A x B\n");
    printf("Number of ranks          = %d\n", Num_procs);
    printf("Number of ranks/process  = %d\n", procsize);
    printf("Ranks grid               = %d rows x %d columns\n", nprow, npcol); 
    printf("Matrix order             = %d\n", order);
    printf("Outer block size         = %d\n", nb);
    printf("Number of iterations     = %d\n", iterations);
    if (inner_block_flag)
      printf("Using local dgemm blocking\n");
    else
      printf("No local dgemm blocking\n");
  }

  /* set up row and column communicators                           */

  ranks = (int *) malloc (3*Num_procs*sizeof(int));
  if (!ranks) {
    printf("ERROR: Proc %d could not allocate rank work arrays\n",
           my_ID);
    error = 1;
  }
  bail_out(error);
  mm = ranks + Num_procs;
  nn = mm + Num_procs;

  /* 1. extract group of ranks that make up WORLD                  */
  MPI_Comm_group( MPI_COMM_WORLD, &world_group );

  /* 2. create list of all ranks in same row of rank grid          */
  ranks[0] = my_ID/npcol * npcol;
  for (i=1; i<npcol; i++) ranks[i] = ranks[i-1] + 1;

  /* create row group and communicator                             */
  MPI_Group_incl( world_group, npcol, ranks, &temp_group );
  MPI_Comm_create( MPI_COMM_WORLD, temp_group, &comm_row );

  /* 3. create list of all ranks in same column of rank grid       */
  ranks[0] = my_ID%npcol;
  for (i=1; i<nprow; i++) ranks[i] = ranks[i-1] + npcol;

  /* create column group and communicator                          */
  MPI_Group_incl( world_group, nprow, ranks, &temp_group );
  MPI_Comm_create( MPI_COMM_WORLD, temp_group, &comm_col );

  /* extract this node's row and column index                      */
  MPI_Comm_rank( comm_row, &mycol );
  MPI_Comm_rank( comm_col, &myrow );

  /* mynrows = number of rows assigned to me; distribute excess
     rows evenly if nprow does not divide matrix order evenly      */
  if (myrow < order%nprow) mynrows = (order/nprow)+1;
  else                     mynrows = (order/nprow);

  /* make sure lda is a multiple of the block size nb              */
  if (mynrows%nb==0 || mynrows<nb) lda = mynrows;
  else                             lda = (mynrows/nb+1)*nb;

  /* myncols = number of colums assigned to me; distribute excess
     columns evenly if npcol does not divide order evenly          */
  if (mycol < order%npcol) myncols = (order/npcol)+1;
  else                     myncols = (order/npcol);

  /* get space for local blocks of A, B, C                         */
  a = (double *) malloc( lda*myncols*sizeof(double) );
  b = (double *) malloc( lda*myncols*sizeof(double) );
  c = (double *) malloc( lda*myncols*sizeof(double) );
  if ( a == NULL || b == NULL || c == NULL ) {
    error = 1;
    printf("ERROR: Proc %d could not allocate a, b, and/or c\n",my_ID);
  }
  bail_out(error);

  /* get space for two work arrays for dgemm                       */
  work1 = (double *) malloc( nb*lda*sizeof(double) );
  work2 = (double *) malloc( nb*myncols*sizeof(double) );
  if ( !work1 || !work2 ) {
    printf("ERROR: Proc %d could not allocate work buffers\n", my_ID);
    error = 1;
  }  
  bail_out(error);

  /* collect array that holds mynrows from all nodes in my row
     of the rank grid (array of all m_i)                           */
  MPI_Allgather( &mynrows, 1, MPI_INT, mm, 1, MPI_INT, comm_col );

  /* myfrow = first row on my node                                 */
  for (myfrow=1,i=0; i<myrow; i++) myfrow += mm[i];
  mylrow = myfrow+mynrows-1;

  /* collect array that holds myncols from all nodes in my column 
     of the rank grid (array of all n_j)                           */
  MPI_Allgather( &myncols, 1, MPI_INT, nn, 1, MPI_INT, comm_row );
  /* myfcol = first col on my node                                 */
  for (myfcol=1,i=0; i<mycol; i++) myfcol += nn[i];
  mylcol = myfcol+myncols-1;

  /* initialize matrices A, B, and C                               */
  ldc = ldb = lda;
  for (jj=0, j=myfcol; j<=mylcol; j++,jj++ ) 
  for (ii=0, i=myfrow; i<=mylrow; i++, ii++ ) {
    A(ii,jj) = (double) (j-1); 
    B(ii,jj) = (double) (j-1); 
    C(ii,jj) = 0.0;
  }

  for (iter=0; iter<=iterations; iter++) {

    /* start timer after a warmup iteration */
    if (iter == 1) { 
      MPI_Barrier(MPI_COMM_WORLD);
      local_dgemm_time = wtime();
    }

    /* actual matrix-vector multiply                               */
    dgemm(order, nb, inner_block_flag, a, lda, b, lda, c, lda, 
          mm, nn, comm_row, comm_col, work1, work2 );  

  } /* end of iterations                                           */

  local_dgemm_time = wtime() - local_dgemm_time;
  MPI_Reduce(&local_dgemm_time, &dgemm_time, 1, MPI_DOUBLE, MPI_MAX, root,
             MPI_COMM_WORLD);

  /* verification test                                             */
  for (jj=0, j=myfcol; j<=mylcol; j++, jj++) 
  for (ii=0, i=myfrow; i<=mylrow; i++, ii++)
    checksum_local += C(ii,jj);

  MPI_Reduce(&checksum_local, &checksum, 1, MPI_DOUBLE, MPI_SUM, 
             root, MPI_COMM_WORLD);
 
  forder = (double) order;
  ref_checksum = (0.25*forder*forder*forder*(forder-1.0)*(forder-1.0));
  ref_checksum *= (iterations+1);

  if (my_ID == root) { 
    if (ABS((checksum - ref_checksum)/ref_checksum) > epsilon) {
      printf("ERROR: Checksum = %lf, Reference checksum = %lf\n",
             checksum, ref_checksum);
      error = 1;
    }
    else {
      printf("Solution validates\n");
#ifdef VERBOSE
      printf("Reference checksum = %lf, checksum = %lf\n", 
             ref_checksum, checksum);
#endif
    }
  }
  bail_out(error);

  /* report elapsed time                                           */
  nflops = 2.0*forder*forder*forder;
  if ( my_ID == root ) {
      avgtime = dgemm_time/iterations;
      printf("Rate (MFlops/s): %lf Avg time (s): %lf\n",
             1.0E-06 * nflops/avgtime, avgtime);
  }

  MPI_Finalize();
}

void dgemm(k, nb, inner_block_flag, a, lda, b, ldb, c, ldc, mm, nn, 
           comm_row, comm_col, work1, work2 )
int    k,               /* global matrix dimensions                */
       nb,              /* panel width                             */
       inner_block_flag,/* determines local dgemm blocking         */
       mm[], nn[],      /* dimensions of blocks of A, B, C         */
       lda, ldb, ldc;   /* leading dimension of local arrays that 
                           hold local portions of matrices A, B, C */
double *a, *b, *c,      /* arrays that hold local parts of A, B, C */
       *work1, *work2;  /* work arrays                             */
MPI_Comm comm_row,      /* Communicator for this row of nodes      */
       comm_col;        /* Communicator for this column of nodes   */
{
  int myrow, mycol,     /* my  row and column index                */
      nprow, npcol,     /* number of node rows and columns         */
      i, j, kk, updt,   /* misc. index variables                   */
      currow, curcol,   /* index of row and column that hold current 
                           row and column, resp., for rank-1 update*/
      ii, jj;           /* local index (on currow and curcol, resp.)
                           of row and column for rank-updt update  */
  int my_ID;

  /* get row, column, and global MPI rank                          */
  MPI_Comm_rank(comm_row, &mycol);  
  MPI_Comm_rank(comm_col, &myrow);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_ID);

  /* This routine does a rank "updt" update of the matrix block
     owned by the calling rank. This requires updt whole columns
     of A and updt whole rows of B. The value of updt is determined
     as the minimum of 1. the maximum number of remaining columns 
     of A and 2. the maximum number of remaining rows of B, 
     respectively, that can be gathered from the current row and column 
     of the rank grid, with an overall maximum of the block factor nb.
     We keep track of how many matrix columns and rows of the current 
     rank grid row and column have been visited through the
     indices ii and jj. When a certain rank grid row or column 
     has been exhausted, we move to the next row or column (increment
     currow or curcol) and reset ii or jj to zero                  */

  currow = curcol = ii = jj = 0;
  updt = nb;

  for ( kk=0; kk<k; kk+=updt) {
    updt = MIN(updt,mm[currow]-ii);
    updt = MIN(updt,nn[curcol]-jj);

    /* pack current "updt" columns of A into work1                 */
    if ( mycol == curcol ) 
       dlacpy(mm[myrow], updt, &A(0,jj), lda, work1, mm[myrow]);

    /* pack current "updt" rows of B into work2                    */
    if ( myrow == currow ) 
       dlacpy(updt, nn[mycol], &B(ii,0), ldb, work2, updt );

    /* broadcast work1 and work2                                   */
    RING_Bcast(work1, mm[myrow]*updt, MPI_DOUBLE, curcol, comm_row); 
    RING_Bcast(work2, nn[mycol]*updt, MPI_DOUBLE, currow, comm_col); 

    /* update local block                                          */
    dgemm_local(mm[myrow], nn[mycol], updt, work1, mm[myrow], 
          work2, updt, c, ldc, nb, inner_block_flag);

    /* update curcol, currow, ii, jj                             */
    ii += updt;           jj += updt;
    if (jj>=nn[curcol]) {curcol++; jj = 0;};
    if (ii>=mm[currow]) {currow++; ii = 0;};
  }
}

void dgemm_local(int M, int N, int K, double *a, int lda, double *b,
           int ldb, double *c, int ldc, int nb, int inner_block_flag) {

  int m, n, k, mg, ng, kg, mm, nn, kk, ldaa, ldbb, ldcc;
  double *aa, *bb, *cc;

  if (nb >= MAX(M,MAX(N,K)) || !inner_block_flag) {
    for (m=0; m<M; m++) 
    for (n=0; n<N; n++)
    for (k=0; k<K; k++)
      C(m,n) += A(m,k)*B(k,n);
  }
  else {
    aa = (double *) malloc(3*(nb+BOFFSET)*nb*sizeof(double));
    /* if this allocation fails, we make an ungraceful exit; we do not
       want to test whether any other ranks failed, because that
       requires an expensive barrier                               */
    if (!aa) MPI_Abort(MPI_COMM_WORLD,666);
    bb = aa + (nb+BOFFSET)*nb;
    cc = bb + (nb+BOFFSET)*nb;
    /* select leading dimensions of tiles such that storage
       is (mostly) contiguous                                 */
    ldaa = MIN(M,(nb+BOFFSET));
    ldbb = MIN(N,(nb+BOFFSET));
    ldcc = MIN(M,(nb+BOFFSET));
    for(nn = 0; nn < N; nn+=nb){
      for(kk = 0; kk < K; kk+=nb) {

        for (ng=nn,n=0; ng<MIN(nn+nb,N); n++,ng++) 
        for (kg=kk,k=0; kg<MIN(kk+nb,K); k++,kg++) 
          BB(n,k) =  B(kg,ng);

        for(mm = 0; mm < M; mm+=nb){

          for (kg=kk,k=0; kg<MIN(kk+nb,K); k++,kg++)
          for (mg=mm,m=0; mg<MIN(mm+nb,M); m++,mg++)
            AA(m,k) = A(mg,kg);

          for (ng=nn,n=0; ng<MIN(nn+nb,N); n++,ng++) 
          for (mg=mm,m=0; mg<MIN(mm+nb,M); m++,mg++)
            CC(m,n) = 0.0;
       
          for (kg=kk,k=0; kg<MIN(kk+nb,K); k++,kg++)
          for (ng=nn,n=0; ng<MIN(nn+nb,N); n++,ng++) 
          for (mg=mm,m=0; mg<MIN(mm+nb,M); m++,mg++)
            CC(m,n) += AA(m,k)*BB(n,k);

          for (ng=nn,n=0; ng<MIN(nn+nb,N); n++,ng++) 
          for (mg=mm,m=0; mg<MIN(mm+nb,M); m++,mg++)
            C(mg,ng) += CC(m,n);
        }
      }
    }
    free (aa);
  }
  return;
}

void dlacpy(int m, int n, double *a, int lda, double *b, int ldb ) {

  int i, j;
  for (j=0; j<n; j++) for (i=0; i<m; i++)
    B(i,j) = A(i,j);

  return;
}

void RING_Bcast(double *buf, int count, MPI_Datatype type, int root, 
              MPI_Comm comm )
{
  int my_ID, Num_procs;
  MPI_Status status;

  MPI_Comm_rank( comm, &my_ID );    MPI_Comm_size( comm, &Num_procs );
  if (my_ID != root) 
    MPI_Recv(buf, count, type, (my_ID-1+Num_procs)%Num_procs, root, 
             comm, &status);
  if (( my_ID+1 )%Num_procs != root)
    MPI_Send(buf, count, type, (my_ID+1)%Num_procs, root, comm);
}
