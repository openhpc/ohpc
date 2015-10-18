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
/*************************************************************
Copyright (c)  2013 The University of Tennessee. All rights reserved.
Redistribution and use in source and binary forms, with or 
without modification, are permitted provided that the following
conditions are met:

- Redistributions of source code must retain the 
  above copyright notice, this list of conditions and 
  the following disclaimer.

- Redistributions in binary form must reproduce the 
  above copyright notice, this list of conditions and 
  the following disclaimer listed in this license in the 
  documentation and/or other materials provided with the 
  distribution.

- Neither the name of the copyright holders nor the names 
  of its contributors may be used to endorse or promote 
  products derived from this software without specific 
  prior written permission.

This software is provided by the copyright holders and 
contributors "as is" and any express or implied warranties, 
including, but not limited to, the implied warranties of
merchantability and fitness for a particular purpose are 
disclaimed. in no event shall the copyright owner or 
contributors be liable for any direct, indirect, incidental, 
special, exemplary, or consequential damages (including, but 
not limited to, procurement of substitute goods or services;
loss of use, data, or profits; or business interruption) 
however caused and on any theory of liability, whether in 
contract, strict liability, or tort (including negligence or 
otherwise) arising in any way out of the use of this software, 
even if advised of the possibility of such damage.

*************************************************************/
/******************************************************************* 
 
NAME:    RandomAccess

PURPOSE: This program tests the efficiency of the memory subsystem to 
         update elements of a distributed array with irregular stride.

USAGE:   The program takes as input the 2log of the size of the table that 
         gets updated and the ratio of table size over number of updates.
         The table is distributed evenly over all participating ranks.
         The code can be vectorized, in principle, with a vector length
         that is automatically set to the size of the LOOKAHEAD parameter.

         <progname> <log2 tablesize> <#update ratio> 

FUNCTIONS CALLED:

         Other than MPI or standard C functions, the following 
         functions are used in this program:

         wtime
         bail_out()
         PRK_starts
         poweroftwo

NOTES:   This program is derived from HPC Challenge Random Access. The random 
         number generator computes successive powers of 0x2, modulo the 
         primitive polynomial x^63+x^2+x+1. The principal differences between 
         this code and the HPCC version are:
         - we start the stream of random numbers not with seed 0x1, but the 
           SEQSEED-th element in the stream of powers of 0x2.
         - the timed code applies the RandomAccess operator twice to the table of 
           computed resuls (starting with the same seed(s) for "ran" in both 
           iterations. The second pass makes sure that any update to any table 
           element that sets high-order bits in the first pass resets those bits 
           to zero.
         - the verification test now simply constitutes checking whether table
           element j equals j.

         The program uses aggragation to reduce latency costs. The parameter
         LOOKAHEAD determines how many random numbers are generated by each 
         rank before they are sent to the ranks that are responsible for
         updating the corresponding table elements. LOOKAHEAD also determines the
         (potential) level of vectorization (independent streams) of the code.

         We note that the vectorized version of this code (i.e. LOOKAHEAD unequal 
         to 1), does not feature exactly the same sequence of accesses and 
         intermediate update values in the table as the scalar version. The 
         reason for this is twofold.
         1. the elements in the stream of powers of 0x2 that get computed outside 
            the main timed loop as seeds for independent streams in the vectorized 
            version, using the jump-ahead function PRK_starts, are computed inside 
            the timed loop for the scalar version. However, since both versions do 
            the same number of timed random accesses, the vectorized version must
            progress further in the sequence of powers of 0x2.
         2. The independent streams of powers of 0x2 computed in the vectorized 
            version can (and will) cause updates of the same elements of the table 
            in an order that is not consistent with the scalar version. That is, 
            distinct values of "ran" can refer to the same table element 
            "ran&(tablesize-1)," but the operation 
            Table[ran&(tablesize-1)] ^= ran will deposit different values in that 
            table element for different values of ran. At the end of each pass over 
            the data set, the table will contain the same values in the vector and 
            scalar version (ignoring the small differences caused by 1.) because of 
            commutativity of the XOR operator. If the update operator had been 
            non-commutative, the vector and scalar version would have yielded 
            different results.

HISTORY: Written by Rob Van der Wijngaart, December 2007.
  
************************************************************************************/

#include <par-res-kern_general.h>
#include <par-res-kern_fg-mpi.h>

/* Define 64-bit types and corresponding format strings for printf() and constants */
/* PERIOD = (2^63-1)/7 = 7*73*127*337*92737*649657                                 */
#ifdef LONG_IS_64BITS 
  #define POLY               0x0000000000000007UL
  #define PERIOD             1317624576693539401L
  /* sequence number in series of random numbers to be used as initial value       */
  #define SEQSEED            834568137686317453L
#else 
  #define POLY               0x0000000000000007ULL
  #define PERIOD             1317624576693539401LL
  /* sequence number in series of random numbers to be used as initial value       */
  #define SEQSEED            834568137686317453LL
#endif 

#ifndef LOOKAHEAD
#define LOOKAHEAD            1024
#endif

static u64Int PRK_starts(s64Int);
static int    poweroftwo(int);

int main(int argc, char **argv) {

  int               update_ratio;/* multiplier of tablesize for # updates          */
  int               nstarts;     /* vector length                                  */
  s64Int            i, j, round, oldsize, index, global_index; /* dummies          */
  int               proc, dest;  /* dummies                                        */
  s64Int            tablesize;   /* aggregate table size (all ranks)               */
  s64Int            loctablesize;/* local table size (each rank)                   */
  s64Int            nupdate;     /* number of updates per rank                     */
  s64Int            tablespace;  /* bytes per rank required for table              */
  u64Int            *ran;        /* vector of random numbers                       */
  u64Int            **ranSendBucket; /* send list of buckets of random numbers     */
  u64Int            **ranRecvBucket; /* receive list of buckets of random numbers  */
  int               *sizeSendBucket; /* list of send bucket sizes                  */
  int               *sizeRecvBucket; /* list of receive buffer sizes               */
  int               sizeRecvTotal;/* total number of elements in receive buffer    */
  int               *senddispls; /* successive displacements in send buffer        */
  int               *recvdispls; /* successive dispalcemetns in receive buffer     */
  u64Int * RESTRICT Table;       /* (pseudo-)randomly accessed array               */
  double            random_time, /* timing parameters                              */
                    avgtime = 0.0;
  int               Num_procs,   /* rank parameters                                */
                    my_ID,       /* rank of calling rank                           */
                    root=0;      /* ID of master rank                              */
  s64Int            error=0;     /* error flag for individual rank                 */
  s64Int            tot_error;   /* error flag for all ranks combined              */
  int               log2nproc;   /* log2 of # ranks                                */
  int               log2nstarts; /* log2 of vector length                          */
  int               log2tablesize; /* log2 of aggregate table size                 */
  int               log2update_ratio; /* log2 of update ratio                      */
  int               procsize;    /* number of ranks per OS rank                    */

#ifdef LONG_IS_64BITS
  if (sizeof(long) != 8) {
    printf("ERROR: Makefile says \"long\" is 8 bytes, but it is %d bytes\n",
           sizeof(long)); 
    exit(EXIT_FAILURE);
  }
#endif

/***********************************************************************************
** process and test input parameters    
************************************************************************************/

  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD,&Num_procs);
  MPI_Comm_rank(MPI_COMM_WORLD,&my_ID);

  if (my_ID == root) {
    if (argc != 3){
      printf("Usage: %s <log2 tablesize> <#update ratio>\n", *argv);
      error = 1;
      goto ENDOFTESTS;
    }

    /* test whether number of ranks is a power of two                             */
    log2nproc = poweroftwo(Num_procs);
    if (log2nproc <0) {
      printf("ERROR: Invalid number of ranks: %d, must be a power of 2\n",
             Num_procs);
      error = 1;
      goto ENDOFTESTS;
    }

    log2tablesize  = atoi(*++argv);
    if (log2tablesize < 1){
      printf("ERROR: Log2 tablesize is %d; must be >= 1\n",log2tablesize);
      error = 1;
      goto ENDOFTESTS;      
    }

    update_ratio  = atoi(*++argv);
    /* test whether update ratio is a power of two                                 */
    log2update_ratio = poweroftwo(update_ratio);
    if (log2update_ratio <0) {
      printf("ERROR: Invalid update ratio: %d, must be a power of 2\n",
             update_ratio);
      error = 1;
      goto ENDOFTESTS;
    }

    /* for simplicity we set the vector length equal to the LOOKAHEAD size         */
    nstarts = LOOKAHEAD;

    /* test whether vector length is a power of two                                */
    log2nstarts = poweroftwo(nstarts);
    if (log2nstarts <0) {
      printf("ERROR: Invalid vector length: %d, must be a power of 2\n",
             nstarts);
      error = 1;
      goto ENDOFTESTS;
    }

    /* compute (local) table size carefully to make sure it can be represented     */
    loctablesize = 1;
    for (i=0; i<log2tablesize-log2nproc; i++) {
      oldsize =  loctablesize;
      loctablesize <<=1;
      if (loctablesize/2 != oldsize) {
        printf("ERROR: Requested table size too large; reduce log2 tablesize = %d\n",
                log2tablesize);
        error = 1;
        goto ENDOFTESTS;
      }
    }
    tablesize = loctablesize * Num_procs;
    if (tablesize/Num_procs != loctablesize) {
      printf("ERROR: Requested table size too large; reduce log2 tablesize = %d\n",
              log2tablesize);
      error = 1;
      goto ENDOFTESTS;
    }

    if ((log2tablesize + log2update_ratio) < (log2nproc+log2nstarts)) {
      printf("ERROR: Table size ("FSTR64U") times update ratio (%d) must be at ",
             ((s64Int)1<<log2tablesize), update_ratio);
      printf("least equal to number of ranks (%d) times vector length (%d)\n", 
             Num_procs, nstarts);
      error = 1;
      goto ENDOFTESTS;
    }
    
    /* even though the table size can be represented, computing the space 
       required for the table may lead to overflow                                 */
    tablespace = (size_t) loctablesize*sizeof(u64Int);
    if ((tablespace/sizeof(u64Int)) != loctablesize || tablespace <=0) {
      printf("ERROR: Cannot represent space for table on this system; ");
      printf("reduce log2 tablesize\n");
      error = 1;
      goto ENDOFTESTS;
    }

    /* compute number of updates carefully to make sure it can be represented      */
    nupdate = update_ratio * loctablesize;
    if (nupdate/loctablesize != update_ratio) {
      printf("Requested number of updates too large; ");
      printf("reduce log2 tablesize or update ratio\n");
      error = 1;
      goto ENDOFTESTS;
    }

    MPIX_Get_collocated_size(&procsize);
    printf("FG_MI Random access\n");
    printf("Number of ranks               = "FSTR64U"\n", Num_procs);
    printf("Number of ranks/process       = "FSTR64U"\n", procsize);
    printf("Table size (aggregate)        = "FSTR64U"\n", tablesize);
    printf("Update ratio                  = "FSTR64U"\n", (u64Int) update_ratio);
    printf("Number of updates (aggregate) = "FSTR64U"\n", nupdate*Num_procs);
    printf("Vector (LOOKAHEAD) length     = "FSTR64U"\n", (u64Int) nstarts);

    ENDOFTESTS:;
  }
  bail_out(error);

  /* broadcast initialization data                                                 */
  MPI_Bcast(&log2nproc,        1, MPI_INT,           root, MPI_COMM_WORLD);
  MPI_Bcast(&log2tablesize,    1, MPI_INT,           root, MPI_COMM_WORLD);
  MPI_Bcast(&update_ratio,     1, MPI_INT,           root, MPI_COMM_WORLD);
  MPI_Bcast(&log2update_ratio, 1, MPI_INT,           root, MPI_COMM_WORLD);
  MPI_Bcast(&nstarts,          1, MPI_INT,           root, MPI_COMM_WORLD);
  MPI_Bcast(&log2nstarts,      1, MPI_INT,           root, MPI_COMM_WORLD);
  MPI_Bcast(&tablesize,        1, MPI_LONG_LONG_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&loctablesize,     1, MPI_LONG_LONG_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&tablespace,       1, MPI_LONG_LONG_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&nupdate,          1, MPI_LONG_LONG_INT, root, MPI_COMM_WORLD);

  ran = (u64Int *) malloc(nstarts*sizeof(u64Int));
  if (!ran) {
    printf("ERROR: rank %d could not allocate %d bytes for random numbers\n",
           my_ID, nstarts*sizeof(u64Int));
    error = 1;
  }
  bail_out(error);
  
  Table = (u64Int *) malloc(tablespace);
  if (!Table) {
    printf("ERROR: rank %d could not allocate space of "FSTR64U"  bytes for table\n",
           my_ID, (u64Int) tablespace);
    error = 1;
  }
  bail_out(error);

  /* allocate send and receive buckets                                             */
  ranSendBucket = (u64Int **) malloc((Num_procs+1)*sizeof(u64Int *));
  if (!ranSendBucket) {
    printf("ERROR: rank %d Could not allocate bucket pointers\n", my_ID);
    error = 1;
  }
  bail_out(error);
  ranRecvBucket = ranSendBucket + Num_procs;

  ranSendBucket[0] = (u64Int *) malloc(2*Num_procs*nstarts*sizeof(u64Int));
  if (!ranSendBucket[0]) {
    printf("ERROR: rank %d Could not allocate bucket space\n", my_ID);
    error = 1;
  }
  bail_out(error);

  for (proc=1; proc<Num_procs; proc++) 
    ranSendBucket[proc] = ranSendBucket[0] + proc*nstarts;
  /* we only need one (large) receive bucket                                       */
  ranRecvBucket[0] = ranSendBucket[0] + Num_procs*nstarts;

  /* allocate send and receive bucket sizes plus buffer offsets                    */
  sizeSendBucket = (int *) malloc(4*Num_procs*sizeof(int));
  if (!sizeSendBucket) {
    printf("ERROR: rank %d Could not allocate bucket sizes\n", my_ID);
    error = 1;
  }
  bail_out(error);

  sizeRecvBucket = sizeSendBucket + Num_procs;
  senddispls     = sizeRecvBucket + Num_procs;
  recvdispls     = senddispls     + Num_procs;

  /* send displacements are regular, they can be calculated in advance             */
  for (proc=0; proc<Num_procs; proc++) senddispls[proc] = nstarts*proc;
  /* only the first receive displacement is always the same                        */
  recvdispls[0] = 0;

  /* initialize the table */
  for(i=0;i<loctablesize;i++) Table[i] = (u64Int) (i+ loctablesize*my_ID);

  MPI_Barrier(MPI_COMM_WORLD);
  if (my_ID == root) {
    random_time = wtime();
  }

  /* do two identical rounds of Random Access to ensure we recover initial table   */
  for (round=0; round <2; round++) {
    /* compute seeds for independent streams, using jump-ahead feature             */
    for (j=0; j<nstarts; j++) {
      ran[j] = PRK_starts(SEQSEED+(nupdate/nstarts)*j+loctablesize*my_ID);
    }

    /* because we do two rounds, we divide nupdate in two                          */
    for (i=0; i<nupdate/(nstarts*2); i++) {

      /* reset actual send bucket sizes                                            */
      for (proc=0; proc<Num_procs; proc++) sizeSendBucket[proc] = 0;

      for (j=0; j<nstarts; j++) {
        /* compute new random number                                               */
        ran[j] = (ran[j] << 1) ^ ((s64Int)ran[j] < 0? POLY: 0);
        global_index = (ran[j] & (tablesize-1));
        /* determine destination rank (high order bits of global table index)      */
        dest = global_index>>(log2tablesize-log2nproc);
        /* place new random number in first available element of the appropriate 
           send bucket and increment that bucket size                              */
        ranSendBucket[dest][sizeSendBucket[dest]++] = ran[j];
      }

      /* let all other rankes know how many indices to expect                      */
      MPI_Alltoall(sizeSendBucket, 1, MPI_INTEGER,
                   sizeRecvBucket, 1, MPI_INTEGER, MPI_COMM_WORLD);

      /* compute receive buffer offsets so that received data is contiguous        */
      for (proc=1; proc<Num_procs; proc++) 
        recvdispls[proc] = recvdispls[proc-1]+sizeRecvBucket[proc-1];

      /* scatter the send bucket contents                                          */
      MPI_Alltoallv(ranSendBucket[0], sizeSendBucket, senddispls, MPI_LONG_LONG_INT,
                    ranRecvBucket[0], sizeRecvBucket, recvdispls, MPI_LONG_LONG_INT,
                    MPI_COMM_WORLD);

      /* do the actual table updates. Because the receive buckets are contiguous, 
         we can view them as a single large bucket.                                */
      sizeRecvTotal = recvdispls[Num_procs-1]+sizeRecvBucket[Num_procs-1];
      /* uncomment the following two pragmas if your compiler allows you to produce 
         incorrect vector code, and you do not care if the results are incorrect   */
      /*  #pragma ivdep */
      /*  #pragma vector always */
      for (j=0; j<sizeRecvTotal; j++) {
        index = ranRecvBucket[0][j] & (loctablesize-1);
        Table[index] ^= ranRecvBucket[0][j];
      }
    }
  }

  if (my_ID == root) random_time = wtime() - random_time;

  /* verification test */
  for(i=0;i<loctablesize;i++) {
    if(Table[i] != (u64Int) (i + loctablesize*my_ID)) {
#ifdef VERBOSE
      printf("ERROR: Table["FSTR64U"]="FSTR64U" on rank %d\n",i,Table[i],my_ID);
#endif
      error++;
    }
  }

  if (error != 0) {
    printf("ERROR: number of incorrect table elements on rank %d = "FSTR64U"\n", 
           my_ID, error);
  }

  MPI_Reduce(&error, &tot_error, 1, MPI_LONG_LONG_INT, MPI_SUM, root, MPI_COMM_WORLD);
  if (my_ID==root) {
    if (!tot_error) {
      printf("Solution validates\n");
      printf("Rate (GUPS/s): %lf, Time (s): %lf\n", 
             1.e-9*(nupdate*Num_procs)/random_time, random_time);
    }
    else {
      printf("Total number of incorrect table elements: "FSTR64U"\n", tot_error);
    }
  }

  MPI_Finalize();
}

/* Utility routine to start random number generator at nth step                    */
u64Int PRK_starts(s64Int n)
{ 
  int i, j; 
  u64Int m2[64];
  u64Int temp, ran; 
 
  while (n < 0) n += PERIOD;
  while (n > PERIOD) n -= PERIOD;
  if (n == 0) return 0x1;
 
  temp = 0x1;
  for (i=0; i<64; i++) {
    m2[i] = temp;
    temp = (temp << 1) ^ ((s64Int) temp < 0 ? POLY : 0); 
    temp = (temp << 1) ^ ((s64Int) temp < 0 ? POLY : 0); 
  } 
 
  for (i=62; i>=0; i--) 
    if ((n >> i) & 1) 
      break; 

  ran = 0x2;    
  while (i > 0) { 
    temp = 0; 
    for (j=0; j<64; j++)
      if ((unsigned int)((ran >> j) & 1)) 
        temp ^= m2[j]; 
    ran = temp; 
    i -= 1; 
    if ((n >> i) & 1)
      ran = (ran << 1) ^ ((s64Int) ran < 0 ? POLY : 0); 
  } 
 
  return ran; 
} 

/* utility routine that tests whether an integer is a power of two                 */
int poweroftwo(int n) {
  int log2n = 0;

  while ((1<<log2n)<n) log2n++;
  if (1<<log2n != n) return (-1);
  else               return (log2n);
}
