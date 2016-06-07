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
         update elements of an array with irregular stride.

USAGE:   The program takes as input the 2log 
         of the size of the table that gets updated, the ratio of table size 
         over number of updates, and the vector length of simultaneously
         updatable table elements. 

         <progname> <log2 tablesize> <#update ratio> <vector length>

FUNCTIONS CALLED:

         Other than standard C functions, the following 
         functions are used in this program:

         wtime()
         PRK_starts()
         poweroftwo()

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
         - the number of independent streams (vector length) can be changed by the 
           user.

         We note that the vectorized version of this code (i.e. nstarts unequal 
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

HISTORY: Written by Rob Van der Wijngaart, February 2009.
         Histogram code (verbose mode) courtesy Roger Golliver
  
************************************************************************************/

#include <par-res-kern_general.h>

/* Define constants                                                                */
/* PERIOD = (2^63-1)/7 = 7*73*127*337*92737*649657                                 */
#ifdef LONG_IS_64BITS 
  #define POLY               0x0000000000000007UL
  #define PERIOD             1317624576693539401L
  /* sequence number in stream of random numbers to be used as initial value       */
  #define SEQSEED            834568137686317453L
#else 
  #define POLY               0x0000000000000007ULL
  #define PERIOD             1317624576693539401LL
  /* sequence number in stream of random numbers to be used as initial value       */
  #define SEQSEED            834568137686317453LL
#endif 

#ifdef HPCC
  #undef  ERRORPERCENT
  #define ERRORPERCENT 1
#else
  #ifndef ERRORPERCENT
    #define ERRORPERCENT 0
  #endif
#endif

static u64Int PRK_starts(s64Int);
static int    poweroftwo(int);

int main(int argc, char **argv) {

  int               update_ratio;  /* multiplier of tablesize for # updates        */
  int               nstarts;     /* vector length                                  */
  s64Int            i, j, round, oldsize; /* dummies                               */
  s64Int            error;       /* number of incorrect table elements             */
  s64Int            tablesize;   /* aggregate table size (all threads              */
  s64Int            nupdate;     /* number of updates per thread                   */
  size_t            tablespace;  /* bytes per thread required for table            */
  u64Int            *ran;        /* vector of random numbers                       */
  s64Int            index;       /* index into Table                               */
#ifdef VERBOSE
  u64Int * RESTRICT Hist;        /* histogram of # updates to table elements       */
  unsigned int      *HistHist;   /* histogram of update frequencies                */
#endif
  u64Int * RESTRICT Table;       /* (pseudo-)randomly accessed array               */
  double            random_time;
  int               log2nstarts; /* log2 of vector length                          */
  int               log2tablesize; /* log2 of aggregate table size                 */
  int               log2update_ratio; /* log2 of update ratio                      */

#ifdef LONG_IS_64BITS
  if (sizeof(long) != 8) {
    printf("ERROR: Makefile says \"long\" is 8 bytes, but it is %d bytes\n",
           sizeof(long)); 
    exit(EXIT_FAILURE);
  }
#endif

/*********************************************************************
** process and test input parameters    
*********************************************************************/

  if (argc != 4){
    printf("Usage: %s <log2 tablesize> <#update ratio> ", *argv);
    printf("<vector length>\n");
    exit(EXIT_FAILURE);
  }

  log2tablesize  = atoi(*++argv);
  if (log2tablesize < 1){
    printf("ERROR: Log2 tablesize is %d; must be >= 1\n",log2tablesize);
    exit(EXIT_FAILURE);
  }

  update_ratio  = atoi(*++argv);
  /* test whether update ratio is a power of two                           */
  log2update_ratio = poweroftwo(update_ratio);
  if (log2update_ratio <0) {
    printf("ERROR: Invalid update ratio: %d, must be a power of 2\n",
           update_ratio);
    exit(EXIT_FAILURE);
  }

  nstarts = atoi(*++argv);
  /* test whether vector length is a power of two                          */
  log2nstarts = poweroftwo(nstarts);
  if (log2nstarts <0) {
    printf("ERROR: Invalid vector length: %d, must be a power of 2\n",
           nstarts);
    exit(EXIT_FAILURE);
  } 

  /* compute table size carefully to make sure it can be represented        */
  tablesize = 1;
  for (i=0; i<log2tablesize; i++) {
    oldsize =  tablesize;
    tablesize <<=1;
    if (tablesize/2 != oldsize) {
      printf("Requested table size too large; reduce log2 tablesize = %d\n",
              log2tablesize);
      exit(EXIT_FAILURE);
    }
  }

  if ((log2tablesize + log2update_ratio) < log2nstarts) {
    printf("ERROR: Table size ("FSTR64U") times update ratio (%d) must be at ",
           ((s64Int)1<<log2tablesize), update_ratio);
    printf("least equal to vector length (%d)\n", nstarts);
    exit(EXIT_FAILURE);
  }

  /* even though the table size can be represented, computing the space 
     required for the table may lead to overflow                            */
  tablespace = (size_t) tablesize*sizeof(u64Int);
  if ((tablespace/sizeof(u64Int)) != tablesize || tablespace <=0) {
    printf("Cannot represent space for table on this system; ");
    printf("reduce log2 tablesize\n");
    exit(EXIT_FAILURE);
  }

#ifdef VERBOSE
  Hist = (u64Int *) malloc(tablespace);
  HistHist = (unsigned int *) malloc(tablespace);
  if (!Hist || ! HistHist) {
    printf("ERROR: Could not allocate space for histograms\n");
    exit(EXIT_FAILURE);
  }
#endif

  /* compute number of updates carefully to make sure it can be represented */
  nupdate = update_ratio * tablesize;
  if (nupdate/tablesize != update_ratio) {
    printf("Requested number of updates too large; ");
    printf("reduce log2 tablesize or update ratio\n");
    exit(EXIT_FAILURE);
  }

  Table = (u64Int *) malloc(tablespace);
  if (!Table) {
    printf("ERROR: Could not allocate space of "FSTR64U"  bytes for table\n",
           (u64Int) tablespace);
    exit(EXIT_FAILURE);
  }

  error = 0;

  printf("Serial Random Access\n");
  printf("Table size (shared)    = "FSTR64U"\n", tablesize);
  printf("Update ratio           = "FSTR64U"\n", (u64Int) update_ratio);
  printf("Number of updates      = "FSTR64U"\n", nupdate);
  printf("Vector length          = "FSTR64U"\n", (u64Int) nstarts);
  printf("Percent errors allowed = "FSTR64U"\n", (u64Int) ERRORPERCENT);

  ran = (u64Int *) malloc(nstarts*sizeof(u64Int));
  if (!ran) {
    printf("ERROR: Could not allocate %d bytes for random numbers\n",
           nstarts*(int)sizeof(u64Int));
    exit(EXIT_FAILURE);
  }

  /* initialize the table */
  for(i=0;i<tablesize;i++) Table[i] = (u64Int) i;

  random_time = wtime();

  /* do two identical rounds of Random Access to make sure we recover 
     the initial condition                                                 */
  for (round=0; round <2; round++) {

    for (j=0; j<nstarts; j++) {
      ran[j] = PRK_starts(SEQSEED+(nupdate/nstarts)*j);
    }
    for (j=0; j<nstarts; j++) {
      /* because we do two rounds, we divide nupdates in two               */
      for (i=0; i<nupdate/(nstarts*2); i++) {
        ran[j] = (ran[j] << 1) ^ ((s64Int)ran[j] < 0? POLY: 0);
        index = ran[j] & (tablesize-1);
        Table[index] ^= ran[j];
#ifdef VERBOSE
        Hist[index] += 1;
#endif
      }
    }
  }

  random_time = wtime() - random_time;

  /* verification test */
  for(i=0;i<tablesize;i++) {
    if(Table[i] != (u64Int) i) {
#ifdef VERBOSE
      printf("Error Table["FSTR64U"]="FSTR64U"\n",i,Table[i]);
#endif
      error++;
    }
  }

  if ((error && (ERRORPERCENT==0)) ||
      ((double)error/(double)tablesize > ((double) ERRORPERCENT)*0.01)) {
    printf("ERROR: number of incorrect table elements = "FSTR64U"\n", error);
    exit(EXIT_FAILURE);
  }
  else {
    printf("Solution validates, number of errors: %ld\n",(long) error);
    printf("Rate (GUPs/s): %lf time (s) = %lf\n", 
           1.e-9*nupdate/random_time,random_time);
  }

#ifdef VERBOSE
  for(i=0;i<tablesize;i++) HistHist[Hist[i]]+=1;
  for(i=0;i<=tablesize;i++) if (HistHist[i] != 0)
	printf("HistHist[%4.1d]=%9.1d\n",(int)i,HistHist[i]);
#endif

  exit(EXIT_SUCCESS);
}

/* Utility routine to start random number generator at nth step            */
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

/* utility routine that tests whether an integer is a power of two         */
int poweroftwo(int n) {
  int log2n = 0;

  while ((1<<log2n)<n) log2n++;
  if (1<<log2n != n) return (-1);
  else               return (log2n);
}
