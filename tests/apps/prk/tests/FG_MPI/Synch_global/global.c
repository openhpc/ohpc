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

NAME:    StopNGo

PURPOSE: This program tests the efficiency of a global synchronization
         on the target system. 
  
USAGE:   The program takes as input the number of times the test of
         string manipulation involving the global synchronization is
         carried out, as well as the length of the string.

         <progname>  <# iterations> <length of numerical string>
  
         The output consists of diagnostics to make sure the 
         algorithm worked, and of timing statistics.

         Compile with VERBOSE defined if you want lots of output.

FUNCTIONS CALLED:

         Other than MPI or standard C functions, the following 
         functions are used in this program:

         wtime()
         bail_out()
         chartoi()

HISTORY: Written by Rob Van der Wijngaart, December 2005.
  
*******************************************************************/

#include <par-res-kern_general.h>
#include <par-res-kern_fg-mpi.h>

#define EOS '\0'

int chartoi(char c) {
  /* define short string; need two characters, second contains string terminator */
  char letter[2]="0";
  letter[0]=c;
  return (atoi(letter));
}


int main(int argc, char ** argv)
{
  int    my_ID;       /* rank                                                    */
  int    root=0;
  int    iterations;  /* number of times to rehash strings                       */
  int    i, iter;     /* dummies                                                 */
  int    checksum;    /* computed checksum of final aggregate string             */
  char   *scramble = "27638472638746283742712311207892";
  char   *basestring; /* initial string to be copied to private strings          */
  char   *iterstring; /* private copy of string                                  */
  char   *catstring;  /* concatenated, scrambled string                          */
  long   length,      /* total length of scramble string                         */
         proc_length; /* length of string per rank                               */
  int    basesum;     /* checksum of base string                                 */
  MPI_Datatype mpi_word; /* chunk of scramble string to be communicated          */
  double stopngo_time;/* timing parameter                                        */
  int    Num_procs;   /* Number of ranks                                         */
  int    error = 0;   /* error flag                                              */
  int    procsize;    /* number of ranks per OS process                          */

/*********************************************************************************
** Initialize the MPI environment
**********************************************************************************/
  MPI_Init(&argc,&argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_ID);
  MPI_Comm_size(MPI_COMM_WORLD, &Num_procs);

/*********************************************************************************
** process, test and broadcast input parameter
**********************************************************************************/

  if (my_ID == root){
    if (argc != 3){
      printf("Usage: %s <# iterations> <scramble string length>\n", *argv);
      error = 1;
      goto ENDOFTESTS;
    }

    iterations  = atoi(*++argv);
    if (iterations < 1){
      printf("ERROR: iterations must be positive: %d \n",iterations);
      error = 1;
      goto ENDOFTESTS;
    }

    length      = atol(*++argv);
    if (length <Num_procs || length%Num_procs !=0) {
      printf("ERROR: length of string %d must be multiple of # ranks: %ld\n", 
             length, Num_procs);
      error = 1;
      goto ENDOFTESTS;
     }

     ENDOFTESTS:;
  }
  bail_out(error);

  if (my_ID == root) {
    MPIX_Get_collocated_size(&procsize);
    printf("FG_MPI global synchronization\n");
    printf("Number of ranks          = %d\n", Num_procs);
    printf("Number of ranks/process  = %d\n", procsize);
    printf("Scramble string length   = %ld\n", length);
    printf("Number of iterations     = %d\n", iterations);
  }

  /* Broadcast benchmark data to all ranks */
  MPI_Bcast(&iterations, 1, MPI_INT, root, MPI_COMM_WORLD);
  MPI_Bcast(&length,     1, MPI_LONG, root, MPI_COMM_WORLD);
  proc_length = length/Num_procs;

  basestring = malloc((proc_length+1)*sizeof(char));
  if (basestring==NULL) {
    printf("ERROR: Could not allocate space for scramble string\n");
    error = 1;
  }
  bail_out(error);

  /* fill the base string with copies (truncated) of scrable string         */
  for (i=0; i<proc_length; i++) basestring[i]=scramble[i%32];
  basestring[proc_length] = EOS;

  catstring=(char *) malloc((length+1)*sizeof(char));
  if (catstring==NULL) {
    printf("ERROR: Could not allocate space for concatenation string: %d\n",
           length+1);
    error = 1;
  }
  bail_out(error);

  /* initialize concatenation string with nonsense                          */
  for (i=0; i<length; i++) catstring[i]='9';
  catstring[length]=EOS;

  iterstring= (char *) malloc((proc_length+1)*sizeof(char)); 
  if (iterstring==NULL) {
    printf("ERROR: Could not allocate space for strings in rank %d\n", my_ID);
    error = 1;
  }
  bail_out(error);

  strcpy(iterstring, basestring);

  catstring=(char *) malloc((length+1)*sizeof(char));
  if (catstring==NULL) {
    printf("ERROR: Could not allocate space for strings in rank %d\n", my_ID);
    error = 1;
  }

  bail_out(error);

  for (i=0; i<length; i++) catstring[i]='9';
  catstring[length]=EOS;

  /* define a user type consisting of a chunk of chars to be communicated */
  MPI_Type_contiguous(proc_length,MPI_CHAR, &mpi_word);
  MPI_Type_commit(&mpi_word);

  MPI_Barrier(MPI_COMM_WORLD);
  stopngo_time = wtime();

  for (iter=0; iter<iterations; iter++) { 

    /* Everybody sends own string to everybody else and concatenates */
    MPI_Allgather(iterstring,1,mpi_word, catstring,1,mpi_word, MPI_COMM_WORLD);

    /* now everybody selects a different substring */
    for (i=0; i<proc_length; i++) iterstring[i]=catstring[my_ID+i*Num_procs];

#ifdef VERBOSE
    if (my_ID==0) {
      checksum=0;
      for (i=0; i<length+1;i++) checksum+= chartoi(catstring[i]);
      printf("Iteration %d, cat string %s, checksum equals: %d\n", 
             iter, catstring, checksum);
    }
#endif
  }

  stopngo_time = wtime() - stopngo_time;

  /* compute checksum on obtained result, adding all digits in the string */
  if (my_ID==0) {
    basesum=0;
    for (i=0; i<proc_length; i++) basesum += chartoi(basestring[i]);
    checksum=0;
    for (i=0; i<length+1;i++) checksum += chartoi(catstring[i]);
    if (checksum != basesum*Num_procs) {
      printf("Incorrect checksum: %d instead of %d\n", checksum, basesum*Num_procs);
    }
    else {
      printf("Solution validates\n");
    }
    printf("Rate (synch/s): %lf, time (s): %lf\n", 
           (iterations/stopngo_time), stopngo_time);
  }

  MPI_Finalize();


}  /* end of main */

