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

NAME:      bail_out

PURPOSE:   Exit gracefully when an SHMEM process has encountered an error
  
Arguments: error code, work space  

Returns:   nothing, but the program terminates with a nonzero exit status

Notes:     This function must be called by all participating processes

HISTORY: - Written by Gabriele Jost, March 2015.
  
**********************************************************************************/

#include <par-res-kern_general.h>
#include <shmem.h>

#define shmem_finalize()

void bail_out (int error, long *pSync) {
   long *global_error;
   long *local_error;
   long pWrk [_SHMEM_BCAST_SYNC_SIZE];

   int i;
   global_error = shmalloc(sizeof(long));
   local_error = shmalloc(sizeof(long));
   if (!global_error || !local_error) {
     printf("SHMEM_bail_out could not allocate error space on symmetric heap\n");
     exit(2);
   }
   local_error [0] = error;
   shmem_long_max_to_all (global_error, local_error, 1, 0, 0, _num_pes (), pWrk, pSync);
   if (global_error[0] > 0) {
     shmem_finalize ();
     exit (1);
  }
  return;
}

