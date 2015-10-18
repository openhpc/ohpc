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

/****************************************************************

Name:      wtime

Purpose:   returns wall clock time with a fixed reference point.

Arguments: None

Returns:   The wall clock time in seconds as a double is returned. 

Notes:     This function uses two structures defined in the UNIX
           system call, gettimeofday(2). The structure, "timeval" 
           is defined in the include file <sys/time.h> as:

                 struct timeval {
                     long tv_sec;
                     long tv_usec;
                 }

           where timeval.tv_sec is the seconds and timeval.tv_usec
           is the microseconds.  
 
History:   Written by Tim Mattson, Dec 1, 1988
           Modified by Rob van der Wijngaart, May 2006, to change
           default clock to the Unix system clock.

****************************************************************/

#include <stdlib.h>
#if defined(_OPENMP)
  #include <omp.h>
#elif defined(MPI)
  #include "mpi.h"
#else
  #include <sys/time.h>
  #define  USEC_TO_SEC   1.0e-6    /* to convert microsecs to secs */
#endif


double wtime() {
  double time_seconds;

#if defined(_OPENMP)
  time_seconds = omp_get_wtime();

#elif defined(MPI)
  time_seconds = MPI_Wtime();

#else
  struct timeval  time_data; /* seconds since 0 GMT */
   
  gettimeofday(&time_data,NULL);
   
  time_seconds  = (double) time_data.tv_sec;
  time_seconds += (double) time_data.tv_usec * USEC_TO_SEC;
#endif

  return time_seconds;
}
