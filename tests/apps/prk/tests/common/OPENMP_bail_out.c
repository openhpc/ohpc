/**********************************************************************

Name:      bail_out

Purpose:   Exit gracefully when an OpenMP thread has encountered an error
           inside a parallel region

Arguments: error code (zero for no error).

Returns:   nothing, but the program terminates with a nonzero exit status

Notes:     This function must be called by all threads in the team. Multiple
           threads may have tried to update the shared error variable at the
           same time, so this needs to be done atomically if we want to
           guarantee that the value of 1 is put into error. In our case,
           however, we merely want to know if the value is different from
           zero, so we do not need atomicity.
 
History:   Written by Rob Van der Wijngaart, July 2006

**********************************************************************/

#include <par-res-kern_general.h>

void bail_out(int error) {

  #pragma omp barrier
  if (error != 0) exit(EXIT_FAILURE);
}
