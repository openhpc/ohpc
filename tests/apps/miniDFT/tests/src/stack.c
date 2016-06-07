/*
  Copyright (C) 2007-2008 Quantum ESPRESSO group
  This file is distributed under the terms of the
  GNU General Public License. See the file `License'
  in the root directory of the present distribution,
  or http://www.gnu.org/copyleft/gpl.txt .
*/

#include "c_defs.h"
#include <stdio.h>
#include <stdlib.h>
#ifdef __INTEL

#include <sys/resource.h>

void F77_FUNC_(remove_stack_limit,REMOVE_STACK_LIMIT) (void) {
 
  struct rlimit rlim = { RLIM_INFINITY, RLIM_INFINITY };

  /* Modified according to Cesar Da Silva suggestions */
  if ( setrlimit(RLIMIT_STACK, &rlim) == -1 ) {
    if ( getrlimit(RLIMIT_STACK, &rlim) == 0 ) {
      rlim.rlim_cur = rlim.rlim_max;
      if ( setrlimit(RLIMIT_STACK, &rlim) == 0 ) {
        getrlimit(RLIMIT_STACK, &rlim);
      } else {
        perror("  Cannot set stack size to new value");
      }
    }
  }
}

#else
void F77_FUNC_(remove_stack_limit,REMOVE_STACK_LIMIT) (void) {
}
#endif
