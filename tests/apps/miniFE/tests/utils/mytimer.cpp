
//@HEADER
// ************************************************************************
//
// MiniFE: Simple Finite Element Assembly and Solve
// Copyright (2006-2013) Sandia Corporation
//
// Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
// license for use of this work by or on behalf of the U.S. Government.
//
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
//
// ************************************************************************
//@HEADER

#include <cstddef>
#include <cstdlib>
#include <mytimer.hpp>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

namespace miniFE {

/////////////////////////////////////////////////////////////////////////

// Function to return time in seconds.
// If compiled with no flags, return CPU time (user and system).
// If compiled with -DWALL, returns elapsed time.

/////////////////////////////////////////////////////////////////////////

#if defined(HAVE_MPI) && defined(USE_MPI_WTIME)

timer_type mytimer()
{
  return((timer_type) MPI_Wtime());
}


#elif defined(UseClock)

#include <time.hpp>
timer_type mytimer(void)
{
   clock_t t1;
   static clock_t t0=0;
   static timer_type CPS = CLOCKS_PER_SEC;
   timer_type d;

   if (t0 == 0) t0 = clock();
   t1 = clock() - t0;
   d = t1 / CPS;
   return(d);
}

#elif defined(WALL)

#include <cstdlib>
#include <sys/time.h>
#include <sys/resource.h>
timer_type mytimer(void)
{
   struct timeval tp;
   static long start=0, startu;
   if (!start)
   {
      gettimeofday(&tp, NULL);
      start = tp.tv_sec;
      startu = tp.tv_usec;
      return(0.0);
   }
   gettimeofday(&tp, NULL);
   return( ((timer_type) (tp.tv_sec - start)) + (tp.tv_usec-startu)/1000000.0 );
}

#elif defined(UseTimes)

#include <cstdlib>
#include <sys/times.h>
#include <unistd.h>
timer_type mytimer(void)
{
   struct tms ts;
   static timer_type ClockTick=0.0;

   if (ClockTick == 0.0) ClockTick = (timer_type) sysconf(_SC_CLK_TCK);
   times(&ts);
   return( (timer_type) ts.tms_utime / ClockTick );
}

#else

#include <cstdlib>
#include <sys/time.h>
#include <sys/resource.h>
timer_type mytimer(void)
{
//This function now uses gettimeofday instead of getrusage. See note below.
//
  struct timeval tv;
  struct timezone tz;
  gettimeofday(&tv, &tz);
  return ( (timer_type)tv.tv_sec + tv.tv_usec/1000000.0 );

//The below use of 'getrusage' is not used because it doesn't do the right thing
//for the case of using threads. It adds up the time spent in multiple threads,
//rather than giving elapsed time.
//
//   struct rusage ruse;
//   getrusage(RUSAGE_SELF, &ruse);
//   return( (timer_type)(ruse.ru_utime.tv_sec+ruse.ru_utime.tv_usec / 1000000.0) );
}

#endif

}//namespace miniFE

