#include "fpapi_test.h"

      program highlevel
      implicit integer (p)

      integer*8 values(10)
      integer events(2)
      integer eventnum
      integer availcounters
      integer retval
      integer tests_quiet, get_quiet
      external get_quiet
      character*PAPI_MAX_STR_LEN name
      integer last_char, n
      external last_char

      tests_quiet = get_quiet()

      eventnum = 2
      call PAPIf_num_counters(availcounters)
      if (eventnum .GT. availcounters) then
         print *, "Not enough hardware counters!"
         stop
      end if

      retval = PAPI_VER_CURRENT
      call PAPIf_library_init(retval)
      if ( retval.NE.PAPI_VER_CURRENT) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPI_library_init', retval)
      end if

      call PAPIf_query_event(PAPI_FP_INS, retval)
      if (retval .NE. PAPI_OK) then
        events(1)=PAPI_TOT_INS
      else
        events(1)=PAPI_FP_INS
      end if
      events(2)=PAPI_TOT_CYC

      call PAPIf_start_counters(events, eventnum, retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_start_counters', 
     *retval)
      end if

      call fdo_flops(NUM_FLOPS)

      call PAPIf_read_counters(values(1), eventnum, retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_read_counters', 
     *retval)
      end if

      call fdo_flops(NUM_FLOPS)

      call PAPIf_stop_counters(values(3), eventnum, retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_stop_counters', 
     *retval)
      end if
      
      if (tests_quiet .EQ. 0) then
        call PAPIf_event_code_to_name (events(1), name, retval)
        if ( retval.NE.PAPI_OK) then
          call ftest_fail(__FILE__, __LINE__, 
     * 'PAPIf_event_code_to_name', retval)
        end if
        n=last_char(name)
        print *, "Test case highlevel: Test of high-level APIs."
        print *, "---------------------------------------------",
     *    "---------------------"
        write (*,100) "Test type", 1, 2
        write (*,100) name(1:n), values(1), values(3)
        write (*,100) "PAPI_TOT_CYC", values(2), values(4)
 100    format(a15, ":", i12, i12)
        print *, "---------------------------------------------",
     * "---------------------"
      end if

      call ftests_pass(__FILE__)
      End
