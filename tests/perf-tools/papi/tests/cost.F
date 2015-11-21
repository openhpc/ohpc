#include "fpapi_test.h"

      program cost
      implicit integer (p)
      
      integer es
      integer*8 values(10)
      integer*8 ototcyc, ntotcyc
      integer*4 i
      integer retval

      Integer last_char
      External last_char
      integer tests_quiet, get_quiet
      external get_quiet

      tests_quiet = get_quiet()
      es = PAPI_NULL

      if (tests_quiet .EQ. 0) then
      print *, "Cost of execution for PAPI start/stop",
     *" and PAPI read."
      print *, "This test takes a while. Please be patient..."
      end if

      retval = PAPI_VER_CURRENT
      call PAPIf_library_init(retval)
      if ( retval.NE.PAPI_VER_CURRENT) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPI_library_init', retval)
      end if

      call PAPIf_query_event(PAPI_TOT_CYC, retval)
      if ( retval.NE.PAPI_OK) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_query_event', retval)
      end if

      call PAPIf_query_event(PAPI_TOT_INS, retval)
      if ( retval.NE.PAPI_OK) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_query_event', retval)
      end if

      call PAPIf_create_eventset(es, retval)
      if ( retval.NE.PAPI_OK) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_create_eventset', 
     *retval)
      end if

      call PAPIf_add_event( es, PAPI_TOT_CYC, retval )
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_add_event', retval)
      end if

      call PAPIf_add_event( es, PAPI_TOT_INS, retval )
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_add_event', retval)
      end if

      if (tests_quiet .EQ. 0) then
      print *, "Performing start/stop test..."
      end if

      call PAPIf_start(es, retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_start', retval)
      end if

      call PAPIf_stop(es, values(1), retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_stop', retval)
      end if

      call PAPIf_get_real_cyc(ototcyc)
      
      do i=0, 50000
         call PAPIf_start(es, retval)
         call PAPIf_stop(es, values(1), retval)
      end do
      
      call PAPIf_get_real_cyc(ntotcyc)
      ntotcyc=ntotcyc-ototcyc

      if (tests_quiet .EQ. 0) then
      print *
      print *
      print *, "Total cost for PAPI_start/stop(2 counters) over",
     *" 50000 iterations:"
      write (*, 100) ntotcyc, "total cyc"
      write (*, 200) ntotcyc/50001.0, "cyc/call pair"
      print *
      print *
      
C     Start the read val
      
      print *, "Performing read test..."
      end if

      call PAPIf_start(es, retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_start', retval)
      end if

      call PAPIf_get_real_cyc(ototcyc)

      do i=0, 50000
         call PAPIf_read(es, values(1), retval)
      end do
      
      call PAPIf_stop(es, values(1), retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_stop', retval)
      end if

      call PAPIf_get_real_cyc(ntotcyc)


      ntotcyc=ntotcyc-ototcyc

      if (tests_quiet .EQ. 0) then
      print *
      print *, "User level cost for PAPI_read(2 counters) over",
     *" 50000 iterations:"
      print *
      print *, "Total cost for PAPI_read(2 counters) over ",
     *"50000 iterations:"
      write (*, 100) ntotcyc, "total cyc"
      write (*, 200) ntotcyc/50001.0, "cyc/call"
      end if

 100  format (I15, A15)
 200  format (F15.6, A15)
      call ftests_pass(__FILE__)
      end

