#include "fpapi_test.h"
      
      program zero

      integer*8 values(10)
      integer es, event
      integer*8 uso, usn, cyco, cycn
      integer domain, granularity
      character*(PAPI_MAX_STR_LEN) domainstr, grnstr
      character*(PAPI_MAX_STR_LEN) name
      integer retval

      Integer last_char, n
      External last_char
      integer tests_quiet, get_quiet
      external get_quiet

      tests_quiet = get_quiet()
      es = PAPI_NULL

      retval = PAPI_VER_CURRENT
      call PAPIf_library_init(retval)
      if ( retval.NE.PAPI_VER_CURRENT) then
        call ftest_fail(__FILE__, __LINE__,
     .       'PAPI_library_init', retval)
      end if

      call PAPIf_query_event(PAPI_FP_INS, retval)
      if (retval .NE. PAPI_OK) then
        event = PAPI_TOT_INS
      else
        event = PAPI_FP_INS
      end if

      call PAPIf_create_eventset(es, retval)
      if ( retval.NE.PAPI_OK) then
        call ftest_fail(__FILE__, __LINE__,
     .       'PAPIf_create_eventset', 
     *       retval)
      end if

      call PAPIf_add_event( es, event, retval )
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     .       'PAPIf_add_event', retval)
      end if
      
      call PAPIf_add_event( es, PAPI_TOT_CYC, retval )
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     .       'PAPIf_add_event', retval)
      end if

      call PAPIf_get_real_usec(uso)
      call PAPIf_get_real_cyc(cyco)

      call PAPIf_start(es, retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     .       'PAPIf_start', retval)
      end if

      call fdo_flops(NUM_FLOPS)

      call PAPIf_stop(es, values(1), retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     .       'PAPIf_stop', retval)
      end if

      call PAPIf_get_real_usec(usn)
      call PAPIf_get_real_cyc(cycn)

      if (tests_quiet .EQ. 0) then
        print *, "Test case 0: start, stop."
        print *, "-----------------------------------------------",
     *       "--------------------------"
      end if

      call PAPIf_get_domain(es, domain, PAPI_DEFDOM, retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     .       'PAPIf_get_domain', retval)
      end if

      call stringify_domain(domain, domainstr)
      if (tests_quiet .EQ. 0) then
        write (*,800) "Default domain is     :", domain, domainstr
      end if

      call PAPIf_get_granularity(es, granularity, PAPI_DEFGRN, 
     *     retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     .       'PAPIf_get_granularity',
     *       retval)
      end if

      call stringify_granularity(granularity, grnstr)
      if (tests_quiet .EQ. 0) then
        call PAPIf_event_code_to_name (event, name, retval)
        if ( retval.NE.PAPI_OK) then
          call ftest_fail(__FILE__, __LINE__, 
     *         'PAPIf_event_code_to_name', retval)
        end if
        n=last_char(name)
        write (*,800) "Default granularity is:", granularity, grnstr
 800    format(a25, i3, " ", a70)
        write (*,810) "Using", NUM_FLOPS,
     $       " iterations of c = c + a * b"
 810    format(a7, i9, a)

        print *, "-----------------------------------------------",
     *       "--------------------------"
        
        write (*,100) "Test type", 1
        write (*,100) name(1:n), values(1)
        write (*,100) "PAPI_TOT_CYC", values(2)
        write (*,100) "Real usec", usn-uso
        write (*,100) "Real cycles", cycn-cyco
 100    format(a13, ":", i12)
        print *, "-----------------------------------------------",
     *       "--------------------------"
        
        print *, "Verification: none"
      endif
      
      call ftests_pass(__FILE__)
      end

