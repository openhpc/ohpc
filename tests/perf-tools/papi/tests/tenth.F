#include "fpapi_test.h"
#define ITERS 100

#if defined(sun) && defined(sparc)
#define CACHE_LEVEL "PAPI_L2_TCM"
#define EVT1            PAPI_L2_TCM
#define EVT2            PAPI_L2_TCA
#define EVT3            PAPI_L2_TCH
#define EVT1_STR        "PAPI_L2_TCM"
#define EVT2_STR        "PAPI_L2_TCA"
#define EVT3_STR        "PAPI_L2_TCH"
#else
#if defined(__powerpc__)
#define CACHE_LEVEL "PAPI_L1_DCA"
#define EVT1            PAPI_L1_DCA
#define EVT2            PAPI_L1_DCW
#define EVT3            PAPI_L1_DCR
#define EVT1_STR        "PAPI_L1_DCA"
#define EVT2_STR        "PAPI_L1_DCW"
#define EVT3_STR        "PAPI_L1_DCR"
#else
#define CACHE_LEVEL "PAPI_L1_TCM"
#define EVT1            PAPI_L1_TCM
#define EVT2            PAPI_L1_ICM
#define EVT3            PAPI_L1_DCM
#define EVT1_STR        "PAPI_L1_TCM"
#define EVT2_STR        "PAPI_L1_ICM"
#define EVT3_STR        "PAPI_L1_DCM"
#endif
#endif

      program tenth
      implicit integer (p)

      integer*8 values(10)
      integer es1, es2, es3
      integer*4 mask1, mask2, mask3
      integer domain, granularity
      character*(PAPI_MAX_STR_LEN) domainstr, grnstr
      integer retval

      Integer last_char
      External last_char
      integer tests_quiet, get_quiet
      external get_quiet

      tests_quiet = get_quiet()
      es1 = PAPI_NULL
      es2 = PAPI_NULL
      es3 = PAPI_NULL

      mask1 = EVT1
      mask2 = EVT2
      mask3 = EVT3

      retval = PAPI_VER_CURRENT
      call PAPIf_library_init(retval)
      if ( retval.NE.PAPI_VER_CURRENT) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPI_library_init', retval)
      end if

      call PAPIf_query_event(mask1, retval)
      if ( retval.NE.PAPI_OK) then
        call ftest_skip(__FILE__, __LINE__,
     .'PAPIf_query_event', retval)
      end if
      call PAPIf_query_event(mask2, retval)
      if ( retval.NE.PAPI_OK) then
        call ftest_skip(__FILE__, __LINE__,
     .'PAPIf_query_event', retval)
      end if
      call PAPIf_query_event(mask3, retval)
      if ( retval.NE.PAPI_OK) then
        call ftest_skip(__FILE__, __LINE__,
     .'PAPIf_query_event', retval)
      end if

      call PAPIf_create_eventset(es1, retval)
      if ( retval.NE.PAPI_OK) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_create_eventset',
     *retval)
      end if

      call PAPIf_add_event( es1, mask1, retval )
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_add_event', retval)
      end if

      call PAPIf_create_eventset(es2, retval)
      if ( retval.NE.PAPI_OK) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_create_eventset',
     *retval)
      end if

      call PAPIf_add_event( es2, mask2, retval )
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_add_event', retval)
      end if

      call PAPIf_create_eventset(es3, retval)
      if ( retval.NE.PAPI_OK) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_create_eventset',
     *   retval)
      end if

      call PAPIf_add_event( es3, mask3, retval )
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_add_event', retval)
      end if

      call fdo_l1misses(ITERS)

      call PAPIf_start(es1, retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_start', retval)
      end if

      call fdo_l1misses(ITERS)

      call PAPIf_stop(es1, values(1), retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_stop', retval)
      end if


      call PAPIf_start(es2, retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_start', retval)
      end if

      call fdo_l1misses(ITERS)
      call PAPIf_stop(es2, values(3), retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_stop', retval)
      end if
      
      call PAPIf_start(es3, retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_start', retval)
      end if

      call fdo_l1misses(ITERS)

      call PAPIf_stop(es3, values(5), retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_stop', retval)
      end if

      call PAPIf_remove_event( es1, mask1, retval )
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_remove_event', retval)
      end if

      call PAPIf_remove_event( es2, mask2, retval )
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_remove_event', retval)
      end if

      call PAPIf_remove_event( es3, mask3, retval )
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_remove_event', retval)
      end if

      if (tests_quiet .EQ. 0) then
#if (defined(sun) && defined(sparc))
      print *, "Test case 10: start, stop for derived event ",
     *"PAPI_L2_TCM."
#else
      print *, "Test case 10: start, stop for derived event ",
     *"PAPI_L1_TCM."
#endif
 
      print *, "------------------------------------------------------"
      end if
      call PAPIf_get_domain(es1, domain, PAPI_DEFDOM, retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_get_domain', retval)
      end if

      call stringify_domain(domain, domainstr)
      if (tests_quiet .EQ. 0) then
      write (*,900) "Default domain is:", domain, domainstr
 900  format(a20, i3, " ", a70)
      end if

      call PAPIf_get_granularity(es1, granularity, PAPI_DEFGRN, 
     *retval)
      if ( retval .NE. PAPI_OK ) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_get_granularity', 
     *retval)
      end if

      call stringify_granularity(granularity, grnstr)
      if (tests_quiet .EQ. 0) then
      write (*,800) "Default granularity is:", granularity, grnstr
 800  format(a25, i3, " ", a20)
      print *, "Using", NUM_FLOPS, " iterations of c += b*c"
      print *, "------------------------------------------------------"
      
      write (*,500) "Test type", 1, 2, 3
#if (defined(sun) && defined(sparc))
      write (*,500) EVT1_STR, values(1), 0, 0
      write (*,500) EVT2_STR, 0, values(3), 0
      write (*,500) EVT3_STR, 0, 0, values(5)
      print *, "------------------------------------------------",
     *"------"
 
      print *, "Verification:"
      print *, "First number row 1 approximately equals (2,2) - (3,3) ", 
     *"or ",(values(3)-values(5))
#else
      write (*,500) EVT1_STR, values(1), 0, 0
      write (*,500) EVT2_STR, 0, values(3), 0 
      write (*,500) EVT3_STR, 0, 0, values(5) 
      print *, "------------------------------------------------",
     *"------"
 
      print *, "Verification:"
      print *, "First number row 1 approximately equals (2,2) + (3,3) ",
     *"or ", (values(3)+values(5))
#endif
      end if
 500  format(A13, ": ", I10, I10, I10)

      call ftests_pass(__FILE__)
      end
