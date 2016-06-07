/* This test exercises the PAPI_TOT_CYC and PAPI_REF_CYC counters.
	PAPI_TOT_CYC should measure the number of cycles required to do a fixed amount of
	work. It should be roughly constant for constant work, regardless of the speed state
	a core is in.
	PAPI_REF_CYC should measure the number of cycles at a constant reference clock rate,
	independent of the actual clock rate of the core.
	Thus if the core is running at nominal clock rate, PAPI_TOT_CYC and PAPI_REF_CYC
	should match and the ratio should be approximately 1.
	If the core is in an idle state (such as at startup), the ratio of TOT / REF should be
	less than 1.
	If the core is accelerated above nominal, such as TurboBoost when only one core is
	active, the ratio of TOT / REF should be greater than 1.

	This test measures the ratio first from a roughly idle state. It then does floating
	point intensive work to push this core into a fully active or accelerated state,
	and then it measures the ratio again.
	
	Using this technique allows you to measure the effective clock rate of the processor
	over a specific region of code, allowing you to infer the state of acceleration.
*/

#include "papi_test.h"

static void work (int EventSet, int mhz);

int
main( int argc, char **argv )
{
	int retval;
	int EventSet = PAPI_NULL;
	int numflops = NUM_FLOPS;
	const PAPI_hw_info_t *hwinfo = NULL;
	long long elapsed_cyc;
	long long values[2];
	int mhz;

	/* Set TESTS_QUIET variable */
	tests_quiet( argc, argv );	

	/* Init the PAPI library */
	retval = PAPI_library_init( PAPI_VER_CURRENT );
	if ( retval != PAPI_VER_CURRENT ) {
	   test_fail( __FILE__, __LINE__, "PAPI_library_init", retval );
	}

	retval = PAPI_query_named_event("PAPI_REF_CYC");
	if (PAPI_OK!=retval) {
		test_skip( __FILE__, __LINE__, "PAPI_REF_CYC is not defined on this platform.", PAPI_OK );
	}

    /* create an eventset */
	retval = PAPI_create_eventset( &EventSet );
	if ( retval != PAPI_OK ) {
		test_fail( __FILE__, __LINE__, "PAPI_create_eventset", retval );
  }

    /* add core cycle event */
	retval = PAPI_add_named_event( EventSet, "PAPI_TOT_CYC");
	if ( retval != PAPI_OK ) {
		test_fail( __FILE__, __LINE__, "PAPI_add_named_event: PAPI_TOT_CYC", retval );
  }

    /* add ref cycle event */
	retval = PAPI_add_named_event( EventSet, "PAPI_REF_CYC");
	if ( retval != PAPI_OK ) {
		test_fail( __FILE__, __LINE__, "PAPI_add_events: PAPI_REF_CYC", retval );
  }
  
	retval = papi_print_header
	( "Test case CycleRatio.c: Compute the ratio of TOT and REF cycles.\n",
	  &hwinfo );
	if ( retval != PAPI_OK )
		test_fail( __FILE__, __LINE__, "PAPI_get_hardware_info", 2 );
	
	/* compute a nominal bus clock frequency */
	retval = PAPI_start( EventSet );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_start", retval );
	}
	
	elapsed_cyc = PAPI_get_real_cyc(  );
	usleep(1000000);
	elapsed_cyc = PAPI_get_real_cyc(  ) - elapsed_cyc;
	mhz = elapsed_cyc / 1000000;
	
	retval = PAPI_stop( EventSet, values );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_stop", retval );
	}
	
	if ( values[1] == 0 ) {
	   test_warn( __FILE__, __LINE__, "PAPI_REF_CYC = 0\nTry upgrading your kernel.", 0 );
	}
	
    printf( "CPU Computed Megahertz   : %d\n", mhz );
    printf( "Measure TOT and REF cycles from a cold start\n" );

	work(EventSet, mhz);
	do_flops(10*numflops);
	
	printf( "\nMeasure again after working for a while\n" );

	work(EventSet, mhz);
	test_pass( __FILE__, NULL, 0 );
	return ( 0 );
}

static void work (int EventSet, int mhz)
{
	int retval;
	long long values[2];
	long long elapsed_us, elapsed_cyc, elapsed_virt_us, elapsed_virt_cyc;
	double cycles_error;
	float ratio, ratio1;
	int numflops = NUM_FLOPS;

	ratio = ratio1 = 0;

	/* Gather before stats */
	elapsed_us = PAPI_get_real_usec(  );
	elapsed_cyc = PAPI_get_real_cyc(  );
	elapsed_virt_us = PAPI_get_virt_usec(  );
	elapsed_virt_cyc = PAPI_get_virt_cyc(  );

	/* Start PAPI */
	retval = PAPI_start( EventSet );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_start", retval );
	}

	/* our test code */
	do_flops( numflops );

	/* Stop PAPI */
	retval = PAPI_stop( EventSet, values );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_stop", retval );
	}
	ratio = (float)values[0]/(float)values[1];

	/* Calculate total values */
	elapsed_virt_us = PAPI_get_virt_usec(  ) - elapsed_virt_us;
	elapsed_virt_cyc = PAPI_get_virt_cyc(  ) - elapsed_virt_cyc;
	elapsed_us = PAPI_get_real_usec(  ) - elapsed_us;
	elapsed_cyc = PAPI_get_real_cyc(  ) - elapsed_cyc;

   printf( "-------------------------------------------------------------------------\n" );
   printf( "Using %d iterations of c += a*b\n", numflops );
   printf( "-------------------------------------------------------------------------\n" );

   printf( TAB1, "PAPI_TOT_CYC             : \t", values[0] );
   printf( TAB1, "PAPI_REF_CYC             : \t", values[1] );
   printf( "%-12s %12f\n", "Cycle Ratio              : \t", ratio );
   printf( "%-12s %12d\n", "Effective MHz            : \t", (int)(ratio * mhz) );
   printf( TAB1, "Real usec                : \t", elapsed_us );
   printf( TAB1, "Real cycles              : \t", elapsed_cyc );
   printf( TAB1, "Virt usec                : \t", elapsed_virt_us );
   printf( TAB1, "Virt cycles              : \t", elapsed_virt_cyc );

   printf( "-------------------------------------------------------------------------\n" );

   printf( "Verification: PAPI_REF_CYC should be roughly equal to real_cycles\n" );
   cycles_error=100.0*((double)values[1] - (double)elapsed_cyc)/(double)elapsed_cyc;

   if ((cycles_error>10.0) || (cycles_error<-10.0)) {
	 printf("Error of %.2f%%\n",cycles_error);
	 test_warn( __FILE__, __LINE__, "validation", 0 );
   }

}
