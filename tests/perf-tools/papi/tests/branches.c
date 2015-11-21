/*
 * $Id$
 *
 * Test example for branch accuracy and functionality, originally 
 * provided by Timothy Kaiser, SDSC. It was modified to fit the 
 * PAPI test suite by Nils Smeds, <smeds@pdc.kth.se>.
 * and Phil Mucci <mucci@cs.utk.edu>
 * This example verifies the accuracy of branch events
 */

#include "papi_test.h"
#include <stdio.h>
#include <math.h>

#define MAXEVENTS 4
#define SLEEPTIME 100
#define MINCOUNTS 100000

static double dummy3( double x, int iters );

int
main( int argc, char **argv )
{
	PAPI_event_info_t info;
	int i, j, retval;
	int iters = 15000000;
	double x = 1.1, y;
	long long t1, t2;
	long long values[MAXEVENTS], refvalues[MAXEVENTS];
	int sleep_time = SLEEPTIME;
	double spread[MAXEVENTS];
	int nevents = MAXEVENTS;
	int eventset = PAPI_NULL;
	int events[MAXEVENTS];

	events[0] = PAPI_BR_NTK;
	events[1] = PAPI_BR_PRC;
	events[2] = PAPI_BR_INS;
	events[3] = PAPI_BR_MSP;
/*
  events[3]=PAPI_BR_CN; 
  events[4]=PAPI_BR_UCN;*/
	/*events[5]=PAPI_BR_TKN; */


	for ( i = 0; i < MAXEVENTS; i++ ) {
		values[i] = 0;
	}

	if ( argc > 1 ) {
		if ( !strcmp( argv[1], "TESTS_QUIET" ) )
			tests_quiet( argc, argv );
		else {
			sleep_time = atoi( argv[1] );
			if ( sleep_time <= 0 )
				sleep_time = SLEEPTIME;
		}
	}

	if ( !TESTS_QUIET ) {
		printf( "\nAccuracy check of branch presets.\n" );
		printf( "Comparing a measurement with separate measurements.\n\n" );
	}

	if ( ( retval =
		   PAPI_library_init( PAPI_VER_CURRENT ) ) != PAPI_VER_CURRENT )
		test_fail( __FILE__, __LINE__, "PAPI_library_init", retval );

	if ( ( retval = PAPI_create_eventset( &eventset ) ) )
		test_fail( __FILE__, __LINE__, "PAPI_create_eventset", retval );

#ifdef MPX
	if ( ( retval = PAPI_multiplex_init(  ) ) )
		test_fail( __FILE__, __LINE__, "PAPI_multiplex_init", retval );

	if ( ( retval = PAPI_set_multiplex( eventset ) ) )
		test_fail( __FILE__, __LINE__, "PAPI_set_multiplex", retval );
#endif

	nevents = 0;

	for ( i = 0; i < MAXEVENTS; i++ ) {
		if ( PAPI_query_event( events[i] ) != PAPI_OK )
			continue;
		if ( PAPI_add_event( eventset, events[i] ) == PAPI_OK ) {
			events[nevents] = events[i];
			nevents++;
		}
	}

	if ( nevents < 1 )
		test_skip( __FILE__, __LINE__, "Not enough events left...", 0 );

	/* Find a reasonable number of iterations (each 
	 * event active 20 times) during the measurement
	 */
	t2 = (long long)(10000 * 20) * nevents;	/* Target: 10000 usec/multiplex, 20 repeats */
	if ( t2 > 30e6 )
		test_skip( __FILE__, __LINE__, "This test takes too much time",
				   retval );

	/* Measure one run */
	t1 = PAPI_get_real_usec(  );
	y = dummy3( x, iters );
	t1 = PAPI_get_real_usec(  ) - t1;

	if ( t2 > t1 )			 /* Scale up execution time to match t2 */
        if ( t1 )
		    iters = iters * ( int ) ( t2 / t1 );
        else 
            printf( "ERR: Low clock resolution. Increase work\n" );
	else if ( t1 > 30e6 )	 /* Make sure execution time is < 30s per repeated test */
		test_skip( __FILE__, __LINE__, "This test takes too much time",
				   retval );

	x = 1.0;

	if ( !TESTS_QUIET )
		printf( "\nFirst run: Together.\n" );

	t1 = PAPI_get_real_usec(  );
	if ( ( retval = PAPI_start( eventset ) ) )
		test_fail( __FILE__, __LINE__, "PAPI_start", retval );
	y = dummy3( x, iters );
	if ( ( retval = PAPI_stop( eventset, values ) ) )
		test_fail( __FILE__, __LINE__, "PAPI_stop", retval );
	t2 = PAPI_get_real_usec(  );

	if ( !TESTS_QUIET ) {
		printf( "\tOperations= %.1f Mflop", y * 1e-6 );
        if ( t2 > t1 )
	    	printf( "\t(%g Mflop/s)\n\n", ( y / ( double ) ( t2 - t1 ) ) );
        else 
            printf( "ERR: Low clock resolution. Increase work\n" );
		printf( "PAPI grouped measurement:\n" );
	}
	for ( j = 0; j < nevents; j++ ) {
		PAPI_get_event_info( events[j], &info );
		if ( !TESTS_QUIET ) {
			printf( "%20s = ", info.short_descr );
			printf( LLDFMT, values[j] );
			printf( "\n" );
		}
	}
	if ( !TESTS_QUIET )
		printf( "\n" );


	if ( ( retval = PAPI_remove_events( eventset, events, nevents ) ) )
		test_fail( __FILE__, __LINE__, "PAPI_remove_events", retval );
	if ( ( retval = PAPI_destroy_eventset( &eventset ) ) )
		test_fail( __FILE__, __LINE__, "PAPI_destroy_eventset", retval );
	eventset = PAPI_NULL;
	if ( ( retval = PAPI_create_eventset( &eventset ) ) )
		test_fail( __FILE__, __LINE__, "PAPI_create_eventset", retval );

	for ( i = 0; i < nevents; i++ ) {

		if ( ( retval = PAPI_cleanup_eventset( eventset ) ) )
			test_fail( __FILE__, __LINE__, "PAPI_cleanup_eventset", retval );
		if ( ( retval = PAPI_add_event( eventset, events[i] ) ) )
			test_fail( __FILE__, __LINE__, "PAPI_add_event", retval );

		x = 1.0;

		if ( !TESTS_QUIET )
			printf( "\nReference measurement %d (of %d):\n", i + 1, nevents );

		t1 = PAPI_get_real_usec(  );
		if ( ( retval = PAPI_start( eventset ) ) )
			test_fail( __FILE__, __LINE__, "PAPI_start", retval );
		y = dummy3( x, iters );
		if ( ( retval = PAPI_stop( eventset, &refvalues[i] ) ) )
			test_fail( __FILE__, __LINE__, "PAPI_stop", retval );
		t2 = PAPI_get_real_usec(  );

		if ( !TESTS_QUIET ) {
			printf( "\tOperations= %.1f Mflop", y * 1e-6 );
            if ( t2 > t1 )
    			printf( "\t(%g Mflop/s)\n\n", ( y / ( double ) ( t2 - t1 ) ) );
            else 
                printf( "ERR: Low clock resolution. Increase work\n" );
		}
		PAPI_get_event_info( events[i], &info );
		if ( !TESTS_QUIET ) {
			printf( "PAPI results:\n%20s = ", info.short_descr );
			printf( LLDFMT, refvalues[i] );
			printf( "\n" );
		}
	}
	if ( !TESTS_QUIET )
		printf( "\n" );


	if ( !TESTS_QUIET ) {
		printf( "\n\nRelative accuracy:\n" );
		for ( j = 0; j < nevents; j++ )
			printf( "   Event %.2d", j );
		printf( "\n" );
	}

	for ( j = 0; j < nevents; j++ ) {
		spread[j] = abs( ( int ) ( refvalues[j] - values[j] ) );
		if ( values[j] )
			spread[j] /= ( double ) values[j];
		if ( !TESTS_QUIET )
			printf( "%10.3g ", spread[j] );
		/* Make sure that NaN get counted as errors */
		if ( spread[j] < MPX_TOLERANCE )
			i--;
		else if ( refvalues[j] < MINCOUNTS )	/* Neglect inprecise results with low counts */
			i--;
	}
	if ( !TESTS_QUIET )
		printf( "\n\n" );

	if ( i )
		test_fail( __FILE__, __LINE__, "Values outside threshold", i );
	else
		test_pass( __FILE__, NULL, 0 );

	return 0;
}

static double
dummy3( double x, int iters )
{
	int i;
	double w, y, z, a, b, c, d, e, f, g, h;
	double one;
	one = 1.0;
	w = x;
	y = x;
	z = x;
	a = x;
	b = x;
	c = x;
	d = x;
	e = x;
	f = x;
	g = x;
	h = x;
	for ( i = 1; i <= iters; i++ ) {
		w = w * 1.000000000001 + one;
		y = y * 1.000000000002 + one;
		z = z * 1.000000000003 + one;
		a = a * 1.000000000004 + one;
		b = b * 1.000000000005 + one;
		c = c * 0.999999999999 + one;
		d = d * 0.999999999998 + one;
		e = e * 0.999999999997 + one;
		f = f * 0.999999999996 + one;
		g = h * 0.999999999995 + one;
		h = h * 1.000000000006 + one;
	}
	return 2.0 * ( a + b + c + d + e + f + w + x + y + z + g + h );
}
