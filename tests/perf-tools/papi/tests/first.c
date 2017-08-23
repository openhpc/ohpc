/* This file performs the following test: start, read, stop and again functionality

   - It attempts to use the following three counters. It may use less depending on
     hardware counter resource limitations. These are counted in the default counting
     domain and default granularity, depending on the platform. Usually this is 
     the user domain (PAPI_DOM_USER) and thread context (PAPI_GRN_THR).
     + PAPI_FP_INS or PAPI_TOT_INS if PAPI_FP_INS doesn't exist
     + PAPI_TOT_CYC
   - Start counters
   - Do flops
   - Read counters
   - Reset counters
   - Do flops
   - Read counters
   - Do flops
   - Read counters
   - Do flops
   - Stop and read counters
   - Read counters
*/

#include "papi_test.h"

extern int TESTS_QUIET;				   /* Declared in test_utils.c */

int
main( int argc, char **argv )
{
	int retval, num_tests = 5, num_events, tmp;
	long long **values;
	int EventSet = PAPI_NULL;
	int PAPI_event, mask;
	char event_name[PAPI_MAX_STR_LEN], add_event_str[PAPI_MAX_STR_LEN];
	long long min, max;

	/* Set TESTS_QUIET variable */
	tests_quiet( argc, argv );	

	/* Init PAPI library */
	retval = PAPI_library_init( PAPI_VER_CURRENT );
	if ( retval != PAPI_VER_CURRENT ) {
	   test_fail( __FILE__, __LINE__, "PAPI_library_init", retval );
	}

	/* add PAPI_TOT_CYC and one of the events in 
           PAPI_FP_INS, PAPI_FP_OPS or PAPI_TOT_INS, 
           depending on the availability of the event 
           on the platform                             */
	EventSet = add_two_events( &num_events, &PAPI_event, &mask );

	retval = PAPI_event_code_to_name( PAPI_event, event_name );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_event_code_to_name", retval );
	}
	sprintf( add_event_str, "PAPI_add_event[%s]", event_name );

	/* Allocate space for results */
	values = allocate_test_space( num_tests, num_events );

	/* Start PAPI */
	retval = PAPI_start( EventSet );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_start", retval );
	}

	/* Benchmark code */
	do_flops( NUM_FLOPS );

	/* read results 0 */
	retval = PAPI_read( EventSet, values[0] );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_read", retval );
	}

	/* Reset */
	retval = PAPI_reset( EventSet );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_reset", retval );
	}

	/* Benchmark some more */
	do_flops( NUM_FLOPS );

	/* Read Results 1 */
	retval = PAPI_read( EventSet, values[1] );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_read", retval );
	}

	/* Benchmark some more */
	do_flops( NUM_FLOPS );

	/* Read results 2 */
	retval = PAPI_read( EventSet, values[2] );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_read", retval );
	}

	/* Benchmark some more */
	do_flops( NUM_FLOPS );

	/* Read results 3 */
	retval = PAPI_stop( EventSet, values[3] );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_stop", retval );
	}

	/* Read results 4 */
	retval = PAPI_read( EventSet, values[4] );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_read", retval );
	}

	/* remove results.  We never stop??? */
	remove_test_events( &EventSet, mask );

	if ( !TESTS_QUIET ) {
	   printf( "Test case 1: Non-overlapping start, stop, read.\n" );
	   printf( "-----------------------------------------------\n" );
	   tmp = PAPI_get_opt( PAPI_DEFDOM, NULL );
	   printf( "Default domain is: %d (%s)\n", tmp,
				stringify_all_domains( tmp ) );
	   tmp = PAPI_get_opt( PAPI_DEFGRN, NULL );
	   printf( "Default granularity is: %d (%s)\n", tmp,
				stringify_granularity( tmp ) );
	   printf( "Using %d iterations of c += a*b\n", NUM_FLOPS );
	   printf( "-------------------------------------------------------------------------\n" );

	   printf( "Test type   :        1           2           3           4           5\n" );
	   sprintf( add_event_str, "%s:", event_name );
	   printf( TAB5, add_event_str,
		   values[0][1], values[1][1], values[2][1], 
                   values[3][1], values[4][1] );
	   printf( TAB5, "PAPI_TOT_CYC:", 
                   values[0][0], values[1][0], values[2][0], 
                   values[3][0], values[4][0] );
	   printf( "-------------------------------------------------------------------------\n" );
		
           printf( "Verification:\n" );
	   printf( "Row 1 Column 1 at least %d\n", NUM_FLOPS );
	   printf( "%% difference between %s 1 & 2: %.2f\n", add_event_str,
				100.0 * ( float ) values[0][1] /
				( float ) values[1][1] );
	   printf( "%% difference between %s 1 & 2: %.2f\n", "PAPI_TOT_CYC",
				100.0 * ( float ) values[0][0] /
				( float ) values[1][0] );
	   printf( "Column 1 approximately equals column 2\n" );
	   printf( "Column 3 approximately equals 2 * column 2\n" );
	   printf( "Column 4 approximately equals 3 * column 2\n" );
	   printf( "Column 4 exactly equals column 5\n" );
	}

	/* Validation */

	/* Check cycles constraints */

	min = ( long long ) ( ( double ) values[1][0] * .7 );
	max = ( long long ) ( ( double ) values[1][0] * 1.3 );

	/* Check constraint Col1=Col2 */		
	if ( values[0][0] > max || values[0][0] < min ) {
	   test_fail( __FILE__, __LINE__, "Cycle Col1!=Col2", 1 );
	}
	/* Check constraint col3 == 2*col2 */
	if ( (values[2][0] > ( 2 * max )) || 
	     (values[2][0] < ( 2 * min )) ) {
	   test_fail( __FILE__, __LINE__, "Cycle Col3!=2*Col2", 1 );
	}
	/* Check constraint col4 == 3*col2 */
	if ( (values[3][0] > ( 3 * max )) || 
	     (values[3][0] < ( 3 * min )) ) {
	   test_fail( __FILE__, __LINE__, "Cycle Col3!=3*Col2", 1 );
	}
	/* Check constraint col4 == col5 */
	if ( values[3][0] != values[4][0] ) {
	   test_fail( __FILE__, __LINE__, "Cycle Col4!=Col5", 1 );
	}


	/* Check FLOP constraints */

	min = ( long long ) ( ( double ) values[1][1] * .9 );
	max = ( long long ) ( ( double ) values[1][1] * 1.1 );

	/* Check constraint Col1=Col2 */
	if ( values[0][1] > max || values[0][1] < min ) {
	   /* test_fail( __FILE__, __LINE__, "FLOP Col1!=Col2", 1 ); */
        printf( "Warning: FLOP Col1!=Col2\n");
	}
	/* Check constraint col3 == 2*col2 */
	if ( (values[2][1] > ( 2 * max )) || 
             (values[2][1] < ( 2 * min )) ) {
	   /* test_fail( __FILE__, __LINE__, "FLOP Col3!=2*Col2", 1 ); */
        printf( "Warning: FLOP Col3!=2*Col1\n");
	}
	/* Check constraint col4 == 3*col2 */
	if ( (values[3][1] > ( 3 * max )) || 
	     (values[3][1] < ( 3 * min )) ) {
	   /* test_fail( __FILE__, __LINE__, "FLOP Col4!=3*Col2", 1 ); */
        printf( "Warning: FLOP Col4!=3*Col2\n");
	}
	/* Check constraint col4 == col5 */
	if (values[3][1] != values[4][1]) {
	   /* test_fail( __FILE__, __LINE__, "FLOP Col4!=Col5", 1 ); */
        printf( "Warning: FLOP Col4!=Col5\n");
	}
	/* Check flops are sane */
	if (values[0][1] < ( long long ) NUM_FLOPS ) {
	   test_fail( __FILE__, __LINE__, "FLOP sanity", 1 );
	}


	test_pass( __FILE__, values, num_tests );
	return 0;

}
