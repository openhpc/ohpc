/* This file performs the following test: start, stop and 
   timer functionality for L1 related events

   - They are counted in the default counting domain and default
     granularity, depending on the platform. Usually this is 
     the user domain (PAPI_DOM_USER) and thread context (PAPI_GRN_THR).

   - Start counters
   - Do iterations
   - Stop and read counters
*/

#include "papi_test.h"

#define OUT_FMT		"%12d\t%12lld\t%12lld\t%.2f\n"


int
main( int argc, char **argv )
{
	int retval, i, j;
	int EventSet = PAPI_NULL;
	long long values[2];
	const PAPI_hw_info_t *hwinfo = NULL;
	char descr[PAPI_MAX_STR_LEN];
	PAPI_event_info_t evinfo;
	PAPI_mh_level_t *L;


	const int eventlist[] = {
		PAPI_L1_DCA,
		PAPI_L1_DCM,
		PAPI_L1_DCH,
		PAPI_L2_DCA,
		PAPI_L2_DCM,
		PAPI_L2_DCH,
#if 0
		PAPI_L1_LDM,
		PAPI_L1_STM,
		PAPI_L1_DCR,
		PAPI_L1_DCW,
		PAPI_L1_ICM,
		PAPI_L1_TCM,
		PAPI_LD_INS,
		PAPI_SR_INS,
		PAPI_LST_INS,
		PAPI_L2_DCR,
		PAPI_L2_DCW,
		PAPI_CSR_TOT,
		PAPI_MEM_SCY,
		PAPI_MEM_RCY,
		PAPI_MEM_WCY,
		PAPI_L1_ICH,
		PAPI_L1_ICA,
		PAPI_L1_ICR,
		PAPI_L1_ICW,
		PAPI_L1_TCH,
		PAPI_L1_TCA,
		PAPI_L1_TCR,
		PAPI_L1_TCW,
		PAPI_L2_DCM,
		PAPI_L2_ICM,
		PAPI_L2_TCM,
		PAPI_L2_LDM,
		PAPI_L2_STM,
		PAPI_L2_DCH,
		PAPI_L2_DCA,
		PAPI_L2_DCR,
		PAPI_L2_DCW,
		PAPI_L2_ICH,
		PAPI_L2_ICA,
		PAPI_L2_ICR,
		PAPI_L2_ICW,
		PAPI_L2_TCH,
		PAPI_L2_TCA,
		PAPI_L2_TCR,
		PAPI_L2_TCW,
#endif
		0
	};

	tests_quiet( argc, argv );	/* Set TESTS_QUIET variable */

	if ( ( retval =
		   PAPI_library_init( PAPI_VER_CURRENT ) ) != PAPI_VER_CURRENT )
		test_fail( __FILE__, __LINE__, "PAPI_library_init", retval );

	if ( ( hwinfo = PAPI_get_hardware_info(  ) ) == NULL ) {
		test_fail( __FILE__, __LINE__, "PAPI_get_hardware_info", 2 );
	}

	if ( ( retval = PAPI_create_eventset( &EventSet ) ) != PAPI_OK )
		test_fail( __FILE__, __LINE__, "PAPI_create_eventset", retval );

	/* Extract and report the cache information */
	L = ( PAPI_mh_level_t * ) ( hwinfo->mem_hierarchy.level );
	for ( i = 0; i < hwinfo->mem_hierarchy.levels; i++ ) {
		for ( j = 0; j < 2; j++ ) {
			int tmp;

			tmp = PAPI_MH_CACHE_TYPE( L[i].cache[j].type );
			if ( tmp == PAPI_MH_TYPE_UNIFIED ) {
				printf( "L%d Unified ", i + 1 );
			} else if ( tmp == PAPI_MH_TYPE_DATA ) {
				printf( "L%d Data ", i + 1 );
			} else if ( tmp == PAPI_MH_TYPE_INST ) {
				printf( "L%d Instruction ", i + 1 );
			} else if ( tmp == PAPI_MH_TYPE_VECTOR ) {
				printf( "L%d Vector ", i + 1 );
			} else if ( tmp == PAPI_MH_TYPE_TRACE ) {
				printf( "L%d Trace ", i + 1 );
			} else if ( tmp == PAPI_MH_TYPE_EMPTY ) {
				break;
			} else {
				test_fail( __FILE__, __LINE__, "PAPI_get_hardware_info",
						   PAPI_EBUG );
			}

			tmp = PAPI_MH_CACHE_WRITE_POLICY( L[i].cache[j].type );
			if ( tmp == PAPI_MH_TYPE_WB ) {
				printf( "Write back " );
			} else if ( tmp == PAPI_MH_TYPE_WT ) {
				printf( "Write through " );
			} else {
				test_fail( __FILE__, __LINE__, "PAPI_get_hardware_info",
						   PAPI_EBUG );
			}

			tmp = PAPI_MH_CACHE_REPLACEMENT_POLICY( L[i].cache[j].type );
			if ( tmp == PAPI_MH_TYPE_PSEUDO_LRU ) {
				printf( "Pseudo LRU policy " );
			} else if ( tmp == PAPI_MH_TYPE_LRU ) {
				printf( "LRU policy " );
			} else if ( tmp == PAPI_MH_TYPE_UNKNOWN ) {
				printf( "Unknown policy " );
			} else {
				test_fail( __FILE__, __LINE__, "PAPI_get_hardware_info",
						   PAPI_EBUG );
			}

			printf( "Cache:\n" );
			if ( L[i].cache[j].type ) {
				printf
					( "  Total size: %dKB\n  Line size: %dB\n  Number of Lines: %d\n  Associativity: %d\n\n",
					  ( L[i].cache[j].size ) >> 10, L[i].cache[j].line_size,
					  L[i].cache[j].num_lines, L[i].cache[j].associativity );
			}
		}
	}

	for ( i = 0; eventlist[i] != 0; i++ ) {
	        if (PAPI_event_code_to_name( eventlist[i], descr ) != PAPI_OK)
	           continue;
		if ( PAPI_add_event( EventSet, eventlist[i] ) != PAPI_OK )
		   continue;

		if ( PAPI_get_event_info( eventlist[i], &evinfo ) != PAPI_OK )
			test_fail( __FILE__, __LINE__, "PAPI_get_event_info", retval );

		printf( "\nEvent: %s\nShort: %s\nLong: %s\n\n",
				evinfo.symbol, evinfo.short_descr, evinfo.long_descr );
		printf( "       Bytes\t\tCold\t\tWarm\tPercent\n" );

		if ( ( retval = PAPI_start( EventSet ) ) != PAPI_OK )
			test_fail( __FILE__, __LINE__, "PAPI_start", retval );

		for ( j = 512; j <= 16 * ( 1024 * 1024 ); j = j * 2 ) {
			do_misses( 1, j );
			do_flush(  );

			if ( ( retval = PAPI_reset( EventSet ) ) != PAPI_OK )
				test_fail( __FILE__, __LINE__, "PAPI_reset", retval );

			do_misses( 1, j );

			if ( ( retval = PAPI_read( EventSet, &values[0] ) ) != PAPI_OK )
				test_fail( __FILE__, __LINE__, "PAPI_read", retval );
			if ( ( retval = PAPI_reset( EventSet ) ) != PAPI_OK )
				test_fail( __FILE__, __LINE__, "PAPI_reset", retval );

			do_misses( 1, j );

			if ( ( retval = PAPI_read( EventSet, &values[1] ) ) != PAPI_OK )
				test_fail( __FILE__, __LINE__, "PAPI_read", retval );

			printf( OUT_FMT, j, values[0], values[1],
					( ( float ) values[1] /
					  ( float ) ( ( values[0] !=
									0 ) ? values[0] : 1 ) * 100.0 ) );
		}

		if ( ( retval = PAPI_stop( EventSet, NULL ) ) != PAPI_OK )
			test_fail( __FILE__, __LINE__, "PAPI_stop", retval );

		if ( ( retval =
			   PAPI_remove_event( EventSet, eventlist[i] ) ) != PAPI_OK )
			test_fail( __FILE__, __LINE__, "PAPI_remove_event", retval );
	}

	if ( ( retval = PAPI_destroy_eventset( &EventSet ) ) != PAPI_OK )
		test_fail( __FILE__, __LINE__, "PAPI_destroy_eventset", retval );

	test_pass( __FILE__, NULL, 0 );

	exit( 1 );
}
