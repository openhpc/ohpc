/*
 * This file perfoms the following test:  dynamic memory info
 * The pages used should increase steadily.
 *
 * Author: Kevin London
 *	   london@cs.utk.edu
 */
#include "papi_test.h"

#define ALLOCMEM 200000
static void
dump_memory_info( FILE * output, PAPI_dmem_info_t * d )
{
	fprintf( output, "\n--------\n" );
	fprintf( output, "Mem Size:\t\t%lld\n", d->size );
	fprintf( output, "Mem Peak Size:\t\t%lld\n", d->peak );
	fprintf( output, "Mem Resident:\t\t%lld\n", d->resident );
	fprintf( output, "Mem High Water Mark:\t%lld\n", d->high_water_mark );
	fprintf( output, "Mem Shared:\t\t%lld\n", d->shared );
	fprintf( output, "Mem Text:\t\t%lld\n", d->text );
	fprintf( output, "Mem Library:\t\t%lld\n", d->library );
	fprintf( output, "Mem Heap:\t\t%lld\n", d->heap );
	fprintf( output, "Mem Locked:\t\t%lld\n", d->locked );
	fprintf( output, "Mem Stack:\t\t%lld\n", d->stack );
	fprintf( output, "Mem Pagesize:\t\t%lld\n", d->pagesize );
	fprintf( output, "Mem Page Table Entries:\t\t%lld\n", d->pte );
	fprintf( output, "--------\n\n" );
}

int
main( int argc, char **argv )
{
	PAPI_dmem_info_t dmem;
	long long value[7];
	int retval, i = 0, j = 0;
	double *m[7];

	tests_quiet( argc, argv );	/* Set TESTS_QUIET variable */
	retval = PAPI_library_init( PAPI_VER_CURRENT );
	if ( retval != PAPI_VER_CURRENT )
		test_fail( __FILE__, __LINE__, "PAPI_library_init", retval );

	for ( i = 0; i < 7; i++ ) {
		retval = PAPI_get_dmem_info( &dmem );
		if ( retval != PAPI_OK )
			test_fail( __FILE__, __LINE__, "PAPI_get_dmem_info", retval );
/*	   dump_memory_info(stdout,&dmem); */
		value[i] = dmem.size;
		m[i] = ( double * ) malloc( ALLOCMEM * sizeof ( double ) );
		touch_dummy( m[j], ALLOCMEM );
	}

	if ( !TESTS_QUIET ) {
		printf( "Test case:  Dynamic Memory Information.\n" );
		dump_memory_info( stdout, &dmem );
		printf
			( "------------------------------------------------------------------------\n" );
		for ( i = 0; i < 7; i++ )
			printf( "Malloc additional: %d KB  Memory Size in KB: %d\n",
					( int ) ( ( sizeof ( double ) * ALLOCMEM ) / 1024 ),
					( int ) value[i] );
		printf
			( "------------------------------------------------------------------------\n" );
	}
	if ( value[6] >= value[5] && value[5] >= value[4] && value[4] >= value[3]
		 && value[3] >= value[2] && value[2] >= value[1] &&
		 value[1] >= value[0] )
		test_pass( __FILE__, NULL, 0 );
	else
		test_fail( __FILE__, __LINE__, "Calculating Resident Memory",
				   ( int ) value[6] );

	exit( 1 );
}
