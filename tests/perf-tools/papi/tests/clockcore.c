#include "papi_test.h"

static char *func_name[] = {
	"PAPI_get_real_cyc",
	"PAPI_get_real_usec",
	"PAPI_get_virt_cyc",
	"PAPI_get_virt_usec"
};
static int CLOCK_ERROR = 0;

void
clock_res_check( int flag )
{
	if ( CLOCK_ERROR )
		return;

	long long *elapsed_cyc, total_cyc = 0, uniq_cyc = 0, diff_cyc = 0;
	int i;
	double min, max, average, std, tmp;

	elapsed_cyc = ( long long * ) calloc( NUM_ITERS, sizeof ( long long ) );

	/* Real */
	switch ( flag ) {
	case 0:
		for ( i = 0; i < NUM_ITERS; i++ )
			elapsed_cyc[i] = ( long long ) PAPI_get_real_cyc(  );
		break;
	case 1:
		for ( i = 0; i < NUM_ITERS; i++ )
			elapsed_cyc[i] = ( long long ) PAPI_get_real_usec(  );
		break;
	case 2:
		for ( i = 0; i < NUM_ITERS; i++ )
			elapsed_cyc[i] = ( long long ) PAPI_get_virt_cyc(  );
		break;
	case 3:
		for ( i = 0; i < NUM_ITERS; i++ )
			elapsed_cyc[i] = ( long long ) PAPI_get_virt_usec(  );
		break;
	default:
		test_fail( __FILE__, __LINE__, "clock_res_check", -1 );

	}

	min = max = ( double ) ( elapsed_cyc[1] - elapsed_cyc[0] );

	for ( i = 1; i < NUM_ITERS; i++ ) {
		if ( elapsed_cyc[i] - elapsed_cyc[i - 1] < 0 ) {
			CLOCK_ERROR = 1;
			test_fail( __FILE__, __LINE__, "Negative elapsed time", -1 );
			free( elapsed_cyc );
			return;
		}

		diff_cyc = elapsed_cyc[i] - elapsed_cyc[i - 1];
		if ( min > diff_cyc )
			min = ( double ) diff_cyc;
		if ( max < diff_cyc )
			max = ( double ) diff_cyc;
		if ( diff_cyc != 0 )
			uniq_cyc++;
		total_cyc += diff_cyc;
	}

	average = ( double ) total_cyc / ( NUM_ITERS - 1 );
	std = 0;

	for ( i = 1; i < NUM_ITERS; i++ ) {
		tmp = ( double ) ( elapsed_cyc[i] - elapsed_cyc[i - 1] );
		tmp = tmp - average;
		std += tmp * tmp;
	}

	std = sqrt( std / ( NUM_ITERS - 2 ) );
	printf( "%s: min %.3lf  max %.3lf \n", func_name[flag], min, max );
	printf( "                   average %.3lf std %.3lf\n", average, std );

	if ( !TESTS_QUIET ) {
		if ( uniq_cyc == NUM_ITERS - 1 ) {
			printf( "%s : %7.3f   <%7.3f\n", func_name[flag],
					( double ) total_cyc / ( double ) ( NUM_ITERS ),
					( double ) total_cyc / ( double ) uniq_cyc );
		} else if ( uniq_cyc ) {
			printf( "%s : %7.3f    %7.3f\n", func_name[flag],
					( double ) total_cyc / ( double ) ( NUM_ITERS ),
					( double ) total_cyc / ( double ) uniq_cyc );
		} else {
			printf( "%s : %7.3f   >%7.3f\n", func_name[flag],
					( double ) total_cyc / ( double ) ( NUM_ITERS ),
					( double ) total_cyc );
		}
	}

	free( elapsed_cyc );
}

void
clockcore( void )
{
	/* check PAPI_get_real_cyc */
	clock_res_check( 0 );
	/* check PAPI_get_real_usec */
	clock_res_check( 1 );

	/* check PAPI_get_virt_cyc */
	/* Virtual */
	if ( PAPI_get_virt_cyc(  ) != -1 ) {
		clock_res_check( 2 );
	} else
		test_fail( __FILE__, __LINE__, "PAPI_get_virt_cyc", -1 );

	/* check PAPI_get_virt_usec */
	if ( PAPI_get_virt_usec(  ) != -1 ) {
		clock_res_check( 3 );
	} else
		test_fail( __FILE__, __LINE__, "PAPI_get_virt_usec", -1 );
}
