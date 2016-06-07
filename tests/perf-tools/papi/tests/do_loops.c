/* Compile me with -O0 or else you'll get none. */

#include "papi_test.h"

volatile int buf[CACHE_FLUSH_BUFFER_SIZE_INTS];
volatile int buf_dummy = 0;
volatile int *flush = NULL;
volatile int flush_dummy = 0;
volatile double a = 0.5, b = 2.2;

void
do_reads( int n )
{
	int i, retval;
	static int fd = -1;
	char buf;

	if ( fd == -1 ) {
		fd = open( "/dev/zero", O_RDONLY );
		if ( fd == -1 ) {
			perror( "open(/dev/zero)" );
			exit( 1 );
		}
	}

	for ( i = 0; i < n; i++ ) {
		retval = ( int ) read( fd, &buf, sizeof ( buf ) );
		if ( retval != sizeof ( buf ) ) {
			if ( retval < 0 )
				perror( "/dev/zero cannot be read" );
			else
				fprintf( stderr,
						 "/dev/zero cannot be read: only got %d bytes.\n",
						 retval );
			exit( 1 );
		}
	}
}

void
fdo_reads( int *n )
{
	do_reads( *n );
}

void
fdo_reads_( int *n )
{
	do_reads( *n );
}

void
fdo_reads__( int *n )
{
	do_reads( *n );
}

void
FDO_READS( int *n )
{
	do_reads( *n );
}

void
_FDO_READS( int *n )
{
	do_reads( *n );
}

void
do_flops( int n )
{
	int i;
	double c = 0.11;

	for ( i = 0; i < n; i++ ) {
		c += a * b;
	}
	dummy( ( void * ) &c );
}

void
fdo_flops( int *n )
{
	do_flops( *n );
}

void
fdo_flops_( int *n )
{
	do_flops( *n );
}

void
fdo_flops__( int *n )
{
	do_flops( *n );
}

void
FDO_FLOPS( int *n )
{
	do_flops( *n );
}

void
_FDO_FLOPS( int *n )
{
	do_flops( *n );
}

void
do_misses( int n, int bytes )
{
	register int i, j, tmp = buf_dummy, len = bytes / ( int ) sizeof ( int );
	dummy( ( void * ) buf );
	dummy( ( void * ) &buf_dummy );
	assert( len <= CACHE_FLUSH_BUFFER_SIZE_INTS );
	for ( j = 0; j < n; j++ ) {
		for ( i = 0; i < len; i++ ) {
			/* We need to read, modify, write here to look
			   out for the write allocate policies. */
			buf[i] += tmp;
			/* Fake out some naive prefetchers */
			buf[len - 1 - i] -= tmp;
		}
		tmp += len;
	}
	buf_dummy = tmp;
	dummy( ( void * ) buf );
	dummy( ( void * ) &buf_dummy );
}

void
fdo_misses( int *n, int *size )
{
	do_misses( *n, *size );
}

void
fdo_misses_( int *n, int *size )
{
	do_misses( *n, *size );
}

void
fdo_misses__( int *n, int *size )
{
	do_misses( *n, *size );
}

void
FDO_MISSES( int *n, int *size )
{
	do_misses( *n, *size );
}

void
_FDO_MISSES( int *n, int *size )
{
	do_misses( *n, *size );
}

void
do_flush( void )
{
	register int i;
	if ( flush == NULL )
		flush = ( int * ) malloc( ( 1024 * 1024 * 16 ) * sizeof ( int ) );
	if ( !flush )
		return;

	dummy( ( void * ) flush );
	for ( i = 0; i < ( 1024 * 1024 * 16 ); i++ ) {
		flush[i] += flush_dummy;
	}
	flush_dummy++;
	dummy( ( void * ) flush );
	dummy( ( void * ) &flush_dummy );
}

void
fdo_flush( void )
{
	do_flush(  );
}

void
fdo_flush_( void )
{
	do_flush(  );
}

void
fdo_flush__( void )
{
	do_flush(  );
}

void
FDO_FLUSH( void )
{
	do_flush(  );
}

void
_FDO_FLUSH( void )
{
	do_flush(  );
}

void
do_l1misses( int n )
{
	do_misses( n, L1_MISS_BUFFER_SIZE_INTS );
}

void
fdo_l1misses( int *n )
{
	do_l1misses( *n );
}

void
fdo_l1misses_( int *n )
{
	do_l1misses( *n );
}

void
fdo_l1misses__( int *n )
{
	do_l1misses( *n );
}

void
FDO_L1MISSES( int *n )
{
	do_l1misses( *n );
}

void
_FDO_L1MISSES( int *n )
{
	do_l1misses( *n );
}

void
do_stuff( void )
{
	static int loops = 0;

	if ( loops == 0 ) {
		struct timeval now, then;
		gettimeofday( &then, NULL );
		do {
			do_flops( NUM_FLOPS );
			do_reads( NUM_READS );
			do_misses( 1, 1024 * 1024 );
			gettimeofday( &now, NULL );
			loops++;
		} while ( now.tv_sec - then.tv_sec < NUM_WORK_SECONDS );
	} else {
		int i = 0;
		do {
			do_flops( NUM_FLOPS );
			do_reads( NUM_READS );
			do_misses( 1, 1024 * 1024 );
			i++;
		} while ( i < loops );
	}
}

void
do_stuff_( void )
{
	do_stuff(  );
}

void
do_stuff__( void )
{
	do_stuff(  );
}

void
DO_STUFF( void )
{
	do_stuff(  );
}

void
_DO_STUFF( void )
{
	do_stuff(  );
}
