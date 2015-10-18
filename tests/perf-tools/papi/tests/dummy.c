#include "papi_test.h"

void
dummy( void *array )
{
/* Confuse the compiler so as not to optimize
   away the flops in the calling routine    */
/* Cast the array as a void to eliminate unused argument warning */
	( void ) array;
}

void
dummy_( void *array )
{
	( void ) array;
}

void
dummy__( void *array )
{
	( void ) array;
}

void
DUMMY( void *array )
{
	( void ) array;
}

void
_DUMMY( void *array )
{
	( void ) array;
}

/* We have to actually touch the memory to confuse some
 * systems, so they actually allocate the memory.
 * -KSL
 */
void
touch_dummy( double *array, int size )
{
	int i;
	double *tmp = array;
	for ( i = 0; i < size; i++, tmp++ )
		*tmp = ( double ) rand(  );
}
