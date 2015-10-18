#include <stdlib.h>
#include <string.h>
#include <stdio.h>

int main()
{
	long long int megs = 0;
        while( 1 )
        {
                void *m = malloc( 1024 * 1024 );
                memset( m, 0, 1024 * 1024 );
		printf( "Alloc and touch %15d mb\n", ++megs );
        }
        return 0;
}
