#include <pthread.h>
#include "papi_test.h"


void *
pthread_main( void *arg )
{
	( void ) arg;
	int retval = PAPI_register_thread(  );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_register_thread", retval );
	}

	clockcore(  );

	retval = PAPI_unregister_thread(  );
	if ( retval != PAPI_OK ) {
	   test_fail( __FILE__, __LINE__, "PAPI_unregister_thread", retval );
	}
	return NULL;
}

int
main( int argc, char **argv )
{
    pthread_t t1, t2, t3, t4;
    pthread_attr_t attr;
    int retval;

    /* Set TESTS_QUIET variable */
    tests_quiet( argc, argv );	

    if (( retval = PAPI_library_init( PAPI_VER_CURRENT)) != PAPI_VER_CURRENT) {
       test_fail( __FILE__, __LINE__, "PAPI_library_init", retval );
    }
	
    retval = PAPI_thread_init( ( unsigned long ( * )(void) ) (pthread_self) );
    if ( retval != PAPI_OK ) {
       if ( retval == PAPI_ECMP ) {
	  test_skip( __FILE__, __LINE__, "PAPI_thread_init", retval );
       }
       else {
	  test_fail( __FILE__, __LINE__, "PAPI_thread_init", retval );
       }
    }

    if ( !TESTS_QUIET ) {
       printf( "Test case: Clock latency and resolution.\n" );
       printf( "Note: Virtual timers are proportional to # CPUs.\n" );
       printf( "------------------------------------------------\n" );
    }

    pthread_attr_init( &attr );

#ifdef PTHREAD_CREATE_UNDETACHED
    pthread_attr_setdetachstate( &attr, PTHREAD_CREATE_UNDETACHED );
#endif

#ifdef PTHREAD_SCOPE_SYSTEM
    retval = pthread_attr_setscope( &attr, PTHREAD_SCOPE_SYSTEM );
    if ( retval != 0 ) {
       test_skip( __FILE__, __LINE__, "pthread_attr_setscope", retval );
    }
#endif

    if (pthread_create( &t1, &attr, pthread_main, NULL )) {
       test_fail(__FILE__, __LINE__, "cannot create thread", retval);
    }
	
    if (pthread_create( &t2, &attr, pthread_main, NULL )) {
       test_fail(__FILE__, __LINE__, "cannot create thread", retval);
    }
	
    if (pthread_create( &t3, &attr, pthread_main, NULL )) {
       test_fail(__FILE__, __LINE__, "cannot create thread", retval);
    }

    if (pthread_create( &t4, &attr, pthread_main, NULL )) {
       test_fail(__FILE__, __LINE__, "cannot create thread", retval);
    }

    pthread_main( NULL );

    pthread_join( t1, NULL );
    pthread_join( t2, NULL );
    pthread_join( t3, NULL );
    pthread_join( t4, NULL );

    test_pass( __FILE__, NULL, 0 );
    exit( 0 );
}
