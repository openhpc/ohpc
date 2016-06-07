#include "papi_test.h"

#include <unistd.h>

/*  Variable to hold reporting status
	if TRUE, output is suppressed
	if FALSE output is sent to stdout
	initialized to FALSE
	declared here so it can be available globally
*/
int TESTS_QUIET = 0;
static int TESTS_COLOR = 0;
static int TEST_WARN = 0;

/*  Support routine to display header information to the screen
	from the hardware info data structure. The same code was duplicated
	in a number of tests and utilities. Seems to make sense to refactor.
	This may not be the best place for it to live, but it works for now.
 */
int
papi_print_header( char *prompt, const PAPI_hw_info_t ** hwinfo )
{
	int cnt, mpx;
	
	if ( ( *hwinfo = PAPI_get_hardware_info(  ) ) == NULL ) {
   		return PAPI_ESYS;
	}

	printf( "%s", prompt );
	printf
		( "--------------------------------------------------------------------------------\n" );
	printf( "PAPI Version             : %d.%d.%d.%d\n",
			PAPI_VERSION_MAJOR( PAPI_VERSION ),
			PAPI_VERSION_MINOR( PAPI_VERSION ),
			PAPI_VERSION_REVISION( PAPI_VERSION ),
			PAPI_VERSION_INCREMENT( PAPI_VERSION ) );
	printf( "Vendor string and code   : %s (%d)\n", ( *hwinfo )->vendor_string,
			( *hwinfo )->vendor );
	printf( "Model string and code    : %s (%d)\n", ( *hwinfo )->model_string,
			( *hwinfo )->model );
	printf( "CPU Revision             : %f\n", ( *hwinfo )->revision );
	if ( ( *hwinfo )->cpuid_family > 0 )
		printf
			( "CPUID Info               : Family: %d  Model: %d  Stepping: %d\n",
			  ( *hwinfo )->cpuid_family, ( *hwinfo )->cpuid_model,
			  ( *hwinfo )->cpuid_stepping );
	printf( "CPU Max Megahertz        : %d\n", ( *hwinfo )->cpu_max_mhz );
	printf( "CPU Min Megahertz        : %d\n", ( *hwinfo )->cpu_min_mhz );
	if ( ( *hwinfo )->threads > 0 )
		printf( "Hdw Threads per core     : %d\n", ( *hwinfo )->threads );
	if ( ( *hwinfo )->cores > 0 )
		printf( "Cores per Socket         : %d\n", ( *hwinfo )->cores );
	if ( ( *hwinfo )->sockets > 0 )
		printf( "Sockets                  : %d\n", ( *hwinfo )->sockets );
	if ( ( *hwinfo )->nnodes > 0 )
		printf( "NUMA Nodes               : %d\n", ( *hwinfo )->nnodes );
	printf( "CPUs per Node            : %d\n", ( *hwinfo )->ncpu );
	printf( "Total CPUs               : %d\n", ( *hwinfo )->totalcpus );
	printf( "Running in a VM          : %s\n", ( *hwinfo )->virtualized?
		"yes":"no");
	if ( (*hwinfo)->virtualized) {
           printf( "VM Vendor:               : %s\n", (*hwinfo)->virtual_vendor_string);
	}
	cnt = PAPI_get_opt( PAPI_MAX_HWCTRS, NULL );
	mpx = PAPI_get_opt( PAPI_MAX_MPX_CTRS, NULL );
	if ( cnt >= 0 ) {
		printf( "Number Hardware Counters : %d\n",cnt );
	} else {
		printf( "Number Hardware Counters : PAPI error %d: %s\n", cnt, PAPI_strerror(cnt));
	}
	if ( mpx >= 0 ) {
		printf( "Max Multiplex Counters   : %d\n", mpx );
	} else {
		printf( "Max Multiplex Counters   : PAPI error %d: %s\n", mpx, PAPI_strerror(mpx));
	}
	printf
		( "--------------------------------------------------------------------------------\n" );
	printf( "\n" );
	return PAPI_OK;
}


void
validate_string( char *name, char *s )
{
	if ( ( s == NULL ) || ( strlen( s ) == 0 ) ) {
		char s2[1024] = "";
		sprintf( s2, "%s was NULL or length 0", name );
		test_fail( __FILE__, __LINE__, s2, 0 );
	}
}

int
approx_equals( double a, double b )
{
	if ( ( a >= b * ( 1.0 - TOLERANCE ) ) && ( a <= b * ( 1.0 + TOLERANCE ) ) )
		return 1;
	else {
		printf( "Out of tolerance range %2.2f: %.0f vs %.0f [%.0f,%.0f]\n",
				TOLERANCE, a, b, b * ( 1.0 - TOLERANCE ),
				b * ( 1.0 + TOLERANCE ) );
		return 0;
	}
}

long long **
allocate_test_space( int num_tests, int num_events )
{
	long long **values;
	int i;

	values =
		( long long ** ) malloc( ( size_t ) num_tests *
								 sizeof ( long long * ) );
	if ( values == NULL )
		exit( 1 );
	memset( values, 0x0, ( size_t ) num_tests * sizeof ( long long * ) );

	for ( i = 0; i < num_tests; i++ ) {
		values[i] =
			( long long * ) malloc( ( size_t ) num_events *
									sizeof ( long long ) );
		if ( values[i] == NULL )
			exit( 1 );
		memset( values[i], 0x00, ( size_t ) num_events * sizeof ( long long ) );
	}
	return ( values );
}

void
free_test_space( long long **values, int num_tests )
{
	int i;

	for ( i = 0; i < num_tests; i++ )
		free( values[i] );
	free( values );
}



int is_event_derived(unsigned int event) {

  PAPI_event_info_t info;

  if (event & PAPI_PRESET_MASK) {

     PAPI_get_event_info(event,&info);

     if (strcmp(info.derived,"NOT_DERIVED")) {
       //       printf("%#x is derived\n",event);
        return 1;
     }
  }
  return 0;
}


int find_nonderived_event( void )
{
	/* query and set up the right event to monitor */
	PAPI_event_info_t info;
	int potential_evt_to_add[3] = { PAPI_FP_OPS, PAPI_FP_INS, PAPI_TOT_INS };
	int i;

	for ( i = 0; i < 3; i++ ) {
		if ( PAPI_query_event( potential_evt_to_add[i] ) == PAPI_OK ) {
			if ( PAPI_get_event_info( potential_evt_to_add[i], &info ) ==
				 PAPI_OK ) {
				if ( ( info.count > 0 ) &&
					 !strcmp( info.derived, "NOT_DERIVED" ) )
					return ( potential_evt_to_add[i] );
			}
		}
	}
	return ( 0 );
}


/* Add events to an EventSet, as specified by a mask.

   Returns: number = number of events added

*/

//struct test_events_t {
//  unsigned int mask;
//  unsigned int event;
//};

struct test_events_t test_events[MAX_TEST_EVENTS] = {
  { MASK_TOT_CYC, PAPI_TOT_CYC },
  { MASK_TOT_INS, PAPI_TOT_INS },
  { MASK_FP_INS,  PAPI_FP_INS },
  { MASK_L1_TCM,  PAPI_L1_TCM },
  { MASK_L1_ICM,  PAPI_L1_ICM },
  { MASK_L1_DCM,  PAPI_L1_DCM },
  { MASK_L2_TCM,  PAPI_L2_TCM },
  { MASK_L2_TCA,  PAPI_L2_TCA },
  { MASK_L2_TCH,  PAPI_L2_TCH },
  { MASK_BR_CN,   PAPI_BR_CN  },
  { MASK_BR_MSP,  PAPI_BR_MSP },
  { MASK_BR_PRC,  PAPI_BR_PRC },
  { MASK_TOT_IIS, PAPI_TOT_IIS},
  { MASK_L1_DCR,  PAPI_L1_DCR},
  { MASK_L1_DCW,  PAPI_L1_DCW},
  { MASK_L1_DCA,  PAPI_L1_DCA},
  { MASK_FP_OPS,  PAPI_FP_OPS},
};


int
add_test_events( int *number, int *mask, int allow_derived )
{
  int retval,i;
  int EventSet = PAPI_NULL;
  int num_counters = 0;
  char name_string[BUFSIZ];

  *number = 0;

     /* get the number of available HW counters */
  num_counters = PAPI_get_opt( PAPI_MAX_HWCTRS, NULL );
  if ( num_counters < 1 ) {
     test_fail( __FILE__, __LINE__, "Zero HW Counters available", 
			   num_counters );
  }

     /* create the eventset */
  retval = PAPI_create_eventset( &EventSet );
  if ( retval != PAPI_OK ) {
     test_fail( __FILE__, __LINE__, "PAPI_create_eventset", 
			   retval );
  }

     /* check all the masks */
  for(i=0;i<MAX_TEST_EVENTS;i++) {
    
     if ( *mask & test_events[i].mask ) {

           /* remove any derived events if told to */
        if ((is_event_derived(test_events[i].event)) && (!allow_derived)) {
	   *mask = *mask ^ test_events[i].mask;
	   continue;
        }

	retval = PAPI_add_event( EventSet, test_events[i].event );

	if ( retval == PAPI_OK ) {

	   ( *number )++;
#if 0
	   if ((*number)==num_counters) {
	     if ( !TESTS_QUIET) {
	       fprintf(stdout, "Stopping with %d events due to HW limit\n",
		       num_counters);
	     }
	     break;
	   }
#endif
	}
	else {
	   if ( !TESTS_QUIET ) {
	     PAPI_event_code_to_name(test_events[i].event,name_string);
	     fprintf( stdout, "%#x %s is not available.\n", 
		      test_events[i].event,name_string);
	   }
	   *mask = *mask ^ test_events[i].mask;
	}
     }
  }

  return EventSet;
}

int
remove_test_events( int *EventSet, int mask )
{
	int retval = PAPI_OK;

	if ( mask & MASK_L1_DCA ) {
		retval = PAPI_remove_event( *EventSet, PAPI_L1_DCA );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_L1_DCW ) {
		retval = PAPI_remove_event( *EventSet, PAPI_L1_DCW );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_L1_DCR ) {
		retval = PAPI_remove_event( *EventSet, PAPI_L1_DCR );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_L2_TCH ) {
		retval = PAPI_remove_event( *EventSet, PAPI_L2_TCH );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_L2_TCA ) {
		retval = PAPI_remove_event( *EventSet, PAPI_L2_TCA );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_L2_TCM ) {
		retval = PAPI_remove_event( *EventSet, PAPI_L2_TCM );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_L1_DCM ) {
		retval = PAPI_remove_event( *EventSet, PAPI_L1_DCM );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_L1_ICM ) {
		retval = PAPI_remove_event( *EventSet, PAPI_L1_ICM );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_L1_TCM ) {
		retval = PAPI_remove_event( *EventSet, PAPI_L1_TCM );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_FP_OPS ) {
		retval = PAPI_remove_event( *EventSet, PAPI_FP_OPS );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_FP_INS ) {
		retval = PAPI_remove_event( *EventSet, PAPI_FP_INS );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_TOT_INS ) {
		retval = PAPI_remove_event( *EventSet, PAPI_TOT_INS );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_TOT_IIS ) {
		retval = PAPI_remove_event( *EventSet, PAPI_TOT_IIS );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	if ( mask & MASK_TOT_CYC ) {
		retval = PAPI_remove_event( *EventSet, PAPI_TOT_CYC );
		if ( retval < PAPI_OK )
			return ( retval );
	}

	return ( PAPI_destroy_eventset( EventSet ) );
}

char *
stringify_all_domains( int domains )
{
	static char buf[PAPI_HUGE_STR_LEN];
	int i, did = 0;
	buf[0] = '\0';

	for ( i = PAPI_DOM_MIN; i <= PAPI_DOM_MAX; i = i << 1 )
		if ( domains & i ) {
			if ( did )
				strcpy( buf + strlen( buf ), "|" );
			strcpy( buf + strlen( buf ), stringify_domain( domains & i ) );
			did++;
		}
	if ( did == 0 )
		test_fail( __FILE__, __LINE__, "Unrecognized domains!", 0 );
	return ( buf );
}

char *
stringify_domain( int domain )
{
	switch ( domain ) {
	case PAPI_DOM_SUPERVISOR:
		return ( "PAPI_DOM_SUPERVISOR" );
	case PAPI_DOM_USER:
		return ( "PAPI_DOM_USER" );
	case PAPI_DOM_KERNEL:
		return ( "PAPI_DOM_KERNEL" );
	case PAPI_DOM_OTHER:
		return ( "PAPI_DOM_OTHER" );
	case PAPI_DOM_ALL:
		return ( "PAPI_DOM_ALL" );
	default:
		test_fail( __FILE__, __LINE__, "Unrecognized domains!", 0 );
	}
	return ( NULL );
}

char *
stringify_all_granularities( int granularities )
{
	static char buf[PAPI_HUGE_STR_LEN];
	int i, did = 0;

	buf[0] = '\0';
	for ( i = PAPI_GRN_MIN; i <= PAPI_GRN_MAX; i = i << 1 )
		if ( granularities & i ) {
			if ( did )
				strcpy( buf + strlen( buf ), "|" );
			strcpy( buf + strlen( buf ),
					stringify_granularity( granularities & i ) );
			did++;
		}
	if ( did == 0 )
		test_fail( __FILE__, __LINE__, "Unrecognized granularity!", 0 );

	return ( buf );
}

char *
stringify_granularity( int granularity )
{
	switch ( granularity ) {
	case PAPI_GRN_THR:
		return ( "PAPI_GRN_THR" );
	case PAPI_GRN_PROC:
		return ( "PAPI_GRN_PROC" );
	case PAPI_GRN_PROCG:
		return ( "PAPI_GRN_PROCG" );
	case PAPI_GRN_SYS_CPU:
		return ( "PAPI_GRN_SYS_CPU" );
	case PAPI_GRN_SYS:
		return ( "PAPI_GRN_SYS" );
	default:
		test_fail( __FILE__, __LINE__, "Unrecognized granularity!", 0 );
	}
	return ( NULL );
}

void
tests_quiet( int argc, char **argv )
{
	if ( ( argc > 1 )
		 && ( ( strcasecmp( argv[1], "TESTS_QUIET" ) == 0 )
			  || ( strcasecmp( argv[1], "-q" ) == 0 ) ) ) {
		TESTS_QUIET = 1;
	} else {
		int retval;

		retval = PAPI_set_debug( PAPI_VERB_ECONT );
		if ( retval != PAPI_OK )
			test_fail( __FILE__, __LINE__, "PAPI_set_debug", retval );
	}
   if (getenv("TESTS_COLOR")!=NULL) {
      TESTS_COLOR=1;
   }
   
}

#define RED    "\033[1;31m"
#define YELLOW "\033[1;33m"
#define GREEN  "\033[1;32m"
#define NORMAL "\033[0m"

void
test_pass( char *file, long long **values, int num_tests )
{
  int line_pad;

  line_pad=(int)(50-strlen(file));
  if (line_pad<0) line_pad=0;

        if ( TEST_WARN ) {
	  if (TESTS_COLOR) {
	    fprintf( stdout, "%-*s %sPASSED with WARNING%s\n", 
		     line_pad, file, YELLOW, NORMAL);	   
	  }
	  else {
	        fprintf( stdout, "%-*s PASSED with WARNING\n", 
			 line_pad, file );	   
	  }
	}
        else {
	  if (TESTS_COLOR) {
	    fprintf( stdout, "%-*s %sPASSED%s\n", line_pad, file, 
		     GREEN, NORMAL );
	  }
	  else {
	    fprintf( stdout, "%-*s PASSED\n", line_pad, file );
	  }
	}
   
	if ( values )
		free_test_space( values, num_tests );

	if ( PAPI_is_initialized(  ) )
		PAPI_shutdown(  );

	exit( 0 );

}

/* Use a positive value of retval to simply print an error message */
void
test_fail( char *file, int line, char *call, int retval )
{

  int line_pad;
  char buf[128];

  line_pad=(int)(50-strlen(file));
  if (line_pad<0) line_pad=0;

  memset( buf, '\0', sizeof ( buf ) );

  if (TESTS_COLOR) {
     fprintf( stdout, "%-*s %sFAILED%s\nLine # %d\n", line_pad, file, 
		    RED,NORMAL,line );
  }
  else {
     fprintf( stdout, "%-*s FAILED\nLine # %d\n", line_pad, file, line );
  }

  if ( retval == PAPI_ESYS ) {
     sprintf( buf, "System error in %s", call );
     perror( buf );
  } else if ( retval > 0 ) {
     fprintf( stdout, "Error: %s\n", call );
  } else if ( retval == 0 ) {
#if defined(sgi)
		fprintf( stdout, "SGI requires root permissions for this test\n" );
#else
		fprintf( stdout, "Error: %s\n", call );
#endif
  } else {
     fprintf( stdout, "Error in %s: %s\n", call, PAPI_strerror( retval ) );
  }

  fprintf( stdout, "\n" );

	/* NOTE: Because test_fail is called from thread functions, 
	   calling PAPI_shutdown here could prevent some threads
	   from being able to free memory they have allocated.
	 */

	/* This is stupid.  Threads are the rare case */
	/* and in any case an exit() should clear everything out */
	/* adding back the exit() call */

	exit(1);
}

/* This routine mimics the previous implementation of test_fail()
	by exiting on completion. It caused problems for threaded apps.
	If you are not threaded and want to exit, replace calls to 
	test_fail() with calls to test_fail_exit().
*/
void
test_fail_exit( char *file, int line, char *call, int retval )
{
	test_fail( file, line, call, retval );
	if ( PAPI_is_initialized(  ) )
		PAPI_shutdown(  );
	exit( 1 );
}


/* Use a positive value of retval to simply print an error message */
void
test_warn( char *file, int line, char *call, int retval )
{

  int line_pad;

  line_pad=(int)(50-strlen(file));
  if (line_pad<0) line_pad=0;

	char buf[128];
	memset( buf, '\0', sizeof ( buf ) );

	if (TESTS_COLOR) {
	  fprintf( stdout, "%-*s %sWARNING%s\nLine # %d\n", line_pad, file, 
		   YELLOW, NORMAL, line );
	}
	else {
	  fprintf( stdout, "%-*s WARNING\nLine # %d\n", line_pad, file, line );
	}

	if ( retval == PAPI_ESYS ) {
		sprintf( buf, "System warning in %s", call );
		perror( buf );
	} else if ( retval > 0 ) {
		fprintf( stdout, "Warning: %s\n", call );
	} else if ( retval == 0 ) {
		fprintf( stdout, "Warning: %s\n", call );
	} else {
		fprintf( stdout, "Warning in %s: %s\n", call, PAPI_strerror( retval ));
	}

	fprintf( stdout, "\n" );
	TEST_WARN++;

}

void
test_skip( char *file, int line, char *call, int retval )
{
	char buf[128];

	memset( buf, '\0', sizeof ( buf ) );
	fprintf( stdout, "%-40s SKIPPED\n", file );
	if ( !TESTS_QUIET ) {
		if ( retval == PAPI_ESYS ) {
			fprintf( stdout, "Line # %d\n", line );
			sprintf( buf, "System error in %s:", call );
			perror( buf );
		} else if ( retval == PAPI_EPERM ) {
			fprintf( stdout, "Line # %d\n", line );
			fprintf( stdout, "Invalid permissions for %s.", call );
		} else if ( retval == PAPI_ECMP ) {
			fprintf( stdout, "Line # %d\n", line );
			fprintf( stdout, "%s.", call );
		} else if ( retval >= 0 ) {
			fprintf( stdout, "Line # %d\n", line );
			fprintf( stdout, "Error calculating: %s\n", call );
		} else if ( retval < 0 ) {
			fprintf( stdout, "Line # %d\n", line );
			fprintf( stdout, "Error in %s: %s\n", call, PAPI_strerror(retval) );
		}
		fprintf( stdout, "\n" );
	}
	exit( 0 );
}


void
test_print_event_header( char *call, int evset )
{
        int *ev_ids;
	int i, nev;
	int retval;
	char evname[PAPI_MAX_STR_LEN];

	if ( *call )
		fprintf( stdout, "%s", call );

	if ((nev = PAPI_get_cmp_opt(PAPI_MAX_MPX_CTRS,NULL,0)) <= 0) {
		fprintf( stdout, "Can not list event names.\n" );
		return;
	}

	if ((ev_ids = calloc(nev,sizeof(int))) == NULL) {
		fprintf( stdout, "Can not list event names.\n" );
		return;
	}

	retval = PAPI_list_events( evset, ev_ids, &nev );

	if ( retval == PAPI_OK ) {
		for ( i = 0; i < nev; i++ ) {
			PAPI_event_code_to_name( ev_ids[i], evname );
			printf( ONEHDR, evname );
		}
	} else {
		fprintf( stdout, "Can not list event names." );
	}
	fprintf( stdout, "\n" );
	free(ev_ids);
}

int
add_two_events( int *num_events, int *papi_event, int *mask ) {

     /* query and set up the right event to monitor */
  int EventSet = PAPI_NULL;
  PAPI_event_info_t info;
  unsigned int potential_evt_to_add[3][2] =
    { {( unsigned int ) PAPI_FP_INS, MASK_FP_INS},
      {( unsigned int ) PAPI_FP_OPS, MASK_FP_OPS},
      {( unsigned int ) PAPI_TOT_INS, MASK_TOT_INS}
    };
  int i = 0;
  int counters = 0;

  *mask = 0;
  counters = PAPI_num_hwctrs(  );

  if (counters<=0) {
     test_fail(__FILE__,__LINE__,"Zero Counters Available!  PAPI Won't like this!\n",0);
  }

  /* This code tries to ensure that the event  generated will fit in the */
  /* number of available counters. It has the potential to leak up to    */
  /* two event sets if events fail to add successfully.                  */

  for(i=0;i<3;i++) {
	if ( PAPI_query_event( (int) potential_evt_to_add[i][0] ) == PAPI_OK ) {
		if ( PAPI_get_event_info( (int) potential_evt_to_add[i][0], &info ) == PAPI_OK ) {
			if ( ( info.count > 0 ) && ( (unsigned) counters > info.count ) ) {
				*papi_event = ( int ) potential_evt_to_add[i][0];
				*mask = ( int ) potential_evt_to_add[i][1] | MASK_TOT_CYC;
				EventSet = add_test_events( num_events, mask, 1 );
				if ( *num_events == 2 ) break;
			}
		}
	}
  }
  if ( i == 3 ) {
     test_fail( __FILE__, __LINE__, "Not enough room to add an event!", 0 );
  }
  return EventSet;
}

int
add_two_nonderived_events( int *num_events, int *papi_event, int *mask ) {

	/* query and set up the right event to monitor */
  int EventSet = PAPI_NULL;

#define POTENTIAL_EVENTS 3

  unsigned int potential_evt_to_add[POTENTIAL_EVENTS][2] =
		{ {( unsigned int ) PAPI_FP_INS, MASK_FP_INS},
		  {( unsigned int ) PAPI_FP_OPS, MASK_FP_OPS},
		  {( unsigned int ) PAPI_TOT_INS, MASK_TOT_INS}
		};

  int i;

  *mask = 0;
  
   /* could leak up to two event sets. */
  for(i=0;i<POTENTIAL_EVENTS;i++) {

     if ( PAPI_query_event( ( int ) potential_evt_to_add[i][0] ) == PAPI_OK ) {
       if ( !is_event_derived(potential_evt_to_add[i][0])) {
		 *papi_event = ( int ) potential_evt_to_add[i][0];
		 *mask = ( int ) potential_evt_to_add[i][1] | MASK_TOT_CYC;
		 EventSet = add_test_events( num_events, mask, 0 );
		 if ( *num_events == 2 ) break;
       }
    }
  }
	
  if ( i == POTENTIAL_EVENTS ) {
     test_fail( __FILE__, __LINE__, "Can't find a non-derived event!", 0 );
  }
  return EventSet;
}

/* add native events to use all counters */
int
enum_add_native_events( int *num_events, int **evtcodes, 
			int need_interrupt, int no_software_events,
			int cidx)
{
	/* query and set up the right event to monitor */
     int EventSet = PAPI_NULL;
     int i = 0, k, event_code, retval;
     int counters, event_found = 0;
     PAPI_event_info_t info;
     const PAPI_component_info_t *s = NULL;
     const PAPI_hw_info_t *hw_info = NULL;
   
     s = PAPI_get_component_info( cidx );
     if ( s == NULL ) {
	test_fail( __FILE__, __LINE__, 
			   "PAPI_get_component_info", PAPI_ECMP );
     }

     hw_info = PAPI_get_hardware_info(  );
     if ( hw_info == NULL ) {
        test_fail( __FILE__, __LINE__, "PAPI_get_hardware_info", 2 );
     }
   
     counters = PAPI_num_hwctrs(  );
     if (counters<1) {
	test_fail(__FILE__,__LINE__, "No counters available!\n",1);
     }

     if (!TESTS_QUIET) printf("Trying to fill %d hardware counters...\n",
			      counters);
   
     if (need_interrupt) {
        if ( (!strcmp(hw_info->model_string,"POWER6")) ||
	     (!strcmp(hw_info->model_string,"POWER5")) ) {
	   
	   test_warn(__FILE__, __LINE__,
		    "Limiting num_counters because of LIMITED_PMC on Power5 and Power6",1);
           counters=4;
	}
     }

     ( *evtcodes ) = ( int * ) calloc( counters, sizeof ( int ) );

     retval = PAPI_create_eventset( &EventSet );
     if ( retval != PAPI_OK ) {
	test_fail( __FILE__, __LINE__, "PAPI_create_eventset", retval );
     }

     /* For platform independence, always ASK FOR the first event */
     /* Don't just assume it'll be the first numeric value */
     i = 0 | PAPI_NATIVE_MASK;
     retval = PAPI_enum_cmp_event( &i, PAPI_ENUM_FIRST, cidx );
     if ( retval != PAPI_OK )
     {
	 test_fail( __FILE__, __LINE__, "PAPI_enum_cmp_event", retval );
     }

     do {
        retval = PAPI_get_event_info( i, &info );

	/* HACK! FIXME */
        if (no_software_events && ( strstr(info.symbol,"PERF_COUNT_SW") || strstr(info.long_descr, "PERF_COUNT_SW") ) ) {
	   if (!TESTS_QUIET) {
	      printf("Blocking event %s as a SW event\n", info.symbol);
	   }
	   continue;
	}

	if ( s->cntr_umasks ) {
	   k = i;
			
	   if ( PAPI_enum_cmp_event( &k, PAPI_NTV_ENUM_UMASKS, cidx ) == PAPI_OK ) {
	      do {
	         retval = PAPI_get_event_info( k, &info );
		 event_code = ( int ) info.event_code;

		 retval = PAPI_add_event( EventSet, event_code );
		 if ( retval == PAPI_OK ) {
		    ( *evtcodes )[event_found] = event_code;
		    if ( !TESTS_QUIET ) {
		       printf( "event_code[%d] = %#x (%s)\n",
			       event_found, event_code, info.symbol );
		    }
		    event_found++;
		 } else {
		    if ( !TESTS_QUIET ) {
		       printf( "%#x (%s) can't be added to the EventSet.\n",
			       event_code, info.symbol );
		    }
		 }
	      } while ( PAPI_enum_cmp_event( &k, PAPI_NTV_ENUM_UMASKS, cidx ) == PAPI_OK
						&& event_found < counters );
	   } else {
	      event_code = ( int ) info.event_code;
	      retval = PAPI_add_event( EventSet, event_code );
	      if ( retval == PAPI_OK ) {
		  ( *evtcodes )[event_found] = event_code;
		  if ( !TESTS_QUIET ) {
		     printf( "event_code[%d] = %#x (%s)\n",
			       event_found, event_code, info.symbol );
		  }
		  event_found++;
	      }
	   }
	   if ( !TESTS_QUIET && retval == PAPI_OK ) {
	     /* */
	   }
	} else {
			event_code = ( int ) info.event_code;
			retval = PAPI_add_event( EventSet, event_code );
			if ( retval == PAPI_OK ) {
				( *evtcodes )[event_found] = event_code;
				event_found++;
			} else {
				if ( !TESTS_QUIET )
					fprintf( stdout, "%#x is not available.\n", event_code );
			}
		}
	}
     while ( PAPI_enum_cmp_event( &i, PAPI_ENUM_EVENTS, cidx ) == PAPI_OK &&
			event_found < counters );

	*num_events = ( int ) event_found;

	if (!TESTS_QUIET) printf("Tried to fill %d counters with events, "
				 "found %d\n",counters,event_found);

	return EventSet;
}

void
init_multiplex( void )
{
	int retval;
	const PAPI_hw_info_t *hw_info;
	const PAPI_component_info_t *cmpinfo;

	/* Initialize the library */

	/* for now, assume multiplexing on CPU compnent only */
	cmpinfo = PAPI_get_component_info( 0 );
	if ( cmpinfo == NULL )
		test_fail( __FILE__, __LINE__, "PAPI_get_component_info", 2 );

	hw_info = PAPI_get_hardware_info(  );
	if ( hw_info == NULL )
		test_fail( __FILE__, __LINE__, "PAPI_get_hardware_info", 2 );

	if ( ( strstr( cmpinfo->name, "perfctr.c" ) ) && (hw_info !=NULL) &&
		 strcmp( hw_info->model_string, "POWER6" ) == 0 ) {
		retval = PAPI_set_domain( PAPI_DOM_ALL );
		if ( retval != PAPI_OK )
			test_fail( __FILE__, __LINE__, "PAPI_set_domain", retval );
	}
	retval = PAPI_multiplex_init(  );
	if ( retval != PAPI_OK )
		test_fail( __FILE__, __LINE__, "PAPI multiplex init fail\n", retval );
}

