/* SPDX-License-Identifier: BSD-3-Clause */
/* Simple PAPI smoke test from upstream */

#include <papi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define NUM_EVENTS 2

int main(int argc, char **argv)
{
	int retval, i;
	long long values[NUM_EVENTS];
	int EventSet = PAPI_NULL;
	int events[NUM_EVENTS];
	char *EventName[] = { "PAPI_TOT_CYC", "PAPI_TOT_INS" };

	retval = PAPI_library_init(PAPI_VER_CURRENT);
	if (retval != PAPI_VER_CURRENT) {
		printf("ERROR: PAPI_library_init: %d: %s\n", retval,
		       PAPI_strerror(retval));
		exit(EXIT_FAILURE);
	} else {
		printf("PAPI_VERSION     : %4d %6d %7d\n",
		       PAPI_VERSION_MAJOR(PAPI_VERSION),
		       PAPI_VERSION_MINOR(PAPI_VERSION),
		       PAPI_VERSION_REVISION(PAPI_VERSION));
	}

	retval = PAPI_create_eventset(&EventSet);
	if (retval != PAPI_OK) {
		printf("ERROR: PAPI_create_eventset: %d: %s\n", retval,
		       PAPI_strerror(retval));
		exit(EXIT_FAILURE);
	}

	for (i = 0; i < NUM_EVENTS; i++) {
		retval = PAPI_event_name_to_code(EventName[i], &events[i]);
		if (retval != PAPI_OK) {
			printf("ERROR: PAPI_event_name_to_code: %d: %s\n",
			       retval, PAPI_strerror(retval));
			exit(EXIT_FAILURE);
		}
	}

	retval = PAPI_add_events(EventSet, events, NUM_EVENTS);
	if (retval != PAPI_OK) {
		printf("ERROR: PAPI_add_events: %d: %s\n", retval,
		       PAPI_strerror(retval));
		exit(EXIT_FAILURE);
	}

	retval = PAPI_start(EventSet);
	if (retval != PAPI_OK) {
		printf("ERROR: PAPI_start: %d: %s\n", retval,
		       PAPI_strerror(retval));
		exit(EXIT_FAILURE);
	}

	/* do work */
	sleep(1);

	retval = PAPI_stop(EventSet, values);
	if (retval != PAPI_OK) {
		printf("ERROR: PAPI_stop: %d: %s\n", retval,
		       PAPI_strerror(retval));
		exit(EXIT_FAILURE);
	}

	for (i = 0; i < NUM_EVENTS; i++) {
		printf("%12lld \t\t --> %s\n", values[i], EventName[i]);
	}

	PAPI_shutdown();
	return EXIT_SUCCESS;
}
