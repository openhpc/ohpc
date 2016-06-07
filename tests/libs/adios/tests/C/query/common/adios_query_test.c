/*
 * adios_query_test.c
 *
 *  Created on: Oct 2, 2014
 *      Author: Houjun Tang
 */

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <assert.h>
#include "adios_selection.h"
#include "adios_query.h"
#include "adios_error.h"
#include <mxml.h>
#include <sys/stat.h>
#include "adios_query_xml_parse.h"

void printRids(const ADIOS_SELECTION_POINTS_STRUCT * pts,  uint64_t *deststart, uint64_t *destcount) {
    uint64_t i = 0, rid=0;
    if (pts->ndim == 3) {
        for (i = 0; i < pts->npoints; i++) {
            rid =  (pts->points[i * 3 + 2] - deststart[2]) + (pts->points[i * 3 + 1] - deststart[1])  * destcount[2] +  (pts->points[i * 3] - deststart[0]) * destcount[2] * destcount[1];
            fprintf(stdout,"[ %"PRIu64" ] ,", rid);
        }
    }

    if (pts->ndim == 2) {
        for (i = 0; i < pts->npoints; i++) {
            rid =  (pts->points[i * 2 + 1] - deststart[1]) + (pts->points[i * 2 ] - deststart[0])  * destcount[1];
            fprintf(stdout,"[ %"PRIu64" ] ,", rid);
        }
    }
    fprintf(stdout,"\n");
}

void printPoints(const ADIOS_SELECTION_POINTS_STRUCT * pts, const int timestep) {
    uint64_t i = 0;
    int j;
    for (i = 0; i < pts->npoints; i++) {
        // first print timestep
        fprintf(stdout,"%d", timestep);

        for (j = 0; j < pts->ndim; j++) {
            fprintf(stdout," %"PRIu64"", pts->points[i * pts->ndim + j]);
        }
        printf("\n");

    }
}

int performQuery(ADIOS_QUERY_TEST_INFO *queryInfo, ADIOS_FILE *f, int use_streaming, int print_points, int read_results)
{
    int i = 0, timestep = 0 ;
    ADIOS_VARINFO * tempVar = adios_inq_var(f, queryInfo->varName);

    if (use_streaming)
    	for (timestep = 0; timestep < queryInfo->fromStep; ++timestep)
    		assert(adios_advance_step(f, 0, 0) == 0);

    fprintf(stderr,"times steps for variable is: [%d, %d], batch size is %llu\n", queryInfo->fromStep, queryInfo->fromStep + queryInfo->numSteps, queryInfo->batchSize);
    for (timestep = queryInfo->fromStep; timestep < queryInfo->fromStep + queryInfo->numSteps; timestep ++) {
        fprintf(stderr, "querying on timestep %d \n", timestep);

        ADIOS_SELECTION* currBatch = NULL;

        while (adios_query_evaluate(queryInfo->query, queryInfo->outputSelection, use_streaming ? 0 : timestep, queryInfo->batchSize, &currBatch) >= 0) {
        	if (currBatch == NULL) {
        		break;
        	}
        	assert(currBatch->type ==ADIOS_SELECTION_POINTS);
        	const ADIOS_SELECTION_POINTS_STRUCT * retrievedPts = &(currBatch->u.points);
        	/* fprintf(stderr,"retrieved points %" PRIu64 " \n", retrievedPts->npoints); */

        	if (print_points) {
        		printPoints(retrievedPts, timestep);
        	}

        	if (read_results) {
        		int elmSize = adios_type_size(tempVar->type, NULL);
        		void *data = malloc(retrievedPts->npoints * elmSize);

        		// read returned temp data
        		adios_schedule_read (f, currBatch, queryInfo->varName, use_streaming ? 0 : timestep, 1, data);
        		adios_perform_reads(f, 1);

        		free(data);
        	}

        	fprintf(stderr,"Total data retrieved:%"PRIu64"\n", retrievedPts->npoints);
        	/* if (tempVar->type == adios_double) { */
        	/*     for (i = 0; i < retrievedPts->npoints; i++) { */
        	/*         fprintf(stderr,"%.6f\t", ((double*)data)[i]); */
        	/*     } */
        	/*     fprintf(stderr,"\n"); */
        	/* } */
        	/* else if (tempVar->type == adios_real) { */
        	/*     for (i = 0; i < retrievedPts->npoints; i++) { */
        	/*         fprintf(stderr,"%.6f\t", ((float*)data)[i]); */
        	/*     } */
        	/*     fprintf(stderr,"\n"); */
        	/* } */

        	adios_selection_delete(currBatch);
        	currBatch = NULL;
        }

        if (use_streaming) {
        	const int err = adios_advance_step(f, 0, 0);
        	if (timestep < queryInfo->fromStep + queryInfo->numSteps - 1) {
        		assert(err == 0);
        	} else {
        		assert(err == err_end_of_stream || err == err_step_notready);
        	}
        }
    }

    adios_query_free(queryInfo->query);
}

int main(int argc, char ** argv) {

    int i, j, datasize, if_any;
    char xmlFileName[256];
    enum ADIOS_READ_METHOD method = ADIOS_READ_METHOD_BP;

    MPI_Comm comm = MPI_COMM_WORLD;

    ADIOS_QUERY_TEST_INFO *queryInfo;
    ADIOS_FILE *f;


    MPI_Init(&argc, &argv);

    if (argc < 4 || argc > 7) {
        fprintf(stderr," usage: %s {input bp file} {xml file} {query engine (ALACRITY/FASTBIT)} [mode (FILE/stream)] [print points? (TRUE/false)] [read results? (true/FALSE)]\n", argv[0]);
        MPI_Abort(comm, 1);
    }
    else {
        strcpy(xmlFileName,  argv[2]);
    }

    enum ADIOS_QUERY_METHOD query_method = ADIOS_QUERY_METHOD_UNKNOWN;
    if (strcasecmp(argv[3], "ALACRITY") == 0) {
        // init with ALACRITY
        //adios_query_init(ADIOS_QUERY_TOOL_ALACRITY);
    	query_method = ADIOS_QUERY_METHOD_ALACRITY;
    }
    else if (strcasecmp(argv[3], "FASTBIT") == 0) {
        // init with FastBit
    	query_method = ADIOS_QUERY_METHOD_FASTBIT;
    	//fprintf(stderr,"FastBit not supported in this test yet, exiting...\n");
    	//MPI_Abort(comm, 1);
    }
    else {
    	fprintf(stderr,"Unsupported query engine %s, exiting...\n", argv[3]);
        MPI_Abort(comm, 1);
    }

    const int use_streaming = (argc >= 5) && (strcasecmp(argv[4], "stream") == 0);
    const int read_results = (argc >= 6) && (strcasecmp(argv[5], "true") == 0);
    const int print_points = !(argc >= 7) || (strcasecmp(argv[6], "true") == 0);

    fprintf(stderr, "NOTE: Running the query in %s mode\n", use_streaming ? "STREAM" : "FILE");
    fprintf(stderr, "NOTE: %s print query result points\n", print_points ? "WILL" : "WILL NOT");
    fprintf(stderr, "NOTE: %s read data using query result point selection\n", read_results ? "WILL" : "WILL NOT");

    // ADIOS init
    adios_read_init_method(method, comm, NULL);

    f = use_streaming ?
    		adios_read_open(argv[1], method, comm, ADIOS_LOCKMODE_ALL, -1) :
    		adios_read_open_file(argv[1], method, comm);
    if (f == NULL) {
        fprintf(stderr," can not open file %s \n", argv[1]);
        MPI_Abort(comm, 1);
    }

    // Parse the xml file to generate query info
    queryInfo = parseXml(xmlFileName, f);

    // perform query
    adios_query_set_method(queryInfo->query, query_method);
    performQuery(queryInfo, f, use_streaming, print_points, read_results);


    adios_read_close(f);
    adios_read_finalize_method(ADIOS_READ_METHOD_BP);

    MPI_Finalize();
    return 0;
}
