/*
 * adios_query_xml_parse.c
 *
 *  Created on: Sep 30, 2014
 *      Author: Houjun Tang
 */

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <assert.h>
#include "adios_selection.h"
#include "adios_query.h"
#include <mxml.h>
#include <sys/stat.h>

#include "adios_query_xml_parse.h"

#define MAXDIM    10
#define MAXQUERY  1000

#define GET_ATTR2(n,attr,var,en)                                 \
    if (!strcasecmp (n, attr->name)) {                           \
        if (!var)                                                \
        {                                                        \
            var = attr->value;                                   \
            continue;                                            \
        }                                                        \
        else                                                     \
        {                                                        \
            printf ("xml: duplicate attribute %s on %s (ignored)",n,en); \
            continue;                                            \
        }                                                        \
    }

// Stack for storing queries
typedef struct {
    int size;
    ADIOS_QUERY *stack[MAXQUERY];
} QueryStack;

// init query stack
static void queryStackInit(QueryStack* queryStack)
{
    queryStack->size=0;
}

static void queryPush(QueryStack* queryStack, ADIOS_QUERY *q)
{
    if (queryStack->size>=MAXQUERY) {
        fprintf(stderr, "Query number exceeds MAXQUERY, exiting\n");
        abort();
    }
    queryStack->stack[queryStack->size++] = q;

}

static int queryStackSize(QueryStack* queryStack)
{
    return queryStack->size;
}

static ADIOS_QUERY * queryPop(QueryStack* queryStack)
{
    if (queryStackSize(queryStack)==0) {
        fprintf(stderr, "Error: popping empty query stack, exiting...\n");
        abort();
    }
    return queryStack->stack[--queryStack->size];
}

void trim_spaces(char * str); // in adios_internals.c
static void tokenize_dimensions2 (const char * str, char *** tokens, int * count) {
    if (!str) {
        *tokens = 0;
        *count = 0;

        return;
    }

    char * save_str = strdup (str);
    char * t = save_str;
    int i;

    trim_spaces (save_str);

    if (strlen (save_str) > 0)
        *count = 1;
    else
    {
        *tokens = 0;
        *count = 0;
        free (save_str);

        return;
    }

    while (*t)
    {
        if (*t == ',')
            (*count)++;
        t++;
    }

    *tokens = (char **) malloc (sizeof (char **) * *count);
    (*tokens) [0] = strdup (strtok (save_str, ","));
    for (i = 1; i < *count; i++)
    {
        (*tokens) [i] = strdup (strtok (NULL, ","));
    }

    free (save_str);
}
//end of stolen functions

#define CHECK_ERROR_DATA(data, num, check) {                     \
		 uint64_t di = 0;                                        \
		 for(di = 0; di < (num); di++){                          \
				if (check)                                       \
					fprintf(stderr, "error data: %f, ", (data)[di]);      \
		 }                                                       \
}

ADIOS_QUERY_TEST_INFO * parseXml(const char *inputxml, ADIOS_FILE* f) {
	int i, j;
	FILE * fp = fopen (inputxml,"r");
	if (!fp){
		fprintf(stderr, "missing xml input file %s \n", inputxml);
		return NULL;
	}
	struct stat s;
	char * buffer = NULL;
	if (stat (inputxml, &s) == 0) {
		buffer = malloc (s.st_size + 1);
		buffer [s.st_size] = 0;
	}

	if (buffer)     {
		size_t bytes_read = fread (buffer, 1, s.st_size, fp);

		if (bytes_read != s.st_size) {
			fprintf(stderr, "error reading input xml file: %s. Expected %ld Got %lld\n"
					,inputxml, s.st_size, (long long int)bytes_read );
			fclose(fp);
			return NULL;
		}
	}
	fclose (fp);
	mxml_node_t * root = NULL;
	root = mxmlLoadString (NULL, buffer, MXML_TEXT_CALLBACK);
	free (buffer);
	buffer = NULL;

	if (!root) {
		fprintf(stderr,  "unknown error parsing XML (probably structural)\n"
				"Did you remember to start the file with\n"
				"<?xml version=\"1.0\"?>\n");
		return NULL;
	}
	if (strcasecmp(root->value.element.name, "query") != 0) {
		root = mxmlFindElement(root, root, "query", NULL, NULL, MXML_DESCEND_FIRST);
	}

	const char *numVarS=NULL;
	const char *fromTimestepS=NULL;
	const char *numTimestepsS=NULL;
	const char *batchsizeS=NULL;

	int numQuery = 0;
	int fromTimestep = 1;
	int numTimesteps = 1;
	uint64_t batchsize= 1;
	for (i = 0; i < root->value.element.num_attrs; i++) {
		mxml_attr_t * attr = &root->value.element.attrs [i];
		GET_ATTR2("num",attr,numVarS,"query");
		GET_ATTR2("from-timestep",attr,fromTimestepS,"query");
		GET_ATTR2("num-timesteps",attr,numTimestepsS,"query");
		GET_ATTR2("batchsize",attr,batchsizeS,"query");
	}
	if ( !numVarS || !strcmp ( numVarS, "")) {
		fprintf(stderr, "missing values for num attribute \n");
		mxmlRelease(root);
		return NULL;
	}
	else {
		numQuery  = atoi(numVarS);
		fromTimestep  = atoi(fromTimestepS);
		numTimesteps = atoi(numTimestepsS);
		batchsize = strtoull(batchsizeS, NULL, 10);
	}

	mxml_node_t *outputNode     = NULL;
	const char *outputTypeS=NULL, *outputDimS=NULL, *outputStartS=NULL, *outputCountS=NULL, *outputWbIndexS=NULL;
	int outputDim;
	int outputWbIndex;
	int selType;
	char** outputCountTokens=NULL;
	char** outputStartTokens=NULL;
	ADIOS_SELECTION *outputBox;

	// Parse output selection info

	outputNode = mxmlFindElement(root, root, "output", NULL, NULL, MXML_DESCEND_FIRST);
	for (i = 0; i < outputNode->value.element.num_attrs; i++) {
		mxml_attr_t * attr = &outputNode->value.element.attrs [i];
		GET_ATTR2("type",attr,outputTypeS,"output");
		if ( strcmp(outputTypeS, "ADIOS_SELECTION_BOUNDINGBOX") == 0) {
			selType = ADIOS_SELECTION_BOUNDINGBOX;
			GET_ATTR2("dim",attr,outputDimS,"output");
			GET_ATTR2("start",attr,outputStartS,"output");
			GET_ATTR2("count",attr,outputCountS,"output");
		}
		else if ( strcmp(outputTypeS, "ADIOS_SELECTION_WRITEBLOCK") == 0) {
			selType = ADIOS_SELECTION_WRITEBLOCK;
			GET_ATTR2("index",attr,outputWbIndexS,"selection");
		}
	}
	if ( selType == ADIOS_SELECTION_BOUNDINGBOX ) {
		if ( !outputTypeS || !outputDimS || !outputStartS || !outputCountS || !strcmp (outputTypeS, "")|| !strcmp (outputDimS, "") || !strcmp (outputStartS, "") || !strcmp (outputCountS, "") ) {
			fprintf(stderr, "missing values for output attribute \n");
			mxmlRelease(root);
			return NULL;
		}
		else {
			outputDim = atoi(outputDimS);
			if (outputDim > MAXDIM) {
				fprintf(stderr, "QueryDim exceeds 10, readjust MAXDIM to larger value, exiting...\n");
				abort();
			}

			tokenize_dimensions2(outputStartS, &outputStartTokens, &outputDim);
			tokenize_dimensions2(outputCountS, &outputCountTokens, &outputDim);

			// Allocate arrays to give to the bounding box constructor
			uint64_t *outputStart = malloc(outputDim * sizeof(uint64_t));
			uint64_t *outputCount = malloc(outputDim * sizeof(uint64_t));

			for (j = 0; j < outputDim; j ++){
				outputStart[j] = atoi(outputStartTokens[j]);
				outputCount[j] = atoi(outputCountTokens[j]);
				free(outputStartTokens[j]);
				free(outputCountTokens[j]);
			}
			free(outputStartTokens);
			free(outputCountTokens);

			outputBox = adios_selection_boundingbox(outputDim, outputStart, outputCount);

			/* fprintf(stderr, "Selected output boundingbox: dim:%d start:", outputDim); */
			/* for (j = 0; j < outputDim; j ++){ */
			/* 	fprintf(stderr, " %d", outputStart[j]); */
			/* } */
			/* fprintf(stderr, "\t count:"); */
			/* for (j = 0; j < outputDim; j ++){ */
			/* 	fprintf(stderr, " %d", outputCount[j]); */
			/* } */
			/* fprintf(stderr, "\n"); */

		}

	}
	else if( selType == ADIOS_SELECTION_WRITEBLOCK ) {

		if ( !outputWbIndexS || !strcmp (outputWbIndexS, "") ) {
			fprintf(stderr, "missing values for selection attribute \n");
			mxmlRelease(root);
			return NULL;
		}
		else {
			outputWbIndex = atoi(outputWbIndexS);
			outputBox = adios_selection_writeblock(outputWbIndex);

			/* fprintf(stderr, "Selected output writeblock: %d\n", outputWbIndex); */
		}
	}


	// Iterate all combine/entry nodes in <query>
	mxml_node_t *entryNode      = NULL;
	mxml_node_t *selectionNode  = NULL;
	const char *varNameS=NULL, *opS=NULL, *constraintS=NULL;
	const char *typeS=NULL, *dimS=NULL, *startS=NULL, *countS=NULL, *wbIndexS=NULL;
	int entryIter;
	int queryDim;
	int wbIndex;
	ADIOS_SELECTION *inputSelection;
	ADIOS_QUERY *q, *q1, *q2, *qc;
	char** queryCountTokens=NULL;
	char** queryStartTokens=NULL;
	char *queryCombineOp=NULL;

	// init query stack
	QueryStack queryStack;
	queryStackInit(&queryStack);

	for (entryIter = 0; entryIter < (numQuery*2-1); entryIter++) {

		// Find entry node
		if (entryIter == 0) {
			entryNode = mxmlFindElement(root, root, "entry", NULL, NULL, MXML_DESCEND_FIRST);
		}
		else {
			// this is the only way I found for getting the next <entry> or <combine> node
			entryNode= mxmlWalkNext(entryNode, root, MXML_NO_DESCEND);
			entryNode= mxmlWalkNext(entryNode, root, MXML_NO_DESCEND);
		}

		// check if current node is <combine>
		if ( strcmp( (&(entryNode->value.element.attrs[0]))->name, "op") == 0 ) {
			queryCombineOp = (&(entryNode->value.element.attrs[0]))->value;
			/* fprintf(stderr, "Found combine op %s\n", queryCombineOp); */
			// pop up two query and perform the op
			if (queryStackSize(&queryStack)<2) {
				fprintf(stderr, "Popping with less than 2 queries in query stack, exiting...\n");
				abort();
			}

			q1 = queryPop(&queryStack);
			q2 = queryPop(&queryStack);
			if (strcmp(queryCombineOp, "AND") == 0 || strcmp(queryCombineOp, "and") == 0) {
				qc = adios_query_combine(q1, ADIOS_QUERY_OP_AND, q2);
			}
			else if (strcmp(queryCombineOp, "OR") == 0 || strcmp(queryCombineOp, "or") == 0) {
				qc = adios_query_combine(q1, ADIOS_QUERY_OP_OR, q2);
			}
			queryPush(&queryStack,qc);

			//adios_query_free(q1);
			//adios_query_free(q2);

			continue;
		}


		// Make sure all *S are NULL for verification
		varNameS=NULL, opS=NULL, constraintS=NULL;
		typeS=NULL, dimS=NULL, startS=NULL, countS=NULL, wbIndexS=NULL;

		for (i = 0; i < entryNode->value.element.num_attrs; i++) {
			mxml_attr_t * attr = &entryNode->value.element.attrs [i];
			GET_ATTR2("var",attr,varNameS,"entry");
			GET_ATTR2("op",attr,opS,"entry");
			GET_ATTR2("constraint",attr,constraintS,"entry");
		}
		if ( !varNameS || !opS || !constraintS || !strcmp (varNameS, "")|| !strcmp (opS, "") || !strcmp (constraintS, "") ) {
			fprintf(stderr, "missing values for entry attribute \n");
			mxmlRelease(root);
			return NULL;
		}

		// Parse selection
		selectionNode = mxmlFindElement(entryNode, entryNode, "selection", NULL, NULL, MXML_DESCEND_FIRST);

                if (selectionNode == NULL) {
                    inputSelection = NULL;
                }
                else {
                    // selection is not NULL

		        for (i = 0; i < selectionNode->value.element.num_attrs; i++) {
		        	mxml_attr_t * attr = &selectionNode->value.element.attrs [i];
		        	GET_ATTR2("type",attr,typeS,"selection");
		        	if ( strcmp(typeS, "ADIOS_SELECTION_BOUNDINGBOX") == 0) {
		        		selType = ADIOS_SELECTION_BOUNDINGBOX;
		        		GET_ATTR2("dim",attr,dimS,"selection");
		        		GET_ATTR2("start",attr,startS,"selection");
		        		GET_ATTR2("count",attr,countS,"selection");
		        	}
		        	else if ( strcmp(typeS, "ADIOS_SELECTION_WRITEBLOCK") == 0) {
		        		selType = ADIOS_SELECTION_WRITEBLOCK;
		        		GET_ATTR2("index",attr,wbIndexS,"selection");
		        	}
		        }
        		if ( selType == ADIOS_SELECTION_BOUNDINGBOX ) {
        
        			if ( !typeS || !dimS || !startS || !countS || !strcmp (typeS, "")|| !strcmp (dimS, "") || !strcmp (startS, "") || !strcmp (countS, "") ) {
        				fprintf(stderr, "missing values for selection attribute \n");
        				mxmlRelease(root);
        				return NULL;
        			}
        			else {
        				queryDim = atoi(dimS);
        				if (queryDim > MAXDIM) {
        					fprintf(stderr, "QueryDim exceeds 10, readjust MAXDIM to larger value, exiting...\n");
        					abort();
        				}
        
        				tokenize_dimensions2(startS, &queryStartTokens, &queryDim);
        				tokenize_dimensions2(countS, &queryCountTokens, &queryDim);
        
        				// Allocate arrays to give to the bounding box constructor
        				uint64_t *queryStart = malloc(queryDim * sizeof(uint64_t));
        				uint64_t *queryCount = malloc(queryDim * sizeof(uint64_t));
        
        				for (j = 0; j < queryDim; j ++){
        					queryStart[j] = atoi(queryStartTokens[j]);
        					queryCount[j] = atoi(queryCountTokens[j]);
        					free(queryStartTokens[j]);
        					free(queryCountTokens[j]);
        				}
        				free(queryStartTokens);
        				free(queryCountTokens);
        
        				inputSelection = adios_selection_boundingbox(queryDim, queryStart, queryCount);
                                }
        
				/* fprintf(stderr, "Selected input bounding box:  dim:%d start:", queryDim); */
				/* for (j = 0; j < queryDim; j ++){ */
				/* 	fprintf(stderr, " %d", queryStart[j]); */
				/* } */
				/* fprintf(stderr, "\t count:"); */
				/* for (j = 0; j < queryDim; j ++){ */
				/* 	fprintf(stderr, " %d", queryCount[j]); */
				/* } */
				/* fprintf(stderr, "\n"); */

        		} // end of selType == ADIOS_SELECTION_BOUNDINGBOX
		        else {
			        // selType == ADIOS_SELECTION_WRITEBLOCK
                                if ( !wbIndexS || !strcmp (wbIndexS, "") ) {
			        	fprintf(stderr, "missing values for selection attribute \n");
			        	mxmlRelease(root);
			        	return NULL;
			        }
			        else {
			        	wbIndex = atoi(wbIndexS);
                                }
				
                                inputSelection = adios_selection_writeblock(wbIndex);
                        }
                }// end of selection is not NULL
        

		if( strcmp(opS, "<=") == 0 )
	                q = adios_query_create(f, inputSelection, varNameS, ADIOS_LTEQ, constraintS);
		else if( strcmp(opS, "<") == 0 )
		        q = adios_query_create(f, inputSelection, varNameS, ADIOS_LT, constraintS);
		else if( strcmp(opS, ">=") == 0 )
			q = adios_query_create(f, inputSelection, varNameS, ADIOS_GTEQ, constraintS);
		else if( strcmp(opS, ">") == 0 )
			q = adios_query_create(f, inputSelection, varNameS, ADIOS_GT, constraintS);
		else {
			fprintf(stderr, "Unsupported entry op %s\n", opS);
			return NULL;
		}

	        queryPush(&queryStack,q);

		/* fprintf(stderr, "Parsed entry: var=%s op=%s constraint=%s\n", varNameS, opS, constraintS); */
	}

	ADIOS_QUERY_TEST_INFO *retval = (ADIOS_QUERY_TEST_INFO *)malloc(sizeof(ADIOS_QUERY_TEST_INFO));
	*retval = (ADIOS_QUERY_TEST_INFO){
		.query           = queryPop(&queryStack),
		.outputSelection = outputBox,
		.fromStep        = fromTimestep,
		.numSteps        = numTimesteps,
        .batchSize       = batchsize,
        .varName         = varNameS,
	};
	return retval;
}
