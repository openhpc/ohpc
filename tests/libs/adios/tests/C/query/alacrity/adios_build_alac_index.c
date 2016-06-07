/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C Example: write a global array from N processors with gwrite
 *
 * How to run: mpirun -np <N> adios_global
 * Output: adios_global.bp
 * ADIOS config file: adios_global.xml
 *
*/
#include <stdio.h>
#include <string.h>
#include "mpi.h"
#include "adios.h"
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <inttypes.h>
#include <mxml.h>
#include <sys/stat.h>
//#include "core/adios_internals.c"

//#define GET_ATTR(n,attr,var,en)
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


void trim_spaces2 (char * str)
{
    char * t = str, * p = NULL;
    while (*t != '\0')
    {
        if (*t == ' ')
        {
            p = t + 1;
            strcpy (t, p);
        }
        else
            t++;
    }

}

void tokenize_dimensions2 (const char * str, char *** tokens, int * count)
{
    if (!str)
    {
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

struct dimensions {
    uint8_t ndims;
    uint32_t * dims ;
    uint8_t element_size;
};

typedef struct dimensions dim_t;

void adios_pin_timestep(uint32_t ts); // Not in the standard header, but accessible

dim_t * initDimension(uint8_t ndims, uint8_t elementSize){
	dim_t * D = (dim_t *) malloc(sizeof(dim_t));
	D->ndims = ndims;
	D->element_size  = elementSize;
	D->dims = (uint32_t* ) malloc(sizeof(uint32_t) * ndims);
	return D;
}

void freeDimension(dim_t * D){
	free(D->dims);
}

void printDimension(dim_t *D){
	printf("ndims[%u], element size [%u], ", D->ndims, D->element_size);
	printf("dims [");
	uint8_t i = 0;
	for(i = 0; i < D->ndims; i ++){
		if (i != D->ndims -1){
			printf("%"PRIu32",", D->dims[i]);
		}else {
			printf("%"PRIu32"]", D->dims[i]);
		}
	}
	printf("\n");
}
void printListVars(char **vars, int numVars){
	int i = 0;
	printf("[");
	for (i = 0; i < numVars; i ++){
		if ( i != numVars -1){
			printf("%s,", vars[i]);
		}else {
			printf("%s]", vars[i]);
		}
	}
	printf("\n");
}

// Tang: add timestep and pg offset struct
struct timesteps {
    uint8_t nts;
    //uint32_t * tss ;
};
typedef struct timesteps ts_t;

ts_t* initTimestep(uint8_t nts){
    ts_t * D = (ts_t *) malloc(sizeof(ts_t));
    D->nts = nts;
    //D->tss = (uint32_t* ) malloc(sizeof(uint32_t) * nts);
    return D;
}

void freeTimestep(ts_t * D){
    //free(D->tss);
    free(D);
}

struct pgoffset{
    uint8_t ndims;
    uint8_t npg;
    uint32_t * off;
};
typedef struct pgoffset pgoff_t;

pgoff_t* initPGoff(uint8_t npg, uint8_t ndims){
    pgoff_t * D = (pgoff_t *) malloc(sizeof(pgoff_t));
    D->npg   = npg;
    D->ndims = ndims;
    D->off   = (uint32_t* ) malloc(sizeof(uint32_t) * npg * ndims);
    return D;
}

void freePGoff(pgoff_t* D){
    free(D->off);
}

// Tang ^


/*
 * calculate the offset of each PG(rank), highest dimension is the fastest dimension
 */
uint32_t * calPGOffsets(const int rank , const uint32_t *dataDim, const uint32_t *pgDim, uint8_t ndims){

	uint32_t pgSize = 1, tmpSize = 1;
	uint8_t i = 0, j = 0, k = 0;
    for (i = 0; i < ndims; i ++) {
        pgSize *= pgDim[i];
    }
    uint32_t startOffset = rank * pgSize;
    uint32_t remain = startOffset;
    uint32_t * offsets= malloc(ndims* sizeof(uint32_t));
	while ( j < ndims){ // j is the dimension to been set
		k = j + 1;
		tmpSize = 1;
		while ( k < ndims){
			tmpSize *= dataDim[k];
			k++;
		}
		offsets[j] = remain / tmpSize ;
		remain = remain  %  tmpSize;
		j ++;
	}
	return offsets;
}

// Given the input file, you want to divide the data into different PG sizes, data is transformed by ALACRITY plugin
// Run this program with only ONE processor.
// this file is stolen from ../transform/adios_write_all_3D.c

void adios_write_pg ( char input_dir [], char transform [], uint8_t nvars, char **vars,
                       dim_t *data_dim, dim_t *pg_dim, ts_t *data_ts, pgoff_t *pgOff)
{
    int         rank, size;
    int         i = 0,   pg = 0; // timestep
    uint32_t    pg_var_size = pg_dim->element_size;
    uint32_t    data_var_size = data_dim->element_size;

    uint32_t    numPGs = 1;
    char        varfile [nvars][256];
    FILE        *fp [nvars];

    int         adios_err;
    uint64_t    adios_groupsize, adios_totalsize;
    int64_t     adios_handle;

    char input_xml [256];
    char output_bp_file [256];

    MPI_Comm    comm = MPI_COMM_WORLD;
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);

    // Get the var size and the pg size
    for (i = 0; i < pg_dim->ndims; i ++) {
        pg_var_size *= pg_dim->dims [i];
    }

    for (i = 0; i < data_dim->ndims; i ++) {
        data_var_size *= data_dim->dims [i];
    }
    printf ("pg_var_size = %u, data_var_size = %u\n", pg_var_size, data_var_size);

    // Read the XML file specific to this transform and PG size
    sprintf (input_xml, "%s/%s.xml", input_dir, transform);
    adios_init (input_xml, comm);

    // Name the output bp file based on the name of the transform
    sprintf (output_bp_file, "%s/%s_%d.bp", input_dir, transform, pg_var_size);

    numPGs = data_var_size / pg_var_size;

    // Open the input raw data file for each variable
    for (i = 0; i < nvars; i ++) {
        sprintf (varfile [i], "%s/%s", input_dir, vars [i]);
        fp [i] = fopen (varfile [i], "rb");
        printf("%s\n", varfile [i]);
        if ( fp[i] == NULL){
        	printf("can not open file %s\n", varfile[i]);
        	return ;
        }
//        assert (fp [i] != 0);
    }
    char *pg_var_data = (char *) malloc (pg_var_size);

    int timestep = 1;


    adios_groupsize = 0;
    adios_groupsize += 4 /*rank*/   + 4  /*size*/ ;
    adios_groupsize += (4 * data_dim->ndims); // /* N_0 + N_{ndim -1} , entire data domain space*/
    adios_groupsize += (4 * data_dim->ndims); // /* O_0 + O_{ndim -1} , offset of write data subspace*/
    adios_groupsize += (4 * data_dim->ndims); // /* D_0 + D_{ndim -1} , pg dimension*/
    adios_groupsize += nvars * ( pg_var_size ); //  total variable size

    /*quick dirty fix on the incorrect data size estimation on the very small data size from transformer */
    adios_groupsize += 10000;

    printf("adios_groupsize = %"PRIu64 "\n", adios_groupsize);

    char tmp[256];
    uint8_t k  = 0;

    int tsIdx;
    for (tsIdx = 0; tsIdx < data_ts->nts; tsIdx++) {
        printf("Writing timestep %d\n", tsIdx);
        if (tsIdx == 0) {
            adios_open (&adios_handle, "S3D", output_bp_file, "w", comm);
        } else {
            adios_open (&adios_handle, "S3D", output_bp_file, "a", comm);
        }

           adios_group_size (adios_handle, adios_groupsize, &adios_totalsize);
            //    adios_open (&adios_handle, "S3D", output_bp_file, "w", comm);
        for (pg = 0; pg < numPGs; pg ++) {

            char prefix[256];
            strcpy(prefix, "N%d");
            	for (k = 0; k < data_dim -> ndims ; k ++ ) {
            		sprintf(tmp, prefix, k); // N0, N1... D0, D1 ... O0, O1, ...
            		adios_write (adios_handle, tmp, &(data_dim->dims[k]));
            	}
            	strcpy(prefix, "D%d");
            	for (k = 0; k < pg_dim -> ndims ; k ++ ) {
            		sprintf(tmp, prefix, k); // N0, N1... D0, D1 ... O0, O1, ...
            		adios_write (adios_handle, tmp, &(pg_dim->dims[k]));
            	}
            	strcpy(prefix, "O%d");
            	for (k = 0; k < data_dim -> ndims ; k ++ ) {
            		sprintf(tmp, prefix, k); // N0, N1... D0, D1 ... O0, O1, ...
            		adios_write (adios_handle, tmp, &(pgOff->off[pg*(pg_dim->ndims)+k]));
            		//adios_write (adios_handle, tmp, &(offsets[k]));
            	}

            adios_write (adios_handle, "size", &size);
            adios_write (adios_handle, "rank", &pg);

            // fread and adios_write for each variable
            int readBytes;
            for (i = 0; i < nvars; i ++) {
                readBytes = fread (pg_var_data, sizeof (char), pg_var_size, fp [i]);
                //printf("%d %x %d\n", pg_var_size, *fp [i], readBytes);
                adios_write (adios_handle, vars [i], pg_var_data);
            }

        }
            adios_close (adios_handle);
    }

    free (pg_var_data);
    for (i = 0; i < nvars; i ++) {
        fclose (fp [i]);
    }

    adios_finalize (rank);

    return ;
}

void testCalculation(dim_t *data_dim, dim_t *pg_dim){
    uint32_t pg_var_size = pg_dim->element_size;
    uint32_t data_var_size = data_dim->element_size;
    int i = 0, k;
    for(i = 0;i < pg_dim->ndims;i++){
        pg_var_size *= pg_dim->dims[i];
    }
    for(i = 0;i < data_dim->ndims;i++){
        data_var_size *= data_dim->dims[i];
    }
    uint32_t numPGs = data_var_size / pg_var_size;
    uint32_t j = 0;
    uint32_t *offset;
    for(j = 0;j < numPGs;j++){
        offset = calPGOffsets(j, data_dim->dims, pg_dim->dims, data_dim->ndims);
        printf("rank [%d] {", j);
        for(k = 0;k < data_dim->ndims;k++){
            if(k != data_dim->ndims - 1){
                printf("%d,", offset[k]);
            }else{
                printf("%d}", offset[k]);
            }
        }
        printf("\n");
		free(offset);

    }

}

/*
 * a bunch of fixed test cases
 */
void testCalPGOffset(){
	dim_t d1 ;
	dim_t p1 ;
	d1.ndims = 1;
	d1.element_size = 8;
	d1.dims [ 0] = 128;
	p1.ndims = 1 ;
	p1.dims[ 0] = 32;
	p1.element_size = 8;
	printf("testing 1 dimension \n");
	testCalculation(&d1, &p1);

	dim_t d2 ;
	dim_t p2 ;
	d2.ndims = 2;
	d2.element_size = 8;
	d2.dims [ 0] = 32;
	d2.dims [ 1] = 32;
	p2.ndims = 2 ;
	p2.dims[ 0] = 16;
	p2.dims[ 1] = 8;
	p2.element_size = 8;
	printf("testing 2 dimension \n");
	testCalculation(&d2, &p2);

	dim_t data_dim;
	dim_t pg_dim;

	data_dim.ndims = 3;  // data variable dimension size
	data_dim.dims [0] = 32;  //256;
	data_dim.dims [1] = 32 ;  //128;
	data_dim.dims [2] = 32;   //128;
	data_dim.element_size = 8;

	pg_dim.ndims = 3; // each block dimension size
	pg_dim.dims [0] = 16; //64
	pg_dim.dims [1] = 16; //32
	pg_dim.dims [2] = 16; //32
	pg_dim.element_size = 8;
	printf("testing 3 dimension \n");
    testCalculation(&data_dim, &pg_dim);
}


int  parseInputs(char * inputxml, dim_t **dataDim, dim_t **pgDim, char*** varList, int *numVarsOut, ts_t **dataTs, pgoff_t **pgOff){

	FILE * fp = fopen (inputxml,"r");
	if (!fp){
		printf("missing xml input file %s \n", inputxml);
		return 1;
	}
	struct stat s;
	char * buffer = NULL;
	if (stat (inputxml, &s) == 0) {
		buffer = malloc (s.st_size + 1);
		buffer [s.st_size] = 0;
	}

	if (buffer)	{
		size_t bytes_read = fread (buffer, 1, s.st_size, fp);

		if (bytes_read != s.st_size) {
			printf("error reading input xml file: %s. Expected %ld Got %ld\n"
		                        ,inputxml, s.st_size, bytes_read );
			fclose(fp);
		    return 1;
		}
	}
	fclose (fp);
	mxml_node_t * doc = NULL;
	mxml_node_t * root = NULL;
	mxml_node_t * varsNode = NULL;
	doc = mxmlLoadString (NULL, buffer, MXML_TEXT_CALLBACK);
	free (buffer);
	buffer = NULL;
	root = doc;

	 if (!doc) {
	        printf( "unknown error parsing XML (probably structural)\n"
	                "Did you remember to start the file with\n"
	                "<?xml version=\"1.0\"?>\n");

	        return 0;
	    }
	if (strcasecmp (doc->value.element.name, "adios-alac-test-inputs")) {
	        root = mxmlFindElement (doc, doc, "adios-alac-test-inputs", NULL, NULL, MXML_DESCEND_FIRST);
        }

	varsNode = mxmlFindElement(root, root, "vars", NULL, NULL, MXML_DESCEND_FIRST);
	mxml_node_t * varnode = NULL, *dimnode = NULL;
	const char * varname = 0,  *dimS = 0, *dataDimS = 0, *pgDimS = 0, *numVarS=0, *elmSizeS=0;

	int numVars = 0, i, j, numDim=0, countVar= 0, elmSize = 0;
	for (i = 0; i < varsNode->value.element.num_attrs; i++) {
		mxml_attr_t * attr = &varsNode->value.element.attrs [i];
		GET_ATTR2("num",attr,numVarS,"vars");
	}
	if ( !numVarS || !strcmp ( numVarS, "")) {
			printf("missing values for num attribute \n");
			mxmlRelease(doc);
			return 0;
	}else {
			numVars = atoi(numVarS);
	}

        // Tang: parsing timestep info
        mxml_node_t * tsnode = NULL;                     
        const char * tsNumS = NULL, *tsS = NULL;               
        int tsNum = 0;                                   
        tsnode = mxmlFindElement(varsNode, varsNode, "timestep", NULL, NULL, MXML_DESCEND_FIRST);
	if ( !tsnode) {
	    printf("missing timestep element under vars \n");
	    mxmlRelease(doc);
	    return 0;
	}
	for (i = 0; i < tsnode->value.element.num_attrs; i++) {
	    mxml_attr_t * attr = &tsnode->value.element.attrs [i];
	    GET_ATTR2("tsnum",attr,tsNumS,"timestep");
	}
	if ( !tsNumS || !strcmp ( tsNumS, "")) {
	    printf("missing values for tsNum attribute on timestep element\n");
	    mxmlRelease(doc);
	    return 0;
	}
        else {
	    tsNum = atoi(tsNumS);
	}

        (*dataTs) = initTimestep(tsNum); 

        // Tang: ^

	dimnode = mxmlFindElement(varsNode, varsNode, "dimension", NULL, NULL, MXML_DESCEND_FIRST);
	if ( !dimnode) {
		printf("missing dimension element under vars \n");
		mxmlRelease(doc);
		return 0;
	}

	uint32_t * inputDataDim, * inputPGDim ;
	for (i = 0; i < dimnode->value.element.num_attrs; i++) {
		mxml_attr_t * attr = &dimnode->value.element.attrs [i];
		GET_ATTR2("dim",attr,dimS,"dimension");
		GET_ATTR2("elementSize",attr,elmSizeS,"dimension");
		GET_ATTR2("dataDim",attr,dataDimS,"dimension");
		GET_ATTR2("pgDim",attr,pgDimS,"dimension");

	}
	if ( !dimS || !strcmp ( dimS, "")) {
		printf("missing values for dim attribute on dimension element\n");
		mxmlRelease(doc);
		return 0;
	}else{
		numDim = atoi(dimS);
	}
	if ( !elmSizeS || !strcmp ( elmSizeS, "")) {
				printf("missing values for elementSize attribute on dimension element \n");
				mxmlRelease(doc);
				return 0;
	}else{
		elmSize = atoi(elmSizeS);
	}


	int dim_count, pgCount;
	char ** dim_tokens = 0, **pgDimTokens = 0;
	if ( !dataDimS || !strcmp ( dataDimS, "")) {
		printf("missing values for dataDim attribute on dimension element  \n");
		mxmlRelease(doc);
		return 0;
	}

	if ( !pgDimS || !strcmp ( pgDimS, "")) {
		printf("missing values for pgDim attribute \n");
		mxmlRelease(doc);
		return 0;
	}
	tokenize_dimensions2 (dataDimS, &dim_tokens, &dim_count);
	tokenize_dimensions2 (pgDimS, &pgDimTokens, &pgCount);
	if (dim_count != numDim || pgCount != numDim){
		printf("input dimension does not match expected number dimension \n");
		mxmlRelease(doc);
		return 0;
	}
	inputDataDim = (uint32_t *) malloc(sizeof(uint32_t)*numDim);
	inputPGDim = (uint32_t *) malloc(sizeof(uint32_t)*numDim);

	for (j = 0; j < numDim ; j ++){
		inputDataDim[j] = atoi(dim_tokens[j]);
		inputPGDim[j] = atoi(pgDimTokens[j]);
	}

	(*dataDim) = initDimension(numDim, elmSize);
	memcpy((*dataDim)->dims, inputDataDim, sizeof(uint32_t)* (*dataDim)->ndims);
	(*pgDim )= initDimension(numDim, elmSize);
	memcpy((*pgDim)->dims, inputPGDim, sizeof(uint32_t)* (*pgDim)->ndims);

	(*varList) = (char **) malloc(sizeof(char*) * numVars);
	for(i= 0; i < numVars;  i ++){
		(*varList)[i] = (char*) malloc(256);
	}
	for (varnode = mxmlWalkNext (varsNode, doc, MXML_DESCEND_FIRST)
	           ;varnode
	           ;varnode = mxmlWalkNext (varnode, varsNode, MXML_NO_DESCEND) ){
	        if (varnode->type != MXML_ELEMENT) {
	                   continue;
	        }
	        if (!strcasecmp (varnode->value.element.name, "var")) {
	       	 for (i = 0; i < varnode->value.element.num_attrs; i++) {
	       		 mxml_attr_t * attr = &varnode->value.element.attrs [i];
	       		 if (!strcmp(attr->name, "name")){
	       			 strcpy((*varList)[countVar], attr->value);
	       			 countVar ++;
	       		 }
	       	 }

	        }
	}

        *numVarsOut = numVars;
	
        
        // Tang: Parsing pg offset info, assuming we have already know dim dataDim and pgDim
        // from parsed result of <dimension/>
        int nPG = 1;
        for (i = 0; i < numDim; i++) {
            nPG *= ((*dataDim)->dims[i] / (*pgDim)->dims[i]); 
        }


        mxml_node_t * pgoffnode = NULL;                     
        const char * pgoffNumS = NULL, *pgoffS = NULL;               
        int pgoffNum= 0;                                   
        pgoffnode = mxmlFindElement(varsNode, varsNode, "pgoffset", NULL, NULL, MXML_DESCEND_FIRST);

	if ( !pgoffnode) {
	    printf("missing pgoffset element under vars \n");
	    mxmlRelease(doc);
	    return 0;
	}

	for (i = 0; i < pgoffnode->value.element.num_attrs; i++) {
	    mxml_attr_t * attr = &pgoffnode->value.element.attrs [i];
	    GET_ATTR2("pgNum",attr,pgoffNumS,"pgoffset");
	}

	if ( !pgoffNumS || !strcmp ( pgoffNumS, "")) {
	    printf("missing values for tsNum attribute on timestep element\n");
	    mxmlRelease(doc);
	    return 0;
	}
        else {
	    pgoffNum = atoi(pgoffNumS);
	}

        if (pgoffNum != nPG) {
	    printf("pgNum from pgoffset doesn't match the number of pgs calculated from dataDim/pgDim \n");
	    mxmlRelease(doc);
	    return 0;
        }

        (*pgOff) = initPGoff(nPG, numDim);

        char pgoffName[16];
        char** pgoffValueS;
        char** pgoffValue_tokens=NULL;
        pgoffValueS = (char**)malloc(sizeof(char*) * nPG);
        for (i = 0; i < nPG; i++) {
            pgoffValueS[i] = NULL;
        }
        pgoffnode = mxmlFindElement(varsNode, varsNode, "pgoffset", NULL, NULL, MXML_DESCEND_FIRST);
	for (i = 0; i < pgoffnode->value.element.num_attrs; i++) {
	    mxml_attr_t * attr = &pgoffnode->value.element.attrs [i];
            for (j = 0; j < nPG; j++) {
                sprintf(pgoffName,"pg_%d", j);
                if (pgoffValueS[j] != NULL) {
                    continue;
                }
	        GET_ATTR2(pgoffName,attr,pgoffValueS[j],"pgoffset");
            }
	}

        for (i = 0; i < nPG; i++) {
            tokenize_dimensions2 (pgoffValueS[i], &pgoffValue_tokens, &numDim);
            for (j = 0; j < numDim; j++) {
                (*pgOff)->off[i*numDim+j] = atoi(pgoffValue_tokens[j]);
            }
        }

        for (i = 0; i < nPG; i++) {
            printf("PG offset_%d: ",i);
            for (j = 0; j < numDim; j++) {
                printf("%d ",(*pgOff)->off[i*numDim+j]);
            }
            printf("\n");
        }

        free(pgoffValueS);
	free(inputDataDim);
	free(inputPGDim);
	return 0;
}
/*
 * ./adios_build_alac_index ./xml alacrity-1var ./xml/build-alac-index-input.xml
 */
int main (int argc, char ** argv)
{
    MPI_Init (&argc, &argv);

  if (argc >= 4) {
	 dim_t *dataDim , *pgDim ;
         ts_t *dataTs;              // Tang: add for timestep
         pgoff_t *pgOff;            // Tang: add for pg offsets
	 char **vars ;
	 int numVar;
	 //if ( parseInputs(argv[3],&dataDim,&pgDim, &vars , &numVar) ){
	 if ( parseInputs(argv[3],&dataDim,&pgDim, &vars , &numVar, &dataTs, &pgOff) ){
		 printf("errors in the input xml file parse \n");
		 return 0;
	 }else {
		 printf("XML Inputs info \n");
		 printf("data space: ");
		 printDimension(dataDim);
		 printf("PG space: ");
		 printDimension(pgDim);
		 printListVars(vars, numVar);
	 }
	 printf("Write BP file \n");
	 //adios_write_pg (argv [1], argv [2], numVar, vars, dataDim, pgDim);
	 adios_write_pg (argv [1], argv [2], numVar, vars, dataDim, pgDim, dataTs, pgOff);
	 freeDimension(dataDim);
	 freeDimension(pgDim);
         freeTimestep(dataTs);     // Tang: free dataTs
         freePGoff(pgOff);

  } else {
	  printf ("Usage: %s <base directory> <transform> <input xml> \n Example: ./adios_build_alac_index ./xml alacrity-1var ./xml/build-alac-index-input.xml \n", argv [0]);
 }


    MPI_Finalize ();
    return 0;
}

