/**
 * utils.c
 *
 *  Created on: Jul 5, 2013
 *  Author: Magda Slawinska aka Magic Magg magg dot gatech at gmail.com
 *
 *  Utility functions.
 */
#include "mpi.h"
#include "adios_read.h"
#include "adios.h"

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

#include "misc.h"
#include "utils.h"


/**
 * @param program_name The name of the program
 * @param program_desc The description of the program
 * @return DIAG_OK Returns always DIAG_OK
 */
diag_t usage(char *program_name, char *program_desc){
	printf("USAGE\n");
	printf("  mpirun -np nprocs %s [-h] -t method\n", program_name);
	printf("\nDESCRIPTION\n");
	printf("  %s\n", program_desc);
	printf("\n nprocs Number of processors you want to use\n");
	printf(" -t method\n");
	printf("     The transport/read method. Currently supported flx and mpi.\n");
	printf("     flx sets the FLEXPATH/ADIOS_READ_METHOD_FLEXPATH\n");
	printf("     mpi sets the MPI/ADIOS_READ_METHOD_BP\n");
	printf(" -h\n");
	printf("     show usage\n");
	printf("\nEXAMPLES\n");
	printf("mpirun -np 2 %s -t flx\n", program_name);
	printf("mpirun -np 2 %s -t mpi\n", program_name);

	return DIAG_OK;
}

/**
 * Generates the 1D array of length arr_len, based on the provided rank
 *
 * 	 p_arr : rank 0: 1, 2, 3, 4, 5, ....
 *           rank 1: 10, 11, 12, 13, ...
 *           rank 2: 20, 21, 22, 23, ...
 *
 * The function does not check if arr_len does check if the memory is overwritten
 *
 * @param p_arr The pointer to the array that will hold the values
 * @param arr_len The number of elements in the array
 * @param rank The rank for which I want to have the number generated
 *
 * @return DIAG_ERR if the p_arr is NULL
 *         DIAG_OK otherwise
 *          p_arr (with generated numbers)
 */
diag_t gen_1D_array(double *p_arr, int arr_len, int rank){

	if (!p_arr){
		fprintf(stderr, "ERROR: p_arr is NULL\n.");
		return DIAG_ERR;
	}
	int i = 0;
	for( i = 0; i < arr_len; i++){
		p_arr[i] = rank * 10 + i;
	}

	return DIAG_OK;
}

/**
 * TODO each rank generates the same sequence of numbers
 * Generates the 1D array of length arr_len, based on the provided rank
 * It assumes that the memory is allocated for p_arr
 *
 * 	 p_arr : rank 0: 1, 2, 3, 4, 5, ....
 *           rank 1: 10, 11, 12, 13, ...
 *           rank 2: 20, 21, 22, 23, ...
 *
 * The function does not check if the memory is overwritten
 *
 * @param p_arr The pointer to the array that will hold the values
 * @param arr_len The number of elements in the array
 * @param rank The rank for which I want to have the number generated
 *
 * @return DIAG_ERR if the p_arr is NULL
 *         DIAG_OK otherwise
 *          p_arr (with generated numbers)
 */
diag_t gen_1D_array2(double *p_arr, int arr_len, int rank){

	if (!p_arr){
		fprintf(stderr, "ERROR: p_arr is NULL\n.");
		return DIAG_ERR;
	}
	int i = 0;
	for( i = 0; i < arr_len; i++){
		// see comment in @gen_1D_array
		//p_arr[i] = rank * 10.0 + i;
		p_arr[i] = i * 1.0;
	}

	return DIAG_OK;
}

/**
 * TODO this is not true; each rank generates the same sequence of numbers
 * Generates the 1D array of length arr_len, based on the provided rank
 * It assumes that the memory is allocated for p_arr
 *
 * 	 p_arr : rank 0: 1, 2, 3, 4, 5, ....
 *           rank 1: 10, 11, 12, 13, ...
 *           rank 2: 20, 21, 22, 23, ...
 *
 * The function does not check if the memory is overwritten
 *
 * @param p_arr The pointer to the array that will hold the values
 * @param arr_len The number of elements in the array
 * @param rank The rank for which I want to have the number generated
 *
 * @return DIAG_ERR if the p_arr is NULL
 *         DIAG_OK otherwise
 *          p_arr (with generated numbers)
 */
diag_t gen_1D_array_int(int *p_arr, int arr_len, int rank){

	if (!p_arr){
		fprintf(stderr, "ERROR: p_arr is NULL\n.");
		return DIAG_ERR;
	}
	int i = 0;
	for( i = 0; i < arr_len; i++){
		// the diversification with rank will not work for as the last one will be used
		// if more than 1 ranks will be used and the reader will signal the error
		// that's why I decided to go with the same value independent on
		// the rank
		// TODO but the above and below is something to explore
		//p_arr[i] = rank * 10 + i;
		p_arr[i] = i;
	}

	return DIAG_OK;
}

/**
 * get the commandline options and fills the p_tsprt_opts structure with
 * relevant parameters for ADIOS; sets *p_show_help to 1 if the user
 * requested the help information
 *
 * Currently the following options are supported:
 * ./executable -h
 * ./executable -t flx
 * ./executable -t mpi
 *
 * @param p_tsprt_opts (OUT) the structure filled with parameters from
 *                           the command line
 * @param argc (IN) as from main()
 * @param argv (IN) same meaning as main()
 * @param p_show_help (OUT) 0 - means no help printf requested
 *                          1 - means the help printf info requested
 *
 * @return DIAG_OK if everything ok
 *         DIAG_ERR if some errors, e.g., the option has not been found
 *
 */
diag_t get_options(struct adios_tsprt_opts * p_tsprt_opts, int argc, char ** argv, int *p_show_help){

	diag_t diag = DIAG_OK;
	int c;
	char *cvalue = NULL;
	int i = 0;

	opterr = 0;

	// assume that no showing help
	*p_show_help = 0;

	while( (c = getopt(argc, argv, "ht:")) != -1){
		switch(c){
		case 'h':
			*p_show_help = 1;
			break;
		case 't':
			cvalue = optarg;
			// by default I set adios options to be verbosed
			// TODO might be changed to the actual option
			strcpy(p_tsprt_opts->adios_options, "verbose=4; show hidden_attrs");

			if (strcmp(cvalue, "flx") == 0){
				p_tsprt_opts->method = ADIOS_READ_METHOD_FLEXPATH;
				strcpy(p_tsprt_opts->transport, "FLEXPATH");
				strcpy(p_tsprt_opts->xml_adios_init_filename, "test_config_flex.xml");
			} else if (strcmp(cvalue, "mpi") == 0) {
				p_tsprt_opts->method = ADIOS_READ_METHOD_BP;
				strcpy(p_tsprt_opts->transport, "MPI");
				strcpy(p_tsprt_opts->xml_adios_init_filename, "test_config_mpi.xml");
			} else {
				diag = DIAG_ERR;
			}
			break;
		case '?':
			if ('t' == optopt){
				fprintf(stderr, "ERROR: option -%c requires transport argument. See help '-h'.\n", optopt);
			} else if (isprint (optopt)) {
				fprintf (stderr, "ERROR: Unknown option `-%c'.\n", optopt);
			} else {
				fprintf (stderr, "ERROR: Unknown option character `\\x%x'.\n", optopt);
			}
			diag = DIAG_ERR;
	        break;
	    default:
	    	break;
		}
	}

	for(i = optind; i < argc; ++i){
		printf("ERROR: non-option argument %s\n", argv[i]);
	}

	if (DIAG_OK != diag) {
		p_tsprt_opts->transport[0]='\0';
	}
	return diag;
}


/**
 * Sets values the 1D array of length arr_len with a specified value
 * It assumes that the memory is allocated for p_arr
 *
 * The function does not check if the memory is overwritten
 *
 * @param p_arr The pointer to the array that will hold the values
 * @param arr_len The number of elements in the array
 * @param value The value to be set
 *
 * @return DIAG_ERR if the p_arr is NULL
 *          DIAG_OK otherwise
 *          p_arr (with generated numbers)
 */
diag_t set_value(double *p_arr, int arr_len, double value){

	if (!p_arr){
		fprintf(stderr, "p_arr is NULL\n.");
		return DIAG_ERR;
	}
	int i = 0;
	for( i = 0; i < arr_len; i++){
		p_arr[i] = value;
	}

	return DIAG_OK;
}

/**
 * Get the size of the data. The function assumes that the data will be
 * of a "double" type
 *
 * @param shape The array containing the shape values
 * @param shape_elem_count How many elements is in th the shape array
 * @param data_size (OUT) This value is computed based on the shape array
 *
 * @return DIAG_ERR if the shape is null
 *          DIAG_OK if the data_size contains the valid value
 */
diag_t get_data_size(int *shape, int shape_elem_count, int* data_size){

	if (!shape){
		fprintf(stderr, "ERROR: shape is NULL\n");
		return DIAG_ERR;
	}

	int i = 0;
	int my_size = 8; //sizeof(double);

	for(i = 0; i < shape_elem_count; ++i){
		my_size *= shape[i];
	}

	*data_size = my_size;

	return DIAG_OK;
}

/**
 * Generates the maya var name based on the value of MAYA_GF_VAR_PFX macro and the number.
 * Now it generates: MAYA_GF_VAR_PFX0, MAYA_GF_VAR_PFX1, ...
 *
 * The function doesn't protect against too small arra
 * @param (in/out) buf  the buffer for holding the maya variable; it should be big enough
 *                to hold additional characters that will create the final
 *                name of the variable; it is cleaned
 * @param (in) buf_size The size of the buffer in characters
 * @param (in) maya_var_pfx The prefix for the maya variable
 * @param (in) number  The number that will be concatenated with the prefix;
 *                     the number should be positive
 *
 * @return DIAG_OK everything is ok
 *         != DIAG_OK something is wrong
 */
diag_t gen_maya_var_name(char *buf, int buf_size, char *maya_var_pfx, int number){
	if (number < 0){
		return DIAG_ERR;
	}

	memset(buf, 0, buf_size);
	sprintf(buf, "%s%d", maya_var_pfx, number);

	return DIAG_OK;
}

