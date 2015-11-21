/**
 * misc.h
 *
 *  Created on: Jul 5, 2013
 *  Author: Magda Slawinska aka Magic Magg magg dot gatech at gmail.com
 */

#ifndef MISC_H_
#define MISC_H_

#include <adios_read.h>

//! if defined (not commented) the flexpath method will
//! be used; otherwise the  ADIOS_READ_METHOD_BP and MPI
//! if you are switching methods, make sure that arrays.xml
//! contains the correct method
//! you can use the -DFLEXPATH_METHOD in make CFLAGS="-DFLEXPATH_METHOD"
//! to turn this on as well
//#define FLEXPATH_METHOD 1

//! the name of the file to be written test values
#define FILE_NAME "test.bp"

/*
//! size of the X dimension
#define NX_DIM 10

//! the xml containing configuration of ADIOS
#ifdef FLEXPATH_METHOD
#define XML_ADIOS_INIT_FILENAME "test_config_flex.xml"
#define METHOD ADIOS_READ_METHOD_FLEXPATH
#define TRANSPORT "FLEXPATH"
#else
#define XML_ADIOS_INIT_FILENAME "test_config_mpi.xml"
#define METHOD ADIOS_READ_METHOD_BP
#define TRANSPORT "MPI"
#endif

//! options how verbose is ADIOS (see  adios_read_init_method)
//! 0=quiet, ..., 4=debug
#define ADIOS_OPTIONS "verbose=4; show hidden_attrs"

//! defines if the test passed
#define TEST_PASSED 0
//! defines if the test failed
#define TEST_FAILED -1

//! indicates that the program
#define PROGRAM_ERROR -2

*/

/**
 * Describes the status of a test
 */
enum test_status {
	TEST_UNKNOWN = -2,

	TEST_FAILED = -1,
	TEST_PASSED = 0,
};

/**
 * Store the information about the tests
 */
struct test_info {
	//! the result of the status
	enum test_status result;
	//! test name
	char *name;
};

//! diagnostic information
typedef enum {
	DIAG_ERR = -1,
	DIAG_OK = 0,
} diag_t;

/**
 * This describes the input parameters for the test
 * TODO probably not used - to be removed
 */
struct test_input_params {
	//! the transport e.g., "flx", "mpi"
	char tsprt[256];
	//! 0 - don't show the help
	//! 1 - help requested
	int help;
};

/**
 * The structure that describes the
 * ADIOS options, transport and a reading method
 */
struct adios_tsprt_opts {
	//! the name of the xml file if xml method is chosen
	char xml_adios_init_filename[256];
	//! the  read method e.g. ADIOS_READ_METHOD_FLEXPATH or ADIOD_READ_METHOD_BP
	enum ADIOS_READ_METHOD method;
	//! the transport e.g., "FLEXPATH", "MPI"
	char transport[256];
	//! can be used to store adios options as used by adios_read_init_method
	//! 0=quieet, ..., 4=debug
	char adios_options[256];
};

/**
 * For storing errors
 */
struct err_counts {
	int adios;        // counter for adios calls errors
	int test;		  // counter for comparisons errors
};




#endif /* MISC_H_ */
