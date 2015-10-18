/**
 * @file: test_common.h
 * @author: Magda Slawinska, aka Magic Magg, magg dot gatech at gmail dot com
 * @date: Dec 19, 2012
 * Modified: Jan 03, 2013
 * The utility macros
 */

#ifndef TEST_COMMON_H_
#define TEST_COMMON_H_

#include <stdio.h>
#include <stdlib.h>


#define DBG_TEST_FAILED_STR		"TEST_FAILED"
#define DBG_TEST_PASSED_STR		"TEST_PASSED"

#define DEBUG

//! Debug printing verbosity
#define DBG_LEVEL   DBG_DEBUG

// New debug messaging state. There is no sense of a "level" for debugging. Each of these define the
// purpose of the messages and is enabled/disabled per file

//! system cannot continue, e.g. malloc
#define DBG_ERROR   0
//!
#define DBG_CRITICAL 1
//! some serious problems
#define DBG_WARNING 2
#define DBG_MESSAGE 3
//! messages about state or configuration; high-level flow
#define DBG_INFO    4
//!  func args, variable values, etc; full flow, may slow system down
#define DBG_DEBUG   5

#define DBG_ERROR_STR 		"ERROR\t"
#define DBG_CRITICAL_STR 	"CRITICAL\t"
#define DBG_WARNING_STR 	"WARNING\t"
#define DBG_MESSAGE_STR 	"MESSAGE\t"
#define DBG_INFO_STR		"INFO\t"
#define DBG_DEBUG_STR		"DEBUG\t"
#define DBG_TODO_STR		"TODO\t"

#define p_test_failed(fmt, args...)                             \
    do {                                                  \
         printf("%s " fmt, DBG_TEST_FAILED_STR,  ##args);  \
         fflush(stdout);  											\
    } while(0)

#define p_test_passed(fmt, args...)                             \
    do {                                                  \
         printf("%s " fmt, DBG_TEST_PASSED_STR,  ##args);  \
         fflush(stdout);  											\
    } while(0)



//! @todo do something like that but smarter without unnecessary copying
#define p_error(fmt, args...)                             				\
    do {                                                                \
        if((DBG_ERROR) <= DBG_LEVEL) {                                  \
            printf("%s(%d) %s:%s:%d: " fmt, DBG_ERROR_STR, (DBG_ERROR), __FILE__, __FUNCTION__, __LINE__, ##args);   \
            fflush(stdout);  											\
        }                                                               \
    } while(0)


#define p_warn(fmt, args...) \
	do {                                                                \
        if((DBG_WARNING) <= DBG_LEVEL) {                                      \
        	printf("%s(%d) %s:%s:%d: " fmt, DBG_WARN_STR, (DBG_ERROR), __FILE__, __FUNCTION__, __LINE__, ##args);   \
            fflush(stdout);												\
        }                                                               \
    } while(0)

#define p_info(fmt, args...) \
	do {                                                                \
        if((DBG_INFO) <= DBG_LEVEL) {                                      \
        	printf("%s(%d) %s:%s:%d: " fmt, DBG_INFO_STR, (DBG_ERROR), __FILE__, __FUNCTION__, __LINE__, ##args);   \
            fflush(stdout);												\
        }                                                               \
    } while(0)

#define p_debug(fmt, args...) \
	do {                                                                \
        if((DBG_DEBUG) <= DBG_LEVEL) {                                      \
        	printf("%s(%d) %s:%s:%d: " fmt, DBG_DEBUG_STR, (DBG_ERROR), __FILE__, __FUNCTION__, __LINE__, ##args);   \
            fflush(stdout);												\
        }                                                               \
    } while(0)


// ------------------------------------
// define some useful macro idioms
// ADIOS UTILS
#define CLOSE_ADIOS_READER(handle, method) \
	adios_read_close(handle); 		 						\
	adios_read_finalize_method(method);						\
	MPI_Finalize();

/**
 * The macro gets the options from the command line
 * It assumes presence and visibility of a few variables such as argc, argv
 *
 * @param adios_opts The structure where adios related command line options will be stored
 *
 * The macro causes to return DIAG_ERR if getting options returned errors
 */
#define GET_ENTRY_OPTIONS(adios_opts, help_string) \
	if (1 == argc){ \
		p_error("See '-h' for options. At least transport param needs to be specified. Quitting ...\n"); \
		return DIAG_ERR; \
	} \
	do { \
		int show_help = 0; \
		if( DIAG_OK != get_options(&adios_opts, argc, argv, &show_help) ){ \
			p_error("Got from get_options(). Quitting ...\n."); \
			return DIAG_ERR; \
		} \
		if (show_help){ \
			usage(argv[0], help_string); \
			return DIAG_OK; \
		} \
	} while(0)

/**
 * checks if the adios call returned not zero and sets the error
 * if yes. Sets the error means increasing err_count++
 * @param fn_call adios function call
 * @param (in/out) err_count incremented if the error observed
 */
#define SET_ERROR_IF_NOT_ZERO(fn_call, err_count)             \
	do { \
		int _error_code = fn_call;                                         \
		if (_error_code != 0){                                             \
			p_error("rank %d %s (%d) %s\n", rank, #fn_call, adios_errno, adios_errmsg()) ;\
			err_count++;                                                   \
		} \
	} while(0)
/**
 * checks if the adios call returned not zero
 * err_count++
 * @param fn_call adios function call
 * @param (in/out) err_count incremented if the error observed
 */
#define SET_ERROR_IF_ZERO(fn_call, err_count)             \
	do { \
		int _error_code = fn_call;                                         \
		if (_error_code == 0){                                             \
			p_error("rank %d: %s: (%d) %s\n", rank, #fn_call, adios_errno, adios_errmsg()) ;\
			err_count++;                                                   \
		} \
	} while(0)
/**
 * prints the info; closes adios and returns the error code
 * if the err_count is set to a positive number
 *
 * @param err_count The variable that positive value indicates that there is an error
 */
#define RET_IF_ERROR(err_count, rank) \
	if ( err_count > 0) { \
		p_info("Rank %d: Quitting ...\n", rank); \
		return DIAG_ERR;                     \
	}

/**
 * prints the info; closes adios and returns the error code
 * if the err_count is set to a positive number
 *
 * @param err_count The variable that positive value indicates that there is an error
 * @param rank the rank that closes the adios
 * @param handle adios handle to close ADIOS
 * @param method what method to close
 */
#define RET_AND_CLOSE_ADIOS_READER_IF_ERROR(err_count, rank, handle, method) \
	if ( err_count > 0) { \
		p_error("rank %d: Quitting ...\n", rank); \
		CLOSE_ADIOS_READER(handle, method);                              \
		return DIAG_ERR;                     \
	}

/**
 * prints the info; closes adios and returns the error code
 * if the err_count is set to a positive number
 *
 * @param test_res the test result
 * @param rank the rank that closes the adios
 * @param handle adios handle to close ADIOS
 * @param method what method to close
 */
#define RET_AND_CLOSE_ADIOS_READER_IF_TEST_FAILED(test_res, rank, handle, method) \
	if ( TEST_FAILED == test_res.result ) { \
		p_test_failed("%s: rank %d\n", test_res.name, rank); \
		CLOSE_ADIOS_READER(handle, method);                 \
		return DIAG_ERR;                                    \
	}
/**
 * breaks the loop if error count is positive
 *
 * @param err_count The variable that positive value indicates that ther is an error
 */
#define BREAK_IF_ERROR(err_count) \
	if ( err_count > 0) { \
		break; \
	}



// -------------------------------
// test macros
// -------------------------------
/**
 * assumes that err_count is defined
 * @param value_ref The reference value
 * @param value     The actual value
 * @param err_count The value of the error counter; if the error increases
 *                  the value will be increased
 * @param test_res  The result of the test
 */
#define TEST_INT_EQUAL(value_ref, value, err_count, test_res) \
	if (value != value_ref ){ \
		test_res = TEST_FAILED; \
		p_test_failed("(expected=%d, got=%d)\n", value_ref, value); \
		err_count++; \
	}

/**
 * assumes that err_count is defined
 * @param value_ref The reference value
 * @param value     The actual value
 * @param err_count The value of the error counter; if the error increases
 *                  the value will be increased
 * @param test_res  The result of the test
 */
#define TEST_LONG_EQUAL(value_ref, value, err_count, test_res) \
	if (value != value_ref ){ \
		test_res = TEST_FAILED; \
		p_test_failed("(expected=%lld, got=%lld)\n", value_ref, value); \
		err_count++; \
	}

/**
 * assumes that err_count is defined
 * @param value_ref The reference value
 * @param value     The actual value
 * @param err_count The value of the error counter; if the error increases
 *                  the value will be increased
 * @param test_res  The result of the test
 */
#define TEST_DOUBLE_EQUAL(value_ref, value, err_count, test_res) \
	if (value != value_ref ){ \
		test_res = TEST_FAILED; \
		p_test_failed("(expected=%0.2f, got=%0.2f)\n", value_ref, value); \
		err_count++; \
	}

#endif /* TEST_COMMON_H_ */
