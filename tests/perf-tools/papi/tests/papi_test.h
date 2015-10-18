/* Standard headers for PAPI test applications.
	This file is customized to hide Windows / Unix differences.
*/

#include <stdlib.h>
#include <stdio.h>

#include <unistd.h>
#include <sys/wait.h>
#if (!defined(NO_DLFCN) && !defined(_BGL) && !defined(_BGP))
#include <dlfcn.h>
#endif

#include <errno.h>
#include <memory.h>
#if !defined(__FreeBSD__) && !defined(__APPLE__)
#include <malloc.h>
#endif
#include <assert.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <math.h>

#include "papiStdEventDefs.h"
#include "papi.h"
#include "test_utils.h"

/* Masks to select operations for add_test_events() and remove_test_events()
	Mask value tells us what events to select.
*/
#define MASK_FP_OPS  0x80000
#define MASK_L1_DCA  0x40000    /* three new events for POWER4 */
#define MASK_L1_DCW  0x20000
#define MASK_L1_DCR  0x10000
#define MASK_TOT_IIS 0x04000    /* Try this if TOT_INS won't work */
#define MASK_BR_PRC  0x02000
#define MASK_BR_MSP  0x01000
#define MASK_BR_CN   0x00800
#define MASK_L2_TCH  0x00400
#define MASK_L2_TCA  0x00200
#define MASK_L2_TCM  0x00100
#define MASK_L1_DCM  0x00040
#define MASK_L1_ICM  0x00020
#define MASK_L1_TCM  0x00010
#define MASK_FP_INS  0x00004
#define MASK_TOT_INS 0x00002
#define MASK_TOT_CYC 0x00001

#define MAX_TEST_EVENTS 18

struct test_events_t {                                                       
  unsigned int mask;                                                         
  unsigned int event;                                                        
};      

extern struct test_events_t test_events[];

int papi_print_header (char *prompt, const PAPI_hw_info_t **hwinfo);
void validate_string(char *name, char *s);
void *get_overflow_address(void *context);
void free_test_space(long long ** values, int num_tests);
long long **allocate_test_space(int num_tests, int num_events);
int add_test_events(int *number, int *mask, int allow_derived);
int add_two_events(int *num_events, int *papi_event, int *mask);
int add_two_nonderived_events(int *num_events, int *papi_event, int *mask);
int add_test_events_r(int *number, int *mask, void *handle);
int find_nonderived_event( void );
int enum_add_native_events(int *num_events, int **evtcodes, int need_interrupts, int no_software_events, int cidx);
int remove_test_events(int *EventSet, int mask);
void do_flush(void);
void do_misses(int n, int size);
void do_flops(int n);
/* export the next symbol as 'end' address of do_flops for profiling */
void fdo_flops(int *n); 
void do_reads(int n);
void do_both(int n);
void do_l1misses(int n);
void do_stuff(void);
void dummy(void *);
char *stringify_domain(int domain);
char *stringify_all_domains(int domains);
char *stringify_granularity(int granularity);
char *stringify_all_granularities(int granularities);
void tests_quiet(int argc, char **argv);
void test_pass(char *file, long long ** values, int num_tests);
void test_fail(char *file, int line, char *call, int retval);
void test_fail_exit(char *file, int line, char *call, int retval);
void test_skip(char *file, int line, char *call, int retval);
void test_warn(char *file, int line, char *call, int retval);
void test_print_event_header(char *call, int evset);
void touch_dummy(double *ptr, int size);
int approx_equals(double a, double b);
void init_multiplex(void);

void clockcore(void);

/* Unix systems use %lld to display long long values
	Windows uses %I64d for the same purpose.
	Since these occur inside a quoted string,
	we must #define the entire format string.
	Below are several common forms of this string
	for both platforms.
*/

#define ONEHDR  " %12s"
#define TAB2HDR	"%s %12s %12s\n"
#define TAB3HDR	"%s %12s %12s %12s\n"
#define TAB4HDR	"%s %12s %12s %12s %12s\n"
#define ONENUM  " %12lld"
#define TAB1	"%-12s %12lld\n"
#define TAB2	"%-12s %12lld %12lld\n"
#define TAB3	"%-12s %12lld %12lld %12lld\n"
#define TAB4	"%-12s %12lld %12lld %12lld %12lld\n"
#define TAB5	"%-12s %12lld %12lld %12lld %12lld %12lld\n"
#define TWO12	"%12lld %12lld  %s"
#define LLDFMT  "%lld"
#define LLDFMT10 "%10lld"
#define LLDFMT12 "%12lld"
#define LLDFMT15 "%15lld"

extern int TESTS_QUIET;         /* Declared in test_utils.c */

#define printf if (!TESTS_QUIET) printf

