/**
 *
 * @file
 *
 *  PLASMA is a software package provided by:
 *  University of Tennessee, US,
 *  University of Manchester, UK.
 *
 **/
#ifndef TEST_H
#define TEST_H

#include "plasma_types.h"

#include <stdbool.h>

//==============================================================================
// parameter labels
// The order here determines the order of output columns
//==============================================================================
typedef enum {
    //------------------------------------------------------
    // output parameters
    //------------------------------------------------------
    PARAM_SUCCESS, // success indicator
    PARAM_ERROR,   // numerical error
    PARAM_ORTHO,   // orthogonality error
    PARAM_TIME,    // time to solution
    PARAM_GFLOPS,  // GFLOPS rate
    PARAM_ITERSV,  // iterations to solution

    //------------------------------------------------------
    // tester parameters
    //------------------------------------------------------
    PARAM_ITER,    // number of iterations
    PARAM_OUTER,   // outer product iteration?
    PARAM_DIM_OUTER, // outer product iteration for dimensions M, N, K?
    PARAM_TEST,    // test the solution?
    PARAM_TOL,     // tolerance

    //------------------------------------------------------
    // function input parameters
    //------------------------------------------------------
    // char params
    PARAM_COLROW,  // columnwise or rowwise operation
    PARAM_NORM,    // type of matrix norm
    PARAM_TRANS,   // transposition
    PARAM_TRANSA,  // transposition of A
    PARAM_TRANSB,  // transposition of B
    PARAM_SIDE,    // left of right side application
    PARAM_UPLO,    // general rectangular or upper or lower triangular
    PARAM_DIAG,    // non-unit or unit diagonal
    PARAM_HMODE,   // Householder mode - tree or flat

    // numeric params
    PARAM_DIM,     // M, N, K dimensions
    PARAM_KL,      // lower bandwidth
    PARAM_KU,      // upper bandwidth
    PARAM_NRHS,    // number of RHS
    PARAM_NB,      // tile size NBxNB
    PARAM_IB,      // inner blocking size
    PARAM_ALPHA,   // scalar alpha
    PARAM_BETA,    // scalar beta
    PARAM_PADA,    // padding of A
    PARAM_PADB,    // padding of B
    PARAM_PADC,    // padding of C
    PARAM_MTPF,    // maximum number of threads for panel factorization
    PARAM_ZEROCOL, // if positive, a column of zeros inserted at that index
    PARAM_INCX,    // 1 to pivot forward, -1 to pivot backward

    //------------------------------------------------------
    // Keep at the end!
    //------------------------------------------------------
    PARAM_SIZEOF   // size of parameter array
} param_label_t;

//==============================================================================
// tester infrastructure
//==============================================================================
typedef struct {
    int m, n, k;
} int3_t;

// parameter value type
typedef struct {
    int i;                 // integer
    char c;                // character
    double d;              // double precision
    plasma_complex64_t z;  // double complex
    int3_t dim;            // m, n, k problem size
    int used;              // whether routine uses parameter
} param_value_t;

// bit flags to differentiate use of M, N, K in PARAM_DIM
enum {
    PARAM_USE_M = 0x1,
    PARAM_USE_N = 0x2,
    PARAM_USE_K = 0x4,
};

// parameter type
typedef struct {
    bool is_list;       // parameter is single value or list of values?
    int num;            // number of values for a parameter
    int pos;            // current position in the array
    int size;           // size of parameter values array
    param_value_t *val; // array of values for a parameter
} param_t;

// hiding double from precision translation when used for taking time
typedef double plasma_time_t;

// initial size of values array
static const int InitValArraySize = 1024;

// indentation of option descriptions
static const int DescriptionIndent = -20;

// maximum length of info string
static const int InfoLen = 1024;

// spacing in info output string
// each column is InfoSpacing wide + 1 space between columns
static const int InfoSpacing = 11;

// function declarations
void print_main_usage(const char *program_name);
void print_routine_usage(const char *program_name, const char *name, param_value_t pval[]);
void print_usage(int label);
void print_header(const char *name, param_value_t param[]);
int  test_routine(const char *name, param_value_t param[], bool test);
void run_routine(const char *name, param_value_t pval[], bool run);
void param_init(param_t param[]);
void param_read(int argc, char **argv, param_t param[]);
int  param_starts_with(const char *str, const char *prefix);
int  scan_irange(const char **strp, int *start, int *end, int *step);
int  scan_drange(const char **strp, double *start, double *end, double *step);
int  param_scan_int(const char *str, param_t *param);
int  param_scan_int3(const char *str, param_t *param, bool outer);
int  param_scan_char(const char *str, param_t *param);
int  param_scan_double(const char *str, param_t *param);
int  param_scan_complex(const char *str, param_t *param);
void param_add(param_t *param);
void param_add_int(int val, param_t *param);
void param_add_int3(int3_t val, param_t *param);
void param_add_char(char cval, param_t *param);
void param_add_double(double dval, param_t *param);
void param_add_complex(plasma_complex64_t zval, param_t *param);
int  param_step_inner(param_t param[]);
int  param_step_outer(param_t param[], int idx);
int  param_snap(param_t param[], param_value_t value[]);

//==============================================================================
static inline int imin(int a, int b)
{
    if (a < b)
        return a;
    else
        return b;
}

//==============================================================================
static inline int imax(int a, int b)
{
    if (a > b)
        return a;
    else
        return b;
}

#include "test_s.h"
#include "test_d.h"
#include "test_ds.h"
#include "test_c.h"
#include "test_z.h"
#include "test_zc.h"

#endif // TEST_H
