/**
 *
 * @file
 *
 *  PLASMA is a software package provided by:
 *  University of Tennessee, US,
 *  University of Manchester, UK.
 *
 **/
#include "test.h"
#include "plasma.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/******************************************************************************/
typedef void (*test_func_ptr)(param_value_t param[], bool run);

struct routines_t {
    const char *name;
    test_func_ptr func;
};

// To maintain 4 columns (z, d, s, c), each routine should have 4 entries;
// use { "", NULL }  entries for missing precisions.
struct routines_t routines[] =
{
    { "dgesv", test_dgesv },
    { NULL, NULL }  // last entry
};

//==============================================================================
// parameter descriptions
//==============================================================================

// 2nd line indent for help text
#define INDENT "\t                    "

typedef struct {
    const char *arg;
    const char *header;
    int width;
    bool is_list;
    const char *help;
} param_desc_t;

static param_desc_t ParamDesc[] = {
    //------------------------------------------------------
    // output parameters
    //------------------------------------------------------
    // argument            header          width  is_list  help
    {"success",            "Status",       6,     false,
     "success indicator"},

    {"error",              "Error",        9,     false,
     "numerical error"},

    {"ortho",              "Orth. error",  9,     false,
     "orthogonality error"},

    {"time",               "Time",         9,     false,
     "time to solution"},

    {"gflops",             "Gflop/s",      9,     false,
     "GFLOPS rate"},

    {"itersv",             "IterSv",       9,     false,
     "iterations to solution"},

    //------------------------------------------------------
    // tester parameters
    //------------------------------------------------------
    // argument            header          width  is_list  help
    {"--iter=",            "iter",         0,     false,
     "number of iterations per set of parameters [default: 1]"},

    {"--outer=[y|n]",      "outer",        0,     false,
     "outer product iteration [default: y]"},

    {"--dim-outer=[y|n]",  "dim-outer",    0,     false,
     "outer product iteration of subsequent --dim dimensions [default: n]"},

    {"--test=[y|n]",       "test",         0,     false,
     "test the solution [default: y]"},

    {"--tol=",             "tol",          0,     false,
     "tolerance [default: 50.0]"},

    //------------------------------------------------------
    // function input parameters
    //------------------------------------------------------
    // argument            header          width  is_list  help
    {"--colrow=[c|r]",     "colrow",       6,     true,
     "columnwise or rowwise [default: c]"},

    {"--norm=[m|o|i|f]",   "norm",         4,     true,
     "type of matrix norm (max, one, inf, frobenius) [default: o]"},

    {"--trans=[n|t|c]",    "trans",        6,     true,
     "transposition [default: n]"},

    {"--transa=[n|t|c]",   "transA",       6,     true,
     "transposition of A [default: n]"},

    {"--transb=[n|t|c]",   "transB",       6,     true,
     "transposition of B [default: n]"},

    {"--side=[l|r]",       "side",         6,     true,
     "left or right side application [default: l]"},

    {"--uplo=[g|u|l]",     "uplo",         6,     true,
     "general rectangular or upper or lower triangular matrix [default: l]"},

    {"--diag=[n|u]",       "diag",         6,     true,
     "non-unit diagonal or unit diagonal [default: n]"},

    {"--hmode=[f|t]",      "House. mode",  11,    true,
     "Householder mode for QR/LQ - flat or tree [default: f]"},

    {"--dim=",             "Dimensions",   6,     true,
     "M x N x K dimensions [default: 1000 x 1000 x 1000]\n"
     INDENT "M, N, K can each be a single value or a range.\n"
     INDENT "N and K are optional; if not given, N=M and K=N.\n"
     INDENT "Ex: --dim=100:300:100x64 is 100x64x64, 200x64x64, 300x64x64."},

    {"--kl=",              "kl",           6,     true,
     "Lower bandwidth [default: 200]"},

    {"--ku=",              "ku",           6,     true,
     "Upper bandwidth [default: 200]"},

    {"--nrhs=",            "nrhs",         6,     true,
     "NHRS dimension (number of columns) [default: 1000]"},

    {"--nb=",              "nb",           4,     true,
     "NB size of tile (NB by NB) [default: 256]"},

    {"--ib=",              "ib",           4,     true,
     "IB inner blocking size [default: 64]"},

    {"--alpha=",           "alpha",        14,    true,
     "scalar alpha"},

    {"--beta=",            "beta",         14,    true,
     "scalar beta"},

    {"--pada=",            "padA",         4,     true,
     "padding added to lda [default: 0]"},

    {"--padb=",            "padB",         4,     true,
     "padding added to ldb [default: 0]"},

    {"--padc=",            "padC",         4,     true,
     "padding added to ldc [default: 0]"},

    {"--mtpf=",            "mtpf",         4,     true,
     "maximum number of threads for panel factorization [default: 1]"},

    {"--zerocol=",         "zerocol",      7,     true,
     "if positive, a column of zeros inserted at that index [default: -1]"},

    {"--incx=",            "incx",         4,     true,
     "1 to pivot forward, -1 to pivot backward [default: 1]"},

    { NULL }  // last entry
};

/***************************************************************************//**
 *
 * @brief Tests and times a PLASMA routine.
 *        Prints usage information when ran without options.
 *
 * @param[in] argc
 * @param[in] argv
 *
 * @retval EXIT_SUCCESS - correct invocation
 * @retval EXIT_FAILURE - incorrect invocation
 * @retval > 0 - number of tests that failed
 *
 ******************************************************************************/
int main(int argc, char **argv)
{

    //const char *routine = argv[1];
    const char *routine = "dgesv";

    // Ensure that ParamDesc has an entry for every param_label_t value.
    assert(PARAM_SIZEOF == sizeof(ParamDesc)/sizeof(param_desc_t) - 1);

    param_t param[PARAM_SIZEOF];      // set of parameters
    param_value_t pval[PARAM_SIZEOF]; // snapshot of values

    param_init(param);
    param_read(argc, argv, param);
    int  iter  = param[PARAM_ITER].val[0].i;
    bool outer = param[PARAM_OUTER].val[0].c == 'y';
    bool test  = param[PARAM_TEST].val[0].c == 'y';
    int err = 0;

    // Print labels.
    param_snap(param, pval);
    print_header(routine, pval);

    // Iterate over parameters and run tests
    plasma_init();
        do {
            param_snap(param, pval);
            for (int i = 0; i < iter; i++) {
            err += test_routine(routine, pval, test);
            }
            if (iter > 1) {
                printf("\n");
            }
        }
    while (outer ? param_step_outer(param, 0) : param_step_inner(param));
    plasma_finalize();
    printf("\n");
    return err;
}

/***************************************************************************//**
 *
 * @brief Prints generic usage information.
 *
 ******************************************************************************/
void print_main_usage(const char *program_name)
{
    printf("Usage:\n"
           "\t%s [-h|--help]\n"
           "\t%s [-v|--version]\n"
           "\t%s routine [-h|--help]\n"
           "\t%s routine [parameter1, parameter2, ...]\n"
           "\n"
           "Available routines:", program_name, program_name, program_name);
    for (int i = 0; routines[i].name != NULL; ++i) {
        if (i % 4 == 0) {
            printf("\n\t");
        }
        printf("%-*s ", InfoSpacing, routines[i].name);
    }
    printf("\n");
}

/***************************************************************************//**
 *
 * @brief Prints routine-specific usage information.
 *
 * @param[in] name - routine name
 * @param[in,out] pval - array of parameter values
 *
 ******************************************************************************/
void print_routine_usage(const char *program_name, const char *name, param_value_t pval[])
{
    printf("Usage:\n"
           "\t%s %s [-h|--help]\n"
           "\t%s %s (parameter1, parameter2, ...)\n\n"
           "Options:\n"
           "\t%*sshow this screen\n",
           program_name, name,
           program_name, name,
           DescriptionIndent, "-h --help");
    print_usage(PARAM_ITER);
    print_usage(PARAM_OUTER);
    print_usage(PARAM_DIM_OUTER);
    print_usage(PARAM_TEST);
    print_usage(PARAM_TOL);

    printf("\n"
           "Options below accept multiple values separated by commas\n"
           "(e.g., --nb=32,64) and may be repeated (--nb=32 --nb=64).\n"
           "Numeric options accept start:end:step ranges (--nb=32:128:32).\n"
           "\n");
    run_routine(name, pval, false);
    for (int i = 0; i < PARAM_SIZEOF; ++i) {
        if (pval[i].used) {
            switch (i) {
            // ignore output params
            case PARAM_SUCCESS:
            case PARAM_ERROR:
            case PARAM_ORTHO:
            case PARAM_TIME:
            case PARAM_GFLOPS:
            case PARAM_ITERSV:
                break;

            default:
                print_usage(i);
                break;
            }
        }
    }
}

/***************************************************************************//**
 *
 * @brief Prints usage information for a specific command line option.
 *
 * @param[in] label - command line option label
 *
 ******************************************************************************/
void print_usage(int label)
{
    printf("\t%*s%s\n",
        DescriptionIndent,
        ParamDesc[label].arg,
        ParamDesc[label].help);
}

/***************************************************************************//**
 *
 * @brief Prints column headers.
 *
 * @param[in]     name - routine name
 * @param[in,out] pval - array of parameter values
 *
 ******************************************************************************/
void print_header(const char *name, param_value_t pval[])
{
    run_routine(name, pval, false);

    printf("\n");
    for (int i = 0; i < PARAM_SIZEOF; ++i) {
        if (pval[i].used) {
            switch (i) {
            case PARAM_DIM:
                if (pval[i].used & PARAM_USE_M)
                    printf("  %*s", ParamDesc[i].width, "m");
                if (pval[i].used & PARAM_USE_N)
                    printf("  %*s", ParamDesc[i].width, "n");
                if (pval[i].used & PARAM_USE_K)
                    printf("  %*s", ParamDesc[i].width, "k");
                break;

            default:
                printf("  %*s", ParamDesc[i].width, ParamDesc[i].header);
            }
        }
    }
    printf("\n\n");
}

/***************************************************************************//**
 *
 * @brief Tests a routine for a set of parameter values.
 *        Performs testing and timing.
 *        Runs routine and prints column values.
 *
 * @param[in]     name - routine name
 * @param[in,out] pval - array of parameter values
 * @param[in]    test - if true, tests routine, else only times routine
 *
 * @retval 1 - failure
 * @retval 0 - success
 *
 ******************************************************************************/
int test_routine(const char *name, param_value_t pval[], bool test)
{
    run_routine(name, pval, true);

    for (int i = 0; i < PARAM_SIZEOF; ++i) {
        if (pval[i].used) {
            switch (i) {
            case PARAM_SUCCESS:
                if (test)
                    printf("  %*s", ParamDesc[i].width, pval[i].i ? "pass" : "FAILED");
                else
                    printf("  %*s", ParamDesc[i].width, "--");
                break;

            // errors
            case PARAM_ERROR:
            case PARAM_ORTHO:
                if (test)
                    printf("  %*.2e", ParamDesc[i].width, pval[i].d);
                else
                    printf("  %*s", ParamDesc[i].width, "--");
                break;

            // character parameters
            case PARAM_TRANS:
            case PARAM_TRANSA:
            case PARAM_TRANSB:
            case PARAM_SIDE:
            case PARAM_UPLO:
            case PARAM_DIAG:
            case PARAM_COLROW:
            case PARAM_NORM:
            case PARAM_HMODE:
                printf("  %*c", ParamDesc[i].width, pval[i].c);
                break;

            // dimensions
            case PARAM_DIM:
                if (pval[i].used & PARAM_USE_M)
                    printf("  %*d", ParamDesc[i].width, pval[i].dim.m);
                if (pval[i].used & PARAM_USE_N)
                    printf("  %*d", ParamDesc[i].width, pval[i].dim.n);
                if (pval[i].used & PARAM_USE_K)
                    printf("  %*d", ParamDesc[i].width, pval[i].dim.k);
                break;

            // integer parameters
            case PARAM_KL:
            case PARAM_KU:
            case PARAM_NRHS:
            case PARAM_NB:
            case PARAM_IB:
            case PARAM_PADA:
            case PARAM_PADB:
            case PARAM_PADC:
            case PARAM_MTPF:
            case PARAM_ZEROCOL:
            case PARAM_INCX:
            case PARAM_ITERSV:
                printf("  %*d", ParamDesc[i].width, pval[i].i);
                break;

            // double parameters
            case PARAM_TIME:
            case PARAM_GFLOPS:
                printf("  %*.4f", ParamDesc[i].width, pval[i].d);
                break;

            // complex parameters
            case PARAM_ALPHA:
            case PARAM_BETA:
                assert(ParamDesc[i].width == 14);
                printf("  %5.2f + %5.2fi",
                       creal(pval[i].z), cimag(pval[i].z));
                break;

            default:
                fprintf(stderr, "\nunknown column %d\n", i);
                assert(false);
                break;
            }
        }
    }
    printf("\n");
    return (pval[PARAM_SUCCESS].i == 0);
}

/***************************************************************************//**
 *
 * @brief Invokes a specific routine.
 *
 * @param[in]     name - routine name
 * @param[in,out] pval - array of parameter values
 * @param[in]     run  - whether to run routine or just mark used parameters
 *
 ******************************************************************************/
void run_routine(const char *name, param_value_t pval[], bool run)
{
    pval[PARAM_SUCCESS].used = true;
    pval[PARAM_ERROR  ].used = true;
    pval[PARAM_TIME   ].used = true;
    pval[PARAM_GFLOPS ].used = true;

    bool found = false;
    for (int i = 0; routines[i].name != NULL; ++i) {
        if (strcmp(name, routines[i].name) == 0) {
            routines[i].func(pval, run);
            found = true;
            break;
        }
    }
    if (! found) {
        printf("unknown routine: %s\n", name);
        exit(EXIT_FAILURE);
    }
}

/***************************************************************************//**
 *
 * @brief Creates an empty array of parameter iterators.
 *
 * @param[out] param - array of parameter iterators.
 *
 ******************************************************************************/
void param_init(param_t param[])
{
    for (int i = 0; i < PARAM_SIZEOF; i++) {
        param[i].is_list = ParamDesc[i].is_list;
        param[i].num = 0;
        param[i].pos = 0;
        param[i].val =
            (param_value_t*)malloc(InitValArraySize*sizeof(param_value_t));
        assert(param[i].val != NULL);
        param[i].val->used = false;
        param[i].size = InitValArraySize;
    }
}

/***************************************************************************//**
 *
 * @brief Initializes an array of parameter iterators
 *        according to command lineoptions.
 *        Assumes argv[1] is function name; parses argv[2:argc-1].
 *
 * @param[in]    argc
 * @param[in]    argv
 * @param[in,out] param - array of parameter iterators
 *
 ******************************************************************************/
void param_read(int argc, char **argv, param_t param[])
{
    int err = 0;
    const char *routine = argv[1];

    //================================================================
    // Set default values for singleton parameters before scanning.
    //================================================================
    param_add_int(1, &param[PARAM_ITER]);
    param_add_char('y', &param[PARAM_OUTER]);
    param_add_char('n', &param[PARAM_DIM_OUTER]);
    param_add_char('y', &param[PARAM_TEST]);
    param_add_double(50.0, &param[PARAM_TOL]);

    //================================================================
    // Initialize parameters from the command line.
    //================================================================
    for (int i = 2; i < argc && argv[i]; i++) {
        //--------------------------------------------------
        // Scan character parameters.
        //--------------------------------------------------
        if (param_starts_with(argv[i], "--outer="))
            err = param_scan_char(strchr(argv[i], '=')+1, &param[PARAM_OUTER]);
        else if (param_starts_with(argv[i], "--dim-outer="))
            err = param_scan_char(strchr(argv[i], '=')+1, &param[PARAM_DIM_OUTER]);
        else if (param_starts_with(argv[i], "--test="))
            err = param_scan_char(strchr(argv[i], '=')+1, &param[PARAM_TEST]);

        else if (param_starts_with(argv[i], "--side="))
            err = param_scan_char(strchr(argv[i], '=')+1, &param[PARAM_SIDE]);

        else if (param_starts_with(argv[i], "--trans="))
            err = param_scan_char(strchr(argv[i], '=')+1, &param[PARAM_TRANS]);
        else if (param_starts_with(argv[i], "--transa="))
            err = param_scan_char(strchr(argv[i], '=')+1, &param[PARAM_TRANSA]);
        else if (param_starts_with(argv[i], "--transb="))
            err = param_scan_char(strchr(argv[i], '=')+1, &param[PARAM_TRANSB]);

        else if (param_starts_with(argv[i], "--uplo="))
            err = param_scan_char(strchr(argv[i], '=')+1, &param[PARAM_UPLO]);

        else if (param_starts_with(argv[i], "--diag="))
            err = param_scan_char(strchr(argv[i], '=')+1, &param[PARAM_DIAG]);

        else if (param_starts_with(argv[i], "--colrow="))
            err = param_scan_char(strchr(argv[i], '=')+1, &param[PARAM_COLROW]);

        else if (param_starts_with(argv[i], "--norm="))
            err = param_scan_char(strchr(argv[i], '=')+1, &param[PARAM_NORM]);

        else if (param_starts_with(argv[i], "--hmode="))
            err = param_scan_char(strchr(argv[i], '=')+1, &param[PARAM_HMODE]);

        //--------------------------------------------------
        // Scan integer parameters.
        //--------------------------------------------------
        else if (param_starts_with(argv[i], "--iter="))
            err = param_scan_int(strchr(argv[i], '=')+1, &param[PARAM_ITER]);

        else if (param_starts_with(argv[i], "--dim=")) {
            bool outer = param[PARAM_DIM_OUTER].val[0].c == 'y';
            err = param_scan_int3(strchr(argv[i], '=')+1, &param[PARAM_DIM],
                                  outer);
        }

        else if (param_starts_with(argv[i], "--kl="))
            err = param_scan_int(strchr(argv[i], '=')+1, &param[PARAM_KL]);
        else if (param_starts_with(argv[i], "--ku="))
            err = param_scan_int(strchr(argv[i], '=')+1, &param[PARAM_KU]);
        else if (param_starts_with(argv[i], "--nrhs="))
            err = param_scan_int(strchr(argv[i], '=')+1, &param[PARAM_NRHS]);

        else if (param_starts_with(argv[i], "--nb="))
            err = param_scan_int(strchr(argv[i], '=')+1, &param[PARAM_NB]);
        else if (param_starts_with(argv[i], "--ib="))
            err = param_scan_int(strchr(argv[i], '=')+1, &param[PARAM_IB]);

        else if (param_starts_with(argv[i], "--pada="))
            err = param_scan_int(strchr(argv[i], '=')+1, &param[PARAM_PADA]);
        else if (param_starts_with(argv[i], "--padb="))
            err = param_scan_int(strchr(argv[i], '=')+1, &param[PARAM_PADB]);
        else if (param_starts_with(argv[i], "--padc="))
            err = param_scan_int(strchr(argv[i], '=')+1, &param[PARAM_PADC]);

        else if (param_starts_with(argv[i], "--mtpf="))
            err = param_scan_int(strchr(argv[i], '=')+1, &param[PARAM_MTPF]);
        else if (param_starts_with(argv[i], "--zerocol="))
            err = param_scan_int(strchr(argv[i], '=')+1, &param[PARAM_ZEROCOL]);
        else if (param_starts_with(argv[i], "--incx="))
            err = param_scan_int(strchr(argv[i], '=')+1, &param[PARAM_INCX]);

        //--------------------------------------------------
        // Scan double precision parameters.
        //--------------------------------------------------
        else if (param_starts_with(argv[i], "--tol="))
            err = param_scan_double(strchr(argv[i], '=')+1, &param[PARAM_TOL]);

        //--------------------------------------------------
        // Scan complex parameters.
        //--------------------------------------------------
        else if (param_starts_with(argv[i], "--alpha="))
            err = param_scan_complex(strchr(argv[i], '=')+1,
                                     &param[PARAM_ALPHA]);
        else if (param_starts_with(argv[i], "--beta="))
            err = param_scan_complex(strchr(argv[i], '=')+1,
                                     &param[PARAM_BETA]);

        //--------------------------------------------------
        // Handle help and errors.
        //--------------------------------------------------
        else if (strcmp(argv[i], "-h") == 0 ||
                 strcmp(argv[i], "--help") == 0) {
            param_value_t pval[PARAM_SIZEOF]; // snapshot of values
            param_snap(param, pval);
            print_routine_usage(argv[0], routine, pval);
            exit(EXIT_SUCCESS);
        }
        else {
            printf("unknown argument: %s\n", argv[i]);
            exit(EXIT_FAILURE);
        }

        if (err) {
            printf("error scanning argument: %s\n", argv[i]);
            exit(EXIT_FAILURE);
        }
    }

    //================================================================
    // Set default values for uninitialized list parameters.
    //================================================================

    //--------------------------------------------------
    // Set character parameters.
    //--------------------------------------------------
    if (param[PARAM_SIDE].num == 0)
        param_add_char('l', &param[PARAM_SIDE]);
    if (param[PARAM_TRANS].num == 0)
        param_add_char('n', &param[PARAM_TRANS]);
    if (param[PARAM_TRANSA].num == 0)
        param_add_char('n', &param[PARAM_TRANSA]);
    if (param[PARAM_TRANSB].num == 0)
        param_add_char('n', &param[PARAM_TRANSB]);
    if (param[PARAM_UPLO].num == 0)
        param_add_char('l', &param[PARAM_UPLO]);
    if (param[PARAM_DIAG].num == 0)
        param_add_char('n', &param[PARAM_DIAG]);
    if (param[PARAM_COLROW].num == 0)
        param_add_char('c', &param[PARAM_COLROW]);
    if (param[PARAM_NORM].num == 0)
        param_add_char('o', &param[PARAM_NORM]);
    if (param[PARAM_HMODE].num == 0)
        param_add_char('f', &param[PARAM_HMODE]);

    //--------------------------------------------------
    // Set integer parameters.
    //--------------------------------------------------
    if (param[PARAM_DIM].num == 0) {
        int3_t dim = { 1000, 1000, 1000 };
        param_add_int3(dim, &param[PARAM_DIM]);
    }
    if (param[PARAM_KL].num == 0)
        param_add_int(200, &param[PARAM_KL]);
    if (param[PARAM_KU].num == 0)
        param_add_int(200, &param[PARAM_KU]);
    if (param[PARAM_NRHS].num == 0)
        param_add_int(1000, &param[PARAM_NRHS]);

    if (param[PARAM_NB].num == 0)
        param_add_int(256, &param[PARAM_NB]);
    if (param[PARAM_IB].num == 0)
        param_add_int(64, &param[PARAM_IB]);

    if (param[PARAM_PADA].num == 0)
        param_add_int(0, &param[PARAM_PADA]);
    if (param[PARAM_PADB].num == 0)
        param_add_int(0, &param[PARAM_PADB]);
    if (param[PARAM_PADC].num == 0)
        param_add_int(0, &param[PARAM_PADC]);

    if (param[PARAM_MTPF].num == 0)
        param_add_int(1, &param[PARAM_MTPF]);
    if (param[PARAM_ZEROCOL].num == 0)
        param_add_int(-1, &param[PARAM_ZEROCOL]);
    if (param[PARAM_INCX].num == 0)
        param_add_int(1, &param[PARAM_INCX]);

    //--------------------------------------------------
    // Set double precision parameters.
    //--------------------------------------------------

    //--------------------------------------------------
    // Set complex parameters.
    //--------------------------------------------------
    if (param[PARAM_ALPHA].num == 0) {
        plasma_complex64_t z = 1.2345 + 2.3456*_Complex_I;
        param_add_complex(z, &param[PARAM_ALPHA]);
    }
    if (param[PARAM_BETA].num == 0) {
        plasma_complex64_t z = 6.7890 + 7.8901*_Complex_I;
        param_add_complex(z, &param[PARAM_BETA]);
    }
}

/***************************************************************************//**
 *
 * @brief Checks if a string starts with a specific prefix.
 *
 * @param[in] str
 * @param[in] prefix
 *
 * @retval 1 - match
 * @retval 0 - no match
 *
 ******************************************************************************/
int param_starts_with(const char *str, const char *prefix)
{
    size_t n = strlen(prefix);
    if (strncmp(str, prefix, n))
        return 0;
    return 1;
}

/***************************************************************************//**
 *
 * @brief Scans string for single integer or range of integers (start:end:step).
 *        Advances the string to after the range or number.
 *
 * @param[in,out] strp - pointer to string containing an integer or range.
 *                       On output, advanced to after the number or range.
 * @param[out] start - start of range
 * @param[out] end   - end of range
 * @param[out] step  - step size; 0 if start = end
 *
 * @retval 1 - failure
 * @retval 0 - success
 *
 ******************************************************************************/
int scan_irange(const char **strp, int *start, int *end, int *step)
{
    int bytes1, bytes2, bytes3, cnt;
    cnt = sscanf(*strp, "%d %n: %d %n: %d %n",
                 start, &bytes1, end, &bytes2, step, &bytes3);
    if (cnt == 3) {
        if (*start == *end)
            *step = 0;
        *strp += bytes3;
        return ! ((*step == 0 && *start == *end) ||
                  (*step >  0 && *start <  *end) ||
                  (*step <  0 && *start >  *end));
    }
    else if (cnt == 2) {
        *strp += bytes2;
        if (*start == *end)
            *step = 0;
        else
            *step = 1;
        return ! (*start <= *end);
    }
    else if (cnt == 1) {
        *strp += bytes1;
        *end  = *start;
        *step = 0;
        return 0;
    }
    else {
        return 1;
    }
}

/***************************************************************************//**
 *
 * @brief Scans string for single double or range of doubles (start:end:step).
 *        Advances the string to after the range or number.
 *
 * @param[in,out] strp - pointer to string containing a double or range.
 *                       On output, advanced to after the number or range.
 * @param[out] start - start of range
 * @param[out] end   - end of range
 * @param[out] step  - step size; 0 if start = end
 *
 * @retval 1 - failure
 * @retval 0 - success
 *
 ******************************************************************************/
int scan_drange(const char **strp, double *start, double *end, double *step)
{
    int bytes1, bytes2, bytes3, cnt;
    cnt = sscanf(*strp, "%lf %n: %lf %n: %lf %n",
                 start, &bytes1, end, &bytes2, step, &bytes3);
    if (cnt == 3) {
        if (*start == *end)
            *step = 0;
        *strp += bytes3;
        return ! ((*step == 0 && *start == *end) ||
                  (*step >  0 && *start <  *end) ||
                  (*step <  0 && *start >  *end));
    }
    else if (cnt == 2) {
        *strp += bytes2;
        if (*start == *end)
            *step = 0;
        else
            *step = 1;
        return ! (*start <= *end);
    }
    else if (cnt == 1) {
        *strp += bytes1;
        *end  = *start;
        *step = 0;
        return 0;
    }
    else {
        return 1;
    }
}

/***************************************************************************//**
 *
 * @brief Scans a list of integers or ranges (start:end:step).
 *        Adds the value(s) to a parameter iterator.
 *
 * @param[in]     str   - string containin an integer
 * @param[in,out] param - parameter iterator
 *
 * @retval 1 - failure
 * @retval 0 - success
 *
 ******************************************************************************/
int param_scan_int(const char *str, param_t *param)
{
    int start, end, step;
    while (true) {
        if (scan_irange(&str, &start, &end, &step) != 0)
            return 1;
        if (start == end) {
            param_add_int(start, param);
        }
        else {
            for (int i = start; i <= end; i += step) {
                param_add_int(i, param);
            }
        }
        if (*str == '\0')
            break;
        str += 1;
    }
    return 0;
}

/***************************************************************************//**
 *
 * @brief Scans a list of integers or ranges (start:end:step).
 *        Adds the value(s) to a parameter iterator.
 *
 * @param[in]     str   - string containin an integer
 * @param[in,out] param - parameter iterator
 * @param[in]     outer - if true, uses outer product iteration of M x N x K;
 *                        else uses inner product iteration
 *
 * @retval 1 - failure
 * @retval 0 - success
 *
 ******************************************************************************/
int param_scan_int3(const char *str, param_t *param, bool outer)
{
    int m_start, m_end, m_step;
    int n_start, n_end, n_step;
    int k_start, k_end, k_step;
    int len;
    while (true) {
        // scan M
        if (scan_irange(&str, &m_start, &m_end, &m_step) != 0)
            return 1;

        // if "x", scan N; else K = N = M
        len = 0;
        sscanf(str, " x %n", &len);
        if (len > 0) {
            str += len;
            if (scan_irange(&str, &n_start, &n_end, &n_step) != 0)
                return 1;

            // if "x", scan K; else K = N
            len = 0;
            sscanf(str, " x %n", &len);
            if (len > 0) {
                str += len;
                if (scan_irange(&str, &k_start, &k_end, &k_step) != 0)
                    return 1;
            }
            else {
                k_start = n_start;
                k_end   = n_end;
                k_step  = n_step;
            }
        }
        else {
            k_start = n_start = m_start;
            k_end   = n_end   = m_end;
            k_step  = n_step  = m_step;
        }

        if (m_start == m_end && n_start == n_end && k_start == k_end) {
            // single size
            int3_t dim = { m_start, n_start, k_start };
            param_add_int3(dim, param);
        }
        else if (outer) {
            // outer product of M x N x K
            // require non-zero step
            if (m_step == 0) m_step = 1;
            if (n_step == 0) n_step = 1;
            if (k_step == 0) k_step = 1;
            for (int m = m_start;
                 (m_step >= 0 ? m <= m_end : m >= m_end);
                 m += m_step) {

                for (int n = n_start;
                     (n_step >= 0 ? n <= n_end : n >= n_end);
                     n += n_step) {

                    for (int k = k_start;
                         (k_step >= 0 ? k <= k_end : k >= k_end);
                         k += k_step)
                    {
                        int3_t dim = { m, n, k };
                        param_add_int3(dim, param);
                    }
                }
            }
        }
        else {
            // inner product of M x N x K
            // at least one of the variables must advance
            assert(m_step != 0 || n_step != 0 || k_step != 0);
            for (int m = m_start,
                     n = n_start,
                     k = k_start;

                 (m_step >= 0 ? m <= m_end : m >= m_end) &&
                 (n_step >= 0 ? n <= n_end : n >= n_end) &&
                 (k_step >= 0 ? k <= k_end : k >= k_end);

                 m += m_step,
                 n += n_step,
                 k += k_step)
            {
                int3_t dim = { m, n, k };
                param_add_int3(dim, param);
            }
        }
        if (*str == '\0')
            break;
        str += 1;
    }
    return 0;
}

/***************************************************************************//**
 *
 * @brief Scans a list of characters.
 *        Adds the value(s) to a parameter iterator.
 *
 * @param[in]     str   - string containing a single character
 * @param[in,out] param - parameter iterator
 *
 * @retval 1 - failure
 * @retval 0 - success
 *
 ******************************************************************************/
int param_scan_char(const char *str, param_t *param)
{
    const char *endptr;
    do {
        if (*str == '\0') {
            return 1;
        }
        param_add_char(*str, param);
        endptr = str+1;
        str = endptr+1;
    }
    while (*endptr != '\0');
    return 0;
}

/***************************************************************************//**
 *
 * @brief Scans a list of double precision numbers or ranges (start:end:step).
 *        Adds the value(s) to a parameter iterator.
 *
 * @param[in]     str   - string containing a double precision number
 * @param[in,out] param - parameter iterator
 *
 * @retval 1 - failure
 * @retval 0 - success
 *
 ******************************************************************************/
int param_scan_double(const char *str, param_t *param)
{
    double start, end, step;
    while (true) {
        if (scan_drange(&str, &start, &end, &step) != 0)
            return 1;
        if (start == end) {
            param_add_double(start, param);
        }
        else {
            for (double d = start; d <= end + step/10.; d += step) {
                param_add_double(d, param);
            }
        }
        if (*str == '\0')
            break;
        str += 1;
    }
    return 0;
}

/***************************************************************************//**
 *
 * @brief Scans a list of complex numbers in format: 1.23 or 1.23+2.45i.
 *        Adds the value to a parameter iterator. No ranges.
 *
 * @param[in]     str   - string containing a double precision number
 * @param[in,out] param - parameter iterator
 *
 * @retval 1 - failure
 * @retval 0 - success
 *
 ******************************************************************************/
int param_scan_complex(const char *str, param_t *param)
{
    char *endptr;
    do {
        double re = strtod(str, &endptr);
        double im = 0.0;
        if (endptr == str) {
            return 1;
        }
        if (*endptr == '+') {
            str = endptr+1;
            im = strtod(str, &endptr);
            if (endptr == str || *endptr != 'i') {
                return 1;
            }
            endptr += 1;  // skip 'i'
        }
        plasma_complex64_t z = re + im*_Complex_I;
        param_add_complex(z, param);
        str = endptr+1;
    }
    while (*endptr != '\0');
    return 0;
}

/***************************************************************************//**
 *
 * @brief If parameter is a list, increments number of elements in a parameter
 *        iterator, dynamically growing its storage as needed.
 *        Also clears parameter's used flag.
 *
 * @param[in,out] param - parameter iterator
 *
 ******************************************************************************/
void param_add(param_t *param)
{
    param->val[param->num].used = false;
    if (param->is_list) {
        param->num++;
        if (param->num == param->size) {
            param->size *= 2;
            param->val = (param_value_t*) realloc(
                param->val, param->size*sizeof(param_value_t));
            assert(param->val != NULL);
        }
    }
}

/***************************************************************************//**
 *
 * @brief Adds an integer to a parameter iterator.
 *
 * @param[in]    ival  - integer
 * @param[in,out] param - parameter iterator
 *
 ******************************************************************************/
void param_add_int(int ival, param_t *param)
{
    param->val[param->num].i = ival;
    param_add(param);
}

/***************************************************************************//**
 *
 * @brief Adds an integer to a parameter iterator.
 *
 * @param[in]     ival  - integer
 * @param[in,out] param - parameter iterator
 *
 ******************************************************************************/
void param_add_int3(int3_t ival, param_t *param)
{
    param->val[param->num].dim = ival;
    param_add(param);
}

/***************************************************************************//**
 *
 * @brief Adds a character to a parameter iterator.
 *
 * @param[in]     cval  - character
 * @param[in,out] param - parameter iterator
 *
 ******************************************************************************/
void param_add_char(char cval, param_t *param)
{
    param->val[param->num].c = cval;
    param_add(param);
}

/***************************************************************************//**
 *
 * @brief Adds a double precision number to a parameter iterator.
 *
 * @param[in]     dval  - double precision value
 * @param[in,out] param - parameter iterator
 *
 ******************************************************************************/
void param_add_double(double dval, param_t *param)
{
    param->val[param->num].d = dval;
    param_add(param);
}

/***************************************************************************//**
 *
 * @brief Adds a complex number to a parameter iterator.
 *
 * @param[in]     zval  - complex value
 * @param[in,out] param - parameter iterator
 *
 ******************************************************************************/
void param_add_complex(plasma_complex64_t zval, param_t *param)
{
    param->val[param->num].z = zval;
    param_add(param);
}

/***************************************************************************//**
 *
 * @brief Steps through an array of parameter iterators
 *        (inner product evaluation).
 *        Advances all iterators at the same time.
 *        Iterators that exhausted their range return the last value.
 *
 * @param[in,out] param - array of parameter iterators
 *
 * @retval 1 - more iterations
 * @retval 0 - no more iterations
 *
 ******************************************************************************/
int param_step_inner(param_t param[])
{
    int finished = 1;
    for (int i = 0; i < PARAM_SIZEOF; i++) {
        if (param[i].pos < param[i].num-1) {
            param[i].pos++;
            finished = 0;
        }
    }
    return ! finished;
}

/***************************************************************************//**
 *
 * @brief Steps through an array of parameter iterators
 *        (outer product evaluation).
 *        Advances one iterator at a time.
 *
 * @param[in,out] param - array of parameter iterators
 *
 * @retval 1 - more iterations
 * @retval 0 - no more iterations
 *
 ******************************************************************************/
int param_step_outer(param_t param[], int idx)
{
    // reverse index to iterate right-to-left (e.g., ib, then nb, k, n, m).
    int ridx = PARAM_SIZEOF - 1 - idx;
    while (ridx >= 0 && param[ridx].num == 0)
        --ridx;
    if (ridx < 0)
        return 0;

    if (++param[ridx].pos == param[ridx].num) {
        param[ridx].pos = 0;
        return param_step_outer(param, idx+1);
    }
    return 1;
}

/***************************************************************************//**
 *
 * @brief Copies a snapshot of the current iteration of param iterators to pval.
 *
 * @param[in]  param - array of parameter iterators
 * @param[out] pval  - array of parameter values
 *
 ******************************************************************************/
int param_snap(param_t param[], param_value_t pval[])
{
    for (int i = 0; i < PARAM_SIZEOF; i++) {
        pval[i] = param[i].val[param[i].pos];
    }
    return 0;
}
