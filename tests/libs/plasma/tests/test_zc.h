/**
 *
 * @file
 *
 *  PLASMA is a software package provided by:
 *  University of Tennessee, US,
 *  University of Manchester, UK.
 *
 * @precisions mixed zc -> ds
 *
 **/
#ifndef TEST_ZC_H
#define TEST_ZC_H

#include "test.h"

//==============================================================================
// test routines
//==============================================================================
void test_zcgesv(param_value_t param[], bool run);
void test_zcposv(param_value_t param[], bool run);
void test_zcgbsv(param_value_t param[], bool run);
void test_zlag2c(param_value_t param[], bool run);
void test_clag2z(param_value_t param[], bool run);

#endif // TEST_ZC_H
