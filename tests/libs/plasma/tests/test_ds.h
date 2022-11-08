/**
 *
 * @file
 *
 *  PLASMA is a software package provided by:
 *  University of Tennessee, US,
 *  University of Manchester, UK.
 *
 * @generated from test/test_zc.h, mixed zc -> ds, Sun Aug 29 23:28:23 2021
 *
 **/
#ifndef TEST_DS_H
#define TEST_DS_H

#include "test.h"

//==============================================================================
// test routines
//==============================================================================
void test_dsgesv(param_value_t param[], bool run);
void test_dsposv(param_value_t param[], bool run);
void test_dsgbsv(param_value_t param[], bool run);
void test_dlag2s(param_value_t param[], bool run);
void test_slag2d(param_value_t param[], bool run);

#endif // TEST_DS_H
