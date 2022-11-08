/**
 *
 * @file
 *
 *  PLASMA is a software package provided by:
 *  University of Tennessee, US,
 *  University of Manchester, UK.
 *
 * @generated from test/test_z.h, normal z -> c, Sun Aug 29 23:28:55 2021
 *
 **/
#ifndef TEST_C_H
#define TEST_C_H

#include "test.h"

//==============================================================================
// test routines
//==============================================================================
void test_scamax(param_value_t param[], bool run);
void test_cgbsv(param_value_t param[], bool run);
void test_cgbtrf(param_value_t param[], bool run);
void test_cgeadd(param_value_t param[], bool run);
void test_cgeinv(param_value_t param[], bool run);
void test_cgelqf(param_value_t param[], bool run);
void test_cgelqs(param_value_t param[], bool run);
void test_cgels(param_value_t param[], bool run);
void test_cgemm(param_value_t param[], bool run);
void test_cgeqrf(param_value_t param[], bool run);
void test_cgeqrs(param_value_t param[], bool run);
void test_cgesv(param_value_t param[], bool run);
void test_cgetrf(param_value_t param[], bool run);
void test_cgetri(param_value_t param[], bool run);
void test_cgetri_aux(param_value_t param[], bool run);
void test_cgetrs(param_value_t param[], bool run);
void test_chemm(param_value_t param[], bool run);
void test_cher2k(param_value_t param[], bool run);
void test_cherk(param_value_t param[], bool run);
void test_chetrf(param_value_t param[], bool run);
void test_chesv(param_value_t param[], bool run);
void test_clacpy(param_value_t param[], bool run);
void test_clag2z(param_value_t param[], bool run);
void test_clange(param_value_t param[], bool run);
void test_clangb(param_value_t param[], bool run);
void test_clanhe(param_value_t param[], bool run);
void test_clansy(param_value_t param[], bool run);
void test_clantr(param_value_t param[], bool run);
void test_clascl(param_value_t param[], bool run);
void test_claset(param_value_t param[], bool run);
void test_cgeswp(param_value_t param[], bool run);
void test_clauum(param_value_t param[], bool run);
void test_cpbsv(param_value_t param[], bool run);
void test_cpbtrf(param_value_t param[], bool run);
void test_cpoinv(param_value_t param[], bool run);
void test_cposv(param_value_t param[], bool run);
void test_cpotrf(param_value_t param[], bool run);
void test_cpotri(param_value_t param[], bool run);
void test_cpotrs(param_value_t param[], bool run);
void test_csymm(param_value_t param[], bool run);
void test_csyr2k(param_value_t param[], bool run);
void test_csyrk(param_value_t param[], bool run);
void test_ctradd(param_value_t param[], bool run);
void test_ctrmm(param_value_t param[], bool run);
void test_ctrsm(param_value_t param[], bool run);
void test_ctrtri(param_value_t param[], bool run);
void test_cunmlq(param_value_t param[], bool run);
void test_cunmqr(param_value_t param[], bool run);

#endif // TEST_C_H
