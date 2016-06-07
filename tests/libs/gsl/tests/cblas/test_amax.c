#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_amax (void) {
  {
   int N = 1;
   float X[] = { -0.388f };
   int incX = -1;
   int expected = 0;
   int k;
   k = cblas_isamax(N, X, incX);
   gsl_test_int(k, expected, "samax(case 52)");
  };


  {
   int N = 1;
   double X[] = { 0.247 };
   int incX = -1;
   int expected = 0;
   int k;
   k = cblas_idamax(N, X, incX);
   gsl_test_int(k, expected, "damax(case 53)");
  };


  {
   int N = 1;
   float X[] = { 0.704f, 0.665f };
   int incX = -1;
   int expected = 0;
   int k;
   k = cblas_icamax(N, X, incX);
   gsl_test_int(k, expected, "camax(case 54)");
  };


  {
   int N = 1;
   double X[] = { -0.599, -0.758 };
   int incX = -1;
   int expected = 0;
   int k;
   k = cblas_izamax(N, X, incX);
   gsl_test_int(k, expected, "zamax(case 55)");
  };


  {
   int N = 2;
   float X[] = { 0.909f, 0.037f };
   int incX = 1;
   int expected = 0;
   int k;
   k = cblas_isamax(N, X, incX);
   gsl_test_int(k, expected, "samax(case 56)");
  };


  {
   int N = 2;
   double X[] = { 0.271, -0.426 };
   int incX = 1;
   int expected = 1;
   int k;
   k = cblas_idamax(N, X, incX);
   gsl_test_int(k, expected, "damax(case 57)");
  };


  {
   int N = 2;
   float X[] = { -0.648f, 0.317f, 0.62f, 0.392f };
   int incX = 1;
   int expected = 1;
   int k;
   k = cblas_icamax(N, X, incX);
   gsl_test_int(k, expected, "camax(case 58)");
  };


  {
   int N = 2;
   double X[] = { -0.789, 0.352, 0.562, 0.697 };
   int incX = 1;
   int expected = 1;
   int k;
   k = cblas_izamax(N, X, incX);
   gsl_test_int(k, expected, "zamax(case 59)");
  };


  {
   int N = 2;
   float X[] = { 0.487f, 0.918f };
   int incX = -1;
   int expected = 0;
   int k;
   k = cblas_isamax(N, X, incX);
   gsl_test_int(k, expected, "samax(case 60)");
  };


  {
   int N = 2;
   double X[] = { 0.537, 0.826 };
   int incX = -1;
   int expected = 0;
   int k;
   k = cblas_idamax(N, X, incX);
   gsl_test_int(k, expected, "damax(case 61)");
  };


  {
   int N = 2;
   float X[] = { 0.993f, 0.172f, -0.825f, 0.873f };
   int incX = -1;
   int expected = 0;
   int k;
   k = cblas_icamax(N, X, incX);
   gsl_test_int(k, expected, "camax(case 62)");
  };


  {
   int N = 2;
   double X[] = { 0.913, -0.436, -0.134, 0.129 };
   int incX = -1;
   int expected = 0;
   int k;
   k = cblas_izamax(N, X, incX);
   gsl_test_int(k, expected, "zamax(case 63)");
  };


}
