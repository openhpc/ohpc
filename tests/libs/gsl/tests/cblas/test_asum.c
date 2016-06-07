#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_asum (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int N = 1;
   float X[] = { 0.239f };
   int incX = -1;
   float expected = 0.0f;
   float f;
   f = cblas_sasum(N, X, incX);
   gsl_test_rel(f, expected, flteps, "sasum(case 40)");
  };


  {
   int N = 1;
   double X[] = { -0.413 };
   int incX = -1;
   double expected = 0;
   double f;
   f = cblas_dasum(N, X, incX);
   gsl_test_rel(f, expected, dbleps, "dasum(case 41)");
  };


  {
   int N = 1;
   float X[] = { 0.1f, 0.017f };
   int incX = -1;
   float expected = 0.0f;
   float f;
   f = cblas_scasum(N, X, incX);
   gsl_test_rel(f, expected, flteps, "scasum(case 42)");
  };


  {
   int N = 1;
   double X[] = { -0.651, 0.079 };
   int incX = -1;
   double expected = 0;
   double f;
   f = cblas_dzasum(N, X, incX);
   gsl_test_rel(f, expected, dbleps, "dzasum(case 43)");
  };


  {
   int N = 2;
   float X[] = { 0.899f, -0.72f };
   int incX = 1;
   float expected = 1.619f;
   float f;
   f = cblas_sasum(N, X, incX);
   gsl_test_rel(f, expected, flteps, "sasum(case 44)");
  };


  {
   int N = 2;
   double X[] = { 0.271, -0.012 };
   int incX = 1;
   double expected = 0.283;
   double f;
   f = cblas_dasum(N, X, incX);
   gsl_test_rel(f, expected, dbleps, "dasum(case 45)");
  };


  {
   int N = 2;
   float X[] = { -0.567f, -0.645f, 0.098f, 0.256f };
   int incX = 1;
   float expected = 1.566f;
   float f;
   f = cblas_scasum(N, X, incX);
   gsl_test_rel(f, expected, flteps, "scasum(case 46)");
  };


  {
   int N = 2;
   double X[] = { -0.046, -0.671, -0.323, 0.785 };
   int incX = 1;
   double expected = 1.825;
   double f;
   f = cblas_dzasum(N, X, incX);
   gsl_test_rel(f, expected, dbleps, "dzasum(case 47)");
  };


  {
   int N = 2;
   float X[] = { 0.169f, 0.833f };
   int incX = -1;
   float expected = 0.0f;
   float f;
   f = cblas_sasum(N, X, incX);
   gsl_test_rel(f, expected, flteps, "sasum(case 48)");
  };


  {
   int N = 2;
   double X[] = { -0.586, -0.486 };
   int incX = -1;
   double expected = 0;
   double f;
   f = cblas_dasum(N, X, incX);
   gsl_test_rel(f, expected, dbleps, "dasum(case 49)");
  };


  {
   int N = 2;
   float X[] = { -0.314f, -0.318f, -0.835f, -0.807f };
   int incX = -1;
   float expected = 0.0f;
   float f;
   f = cblas_scasum(N, X, incX);
   gsl_test_rel(f, expected, flteps, "scasum(case 50)");
  };


  {
   int N = 2;
   double X[] = { -0.927, 0.152, -0.554, -0.844 };
   int incX = -1;
   double expected = 0;
   double f;
   f = cblas_dzasum(N, X, incX);
   gsl_test_rel(f, expected, dbleps, "dzasum(case 51)");
  };


}
