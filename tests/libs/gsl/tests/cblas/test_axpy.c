#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_axpy (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int N = 1;
   float alpha = 0.0f;
   float X[] = { 0.018f };
   int incX = 1;
   float Y[] = { -0.417f };
   int incY = -1;
   float expected[] = { -0.417f };
   cblas_saxpy(N, alpha, X, incX, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], expected[i], flteps, "saxpy(case 64)");
     }
   };
  };


  {
   int N = 1;
   double alpha = 0;
   double X[] = { 0.071 };
   int incX = 1;
   double Y[] = { -0.888 };
   int incY = -1;
   double expected[] = { -0.888 };
   cblas_daxpy(N, alpha, X, incX, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], expected[i], dbleps, "daxpy(case 65)");
     }
   };
  };


  {
   int N = 1;
   float alpha[2] = {1.0f, 0.0f};
   float X[] = { -0.542f, -0.362f };
   int incX = 1;
   float Y[] = { -0.459f, -0.433f };
   int incY = -1;
   float expected[] = { -1.001f, -0.795f };
   cblas_caxpy(N, alpha, X, incX, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[2*i], expected[2*i], flteps, "caxpy(case 66) real");
       gsl_test_rel(Y[2*i+1], expected[2*i+1], flteps, "caxpy(case 66) imag");
     };
   };
  };


  {
   int N = 1;
   double alpha[2] = {-1, 0};
   double X[] = { 0.003, -0.514 };
   int incX = 1;
   double Y[] = { -0.529, 0.743 };
   int incY = -1;
   double expected[] = { -0.532, 1.257 };
   cblas_zaxpy(N, alpha, X, incX, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[2*i], expected[2*i], dbleps, "zaxpy(case 67) real");
       gsl_test_rel(Y[2*i+1], expected[2*i+1], dbleps, "zaxpy(case 67) imag");
     };
   };
  };


  {
   int N = 1;
   float alpha = 0.1f;
   float X[] = { 0.771f };
   int incX = -1;
   float Y[] = { 0.507f };
   int incY = 1;
   float expected[] = { 0.5841f };
   cblas_saxpy(N, alpha, X, incX, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], expected[i], flteps, "saxpy(case 68)");
     }
   };
  };


  {
   int N = 1;
   double alpha = -0.3;
   double X[] = { 0.029 };
   int incX = -1;
   double Y[] = { -0.992 };
   int incY = 1;
   double expected[] = { -1.0007 };
   cblas_daxpy(N, alpha, X, incX, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], expected[i], dbleps, "daxpy(case 69)");
     }
   };
  };


  {
   int N = 1;
   float alpha[2] = {-0.3f, 0.1f};
   float X[] = { 0.194f, -0.959f };
   int incX = -1;
   float Y[] = { 0.096f, 0.032f };
   int incY = 1;
   float expected[] = { 0.1337f, 0.3391f };
   cblas_caxpy(N, alpha, X, incX, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[2*i], expected[2*i], flteps, "caxpy(case 70) real");
       gsl_test_rel(Y[2*i+1], expected[2*i+1], flteps, "caxpy(case 70) imag");
     };
   };
  };


  {
   int N = 1;
   double alpha[2] = {0, 1};
   double X[] = { 0.776, -0.671 };
   int incX = -1;
   double Y[] = { 0.39, 0.404 };
   int incY = 1;
   double expected[] = { 1.061, 1.18 };
   cblas_zaxpy(N, alpha, X, incX, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[2*i], expected[2*i], dbleps, "zaxpy(case 71) real");
       gsl_test_rel(Y[2*i+1], expected[2*i+1], dbleps, "zaxpy(case 71) imag");
     };
   };
  };


  {
   int N = 1;
   float alpha = 1.0f;
   float X[] = { 0.647f };
   int incX = -1;
   float Y[] = { 0.016f };
   int incY = -1;
   float expected[] = { 0.663f };
   cblas_saxpy(N, alpha, X, incX, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], expected[i], flteps, "saxpy(case 72)");
     }
   };
  };


  {
   int N = 1;
   double alpha = -1;
   double X[] = { -0.558 };
   int incX = -1;
   double Y[] = { 0.308 };
   int incY = -1;
   double expected[] = { 0.866 };
   cblas_daxpy(N, alpha, X, incX, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], expected[i], dbleps, "daxpy(case 73)");
     }
   };
  };


  {
   int N = 1;
   float alpha[2] = {-0.3f, 0.1f};
   float X[] = { 0.899f, -0.624f };
   int incX = -1;
   float Y[] = { 0.155f, -0.33f };
   int incY = -1;
   float expected[] = { -0.0523f, -0.0529f };
   cblas_caxpy(N, alpha, X, incX, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[2*i], expected[2*i], flteps, "caxpy(case 74) real");
       gsl_test_rel(Y[2*i+1], expected[2*i+1], flteps, "caxpy(case 74) imag");
     };
   };
  };


  {
   int N = 1;
   double alpha[2] = {0, 1};
   double X[] = { -0.451, 0.768 };
   int incX = -1;
   double Y[] = { 0.007, 0.732 };
   int incY = -1;
   double expected[] = { -0.761, 0.281 };
   cblas_zaxpy(N, alpha, X, incX, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[2*i], expected[2*i], dbleps, "zaxpy(case 75) real");
       gsl_test_rel(Y[2*i+1], expected[2*i+1], dbleps, "zaxpy(case 75) imag");
     };
   };
  };


}
