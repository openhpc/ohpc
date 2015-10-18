#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_ger (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int M = 1;
   int N = 1;
   int lda = 1;
   float alpha = 1.0f;
   float A[] = { -0.515f };
   float X[] = { 0.611f };
   int incX = -1;
   float Y[] = { -0.082f };
   int incY = -1;
   float A_expected[] = { -0.565102f };
   cblas_sger(order, M, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], flteps, "sger(case 1390)");
     }
   };
  };


  {
   int order = 102;
   int M = 1;
   int N = 1;
   int lda = 1;
   float alpha = 1.0f;
   float A[] = { -0.515f };
   float X[] = { 0.611f };
   int incX = -1;
   float Y[] = { -0.082f };
   int incY = -1;
   float A_expected[] = { -0.565102f };
   cblas_sger(order, M, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], flteps, "sger(case 1391)");
     }
   };
  };


  {
   int order = 101;
   int M = 1;
   int N = 1;
   int lda = 1;
   double alpha = 1;
   double A[] = { -0.809 };
   double X[] = { -0.652 };
   int incX = -1;
   double Y[] = { 0.712 };
   int incY = -1;
   double A_expected[] = { -1.273224 };
   cblas_dger(order, M, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], dbleps, "dger(case 1392)");
     }
   };
  };


  {
   int order = 102;
   int M = 1;
   int N = 1;
   int lda = 1;
   double alpha = 1;
   double A[] = { -0.809 };
   double X[] = { -0.652 };
   int incX = -1;
   double Y[] = { 0.712 };
   int incY = -1;
   double A_expected[] = { -1.273224 };
   cblas_dger(order, M, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], dbleps, "dger(case 1393)");
     }
   };
  };


  {
   int order = 101;
   int M = 1;
   int N = 1;
   int lda = 1;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { -0.651f, 0.856f };
   float X[] = { -0.38f, -0.235f };
   int incX = -1;
   float Y[] = { -0.627f, 0.757f };
   int incY = -1;
   float A_expected[] = { -0.651f, 0.856f };
   cblas_cgeru(order, M, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[2*i], A_expected[2*i], flteps, "cgeru(case 1394) real");
       gsl_test_rel(A[2*i+1], A_expected[2*i+1], flteps, "cgeru(case 1394) imag");
     };
   };
  };


  {
   int order = 101;
   int M = 1;
   int N = 1;
   int lda = 1;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { -0.651f, 0.856f };
   float X[] = { -0.38f, -0.235f };
   int incX = -1;
   float Y[] = { -0.627f, 0.757f };
   int incY = -1;
   float A_expected[] = { -0.651f, 0.856f };
   cblas_cgerc(order, M, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[2*i], A_expected[2*i], flteps, "cgerc(case 1395) real");
       gsl_test_rel(A[2*i+1], A_expected[2*i+1], flteps, "cgerc(case 1395) imag");
     };
   };
  };


  {
   int order = 102;
   int M = 1;
   int N = 1;
   int lda = 1;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { -0.651f, 0.856f };
   float X[] = { -0.38f, -0.235f };
   int incX = -1;
   float Y[] = { -0.627f, 0.757f };
   int incY = -1;
   float A_expected[] = { -0.651f, 0.856f };
   cblas_cgeru(order, M, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[2*i], A_expected[2*i], flteps, "cgeru(case 1396) real");
       gsl_test_rel(A[2*i+1], A_expected[2*i+1], flteps, "cgeru(case 1396) imag");
     };
   };
  };


  {
   int order = 102;
   int M = 1;
   int N = 1;
   int lda = 1;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { -0.651f, 0.856f };
   float X[] = { -0.38f, -0.235f };
   int incX = -1;
   float Y[] = { -0.627f, 0.757f };
   int incY = -1;
   float A_expected[] = { -0.651f, 0.856f };
   cblas_cgerc(order, M, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[2*i], A_expected[2*i], flteps, "cgerc(case 1397) real");
       gsl_test_rel(A[2*i+1], A_expected[2*i+1], flteps, "cgerc(case 1397) imag");
     };
   };
  };


  {
   int order = 101;
   int M = 1;
   int N = 1;
   int lda = 1;
   double alpha[2] = {-1, 0};
   double A[] = { -0.426, 0.757 };
   double X[] = { -0.579, -0.155 };
   int incX = -1;
   double Y[] = { 0.831, 0.035 };
   int incY = -1;
   double A_expected[] = { 0.049724, 0.90607 };
   cblas_zgeru(order, M, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[2*i], A_expected[2*i], dbleps, "zgeru(case 1398) real");
       gsl_test_rel(A[2*i+1], A_expected[2*i+1], dbleps, "zgeru(case 1398) imag");
     };
   };
  };


  {
   int order = 101;
   int M = 1;
   int N = 1;
   int lda = 1;
   double alpha[2] = {-1, 0};
   double A[] = { -0.426, 0.757 };
   double X[] = { -0.579, -0.155 };
   int incX = -1;
   double Y[] = { 0.831, 0.035 };
   int incY = -1;
   double A_expected[] = { 0.060574, 0.86554 };
   cblas_zgerc(order, M, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[2*i], A_expected[2*i], dbleps, "zgerc(case 1399) real");
       gsl_test_rel(A[2*i+1], A_expected[2*i+1], dbleps, "zgerc(case 1399) imag");
     };
   };
  };


  {
   int order = 102;
   int M = 1;
   int N = 1;
   int lda = 1;
   double alpha[2] = {-1, 0};
   double A[] = { -0.426, 0.757 };
   double X[] = { -0.579, -0.155 };
   int incX = -1;
   double Y[] = { 0.831, 0.035 };
   int incY = -1;
   double A_expected[] = { 0.049724, 0.90607 };
   cblas_zgeru(order, M, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[2*i], A_expected[2*i], dbleps, "zgeru(case 1400) real");
       gsl_test_rel(A[2*i+1], A_expected[2*i+1], dbleps, "zgeru(case 1400) imag");
     };
   };
  };


  {
   int order = 102;
   int M = 1;
   int N = 1;
   int lda = 1;
   double alpha[2] = {-1, 0};
   double A[] = { -0.426, 0.757 };
   double X[] = { -0.579, -0.155 };
   int incX = -1;
   double Y[] = { 0.831, 0.035 };
   int incY = -1;
   double A_expected[] = { 0.060574, 0.86554 };
   cblas_zgerc(order, M, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[2*i], A_expected[2*i], dbleps, "zgerc(case 1401) real");
       gsl_test_rel(A[2*i+1], A_expected[2*i+1], dbleps, "zgerc(case 1401) imag");
     };
   };
  };


}
