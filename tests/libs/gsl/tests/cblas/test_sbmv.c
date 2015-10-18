#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_sbmv (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int uplo = 121;
   float alpha = 1.0f;
   float beta = 0.0f;
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.627f, -0.312f, 0.031f, 0.308f, 0.323f, -0.578f, 0.797f, 0.545f, -0.476f };
   float X[] = { -0.542f, 0.606f, 0.727f };
   int incX = -1;
   float Y[] = { 0.755f, 0.268f, -0.99f };
   int incY = -1;
   float y_expected[] = { -0.236236f, -0.215242f, 0.266757f };
   cblas_ssbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssbmv(case 1102)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   float alpha = 1.0f;
   float beta = 0.0f;
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.627f, -0.312f, 0.031f, 0.308f, 0.323f, -0.578f, 0.797f, 0.545f, -0.476f };
   float X[] = { -0.542f, 0.606f, 0.727f };
   int incX = -1;
   float Y[] = { 0.755f, 0.268f, -0.99f };
   int incY = -1;
   float y_expected[] = { -0.236236f, -0.215242f, 0.266757f };
   cblas_ssbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssbmv(case 1103)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   float alpha = 1.0f;
   float beta = 0.0f;
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.627f, -0.312f, 0.031f, 0.308f, 0.323f, -0.578f, 0.797f, 0.545f, -0.476f };
   float X[] = { -0.542f, 0.606f, 0.727f };
   int incX = -1;
   float Y[] = { 0.755f, 0.268f, -0.99f };
   int incY = -1;
   float y_expected[] = { 0.187592f, -0.01232f, -0.040176f };
   cblas_ssbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssbmv(case 1104)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   float alpha = 1.0f;
   float beta = 0.0f;
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.627f, -0.312f, 0.031f, 0.308f, 0.323f, -0.578f, 0.797f, 0.545f, -0.476f };
   float X[] = { -0.542f, 0.606f, 0.727f };
   int incX = -1;
   float Y[] = { 0.755f, 0.268f, -0.99f };
   int incY = -1;
   float y_expected[] = { 0.187592f, -0.01232f, -0.040176f };
   cblas_ssbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssbmv(case 1105)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   float alpha = 1.0f;
   float beta = 0.0f;
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.627f, -0.312f, 0.031f, 0.308f, 0.323f, -0.578f, 0.797f, 0.545f, -0.476f };
   float X[] = { -0.542f, 0.606f, 0.727f };
   int incX = -1;
   float Y[] = { 0.755f, 0.268f, -0.99f };
   int incY = -1;
   float y_expected[] = { 0.187592f, -0.01232f, -0.040176f };
   cblas_ssbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssbmv(case 1106)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   float alpha = 1.0f;
   float beta = 0.0f;
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.627f, -0.312f, 0.031f, 0.308f, 0.323f, -0.578f, 0.797f, 0.545f, -0.476f };
   float X[] = { -0.542f, 0.606f, 0.727f };
   int incX = -1;
   float Y[] = { 0.755f, 0.268f, -0.99f };
   int incY = -1;
   float y_expected[] = { 0.187592f, -0.01232f, -0.040176f };
   cblas_ssbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssbmv(case 1107)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   float alpha = 1.0f;
   float beta = 0.0f;
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.627f, -0.312f, 0.031f, 0.308f, 0.323f, -0.578f, 0.797f, 0.545f, -0.476f };
   float X[] = { -0.542f, 0.606f, 0.727f };
   int incX = -1;
   float Y[] = { 0.755f, 0.268f, -0.99f };
   int incY = -1;
   float y_expected[] = { -0.236236f, -0.215242f, 0.266757f };
   cblas_ssbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssbmv(case 1108)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   float alpha = 1.0f;
   float beta = 0.0f;
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.627f, -0.312f, 0.031f, 0.308f, 0.323f, -0.578f, 0.797f, 0.545f, -0.476f };
   float X[] = { -0.542f, 0.606f, 0.727f };
   int incX = -1;
   float Y[] = { 0.755f, 0.268f, -0.99f };
   int incY = -1;
   float y_expected[] = { -0.236236f, -0.215242f, 0.266757f };
   cblas_ssbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssbmv(case 1109)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   double alpha = 0;
   double beta = 1;
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.83, -0.568, -0.888, 0.281, -0.779, -0.148, 0.138, 0.053, -0.757 };
   double X[] = { 0.166, 0.808, 0.723 };
   int incX = -1;
   double Y[] = { 0.9, 0.99, -0.578 };
   int incY = -1;
   double y_expected[] = { 0.9, 0.99, -0.578 };
   cblas_dsbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsbmv(case 1110)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   double alpha = 0;
   double beta = 1;
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.83, -0.568, -0.888, 0.281, -0.779, -0.148, 0.138, 0.053, -0.757 };
   double X[] = { 0.166, 0.808, 0.723 };
   int incX = -1;
   double Y[] = { 0.9, 0.99, -0.578 };
   int incY = -1;
   double y_expected[] = { 0.9, 0.99, -0.578 };
   cblas_dsbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsbmv(case 1111)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   double alpha = 0;
   double beta = 1;
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.83, -0.568, -0.888, 0.281, -0.779, -0.148, 0.138, 0.053, -0.757 };
   double X[] = { 0.166, 0.808, 0.723 };
   int incX = -1;
   double Y[] = { 0.9, 0.99, -0.578 };
   int incY = -1;
   double y_expected[] = { 0.9, 0.99, -0.578 };
   cblas_dsbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsbmv(case 1112)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   double alpha = 0;
   double beta = 1;
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.83, -0.568, -0.888, 0.281, -0.779, -0.148, 0.138, 0.053, -0.757 };
   double X[] = { 0.166, 0.808, 0.723 };
   int incX = -1;
   double Y[] = { 0.9, 0.99, -0.578 };
   int incY = -1;
   double y_expected[] = { 0.9, 0.99, -0.578 };
   cblas_dsbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsbmv(case 1113)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   double alpha = 0;
   double beta = 1;
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.83, -0.568, -0.888, 0.281, -0.779, -0.148, 0.138, 0.053, -0.757 };
   double X[] = { 0.166, 0.808, 0.723 };
   int incX = -1;
   double Y[] = { 0.9, 0.99, -0.578 };
   int incY = -1;
   double y_expected[] = { 0.9, 0.99, -0.578 };
   cblas_dsbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsbmv(case 1114)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   double alpha = 0;
   double beta = 1;
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.83, -0.568, -0.888, 0.281, -0.779, -0.148, 0.138, 0.053, -0.757 };
   double X[] = { 0.166, 0.808, 0.723 };
   int incX = -1;
   double Y[] = { 0.9, 0.99, -0.578 };
   int incY = -1;
   double y_expected[] = { 0.9, 0.99, -0.578 };
   cblas_dsbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsbmv(case 1115)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   double alpha = 0;
   double beta = 1;
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.83, -0.568, -0.888, 0.281, -0.779, -0.148, 0.138, 0.053, -0.757 };
   double X[] = { 0.166, 0.808, 0.723 };
   int incX = -1;
   double Y[] = { 0.9, 0.99, -0.578 };
   int incY = -1;
   double y_expected[] = { 0.9, 0.99, -0.578 };
   cblas_dsbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsbmv(case 1116)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   double alpha = 0;
   double beta = 1;
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.83, -0.568, -0.888, 0.281, -0.779, -0.148, 0.138, 0.053, -0.757 };
   double X[] = { 0.166, 0.808, 0.723 };
   int incX = -1;
   double Y[] = { 0.9, 0.99, -0.578 };
   int incY = -1;
   double y_expected[] = { 0.9, 0.99, -0.578 };
   cblas_dsbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsbmv(case 1117)");
     }
   };
  };


}
