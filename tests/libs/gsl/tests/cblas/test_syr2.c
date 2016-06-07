#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_syr2 (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int uplo = 121;
   int N = 1;
   int lda = 1;
   float alpha = 0.0f;
   float A[] = { 0.862f };
   float X[] = { 0.823f };
   int incX = -1;
   float Y[] = { 0.699f };
   int incY = -1;
   float A_expected[] = { 0.862f };
   cblas_ssyr2(order, uplo, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], flteps, "ssyr2(case 1434)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int N = 1;
   int lda = 1;
   float alpha = 0.0f;
   float A[] = { 0.862f };
   float X[] = { 0.823f };
   int incX = -1;
   float Y[] = { 0.699f };
   int incY = -1;
   float A_expected[] = { 0.862f };
   cblas_ssyr2(order, uplo, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], flteps, "ssyr2(case 1435)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int N = 1;
   int lda = 1;
   float alpha = 0.0f;
   float A[] = { 0.862f };
   float X[] = { 0.823f };
   int incX = -1;
   float Y[] = { 0.699f };
   int incY = -1;
   float A_expected[] = { 0.862f };
   cblas_ssyr2(order, uplo, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], flteps, "ssyr2(case 1436)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int N = 1;
   int lda = 1;
   float alpha = 0.0f;
   float A[] = { 0.862f };
   float X[] = { 0.823f };
   int incX = -1;
   float Y[] = { 0.699f };
   int incY = -1;
   float A_expected[] = { 0.862f };
   cblas_ssyr2(order, uplo, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], flteps, "ssyr2(case 1437)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int N = 1;
   int lda = 1;
   double alpha = 0;
   double A[] = { -0.824 };
   double X[] = { 0.684 };
   int incX = -1;
   double Y[] = { 0.965 };
   int incY = -1;
   double A_expected[] = { -0.824 };
   cblas_dsyr2(order, uplo, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], dbleps, "dsyr2(case 1438)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int N = 1;
   int lda = 1;
   double alpha = 0;
   double A[] = { -0.824 };
   double X[] = { 0.684 };
   int incX = -1;
   double Y[] = { 0.965 };
   int incY = -1;
   double A_expected[] = { -0.824 };
   cblas_dsyr2(order, uplo, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], dbleps, "dsyr2(case 1439)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int N = 1;
   int lda = 1;
   double alpha = 0;
   double A[] = { -0.824 };
   double X[] = { 0.684 };
   int incX = -1;
   double Y[] = { 0.965 };
   int incY = -1;
   double A_expected[] = { -0.824 };
   cblas_dsyr2(order, uplo, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], dbleps, "dsyr2(case 1440)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int N = 1;
   int lda = 1;
   double alpha = 0;
   double A[] = { -0.824 };
   double X[] = { 0.684 };
   int incX = -1;
   double Y[] = { 0.965 };
   int incY = -1;
   double A_expected[] = { -0.824 };
   cblas_dsyr2(order, uplo, N, alpha, X, incX, Y, incY, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], dbleps, "dsyr2(case 1441)");
     }
   };
  };


}
