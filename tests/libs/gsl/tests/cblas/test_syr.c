#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_syr (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int uplo = 121;
   int N = 1;
   int lda = 1;
   float alpha = 0.1f;
   float A[] = { -0.291f };
   float X[] = { 0.845f };
   int incX = -1;
   float A_expected[] = { -0.219597f };
   cblas_ssyr(order, uplo, N, alpha, X, incX, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], flteps, "ssyr(case 1402)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int N = 1;
   int lda = 1;
   float alpha = 0.1f;
   float A[] = { -0.291f };
   float X[] = { 0.845f };
   int incX = -1;
   float A_expected[] = { -0.219597f };
   cblas_ssyr(order, uplo, N, alpha, X, incX, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], flteps, "ssyr(case 1403)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int N = 1;
   int lda = 1;
   float alpha = 0.1f;
   float A[] = { -0.291f };
   float X[] = { 0.845f };
   int incX = -1;
   float A_expected[] = { -0.219597f };
   cblas_ssyr(order, uplo, N, alpha, X, incX, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], flteps, "ssyr(case 1404)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int N = 1;
   int lda = 1;
   float alpha = 0.1f;
   float A[] = { -0.291f };
   float X[] = { 0.845f };
   int incX = -1;
   float A_expected[] = { -0.219597f };
   cblas_ssyr(order, uplo, N, alpha, X, incX, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], flteps, "ssyr(case 1405)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int N = 1;
   int lda = 1;
   double alpha = -0.3;
   double A[] = { -0.65 };
   double X[] = { -0.891 };
   int incX = -1;
   double A_expected[] = { -0.8881643 };
   cblas_dsyr(order, uplo, N, alpha, X, incX, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], dbleps, "dsyr(case 1406)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int N = 1;
   int lda = 1;
   double alpha = -0.3;
   double A[] = { -0.65 };
   double X[] = { -0.891 };
   int incX = -1;
   double A_expected[] = { -0.8881643 };
   cblas_dsyr(order, uplo, N, alpha, X, incX, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], dbleps, "dsyr(case 1407)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int N = 1;
   int lda = 1;
   double alpha = -0.3;
   double A[] = { -0.65 };
   double X[] = { -0.891 };
   int incX = -1;
   double A_expected[] = { -0.8881643 };
   cblas_dsyr(order, uplo, N, alpha, X, incX, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], dbleps, "dsyr(case 1408)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int N = 1;
   int lda = 1;
   double alpha = -0.3;
   double A[] = { -0.65 };
   double X[] = { -0.891 };
   int incX = -1;
   double A_expected[] = { -0.8881643 };
   cblas_dsyr(order, uplo, N, alpha, X, incX, A, lda);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(A[i], A_expected[i], dbleps, "dsyr(case 1409)");
     }
   };
  };


}
