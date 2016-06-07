#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_symv (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int uplo = 121;
   float alpha = 1.0f;
   float beta = -1.0f;
   int N = 1;
   int lda = 1;
   float A[] = { -0.428f };
   float X[] = { -0.34f };
   int incX = -1;
   float Y[] = { -0.888f };
   int incY = -1;
   float y_expected[] = { 1.03352f };
   cblas_ssymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssymv(case 1054)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   float alpha = 1.0f;
   float beta = -1.0f;
   int N = 1;
   int lda = 1;
   float A[] = { -0.428f };
   float X[] = { -0.34f };
   int incX = -1;
   float Y[] = { -0.888f };
   int incY = -1;
   float y_expected[] = { 1.03352f };
   cblas_ssymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssymv(case 1055)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   float alpha = 1.0f;
   float beta = -1.0f;
   int N = 1;
   int lda = 1;
   float A[] = { -0.428f };
   float X[] = { -0.34f };
   int incX = -1;
   float Y[] = { -0.888f };
   int incY = -1;
   float y_expected[] = { 1.03352f };
   cblas_ssymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssymv(case 1056)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   float alpha = 1.0f;
   float beta = -1.0f;
   int N = 1;
   int lda = 1;
   float A[] = { -0.428f };
   float X[] = { -0.34f };
   int incX = -1;
   float Y[] = { -0.888f };
   int incY = -1;
   float y_expected[] = { 1.03352f };
   cblas_ssymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssymv(case 1057)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   float alpha = 1.0f;
   float beta = -1.0f;
   int N = 1;
   int lda = 1;
   float A[] = { -0.428f };
   float X[] = { -0.34f };
   int incX = -1;
   float Y[] = { -0.888f };
   int incY = -1;
   float y_expected[] = { 1.03352f };
   cblas_ssymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssymv(case 1058)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   float alpha = 1.0f;
   float beta = -1.0f;
   int N = 1;
   int lda = 1;
   float A[] = { -0.428f };
   float X[] = { -0.34f };
   int incX = -1;
   float Y[] = { -0.888f };
   int incY = -1;
   float y_expected[] = { 1.03352f };
   cblas_ssymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssymv(case 1059)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   float alpha = 1.0f;
   float beta = -1.0f;
   int N = 1;
   int lda = 1;
   float A[] = { -0.428f };
   float X[] = { -0.34f };
   int incX = -1;
   float Y[] = { -0.888f };
   int incY = -1;
   float y_expected[] = { 1.03352f };
   cblas_ssymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssymv(case 1060)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   float alpha = 1.0f;
   float beta = -1.0f;
   int N = 1;
   int lda = 1;
   float A[] = { -0.428f };
   float X[] = { -0.34f };
   int incX = -1;
   float Y[] = { -0.888f };
   int incY = -1;
   float y_expected[] = { 1.03352f };
   cblas_ssymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "ssymv(case 1061)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   double alpha = 0;
   double beta = -0.3;
   int N = 1;
   int lda = 1;
   double A[] = { 0.544 };
   double X[] = { -0.601 };
   int incX = -1;
   double Y[] = { -0.852 };
   int incY = -1;
   double y_expected[] = { 0.2556 };
   cblas_dsymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsymv(case 1062)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   double alpha = 0;
   double beta = -0.3;
   int N = 1;
   int lda = 1;
   double A[] = { 0.544 };
   double X[] = { -0.601 };
   int incX = -1;
   double Y[] = { -0.852 };
   int incY = -1;
   double y_expected[] = { 0.2556 };
   cblas_dsymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsymv(case 1063)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   double alpha = 0;
   double beta = -0.3;
   int N = 1;
   int lda = 1;
   double A[] = { 0.544 };
   double X[] = { -0.601 };
   int incX = -1;
   double Y[] = { -0.852 };
   int incY = -1;
   double y_expected[] = { 0.2556 };
   cblas_dsymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsymv(case 1064)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   double alpha = 0;
   double beta = -0.3;
   int N = 1;
   int lda = 1;
   double A[] = { 0.544 };
   double X[] = { -0.601 };
   int incX = -1;
   double Y[] = { -0.852 };
   int incY = -1;
   double y_expected[] = { 0.2556 };
   cblas_dsymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsymv(case 1065)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   double alpha = 0;
   double beta = -0.3;
   int N = 1;
   int lda = 1;
   double A[] = { 0.544 };
   double X[] = { -0.601 };
   int incX = -1;
   double Y[] = { -0.852 };
   int incY = -1;
   double y_expected[] = { 0.2556 };
   cblas_dsymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsymv(case 1066)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   double alpha = 0;
   double beta = -0.3;
   int N = 1;
   int lda = 1;
   double A[] = { 0.544 };
   double X[] = { -0.601 };
   int incX = -1;
   double Y[] = { -0.852 };
   int incY = -1;
   double y_expected[] = { 0.2556 };
   cblas_dsymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsymv(case 1067)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   double alpha = 0;
   double beta = -0.3;
   int N = 1;
   int lda = 1;
   double A[] = { 0.544 };
   double X[] = { -0.601 };
   int incX = -1;
   double Y[] = { -0.852 };
   int incY = -1;
   double y_expected[] = { 0.2556 };
   cblas_dsymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsymv(case 1068)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   double alpha = 0;
   double beta = -0.3;
   int N = 1;
   int lda = 1;
   double A[] = { 0.544 };
   double X[] = { -0.601 };
   int incX = -1;
   double Y[] = { -0.852 };
   int incY = -1;
   double y_expected[] = { 0.2556 };
   cblas_dsymv(order, uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dsymv(case 1069)");
     }
   };
  };


}
