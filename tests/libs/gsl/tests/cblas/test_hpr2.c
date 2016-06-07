#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_hpr2 (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int uplo = 121;
   int N = 1;
   float alpha[2] = {-1.0f, 0.0f};
   float Ap[] = { 0.159f, -0.13f };
   float X[] = { 0.854f, 0.851f };
   int incX = -1;
   float Y[] = { 0.526f, -0.267f };
   int incY = -1;
   float Ap_expected[] = { -0.284974f, 0.0f };
   cblas_chpr2(order, uplo, N, alpha, X, incX, Y, incY, Ap);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], flteps, "chpr2(case 1458) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], flteps, "chpr2(case 1458) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int N = 1;
   float alpha[2] = {-1.0f, 0.0f};
   float Ap[] = { 0.159f, -0.13f };
   float X[] = { 0.854f, 0.851f };
   int incX = -1;
   float Y[] = { 0.526f, -0.267f };
   int incY = -1;
   float Ap_expected[] = { -0.284974f, 0.0f };
   cblas_chpr2(order, uplo, N, alpha, X, incX, Y, incY, Ap);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], flteps, "chpr2(case 1459) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], flteps, "chpr2(case 1459) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int N = 1;
   float alpha[2] = {-1.0f, 0.0f};
   float Ap[] = { 0.159f, -0.13f };
   float X[] = { 0.854f, 0.851f };
   int incX = -1;
   float Y[] = { 0.526f, -0.267f };
   int incY = -1;
   float Ap_expected[] = { -0.284974f, 0.0f };
   cblas_chpr2(order, uplo, N, alpha, X, incX, Y, incY, Ap);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], flteps, "chpr2(case 1460) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], flteps, "chpr2(case 1460) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int N = 1;
   float alpha[2] = {-1.0f, 0.0f};
   float Ap[] = { 0.159f, -0.13f };
   float X[] = { 0.854f, 0.851f };
   int incX = -1;
   float Y[] = { 0.526f, -0.267f };
   int incY = -1;
   float Ap_expected[] = { -0.284974f, 0.0f };
   cblas_chpr2(order, uplo, N, alpha, X, incX, Y, incY, Ap);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], flteps, "chpr2(case 1461) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], flteps, "chpr2(case 1461) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int N = 1;
   double alpha[2] = {-0.3, 0.1};
   double Ap[] = { 0.772, 0.997 };
   double X[] = { -0.173, -0.839 };
   int incX = -1;
   double Y[] = { 0.941, -0.422 };
   int incY = -1;
   double Ap_expected[] = { 0.829742, 0.0 };
   cblas_zhpr2(order, uplo, N, alpha, X, incX, Y, incY, Ap);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], dbleps, "zhpr2(case 1462) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], dbleps, "zhpr2(case 1462) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int N = 1;
   double alpha[2] = {-0.3, 0.1};
   double Ap[] = { 0.772, 0.997 };
   double X[] = { -0.173, -0.839 };
   int incX = -1;
   double Y[] = { 0.941, -0.422 };
   int incY = -1;
   double Ap_expected[] = { 0.829742, 0.0 };
   cblas_zhpr2(order, uplo, N, alpha, X, incX, Y, incY, Ap);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], dbleps, "zhpr2(case 1463) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], dbleps, "zhpr2(case 1463) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int N = 1;
   double alpha[2] = {-0.3, 0.1};
   double Ap[] = { 0.772, 0.997 };
   double X[] = { -0.173, -0.839 };
   int incX = -1;
   double Y[] = { 0.941, -0.422 };
   int incY = -1;
   double Ap_expected[] = { 0.829742, 0.0 };
   cblas_zhpr2(order, uplo, N, alpha, X, incX, Y, incY, Ap);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], dbleps, "zhpr2(case 1464) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], dbleps, "zhpr2(case 1464) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int N = 1;
   double alpha[2] = {-0.3, 0.1};
   double Ap[] = { 0.772, 0.997 };
   double X[] = { -0.173, -0.839 };
   int incX = -1;
   double Y[] = { 0.941, -0.422 };
   int incY = -1;
   double Ap_expected[] = { 0.829742, 0.0 };
   cblas_zhpr2(order, uplo, N, alpha, X, incX, Y, incY, Ap);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], dbleps, "zhpr2(case 1465) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], dbleps, "zhpr2(case 1465) imag");
     };
   };
  };


}
