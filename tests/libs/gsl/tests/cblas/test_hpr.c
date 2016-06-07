#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_hpr (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int uplo = 121;
   int N = 2;
   float alpha = 0.1f;
   float Ap[] = { -0.273f, -0.499f, -0.305f, -0.277f, 0.238f, -0.369f };
   float X[] = { 0.638f, -0.905f, 0.224f, 0.182f };
   int incX = -1;
   float Ap_expected[] = { -0.26467f, 0.0f, -0.30718f, -0.245116f, 0.360607f, 0.0f };
   cblas_chpr(order, uplo, N, alpha, X, incX, Ap);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], flteps, "chpr(case 1418) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], flteps, "chpr(case 1418) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int N = 2;
   float alpha = 0.1f;
   float Ap[] = { -0.273f, -0.499f, -0.305f, -0.277f, 0.238f, -0.369f };
   float X[] = { 0.638f, -0.905f, 0.224f, 0.182f };
   int incX = -1;
   float Ap_expected[] = { -0.26467f, 0.0f, -0.30718f, -0.308884f, 0.360607f, 0.0f };
   cblas_chpr(order, uplo, N, alpha, X, incX, Ap);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], flteps, "chpr(case 1419) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], flteps, "chpr(case 1419) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int N = 2;
   float alpha = 0.1f;
   float Ap[] = { -0.273f, -0.499f, -0.305f, -0.277f, 0.238f, -0.369f };
   float X[] = { 0.638f, -0.905f, 0.224f, 0.182f };
   int incX = -1;
   float Ap_expected[] = { -0.26467f, 0.0f, -0.30718f, -0.245116f, 0.360607f, 0.0f };
   cblas_chpr(order, uplo, N, alpha, X, incX, Ap);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], flteps, "chpr(case 1420) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], flteps, "chpr(case 1420) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int N = 2;
   float alpha = 0.1f;
   float Ap[] = { -0.273f, -0.499f, -0.305f, -0.277f, 0.238f, -0.369f };
   float X[] = { 0.638f, -0.905f, 0.224f, 0.182f };
   int incX = -1;
   float Ap_expected[] = { -0.26467f, 0.0f, -0.30718f, -0.308884f, 0.360607f, 0.0f };
   cblas_chpr(order, uplo, N, alpha, X, incX, Ap);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], flteps, "chpr(case 1421) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], flteps, "chpr(case 1421) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int N = 2;
   double alpha = 1;
   double Ap[] = { 0.265, 0.362, -0.855, 0.035, 0.136, 0.133 };
   double X[] = { -0.278, -0.686, -0.736, -0.918 };
   int incX = -1;
   double Ap_expected[] = { 1.64942, 0.0, -0.020644, -0.214692, 0.68388, 0.0 };
   cblas_zhpr(order, uplo, N, alpha, X, incX, Ap);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], dbleps, "zhpr(case 1422) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], dbleps, "zhpr(case 1422) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int N = 2;
   double alpha = 1;
   double Ap[] = { 0.265, 0.362, -0.855, 0.035, 0.136, 0.133 };
   double X[] = { -0.278, -0.686, -0.736, -0.918 };
   int incX = -1;
   double Ap_expected[] = { 1.64942, 0.0, -0.020644, 0.284692, 0.68388, 0.0 };
   cblas_zhpr(order, uplo, N, alpha, X, incX, Ap);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], dbleps, "zhpr(case 1423) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], dbleps, "zhpr(case 1423) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int N = 2;
   double alpha = 1;
   double Ap[] = { 0.265, 0.362, -0.855, 0.035, 0.136, 0.133 };
   double X[] = { -0.278, -0.686, -0.736, -0.918 };
   int incX = -1;
   double Ap_expected[] = { 1.64942, 0.0, -0.020644, -0.214692, 0.68388, 0.0 };
   cblas_zhpr(order, uplo, N, alpha, X, incX, Ap);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], dbleps, "zhpr(case 1424) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], dbleps, "zhpr(case 1424) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int N = 2;
   double alpha = 1;
   double Ap[] = { 0.265, 0.362, -0.855, 0.035, 0.136, 0.133 };
   double X[] = { -0.278, -0.686, -0.736, -0.918 };
   int incX = -1;
   double Ap_expected[] = { 1.64942, 0.0, -0.020644, 0.284692, 0.68388, 0.0 };
   cblas_zhpr(order, uplo, N, alpha, X, incX, Ap);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Ap[2*i], Ap_expected[2*i], dbleps, "zhpr(case 1425) real");
       gsl_test_rel(Ap[2*i+1], Ap_expected[2*i+1], dbleps, "zhpr(case 1425) imag");
     };
   };
  };


}
