#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_hbmv (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int uplo = 121;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {-0.3f, 0.1f};
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.937f, -0.035f, 0.339f, 0.847f, 0.022f, 0.153f, -0.785f, 0.193f, -0.731f, -0.166f, -0.243f, -0.319f, 0.173f, -0.24f, 0.079f, -0.058f, 0.124f, 0.445f };
   float X[] = { -0.093f, -0.103f, -0.537f, -0.151f, 0.094f, 0.954f };
   int incX = -1;
   float Y[] = { 0.029f, -0.391f, -0.256f, 0.031f, -0.478f, 0.098f };
   int incY = -1;
   float y_expected[] = { 0.02698f, 0.521724f, -0.379354f, 1.27743f, -0.25427f, -0.043268f };
   cblas_chbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "chbmv(case 1086) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "chbmv(case 1086) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {-0.3f, 0.1f};
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.937f, -0.035f, 0.339f, 0.847f, 0.022f, 0.153f, -0.785f, 0.193f, -0.731f, -0.166f, -0.243f, -0.319f, 0.173f, -0.24f, 0.079f, -0.058f, 0.124f, 0.445f };
   float X[] = { -0.093f, -0.103f, -0.537f, -0.151f, 0.094f, 0.954f };
   int incX = -1;
   float Y[] = { 0.029f, -0.391f, -0.256f, 0.031f, -0.478f, 0.098f };
   int incY = -1;
   float y_expected[] = { 0.02698f, 0.521724f, -0.379354f, 1.27743f, -0.25427f, -0.043268f };
   cblas_chbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "chbmv(case 1087) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "chbmv(case 1087) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {-0.3f, 0.1f};
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.937f, -0.035f, 0.339f, 0.847f, 0.022f, 0.153f, -0.785f, 0.193f, -0.731f, -0.166f, -0.243f, -0.319f, 0.173f, -0.24f, 0.079f, -0.058f, 0.124f, 0.445f };
   float X[] = { -0.093f, -0.103f, -0.537f, -0.151f, 0.094f, 0.954f };
   int incX = -1;
   float Y[] = { 0.029f, -0.391f, -0.256f, 0.031f, -0.478f, 0.098f };
   int incY = -1;
   float y_expected[] = { -0.06422f, -0.016288f, 0.734206f, 0.108366f, -0.411982f, 0.347068f };
   cblas_chbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "chbmv(case 1088) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "chbmv(case 1088) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {-0.3f, 0.1f};
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.937f, -0.035f, 0.339f, 0.847f, 0.022f, 0.153f, -0.785f, 0.193f, -0.731f, -0.166f, -0.243f, -0.319f, 0.173f, -0.24f, 0.079f, -0.058f, 0.124f, 0.445f };
   float X[] = { -0.093f, -0.103f, -0.537f, -0.151f, 0.094f, 0.954f };
   int incX = -1;
   float Y[] = { 0.029f, -0.391f, -0.256f, 0.031f, -0.478f, 0.098f };
   int incY = -1;
   float y_expected[] = { -0.06422f, -0.016288f, 0.734206f, 0.108366f, -0.411982f, 0.347068f };
   cblas_chbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "chbmv(case 1089) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "chbmv(case 1089) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {-0.3f, 0.1f};
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.937f, -0.035f, 0.339f, 0.847f, 0.022f, 0.153f, -0.785f, 0.193f, -0.731f, -0.166f, -0.243f, -0.319f, 0.173f, -0.24f, 0.079f, -0.058f, 0.124f, 0.445f };
   float X[] = { -0.093f, -0.103f, -0.537f, -0.151f, 0.094f, 0.954f };
   int incX = -1;
   float Y[] = { 0.029f, -0.391f, -0.256f, 0.031f, -0.478f, 0.098f };
   int incY = -1;
   float y_expected[] = { 0.19354f, 0.056192f, 0.72585f, 0.42717f, -0.2047f, 0.405354f };
   cblas_chbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "chbmv(case 1090) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "chbmv(case 1090) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {-0.3f, 0.1f};
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.937f, -0.035f, 0.339f, 0.847f, 0.022f, 0.153f, -0.785f, 0.193f, -0.731f, -0.166f, -0.243f, -0.319f, 0.173f, -0.24f, 0.079f, -0.058f, 0.124f, 0.445f };
   float X[] = { -0.093f, -0.103f, -0.537f, -0.151f, 0.094f, 0.954f };
   int incX = -1;
   float Y[] = { 0.029f, -0.391f, -0.256f, 0.031f, -0.478f, 0.098f };
   int incY = -1;
   float y_expected[] = { 0.19354f, 0.056192f, 0.72585f, 0.42717f, -0.2047f, 0.405354f };
   cblas_chbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "chbmv(case 1091) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "chbmv(case 1091) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {-0.3f, 0.1f};
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.937f, -0.035f, 0.339f, 0.847f, 0.022f, 0.153f, -0.785f, 0.193f, -0.731f, -0.166f, -0.243f, -0.319f, 0.173f, -0.24f, 0.079f, -0.058f, 0.124f, 0.445f };
   float X[] = { -0.093f, -0.103f, -0.537f, -0.151f, 0.094f, 0.954f };
   int incX = -1;
   float Y[] = { 0.029f, -0.391f, -0.256f, 0.031f, -0.478f, 0.098f };
   int incY = -1;
   float y_expected[] = { -0.151304f, 0.471592f, -0.507714f, -0.304446f, -1.16395f, -0.299062f };
   cblas_chbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "chbmv(case 1092) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "chbmv(case 1092) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {-0.3f, 0.1f};
   int N = 3;
   int k = 1;
   int lda = 3;
   float A[] = { 0.937f, -0.035f, 0.339f, 0.847f, 0.022f, 0.153f, -0.785f, 0.193f, -0.731f, -0.166f, -0.243f, -0.319f, 0.173f, -0.24f, 0.079f, -0.058f, 0.124f, 0.445f };
   float X[] = { -0.093f, -0.103f, -0.537f, -0.151f, 0.094f, 0.954f };
   int incX = -1;
   float Y[] = { 0.029f, -0.391f, -0.256f, 0.031f, -0.478f, 0.098f };
   int incY = -1;
   float y_expected[] = { -0.151304f, 0.471592f, -0.507714f, -0.304446f, -1.16395f, -0.299062f };
   cblas_chbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "chbmv(case 1093) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "chbmv(case 1093) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.662, 0.24, -0.311, -0.345, -0.782, 0.904, -0.842, 0.065, -0.168, -0.855, -0.692, 0.113, 0.009, -0.707, -0.981, 0.019, -0.687, 0.861 };
   double X[] = { 0.873, -0.509, 0.398, 0.471, 0.214, 0.878 };
   int incX = -1;
   double Y[] = { -0.441, -0.781, 0.979, -0.911, 0.879, 0.807 };
   int incY = -1;
   double y_expected[] = { -0.902712, -0.524419, -0.307439, -2.167713, 1.059385, 1.104445 };
   cblas_zhbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zhbmv(case 1094) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zhbmv(case 1094) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.662, 0.24, -0.311, -0.345, -0.782, 0.904, -0.842, 0.065, -0.168, -0.855, -0.692, 0.113, 0.009, -0.707, -0.981, 0.019, -0.687, 0.861 };
   double X[] = { 0.873, -0.509, 0.398, 0.471, 0.214, 0.878 };
   int incX = -1;
   double Y[] = { -0.441, -0.781, 0.979, -0.911, 0.879, 0.807 };
   int incY = -1;
   double y_expected[] = { -0.902712, -0.524419, -0.307439, -2.167713, 1.059385, 1.104445 };
   cblas_zhbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zhbmv(case 1095) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zhbmv(case 1095) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.662, 0.24, -0.311, -0.345, -0.782, 0.904, -0.842, 0.065, -0.168, -0.855, -0.692, 0.113, 0.009, -0.707, -0.981, 0.019, -0.687, 0.861 };
   double X[] = { 0.873, -0.509, 0.398, 0.471, 0.214, 0.878 };
   int incX = -1;
   double Y[] = { -0.441, -0.781, 0.979, -0.911, 0.879, 0.807 };
   int incY = -1;
   double y_expected[] = { -0.960834, -0.558818, 1.042598, -1.102864, 0.507945, 0.11149 };
   cblas_zhbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zhbmv(case 1096) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zhbmv(case 1096) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.662, 0.24, -0.311, -0.345, -0.782, 0.904, -0.842, 0.065, -0.168, -0.855, -0.692, 0.113, 0.009, -0.707, -0.981, 0.019, -0.687, 0.861 };
   double X[] = { 0.873, -0.509, 0.398, 0.471, 0.214, 0.878 };
   int incX = -1;
   double Y[] = { -0.441, -0.781, 0.979, -0.911, 0.879, 0.807 };
   int incY = -1;
   double y_expected[] = { -0.960834, -0.558818, 1.042598, -1.102864, 0.507945, 0.11149 };
   cblas_zhbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zhbmv(case 1097) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zhbmv(case 1097) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.662, 0.24, -0.311, -0.345, -0.782, 0.904, -0.842, 0.065, -0.168, -0.855, -0.692, 0.113, 0.009, -0.707, -0.981, 0.019, -0.687, 0.861 };
   double X[] = { 0.873, -0.509, 0.398, 0.471, 0.214, 0.878 };
   int incX = -1;
   double Y[] = { -0.441, -0.781, 0.979, -0.911, 0.879, 0.807 };
   int incY = -1;
   double y_expected[] = { -1.626828, 0.003954, 0.437012, -2.365106, 0.446715, 0.16323 };
   cblas_zhbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zhbmv(case 1098) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zhbmv(case 1098) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.662, 0.24, -0.311, -0.345, -0.782, 0.904, -0.842, 0.065, -0.168, -0.855, -0.692, 0.113, 0.009, -0.707, -0.981, 0.019, -0.687, 0.861 };
   double X[] = { 0.873, -0.509, 0.398, 0.471, 0.214, 0.878 };
   int incX = -1;
   double Y[] = { -0.441, -0.781, 0.979, -0.911, 0.879, 0.807 };
   int incY = -1;
   double y_expected[] = { -1.626828, 0.003954, 0.437012, -2.365106, 0.446715, 0.16323 };
   cblas_zhbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zhbmv(case 1099) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zhbmv(case 1099) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.662, 0.24, -0.311, -0.345, -0.782, 0.904, -0.842, 0.065, -0.168, -0.855, -0.692, 0.113, 0.009, -0.707, -0.981, 0.019, -0.687, 0.861 };
   double X[] = { 0.873, -0.509, 0.398, 0.471, 0.214, 0.878 };
   int incX = -1;
   double Y[] = { -0.441, -0.781, 0.979, -0.911, 0.879, 0.807 };
   int incY = -1;
   double y_expected[] = { -0.097302, -1.204999, 1.168771, -0.822543, 0.734395, 1.379065 };
   cblas_zhbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zhbmv(case 1100) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zhbmv(case 1100) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   int N = 3;
   int k = 1;
   int lda = 3;
   double A[] = { 0.662, 0.24, -0.311, -0.345, -0.782, 0.904, -0.842, 0.065, -0.168, -0.855, -0.692, 0.113, 0.009, -0.707, -0.981, 0.019, -0.687, 0.861 };
   double X[] = { 0.873, -0.509, 0.398, 0.471, 0.214, 0.878 };
   int incX = -1;
   double Y[] = { -0.441, -0.781, 0.979, -0.911, 0.879, 0.807 };
   int incY = -1;
   double y_expected[] = { -0.097302, -1.204999, 1.168771, -0.822543, 0.734395, 1.379065 };
   cblas_zhbmv(order, uplo, N, k, alpha, A, lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zhbmv(case 1101) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zhbmv(case 1101) imag");
     };
   };
  };


}
