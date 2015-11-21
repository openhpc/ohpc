#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_hemm (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int M = 1;
   int N = 2;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {0.0f, 0.1f};
   float A[] = { -0.126f, 0.079f };
   int lda = 1;
   float B[] = { -0.954f, -0.059f, 0.296f, -0.988f };
   int ldb = 2;
   float C[] = { -0.859f, -0.731f, 0.737f, 0.593f };
   int ldc = 2;
   float C_expected[] = { 0.0723566f, -0.0738796f, -0.0717488f, 0.0699704f };
   cblas_chemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "chemm(case 1550) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "chemm(case 1550) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int M = 1;
   int N = 2;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {0.0f, 0.1f};
   float A[] = { 0.652f, 0.584f };
   int lda = 1;
   float B[] = { -0.983f, -0.734f, -0.422f, -0.825f };
   int ldb = 1;
   float C[] = { 0.387f, 0.341f, -0.734f, 0.632f };
   int ldc = 1;
   float C_expected[] = { 0.0137568f, -0.0253916f, -0.00941f, -0.100914f };
   cblas_chemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "chemm(case 1551) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "chemm(case 1551) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int M = 1;
   int N = 2;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {-1.0f, 0.0f};
   float A[] = { 0.78f, 0.885f, 0.507f, 0.765f, 0.911f, -0.461f, 0.707f, 0.508f };
   int lda = 2;
   float B[] = { -0.905f, 0.633f, 0.85f, -0.943f };
   int ldb = 2;
   float C[] = { 0.045f, -0.237f, 0.078f, -0.252f };
   int ldc = 2;
   float C_expected[] = { 0.589611f, -0.759345f, 0.960095f, -0.09013f };
   cblas_chemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "chemm(case 1552) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "chemm(case 1552) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int M = 1;
   int N = 2;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {-1.0f, 0.0f};
   float A[] = { 0.947f, 0.939f, -0.267f, -0.819f, -0.827f, -0.937f, 0.991f, 0.838f };
   int lda = 2;
   float B[] = { 0.871f, -0.988f, -0.232f, -0.434f };
   int ldb = 1;
   float C[] = { -0.261f, 0.927f, -0.351f, -0.203f };
   int ldc = 1;
   float C_expected[] = { 1.0551f, 0.496359f, 0.780145f, -1.67298f };
   cblas_chemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "chemm(case 1553) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "chemm(case 1553) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int M = 1;
   int N = 2;
   float alpha[2] = {-1.0f, 0.0f};
   float beta[2] = {0.0f, 0.0f};
   float A[] = { -0.593f, -0.9f };
   int lda = 1;
   float B[] = { -0.861f, 0.747f, -0.984f, 0.595f };
   int ldb = 2;
   float C[] = { -0.589f, -0.671f, -0.011f, -0.417f };
   int ldc = 2;
   float C_expected[] = { -0.510573f, 0.442971f, -0.583512f, 0.352835f };
   cblas_chemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "chemm(case 1554) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "chemm(case 1554) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int M = 1;
   int N = 2;
   float alpha[2] = {-1.0f, 0.0f};
   float beta[2] = {0.0f, 0.0f};
   float A[] = { -0.79f, 0.132f };
   int lda = 1;
   float B[] = { -0.243f, -0.12f, 0.633f, -0.556f };
   int ldb = 1;
   float C[] = { -0.658f, -0.74f, -0.47f, 0.481f };
   int ldc = 1;
   float C_expected[] = { -0.19197f, -0.0948f, 0.50007f, -0.43924f };
   cblas_chemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "chemm(case 1555) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "chemm(case 1555) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int M = 1;
   int N = 2;
   float alpha[2] = {-0.3f, 0.1f};
   float beta[2] = {0.0f, 1.0f};
   float A[] = { -0.114f, -0.515f, -0.513f, -0.527f, -0.995f, 0.986f, 0.229f, -0.076f };
   int lda = 2;
   float B[] = { 0.084f, 0.522f, 0.61f, 0.694f };
   int ldb = 2;
   float C[] = { 0.802f, 0.136f, -0.161f, -0.364f };
   int ldc = 2;
   float C_expected[] = { 0.269101f, 0.716492f, 0.237088f, 0.0290666f };
   cblas_chemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "chemm(case 1556) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "chemm(case 1556) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int M = 1;
   int N = 2;
   float alpha[2] = {-0.3f, 0.1f};
   float beta[2] = {0.0f, 1.0f};
   float A[] = { 0.798f, -0.324f, -0.693f, -0.893f, -0.223f, 0.749f, 0.102f, -0.357f };
   int lda = 2;
   float B[] = { -0.572f, -0.569f, -0.391f, -0.938f };
   int ldb = 1;
   float C[] = { 0.152f, -0.834f, -0.633f, -0.473f };
   int ldc = 1;
   float C_expected[] = { 1.08642f, -0.113853f, 0.234826f, -0.48289f };
   cblas_chemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "chemm(case 1557) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "chemm(case 1557) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int M = 1;
   int N = 2;
   double alpha[2] = {0, 0.1};
   double beta[2] = {0, 0.1};
   double A[] = { -0.359, 0.089 };
   int lda = 1;
   double B[] = { -0.451, -0.337, -0.901, -0.871 };
   int ldb = 2;
   double C[] = { 0.729, 0.631, 0.364, 0.246 };
   int ldc = 2;
   double C_expected[] = { -0.0751983, 0.0890909, -0.0558689, 0.0687459 };
   cblas_zhemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zhemm(case 1558) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zhemm(case 1558) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int M = 1;
   int N = 2;
   double alpha[2] = {0, 0.1};
   double beta[2] = {0, 0.1};
   double A[] = { 0.044, -0.496 };
   int lda = 1;
   double B[] = { -0.674, 0.281, 0.366, 0.888 };
   int ldb = 1;
   double C[] = { -0.9, 0.919, 0.857, -0.049 };
   int ldc = 1;
   double C_expected[] = { -0.0931364, -0.0929656, 0.0009928, 0.0873104 };
   cblas_zhemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zhemm(case 1559) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zhemm(case 1559) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int M = 1;
   int N = 2;
   double alpha[2] = {0, 0};
   double beta[2] = {0, 0.1};
   double A[] = { -0.314, 0.115, 0.114, 0.878, 0.961, -0.224, 0.973, 0.771 };
   int lda = 2;
   double B[] = { 0.5, -0.016, -0.5, 0.149 };
   int ldb = 2;
   double C[] = { -0.054, 0.064, 0.02, 0.245 };
   int ldc = 2;
   double C_expected[] = { -0.0064, -0.0054, -0.0245, 0.002 };
   cblas_zhemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zhemm(case 1560) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zhemm(case 1560) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int M = 1;
   int N = 2;
   double alpha[2] = {0, 0};
   double beta[2] = {0, 0.1};
   double A[] = { 0.186, 0.578, 0.797, -0.957, -0.539, -0.969, -0.21, 0.354 };
   int lda = 2;
   double B[] = { 0.641, -0.968, 0.15, -0.569 };
   int ldb = 1;
   double C[] = { -0.556, -0.9, 0.197, 0.31 };
   int ldc = 1;
   double C_expected[] = { 0.09, -0.0556, -0.031, 0.0197 };
   cblas_zhemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zhemm(case 1561) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zhemm(case 1561) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int M = 1;
   int N = 2;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   double A[] = { 0.323, 0.641 };
   int lda = 1;
   double B[] = { -0.188, 0.091, -0.235, 0.523 };
   int ldb = 2;
   double C[] = { 0.919, 0.806, 0.823, -0.94 };
   int ldc = 2;
   double C_expected[] = { 0.858276, 0.835393, 0.747095, -0.771071 };
   cblas_zhemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zhemm(case 1562) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zhemm(case 1562) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int M = 1;
   int N = 2;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   double A[] = { -0.688, 0.915 };
   int lda = 1;
   double B[] = { 0.914, -0.204, 0.205, -0.476 };
   int ldb = 1;
   double C[] = { 0.27, -0.628, -0.079, 0.507 };
   int ldc = 1;
   double C_expected[] = { -0.358832, -0.487648, -0.22004, 0.834488 };
   cblas_zhemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zhemm(case 1563) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zhemm(case 1563) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int M = 1;
   int N = 2;
   double alpha[2] = {0, 1};
   double beta[2] = {0, 0.1};
   double A[] = { 0.681, 0.574, -0.425, -0.64, 0.792, 0.661, -0.009, 0.005 };
   int lda = 2;
   double B[] = { -0.221, 0.554, -0.465, -0.95 };
   int ldb = 2;
   double C[] = { 0.331, -0.958, -0.826, -0.972 };
   int ldc = 2;
   double C_expected[] = { 0.778291, 0.142269, -0.496199, 0.112747 };
   cblas_zhemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zhemm(case 1564) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zhemm(case 1564) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int M = 1;
   int N = 2;
   double alpha[2] = {0, 1};
   double beta[2] = {0, 0.1};
   double A[] = { 0.959, 0.34, -0.23, 0.064, 0.516, -0.275, 0.714, 0.899 };
   int lda = 2;
   double B[] = { -0.502, -0.987, -0.134, 0.215 };
   int ldb = 1;
   double C[] = { 0.929, 0.181, -0.16, -0.921 };
   int ldc = 1;
   double C_expected[] = { 0.986459, -0.371458, -0.320548, -0.059384 };
   cblas_zhemm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zhemm(case 1565) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zhemm(case 1565) imag");
     };
   };
  };


}
