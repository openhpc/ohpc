#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_her2k (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int uplo = 121;
   int trans = 111;
   int N = 1;
   int K = 2;
   float alpha[2] = {-1.0f, 0.0f};
   float beta = -0.3f;
   float A[] = { 0.178f, 0.545f, -0.491f, 0.979f };
   int lda = 2;
   float B[] = { -0.665f, -0.531f, -0.4f, 0.227f };
   int ldb = 2;
   float C[] = { 0.115f, -0.193f };
   int ldc = 1;
   float C_expected[] = { -0.056236f, 0.0f };
   cblas_cher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cher2k(case 1646) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cher2k(case 1646) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 111;
   int N = 1;
   int K = 2;
   float alpha[2] = {-1.0f, 0.0f};
   float beta = -0.3f;
   float A[] = { -0.808f, 0.447f, 0.145f, -0.226f };
   int lda = 2;
   float B[] = { -0.413f, 0.904f, -0.585f, 0.717f };
   int ldb = 2;
   float C[] = { -0.725f, -0.244f };
   int ldc = 1;
   float C_expected[] = { -0.76435f, 0.0f };
   cblas_cher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cher2k(case 1647) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cher2k(case 1647) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 111;
   int N = 1;
   int K = 2;
   float alpha[2] = {-1.0f, 0.0f};
   float beta = -0.3f;
   float A[] = { 0.337f, -0.737f, -0.993f, 0.69f };
   int lda = 1;
   float B[] = { -0.39f, -0.836f, -0.32f, 0.368f };
   int ldb = 1;
   float C[] = { 0.844f, -0.763f };
   int ldc = 1;
   float C_expected[] = { -2.36596f, 0.0f };
   cblas_cher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cher2k(case 1648) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cher2k(case 1648) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 111;
   int N = 1;
   int K = 2;
   float alpha[2] = {-1.0f, 0.0f};
   float beta = -0.3f;
   float A[] = { 0.386f, -0.465f, 0.719f, -0.378f };
   int lda = 1;
   float B[] = { 0.099f, -0.879f, 0.864f, 0.141f };
   int ldb = 1;
   float C[] = { -0.599f, -0.47f };
   int ldc = 1;
   float C_expected[] = { -1.85003f, 0.0f };
   cblas_cher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cher2k(case 1649) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cher2k(case 1649) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 113;
   int N = 1;
   int K = 2;
   float alpha[2] = {0.0f, 1.0f};
   float beta = -1.0f;
   float A[] = { 0.128f, 0.431f, -0.26f, 0.75f };
   int lda = 1;
   float B[] = { 0.276f, 0.058f, 0.904f, -0.116f };
   int ldb = 1;
   float C[] = { 0.914f, -0.262f };
   int ldc = 1;
   float C_expected[] = { 0.604744f, 0.0f };
   cblas_cher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cher2k(case 1650) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cher2k(case 1650) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 113;
   int N = 1;
   int K = 2;
   float alpha[2] = {0.0f, 1.0f};
   float beta = -1.0f;
   float A[] = { 0.72f, 0.783f, -0.737f, 0.375f };
   int lda = 1;
   float B[] = { 0.531f, 0.167f, 0.203f, -0.221f };
   int ldb = 1;
   float C[] = { 0.618f, 0.392f };
   int ldc = 1;
   float C_expected[] = { -0.200438f, 0.0f };
   cblas_cher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cher2k(case 1651) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cher2k(case 1651) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 113;
   int N = 1;
   int K = 2;
   float alpha[2] = {0.0f, 1.0f};
   float beta = -1.0f;
   float A[] = { -0.372f, -0.735f, -0.711f, 0.051f };
   int lda = 2;
   float B[] = { 0.257f, 0.097f, 0.338f, -0.484f };
   int ldb = 2;
   float C[] = { -0.142f, -0.197f };
   int ldc = 1;
   float C_expected[] = { -0.817394f, 0.0f };
   cblas_cher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cher2k(case 1652) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cher2k(case 1652) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 113;
   int N = 1;
   int K = 2;
   float alpha[2] = {0.0f, 1.0f};
   float beta = -1.0f;
   float A[] = { 0.1f, -0.878f, 0.28f, -0.381f };
   int lda = 2;
   float B[] = { -0.208f, 0.309f, -0.276f, 0.123f };
   int ldb = 2;
   float C[] = { 0.483f, -0.541f };
   int ldc = 1;
   float C_expected[] = { -0.03812f, 0.0f };
   cblas_cher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cher2k(case 1653) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cher2k(case 1653) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 111;
   int N = 1;
   int K = 2;
   double alpha[2] = {1, 0};
   double beta = 1;
   double A[] = { 0.515, -0.034, 0.067, 0.66 };
   int lda = 2;
   double B[] = { 0.408, -0.85, -0.945, -0.799 };
   int ldb = 2;
   double C[] = { -0.918, -0.985 };
   int ldc = 1;
   double C_expected[] = { -1.62127, 0.0 };
   cblas_zher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zher2k(case 1654) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zher2k(case 1654) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 111;
   int N = 1;
   int K = 2;
   double alpha[2] = {1, 0};
   double beta = 1;
   double A[] = { -0.009, 0.495, -0.008, -0.973 };
   int lda = 2;
   double B[] = { -0.239, -0.373, -0.032, -0.539 };
   int ldb = 2;
   double C[] = { 0.443, -0.245 };
   int ldc = 1;
   double C_expected[] = { 1.127438, 0.0 };
   cblas_zher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zher2k(case 1655) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zher2k(case 1655) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 111;
   int N = 1;
   int K = 2;
   double alpha[2] = {1, 0};
   double beta = 1;
   double A[] = { 0.531, 0.721, -0.848, 0.826 };
   int lda = 1;
   double B[] = { -0.711, -0.2, -0.92, -0.676 };
   int ldb = 1;
   double C[] = { -0.447, 0.701 };
   int ldc = 1;
   double C_expected[] = { -1.046914, 0.0 };
   cblas_zher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zher2k(case 1656) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zher2k(case 1656) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 111;
   int N = 1;
   int K = 2;
   double alpha[2] = {1, 0};
   double beta = 1;
   double A[] = { 0.68, 0.079, 0.837, -0.814 };
   int lda = 1;
   double B[] = { -0.986, 0.024, 0.584, -0.248 };
   int ldb = 1;
   double C[] = { 0.477, -0.551 };
   int ldc = 1;
   double C_expected[] = { 0.521192, 0.0 };
   cblas_zher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zher2k(case 1657) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zher2k(case 1657) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 113;
   int N = 1;
   int K = 2;
   double alpha[2] = {-1, 0};
   double beta = 0.1;
   double A[] = { -0.63, 0.787, 0.426, -0.568 };
   int lda = 1;
   double B[] = { -0.228, 0.302, 0.83, 0.023 };
   int ldb = 1;
   double C[] = { 0.354, -0.85 };
   int ldc = 1;
   double C_expected[] = { -1.40826, 0.0 };
   cblas_zher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zher2k(case 1658) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zher2k(case 1658) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 113;
   int N = 1;
   int K = 2;
   double alpha[2] = {-1, 0};
   double beta = 0.1;
   double A[] = { 0.224, -0.191, 0.46, 0.464 };
   int lda = 1;
   double B[] = { -0.815, 0.634, 0.066, -0.873 };
   int ldb = 1;
   double C[] = { -0.49, -0.606 };
   int ldc = 1;
   double C_expected[] = { 1.307732, 0.0 };
   cblas_zher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zher2k(case 1659) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zher2k(case 1659) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 113;
   int N = 1;
   int K = 2;
   double alpha[2] = {-1, 0};
   double beta = 0.1;
   double A[] = { 0.943, 0.075, 0.15, -0.141 };
   int lda = 2;
   double B[] = { -0.962, 0.422, -0.592, -0.789 };
   int ldb = 2;
   double C[] = { 0.728, 0.601 };
   int ldc = 1;
   double C_expected[] = { 1.778934, 0.0 };
   cblas_zher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zher2k(case 1660) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zher2k(case 1660) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 113;
   int N = 1;
   int K = 2;
   double alpha[2] = {-1, 0};
   double beta = 0.1;
   double A[] = { -0.93, -0.386, 0.565, 0.141 };
   int lda = 2;
   double B[] = { -0.801, 0.022, 0.558, -0.932 };
   int ldb = 2;
   double C[] = { 0.068, 0.501 };
   int ldc = 1;
   double C_expected[] = { -1.833792, 0.0 };
   cblas_zher2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zher2k(case 1661) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zher2k(case 1661) imag");
     };
   };
  };


}
