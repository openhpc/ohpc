#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_syr2k (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int uplo = 121;
   int trans = 111;
   int N = 1;
   int K = 2;
   float alpha = 0.1f;
   float beta = 1.0f;
   float A[] = { -0.915f, 0.445f };
   int lda = 2;
   float B[] = { 0.213f, -0.194f };
   int ldb = 2;
   float C[] = { -0.117f };
   int ldc = 1;
   float C_expected[] = { -0.173245f };
   cblas_ssyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyr2k(case 1614)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 111;
   int N = 1;
   int K = 2;
   float alpha = 0.1f;
   float beta = 1.0f;
   float A[] = { 0.089f, -0.889f };
   int lda = 2;
   float B[] = { -0.384f, 0.518f };
   int ldb = 2;
   float C[] = { 0.069f };
   int ldc = 1;
   float C_expected[] = { -0.0299356f };
   cblas_ssyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyr2k(case 1615)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 111;
   int N = 1;
   int K = 2;
   float alpha = 0.1f;
   float beta = 1.0f;
   float A[] = { 0.492f, 0.021f };
   int lda = 1;
   float B[] = { -0.804f, -0.912f };
   int ldb = 1;
   float C[] = { -0.851f };
   int ldc = 1;
   float C_expected[] = { -0.933944f };
   cblas_ssyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyr2k(case 1616)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 111;
   int N = 1;
   int K = 2;
   float alpha = 0.1f;
   float beta = 1.0f;
   float A[] = { -0.376f, 0.689f };
   int lda = 1;
   float B[] = { 0.21f, 0.406f };
   int ldb = 1;
   float C[] = { -0.581f };
   int ldc = 1;
   float C_expected[] = { -0.540845f };
   cblas_ssyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyr2k(case 1617)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 112;
   int N = 1;
   int K = 2;
   float alpha = 1.0f;
   float beta = -0.3f;
   float A[] = { 0.629f, -0.883f };
   int lda = 1;
   float B[] = { -0.165f, 0.02f };
   int ldb = 1;
   float C[] = { 0.236f };
   int ldc = 1;
   float C_expected[] = { -0.31369f };
   cblas_ssyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyr2k(case 1618)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 112;
   int N = 1;
   int K = 2;
   float alpha = 1.0f;
   float beta = -0.3f;
   float A[] = { 0.412f, -0.411f };
   int lda = 1;
   float B[] = { 0.313f, 0.301f };
   int ldb = 1;
   float C[] = { 0.222f };
   int ldc = 1;
   float C_expected[] = { -0.05611f };
   cblas_ssyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyr2k(case 1619)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 112;
   int N = 1;
   int K = 2;
   float alpha = 1.0f;
   float beta = -0.3f;
   float A[] = { -0.02f, 0.593f };
   int lda = 2;
   float B[] = { -0.144f, 0.846f };
   int ldb = 2;
   float C[] = { -0.645f };
   int ldc = 1;
   float C_expected[] = { 1.20262f };
   cblas_ssyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyr2k(case 1620)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 112;
   int N = 1;
   int K = 2;
   float alpha = 1.0f;
   float beta = -0.3f;
   float A[] = { 0.253f, 0.937f };
   int lda = 2;
   float B[] = { 0.24f, -0.27f };
   int ldb = 2;
   float C[] = { 0.128f };
   int ldc = 1;
   float C_expected[] = { -0.42294f };
   cblas_ssyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyr2k(case 1621)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 111;
   int N = 1;
   int K = 2;
   double alpha = 0.1;
   double beta = 0;
   double A[] = { -0.225, 0.857 };
   int lda = 2;
   double B[] = { -0.933, 0.994 };
   int ldb = 2;
   double C[] = { 0.177 };
   int ldc = 1;
   double C_expected[] = { 0.2123566 };
   cblas_dsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyr2k(case 1622)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 111;
   int N = 1;
   int K = 2;
   double alpha = 0.1;
   double beta = 0;
   double A[] = { -0.955, 0.112 };
   int lda = 2;
   double B[] = { -0.695, 0.719 };
   int ldb = 2;
   double C[] = { 0.069 };
   int ldc = 1;
   double C_expected[] = { 0.1488506 };
   cblas_dsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyr2k(case 1623)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 111;
   int N = 1;
   int K = 2;
   double alpha = 0.1;
   double beta = 0;
   double A[] = { 0.216, 0.911 };
   int lda = 1;
   double B[] = { -0.074, -0.256 };
   int ldb = 1;
   double C[] = { -0.621 };
   int ldc = 1;
   double C_expected[] = { -0.04984 };
   cblas_dsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyr2k(case 1624)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 111;
   int N = 1;
   int K = 2;
   double alpha = 0.1;
   double beta = 0;
   double A[] = { -0.343, -0.381 };
   int lda = 1;
   double B[] = { -0.433, -0.087 };
   int ldb = 1;
   double C[] = { -0.889 };
   int ldc = 1;
   double C_expected[] = { 0.0363332 };
   cblas_dsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyr2k(case 1625)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 112;
   int N = 1;
   int K = 2;
   double alpha = 1;
   double beta = -1;
   double A[] = { -0.633, 0.219 };
   int lda = 1;
   double B[] = { 0.817, -0.683 };
   int ldb = 1;
   double C[] = { -0.294 };
   int ldc = 1;
   double C_expected[] = { -1.039476 };
   cblas_dsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyr2k(case 1626)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 112;
   int N = 1;
   int K = 2;
   double alpha = 1;
   double beta = -1;
   double A[] = { -0.887, -0.43 };
   int lda = 1;
   double B[] = { 0.557, 0.912 };
   int ldb = 1;
   double C[] = { 0.831 };
   int ldc = 1;
   double C_expected[] = { -2.603438 };
   cblas_dsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyr2k(case 1627)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 112;
   int N = 1;
   int K = 2;
   double alpha = 1;
   double beta = -1;
   double A[] = { 0.397, -0.173 };
   int lda = 2;
   double B[] = { 0.155, -0.99 };
   int ldb = 2;
   double C[] = { 0.621 };
   int ldc = 1;
   double C_expected[] = { -0.15539 };
   cblas_dsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyr2k(case 1628)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 112;
   int N = 1;
   int K = 2;
   double alpha = 1;
   double beta = -1;
   double A[] = { 0.833, -0.52 };
   int lda = 2;
   double B[] = { 0.28, 0.481 };
   int ldb = 2;
   double C[] = { 0.455 };
   int ldc = 1;
   double C_expected[] = { -0.48876 };
   cblas_dsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyr2k(case 1629)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 111;
   int N = 1;
   int K = 2;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {0.0f, 0.0f};
   float A[] = { -0.248f, -0.037f, -0.124f, 0.998f };
   int lda = 2;
   float B[] = { -0.608f, -0.115f, -0.718f, -0.551f };
   int ldb = 2;
   float C[] = { 0.187f, -0.329f };
   int ldc = 1;
   float C_expected[] = { 0.119445f, 0.157092f };
   cblas_csyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyr2k(case 1630) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyr2k(case 1630) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 111;
   int N = 1;
   int K = 2;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {0.0f, 0.0f};
   float A[] = { 0.068f, 0.751f, -0.449f, -0.598f };
   int lda = 2;
   float B[] = { 0.616f, 0.805f, -0.635f, 0.773f };
   int ldb = 2;
   float C[] = { -0.287f, 0.917f };
   int ldc = 1;
   float C_expected[] = { -0.110002f, 0.0369404f };
   cblas_csyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyr2k(case 1631) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyr2k(case 1631) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 111;
   int N = 1;
   int K = 2;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {0.0f, 0.0f};
   float A[] = { -0.396f, -0.603f, -0.131f, -0.288f };
   int lda = 1;
   float B[] = { -0.64f, -0.444f, -0.085f, 0.936f };
   int ldb = 1;
   float C[] = { 0.375f, -0.434f };
   int ldc = 1;
   float C_expected[] = { -0.0927216f, 0.0532822f };
   cblas_csyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyr2k(case 1632) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyr2k(case 1632) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 111;
   int N = 1;
   int K = 2;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {0.0f, 0.0f};
   float A[] = { 0.655f, 0.16f, 0.45f, -0.747f };
   int lda = 1;
   float B[] = { 0.923f, 0.432f, -0.986f, 0.259f };
   int ldb = 1;
   float C[] = { 0.752f, 0.576f };
   int ldc = 1;
   float C_expected[] = { -0.256746f, 0.0570436f };
   cblas_csyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyr2k(case 1633) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyr2k(case 1633) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 112;
   int N = 1;
   int K = 2;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {0.0f, 0.0f};
   float A[] = { -0.765f, 0.487f, 0.7f, 0.768f };
   int lda = 1;
   float B[] = { -0.529f, 0.056f, -0.584f, 0.928f };
   int ldb = 1;
   float C[] = { -0.426f, 0.836f };
   int ldc = 1;
   float C_expected[] = { 0.019875f, -0.148818f };
   cblas_csyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyr2k(case 1634) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyr2k(case 1634) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 112;
   int N = 1;
   int K = 2;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {0.0f, 0.0f};
   float A[] = { 0.25f, 0.489f, 0.8f, -0.642f };
   int lda = 1;
   float B[] = { -0.732f, -0.856f, -0.654f, 0.591f };
   int ldb = 1;
   float C[] = { -0.101f, 0.322f };
   int ldc = 1;
   float C_expected[] = { -0.064144f, 0.0183612f };
   cblas_csyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyr2k(case 1635) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyr2k(case 1635) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 112;
   int N = 1;
   int K = 2;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {0.0f, 0.0f};
   float A[] = { -0.579f, -0.971f, 0.521f, -0.824f };
   int lda = 2;
   float B[] = { -0.227f, 0.907f, 0.457f, -0.274f };
   int ldb = 2;
   float C[] = { 0.21f, -0.718f };
   int ldc = 1;
   float C_expected[] = { 0.164812f, 0.20489f };
   cblas_csyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyr2k(case 1636) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyr2k(case 1636) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 112;
   int N = 1;
   int K = 2;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {0.0f, 0.0f};
   float A[] = { -0.83f, -0.512f, -0.667f, -0.436f };
   int lda = 2;
   float B[] = { -0.443f, 0.82f, -0.259f, -0.618f };
   int ldb = 2;
   float C[] = { 0.583f, 0.668f };
   int ldc = 1;
   float C_expected[] = { -0.0142692f, 0.138167f };
   cblas_csyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyr2k(case 1637) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyr2k(case 1637) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 111;
   int N = 1;
   int K = 2;
   double alpha[2] = {0, 0};
   double beta[2] = {-0.3, 0.1};
   double A[] = { -0.315, 0.03, 0.281, 0.175 };
   int lda = 2;
   double B[] = { -0.832, -0.964, 0.291, 0.476 };
   int ldb = 2;
   double C[] = { -0.341, 0.743 };
   int ldc = 1;
   double C_expected[] = { 0.028, -0.257 };
   cblas_zsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyr2k(case 1638) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyr2k(case 1638) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 111;
   int N = 1;
   int K = 2;
   double alpha[2] = {0, 0};
   double beta[2] = {-0.3, 0.1};
   double A[] = { -0.159, -0.489, -0.11, 0.611 };
   int lda = 2;
   double B[] = { -0.285, -0.048, -0.673, -0.492 };
   int ldb = 2;
   double C[] = { 0.496, -0.626 };
   int ldc = 1;
   double C_expected[] = { -0.0862, 0.2374 };
   cblas_zsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyr2k(case 1639) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyr2k(case 1639) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 111;
   int N = 1;
   int K = 2;
   double alpha[2] = {0, 0};
   double beta[2] = {-0.3, 0.1};
   double A[] = { 0.796, 0.872, -0.919, 0.748 };
   int lda = 1;
   double B[] = { -0.945, 0.915, -0.252, -0.276 };
   int ldb = 1;
   double C[] = { 0.07, -0.957 };
   int ldc = 1;
   double C_expected[] = { 0.0747, 0.2941 };
   cblas_zsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyr2k(case 1640) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyr2k(case 1640) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 111;
   int N = 1;
   int K = 2;
   double alpha[2] = {0, 0};
   double beta[2] = {-0.3, 0.1};
   double A[] = { 0.984, 0.526, 0.284, 0.806 };
   int lda = 1;
   double B[] = { -0.509, -0.178, 0.188, -0.221 };
   int ldb = 1;
   double C[] = { -0.388, 0.795 };
   int ldc = 1;
   double C_expected[] = { 0.0369, -0.2773 };
   cblas_zsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyr2k(case 1641) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyr2k(case 1641) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 112;
   int N = 1;
   int K = 2;
   double alpha[2] = {1, 0};
   double beta[2] = {0, 0.1};
   double A[] = { 0.628, 0.846, -0.645, 0.032 };
   int lda = 1;
   double B[] = { 0.545, -0.54, 0.493, -0.035 };
   int ldb = 1;
   double C[] = { -0.16, -0.06 };
   int ldc = 1;
   double C_expected[] = { 0.97047, 0.304602 };
   cblas_zsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyr2k(case 1642) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyr2k(case 1642) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 112;
   int N = 1;
   int K = 2;
   double alpha[2] = {1, 0};
   double beta[2] = {0, 0.1};
   double A[] = { -0.556, -0.946, 0.177, -0.859 };
   int lda = 1;
   double B[] = { 0.423, -0.91, 0.736, -0.251 };
   int ldb = 1;
   double C[] = { -0.478, 0.519 };
   int ldc = 1;
   double C_expected[] = { -2.41467, -1.189498 };
   cblas_zsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyr2k(case 1643) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyr2k(case 1643) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 112;
   int N = 1;
   int K = 2;
   double alpha[2] = {1, 0};
   double beta[2] = {0, 0.1};
   double A[] = { -0.582, 0.09, -0.176, 0.784 };
   int lda = 2;
   double B[] = { 0.687, -0.859, 0.945, 0.756 };
   int ldb = 2;
   double C[] = { -0.663, -0.186 };
   int ldc = 1;
   double C_expected[] = { -2.144496, 2.272884 };
   cblas_zsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyr2k(case 1644) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyr2k(case 1644) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 112;
   int N = 1;
   int K = 2;
   double alpha[2] = {1, 0};
   double beta[2] = {0, 0.1};
   double A[] = { 0.231, -0.452, -0.112, -0.837 };
   int lda = 2;
   double B[] = { -0.258, 0.464, -0.224, 0.893 };
   int ldb = 2;
   double C[] = { -0.448, 0.046 };
   int ldc = 1;
   double C_expected[] = { 1.840718, 0.577744 };
   cblas_zsyr2k(order, uplo, trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyr2k(case 1645) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyr2k(case 1645) imag");
     };
   };
  };


}
