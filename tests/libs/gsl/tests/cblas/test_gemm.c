#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_gemm (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int transA = 111;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha = 1.0f;
   float beta = 0.0f;
   float A[] = { 0.199f, 0.237f, 0.456f, 0.377f };
   int lda = 4;
   float B[] = { 0.842f, -0.734f, 0.323f, -0.957f, -0.303f, -0.873f, -0.871f, -0.819f };
   int ldb = 2;
   float C[] = { 0.498f, -0.925f };
   int ldc = 2;
   float C_expected[] = { -0.222426f, -1.07973f };
   cblas_sgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "sgemm(case 1466)");
     }
   };
  };


  {
   int order = 102;
   int transA = 111;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha = 1.0f;
   float beta = 0.0f;
   float A[] = { -0.83f, 0.922f, -0.228f, -0.003f };
   int lda = 1;
   float B[] = { 0.072f, 0.345f, 0.944f, -0.39f, -0.577f, 0.656f, -0.693f, -0.453f };
   int ldb = 4;
   float C[] = { 0.583f, 0.522f };
   int ldc = 1;
   float C_expected[] = { 0.044268f, 1.24311f };
   cblas_sgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "sgemm(case 1467)");
     }
   };
  };


  {
   int order = 101;
   int transA = 111;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha = 0.1f;
   float beta = 0.1f;
   float A[] = { -0.838f, 0.622f, -0.494f, 0.304f };
   int lda = 4;
   float B[] = { 0.147f, 0.134f, 0.169f, 0.734f, -0.7f, 0.541f, -0.794f, -0.256f };
   int ldb = 4;
   float C[] = { -0.632f, -0.559f };
   int ldc = 2;
   float C_expected[] = { -0.0532188f, 0.0678514f };
   cblas_sgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "sgemm(case 1468)");
     }
   };
  };


  {
   int order = 102;
   int transA = 111;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha = 0.1f;
   float beta = 0.1f;
   float A[] = { -0.937f, 0.635f, 0.596f, -0.51f };
   int lda = 1;
   float B[] = { -0.688f, -0.265f, 0.049f, 0.133f, -0.918f, -0.147f, 0.977f, -0.21f };
   int ldb = 2;
   float C[] = { 0.844f, 0.999f };
   int ldc = 1;
   float C_expected[] = { 0.0474373f, 0.135125f };
   cblas_sgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "sgemm(case 1469)");
     }
   };
  };


  {
   int order = 101;
   int transA = 112;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha = -0.3f;
   float beta = 0.1f;
   float A[] = { -0.165f, 0.638f, 0.346f, -0.697f };
   int lda = 1;
   float B[] = { 0.499f, -0.73f, 0.262f, 0.759f, 0.664f, 0.997f, -0.702f, -0.839f };
   int ldb = 2;
   float C[] = { 0.17f, 0.425f };
   int ldc = 2;
   float C_expected[] = { -0.224158f, -0.417831f };
   cblas_sgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "sgemm(case 1470)");
     }
   };
  };


  {
   int order = 102;
   int transA = 112;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha = -0.3f;
   float beta = 0.1f;
   float A[] = { -0.603f, -0.714f, -0.893f, 0.046f };
   int lda = 4;
   float B[] = { 0.859f, -0.694f, -0.868f, -0.98f, -0.103f, 0.567f, -0.277f, -0.734f };
   int ldb = 4;
   float C[] = { 0.517f, -0.622f };
   int ldc = 1;
   float C_expected[] = { -0.160575f, -0.0234604f };
   cblas_sgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "sgemm(case 1471)");
     }
   };
  };


  {
   int order = 101;
   int transA = 112;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha = 0.1f;
   float beta = 1.0f;
   float A[] = { -0.087f, -0.047f, -0.051f, -0.615f };
   int lda = 1;
   float B[] = { -0.722f, -0.077f, 0.563f, 0.501f, 0.855f, 0.605f, 0.556f, -0.627f };
   int ldb = 4;
   float C[] = { -0.181f, -0.89f };
   int ldc = 2;
   float C_expected[] = { -0.208039f, -0.864557f };
   cblas_sgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "sgemm(case 1472)");
     }
   };
  };


  {
   int order = 102;
   int transA = 112;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha = 0.1f;
   float beta = 1.0f;
   float A[] = { -0.753f, -0.074f, -0.247f, -0.19f };
   int lda = 4;
   float B[] = { 0.061f, 0.743f, 0.22f, -0.682f, 0.733f, 0.417f, 0.772f, 0.665f };
   int ldb = 2;
   float C[] = { -0.253f, 0.972f };
   int ldc = 1;
   float C_expected[] = { -0.291994f, 0.898164f };
   cblas_sgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "sgemm(case 1473)");
     }
   };
  };


  {
   int order = 101;
   int transA = 111;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha = 0;
   double beta = 0;
   double A[] = { 0.017, 0.191, 0.863, -0.97 };
   int lda = 4;
   double B[] = { -0.207, -0.916, -0.278, 0.403, 0.885, 0.409, -0.772, -0.27 };
   int ldb = 2;
   double C[] = { -0.274, -0.858 };
   int ldc = 2;
   double C_expected[] = { 0.0, 0.0 };
   cblas_dgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dgemm(case 1474)");
     }
   };
  };


  {
   int order = 102;
   int transA = 111;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha = 0;
   double beta = 0;
   double A[] = { 0.571, 0.081, 0.109, 0.988 };
   int lda = 1;
   double B[] = { -0.048, -0.753, -0.8, -0.89, -0.535, -0.017, -0.018, -0.544 };
   int ldb = 4;
   double C[] = { -0.876, -0.792 };
   int ldc = 1;
   double C_expected[] = { 0.0, 0.0 };
   cblas_dgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dgemm(case 1475)");
     }
   };
  };


  {
   int order = 101;
   int transA = 111;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha = -0.3;
   double beta = 1;
   double A[] = { 0.939, 0.705, 0.977, 0.4 };
   int lda = 4;
   double B[] = { -0.089, -0.822, 0.937, 0.159, 0.789, -0.413, -0.172, 0.88 };
   int ldb = 4;
   double C[] = { -0.619, 0.063 };
   int ldc = 2;
   double C_expected[] = { -0.7137904, -0.1270986 };
   cblas_dgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dgemm(case 1476)");
     }
   };
  };


  {
   int order = 102;
   int transA = 111;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha = -0.3;
   double beta = 1;
   double A[] = { -0.795, 0.81, 0.388, 0.09 };
   int lda = 1;
   double B[] = { -0.847, 0.031, -0.938, 0.09, -0.286, -0.478, -0.981, 0.881 };
   int ldb = 2;
   double C[] = { -0.242, -0.02 };
   int ldc = 1;
   double C_expected[] = { -0.1562981, -0.0026243 };
   cblas_dgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dgemm(case 1477)");
     }
   };
  };


  {
   int order = 101;
   int transA = 112;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha = -1;
   double beta = 0;
   double A[] = { -0.556, 0.532, 0.746, 0.673 };
   int lda = 1;
   double B[] = { -0.525, 0.967, 0.687, -0.024, 0.527, 0.485, 0.109, -0.46 };
   int ldb = 2;
   double C[] = { -0.495, 0.859 };
   int ldc = 2;
   double C_expected[] = { -1.123883, 0.49819 };
   cblas_dgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dgemm(case 1478)");
     }
   };
  };


  {
   int order = 102;
   int transA = 112;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha = -1;
   double beta = 0;
   double A[] = { -0.358, 0.224, -0.941, 0.513 };
   int lda = 4;
   double B[] = { -0.201, -0.159, -0.586, -0.016, -0.324, 0.411, 0.115, -0.229 };
   int ldb = 4;
   double C[] = { 0.558, 0.596 };
   int ldc = 1;
   double C_expected[] = { -0.57956, 0.017636 };
   cblas_dgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dgemm(case 1479)");
     }
   };
  };


  {
   int order = 101;
   int transA = 112;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha = -0.3;
   double beta = 1;
   double A[] = { -0.586, 0.809, 0.709, -0.524 };
   int lda = 1;
   double B[] = { 0.768, 0.7, 0.619, -0.478, -0.129, -0.778, -0.432, 0.454 };
   int ldb = 4;
   double C[] = { 0.042, 0.252 };
   int ldc = 2;
   double C_expected[] = { -0.1996785, 0.5813976 };
   cblas_dgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dgemm(case 1480)");
     }
   };
  };


  {
   int order = 102;
   int transA = 112;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha = -0.3;
   double beta = 1;
   double A[] = { -0.164, 0.522, 0.948, -0.624 };
   int lda = 4;
   double B[] = { -0.142, 0.778, 0.359, 0.622, -0.637, -0.757, -0.282, -0.805 };
   int ldb = 2;
   double C[] = { -0.09, 0.183 };
   int ldc = 1;
   double C_expected[] = { -0.0248334, 0.1884672 };
   cblas_dgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dgemm(case 1481)");
     }
   };
  };


  {
   int order = 101;
   int transA = 111;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {0.0f, 0.0f};
   float A[] = { -0.082f, -0.281f, -0.096f, 0.913f, 0.974f, -0.706f, -0.773f, 0.522f };
   int lda = 4;
   float B[] = { 0.745f, -0.664f, 0.352f, -0.733f, 0.304f, -0.555f, -0.493f, -0.089f, 0.188f, 0.631f, 0.235f, 0.152f, -0.299f, -0.731f, -0.686f, -0.332f };
   int ldb = 2;
   float C[] = { -0.179f, -0.284f, -0.996f, -0.414f };
   int ldc = 2;
   float C_expected[] = { -1.06679f, 1.47116f, 0.599689f, 0.933532f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1482) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1482) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 111;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {0.0f, 0.0f};
   float A[] = { 0.044f, -0.33f, 0.279f, 0.712f, -0.363f, -0.788f, -0.768f, -0.551f };
   int lda = 1;
   float B[] = { 0.138f, 0.927f, -0.178f, -0.864f, 0.888f, 0.844f, -0.199f, 0.706f, -0.034f, 0.483f, 0.499f, 0.664f, 0.648f, 0.324f, 0.97f, 0.609f };
   int ldb = 4;
   float C[] = { -0.129f, 0.842f, 0.214f, -0.626f };
   int ldc = 1;
   float C_expected[] = { 1.81122f, 1.76205f, 1.0574f, -0.564966f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1483) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1483) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 111;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 0.0f};
   float beta[2] = {-1.0f, 0.0f};
   float A[] = { 0.812f, -0.471f, 0.241f, 0.795f, 0.439f, 0.131f, -0.636f, 0.531f };
   int lda = 4;
   float B[] = { 0.062f, 0.807f, 0.873f, 0.372f, 0.239f, 0.804f, 0.537f, -0.954f, -0.396f, 0.838f, 0.081f, 0.15f, 0.489f, -0.438f, 0.165f, 0.429f };
   int ldb = 4;
   float C[] = { 0.868f, 0.329f, -0.509f, 0.724f };
   int ldc = 2;
   float C_expected[] = { -0.868f, -0.329f, 0.509f, -0.724f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1484) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1484) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 111;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 0.0f};
   float beta[2] = {-1.0f, 0.0f};
   float A[] = { 0.832f, 0.198f, 0.794f, -0.522f, -0.319f, 0.578f, 0.332f, 0.746f };
   int lda = 1;
   float B[] = { -0.361f, 0.187f, -0.163f, -0.781f, 0.536f, 0.888f, -0.969f, 0.899f, 0.961f, -0.583f, 0.753f, 0.29f, -0.997f, 0.729f, -0.352f, -0.2f };
   int ldb = 2;
   float C[] = { 0.864f, 0.735f, -0.074f, -0.228f };
   int ldc = 1;
   float C_expected[] = { -0.864f, -0.735f, 0.074f, 0.228f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1485) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1485) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 111;
   int transB = 113;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {0.0f, 0.1f};
   float A[] = { 0.149f, 0.187f, 0.263f, -0.715f, -0.882f, -0.907f, 0.87f, -0.527f };
   int lda = 4;
   float B[] = { -0.915f, -0.249f, -0.986f, -0.799f, -0.136f, 0.712f, 0.964f, 0.799f, -0.569f, 0.686f, 0.603f, 0.758f, 0.161f, -0.698f, -0.263f, -0.256f };
   int ldb = 4;
   float C[] = { 0.622f, -0.824f, -0.482f, -0.161f };
   int ldc = 2;
   float C_expected[] = { -0.246901f, 0.083044f, 1.25556f, 0.009106f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1486) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1486) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 111;
   int transB = 113;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {0.0f, 0.1f};
   float A[] = { 0.963f, -0.943f, -0.734f, -0.253f, 0.832f, 0.545f, -0.815f, -0.434f };
   int lda = 1;
   float B[] = { 0.23f, -0.211f, 0.906f, 0.232f, -0.339f, 0.597f, -0.919f, 0.793f, 0.535f, 0.526f, 0.119f, 0.053f, 0.751f, 0.044f, 0.752f, -0.469f };
   int ldb = 2;
   float C[] = { 0.483f, -0.266f, -0.224f, -0.692f };
   int ldc = 1;
   float C_expected[] = { -0.047537f, 0.667177f, 1.02025f, 0.823778f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1487) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1487) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 112;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {-0.3f, 0.1f};
   float beta[2] = {-1.0f, 0.0f};
   float A[] = { -0.657f, -0.497f, -0.293f, -0.168f, -0.943f, -0.181f, 0.569f, 0.91f };
   int lda = 1;
   float B[] = { -0.047f, 0.796f, -0.913f, 0.998f, 0.365f, 0.467f, -0.627f, -0.523f, 0.885f, 0.234f, -0.494f, 0.071f, -0.361f, -0.154f, -0.055f, -0.32f };
   int ldb = 2;
   float C[] = { 0.956f, 0.268f, 0.152f, 0.717f };
   int ldc = 2;
   float C_expected[] = { -0.668685f, 0.134477f, -0.715786f, -0.478065f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1488) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1488) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 112;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {-0.3f, 0.1f};
   float beta[2] = {-1.0f, 0.0f};
   float A[] = { 0.394f, -0.482f, 0.631f, -0.833f, 0.221f, 0.672f, 0.2f, 0.967f };
   int lda = 4;
   float B[] = { 0.708f, 0.695f, 0.111f, -0.912f, 0.376f, 0.606f, -0.997f, -0.741f, 0.349f, 0.543f, 0.372f, -0.563f, 0.129f, -0.295f, -0.672f, -0.95f };
   int ldb = 4;
   float C[] = { 0.436f, 0.752f, 0.074f, 0.209f };
   int ldc = 1;
   float C_expected[] = { -0.325083f, -0.301952f, -0.283022f, 0.339919f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1489) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1489) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 112;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {1.0f, 0.0f};
   float beta[2] = {-0.3f, 0.1f};
   float A[] = { 0.827f, -0.862f, 0.373f, -0.265f, -0.9f, 0.892f, -0.319f, 0.151f };
   int lda = 1;
   float B[] = { 0.603f, 0.816f, -0.511f, 0.831f, -0.36f, -0.954f, -0.978f, 0.485f, 0.675f, 0.186f, 0.463f, 0.144f, 0.851f, -0.458f, 0.766f, -0.213f };
   int ldb = 4;
   float C[] = { -0.335f, 0.333f, -0.4f, 0.422f };
   int ldc = 2;
   float C_expected[] = { 2.7126f, 0.702111f, 0.437661f, 0.691294f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1490) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1490) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 112;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {1.0f, 0.0f};
   float beta[2] = {-0.3f, 0.1f};
   float A[] = { 0.966f, 0.476f, -0.013f, -0.655f, 0.773f, -0.543f, -0.231f, -0.353f };
   int lda = 4;
   float B[] = { -0.684f, 0.144f, 0.018f, -0.77f, -0.688f, 0.909f, -0.094f, -0.938f, -0.757f, 0.574f, -0.479f, 0.473f, 0.0f, 0.064f, -0.168f, 0.858f };
   int ldb = 2;
   float C[] = { -0.912f, 0.54f, 0.756f, 0.024f };
   int ldc = 1;
   float C_expected[] = { -0.156236f, 0.839112f, -0.230206f, -0.106256f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1491) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1491) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 112;
   int transB = 113;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 0.0f};
   float beta[2] = {1.0f, 0.0f};
   float A[] = { 0.66f, -0.113f, -0.663f, -0.856f, 0.614f, -0.344f, -0.964f, -0.532f };
   int lda = 1;
   float B[] = { -0.606f, -0.965f, -0.279f, -0.312f, 0.63f, 0.967f, 0.041f, -0.557f, 0.663f, 0.619f, -0.134f, 0.261f, -0.388f, 0.525f, 0.222f, 0.538f };
   int ldb = 4;
   float C[] = { 0.114f, -0.376f, -0.851f, -0.682f };
   int ldc = 2;
   float C_expected[] = { 0.114f, -0.376f, -0.851f, -0.682f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1492) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1492) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 112;
   int transB = 113;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 0.0f};
   float beta[2] = {1.0f, 0.0f};
   float A[] = { 0.212f, -0.752f, 0.679f, 0.49f, -0.029f, -0.488f, 0.567f, 0.374f };
   int lda = 4;
   float B[] = { -0.914f, 0.734f, -0.845f, 0.059f, -0.297f, 0.152f, -0.417f, -0.669f, 0.831f, -0.544f, 0.022f, 0.102f, -0.379f, -0.357f, -0.394f, -0.588f };
   int ldb = 2;
   float C[] = { -0.584f, 0.373f, 0.235f, 0.521f };
   int ldc = 1;
   float C_expected[] = { -0.584f, 0.373f, 0.235f, 0.521f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1493) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1493) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 113;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {-1.0f, 0.0f};
   float A[] = { 0.135f, 0.128f, 0.909f, -0.963f, 0.299f, -0.944f, 0.944f, 0.942f };
   int lda = 1;
   float B[] = { 0.924f, -0.317f, -0.992f, -0.854f, -0.435f, 0.102f, 0.126f, 0.862f, 0.952f, 0.68f, 0.545f, 0.168f, 0.752f, 0.549f, 0.687f, -0.76f };
   int ldb = 2;
   float C[] = { -0.369f, -0.33f, 0.849f, -0.632f };
   int ldc = 2;
   float C_expected[] = { 0.326537f, 0.37603f, -0.86067f, 0.529817f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1494) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1494) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 113;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {-1.0f, 0.0f};
   float A[] = { 0.061f, -0.271f, -0.043f, -0.023f, 0.694f, 0.333f, 0.733f, -0.967f };
   int lda = 4;
   float B[] = { 0.088f, -0.607f, 0.589f, 0.375f, -0.897f, -0.954f, -0.216f, -0.195f, -0.865f, -0.511f, -0.219f, 0.535f, 0.976f, 0.582f, 0.464f, -0.041f };
   int ldb = 4;
   float C[] = { 0.533f, -0.63f, 0.405f, 0.667f };
   int ldc = 1;
   float C_expected[] = { -0.459906f, 0.552595f, -0.425391f, -0.533626f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1495) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1495) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 113;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 0.0f};
   float beta[2] = {1.0f, 0.0f};
   float A[] = { -0.676f, -0.116f, 0.707f, -0.256f, -0.893f, -0.966f, 0.159f, -0.246f };
   int lda = 1;
   float B[] = { 0.059f, 0.281f, -0.93f, -0.263f, 0.583f, -0.11f, 0.639f, -0.96f, -0.878f, 0.984f, 0.058f, 0.977f, -0.567f, 0.561f, -0.048f, -0.798f };
   int ldb = 4;
   float C[] = { 0.362f, -0.808f, 0.428f, -0.112f };
   int ldc = 2;
   float C_expected[] = { 0.362f, -0.808f, 0.428f, -0.112f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1496) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1496) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 113;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 0.0f};
   float beta[2] = {1.0f, 0.0f};
   float A[] = { -0.915f, 0.439f, 0.171f, -0.019f, 0.843f, 0.944f, -0.581f, 0.856f };
   int lda = 4;
   float B[] = { -0.284f, 0.207f, -0.27f, 0.832f, 0.894f, -0.626f, -0.305f, -0.006f, 0.562f, -0.744f, -0.533f, 0.126f, -0.375f, -0.333f, 0.275f, 0.748f };
   int ldb = 2;
   float C[] = { -0.763f, -0.829f, 0.708f, -0.613f };
   int ldc = 1;
   float C_expected[] = { -0.763f, -0.829f, 0.708f, -0.613f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1497) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1497) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 113;
   int transB = 113;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {0.0f, 1.0f};
   float A[] = { 0.496f, -0.9f, 0.825f, -0.678f, 0.41f, -0.585f, -0.264f, 0.308f };
   int lda = 1;
   float B[] = { 0.907f, 0.972f, -0.724f, 0.745f, -0.601f, 0.589f, 0.759f, -0.521f, -0.161f, -0.321f, 0.341f, -0.981f, -0.378f, -0.671f, -0.314f, -0.878f };
   int ldb = 4;
   float C[] = { -0.293f, 0.07f, 0.087f, -0.542f };
   int ldc = 2;
   float C_expected[] = { 0.10357f, -0.163927f, 0.444626f, -0.0076744f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1498) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1498) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 113;
   int transB = 113;
   int M = 1;
   int N = 2;
   int K = 4;
   float alpha[2] = {0.0f, 0.1f};
   float beta[2] = {0.0f, 1.0f};
   float A[] = { -0.225f, -0.629f, -0.939f, -0.836f, -0.841f, -0.794f, 0.836f, -0.65f };
   int lda = 4;
   float B[] = { 0.869f, -0.453f, 0.8f, -0.947f, 0.545f, 0.716f, -0.507f, -0.228f, 0.722f, 0.372f, 0.77f, 0.317f, -0.153f, -0.524f, -0.465f, -0.684f };
   int ldb = 2;
   float C[] = { -0.896f, 0.91f, -0.973f, -0.269f };
   int ldc = 1;
   float C_expected[] = { -1.18974f, -1.0134f, 0.189027f, -1.14494f };
   cblas_cgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cgemm(case 1499) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cgemm(case 1499) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 111;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {1, 0};
   double beta[2] = {-1, 0};
   double A[] = { -0.33, 0.457, 0.428, -0.19, 0.86, -0.53, 0.058, -0.942 };
   int lda = 4;
   double B[] = { 0.434, 0.653, -0.124, 0.191, -0.112, -0.84, -0.72, 0.075, -0.503, -0.109, 0.3, -0.898, 0.489, 0.384, 0.993, -0.804 };
   int ldb = 2;
   double C[] = { -0.792, -0.155, -0.608, -0.243 };
   int ldc = 2;
   double C_expected[] = { 0.042563, -0.465908, -0.649991, -1.621116 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1500) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1500) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 111;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {1, 0};
   double beta[2] = {-1, 0};
   double A[] = { 0.726, -0.438, -0.23, -0.054, -0.019, 0.902, -0.883, -0.235 };
   int lda = 1;
   double B[] = { 0.159, -0.18, 0.386, -0.167, 0.971, -0.072, 0.87, -0.839, 0.474, 0.956, -0.235, 0.332, 0.826, -0.056, -0.941, 0.01 };
   int ldb = 4;
   double C[] = { -0.799, 0.973, -0.549, -0.177 };
   int ldc = 1;
   double C_expected[] = { -0.181084, 0.257841, 2.251901, 1.558195 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1501) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1501) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 111;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {0, 0.1};
   double beta[2] = {1, 0};
   double A[] = { 0.109, 0.892, -0.723, 0.793, 0.109, -0.419, -0.534, 0.448 };
   int lda = 4;
   double B[] = { -0.875, -0.31, -0.027, 0.067, 0.274, -0.126, -0.548, 0.497, 0.681, 0.388, 0.909, 0.889, 0.982, -0.074, -0.788, 0.233 };
   int ldb = 4;
   double C[] = { 0.503, 0.067, 0.239, 0.876 };
   int ldc = 2;
   double C_expected[] = { 0.6553584, 0.0864583, 0.2559136, 0.7518389 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1502) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1502) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 111;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {0, 0.1};
   double beta[2] = {1, 0};
   double A[] = { 0.334, 0.192, -0.992, -0.168, 0.154, -0.75, -0.797, -0.76 };
   int lda = 1;
   double B[] = { -0.82, 0.147, -0.237, 0.68, 0.317, 0.257, -0.406, -0.802, 0.058, 0.012, -0.832, 0.949, -0.263, -0.085, -0.064, 0.492 };
   int ldb = 2;
   double C[] = { 0.079, -0.602, -0.392, 0.316 };
   int ldc = 1;
   double C_expected[] = { 0.0980569, -0.6430449, -0.539207, 0.4226848 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1503) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1503) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 111;
   int transB = 113;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {0, 0};
   double beta[2] = {-1, 0};
   double A[] = { -0.305, -0.698, -0.072, -0.383, 0.364, -0.656, 0.819, 0.194 };
   int lda = 4;
   double B[] = { 0.682, 0.498, -0.389, 0.923, -0.853, -0.558, -0.722, -0.085, -0.27, 0.026, -0.107, -0.036, 0.644, -0.327, -0.894, 0.34 };
   int ldb = 4;
   double C[] = { 0.981, -0.336, -0.377, -0.41 };
   int ldc = 2;
   double C_expected[] = { -0.981, 0.336, 0.377, 0.41 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1504) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1504) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 111;
   int transB = 113;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {0, 0};
   double beta[2] = {-1, 0};
   double A[] = { -0.306, -0.709, -0.196, 0.285, 0.873, -0.802, 0.715, -0.179 };
   int lda = 1;
   double B[] = { 0.028, 0.109, 0.87, -0.446, 0.735, 0.731, 0.021, -0.186, 0.541, 0.97, -0.333, 0.002, -0.089, -0.01, 0.331, 0.851 };
   int ldb = 2;
   double C[] = { 0.902, -0.584, -0.695, -0.607 };
   int ldc = 1;
   double C_expected[] = { -0.902, 0.584, 0.695, 0.607 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1505) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1505) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 112;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {-1, 0};
   double beta[2] = {1, 0};
   double A[] = { 0.517, -0.136, 0.72, -0.237, 0.121, -0.66, 0.005, 0.759 };
   int lda = 1;
   double B[] = { -0.606, 0.049, 0.807, -0.236, -0.258, -0.412, 0.75, -0.659, 0.993, -0.029, -0.968, 0.707, -0.362, -0.005, 0.096, -0.241 };
   int ldb = 2;
   double C[] = { 0.63, 0.922, 0.025, -0.535 };
   int ldc = 2;
   double C_expected[] = { 1.117044, 1.983417, -1.276831, -0.447092 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1506) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1506) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 112;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {-1, 0};
   double beta[2] = {1, 0};
   double A[] = { 0.064, 0.371, -0.01, -0.262, 0.143, -0.081, 0.1, -0.062 };
   int lda = 4;
   double B[] = { -0.749, 0.289, -0.239, -0.226, 0.284, 0.668, 0.305, 0.075, -0.36, 0.166, -0.416, 0.234, -0.267, 0.525, 0.116, -0.561 };
   int ldb = 4;
   double C[] = { 0.671, 0.763, 0.444, -0.246 };
   int ldc = 1;
   double C_expected[] = { 0.753107, 0.896395, 0.481996, -0.263126 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1507) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1507) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 112;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {1, 0};
   double beta[2] = {-0.3, 0.1};
   double A[] = { -0.956, -0.751, 0.671, -0.633, 0.648, -0.042, 0.948, 0.826 };
   int lda = 1;
   double B[] = { 0.921, 0.506, -0.609, 0.817, -0.686, 0.991, 0.616, -0.482, -0.02, -0.34, 0.559, 0.976, 0.431, 0.385, -0.164, -0.778 };
   int ldb = 4;
   double C[] = { 0.074, -0.01, 0.165, 0.166 };
   int ldc = 2;
   double C_expected[] = { 0.166046, 0.491557, 1.473191, -0.033821 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1508) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1508) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 112;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {1, 0};
   double beta[2] = {-0.3, 0.1};
   double A[] = { -0.698, -0.062, 0.023, 0.704, 0.443, -0.46, 0.541, 0.296 };
   int lda = 4;
   double B[] = { 0.787, -0.199, 0.835, -0.276, -0.515, 0.467, -0.76, -0.483, 0.015, -0.394, -0.748, 0.02, 0.573, 0.3, -0.088, -0.238 };
   int ldb = 2;
   double C[] = { 0.935, -0.655, -0.797, 0.071 };
   int ldc = 1;
   double C_expected[] = { -1.070679, 0.178755, -0.344714, -0.308137 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1509) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1509) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 112;
   int transB = 113;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {0, 0.1};
   double beta[2] = {-0.3, 0.1};
   double A[] = { -0.202, -0.219, 0.741, 0.527, 0.054, 0.16, -0.359, 0.338 };
   int lda = 1;
   double B[] = { -0.872, 0.995, 0.722, 0.618, -0.27, 0.939, -0.743, 0.547, -0.864, 0.376, -0.997, -0.63, 0.887, -0.454, 0.436, -0.039 };
   int ldb = 4;
   double C[] = { -0.684, 0.463, -0.386, -0.524 };
   int ldc = 2;
   double C_expected[] = { 0.1423153, -0.066679, 0.1175618, 0.0012949 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1510) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1510) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 112;
   int transB = 113;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {0, 0.1};
   double beta[2] = {-0.3, 0.1};
   double A[] = { -0.855, -0.173, -0.679, 0.824, 0.469, 0.786, 0.757, -0.109 };
   int lda = 4;
   double B[] = { 0.483, -0.888, -0.757, 0.551, -0.81, 0.23, -0.078, 0.725, -0.592, 0.394, 0.884, 0.802, -0.813, -0.016, -0.853, 0.783 };
   int ldb = 2;
   double C[] = { 0.181, -0.368, -0.864, -0.784 };
   int ldc = 1;
   double C_expected[] = { 0.1728438, 0.1183508, 0.2526999, 0.3004174 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1511) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1511) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 113;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {-1, 0};
   double beta[2] = {-0.3, 0.1};
   double A[] = { 0.446, -0.65, -0.724, 0.014, 0.792, -0.695, -0.81, -0.358 };
   int lda = 1;
   double B[] = { -0.08, 0.216, 0.689, 0.699, 0.073, -0.346, 0.821, -0.668, -0.798, 0.869, 0.451, -0.061, -0.41, 0.316, 0.104, -0.514 };
   int ldb = 2;
   double C[] = { -0.476, 0.211, -0.912, -0.243 };
   int ldc = 2;
   double C_expected[] = { 1.372475, -0.135616, 0.549353, -1.968747 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1512) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1512) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 113;
   int transB = 111;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {-1, 0};
   double beta[2] = {-0.3, 0.1};
   double A[] = { 0.669, 0.046, -0.094, 0.666, 0.23, 0.448, -0.795, -0.142 };
   int lda = 4;
   double B[] = { 0.037, -0.154, -0.739, 0.905, 0.793, -0.53, -0.34, 0.428, 0.072, -0.263, -0.603, -0.905, 0.681, -0.083, -0.511, -0.337 };
   int ldb = 4;
   double C[] = { 0.247, 0.575, -0.836, -0.883 };
   int ldc = 1;
   double C_expected[] = { -0.975939, 0.415528, 0.275533, 0.002716 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1513) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1513) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 113;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {0, 0};
   double beta[2] = {-1, 0};
   double A[] = { 0.369, 0.506, 0.217, -0.739, -0.395, 0.16, -0.329, -0.954 };
   int lda = 1;
   double B[] = { -0.622, -0.945, 0.416, -0.884, 0.797, -0.74, 0.519, -0.789, -0.348, 0.563, -0.398, -0.956, 0.227, 0.84, -0.079, 0.847 };
   int ldb = 4;
   double C[] = { 0.833, 0.761, 0.074, -0.448 };
   int ldc = 2;
   double C_expected[] = { -0.833, -0.761, -0.074, 0.448 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1514) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1514) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 113;
   int transB = 112;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {0, 0};
   double beta[2] = {-1, 0};
   double A[] = { -0.141, 0.275, 0.717, 0.775, -0.701, -0.689, -0.883, -0.077 };
   int lda = 4;
   double B[] = { -0.526, -0.437, 0.133, -0.209, -0.83, 0.328, 0.916, -0.337, 0.762, -0.664, -0.566, 0.955, 0.168, 0.488, -0.172, -0.535 };
   int ldb = 2;
   double C[] = { -0.88, 0.945, 0.416, 0.99 };
   int ldc = 1;
   double C_expected[] = { 0.88, -0.945, -0.416, -0.99 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1515) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1515) imag");
     };
   };
  };


  {
   int order = 101;
   int transA = 113;
   int transB = 113;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {-0.3, 0.1};
   double beta[2] = {0, 0.1};
   double A[] = { -0.534, -0.013, -0.258, -0.31, -0.211, -0.883, -0.89, -0.499 };
   int lda = 1;
   double B[] = { -0.185, -0.798, -0.34, 0.716, 0.035, 0.968, -0.26, 0.784, -0.889, -0.344, -0.685, -0.647, -0.764, 0.03, 0.626, -0.989 };
   int ldb = 4;
   double C[] = { -0.793, -0.551, 0.182, 0.838 };
   int ldc = 2;
   double C_expected[] = { -0.5507177, -0.0286821, 0.2222276, 0.5197398 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1516) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1516) imag");
     };
   };
  };


  {
   int order = 102;
   int transA = 113;
   int transB = 113;
   int M = 1;
   int N = 2;
   int K = 4;
   double alpha[2] = {-0.3, 0.1};
   double beta[2] = {0, 0.1};
   double A[] = { 0.575, -0.128, -0.702, 0.758, 0.383, -0.914, 0.157, 0.368 };
   int lda = 4;
   double B[] = { 0.572, -0.841, 0.223, -0.334, -0.823, -0.84, 0.671, -0.871, 0.241, 0.927, -0.344, 0.281, -0.034, -0.104, 0.587, -0.329 };
   int ldb = 2;
   double C[] = { -0.612, 0.167, 0.647, 0.447 };
   int ldc = 1;
   double C_expected[] = { -0.7876717, 0.0341179, -0.0800018, 0.5717566 };
   cblas_zgemm(order, transA, transB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zgemm(case 1517) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zgemm(case 1517) imag");
     };
   };
  };


}
