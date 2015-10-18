#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_syrk (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int uplo = 121;
   int trans = 111;
   int N = 2;
   int K = 1;
   float alpha = -1.0f;
   float beta = 0.1f;
   float A[] = { 0.412f, -0.229f };
   int lda = 1;
   float C[] = { 0.628f, -0.664f, -0.268f, 0.096f };
   int ldc = 2;
   float C_expected[] = { -0.106944f, 0.027948f, -0.268f, -0.042841f };
   cblas_ssyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyrk(case 1566)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 111;
   int N = 2;
   int K = 1;
   float alpha = -1.0f;
   float beta = 0.1f;
   float A[] = { 0.101f, -0.653f };
   int lda = 2;
   float C[] = { 0.432f, 0.107f, -0.952f, -0.532f };
   int ldc = 2;
   float C_expected[] = { 0.032999f, 0.107f, -0.029247f, -0.479609f };
   cblas_ssyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyrk(case 1567)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 112;
   int N = 2;
   int K = 1;
   float alpha = 1.0f;
   float beta = 0.1f;
   float A[] = { 0.79f, 0.595f };
   int lda = 2;
   float C[] = { 0.257f, 0.183f, -0.021f, -0.053f };
   int ldc = 2;
   float C_expected[] = { 0.6498f, 0.48835f, -0.021f, 0.348725f };
   cblas_ssyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyrk(case 1568)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 112;
   int N = 2;
   int K = 1;
   float alpha = 1.0f;
   float beta = 0.1f;
   float A[] = { -0.181f, -0.654f };
   int lda = 1;
   float C[] = { -0.4f, 0.615f, 0.147f, -0.163f };
   int ldc = 2;
   float C_expected[] = { -0.007239f, 0.615f, 0.133074f, 0.411416f };
   cblas_ssyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyrk(case 1569)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 111;
   int N = 2;
   int K = 1;
   float alpha = 0.0f;
   float beta = -1.0f;
   float A[] = { -0.191f, 0.584f };
   int lda = 1;
   float C[] = { -0.719f, -0.681f, -0.003f, 0.544f };
   int ldc = 2;
   float C_expected[] = { 0.719f, -0.681f, 0.003f, -0.544f };
   cblas_ssyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyrk(case 1570)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 111;
   int N = 2;
   int K = 1;
   float alpha = 0.0f;
   float beta = -1.0f;
   float A[] = { 0.788f, 0.041f };
   int lda = 2;
   float C[] = { 0.029f, 0.365f, 0.739f, -0.769f };
   int ldc = 2;
   float C_expected[] = { -0.029f, -0.365f, 0.739f, 0.769f };
   cblas_ssyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyrk(case 1571)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 112;
   int N = 2;
   int K = 1;
   float alpha = -0.3f;
   float beta = -1.0f;
   float A[] = { 0.733f, 0.678f };
   int lda = 2;
   float C[] = { -0.941f, 0.96f, 0.07f, -0.295f };
   int ldc = 2;
   float C_expected[] = { 0.779813f, 0.96f, -0.219092f, 0.157095f };
   cblas_ssyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyrk(case 1572)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 112;
   int N = 2;
   int K = 1;
   float alpha = -0.3f;
   float beta = -1.0f;
   float A[] = { -0.87f, 0.675f };
   int lda = 1;
   float C[] = { -0.602f, -0.432f, -0.984f, 0.384f };
   int ldc = 2;
   float C_expected[] = { 0.37493f, 0.608175f, -0.984f, -0.520687f };
   cblas_ssyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssyrk(case 1573)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 111;
   int N = 2;
   int K = 1;
   double alpha = 0.1;
   double beta = -0.3;
   double A[] = { 0.169, -0.875 };
   int lda = 1;
   double C[] = { 0.159, 0.277, 0.865, 0.346 };
   int ldc = 2;
   double C_expected[] = { -0.0448439, -0.0978875, 0.865, -0.0272375 };
   cblas_dsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyrk(case 1574)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 111;
   int N = 2;
   int K = 1;
   double alpha = 0.1;
   double beta = -0.3;
   double A[] = { 0.536, -0.725 };
   int lda = 2;
   double C[] = { 0.154, -0.445, -0.841, -0.91 };
   int ldc = 2;
   double C_expected[] = { -0.0174704, -0.445, 0.21344, 0.3255625 };
   cblas_dsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyrk(case 1575)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 112;
   int N = 2;
   int K = 1;
   double alpha = 0;
   double beta = -1;
   double A[] = { -0.07, 0.8 };
   int lda = 2;
   double C[] = { 0.823, -0.88, -0.136, 0.793 };
   int ldc = 2;
   double C_expected[] = { -0.823, 0.88, -0.136, -0.793 };
   cblas_dsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyrk(case 1576)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 112;
   int N = 2;
   int K = 1;
   double alpha = 0;
   double beta = -1;
   double A[] = { -0.058, 0.649 };
   int lda = 1;
   double C[] = { -0.187, 0.294, -0.004, -0.933 };
   int ldc = 2;
   double C_expected[] = { 0.187, 0.294, 0.004, 0.933 };
   cblas_dsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyrk(case 1577)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 111;
   int N = 2;
   int K = 1;
   double alpha = 1;
   double beta = -1;
   double A[] = { 0.263, -0.289 };
   int lda = 1;
   double C[] = { 0.554, -0.679, 0.993, 0.758 };
   int ldc = 2;
   double C_expected[] = { -0.484831, -0.679, -1.069007, -0.674479 };
   cblas_dsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyrk(case 1578)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 111;
   int N = 2;
   int K = 1;
   double alpha = 1;
   double beta = -1;
   double A[] = { -0.265, -0.837 };
   int lda = 2;
   double C[] = { -0.994, 0.967, -0.34, -0.069 };
   int ldc = 2;
   double C_expected[] = { 1.064225, -0.745195, -0.34, 0.769569 };
   cblas_dsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyrk(case 1579)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 112;
   int N = 2;
   int K = 1;
   double alpha = -0.3;
   double beta = 1;
   double A[] = { -0.464, 0.394 };
   int lda = 2;
   double C[] = { -0.45, -0.447, 0.649, 0.055 };
   int ldc = 2;
   double C_expected[] = { -0.5145888, -0.447, 0.7038448, 0.0084292 };
   cblas_dsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyrk(case 1580)");
     }
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 112;
   int N = 2;
   int K = 1;
   double alpha = -0.3;
   double beta = 1;
   double A[] = { 0.815, 0.168 };
   int lda = 1;
   double C[] = { 0.817, -0.957, -0.395, -0.382 };
   int ldc = 2;
   double C_expected[] = { 0.6177325, -0.998076, -0.395, -0.3904672 };
   cblas_dsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsyrk(case 1581)");
     }
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 111;
   int N = 2;
   int K = 1;
   float alpha[2] = {0.0f, 0.0f};
   float beta[2] = {-0.3f, 0.1f};
   float A[] = { 0.447f, -0.507f, -0.425f, 0.701f };
   int lda = 1;
   float C[] = { 0.16f, -0.245f, 0.922f, -0.437f, 0.24f, 0.008f, -0.095f, 0.749f };
   int ldc = 2;
   float C_expected[] = { -0.0235f, 0.0895f, -0.2329f, 0.2233f, 0.24f, 0.008f, -0.0464f, -0.2342f };
   cblas_csyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyrk(case 1582) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyrk(case 1582) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 111;
   int N = 2;
   int K = 1;
   float alpha[2] = {0.0f, 0.0f};
   float beta[2] = {-0.3f, 0.1f};
   float A[] = { -0.421f, -0.435f, -0.914f, -0.493f };
   int lda = 2;
   float C[] = { -0.761f, -0.38f, 0.043f, -0.999f, 0.779f, 0.238f, 0.082f, 0.394f };
   int ldc = 2;
   float C_expected[] = { 0.2663f, 0.0379f, 0.043f, -0.999f, -0.2575f, 0.0065f, -0.064f, -0.11f };
   cblas_csyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyrk(case 1583) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyrk(case 1583) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 112;
   int N = 2;
   int K = 1;
   float alpha[2] = {-1.0f, 0.0f};
   float beta[2] = {-0.3f, 0.1f};
   float A[] = { 0.827f, -0.896f, 0.417f, 0.865f };
   int lda = 2;
   float C[] = { -0.349f, -0.31f, 0.972f, 0.794f, -0.906f, -0.595f, -0.089f, -0.333f };
   int ldc = 2;
   float C_expected[] = { 0.254587f, 1.54008f, -1.4909f, -0.482723f, -0.906f, -0.595f, 0.634336f, -0.63041f };
   cblas_csyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyrk(case 1584) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyrk(case 1584) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 112;
   int N = 2;
   int K = 1;
   float alpha[2] = {-1.0f, 0.0f};
   float beta[2] = {-0.3f, 0.1f};
   float A[] = { 0.607f, 0.747f, -0.889f, 0.333f };
   int lda = 1;
   float C[] = { 0.244f, 0.564f, 0.009f, 0.578f, -0.827f, 0.558f, -0.337f, 0.731f };
   int ldc = 2;
   float C_expected[] = { 0.05996f, -1.05166f, 0.009f, 0.578f, 0.980674f, 0.211852f, -0.651432f, 0.339074f };
   cblas_csyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyrk(case 1585) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyrk(case 1585) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 111;
   int N = 2;
   int K = 1;
   float alpha[2] = {1.0f, 0.0f};
   float beta[2] = {0.0f, 1.0f};
   float A[] = { 0.784f, -0.281f, -0.88f, 0.479f };
   int lda = 1;
   float C[] = { 0.491f, 0.531f, 0.805f, -0.097f, 0.728f, 0.674f, -0.705f, -0.754f };
   int ldc = 2;
   float C_expected[] = { 0.004695f, 0.050392f, 0.805f, -0.097f, -1.22932f, 1.35082f, 1.29896f, -1.54804f };
   cblas_csyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyrk(case 1586) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyrk(case 1586) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 111;
   int N = 2;
   int K = 1;
   float alpha[2] = {1.0f, 0.0f};
   float beta[2] = {0.0f, 1.0f};
   float A[] = { 0.272f, -0.146f, 0.155f, 0.038f };
   int lda = 2;
   float C[] = { 0.533f, -0.41f, -0.904f, 0.301f, -0.836f, 0.57f, -0.374f, -0.293f };
   int ldc = 2;
   float C_expected[] = { 0.462668f, 0.453576f, -0.253292f, -0.916294f, -0.836f, 0.57f, 0.315581f, -0.36222f };
   cblas_csyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyrk(case 1587) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyrk(case 1587) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 112;
   int N = 2;
   int K = 1;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {-1.0f, 0.0f};
   float A[] = { -0.055f, -0.127f, -0.896f, -0.625f };
   int lda = 2;
   float C[] = { -0.619f, 0.511f, -0.877f, 0.557f, -0.801f, -0.437f, -0.922f, 0.332f };
   int ldc = 2;
   float C_expected[] = { 0.60503f, -0.524104f, -0.877f, 0.557f, 0.652833f, 0.406905f, -0.198f, 0.080191f };
   cblas_csyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyrk(case 1588) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyrk(case 1588) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 112;
   int N = 2;
   int K = 1;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {-1.0f, 0.0f};
   float A[] = { -0.528f, 0.759f, -0.079f, 0.952f };
   int lda = 1;
   float C[] = { 0.775f, 0.855f, 0.786f, 0.525f, 0.85f, 0.044f, 0.658f, 0.947f };
   int ldc = 2;
   float C_expected[] = { 0.026504f, -1.1523f, -0.223383f, -1.20586f, 0.85f, 0.044f, -0.507584f, -1.84706f };
   cblas_csyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csyrk(case 1589) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csyrk(case 1589) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 111;
   int N = 2;
   int K = 1;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   double A[] = { -0.049, -0.687, -0.434, 0.294 };
   int lda = 1;
   double C[] = { 0.937, -0.113, 0.796, 0.293, 0.876, -0.199, -0.757, -0.103 };
   int ldc = 2;
   double C_expected[] = { 0.467432, -0.045674, 1.019244, 0.576752, 0.876, -0.199, -0.65508, -0.358192 };
   cblas_zsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyrk(case 1590) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyrk(case 1590) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 111;
   int N = 2;
   int K = 1;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   double A[] = { 0.359, -0.364, 0.926, -0.69 };
   int lda = 2;
   double C[] = { 0.306, 0.249, 0.28, 0.229, 0.866, 0.092, 0.886, -0.283 };
   int ldc = 2;
   double C_expected[] = { 0.302385, -0.012352, 0.28, 0.229, 0.947274, -0.492774, 1.267376, -1.56088 };
   cblas_zsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyrk(case 1591) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyrk(case 1591) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 112;
   int N = 2;
   int K = 1;
   double alpha[2] = {-0.3, 0.1};
   double beta[2] = {0, 0};
   double A[] = { 0.607, 0.555, -0.85, 0.831 };
   int lda = 2;
   double C[] = { 0.069, 0.368, 0.551, -0.912, -0.243, -0.063, -0.924, 0.192 };
   int ldc = 2;
   double C_expected[] = { -0.0855042, -0.1960886, 0.2898798, -0.1075156, -0.243, -0.063, 0.1316883, 0.4270039 };
   cblas_zsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyrk(case 1592) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyrk(case 1592) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 112;
   int N = 2;
   int K = 1;
   double alpha[2] = {-0.3, 0.1};
   double beta[2] = {0, 0};
   double A[] = { 0.427, 0.86, -0.136, 0.002 };
   int lda = 1;
   double C[] = { 0.398, -0.47, 0.011, -0.547, -0.106, 0.016, 0.681, 0.246 };
   int ldc = 2;
   double C_expected[] = { 0.0937373, -0.2760591, 0.011, -0.547, 0.0295482, 0.0288526, -0.0054932, 0.0020124 };
   cblas_zsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyrk(case 1593) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyrk(case 1593) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 111;
   int N = 2;
   int K = 1;
   double alpha[2] = {-0.3, 0.1};
   double beta[2] = {1, 0};
   double A[] = { 0.718, 0.023, 0.355, -0.492 };
   int lda = 1;
   double C[] = { -0.637, -0.727, -0.475, -0.776, 0.802, -0.55, -0.837, 0.222 };
   int ldc = 2;
   double C_expected[] = { -0.7948013, -0.6854089, -0.475, -0.776, 0.7566473, -0.4198521, -0.7672563, 0.3151921 };
   cblas_zsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyrk(case 1594) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyrk(case 1594) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 111;
   int N = 2;
   int K = 1;
   double alpha[2] = {-0.3, 0.1};
   double beta[2] = {1, 0};
   double A[] = { 0.209, 0.139, -0.202, -0.223 };
   int lda = 2;
   double C[] = { -0.695, 0.524, 0.212, -0.88, -0.752, 0.291, 0.684, -0.124 };
   int ldc = 2;
   double C_expected[] = { -0.7081182, 0.5090054, 0.2228348, -0.8587166, -0.752, 0.291, 0.6776683, -0.1519201 };
   cblas_zsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyrk(case 1595) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyrk(case 1595) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 112;
   int N = 2;
   int K = 1;
   double alpha[2] = {-0.3, 0.1};
   double beta[2] = {1, 0};
   double A[] = { -0.365, -0.624, 0.632, 0.348 };
   int lda = 2;
   double C[] = { 0.877, 0.927, -0.377, 0.967, 0.008, 0.292, -0.779, 0.794 };
   int ldc = 2;
   double C_expected[] = { 0.9082933, 0.7647289, -0.377, 0.967, 0.0641972, 0.4470636, -0.9064832, 0.6898704 };
   cblas_zsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyrk(case 1596) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyrk(case 1596) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 112;
   int N = 2;
   int K = 1;
   double alpha[2] = {-0.3, 0.1};
   double beta[2] = {1, 0};
   double A[] = { -0.067, -0.586, 0.208, 0.331 };
   int lda = 1;
   double C[] = { 0.584, -0.454, 0.93, 0.782, 0.489, -0.278, 0.081, -0.919 };
   int ldc = 2;
   double C_expected[] = { 0.6778197, -0.5114479, 0.8903975, 0.8432225, 0.489, -0.278, 0.0871195, -0.9669385 };
   cblas_zsyrk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsyrk(case 1597) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsyrk(case 1597) imag");
     };
   };
  };


}
