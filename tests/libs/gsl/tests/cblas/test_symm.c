#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_symm (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int M = 1;
   int N = 2;
   float alpha = -0.3f;
   float beta = -1.0f;
   float A[] = { -0.581f };
   int lda = 1;
   float B[] = { 0.157f, 0.451f };
   int ldb = 2;
   float C[] = { -0.869f, -0.871f };
   int ldc = 2;
   float C_expected[] = { 0.896365f, 0.949609f };
   cblas_ssymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssymm(case 1518)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int M = 1;
   int N = 2;
   float alpha = -0.3f;
   float beta = -1.0f;
   float A[] = { 0.874f };
   int lda = 1;
   float B[] = { 0.085f, 0.069f };
   int ldb = 1;
   float C[] = { -0.495f, -0.828f };
   int ldc = 1;
   float C_expected[] = { 0.472713f, 0.809908f };
   cblas_ssymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssymm(case 1519)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int M = 1;
   int N = 2;
   float alpha = -1.0f;
   float beta = 0.0f;
   float A[] = { -0.671f, -0.343f, 0.6f, 0.177f };
   int lda = 2;
   float B[] = { 0.043f, 0.01f };
   int ldb = 2;
   float C[] = { 0.988f, 0.478f };
   int ldc = 2;
   float C_expected[] = { 0.032283f, 0.012979f };
   cblas_ssymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssymm(case 1520)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int M = 1;
   int N = 2;
   float alpha = -1.0f;
   float beta = 0.0f;
   float A[] = { 0.069f, 0.096f, 0.139f, -0.044f };
   int lda = 2;
   float B[] = { -0.448f, 0.07f };
   int ldb = 1;
   float C[] = { 0.361f, 0.995f };
   int ldc = 1;
   float C_expected[] = { 0.021182f, 0.065352f };
   cblas_ssymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssymm(case 1521)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int M = 1;
   int N = 2;
   float alpha = 0.0f;
   float beta = -0.3f;
   float A[] = { 0.745f };
   int lda = 1;
   float B[] = { -0.269f, 0.448f };
   int ldb = 2;
   float C[] = { -0.986f, 0.2f };
   int ldc = 2;
   float C_expected[] = { 0.2958f, -0.06f };
   cblas_ssymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssymm(case 1522)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int M = 1;
   int N = 2;
   float alpha = 0.0f;
   float beta = -0.3f;
   float A[] = { 0.96f };
   int lda = 1;
   float B[] = { 0.392f, -0.07f };
   int ldb = 1;
   float C[] = { -0.235f, 0.554f };
   int ldc = 1;
   float C_expected[] = { 0.0705f, -0.1662f };
   cblas_ssymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssymm(case 1523)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int M = 1;
   int N = 2;
   float alpha = -0.3f;
   float beta = 0.1f;
   float A[] = { -0.839f, 0.498f, -0.215f, -0.314f };
   int lda = 2;
   float B[] = { -0.66f, 0.593f };
   int ldb = 2;
   float C[] = { -0.806f, 0.525f };
   int ldc = 2;
   float C_expected[] = { -0.208474f, 0.0657906f };
   cblas_ssymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssymm(case 1524)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int M = 1;
   int N = 2;
   float alpha = -0.3f;
   float beta = 0.1f;
   float A[] = { 0.994f, -0.117f, -0.639f, 0.925f };
   int lda = 2;
   float B[] = { -0.478f, 0.147f };
   int ldb = 1;
   float C[] = { -0.814f, 0.316f };
   int ldc = 1;
   float C_expected[] = { 0.0662993f, -0.0259703f };
   cblas_ssymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], flteps, "ssymm(case 1525)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int M = 1;
   int N = 2;
   double alpha = -0.3;
   double beta = 1;
   double A[] = { -0.981 };
   int lda = 1;
   double B[] = { -0.823, 0.83 };
   int ldb = 2;
   double C[] = { 0.991, 0.382 };
   int ldc = 2;
   double C_expected[] = { 0.7487911, 0.626269 };
   cblas_dsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsymm(case 1526)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int M = 1;
   int N = 2;
   double alpha = -0.3;
   double beta = 1;
   double A[] = { -0.248 };
   int lda = 1;
   double B[] = { 0.74, 0.068 };
   int ldb = 1;
   double C[] = { -0.905, 0.742 };
   int ldc = 1;
   double C_expected[] = { -0.849944, 0.7470592 };
   cblas_dsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsymm(case 1527)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int M = 1;
   int N = 2;
   double alpha = -1;
   double beta = 1;
   double A[] = { 0.591, -0.01, -0.192, -0.376 };
   int lda = 2;
   double B[] = { 0.561, 0.946 };
   int ldb = 2;
   double C[] = { 0.763, 0.189 };
   int ldc = 2;
   double C_expected[] = { 0.440909, 0.550306 };
   cblas_dsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsymm(case 1528)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int M = 1;
   int N = 2;
   double alpha = -1;
   double beta = 1;
   double A[] = { -0.786, 0.87, 0.222, -0.043 };
   int lda = 2;
   double B[] = { -0.503, -0.526 };
   int ldb = 1;
   double C[] = { -0.027, -0.391 };
   int ldc = 1;
   double C_expected[] = { -0.305586, -0.301952 };
   cblas_dsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsymm(case 1529)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int M = 1;
   int N = 2;
   double alpha = 0.1;
   double beta = 0.1;
   double A[] = { -0.468 };
   int lda = 1;
   double B[] = { -0.881, 0.692 };
   int ldb = 2;
   double C[] = { -0.812, -0.395 };
   int ldc = 2;
   double C_expected[] = { -0.0399692, -0.0718856 };
   cblas_dsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsymm(case 1530)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int M = 1;
   int N = 2;
   double alpha = 0.1;
   double beta = 0.1;
   double A[] = { 0.849 };
   int lda = 1;
   double B[] = { -0.887, 0.518 };
   int ldb = 1;
   double C[] = { 0.414, -0.251 };
   int ldc = 1;
   double C_expected[] = { -0.0339063, 0.0188782 };
   cblas_dsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsymm(case 1531)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int M = 1;
   int N = 2;
   double alpha = -1;
   double beta = 1;
   double A[] = { 0.457, 0.624, 0.807, 0.349 };
   int lda = 2;
   double B[] = { -0.609, 0.03 };
   int ldb = 2;
   double C[] = { 0.719, -0.624 };
   int ldc = 2;
   double C_expected[] = { 0.973103, -0.143007 };
   cblas_dsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsymm(case 1532)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int M = 1;
   int N = 2;
   double alpha = -1;
   double beta = 1;
   double A[] = { -0.133, -0.117, -0.163, 0.795 };
   int lda = 2;
   double B[] = { -0.882, 0.549 };
   int ldb = 1;
   double C[] = { 0.715, -0.327 };
   int ldc = 1;
   double C_expected[] = { 0.661927, -0.866649 };
   cblas_dsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[i], C_expected[i], dbleps, "dsymm(case 1533)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int M = 1;
   int N = 2;
   float alpha[2] = {-1.0f, 0.0f};
   float beta[2] = {1.0f, 0.0f};
   float A[] = { 0.476f, 0.816f };
   int lda = 1;
   float B[] = { 0.282f, 0.852f, -0.891f, -0.588f };
   int ldb = 2;
   float C[] = { 0.9f, 0.486f, -0.78f, -0.637f };
   int ldc = 2;
   float C_expected[] = { 1.461f, -0.149664f, -0.835692f, 0.369944f };
   cblas_csymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csymm(case 1534) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csymm(case 1534) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int M = 1;
   int N = 2;
   float alpha[2] = {-1.0f, 0.0f};
   float beta[2] = {1.0f, 0.0f};
   float A[] = { 0.048f, 0.172f };
   int lda = 1;
   float B[] = { 0.786f, 0.783f, 0.809f, -0.569f };
   int ldb = 1;
   float C[] = { -0.227f, -0.215f, 0.881f, 0.233f };
   int ldc = 1;
   float C_expected[] = { -0.130052f, -0.387776f, 0.7443f, 0.121164f };
   cblas_csymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csymm(case 1535) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csymm(case 1535) imag");
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
   float beta[2] = {0.0f, 1.0f};
   float A[] = { -0.495f, -0.012f, 0.843f, -0.986f, -0.243f, 0.833f, 0.921f, 0.004f };
   int lda = 2;
   float B[] = { 0.876f, 0.612f, 0.805f, -0.57f };
   int ldb = 2;
   float C[] = { 0.938f, -0.24f, -0.874f, -0.062f };
   int ldc = 2;
   float C_expected[] = { 1.82769f, 0.628319f, 0.93157f, 1.21158f };
   cblas_csymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csymm(case 1536) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csymm(case 1536) imag");
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
   float beta[2] = {0.0f, 1.0f};
   float A[] = { -0.812f, 0.83f, 0.705f, 0.15f, -0.463f, 0.901f, -0.547f, -0.483f };
   int lda = 2;
   float B[] = { -0.808f, -0.664f, 0.352f, -0.102f };
   int ldb = 1;
   float C[] = { -0.64f, 0.399f, 0.896f, -0.163f };
   int ldc = 1;
   float C_expected[] = { -0.631906f, 0.496142f, 0.697798f, 1.62656f };
   cblas_csymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csymm(case 1537) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csymm(case 1537) imag");
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
   float beta[2] = {0.0f, 1.0f};
   float A[] = { 0.342f, -0.906f };
   int lda = 1;
   float B[] = { 0.676f, 0.863f, -0.517f, -0.138f };
   int ldb = 2;
   float C[] = { 0.274f, 0.388f, -0.271f, 0.205f };
   int ldc = 2;
   float C_expected[] = { -1.40107f, 0.59131f, 0.096842f, -0.692206f };
   cblas_csymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csymm(case 1538) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csymm(case 1538) imag");
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
   float beta[2] = {0.0f, 1.0f};
   float A[] = { 0.418f, 0.354f };
   int lda = 1;
   float B[] = { -0.74f, 0.018f, 0.395f, 0.248f };
   int ldb = 1;
   float C[] = { -0.162f, 0.175f, -0.853f, 0.652f };
   int ldc = 1;
   float C_expected[] = { 0.140692f, 0.092436f, -0.729318f, -1.09649f };
   cblas_csymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csymm(case 1539) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csymm(case 1539) imag");
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
   float beta[2] = {0.0f, 0.1f};
   float A[] = { 0.12f, 0.496f, 0.313f, -0.136f, 0.987f, 0.532f, 0.58f, -0.687f };
   int lda = 2;
   float B[] = { -0.587f, 0.278f, 0.857f, 0.136f };
   int ldb = 2;
   float C[] = { 0.162f, 0.249f, -0.665f, 0.456f };
   int ldc = 2;
   float C_expected[] = { -0.22769f, -0.0269913f, 0.0502096f, 0.0841558f };
   cblas_csymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csymm(case 1540) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csymm(case 1540) imag");
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
   float beta[2] = {0.0f, 0.1f};
   float A[] = { 0.579f, -0.859f, 0.192f, -0.737f, 0.396f, -0.498f, 0.751f, -0.379f };
   int lda = 2;
   float B[] = { 0.84f, -0.755f, -0.019f, -0.063f };
   int ldb = 1;
   float C[] = { 0.04f, 0.639f, -0.876f, -0.778f };
   int ldc = 1;
   float C_expected[] = { 0.115459f, 0.329813f, 0.288206f, 0.110315f };
   cblas_csymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "csymm(case 1541) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "csymm(case 1541) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int M = 1;
   int N = 2;
   double alpha[2] = {0, 0};
   double beta[2] = {0, 0};
   double A[] = { 0.511, -0.486 };
   int lda = 1;
   double B[] = { 0.985, -0.923, -0.234, -0.756 };
   int ldb = 2;
   double C[] = { -0.16, 0.049, 0.618, -0.349 };
   int ldc = 2;
   double C_expected[] = { 0.0, 0.0, 0.0, 0.0 };
   cblas_zsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsymm(case 1542) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsymm(case 1542) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int M = 1;
   int N = 2;
   double alpha[2] = {0, 0};
   double beta[2] = {0, 0};
   double A[] = { 0.46, -0.816 };
   int lda = 1;
   double B[] = { 0.404, 0.113, -0.904, -0.627 };
   int ldb = 1;
   double C[] = { 0.114, 0.318, 0.636, -0.839 };
   int ldc = 1;
   double C_expected[] = { 0.0, 0.0, 0.0, 0.0 };
   cblas_zsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsymm(case 1543) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsymm(case 1543) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int M = 1;
   int N = 2;
   double alpha[2] = {-1, 0};
   double beta[2] = {-0.3, 0.1};
   double A[] = { -0.835, 0.344, 0.975, 0.634, 0.312, -0.659, -0.624, -0.175 };
   int lda = 2;
   double B[] = { -0.707, -0.846, 0.825, -0.661 };
   int ldb = 2;
   double C[] = { 0.352, -0.499, 0.267, 0.548 };
   int ldc = 2;
   double C_expected[] = { -2.160518, -0.156877, 0.648536, 0.867299 };
   cblas_zsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsymm(case 1544) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsymm(case 1544) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int M = 1;
   int N = 2;
   double alpha[2] = {-1, 0};
   double beta[2] = {-0.3, 0.1};
   double A[] = { -0.409, 0.013, -0.308, -0.317, -0.535, -0.697, -0.385, 0.119 };
   int lda = 2;
   double B[] = { 0.299, -0.233, 0.093, 0.664 };
   int ldb = 1;
   double C[] = { 0.699, 0.47, -0.347, -0.182 };
   int ldc = 1;
   double C_expected[] = { -0.550491, 0.249777, 0.559487, 0.348221 };
   cblas_zsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsymm(case 1545) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsymm(case 1545) imag");
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
   double beta[2] = {0, 1};
   double A[] = { -0.151, 0.635 };
   int lda = 1;
   double B[] = { 0.711, -0.869, 0.153, 0.647 };
   int ldb = 2;
   double C[] = { -0.299, 0.43, -0.307, 0.133 };
   int ldc = 2;
   double C_expected[] = { 0.014454, 0.283704, -0.566948, -0.307542 };
   cblas_zsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsymm(case 1546) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsymm(case 1546) imag");
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
   double beta[2] = {0, 1};
   double A[] = { 0.793, -0.543 };
   int lda = 1;
   double B[] = { 0.054, -0.045, 0.989, 0.453 };
   int ldb = 1;
   double C[] = { 0.443, -0.641, -0.809, -0.83 };
   int ldc = 1;
   double C_expected[] = { 0.659387, 0.377993, 1.860256, -0.986798 };
   cblas_zsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsymm(case 1547) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsymm(case 1547) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int M = 1;
   int N = 2;
   double alpha[2] = {1, 0};
   double beta[2] = {-1, 0};
   double A[] = { -0.432, -0.293, -0.819, 0.44, -0.818, -0.258, -0.836, 0.683 };
   int lda = 2;
   double B[] = { -0.259, -0.878, 0.161, 0.744 };
   int ldb = 2;
   double C[] = { 0.436, -0.655, -0.61, -0.875 };
   int ldc = 2;
   double C_expected[] = { -0.521112, 0.460053, -0.04741, 1.148005 };
   cblas_zsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsymm(case 1548) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsymm(case 1548) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int M = 1;
   int N = 2;
   double alpha[2] = {1, 0};
   double beta[2] = {-1, 0};
   double A[] = { -0.656, 0.378, -0.688, 0.676, 0.967, -0.804, 0.455, -0.425 };
   int lda = 2;
   double B[] = { 0.791, -0.947, -0.945, -0.444 };
   int ldb = 1;
   double C[] = { 0.014, -0.814, -0.091, -0.417 };
   int ldc = 1;
   double C_expected[] = { 0.775374, 1.400882, -0.431711, 1.802857 };
   cblas_zsymm(order, side, uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zsymm(case 1549) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zsymm(case 1549) imag");
     };
   };
  };


}
