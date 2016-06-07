#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_trsm (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { -0.279f, 0.058f, 0.437f, 0.462f };
   int lda = 2;
   float B[] = { 0.578f, 0.473f, -0.34f, -0.128f, 0.503f, 0.2f };
   int ldb = 3;
   float B_expected[] = { 0.638784f, 0.440702f, -0.392589f, 0.0831169f, -0.326623f, -0.12987f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1822)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { -0.735f, -0.861f, 0.772f, -0.242f };
   int lda = 2;
   float B[] = { -0.793f, -0.162f, -0.844f, 0.143f, -0.379f, -0.46f };
   int ldb = 3;
   float B_expected[] = { 0.200963f, 0.146496f, 0.372018f, -0.0429f, 0.1137f, 0.138f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1823)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { -0.498f, 0.777f, -0.913f, 0.779f };
   int lda = 2;
   float B[] = { -0.831f, -0.663f, -0.098f, -0.894f, -0.059f, 0.468f };
   int ldb = 3;
   float B_expected[] = { -0.500602f, -0.399398f, -0.0590361f, -0.242426f, -0.445379f, -0.249422f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1824)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { -0.543f, 0.095f, -0.933f, -0.669f };
   int lda = 2;
   float B[] = { 0.068f, 0.715f, 0.012f, -0.785f, 0.378f, 0.251f };
   int ldb = 3;
   float B_expected[] = { -0.0204f, -0.2145f, -0.0036f, 0.216467f, -0.313528f, -0.0786588f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1825)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { 0.75f, 0.777f, -0.025f, 0.572f };
   int lda = 2;
   float B[] = { 0.03f, 0.392f, -0.056f, 0.399f, -0.489f, -0.167f };
   int ldb = 2;
   float B_expected[] = { -0.0188531f, -0.205594f, 0.0154245f, -0.209266f, 0.19852f, 0.0875874f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1826)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { 0.899f, -0.447f, 0.338f, -0.74f };
   int lda = 2;
   float B[] = { 0.964f, -0.104f, -0.199f, 0.503f, -0.386f, -0.764f };
   int ldb = 2;
   float B_expected[] = { -0.299746f, 0.0312f, 0.110704f, -0.1509f, 0.0383304f, 0.2292f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1827)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { 0.279f, 0.73f, -0.366f, 0.583f };
   int lda = 2;
   float B[] = { -0.572f, 0.75f, 0.603f, 0.697f, 0.908f, 0.119f };
   int ldb = 2;
   float B_expected[] = { 0.615054f, -1.15607f, -0.648387f, 0.453212f, -0.976344f, 1.16129f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1828)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { 0.581f, -0.911f, 0.438f, 0.731f };
   int lda = 2;
   float B[] = { 0.519f, 0.831f, 0.822f, 0.182f, 0.571f, -0.357f };
   int ldb = 2;
   float B_expected[] = { -0.1557f, -0.391143f, -0.2466f, -0.279253f, -0.1713f, -0.0489543f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1829)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { 0.065f, 0.099f, 0.48f, 0.746f, -0.739f, 0.695f, 0.197f, 0.621f, 0.063f };
   int lda = 3;
   float B[] = { 0.01f, -0.612f, 0.756f, -0.225f, 0.546f, 0.432f };
   int ldb = 3;
   float B_expected[] = { -0.0461538f, -0.254627f, -0.439373f, 1.03846f, 0.360768f, -13.9491f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1830)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { -0.86f, -0.653f, 0.87f, -0.037f, 0.788f, 0.015f, 0.028f, -0.804f, -0.357f };
   int lda = 3;
   float B[] = { -0.546f, 0.892f, -0.085f, -0.541f, -0.207f, 0.765f };
   int ldb = 3;
   float B_expected[] = { 0.1638f, -0.160639f, -0.114596f, 0.1623f, 0.168082f, -0.373222f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1831)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { 0.872f, -0.35f, 0.518f, -0.8f, -0.13f, -0.832f, 0.426f, 0.195f, -0.735f };
   int lda = 3;
   float B[] = { 0.773f, 0.069f, 0.45f, 0.189f, 0.504f, 0.996f };
   int ldb = 3;
   float B_expected[] = { 0.0431742f, 0.434741f, 0.183673f, 1.36286f, 1.77287f, 0.406531f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1832)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { -0.053f, -0.132f, -0.515f, -0.411f, 0.134f, 0.657f, 0.072f, -0.007f, -0.34f };
   int lda = 3;
   float B[] = { 0.494f, 0.072f, -0.882f, -0.112f, 0.904f, 0.755f };
   int ldb = 3;
   float B_expected[] = { -0.175368f, -0.0197478f, 0.2646f, -0.0622068f, -0.272786f, -0.2265f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1833)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { -0.154f, -0.54f, 0.146f, -0.106f, -0.478f, 0.938f, -0.731f, 0.25f, -0.4f };
   int lda = 3;
   float B[] = { -0.88f, -0.555f, 0.642f, 0.751f, -0.859f, -0.409f };
   int ldb = 2;
   float B_expected[] = { -1.71429f, -1.08117f, 0.783084f, 0.711096f, 2.97803f, 2.11352f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1834)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { 0.249f, -0.451f, -0.781f, 0.157f, -0.02f, 0.57f, 0.309f, -0.159f, 0.266f };
   int lda = 3;
   float B[] = { -0.546f, 0.839f, 0.392f, -0.445f, -0.818f, 0.953f };
   int ldb = 2;
   float B_expected[] = { 0.1638f, -0.2517f, -0.143317f, 0.173017f, 0.171998f, -0.180615f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1835)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { 0.299f, 0.626f, -0.471f, 0.208f, -0.842f, 0.674f, 0.03f, 0.628f, 0.534f };
   int lda = 3;
   float B[] = { 0.831f, -0.997f, -0.366f, 0.307f, -0.426f, 0.806f };
   int ldb = 2;
   float B_expected[] = { -0.584851f, 0.816906f, 0.0611706f, -0.25308f, 0.239326f, -0.452809f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1836)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = -0.3f;
   float A[] = { 0.301f, 0.168f, 0.934f, 0.107f, 0.068f, 0.384f, -0.201f, 0.116f, -0.436f };
   int lda = 3;
   float B[] = { 0.773f, -0.304f, -0.402f, 0.642f, -0.102f, -0.095f };
   int ldb = 2;
   float B_expected[] = { -0.278767f, 0.0987764f, 0.10885f, -0.203544f, 0.0306f, 0.0285f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1837)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { -0.616f, 0.304f, 0.403f, 0.739f };
   int lda = 2;
   float B[] = { 0.273f, -0.609f, 0.858f, 0.993f, -0.738f, -0.353f };
   int ldb = 3;
   float B_expected[] = { -0.443182f, 0.988636f, -1.39286f, 1.52602f, -1.40534f, 0.0953025f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1838)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { 0.811f, 0.257f, 0.98f, -0.956f };
   int lda = 2;
   float B[] = { 0.996f, 0.329f, 0.273f, -0.744f, 0.662f, -0.31f };
   int ldb = 3;
   float B_expected[] = { 0.996f, 0.329f, 0.273f, -0.999972f, 0.577447f, -0.380161f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1839)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { 0.845f, 0.064f, 0.29f, -0.291f };
   int lda = 2;
   float B[] = { 0.878f, 0.156f, 0.217f, 0.082f, -0.869f, 0.595f };
   int ldb = 3;
   float B_expected[] = { 1.13576f, -0.840253f, 0.958527f, -0.281787f, 2.98625f, -2.04467f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1840)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { 0.836f, 0.359f, -0.415f, 0.154f };
   int lda = 2;
   float B[] = { 0.652f, 0.614f, 0.922f, -0.063f, 0.313f, -0.316f };
   int ldb = 3;
   float B_expected[] = { 0.625855f, 0.743895f, 0.79086f, -0.063f, 0.313f, -0.316f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1841)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { 0.94f, -0.656f, 0.645f, -0.634f };
   int lda = 2;
   float B[] = { -0.948f, -0.596f, -0.799f, 0.133f, -0.843f, -0.179f };
   int ldb = 2;
   float B_expected[] = { -1.00851f, -0.0859454f, -0.85f, -1.07453f, -0.896809f, -0.630034f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1842)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { -0.332f, 0.705f, -0.792f, -0.033f };
   int lda = 2;
   float B[] = { 0.561f, 0.883f, -0.136f, 0.203f, -0.531f, 0.733f };
   int ldb = 2;
   float B_expected[] = { 0.561f, 1.32731f, -0.136f, 0.095288f, -0.531f, 0.312448f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1843)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { 0.991f, 0.614f, 0.108f, -0.125f };
   int lda = 2;
   float B[] = { -0.723f, 0.885f, 0.336f, 0.584f, 0.742f, -0.438f };
   int ldb = 2;
   float B_expected[] = { 3.65703f, -7.08f, 3.23371f, -4.672f, -1.42226f, 3.504f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1844)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { -0.626f, 0.912f, -0.003f, 0.761f };
   int lda = 2;
   float B[] = { 0.736f, -0.383f, 0.0f, -0.238f, 0.013f, 0.473f };
   int ldb = 2;
   float B_expected[] = { 1.0853f, -0.383f, 0.217056f, -0.238f, -0.418376f, 0.473f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1845)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { -0.416f, 0.599f, -0.705f, 0.326f, 0.184f, 0.079f, -0.173f, 0.125f, 0.567f };
   int lda = 3;
   float B[] = { 0.466f, 0.907f, -0.85f, -0.342f, -0.058f, -0.379f };
   int ldb = 3;
   float B_expected[] = { 9.44495f, 5.57299f, -1.49912f, 1.91427f, -0.0282283f, -0.66843f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1846)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { -0.75f, 0.856f, 0.773f, -0.241f, -0.357f, -0.683f, -0.718f, 0.69f, -0.486f };
   int lda = 3;
   float B[] = { -0.532f, -0.817f, 0.85f, -0.135f, 0.797f, 0.981f };
   int ldb = 3;
   float B_expected[] = { -0.986649f, -0.23645f, 0.85f, -2.14908f, 1.46702f, 0.981f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1847)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { 0.765f, -0.408f, 0.404f, 0.764f, 0.157f, -0.741f, 0.844f, 0.206f, -0.215f };
   int lda = 3;
   float B[] = { -0.859f, 0.563f, -0.61f, 0.2f, 0.816f, -0.692f };
   int ldb = 3;
   float B_expected[] = { -1.12288f, 9.05017f, 7.1006f, 0.261438f, 3.92523f, 8.00582f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1848)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { 0.354f, -0.931f, 0.18f, 0.391f, 0.01f, 0.429f, 0.685f, 0.332f, -0.643f };
   int lda = 3;
   float B[] = { -0.645f, 0.847f, 0.014f, 0.83f, 0.761f, 0.187f };
   int ldb = 3;
   float B_expected[] = { -0.645f, 1.09919f, 0.0908923f, 0.83f, 0.43647f, -0.526458f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1849)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { 0.569f, 0.85f, 0.642f, -0.051f, 0.724f, 0.201f, 0.87f, -0.638f, 0.008f };
   int lda = 3;
   float B[] = { -0.923f, 0.27f, -0.319f, -0.856f, -0.533f, 0.183f };
   int ldb = 2;
   float B_expected[] = { 94.9456f, -32.8005f, -59.1516f, 18.9755f, -66.625f, 22.875f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1850)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { 0.244f, 0.931f, 0.857f, -0.295f, 0.551f, 0.832f, 0.744f, -0.326f, 0.111f };
   int lda = 3;
   float B[] = { -0.478f, -0.252f, -0.155f, 0.419f, -0.192f, 0.291f };
   int ldb = 2;
   float B_expected[] = { -0.399342f, -0.316914f, -0.217592f, 0.513866f, -0.192f, 0.291f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1851)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { 0.36f, 0.356f, -0.858f, 0.879f, 0.641f, 0.989f, 0.998f, -0.005f, 0.64f };
   int lda = 3;
   float B[] = { -0.634f, -0.529f, -0.344f, 0.375f, -0.168f, 0.465f };
   int ldb = 2;
   float B_expected[] = { -1.76111f, -1.46944f, 0.441428f, 1.40113f, -3.30563f, -3.40859f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1852)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha = 1.0f;
   float A[] = { 0.389f, 0.997f, 0.909f, -0.598f, -0.43f, -0.345f, -0.897f, 0.119f, -0.285f };
   int lda = 3;
   float B[] = { 0.779f, -0.129f, 0.016f, 0.599f, -0.668f, -0.638f };
   int ldb = 2;
   float B_expected[] = { 0.779f, -0.129f, -0.760663f, 0.727613f, -1.63854f, -0.269713f };
   cblas_strsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strsm(case 1853)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.876, -0.503, -0.062, -0.987 };
   int lda = 2;
   double B[] = { 0.219, -0.986, -0.0, -0.605, 0.289, 0.641 };
   int ldb = 3;
   double B_expected[] = { 0.601967125138, -1.29370052694, -0.372910623494, -0.612968591692, 0.292806484296, 0.649442755826 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1854)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.266, -0.505, -0.55, 0.524 };
   int lda = 2;
   double B[] = { 0.1, -0.105, 0.757, 0.522, -0.269, -0.142 };
   int ldb = 3;
   double B_expected[] = { -0.36361, 0.240845, -0.68529, -0.522, 0.269, 0.142 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1855)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.101, 0.871, 0.202, 0.169 };
   int lda = 2;
   double B[] = { 0.018, 0.292, -0.573, 0.866, 0.749, 0.99 };
   int ldb = 3;
   double B_expected[] = { -0.178217821782, -2.89108910891, 5.67326732673, -4.91124260355, -0.976331360947, -12.6390532544 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1856)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.387, -0.739, -0.599, 0.114 };
   int lda = 2;
   double B[] = { 0.7, 0.473, 0.86, -0.557, 0.283, 0.62 };
   int ldb = 3;
   double B_expected[] = { -0.7, -0.473, -0.86, 0.1377, -0.566327, -1.13514 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1857)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.683, -0.009, -0.451, -0.185 };
   int lda = 2;
   double B[] = { 0.552, 0.083, -0.976, 0.22, -0.895, -0.301 };
   int ldb = 2;
   double B_expected[] = { 0.511946499941, 0.448648648649, -2.21423766373, 1.18918918919, -0.236033397966, -1.62702702703 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1858)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.141, 0.944, 0.529, 0.636 };
   int lda = 2;
   double B[] = { 0.178, -0.22, -0.645, -0.585, -0.342, -0.594 };
   int ldb = 2;
   double B_expected[] = { -0.29438, 0.22, 0.335535, 0.585, 0.027774, 0.594 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1859)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.541, 0.584, -0.394, 0.371 };
   int lda = 2;
   double B[] = { 0.668, 0.848, -0.816, -0.925, -0.145, 0.746 };
   int ldb = 2;
   double B_expected[] = { -1.23475046211, -0.342063962613, 1.50831792976, 0.118982018923, 0.268022181146, -2.43268181614 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1860)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.836, -0.024, 0.226, 0.416 };
   int lda = 2;
   double B[] = { -0.172, -0.601, 0.542, 0.25, 0.746, 0.55 };
   int ldb = 2;
   double B_expected[] = { 0.172, 0.605128, -0.542, -0.263008, -0.746, -0.567904 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1861)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.544, 0.721, 0.623, 0.392, -0.808, -0.022, -0.665, -0.616, -0.735 };
   int lda = 3;
   double B[] = { -0.526, -0.486, -0.716, 0.361, 0.365, -0.492 };
   int ldb = 3;
   double B_expected[] = { 0.966911764706, 0.261316067268, -0.162398536147, -0.663602941176, -0.140417971025, -1.22766726121 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1862)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.907, 0.558, -0.233, 0.073, -0.734, -0.058, -0.115, 0.513, 0.503 };
   int lda = 3;
   double B[] = { -0.606, -0.124, 0.641, -0.074, -0.053, -0.734 };
   int ldb = 3;
   double B_expected[] = { 0.606, -0.214148, -0.512222584, 0.074, 0.011708, 0.751921064 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1863)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.9, 0.063, -0.652, -0.841, 0.251, -0.8, 0.365, 0.809, 0.336 };
   int lda = 3;
   double B[] = { -0.584, -0.058, -0.964, -0.214, -0.632, -0.611 };
   int ldb = 3;
   double B_expected[] = { -8.93978245747, -9.01617340163, 2.86904761905, -3.62368367799, -3.34313934737, 1.81845238095 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1864)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.934, -0.608, 0.49, 0.351, -0.301, 0.602, 0.873, 0.031, -0.2 };
   int lda = 3;
   double B[] = { -0.541, -0.729, -0.382, 0.741, 0.546, -0.833 };
   int ldb = 3;
   double B_expected[] = { -0.044208458, 0.717158, 0.382, -1.267499127, -0.571823, 0.833 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1865)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.339, 0.049, 0.734, -0.182, 0.427, 0.193, -0.959, -0.679, 0.269 };
   int lda = 3;
   double B[] = { 0.824, 0.907, 0.632, -0.348, -0.646, 0.741 };
   int ldb = 2;
   double B_expected[] = { 2.43067846608, 2.67551622419, -0.444066789635, 1.95537225481, 9.9460940476, 11.7193971004 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1866)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.766, -0.422, -0.518, 0.517, 0.669, 0.337, -0.579, 0.885, -0.677 };
   int lda = 3;
   double B[] = { 0.211, -0.911, -0.685, -0.777, -0.919, 0.282 };
   int ldb = 2;
   double B_expected[] = { -0.211, 0.911, 0.794087, 0.306013, 0.094064005, -0.025352505 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1867)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.686, -0.256, 0.028, 0.371, 0.469, 0.115, 0.284, 0.139, 0.677 };
   int lda = 3;
   double B[] = { -0.877, -0.818, 0.191, 0.468, 0.889, -0.002 };
   int ldb = 2;
   double B_expected[] = { -1.30020532939, -0.819646768394, -0.0852626506631, -0.998592183627, -1.31314623338, 0.00295420974889 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1868)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.819, -0.523, 0.042, 0.545, -0.292, 0.283, 0.224, 0.247, -0.325 };
   int lda = 3;
   double B[] = { 0.153, -0.272, -0.226, 0.987, -0.216, -0.218 };
   int ldb = 2;
   double B_expected[] = { -0.075843944, -0.285622962, 0.164872, -1.048694, 0.216, 0.218 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1869)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.164, 0.486, 0.891, -0.508 };
   int lda = 2;
   double B[] = { 0.368, 0.761, -0.349, 0.324, 0.241, 0.561 };
   int ldb = 3;
   double B_expected[] = { -2.24390243902, -4.64024390244, 2.12804878049, -1.50893028615, -3.96487900903, 3.14021989629 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1870)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.019, -0.382, -0.579, 0.76 };
   int lda = 2;
   double B[] = { -0.596, -0.074, 0.576, 0.861, -0.44, 0.842 };
   int ldb = 3;
   double B_expected[] = { 0.596, 0.074, -0.576, -0.633328, 0.468268, -1.062032 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1871)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.449, -0.367, -0.268, 0.1 };
   int lda = 2;
   double B[] = { 0.58, -0.203, 0.053, 0.792, 0.355, -0.685 };
   int ldb = 3;
   double B_expected[] = { -6.01906458797, -1.66681514477, 3.9706013363, -7.92, -3.55, 6.85 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1872)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.159, 0.333, 0.515, 0.715 };
   int lda = 2;
   double B[] = { -0.631, 0.472, 0.796, 0.278, 0.802, 0.298 };
   int ldb = 3;
   double B_expected[] = { 0.77417, -0.05897, -0.64253, -0.278, -0.802, -0.298 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1873)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.056, -0.493, 0.619, -0.028 };
   int lda = 2;
   double B[] = { -0.32, -0.217, 0.301, 0.729, -0.847, -0.577 };
   int ldb = 2;
   double B_expected[] = { 5.71428571429, 118.576530612, -5.375, -92.7901785714, 15.125, 313.763392857 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1874)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.595, 0.64, 0.109, 0.969 };
   int lda = 2;
   double B[] = { 0.186, -0.435, -0.747, 0.212, 0.257, 0.804 };
   int ldb = 2;
   double B_expected[] = { -0.186, 0.455274, 0.747, -0.293423, -0.257, -0.775987 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1875)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.484, 0.769, 0.91, 0.817 };
   int lda = 2;
   double B[] = { -0.668, 0.544, 0.753, 0.796, -0.74, -0.091 };
   int ldb = 2;
   double B_expected[] = { 2.4380974539, -0.665850673195, -0.0077814418807, -0.97429620563, 1.35195534965, 0.111383108935 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1876)");
     }
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.725, 0.73, -0.095, 0.123 };
   int lda = 2;
   double B[] = { -0.26, 0.579, 0.393, -0.18, 0.358, 0.839 };
   int ldb = 2;
   double B_expected[] = { 0.68267, -0.579, -0.5244, 0.18, 0.25447, -0.839 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1877)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.009, 0.237, -0.563, 0.993, 0.508, 0.771, 0.745, 0.233, 0.255 };
   int lda = 3;
   double B[] = { -0.328, -0.482, 0.083, -0.125, -0.712, -0.757 };
   int ldb = 3;
   double B_expected[] = { 21.9110553583, 1.44282075035, -0.325490196078, -281.330646047, -3.10396016674, 2.96862745098 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1878)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.484, -0.131, 0.563, -0.095, 0.012, -0.988, -0.722, 0.738, 0.05 };
   int lda = 3;
   double B[] = { -0.069, -0.137, -0.45, -0.24, 0.221, -0.509 };
   int ldb = 3;
   double B_expected[] = { -0.1081604, 0.5816, 0.45, -0.009639148, 0.281892, 0.509 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1879)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.521, 0.487, -0.961, 0.903, -0.045, 0.059, -0.61, -0.328, 0.883 };
   int lda = 3;
   double B[] = { -0.772, 0.079, -0.227, 0.998, 0.302, -0.099 };
   int ldb = 3;
   double B_expected[] = { 1.48176583493, 31.4896566432, 12.9778986844, -1.91554702495, -31.7275325229, -12.9967319963 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1880)");
     }
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.642, 0.511, 0.762, 0.804, -0.28, -0.318, 0.382, -0.165, -0.007 };
   int lda = 3;
   double B[] = { 0.987, 0.436, -0.783, 0.175, -0.973, -0.319 };
   int ldb = 3;
   double B_expected[] = { -0.987, 0.357548, 1.21902942, -0.175, 1.1137, 0.5696105 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1881)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.995, 0.625, 0.16, -0.127, -0.722, -0.355, -0.14, -0.146, -0.756 };
   int lda = 3;
   double B[] = { 0.676, 0.038, 0.543, 0.296, -0.44, 0.751 };
   int ldb = 2;
   double B_expected[] = { 0.650272121575, -0.128270318012, 0.869769452872, 0.209093640534, -0.582010582011, 0.993386243386 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1882)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { -0.619, 0.548, 0.064, -0.483, -0.508, -0.819, 0.237, 0.852, -0.512 };
   int lda = 3;
   double B[] = { -0.169, 0.429, -0.789, 0.79, 0.479, 0.817 };
   int ldb = 2;
   double B_expected[] = { 0.860726164, -0.280732428, 1.197108, -0.093916, -0.479, -0.817 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1883)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.794, -0.098, 0.442, -0.991, 0.049, 0.079, -0.8, -0.762, 0.395 };
   int lda = 3;
   double B[] = { 0.496, -0.734, -0.679, -0.697, 0.426, 0.094 };
   int ldb = 2;
   double B_expected[] = { -0.624685138539, 0.92443324937, 12.6077725801, 16.0733562947, -2.90102076605, -4.48707504683 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1884)");
     }
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha = -1;
   double A[] = { 0.848, -0.765, 0.528, -0.693, 0.252, -0.135, -0.507, 0.954, -0.056 };
   int lda = 3;
   double B[] = { 0.791, -0.787, 0.636, 0.271, -0.905, -0.974 };
   int ldb = 2;
   double B_expected[] = { -0.791, 0.787, -1.241115, 0.331055, 1.155097475, 0.603156425 };
   cblas_dtrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrsm(case 1885)");
     }
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { 0.491f, -0.317f, -0.14f, -0.739f, -0.969f, -0.518f, 0.702f, -0.287f };
   int lda = 2;
   float B[] = { -0.962f, -0.38f, 0.656f, 0.587f, -0.195f, -0.862f, -0.679f, 0.598f, 0.919f, 0.714f, -0.513f, 0.726f };
   int ldb = 3;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1886) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1886) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { -0.6f, 0.338f, -0.048f, -0.926f, 0.236f, 0.362f, 0.605f, 0.562f };
   int lda = 2;
   float B[] = { -0.009f, 0.371f, -0.989f, 0.728f, -0.062f, 0.113f, 0.714f, 0.604f, -0.293f, 0.859f, -0.875f, 0.216f };
   int ldb = 3;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1887) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1887) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { -0.889f, -0.479f, -0.526f, 0.077f, -0.704f, 0.242f, 0.458f, -0.553f };
   int lda = 2;
   float B[] = { -0.554f, 0.966f, 0.076f, 0.42f, 0.85f, 0.369f, 0.124f, -0.476f, -0.007f, 0.428f, 0.452f, -0.214f };
   int ldb = 3;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1888) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1888) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { 0.947f, 0.444f, 0.079f, -0.597f, 0.978f, -0.64f, 0.82f, 0.808f };
   int lda = 2;
   float B[] = { -0.899f, -0.964f, -0.714f, 0.422f, -0.084f, -0.78f, -0.609f, -0.595f, 0.748f, -0.926f, 0.242f, -0.474f };
   int ldb = 3;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1889) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1889) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { -0.547f, -0.763f, -0.805f, 0.498f, 0.786f, -0.082f, 0.922f, 0.538f };
   int lda = 2;
   float B[] = { -0.074f, -0.617f, 0.359f, -0.383f, -0.172f, 0.911f, -0.934f, 0.066f, -0.67f, 0.895f, 0.92f, 0.255f };
   int ldb = 2;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1890) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1890) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { -0.096f, -0.362f, -0.311f, -0.347f, 0.161f, -0.517f, -0.393f, 0.572f };
   int lda = 2;
   float B[] = { 0.742f, -0.419f, -0.391f, 0.846f, -0.255f, -0.364f, 0.006f, -0.496f, 0.118f, -0.593f, 0.773f, 0.053f };
   int ldb = 2;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1891) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1891) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { 0.669f, 0.845f, 0.657f, -0.43f, 0.19f, 0.206f, -0.305f, 0.761f };
   int lda = 2;
   float B[] = { -0.457f, 0.857f, -0.203f, 0.942f, 0.462f, 0.52f, 0.521f, -0.609f, 0.069f, 0.005f, -0.419f, 0.806f };
   int ldb = 2;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1892) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1892) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { -0.269f, -0.87f, -0.592f, 0.813f, 0.977f, -0.848f, 0.282f, -0.311f };
   int lda = 2;
   float B[] = { -0.654f, 0.857f, -0.834f, 0.796f, 0.414f, -0.499f, 0.961f, 0.643f, 0.117f, 0.758f, -0.189f, -0.768f };
   int ldb = 2;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1893) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1893) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { 0.361f, -0.818f, 0.039f, 0.275f, 0.541f, -0.615f, 0.025f, -0.691f, -0.697f, 0.976f, 0.746f, 0.607f, 0.651f, -0.918f, -0.702f, 0.37f, -0.668f, -0.114f };
   int lda = 3;
   float B[] = { 0.218f, 0.75f, 0.575f, -0.702f, 0.7f, -0.41f, 0.374f, 0.489f, -0.876f, 0.842f, -0.848f, 0.901f };
   int ldb = 3;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1894) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1894) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { 0.483f, 0.088f, -0.192f, 0.17f, 0.683f, 0.293f, -0.773f, 0.365f, -0.28f, 0.257f, 0.818f, 0.45f, -0.551f, -0.051f, 0.899f, -0.127f, -0.915f, 0.152f };
   int lda = 3;
   float B[] = { 0.732f, -0.394f, 0.073f, -0.082f, 0.918f, -0.53f, 0.67f, 0.149f, -0.344f, -0.65f, -0.62f, -0.632f };
   int ldb = 3;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1895) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1895) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { 0.508f, -0.251f, 0.655f, -0.315f, -0.26f, 0.229f, 0.05f, -0.276f, -0.993f, 0.647f, -0.547f, -0.34f, 0.781f, -0.819f, 0.865f, 0.361f, -0.028f, 0.178f };
   int lda = 3;
   float B[] = { 0.972f, 0.048f, 0.71f, -0.168f, -0.274f, 0.92f, 0.789f, 0.485f, 0.578f, 0.73f, -0.931f, 0.288f };
   int ldb = 3;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1896) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1896) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { -0.874f, 0.651f, 0.074f, -0.862f, -0.42f, 0.066f, -0.845f, 0.482f, -0.44f, 0.724f, 0.137f, -0.123f, -0.63f, -0.011f, -0.187f, -0.205f, 0.976f, -0.81f };
   int lda = 3;
   float B[] = { 0.539f, 0.131f, 0.986f, 0.615f, 0.983f, -0.22f, 0.144f, 0.677f, 0.561f, -0.494f, -0.433f, -0.089f };
   int ldb = 3;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1897) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1897) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { 0.284f, 0.871f, -0.835f, 0.926f, 0.459f, -0.889f, 0.387f, 0.319f, -0.366f, 0.884f, 0.236f, 0.921f, 0.619f, -0.41f, -0.709f, -0.372f, 0.06f, 0.551f };
   int lda = 3;
   float B[] = { 0.354f, 0.245f, 0.552f, 0.77f, -0.524f, -0.973f, -0.814f, -0.835f, -0.976f, 0.396f, -0.726f, -0.204f };
   int ldb = 2;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1898) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1898) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { -0.98f, -0.854f, -0.832f, 0.514f, -0.028f, -0.857f, 0.066f, 0.415f, -0.316f, 0.538f, -0.465f, -0.691f, 0.286f, 0.954f, -0.486f, -0.574f, -0.429f, 0.992f };
   int lda = 3;
   float B[] = { 0.295f, 0.578f, -0.167f, 0.106f, -0.782f, 0.668f, 0.278f, 0.855f, 0.038f, 0.976f, 0.167f, -0.777f };
   int ldb = 2;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1899) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1899) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { 0.534f, 0.782f, 0.282f, 0.581f, 0.804f, -0.68f, 0.234f, -0.758f, 0.033f, -0.503f, 0.981f, -0.839f, 0.919f, 0.175f, 0.152f, -0.683f, -0.346f, -0.279f };
   int lda = 3;
   float B[] = { 0.135f, -0.969f, -0.314f, -0.026f, -0.284f, 0.529f, 0.781f, -0.413f, -0.018f, -0.859f, -0.817f, -0.849f };
   int ldb = 2;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1900) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1900) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.0f};
   float A[] = { -0.426f, 0.148f, 0.889f, 0.217f, 0.779f, -0.963f, -0.516f, -0.366f, 0.721f, 0.4f, -0.976f, -0.365f, 0.532f, 0.188f, 0.176f, 0.082f, -0.691f, -0.833f };
   int lda = 3;
   float B[] = { -0.71f, 0.72f, 0.533f, 0.395f, -0.749f, 0.151f, 0.871f, 0.445f, 0.195f, -0.38f, -0.318f, -0.833f };
   int ldb = 2;
   float B_expected[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1901) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1901) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { -0.068f, 0.806f, -0.621f, 0.037f, 0.096f, -0.312f, 0.416f, 0.428f };
   int lda = 2;
   float B[] = { 0.481f, 0.192f, -0.954f, -0.958f, -0.015f, -0.203f, -0.352f, 0.08f, -0.662f, 0.681f, -0.571f, 0.146f };
   int ldb = 3;
   float B_expected[] = { 0.612512f, 0.186537f, -1.27483f, -1.08103f, -0.0395775f, -0.248522f, 0.0478574f, -0.671409f, -3.31165f, 0.315466f, -1.07961f, -0.629312f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1902) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1902) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { 0.863f, 0.689f, 0.171f, -0.164f, 0.065f, -0.727f, -0.245f, -0.556f };
   int lda = 2;
   float B[] = { 0.711f, -0.616f, -0.684f, 0.823f, 0.491f, 0.06f, -0.776f, 0.768f, 0.391f, 0.897f, 0.779f, -0.875f };
   int ldb = 3;
   float B_expected[] = { 0.616f, 0.711f, -0.823f, -0.684f, -0.06f, 0.491f, -0.98994f, -0.796557f, -0.644091f, 0.372992f, 0.804736f, 0.685199f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1903) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1903) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { 0.718f, -0.323f, 0.264f, 0.081f, -0.73f, 0.809f, -0.349f, -0.543f };
   int lda = 2;
   float B[] = { 0.862f, 0.676f, -0.085f, 0.204f, 0.063f, -0.124f, 0.162f, 0.754f, -0.978f, -0.097f, 0.986f, 0.943f };
   int ldb = 3;
   float B_expected[] = { -1.32203f, -1.00495f, 1.84655f, 0.329156f, -1.66053f, -2.19061f, 0.420449f, -1.11835f, 1.19333f, 0.945621f, -0.495118f, -2.05487f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1904) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1904) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { -0.515f, -0.166f, -0.364f, 0.24f, 0.056f, 0.023f, 0.05f, 0.853f };
   int lda = 2;
   float B[] = { 0.779f, 0.443f, -0.852f, 0.037f, -0.649f, 0.554f, 0.469f, 0.632f, 0.224f, -0.148f, 0.457f, -0.78f };
   int ldb = 3;
   float B_expected[] = { -0.396821f, 0.767272f, -0.040136f, -0.867948f, -0.587169f, -0.692532f, -0.632f, 0.469f, 0.148f, 0.224f, 0.78f, 0.457f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1905) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1905) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { 0.576f, 0.785f, 0.297f, -0.561f, -0.164f, 0.463f, -0.454f, 0.803f };
   int lda = 2;
   float B[] = { -0.78f, -0.792f, 0.223f, 0.206f, -0.097f, 0.504f, 0.721f, 0.205f, 0.508f, -0.8f, -0.469f, 0.283f };
   int ldb = 2;
   float B_expected[] = { -0.164671f, -1.12975f, 0.510941f, 0.652691f, -0.386549f, 0.358405f, 0.959415f, -0.414847f, 0.906729f, -0.353789f, -0.734462f, 0.786484f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1906) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1906) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { -0.04f, 0.917f, 0.327f, -0.115f, -0.656f, -0.811f, -0.646f, 0.78f };
   int lda = 2;
   float B[] = { 0.131f, 0.677f, -0.431f, -0.652f, -0.415f, 0.094f, -0.253f, 0.496f, 0.797f, 0.166f, 0.737f, -0.685f };
   int ldb = 2;
   float B_expected[] = { -0.677f, 0.131f, 0.101647f, -0.894111f, -0.094f, -0.415f, -0.221099f, -0.601474f, -0.166f, 0.797f, -0.070263f, 1.12521f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1907) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1907) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { 0.769f, -0.384f, -0.522f, -0.086f, -0.129f, -0.574f, 0.56f, -0.809f };
   int lda = 2;
   float B[] = { 0.367f, 0.169f, -0.321f, -0.982f, -0.563f, -0.051f, -0.742f, 0.595f, 0.067f, -0.183f, -0.524f, 0.77f };
   int ldb = 2;
   float B_expected[] = { -0.178752f, 0.912513f, 0.836303f, 0.634945f, 0.817549f, -0.921899f, 0.275884f, -0.926446f, 0.49345f, -0.309856f, -0.00752416f, -0.946584f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1908) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1908) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { 0.758f, 0.228f, 0.263f, 0.731f, 0.171f, 0.051f, 0.968f, 0.731f };
   int lda = 2;
   float B[] = { 0.783f, 0.422f, -0.649f, -0.428f, 0.216f, 0.659f, -0.608f, -0.239f, -0.588f, 0.01f, -0.009f, -0.374f };
   int ldb = 2;
   float B_expected[] = { -1.00898f, 0.640819f, 0.428f, -0.649f, -1.1663f, 0.201195f, 0.239f, -0.608f, -0.114941f, -0.859027f, 0.374f, -0.009f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1909) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1909) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { 0.601f, -0.017f, 0.518f, -0.975f, -0.394f, 0.396f, 0.395f, -0.374f, -0.321f, 0.221f, 0.809f, 0.74f, -0.009f, 0.88f, 0.057f, 0.65f, 0.761f, -0.839f };
   int lda = 3;
   float B[] = { -0.644f, 0.29f, 0.458f, 0.755f, -0.725f, 0.313f, 0.537f, 0.945f, 0.377f, 0.776f, -0.686f, -0.561f };
   int ldb = 3;
   float B_expected[] = { -5.28862f, 4.51343f, 4.18447f, 0.519474f, 0.288441f, -0.634688f, -7.53878f, 2.5597f, 2.79299f, 2.44873f, 0.781327f, -0.0400353f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1910) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1910) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { 0.746f, 0.079f, -0.151f, -0.433f, 0.524f, -0.201f, 0.198f, -0.368f, -0.449f, 0.693f, -0.14f, -0.574f, -0.242f, -0.584f, -0.298f, 0.41f, -0.234f, 0.92f };
   int lda = 3;
   float B[] = { -0.787f, 0.186f, -0.104f, -0.142f, -0.548f, 0.332f, -0.66f, 0.413f, 0.046f, 0.818f, -0.783f, -0.376f };
   int ldb = 3;
   float B_expected[] = { 0.320805f, -0.445083f, 0.410072f, -0.371288f, -0.332f, -0.548f, -0.566249f, -0.287942f, -0.315918f, 0.152204f, 0.376f, -0.783f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1911) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1911) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { -0.623f, -0.229f, 0.653f, -0.19f, 0.42f, -0.181f, -0.061f, 0.963f, 0.422f, 0.989f, 0.919f, -0.352f, -0.849f, 0.052f, 0.02f, -0.771f, -0.38f, -0.566f };
   int lda = 3;
   float B[] = { 0.018f, 0.461f, -0.184f, 0.334f, 0.075f, 0.694f, 0.022f, 0.239f, 0.971f, -0.339f, 0.203f, 0.083f };
   int ldb = 3;
   float B_expected[] = { 0.642534f, -0.265073f, -0.901268f, 0.171623f, 1.29999f, 0.384146f, 0.326529f, -0.155337f, 0.629902f, 0.0571184f, -0.761884f, -0.282697f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1912) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1912) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { 0.35f, 0.154f, 0.397f, -0.709f, 0.587f, -0.895f, -0.848f, 0.933f, -0.887f, -0.393f, 0.824f, 0.182f, 0.159f, 0.303f, -0.011f, -0.363f, 0.875f, 0.991f };
   int lda = 3;
   float B[] = { -0.513f, 0.564f, 0.404f, -0.635f, 0.924f, 0.238f, -0.059f, 0.96f, 0.341f, 0.483f, -0.844f, 0.84f };
   int ldb = 3;
   float B_expected[] = { -0.564f, -0.513f, -0.321901f, 0.495188f, -0.487057f, 1.06506f, -0.96f, -0.059f, -1.35213f, 1.18665f, -1.15086f, -1.02151f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1913) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1913) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { 0.87f, 0.914f, -0.097f, -0.138f, 0.894f, -0.173f, 0.648f, -0.327f, 0.7f, 0.816f, 0.63f, 0.637f, -0.671f, 0.322f, -0.922f, 0.618f, 0.93f, 0.654f };
   int lda = 3;
   float B[] = { -0.347f, -0.273f, -0.384f, 0.02f, 0.392f, -0.206f, 0.347f, 0.269f, 0.016f, 0.797f, 0.699f, -0.966f };
   int ldb = 2;
   float B_expected[] = { -0.443754f, 0.343363f, 0.300599f, -0.548484f, 0.757674f, 0.722159f, 0.224607f, -0.673284f, -0.565323f, 0.414754f, 1.04867f, 0.014162f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1914) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1914) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { 0.965f, -0.191f, 0.489f, 0.84f, 0.011f, -0.951f, 0.067f, -0.21f, -0.911f, 0.767f, -0.162f, 0.274f, -0.502f, -0.445f, 0.492f, 0.023f, -0.818f, 0.859f };
   int lda = 3;
   float B[] = { 0.66f, -0.303f, 0.223f, 0.261f, -0.252f, -0.238f, -0.012f, -0.485f, 0.783f, -0.196f, -0.57f, 0.929f };
   int ldb = 2;
   float B_expected[] = { 0.177032f, 1.21679f, -0.596808f, -0.300881f, 0.159577f, -0.641744f, 0.928958f, 0.289807f, 0.196f, 0.783f, -0.929f, -0.57f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1915) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1915) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { -0.652f, 0.046f, -0.229f, 0.473f, -0.783f, -0.211f, 0.698f, 0.201f, -0.153f, 0.918f, -0.996f, -0.186f, 0.84f, -0.545f, -0.457f, 0.057f, 0.649f, 0.77f };
   int lda = 3;
   float B[] = { -0.227f, 0.14f, 0.165f, -0.945f, -0.212f, -0.522f, 0.908f, 0.722f, -0.208f, 0.969f, 0.721f, -0.816f };
   int ldb = 2;
   float B_expected[] = { 0.189219f, 0.361509f, -1.42444f, -0.353565f, -0.361882f, -0.741783f, 1.80537f, 1.02311f, -1.24128f, 0.407779f, 2.0229f, -0.0912412f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1916) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1916) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 1.0f};
   float A[] = { -0.945f, 0.36f, 0.3f, 0.128f, -0.27f, -0.834f, 0.349f, -0.6f, -0.293f, 0.122f, -0.481f, -0.681f, -0.815f, -0.195f, 0.728f, 0.016f, 0.037f, 0.989f };
   int lda = 3;
   float B[] = { -0.97f, 0.784f, 0.488f, 0.39f, -0.482f, -0.518f, -0.797f, 0.271f, 0.257f, 0.637f, 0.118f, -0.993f };
   int ldb = 2;
   float B_expected[] = { -0.784f, -0.97f, -0.39f, 0.488f, 0.62904f, -0.090648f, -0.091536f, -0.89348f, 0.3246f, -0.273981f, 1.04514f, -0.5676f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1917) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1917) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { 0.795f, 0.073f, 0.104f, -0.261f, -0.712f, 0.881f, -0.474f, -0.906f };
   int lda = 2;
   float B[] = { -0.41f, -0.191f, -0.359f, -0.718f, -0.902f, 0.646f, -0.703f, -0.809f, -0.342f, -0.783f, -0.053f, 0.917f };
   int ldb = 3;
   float B_expected[] = { 0.0285203f, -0.0489535f, 0.0936712f, -0.036556f, -0.0702473f, -0.11991f, -0.0924979f, -0.0235243f, -0.0742841f, -0.0262764f, 0.074552f, 0.0886899f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1918) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1918) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { -0.281f, -0.111f, 0.055f, -0.643f, 0.33f, -0.663f, 0.32f, 0.423f };
   int lda = 2;
   float B[] = { 0.103f, 0.357f, -0.591f, 0.833f, -0.906f, -0.192f, -0.391f, -0.622f, -0.345f, -0.58f, -0.132f, -0.874f };
   int ldb = 3;
   float B_expected[] = { -0.0357f, 0.0103f, -0.0833f, -0.0591f, 0.0192f, -0.0906f, 0.0707864f, -0.0167114f, 0.0245802f, 0.0223124f, 0.0280882f, -0.0205626f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1919) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1919) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { 0.311f, -0.648f, -0.732f, 0.825f, 0.152f, -0.529f, -0.353f, 0.568f };
   int lda = 2;
   float B[] = { 0.86f, -0.991f, -0.992f, -0.617f, 0.137f, -0.585f, -0.467f, 0.632f, 0.672f, 0.777f, -0.609f, 0.511f };
   int ldb = 3;
   float B_expected[] = { 0.0795347f, -0.0537122f, -0.0885393f, -0.0194836f, -0.0386006f, -0.0674606f, 0.109194f, -0.0434058f, -0.0240177f, -0.151722f, 0.117678f, -0.0168304f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1920) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1920) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { 0.318f, -0.946f, -0.389f, 0.051f, 0.322f, -0.626f, -0.839f, -0.252f };
   int lda = 2;
   float B[] = { 0.372f, -0.23f, 0.515f, 0.213f, 0.222f, 0.296f, -0.524f, 0.442f, -0.581f, -0.409f, 0.894f, -0.246f };
   int ldb = 3;
   float B_expected[] = { 0.00443f, 0.081742f, -0.0708404f, 0.0446048f, 0.0184432f, -0.0219864f, -0.0442f, -0.0524f, 0.0409f, -0.0581f, 0.0246f, 0.0894f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1921) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1921) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { -0.411f, 0.34f, -0.85f, 0.557f, -0.918f, 0.484f, -0.889f, 0.561f };
   int lda = 2;
   float B[] = { -0.763f, -0.514f, -0.744f, -0.948f, -0.312f, 0.818f, -0.686f, 0.341f, -0.043f, 0.235f, -0.201f, 0.874f };
   int ldb = 2;
   float B_expected[] = { 0.0169288f, 0.17164f, -0.0683166f, -0.0596556f, 0.155447f, -0.0526808f, -0.086698f, 0.101645f, 0.039085f, -0.0218708f, 0.0437248f, -0.0036776f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1922) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1922) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { 0.046f, 0.571f, 0.825f, 0.665f, 0.658f, -0.977f, 0.247f, -0.944f };
   int lda = 2;
   float B[] = { -0.342f, 0.089f, -0.975f, 0.027f, -0.621f, -0.127f, 0.937f, -0.332f, -0.357f, -0.213f, 0.57f, 0.134f };
   int ldb = 2;
   float B_expected[] = { -0.0089f, -0.0342f, -0.0302572f, -0.0663011f, 0.0127f, -0.0621f, -0.0358283f, 0.122154f, 0.0213f, -0.0357f, -0.0622943f, 0.0596805f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1923) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1923) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { 0.655f, 0.051f, -0.864f, 0.04f, -0.45f, 0.276f, -0.365f, 0.766f };
   int lda = 2;
   float B[] = { 0.12f, 0.036f, 0.425f, -0.145f, -0.772f, -0.483f, -0.154f, -0.327f, 0.532f, 0.59f, 0.305f, 0.443f };
   int ldb = 2;
   float B_expected[] = { -0.0745593f, 0.00123365f, -0.0525674f, -0.00611891f, 0.0752311f, -0.0558274f, -0.0001932f, 0.0425972f, -0.0986826f, -0.00963885f, -0.00999124f, -0.0625937f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1924) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1924) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { 0.253f, -0.163f, -0.061f, -0.032f, -0.764f, 0.863f, 0.051f, 0.669f };
   int lda = 2;
   float B[] = { 0.966f, 0.42f, -0.765f, 0.186f, -0.798f, 0.278f, -0.37f, -0.484f, -0.724f, -0.682f, 0.034f, 0.352f };
   int ldb = 2;
   float B_expected[] = { -0.0455826f, 0.0925287f, -0.0186f, -0.0765f, -0.0260316f, -0.0836058f, 0.0484f, -0.037f, 0.0661616f, -0.0710662f, -0.0352f, 0.0034f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1925) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1925) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { 0.017f, -0.631f, -0.052f, 0.296f, -0.486f, -0.279f, -0.378f, 0.997f, 0.533f, 0.87f, 0.808f, 0.007f, 0.185f, -0.263f, -0.757f, -0.856f, 0.575f, -0.81f };
   int lda = 3;
   float B[] = { -0.238f, -0.924f, 0.494f, -0.089f, 0.96f, 0.959f, 0.415f, 0.39f, -0.744f, -0.881f, -0.594f, 0.629f };
   int ldb = 3;
   float B_expected[] = { 0.0798921f, -0.243487f, 0.0441094f, -0.0391653f, 0.0229218f, 0.134667f, 0.192099f, 0.152741f, 0.154557f, 0.0857677f, -0.0854154f, 0.0170199f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1926) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1926) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { -0.977f, -0.949f, 0.192f, 0.803f, -0.964f, -0.162f, 0.799f, -0.081f, -0.055f, 0.014f, 0.99f, 0.804f, 0.913f, -0.898f, -0.057f, 0.51f, 0.453f, 0.622f };
   int lda = 3;
   float B[] = { -0.852f, -0.001f, -0.955f, -0.97f, -0.071f, -0.664f, -0.077f, -0.746f, 0.228f, -0.948f, 0.476f, -0.285f };
   int ldb = 3;
   float B_expected[] = { 0.0840343f, -0.066376f, 0.0369724f, -0.0350854f, 0.0664f, -0.0071f, 0.105481f, 0.0565767f, 0.0283146f, -0.00141f, 0.0285f, 0.0476f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1927) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1927) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { 0.822f, 0.618f, -0.935f, 0.49f, 0.885f, -0.488f, 0.412f, 0.861f, -0.144f, 0.906f, -0.054f, 0.455f, 0.213f, 0.34f, -0.465f, 0.107f, -0.611f, 0.088f };
   int lda = 3;
   float B[] = { 0.476f, -0.297f, -0.966f, -0.038f, -0.346f, -0.81f, -0.749f, -0.065f, -0.225f, -0.663f, 0.073f, -0.379f };
   int ldb = 3;
   float B_expected[] = { -0.00473086f, 0.0543508f, 0.139511f, -0.0231317f, -0.199775f, 0.100154f, 0.0488188f, -0.054416f, -0.0610839f, 0.0929832f, -0.0289368f, -0.113983f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1928) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1928) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { -0.188f, 0.741f, 0.583f, 0.527f, 0.025f, 0.216f, -0.44f, -0.071f, -0.126f, -0.093f, 0.743f, -0.476f, 0.661f, -0.66f, 0.564f, -0.943f, -0.976f, -0.035f };
   int lda = 3;
   float B[] = { -0.648f, -0.367f, -0.402f, -0.309f, 0.412f, 0.531f, -0.248f, 0.181f, 0.507f, 0.502f, -0.593f, 0.404f };
   int ldb = 3;
   float B_expected[] = { 0.0367f, -0.0648f, 0.0424472f, -0.0713177f, -0.21132f, 0.0600063f, -0.0181f, -0.0248f, -0.0599248f, 0.0410731f, 0.0277256f, 0.00238266f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1929) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1929) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { 0.76f, -0.021f, -0.011f, 0.14f, 0.699f, 0.94f, 0.296f, 0.333f, 0.654f, -0.917f, 0.008f, -0.999f, -0.963f, 0.687f, -0.481f, 0.106f, 0.128f, -0.165f };
   int lda = 3;
   float B[] = { -0.742f, 0.774f, -0.335f, -0.99f, 0.799f, 0.901f, 0.753f, -0.085f, -0.042f, -0.591f, 0.202f, 0.515f };
   int ldb = 2;
   float B_expected[] = { 0.313744f, -0.259345f, -0.290807f, 0.212822f, -0.00668591f, -0.0164417f, 0.10903f, 0.137068f, 0.157578f, -0.23594f, -0.0747323f, 0.254147f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1930) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1930) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { 0.582f, -0.175f, -0.48f, 0.567f, -0.571f, 0.062f, 0.038f, -0.625f, 0.737f, 0.799f, -0.569f, -0.932f, 0.522f, -0.763f, 0.156f, -0.524f, 0.138f, 0.007f };
   int lda = 3;
   float B[] = { 0.998f, 0.6f, 0.555f, -0.737f, -0.162f, 0.263f, 0.317f, -0.092f, 0.302f, -0.671f, 0.418f, -0.814f };
   int ldb = 2;
   float B_expected[] = { -0.106233f, 0.0480583f, 0.0514817f, -0.0392668f, -0.0209428f, -0.0560716f, 0.0184048f, -0.0174744f, 0.0671f, 0.0302f, 0.0814f, 0.0418f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1931) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1931) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { 0.964f, 0.509f, 0.48f, -0.833f, 0.867f, 0.51f, -0.643f, 0.115f, -0.594f, -0.409f, -0.174f, 0.527f, 0.676f, 0.431f, 0.261f, -0.239f, 0.816f, -0.231f };
   int lda = 3;
   float B[] = { -0.659f, -0.029f, -0.581f, -0.938f, -0.904f, -0.445f, 0.119f, 0.709f, -0.649f, 0.825f, 0.532f, -0.453f };
   int ldb = 2;
   float B_expected[] = { 0.0305784f, -0.0522153f, 0.100975f, -0.00695419f, -0.055793f, 0.11446f, 0.0887801f, 0.177079f, -0.177262f, 0.0336107f, -0.0717714f, 0.251108f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1932) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1932) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   float alpha[2] = {0.0f, 0.1f};
   float A[] = { 0.859f, 0.745f, 0.03f, -0.98f, -0.402f, 0.38f, -0.214f, 0.605f, 0.342f, -0.059f, -0.096f, 0.606f, -0.543f, 0.503f, 0.63f, -0.269f, 0.252f, 0.626f };
   int lda = 3;
   float B[] = { 0.85f, 0.642f, 0.679f, -0.254f, 0.192f, 0.766f, -0.869f, -0.09f, 0.68f, -0.898f, 0.272f, -0.651f };
   int ldb = 2;
   float B_expected[] = { -0.0642f, 0.085f, 0.0254f, 0.0679f, 0.008626f, 0.079566f, 0.07478f, -0.113829f, -0.0156973f, 0.0906397f, 0.125668f, 0.0985369f };
   cblas_ctrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrsm(case 1933) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrsm(case 1933) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { 0.189, 0.519, -0.455, -0.444, -0.21, -0.507, -0.591, 0.859 };
   int lda = 2;
   double B[] = { -0.779, -0.484, 0.249, -0.107, -0.755, -0.047, 0.941, 0.675, -0.757, 0.645, -0.649, 0.242 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1934) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1934) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { -0.988, 0.73, 0.279, -0.967, -0.288, -0.095, -0.821, 0.178 };
   int lda = 2;
   double B[] = { 0.702, 0.943, -0.235, -0.565, 0.279, -0.146, 0.816, 0.473, 0.893, 0.877, -0.797, -0.159 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1935) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1935) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { 0.716, -0.549, 0.436, -0.822, -0.029, -0.586, 0.791, -0.159 };
   int lda = 2;
   double B[] = { 0.021, 0.391, 0.296, -0.154, -0.513, 0.738, -0.336, 0.317, 0.502, 0.543, 0.027, 0.802 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1936) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1936) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { 0.715, -0.875, -0.501, 0.425, -0.928, -0.929, -0.542, 0.915 };
   int lda = 2;
   double B[] = { 0.065, 0.679, -0.545, 0.042, 0.199, -0.86, 0.159, 0.943, 0.19, 0.403, 0.994, 0.76 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1937) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1937) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { -0.936, -0.989, -0.57, 0.018, -0.821, 0.516, -0.479, 0.209 };
   int lda = 2;
   double B[] = { 0.722, -0.756, -0.828, -0.191, -0.981, -0.466, 0.347, 0.85, -0.596, -0.826, -0.182, -0.321 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1938) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1938) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { 0.693, 0.976, -0.356, -0.313, 0.926, -0.164, -0.337, 0.056 };
   int lda = 2;
   double B[] = { -0.988, -0.633, -0.745, -0.392, -0.362, -0.708, -0.706, -0.093, -0.177, 0.837, 0.391, -0.853 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1939) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1939) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { -0.483, -0.383, 0.357, 0.889, 0.523, -0.148, -0.592, 0.481 };
   int lda = 2;
   double B[] = { -0.41, 0.994, -0.779, -0.354, 0.571, 0.51, -0.526, 0.934, 0.469, 0.735, -0.47, -0.164 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1940) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1940) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { -0.576, -0.089, 0.953, -0.317, 0.408, 0.618, 0.092, -0.84 };
   int lda = 2;
   double B[] = { 0.141, -0.32, -0.007, -0.682, -0.068, -0.412, 0.675, -0.809, 0.931, -0.257, -0.048, 0.633 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1941) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1941) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { 0.269, 0.567, 0.497, -0.969, 0.957, 0.538, -0.921, 0.639, 0.599, -0.436, -0.045, 0.164, 0.827, 0.489, -0.729, 0.723, -0.01, 0.934 };
   int lda = 3;
   double B[] = { -0.391, 0.434, -0.349, -0.456, -0.541, 0.289, 0.31, 0.447, 0.971, -0.626, -0.77, -0.882 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1942) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1942) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { -0.523, -0.364, -0.492, 0.294, 0.71, -0.401, 0.947, -0.008, 0.235, -0.47, 0.298, -0.603, -0.193, 0.598, 0.122, -0.733, -0.827, 0.491 };
   int lda = 3;
   double B[] = { 0.872, 0.441, 0.518, 0.607, -0.04, -0.976, 0.201, -0.136, -0.958, -0.501, -0.549, -0.4 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1943) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1943) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { -0.177, -0.965, 0.589, -0.236, -0.303, -0.301, 0.982, 0.006, -0.73, 0.241, 0.636, -0.672, 0.886, 0.952, 0.501, -0.803, -0.823, -0.09 };
   int lda = 3;
   double B[] = { -0.475, -0.646, -0.666, -0.886, 0.04, -0.736, -0.592, -0.995, 0.259, 0.701, -0.033, 0.616 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1944) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1944) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { -0.76, -0.29, -0.601, 0.327, 0.383, 0.883, 0.589, -0.708, 0.912, -0.982, 0.629, 0.879, -0.578, -0.814, 0.168, 0.91, 0.328, 0.223 };
   int lda = 3;
   double B[] = { 0.381, 0.829, 0.096, 0.382, 0.664, 0.006, -0.376, -0.338, 0.344, -0.889, -0.175, 0.083 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1945) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1945) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { 0.129, -0.161, 0.102, 0.443, -0.138, 0.677, -0.87, 0.327, 0.917, 0.446, 0.798, -0.91, -0.574, 0.333, -0.626, 0.14, 0.109, 0.161 };
   int lda = 3;
   double B[] = { -0.689, -0.94, -0.814, 0.761, 0.389, 0.03, -0.175, -0.739, -0.904, 0.463, -0.511, 0.615 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1946) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1946) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { 0.062, 0.756, 0.179, 0.359, -0.047, -0.197, 0.678, 0.873, 0.003, -0.996, 0.507, -0.491, -0.726, -0.833, -0.118, -0.71, 0.714, 0.638 };
   int lda = 3;
   double B[] = { -0.614, 0.193, 0.881, 0.538, 0.183, -0.034, 0.099, -0.154, -0.121, 0.842, -0.182, -0.229 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1947) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1947) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { -0.874, 0.171, 0.637, 0.554, 0.852, -0.203, 0.455, 0.619, -0.128, 0.759, 0.342, 0.372, 0.669, -0.537, -0.76, -0.348, -0.714, 0.573 };
   int lda = 3;
   double B[] = { -0.434, 0.921, -0.949, 0.282, -0.665, 0.223, -0.633, 0.921, -0.73, 0.457, -0.021, -0.844 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1948) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1948) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 111;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0};
   double A[] = { -0.189, -0.931, 0.414, 0.288, -0.245, 0.252, -0.465, -0.073, 0.327, 0.176, -0.067, 0.1, 0.124, 0.885, -0.731, -0.303, 0.954, -0.763 };
   int lda = 3;
   double B[] = { 0.818, 0.948, -0.749, 0.808, -0.959, -0.797, 0.727, 0.701, 0.244, -0.801, 0.354, -0.781 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1949) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1949) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { -0.65, -0.279, -0.543, -0.097, -0.641, 0.984, 0.507, -0.809 };
   int lda = 2;
   double B[] = { -0.176, 0.87, -0.681, 0.409, -0.878, 0.522, 0.348, 0.679, -0.975, -0.815, -0.608, 0.86 };
   int ldb = 3;
   double B_expected[] = { 0.256485077177, 1.22837025149, -0.656630178218, 0.911076645728, -0.849544610576, 1.16772760977, -0.193804546743, -0.283833884163, -0.811035478317, 1.16349859839, 0.292241175557, -0.141827660937 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1950) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1950) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { 0.23, -0.597, 0.068, 0.945, 0.045, -0.436, 0.113, 0.035 };
   int lda = 2;
   double B[] = { -0.744, -0.465, -0.742, 0.996, -0.835, 0.712, -0.968, 0.053, -0.813, 0.36, 0.572, -0.489 };
   int ldb = 3;
   double B_expected[] = { 0.744, 0.465, 0.742, -0.996, 0.835, -0.712, 1.356833, -0.7877, -0.178676, -0.993462, -1.30162, -0.251659 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1951) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1951) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { -0.689, -0.396, 0.415, -0.567, 0.001, 0.513, 0.837, 0.045 };
   int lda = 2;
   double B[] = { -0.012, 0.2, 0.22, 0.81, -0.586, -0.198, 0.16, -0.958, -0.125, 0.833, 0.344, 0.213 };
   int ldb = 3;
   double B_expected[] = { -0.573154258944, 0.525131422048, 1.33801555643, 0.47629585874, -0.770607912552, -0.160087833623, -0.129249609305, 1.15151282248, 0.0955601670381, -1.00035867087, -0.423449388979, -0.231714190557 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1952) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1952) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { 0.102, 0.86, -0.067, 0.12, 0.92, 0.441, 0.367, -0.104 };
   int lda = 2;
   double B[] = { 0.386, 0.59, 0.222, 0.824, 0.091, 0.486, 0.43, 0.766, 0.576, 0.042, 0.013, -0.008 };
   int ldb = 3;
   double B_expected[] = { -0.328206, 0.30435, 0.289398, -0.531344, -0.075512, -0.487627, -0.43, -0.766, -0.576, -0.042, -0.013, 0.008 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1953) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1953) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { -0.087, 0.925, -0.315, 0.251, 0.7, -0.223, 0.448, 0.373 };
   int lda = 2;
   double B[] = { -0.333, -0.495, 0.995, -0.229, 0.425, -0.269, -0.756, -0.783, -0.214, 0.582, -0.351, -0.095 };
   int ldb = 2;
   double B_expected[] = { 0.496880191475, -0.406733596387, -0.965186357327, 2.19761676664, 0.331095906598, 0.428318547163, 1.17655095681, 0.263745306399, -0.645240814927, -0.170663836866, 1.18578937767, -0.829739852214 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1954) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1954) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { 0.717, 0.572, -0.304, 0.878, 0.625, -0.615, -0.565, -0.643 };
   int lda = 2;
   double B[] = { -0.383, -0.669, -0.043, -0.09, -0.999, -0.427, 0.834, 0.539, -0.973, -0.481, 0.071, -0.71 };
   int ldb = 2;
   double B_expected[] = { 0.383, 0.669, -0.60781, -0.09258, 0.999, 0.427, -1.72098, -0.19149, 0.973, 0.481, -0.97494, 1.00777 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1955) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1955) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { 0.143, -0.022, 0.487, 0.444, 0.138, -0.871, 0.572, -0.093 };
   int lda = 2;
   double B[] = { -0.073, -0.9, -0.688, 0.436, -0.213, -0.733, 0.809, -0.618, 0.696, 0.259, 0.494, 0.162 };
   int ldb = 2;
   double B_expected[] = { -6.10129128737, 3.22195959384, 1.29255909931, -0.552083922664, 8.05253150033, 8.35261031753, -1.54904967648, 0.828563601552, -3.66721033067, 1.50334288416, -0.796532800529, -0.412722990296 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1956) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1956) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { 0.544, 0.918, -0.524, 0.547, -0.839, 0.4, -0.548, 0.49 };
   int lda = 2;
   double B[] = { 0.475, -0.594, 0.252, -0.717, 0.867, 0.07, 0.264, 0.538, 0.028, 0.482, -0.59, -0.533 };
   int ldb = 2;
   double B_expected[] = { -0.214849, 1.107552, -0.252, 0.717, -1.299622, -0.207504, -0.264, -0.538, 0.572711, -0.525438, 0.59, 0.533 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1957) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1957) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { -0.038, -0.116, -0.476, -0.818, 0.961, 0.271, -0.593, 0.548, -0.86, 0.429, -0.396, -0.559, 0.766, -0.326, -0.335, 0.633, -0.532, 0.317 };
   int lda = 3;
   double B[] = { -0.459, 0.904, 0.887, 0.07, -0.497, -0.48, -0.313, 0.864, -0.029, -0.754, -0.566, -0.108 };
   int ldb = 3;
   double B_expected[] = { -4.58258258525, -3.00717937382, 0.0668903493808, 0.800759804641, -0.292673260098, -1.0766492922, -0.911020412982, 7.68812066826, -0.0359723342287, -0.157963939743, -0.695872108638, -0.617653117365 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1958) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1958) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { 0.886, 0.945, 0.065, 0.882, -0.46, -0.095, 0.823, -0.245, -0.825, 0.904, -0.214, -0.268, -0.935, -0.017, 0.902, 0.561, 0.954, -0.665 };
   int lda = 3;
   double B[] = { 0.076, -0.043, 0.873, -0.831, -0.329, -0.896, -0.174, 0.653, 0.489, 0.25, -0.896, 0.609 };
   int ldb = 3;
   double B_expected[] = { 1.037824842, 1.333886264, -1.042722, 1.110916, 0.329, 0.896, 0.529073224, -0.720680322, -0.134044, -0.140198, 0.896, -0.609 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1959) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1959) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { -0.691, -0.056, -0.339, -0.483, -0.975, -0.052, -0.198, 0.576, -0.075, 0.718, -0.321, 0.728, -0.124, 0.774, 0.685, -0.112, 0.178, 0.275 };
   int lda = 3;
   double B[] = { -0.062, -0.391, 0.326, 0.42, -0.203, 0.45, 0.338, 0.991, -0.47, -0.363, 0.766, -0.961 };
   int ldb = 3;
   double B_expected[] = { -0.134697690677, -0.554930433172, -0.526377715671, 0.991348747823, -2.94323584375, -1.92805449726, 0.601422754501, 1.38541291715, 0.201151053335, -1.95287726277, 5.96201044303, 2.1797020274 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1960) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1960) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { -0.318, 0.067, -0.097, 0.359, -0.688, 0.307, -0.63, -0.616, 0.193, 0.817, -0.792, -0.117, -0.501, -0.929, -0.595, -0.144, 0.453, 0.658 };
   int lda = 3;
   double B[] = { -0.249, -0.206, 0.424, -0.681, -0.464, 0.21, 0.541, 0.082, 0.803, -0.461, -0.638, 0.358 };
   int ldb = 3;
   double B_expected[] = { 0.249, 0.206, -0.394026, 0.964164, 0.024089914, 0.641464836, -0.541, -0.082, -1.093318, 0.076084, -0.218343306, -1.013838812 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1961) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1961) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { 0.691, 0.808, -0.178, 0.489, 0.159, -0.646, -0.692, -0.968, -0.146, -0.281, -0.385, 0.773, 0.704, 0.782, 0.551, -0.727, 0.669, 0.858 };
   int lda = 3;
   double B[] = { -0.657, -0.69, -0.051, 0.28, -0.846, 0.304, 0.052, 0.543, 0.613, -0.98, 0.983, -0.484 };
   int ldb = 2;
   double B_expected[] = { 2.42007211075, -0.148130095453, 4.93683906416, -0.804178199722, 1.76852672271, 0.633536755193, 4.41638755104, -0.0400468884046, 0.363887727302, 0.998182854971, -0.204739276437, 0.986048279795 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1962) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1962) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { -0.244, -0.925, -0.539, 0.422, 0.285, -0.954, -0.347, -0.255, -0.616, -0.979, 0.631, -0.864, -0.053, -0.715, -0.749, -0.973, -0.409, -0.247 };
   int lda = 3;
   double B[] = { 0.922, -0.728, 0.588, -0.715, -0.92, -0.065, -0.583, 0.178, 0.996, 0.215, -0.614, -0.443 };
   int ldb = 2;
   double B_expected[] = { -0.416484258, -0.267425916, -0.851455486, 1.594186448, 0.383191, -1.065143, 0.611847, 0.751229, -0.996, -0.215, 0.614, 0.443 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1963) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1963) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { 0.992, 0.172, -0.646, 0.067, -0.823, -0.013, -0.55, -0.438, -0.44, -0.302, 0.99, -0.373, 0.513, -0.106, -0.591, -0.504, 0.929, -0.318 };
   int lda = 3;
   double B[] = { 0.467, 0.227, 0.988, -0.709, -0.272, -0.601, 0.719, -0.133, 0.203, 0.937, -0.382, -0.334 };
   int ldb = 2;
   double B_expected[] = { -0.495544804508, -0.142909570186, -0.846593689328, 0.861506163875, -0.485462670276, -0.898345893497, 1.07522946065, -2.43403194583, 0.315527055267, -0.271726799352, -1.73234815305, 3.5434654009 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1964) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1964) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 112;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {-1, 0};
   double A[] = { -0.692, -0.245, -0.874, 0.77, 0.07, 0.01, 0.018, -0.42, -0.405, -0.387, 0.888, -0.912, -0.81, 0.314, 0.66, -0.895, -0.556, 0.157 };
   int lda = 3;
   double B[] = { -0.801, 0.542, 0.699, 0.574, -0.56, 0.043, 0.742, -0.331, -0.614, 0.776, -0.335, 0.131 };
   int ldb = 2;
   double B_expected[] = { 0.801, -0.542, -0.699, -0.574, 0.842734, -1.133478, -1.794906, 0.367554, 0.837894144, 1.029031872, 1.63685728, -2.047172224 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1965) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1965) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { -0.035, -0.456, 0.152, 0.976, 0.687, -0.527, -0.571, 0.832 };
   int lda = 2;
   double B[] = { -0.868, 0.033, -0.131, -0.936, 0.993, 0.104, -0.684, 0.851, 0.523, 0.836, -0.205, 0.319 };
   int ldb = 3;
   double B_expected[] = { -0.188683836853, 0.0217191541444, -0.044222393276, -0.201868895253, 0.218228063549, 0.00605705652583, 0.252579293874, 0.0800538768738, -0.099911150161, 0.0758372341381, -0.116723296822, -0.16542230206 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1966) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1966) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 121;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { -0.481, -0.442, 0.69, 0.415, 0.983, -0.466, 0.503, -0.147 };
   int lda = 2;
   double B[] = { -0.287, -0.777, -0.187, 0.061, 0.631, 0.797, 0.833, -0.49, -0.188, 0.386, -0.904, -0.793 };
   int ldb = 3;
   double B_expected[] = { 0.0777, -0.0287, -0.0061, -0.0187, -0.0797, 0.0631, 0.0072975, 0.1353485, -0.0266305, -0.0084285, 0.1081065, -0.1670145 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1967) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1967) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { -0.286, 0.025, -0.111, 0.724, -0.973, -0.071, 0.527, -0.334 };
   int lda = 2;
   double B[] = { -0.381, -0.131, 0.33, 0.09, 0.35, 0.062, -0.874, 0.252, 0.924, 0.251, 0.559, -0.619 };
   int ldb = 3;
   double B_expected[] = { 0.38447496828, 0.401499279514, -0.210140860451, -0.584596680596, -0.443343106286, -0.127686958741, -0.109102585509, -0.096697792106, 0.045298174859, 0.146623168116, 0.131759250934, 0.0225662432408 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1968) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1968) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 141;
   int uplo = 122;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { -0.862, -0.003, 0.975, 0.364, -0.996, 0.909, -0.316, -0.816 };
   int lda = 2;
   double B[] = { 0.167, 0.961, 0.116, 0.675, 0.086, 0.259, -0.483, 0.898, 0.434, 0.723, 0.505, 0.042 };
   int ldb = 3;
   double B_expected[] = { -0.1416361, -0.113035, -0.1789614, -0.0108943, -0.0759877, 0.0550802, -0.0898, -0.0483, -0.0723, 0.0434, -0.0042, 0.0505 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1969) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1969) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { -0.826, -0.025, 0.638, -0.183, -0.184, 0.806, 0.131, 0.764 };
   int lda = 2;
   double B[] = { -0.038, 0.14, -0.31, -0.494, -0.974, -0.396, -0.217, 0.519, -0.656, -0.737, 0.383, -0.03 };
   int ldb = 2;
   double B_expected[] = { 0.0167945280502, 0.00510879322186, 0.0315562985639, 0.0579039669012, -0.0514636821443, 0.116360058046, 0.0192833017545, -0.206389577002, -0.0915450409357, 0.0766481525141, 0.0107002286761, -0.100817314679 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1970) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1970) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 121;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { 0.282, -0.433, -0.793, -0.008, -0.999, 0.377, -0.979, 0.421 };
   int lda = 2;
   double B[] = { 0.622, -0.722, 0.605, -0.877, 0.935, -0.906, 0.719, -0.607, 0.022, -0.326, -0.905, 0.323 };
   int ldb = 2;
   double B_expected[] = { 0.0722, 0.0622, 0.1363784, 0.1498572, 0.0906, 0.0935, 0.1159599, 0.1994627, 0.0326, 0.0022, -0.000562, -0.076012 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1971) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1971) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { 0.934, 0.007, -0.958, 0.434, 0.263, 0.776, 0.097, 0.83 };
   int lda = 2;
   double B[] = { -0.405, 0.251, 0.13, 0.388, -0.664, -0.732, -0.779, -0.5, 0.775, -0.299, -0.45, 0.923 };
   int ldb = 2;
   double B_expected[] = { -0.026920633021, -0.0986978374343, -0.020841203536, -0.0443113292253, 0.157683298836, 0.0261984465224, 0.099536165222, 0.0486084240644, 0.127725373746, -0.0161073528761, 0.0406652355905, -0.115957262473 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1972) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1972) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 141;
   int uplo = 122;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { -0.169, -0.768, -0.529, 0.236, -0.506, 0.691, -0.786, -0.36 };
   int lda = 2;
   double B[] = { 0.289, -0.985, 0.931, 0.652, -0.861, -0.51, -0.753, -0.542, -0.822, 0.174, 0.799, 0.8 };
   int ldb = 2;
   double B_expected[] = { 0.0420376, 0.0627627, -0.0652, 0.0931, 0.0974426, -0.1131425, 0.0542, -0.0753, -0.0785764, -0.0588129, -0.08, 0.0799 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1973) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1973) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { 0.834, 0.53, 0.278, 0.293, 0.66, 0.497, -0.664, 0.429, -0.294, -0.661, 0.52, -0.247, 0.392, -0.227, 0.209, -0.902, 0.843, 0.37 };
   int lda = 3;
   double B[] = { -0.738, 0.166, 0.721, -0.541, -0.963, -0.832, -0.376, -0.718, 0.765, -0.547, 0.451, -0.581 };
   int ldb = 3;
   double B_expected[] = { -0.115188282202, -0.000411685478887, 0.105497263516, -0.0083759187965, 0.124793492766, -0.0594619308146, 0.0499107469, -0.0152598288542, 0.00927285309719, -0.0831454824908, 0.0380996260983, 0.0702216627003 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1974) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1974) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 121;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { 0.531, -0.691, 0.801, 0.437, 0.402, 0.788, 0.824, 0.599, -0.362, 0.076, 0.192, 0.229, -0.259, -0.279, 0.79, -0.797, 0.728, 0.397 };
   int lda = 3;
   double B[] = { -0.049, 0.642, 0.36, 0.428, 0.523, -0.612, 0.459, -0.664, 0.328, 0.513, -0.225, 0.273 };
   int ldb = 3;
   double B_expected[] = { -0.0941948813, -0.0387898759, -0.0665271, 0.0399732, 0.0612, 0.0523, 0.1143807788, -0.0091687866, -0.0409059, 0.0308683, -0.0273, -0.0225 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1975) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1975) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { 0.169, -0.092, -0.13, 0.001, 0.573, 0.256, 0.632, -0.09, -0.942, 0.948, 0.595, -0.337, 0.01, -0.786, 0.944, 0.906, -0.832, -0.566 };
   int lda = 3;
   double B[] = { -0.461, -0.112, 0.674, -0.268, -0.286, -0.657, 0.329, 0.91, 0.73, 0.488, -0.363, -0.01 };
   int ldb = 3;
   double B_expected[] = { -0.0634274139095, -0.238252532073, -0.142693434208, -0.0938542376785, -0.0907100858097, -0.0412217911039, -0.333617825793, 0.376288993923, -0.0317846476268, 0.175075250306, -0.125200687799, -0.118937960805 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1976) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1976) imag");
     };
   };
  };


  {
   int order = 101;
   int side = 142;
   int uplo = 122;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { -0.53, 0.141, 0.235, 0.474, -0.964, -0.441, 0.197, -0.703, 0.942, 0.98, 0.741, 0.499, -0.738, 0.234, -0.27, -0.158, 0.804, -0.878 };
   int lda = 3;
   double B[] = { 0.46, -0.508, 0.918, -0.516, 0.012, -0.451, -0.676, 0.551, -0.38, 0.053, 0.645, 0.785 };
   int ldb = 3;
   double B_expected[] = { 0.0508, 0.046, 0.0739304, 0.0470256, 0.0992176528, 0.0480511088, -0.0551, -0.0676, -0.0419681, 0.0140525, -0.112456492, 0.0121429348 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1977) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1977) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { 0.286, 0.548, 0.637, -0.856, -0.739, 0.307, -0.049, -0.342, -0.39, 0.618, -0.757, -0.453, -0.533, 0.131, 0.431, 0.087, -0.776, -0.439 };
   int lda = 3;
   double B[] = { 0.968, 0.032, 0.013, 0.684, -0.485, 0.613, 0.316, 0.812, -0.459, 0.34, -0.268, -0.565 };
   int ldb = 2;
   double B_expected[] = { -0.126374952238, 0.0484874156039, -0.0755178690743, -0.200973083054, 0.138328459491, -0.0263170966956, 0.00492064241274, -0.0787874374991, 0.00784239970713, 0.0635860998343, -0.0699577429529, -0.00504052726328 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1978) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1978) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 121;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { -0.911, 0.645, -0.525, 0.045, -0.654, -0.896, -0.39, 0.419, 0.867, 0.561, -0.842, -0.835, -0.249, -0.384, 0.575, -0.41, 0.105, -0.282 };
   int lda = 3;
   double B[] = { 0.777, 0.361, 0.535, 0.441, 0.508, 0.439, -0.347, 0.131, -0.874, 0.646, 0.917, 0.746 };
   int ldb = 2;
   double B_expected[] = { -0.155796389, 0.112639999, 0.0226368685, 0.111048763, -0.042589, 0.127541, 0.067392, -0.0568415, -0.0646, -0.0874, -0.0746, 0.0917 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1979) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1979) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 113;
   int diag = 131;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { -0.519, 0.318, -0.318, 0.73, 0.721, 0.302, -0.604, 0.721, 0.387, 0.673, -0.549, -0.136, 0.101, 0.676, -0.064, -0.659, -0.141, 0.991 };
   int lda = 3;
   double B[] = { -0.856, -0.128, 0.721, -0.511, 0.175, -0.341, 0.832, -0.662, 0.652, -0.939, -0.775, -0.899 };
   int ldb = 2;
   double B_expected[] = { 0.055542329649, 0.130900846188, -0.133470180979, -0.0571415846795, -0.13942012508, 0.0150972236507, 0.0782230770838, 0.0522994181773, -0.00621452256957, -0.0615971232698, 0.0222285648871, 0.258910370231 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1980) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1980) imag");
     };
   };
  };


  {
   int order = 102;
   int side = 142;
   int uplo = 122;
   int trans = 113;
   int diag = 132;
   int M = 2;
   int N = 3;
   double alpha[2] = {0, 0.1};
   double A[] = { -0.092, -0.392, 0.108, -0.918, 0.505, -0.974, 0.213, 0.97, -0.465, 0.604, -0.737, -0.578, -0.051, -0.43, 0.066, -0.934, -0.347, 0.157 };
   int lda = 3;
   double B[] = { -0.489, 0.673, -0.232, 0.668, -0.396, -0.569, 0.763, 0.581, 0.117, -0.249, 0.272, -0.832 };
   int ldb = 2;
   double B_expected[] = { -0.0673, -0.0489, -0.0668, -0.0232, 0.0192782, 0.0274626, -0.0721832, 0.140128, 0.0413393162, 0.1110418366, 0.1221321656, 0.2489754256 };
   cblas_ztrsm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrsm(case 1981) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrsm(case 1981) imag");
     };
   };
  };


}
