#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_trmm (void) {
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
   float A[] = { 0.18f, 0.199f, 0.122f, -0.547f };
   int lda = 2;
   float B[] = { -0.874f, -0.383f, 0.458f, 0.124f, -0.221f, -0.107f };
   int ldb = 3;
   float B_expected[] = { 0.0397932f, 0.0338757f, -0.0183441f, 0.0203484f, -0.0362661f, -0.0175587f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1662)");
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
   float A[] = { 0.195f, -0.453f, -0.579f, 0.697f };
   int lda = 2;
   float B[] = { 0.736f, 0.131f, 0.533f, 0.692f, -0.672f, -0.435f };
   int ldb = 3;
   float B_expected[] = { -0.126757f, -0.130625f, -0.219017f, -0.2076f, 0.2016f, 0.1305f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1663)");
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
   float A[] = { -0.53f, 0.787f, 0.889f, -0.379f };
   int lda = 2;
   float B[] = { -0.355f, 0.002f, 0.266f, 0.972f, 0.712f, -0.353f };
   int ldb = 3;
   float B_expected[] = { -0.056445f, 3.18e-04f, 0.042294f, 0.205195f, 0.080421f, -0.111078f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1664)");
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
   float A[] = { 0.198f, -0.673f, 0.792f, 0.781f };
   int lda = 2;
   float B[] = { 0.901f, 0.719f, -0.339f, -0.36f, 0.539f, 0.192f };
   int ldb = 3;
   float B_expected[] = { -0.2703f, -0.2157f, 0.1017f, -0.106078f, -0.332534f, 0.0229464f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1665)");
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
   float A[] = { -0.522f, 0.851f, 0.586f, 0.196f };
   int lda = 2;
   float B[] = { 0.335f, 0.617f, 0.118f, -0.143f, 0.677f, 0.456f };
   int ldb = 2;
   float B_expected[] = { -0.0560076f, -0.0362796f, 0.0436182f, 0.0084084f, 0.0258534f, -0.0268128f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1666)");
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
   float A[] = { -0.006f, -0.613f, -0.966f, -0.758f };
   int lda = 2;
   float B[] = { 0.64f, -0.723f, -0.765f, 0.801f, 0.376f, 0.91f };
   int ldb = 2;
   float B_expected[] = { -0.401525f, 0.2169f, 0.46163f, -0.2403f, 0.150918f, -0.273f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1667)");
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
   float A[] = { 0.738f, 0.913f, -0.227f, 0.787f };
   int lda = 2;
   float B[] = { 0.194f, 0.988f, -0.274f, -0.652f, -0.281f, -0.359f };
   int ldb = 2;
   float B_expected[] = { -0.0429516f, -0.286403f, 0.0606636f, 0.228986f, 0.0622134f, 0.161726f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1668)");
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
   float A[] = { 0.952f, 0.598f, 0.25f, -0.508f };
   int lda = 2;
   float B[] = { 0.036f, 0.745f, -0.606f, 0.215f, 0.943f, -0.933f };
   int ldb = 2;
   float B_expected[] = { -0.0108f, -0.229958f, 0.1818f, 0.0442164f, -0.2829f, 0.110726f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1669)");
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
   float A[] = { -0.251f, 0.372f, -0.168f, 0.217f, -0.179f, 0.863f, -0.057f, 0.256f, 0.093f };
   int lda = 3;
   float B[] = { -0.727f, -0.461f, 0.162f, 0.579f, -0.305f, -0.735f };
   int ldb = 3;
   float B_expected[] = { -0.0547431f, 0.0563775f, 0.0781923f, 0.0435987f, -0.0809949f, 0.128653f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1670)");
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
   float A[] = { -0.014f, 0.204f, 0.163f, 0.842f, -0.918f, -0.748f, -0.859f, -0.463f, 0.292f };
   int lda = 3;
   float B[] = { -0.587f, -0.625f, -0.994f, 0.681f, -0.577f, -0.434f };
   int ldb = 3;
   float B_expected[] = { 0.1761f, 0.223424f, 0.186654f, -0.2043f, 0.131423f, -0.0325797f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1671)");
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
   float A[] = { -0.682f, -0.71f, 0.475f, -0.59f, -0.748f, 0.548f, 0.245f, 0.761f, -0.4f };
   int lda = 3;
   float B[] = { 0.565f, 0.967f, -0.969f, 0.184f, 0.349f, -0.552f };
   int ldb = 3;
   float B_expected[] = { 0.357979f, 0.438217f, -0.11628f, 0.139991f, 0.204337f, -0.06624f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1672)");
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
   float A[] = { 0.617f, -0.998f, -0.97f, 0.364f, 0.09f, 0.588f, -0.263f, 0.584f, 0.463f };
   int lda = 3;
   float B[] = { 0.773f, 0.074f, -0.388f, 0.825f, -0.608f, 0.788f };
   int ldb = 3;
   float B_expected[] = { -0.270594f, 0.0457776f, 0.1164f, -0.118933f, 0.0443424f, -0.2364f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1673)");
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
   float A[] = { 0.217f, -0.672f, -0.378f, -0.005f, -0.586f, -0.426f, 0.765f, -0.239f, -0.145f };
   int lda = 3;
   float B[] = { 0.01f, 0.387f, -0.953f, -0.374f, -0.673f, -0.724f };
   int ldb = 2;
   float B_expected[] = { -6.51e-04f, -0.0251937f, -0.167522f, -0.0651687f, -0.0999006f, -0.147126f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1674)");
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
   float A[] = { 0.962f, 0.515f, 0.292f, 0.354f, -0.366f, 0.455f, 0.134f, -0.564f, -0.303f };
   int lda = 3;
   float B[] = { -0.337f, 0.718f, -0.866f, -0.454f, -0.439f, -0.668f };
   int ldb = 2;
   float B_expected[] = { 0.1011f, -0.2154f, 0.295589f, 0.0599484f, -0.0012798f, 0.0947196f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1675)");
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
   float A[] = { -0.228f, -0.097f, 0.205f, 0.875f, -0.162f, 0.542f, -0.839f, -0.935f, 0.2f };
   int lda = 3;
   float B[] = { -0.125f, -0.676f, 0.181f, 0.741f, 0.216f, 0.766f };
   int ldb = 2;
   float B_expected[] = { -0.0165669f, -0.0717843f, -0.026325f, -0.088539f, -0.01296f, -0.04596f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1676)");
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
   float A[] = { -0.854f, -0.502f, 0.591f, -0.934f, -0.729f, 0.063f, 0.352f, 0.126f, -0.905f };
   int lda = 3;
   float B[] = { -0.626f, -0.694f, -0.889f, -0.251f, -0.42f, -0.353f };
   int ldb = 2;
   float B_expected[] = { 0.128383f, 0.232986f, 0.274638f, 0.0819717f, 0.126f, 0.1059f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1677)");
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
   float alpha = 0.1f;
   float A[] = { -0.755f, 0.12f, 0.525f, 0.917f };
   int lda = 2;
   float B[] = { -0.927f, -0.813f, 0.624f, -0.366f, -0.864f, -0.046f };
   int ldb = 3;
   float B_expected[] = { 0.0699885f, 0.0613815f, -0.047112f, -0.0446862f, -0.0889848f, 0.0032698f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1678)");
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
   float alpha = 0.1f;
   float A[] = { -0.444f, 0.515f, 0.081f, -0.69f };
   int lda = 2;
   float B[] = { 0.571f, -0.098f, -0.226f, -0.587f, 0.788f, -0.629f };
   int ldb = 3;
   float B_expected[] = { 0.0571f, -0.0098f, -0.0226f, -0.0292935f, 0.073753f, -0.074539f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1679)");
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
   float alpha = 0.1f;
   float A[] = { -0.954f, 0.651f, -0.982f, 0.388f };
   int lda = 2;
   float B[] = { -0.927f, -0.281f, -0.918f, -0.527f, -0.652f, -0.393f };
   int ldb = 3;
   float B_expected[] = { 0.140187f, 0.0908338f, 0.12617f, -0.0204476f, -0.0252976f, -0.0152484f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1680)");
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
   float alpha = 0.1f;
   float A[] = { 0.811f, 0.852f, 0.224f, 0.443f };
   int lda = 2;
   float B[] = { -0.493f, -0.497f, -0.605f, 0.433f, -0.082f, -0.077f };
   int ldb = 3;
   float B_expected[] = { -0.0396008f, -0.0515368f, -0.0622248f, 0.0433f, -0.0082f, -0.0077f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1681)");
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
   float alpha = 0.1f;
   float A[] = { -0.777f, 0.812f, 0.254f, 0.97f };
   int lda = 2;
   float B[] = { -0.509f, 0.171f, 0.986f, -0.644f, -0.97f, 0.814f };
   int ldb = 2;
   float B_expected[] = { 0.0395493f, 0.0036584f, -0.0766122f, -0.0374236f, 0.075369f, 0.05432f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1682)");
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
   float alpha = 0.1f;
   float A[] = { 0.962f, 0.912f, -0.238f, -0.336f };
   int lda = 2;
   float B[] = { -0.666f, 0.066f, -0.176f, 0.402f, 0.286f, -0.703f };
   int ldb = 2;
   float B_expected[] = { -0.0666f, 0.0224508f, -0.0176f, 0.0443888f, 0.0286f, -0.0771068f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1683)");
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
   float alpha = 0.1f;
   float A[] = { 0.859f, -0.547f, 0.076f, 0.542f };
   int lda = 2;
   float B[] = { 0.402f, 0.945f, -0.242f, -0.062f, 0.714f, 0.468f };
   int ldb = 2;
   float B_expected[] = { -0.0171597f, 0.051219f, -0.0173964f, -0.0033604f, 0.035733f, 0.0253656f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1684)");
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
   float alpha = 0.1f;
   float A[] = { -0.779f, 0.435f, 0.612f, -0.723f };
   int lda = 2;
   float B[] = { 0.512f, -0.987f, -0.167f, 0.047f, -0.701f, -0.25f };
   int ldb = 2;
   float B_expected[] = { 0.0082655f, -0.0987f, -0.0146555f, 0.0047f, -0.080975f, -0.025f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1685)");
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
   float alpha = 0.1f;
   float A[] = { -0.757f, 0.396f, -0.927f, -0.558f, -0.289f, -0.66f, 0.83f, 0.363f, -0.13f };
   int lda = 3;
   float B[] = { 0.041f, 0.333f, -0.682f, 0.193f, 0.581f, 0.963f };
   int ldb = 3;
   float B_expected[] = { 0.0733045f, 0.0353883f, 0.008866f, -0.0808726f, -0.0803489f, -0.012519f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1686)");
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
   float alpha = 0.1f;
   float A[] = { -0.75f, 0.674f, -0.576f, 0.376f, -0.46f, -0.813f, 0.419f, 0.792f, 0.226f };
   int lda = 3;
   float B[] = { 0.511f, -0.544f, 0.938f, -0.126f, -0.873f, 0.118f };
   int ldb = 3;
   float B_expected[] = { -0.0395944f, -0.130659f, 0.0938f, -0.078237f, -0.0968934f, 0.0118f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1687)");
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
   float alpha = 0.1f;
   float A[] = { -0.045f, -0.809f, 0.654f, 0.611f, -0.038f, -0.105f, -0.946f, 0.474f, -0.097f };
   int lda = 3;
   float B[] = { -0.625f, -0.123f, -0.48f, -0.088f, -0.757f, 0.974f };
   int ldb = 3;
   float B_expected[] = { 0.0028125f, -0.0377201f, 0.0579508f, 3.96e-04f, -0.0025002f, -0.0370048f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1688)");
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
   float alpha = 0.1f;
   float A[] = { 0.713f, 0.781f, 0.084f, -0.498f, 0.692f, 0.125f, 0.706f, -0.118f, -0.907f };
   int lda = 3;
   float B[] = { 0.442f, -0.563f, 0.065f, -0.18f, 0.63f, -0.328f };
   int ldb = 3;
   float B_expected[] = { 0.0442f, -0.0783116f, 0.0443486f, -0.018f, 0.071964f, -0.052942f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1689)");
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
   float alpha = 0.1f;
   float A[] = { -0.442f, 0.566f, 0.064f, 0.962f, -0.669f, 0.416f, 0.761f, -0.359f, 0.863f };
   int lda = 3;
   float B[] = { 0.261f, -0.659f, -0.536f, 0.694f, -0.305f, -0.675f };
   int ldb = 2;
   float B_expected[] = { -0.0863099f, 0.0445231f, 0.0468079f, -0.0221961f, -0.0263215f, -0.0582525f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1690)");
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
   float alpha = 0.1f;
   float A[] = { 0.386f, 0.643f, -0.028f, -0.758f, -0.63f, -0.043f, 0.666f, -0.088f, 0.382f };
   int lda = 3;
   float B[] = { -0.241f, 0.766f, 0.656f, -0.977f, 0.274f, 0.565f };
   int ldb = 2;
   float B_expected[] = { -0.0555764f, 0.188286f, 0.0631888f, -0.102672f, 0.0274f, 0.0565f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1691)");
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
   float alpha = 0.1f;
   float A[] = { -0.855f, -0.587f, 0.062f, 0.372f, 0.48f, -0.63f, -0.786f, -0.437f, -0.431f };
   int lda = 3;
   float B[] = { 0.116f, 0.534f, 0.043f, 0.73f, 0.945f, 0.528f };
   int ldb = 2;
   float B_expected[] = { -0.009918f, -0.045657f, -0.0047452f, 0.0036942f, -0.0427193f, -0.065436f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1692)");
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
   float alpha = 0.1f;
   float A[] = { -0.068f, 0.119f, -0.244f, -0.05f, 0.685f, 0.752f, -0.059f, -0.935f, -0.571f };
   int lda = 3;
   float B[] = { -0.753f, -0.319f, 0.164f, 0.979f, 0.885f, -0.822f };
   int ldb = 2;
   float B_expected[] = { -0.0753f, -0.0319f, 0.0074393f, 0.0941039f, 0.119206f, -7.956e-04f };
   cblas_strmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], flteps, "strmm(case 1693)");
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
   double alpha = -0.3;
   double A[] = { 0.174, -0.308, 0.997, -0.484 };
   int lda = 2;
   double B[] = { -0.256, -0.178, 0.098, 0.004, 0.97, -0.408 };
   int ldb = 3;
   double B_expected[] = { 0.0137328, 0.0989196, -0.0428148, 5.808e-04, 0.140844, -0.0592416 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1694)");
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
   double alpha = -0.3;
   double A[] = { 0.722, -0.372, 0.466, -0.831 };
   int lda = 2;
   double B[] = { 0.322, -0.183, 0.849, -0.051, -0.343, -0.98 };
   int ldb = 3;
   double B_expected[] = { -0.1022916, 0.0166212, -0.364068, 0.0153, 0.1029, 0.294 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1695)");
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
   double alpha = -0.3;
   double A[] = { -0.656, -0.066, 0.582, 0.141 };
   int lda = 2;
   double B[] = { 0.73, 0.407, 0.721, 0.086, -0.294, 0.941 };
   int ldb = 3;
   double B_expected[] = { 0.143664, 0.0800976, 0.1418928, -0.1310958, -0.058626, -0.1656909 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1696)");
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
   double alpha = -0.3;
   double A[] = { -0.341, 0.386, -0.578, 0.863 };
   int lda = 2;
   double B[] = { -0.306, -0.047, -0.162, -0.784, 0.472, 0.137 };
   int ldb = 3;
   double B_expected[] = { 0.0918, 0.0141, 0.0486, 0.1821396, -0.1497498, -0.0691908 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1697)");
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
   double alpha = -0.3;
   double A[] = { 0.844, -0.832, 0.179, -0.775 };
   int lda = 2;
   double B[] = { -0.415, -0.547, -0.023, 0.42, 0.917, 0.485 };
   int ldb = 2;
   double B_expected[] = { 0.1344519, -0.1271775, -0.0167304, 0.09765, -0.2582289, 0.1127625 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1698)");
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
   double alpha = -0.3;
   double A[] = { 0.239, 0.34, 0.964, -0.575 };
   int lda = 2;
   double B[] = { 0.762, -0.038, -0.8, 0.626, -0.701, 0.639 };
   int ldb = 2;
   double B_expected[] = { -0.2176104, 0.0114, 0.0589608, -0.1878, 0.0255012, -0.1917 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1699)");
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
   double alpha = -0.3;
   double A[] = { 0.785, -0.0, -0.592, -0.661 };
   int lda = 2;
   double B[] = { -0.215, 0.953, 0.527, -0.418, -0.675, 0.283 };
   int ldb = 2;
   double B_expected[] = { 0.0506325, 0.1889799, -0.1241085, -0.0828894, 0.1589625, 0.0561189 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1700)");
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
   double alpha = -0.3;
   double A[] = { -0.423, -0.807, -0.683, -0.225 };
   int lda = 2;
   double B[] = { 0.149, -0.129, 0.149, -0.234, 0.275, 0.658 };
   int ldb = 2;
   double B_expected[] = { -0.0447, 0.0747729, -0.0447, 0.1062729, -0.0825, -0.1308225 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1701)");
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
   double alpha = -0.3;
   double A[] = { -0.276, 0.434, 0.917, 0.682, -0.32, 0.557, -0.302, 0.989, -0.043 };
   int lda = 3;
   double B[] = { -0.943, 0.839, 0.759, 0.752, 0.807, 0.288 };
   int ldb = 3;
   double B_expected[] = { -0.0780804, 0.2033226, 0.1290135, 0.0622656, -0.0204384, -0.3380097 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1702)");
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
   double alpha = -0.3;
   double A[] = { -0.731, -0.953, -0.666, 0.684, 0.38, 0.419, -0.361, 0.378, -0.423 };
   int lda = 3;
   double B[] = { -0.983, 0.479, -0.136, 0.048, 0.745, -0.408 };
   int ldb = 3;
   double B_expected[] = { 0.2949, -0.4247397, -0.2158137, -0.0144, -0.2097768, 0.0383439 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1703)");
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
   double alpha = -0.3;
   double A[] = { -0.953, -0.983, 0.237, 0.128, -0.378, 0.607, 0.41, 0.418, -0.221 };
   int lda = 3;
   double B[] = { -0.561, -0.114, -0.148, 0.488, 0.146, -0.688 };
   int ldb = 3;
   double B_expected[] = { -0.1378083, 0.0056316, -0.0098124, 0.2185368, 0.1028316, -0.0456144 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1704)");
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
   double alpha = -0.3;
   double A[] = { 0.277, -0.587, 0.885, -0.933, -0.582, 0.528, 0.268, -0.804, 0.62 };
   int lda = 3;
   double B[] = { -0.831, -0.319, -0.547, -0.577, 0.295, -0.31 };
   int ldb = 3;
   double B_expected[] = { 0.2039907, -0.0362364, 0.1641, 0.2805945, -0.163272, 0.093 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1705)");
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
   double alpha = -0.3;
   double A[] = { 0.256, 0.554, 0.342, 0.318, -0.824, -0.119, -0.399, -0.653, -0.83 };
   int lda = 3;
   double B[] = { -0.577, 0.861, -0.439, -0.916, 0.452, -0.168 };
   int ldb = 2;
   double B_expected[] = { 0.0443136, -0.0661248, -0.053475, -0.3085746, -0.042519, -0.1182147 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1706)");
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
   double alpha = -0.3;
   double A[] = { 0.837, -0.03, 0.552, -0.43, 0.841, 0.035, 0.7, 0.637, 0.095 };
   int lda = 3;
   double B[] = { -0.82, -0.362, -0.252, -0.062, -0.942, -0.299 };
   int ldb = 2;
   double B_expected[] = { 0.246, 0.1086, -0.03018, -0.028098, 0.5029572, 0.1775682 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1707)");
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
   double alpha = -0.3;
   double A[] = { -0.074, 0.49, 0.802, -0.454, 0.626, 0.123, -0.959, 0.971, 0.75 };
   int lda = 3;
   double B[] = { -0.545, -0.107, 0.096, 0.183, 0.185, -0.218 };
   int ldb = 2;
   double B_expected[] = { -0.070722, 0.0231744, -0.0248553, -0.0263232, -0.041625, 0.04905 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1708)");
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
   double alpha = -0.3;
   double A[] = { 0.048, 0.148, 0.834, -0.98, -0.009, -0.727, 0.241, 0.276, 0.518 };
   int lda = 3;
   double B[] = { -0.664, -0.136, -0.793, -0.742, 0.126, -0.131 };
   int ldb = 2;
   double B_expected[] = { 0.202884, 0.106521, 0.2653806, 0.1940289, -0.0378, 0.0393 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1709)");
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
   double alpha = 0.1;
   double A[] = { 0.427, 0.495, 0.282, 0.158 };
   int lda = 2;
   double B[] = { 0.899, -0.375, 0.376, -0.831, 0.431, -0.387 };
   int ldb = 3;
   double B_expected[] = { 0.0383873, -0.0160125, 0.0160552, 0.0313707, -0.0117527, 0.0124974 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1710)");
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
   double alpha = 0.1;
   double A[] = { 0.632, -0.174, 0.608, -0.669 };
   int lda = 2;
   double B[] = { -0.335, 0.535, -0.978, 0.31, 0.023, -0.853 };
   int ldb = 3;
   double B_expected[] = { -0.0335, 0.0535, -0.0978, 0.036829, -0.007009, -0.0682828 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1711)");
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
   double alpha = 0.1;
   double A[] = { -0.779, -0.73, 0.343, -0.665 };
   int lda = 2;
   double B[] = { -0.976, -0.2, 0.661, -0.975, -0.965, -0.861 };
   int ldb = 3;
   double B_expected[] = { 0.0425879, -0.0175195, -0.0810242, 0.0648375, 0.0641725, 0.0572565 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1712)");
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
   double alpha = 0.1;
   double A[] = { -0.127, -0.634, -0.384, -0.815 };
   int lda = 2;
   double B[] = { -0.348, 0.748, 0.893, 0.91, 0.153, -0.408 };
   int ldb = 3;
   double B_expected[] = { -0.069744, 0.0689248, 0.1049672, 0.091, 0.0153, -0.0408 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1713)");
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
   double alpha = 0.1;
   double A[] = { -0.603, -0.617, 0.402, -0.918 };
   int lda = 2;
   double B[] = { 0.051, -0.096, 0.476, 0.377, 0.931, 0.291 };
   int ldb = 2;
   double B_expected[] = { -0.0030753, 0.010863, -0.0287028, -0.0154734, -0.0561393, 0.0107124 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1714)");
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
   double alpha = 0.1;
   double A[] = { 0.67, -0.475, 0.032, -0.036 };
   int lda = 2;
   double B[] = { -0.19, 0.829, 0.942, 0.885, 0.087, 0.321 };
   int ldb = 2;
   double B_expected[] = { -0.019, 0.082292, 0.0942, 0.0915144, 0.0087, 0.0323784 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1715)");
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
   double alpha = 0.1;
   double A[] = { -0.64, 0.595, 0.642, -0.921 };
   int lda = 2;
   double B[] = { -0.278, -0.83, 0.922, -0.701, -0.598, -0.232 };
   int ldb = 2;
   double B_expected[] = { -0.031593, 0.076443, -0.1007175, 0.0645621, 0.024468, 0.0213672 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1716)");
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
   double alpha = 0.1;
   double A[] = { 0.842, 0.625, 0.967, 0.341 };
   int lda = 2;
   double B[] = { -0.679, -0.846, -0.921, 0.672, 0.292, 0.752 };
   int ldb = 2;
   double B_expected[] = { -0.120775, -0.0846, -0.0501, 0.0672, 0.0762, 0.0752 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1717)");
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
   double alpha = 0.1;
   double A[] = { -0.612, 0.593, 0.113, -0.658, 0.703, -0.023, -0.384, 0.439, 0.958 };
   int lda = 3;
   double B[] = { -0.858, -0.559, 0.499, -0.114, 0.57, 0.847 };
   int ldb = 3;
   double B_expected[] = { 0.0249996, -0.0404454, 0.0478042, 0.0503489, 0.0381229, 0.0811426 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1718)");
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
   double alpha = 0.1;
   double A[] = { 0.844, 0.205, -0.692, -0.401, -0.823, 0.342, -0.384, 0.344, 0.18 };
   int lda = 3;
   double B[] = { 0.823, -0.181, 0.141, 0.932, 0.097, -0.636 };
   int ldb = 3;
   double B_expected[] = { 0.0688323, -0.0132778, 0.0141, 0.1391997, -0.0120512, -0.0636 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1719)");
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
   double alpha = 0.1;
   double A[] = { 0.065, 0.678, 0.044, -0.472, 0.932, -0.388, 0.432, -0.167, -0.277 };
   int lda = 3;
   double B[] = { 0.675, -0.468, -0.564, 0.71, -0.624, 0.023 };
   int ldb = 3;
   double B_expected[] = { 0.0043875, -0.0754776, 0.0525984, 0.004615, -0.0916688, 0.0404557 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1720)");
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
   double alpha = 0.1;
   double A[] = { 0.649, -0.171, -0.462, 0.593, 0.131, -0.317, -0.254, -0.948, 0.002 };
   int lda = 3;
   double B[] = { -0.519, -0.501, -0.024, -0.767, -0.591, -0.738 };
   int ldb = 3;
   double B_expected[] = { -0.0519, -0.0808767, 0.0582774, -0.0767, -0.1045831, 0.0017086 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1721)");
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
   double alpha = 0.1;
   double A[] = { -0.023, -0.872, -0.313, -0.698, 0.06, -0.838, -0.455, -0.715, -0.257 };
   int lda = 3;
   double B[] = { -0.17, -0.184, -0.243, 0.907, -0.423, 0.665 };
   int ldb = 2;
   double B_expected[] = { 0.0365989, -0.0931429, 0.0287865, -0.0421055, 0.0108711, -0.0170905 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1722)");
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
   double alpha = 0.1;
   double A[] = { 0.792, 0.338, -0.155, 0.009, 0.485, -0.633, -0.08, -0.579, 0.223 };
   int lda = 3;
   double B[] = { -0.19, 0.201, 0.685, 0.663, 0.302, -0.506 };
   int ldb = 2;
   double B_expected[] = { -0.0207995, 0.0247447, 0.0510142, 0.0955974, 0.0302, -0.0506 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1723)");
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
   double alpha = 0.1;
   double A[] = { -0.076, 0.103, -0.021, -0.866, 0.777, 0.723, 0.378, 0.98, -0.32 };
   int lda = 3;
   double B[] = { 0.739, -0.996, 0.182, 0.626, 0.291, -0.267 };
   int ldb = 2;
   double B_expected[] = { -0.0056164, 0.0075696, 0.0217531, 0.0383814, 0.0022947, 0.0558954 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1724)");
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
   double alpha = 0.1;
   double A[] = { 0.469, 0.822, -0.619, 0.953, -0.706, 0.318, 0.559, -0.68, -0.208 };
   int lda = 3;
   double B[] = { 0.362, 0.719, -0.661, -0.504, 0.595, -0.771 };
   int ldb = 2;
   double B_expected[] = { 0.0362, 0.0719, -0.0363436, 0.0087018, 0.0160724, -0.1376333 };
   cblas_dtrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[i], B_expected[i], dbleps, "dtrmm(case 1725)");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { -0.023f, 0.762f, -0.687f, -0.039f, -0.459f, 0.047f, 0.189f, 0.33f };
   int lda = 2;
   float B[] = { 0.827f, -0.561f, 0.641f, -0.229f, -0.884f, -0.533f, -0.624f, -0.138f, 0.073f, 0.924f, -0.501f, -0.164f };
   int ldb = 3;
   float B_expected[] = { -0.831767f, -0.762219f, -0.14564f, 0.143926f, -0.764269f, 0.529142f, 0.072396f, 0.232002f, 0.291123f, -0.198726f, 0.040569f, 0.196326f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1726) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1726) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { 0.24f, 0.581f, 0.06f, 0.064f, 0.981f, 0.792f, 0.242f, -0.529f };
   int lda = 2;
   float B[] = { -0.649f, -0.774f, -0.43f, -0.447f, -0.266f, 0.285f, 0.787f, 0.274f, 0.449f, -0.912f, 0.435f, 0.601f };
   int ldb = 3;
   float B_expected[] = { 0.619316f, 0.707192f, 0.344692f, 0.472984f, 0.278364f, -0.3489f, -0.787f, -0.274f, -0.449f, 0.912f, -0.435f, -0.601f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1727) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1727) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { 0.68f, -0.728f, -0.59f, -0.434f, -0.936f, 0.915f, 0.236f, -0.118f };
   int lda = 2;
   float B[] = { 0.461f, 0.48f, 0.224f, 0.215f, -0.419f, -0.525f, 0.113f, -0.582f, 0.468f, 0.269f, 0.943f, -0.587f };
   int ldb = 3;
   float B_expected[] = { -0.66292f, 0.009208f, -0.30884f, 0.016872f, 0.66712f, 0.051968f, 0.912704f, 0.178151f, 0.264199f, -0.01198f, -1.02584f, 0.141791f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1728) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1728) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { 0.699f, -0.709f, -0.775f, 0.779f, 0.5f, 0.774f, -0.399f, -0.843f };
   int lda = 2;
   float B[] = { 0.538f, 0.556f, -0.186f, -0.678f, -0.413f, -0.612f, -0.216f, -0.519f, -0.344f, -0.578f, -0.938f, -0.848f };
   int ldb = 3;
   float B_expected[] = { -0.538f, -0.556f, 0.186f, 0.678f, 0.413f, 0.612f, 0.377344f, -0.175412f, -0.087772f, 1.06096f, 0.670812f, 1.47366f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1729) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1729) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { 0.527f, 0.318f, -0.224f, 0.547f, -0.765f, -0.469f, 0.233f, 0.023f };
   int lda = 2;
   float B[] = { 0.54f, -0.418f, -0.892f, -0.118f, -0.296f, 0.019f, 0.786f, -0.145f, 0.136f, 0.472f, 0.731f, 0.333f };
   int ldb = 2;
   float B_expected[] = { -1.04454f, -0.460052f, 0.205122f, 0.04801f, 0.831329f, 0.341824f, -0.186473f, 0.015707f, 0.481462f, 0.305592f, -0.162664f, -0.094402f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1730) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1730) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { -0.109f, -0.852f, 0.395f, 0.871f, 0.378f, -0.493f, 0.51f, 0.973f };
   int lda = 2;
   float B[] = { -0.867f, -0.758f, 0.687f, -0.596f, -0.912f, -0.561f, -0.389f, 0.21f, -0.561f, 0.132f, 0.689f, 0.653f };
   int ldb = 2;
   float B_expected[] = { 0.901142f, 1.32198f, -0.687f, 0.596f, 0.955512f, 0.289843f, 0.389f, -0.21f, -0.021371f, -0.039157f, -0.689f, -0.653f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1731) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1731) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { 0.686f, 0.349f, 0.299f, -0.462f, 0.009f, -0.693f, -0.478f, -0.617f };
   int lda = 2;
   float B[] = { -0.409f, 0.986f, -0.854f, 0.346f, 0.444f, -0.659f, 0.027f, 0.007f, 0.842f, -0.473f, 0.825f, 0.866f };
   int ldb = 2;
   float B_expected[] = { 0.624688f, -0.533655f, -0.954935f, -0.845302f, -0.534575f, 0.297118f, 0.180289f, 0.422174f, -0.742689f, 0.03062f, -0.173204f, 1.4534f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1732) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1732) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { -0.286f, 0.661f, 0.372f, 0.28f, 0.482f, 0.267f, -0.436f, 0.844f };
   int lda = 2;
   float B[] = { 0.0f, -0.513f, 0.91f, 0.109f, 0.587f, -0.183f, 0.112f, 0.362f, -0.256f, -0.518f, -0.933f, 0.066f };
   int ldb = 2;
   float B_expected[] = { 0.0f, 0.513f, -1.05364f, 0.081836f, -0.587f, 0.183f, -0.381604f, -0.458284f, 0.256f, 0.518f, 0.883192f, 0.198376f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1733) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1733) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { 0.678f, 0.717f, 0.228f, 0.001f, -0.16f, -0.387f, -0.281f, -0.002f, 0.623f, 0.162f, -0.594f, 0.632f, 0.566f, 0.352f, -0.411f, 0.574f, 0.314f, -0.139f };
   int lda = 3;
   float B[] = { -0.823f, -0.042f, 0.171f, -0.928f, 0.66f, 0.965f, 0.472f, 0.006f, -0.083f, 0.937f, -0.814f, 0.9f };
   int ldb = 3;
   float B_expected[] = { 0.52788f, 0.618567f, -0.069267f, 0.560841f, -0.941723f, -1.19579f, -0.315714f, -0.342492f, 0.095893f, -0.572145f, 0.746576f, 0.396912f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1734) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1734) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { 0.346f, 0.915f, -0.227f, -0.066f, -0.166f, -0.921f, -0.373f, 0.312f, -0.824f, 0.699f, -0.114f, -0.152f, 0.862f, -0.077f, 0.221f, -0.757f, -0.413f, -0.494f };
   int lda = 3;
   float B[] = { -0.02f, -0.247f, -0.62f, 0.651f, -0.07f, -0.491f, 0.042f, 0.936f, 0.272f, -0.582f, 0.012f, -0.534f };
   int ldb = 3;
   float B_expected[] = { 0.02f, 0.247f, 0.631762f, -0.708389f, 0.124535f, 0.411552f, -0.042f, -0.936f, -0.324242f, 0.797244f, -0.747612f, 0.703054f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1735) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1735) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { -0.493f, -0.882f, -0.82f, 0.627f, 0.301f, -0.903f, -0.092f, 0.787f, -0.426f, -0.854f, -0.993f, 0.118f, 0.615f, 0.362f, -0.238f, -0.076f, 0.817f, -0.286f };
   int lda = 3;
   float B[] = { 0.395f, 0.074f, -0.191f, -0.548f, 0.858f, 0.323f, -0.734f, 0.612f, 0.895f, 0.849f, 0.811f, 0.402f };
   int ldb = 3;
   float B_expected[] = { -0.730125f, -0.024468f, 0.566282f, -0.25448f, -0.793364f, -0.018503f, -0.504384f, -1.51274f, -0.18131f, 1.28332f, -0.777559f, -0.096488f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1736) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1736) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { -0.033f, -0.383f, 0.116f, 0.797f, -0.99f, 0.765f, 0.915f, 0.002f, 0.228f, 0.077f, 0.597f, -0.454f, -0.629f, 0.424f, -0.89f, 0.339f, -0.484f, 0.169f };
   int lda = 3;
   float B[] = { -0.377f, -0.451f, -0.464f, -0.673f, 0.231f, -0.712f, -0.457f, -0.588f, 0.373f, -0.754f, -0.468f, 0.433f };
   int ldb = 3;
   float B_expected[] = { 0.643625f, 0.521931f, 0.428222f, -0.038989f, -0.231f, 0.712f, 0.003417f, 1.74795f, -0.642733f, 1.29802f, 0.468f, -0.433f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1737) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1737) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { 0.946f, -0.007f, 0.677f, -0.923f, 0.651f, -0.685f, 0.591f, 0.135f, 0.171f, 0.979f, -0.029f, -0.008f, -0.049f, 0.174f, 0.578f, 0.388f, 0.187f, -0.479f };
   int lda = 3;
   float B[] = { -0.607f, -0.907f, -0.156f, -0.141f, -0.254f, 0.364f, 0.209f, 0.955f, 0.93f, 0.962f, 0.494f, 0.079f };
   int ldb = 2;
   float B_expected[] = { 0.580571f, 0.853773f, 0.148563f, 0.132294f, 0.636082f, 0.804404f, 0.972367f, -0.263525f, -0.534225f, 0.214911f, 0.087341f, -0.390994f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1738) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1738) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { 0.203f, -0.791f, -0.415f, -0.56f, 0.782f, -0.691f, -0.441f, 0.545f, -0.09f, 0.595f, -0.438f, 0.952f, 0.88f, 0.944f, -0.55f, -0.762f, -0.035f, -0.949f };
   int lda = 3;
   float B[] = { -0.035f, 0.448f, 0.487f, -0.108f, -0.482f, -0.708f, -0.317f, 0.816f, -0.547f, 0.22f, -0.654f, 0.57f };
   int ldb = 2;
   float B_expected[] = { 0.035f, -0.448f, -0.487f, 0.108f, 0.710725f, 0.924643f, 0.472907f, -1.12904f, 1.27511f, -1.33788f, -0.672654f, -0.727442f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1739) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1739) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { -0.09f, 0.742f, 0.081f, 0.459f, -0.54f, 0.04f, 0.574f, -0.858f, 0.704f, 0.686f, -0.9f, -0.519f, 0.538f, -0.934f, 0.467f, 0.376f, 0.149f, 0.322f };
   int lda = 3;
   float B[] = { 0.307f, 0.294f, -0.428f, -0.7f, 0.496f, 0.167f, -0.611f, 0.904f, -0.846f, -0.411f, 0.29f, 0.004f };
   int ldb = 2;
   float B_expected[] = { -0.191025f, -0.630625f, 0.063267f, 0.452361f, -0.782713f, -1.2668f, 1.30921f, -0.06316f, -0.006288f, 0.333651f, -0.041922f, -0.093976f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1740) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1740) imag");
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
   float alpha[2] = {-1.0f, 0.0f};
   float A[] = { 0.434f, 0.691f, 0.983f, -0.481f, -0.156f, -0.117f, -0.231f, 0.526f, 0.935f, 0.417f, -0.142f, -0.541f, 0.529f, 0.014f, 0.266f, 0.086f, 0.666f, 0.033f };
   int lda = 3;
   float B[] = { 0.972f, -0.219f, -0.735f, -0.967f, 0.084f, -0.355f, -0.152f, -0.156f, 0.267f, 0.928f, 0.708f, -0.267f };
   int ldb = 2;
   float B_expected[] = { -0.950741f, 0.784376f, 1.10114f, 1.08842f, -0.548134f, 0.631223f, 0.396983f, 0.501114f, -0.267f, -0.928f, -0.708f, 0.267f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1741) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1741) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.25f, -0.779f, -0.138f, -0.017f, -0.319f, -0.555f, 0.674f, -0.256f };
   int lda = 2;
   float B[] = { -0.651f, -0.525f, 0.409f, -0.932f, 0.359f, 0.321f, 0.419f, 0.027f, 0.67f, 0.328f, 0.446f, -0.615f };
   int ldb = 3;
   float B_expected[] = { 0.0100296f, -0.216136f, 0.257045f, -0.0571445f, -0.0121016f, 0.124004f, -0.110514f, 0.0386878f, -0.1561f, -0.0050383f, 0.028185f, 0.183634f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1742) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1742) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.012f, 0.978f, 0.617f, -0.361f, -0.349f, 0.712f, 0.008f, 0.305f };
   int lda = 2;
   float B[] = { -0.771f, -0.335f, -0.565f, 0.866f, -0.516f, -0.869f, -0.097f, -0.711f, 0.308f, 0.207f, -0.459f, 0.766f };
   int ldb = 3;
   float B_expected[] = { 0.2648f, 0.0234f, 0.0829f, -0.3163f, 0.2417f, 0.2091f, 0.272029f, 0.122445f, -0.176135f, -0.256384f, 0.285714f, -0.233939f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1743) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1743) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.063f, -0.605f, 0.473f, 0.763f, 0.548f, -0.167f, -0.825f, 0.011f };
   int lda = 2;
   float B[] = { -0.262f, 0.135f, -0.333f, -0.671f, 0.91f, 0.874f, 0.305f, -0.255f, 0.882f, 0.883f, 0.088f, -0.473f };
   int ldb = 3;
   float B_expected[] = { -0.0627538f, 0.0344746f, -0.131779f, -0.149516f, -0.0442507f, 0.307921f, 0.053273f, -0.089001f, 0.293086f, 0.141896f, -0.0189002f, -0.124098f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1744) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1744) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.493f, -0.852f, -0.567f, 0.21f, 0.168f, 0.666f, -0.328f, 0.803f };
   int lda = 2;
   float B[] = { 0.24f, -0.578f, 0.293f, -0.233f, -0.348f, -0.853f, -0.145f, 0.192f, -0.785f, -0.72f, -0.508f, 0.023f };
   int ldb = 3;
   float B_expected[] = { 0.037901f, 0.201471f, -0.104515f, 0.327095f, 0.253345f, 0.311373f, 0.0243f, -0.0721f, 0.3075f, 0.1375f, 0.1501f, -0.0577f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1745) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1745) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.089f, -0.135f, 0.987f, 0.936f, 0.353f, 0.638f, 0.845f, 0.343f };
   int lda = 2;
   float B[] = { 0.744f, 0.445f, 0.835f, 0.273f, 0.702f, 0.03f, -0.618f, 0.141f, -0.303f, -0.399f, 0.63f, -0.037f };
   int ldb = 2;
   float B_expected[] = { 0.0158468f, 0.0413994f, -0.292082f, -0.285588f, 0.0272724f, 0.0233892f, 0.0660084f, -0.143882f, 0.0004278f, -0.0256146f, -0.19286f, 0.114065f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1746) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1746) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.187f, -0.741f, 0.287f, -0.599f, -0.293f, -0.297f, 0.778f, -0.056f };
   int lda = 2;
   float B[] = { -0.335f, -0.713f, 0.081f, -0.589f, -0.256f, -0.809f, -0.473f, 0.418f, 0.646f, -0.447f, -0.147f, 0.314f };
   int ldb = 2;
   float B_expected[] = { 0.1718f, 0.1804f, 0.0378414f, 0.0809182f, 0.1577f, 0.2171f, 0.118373f, -0.283147f, -0.1491f, 0.1987f, 0.1154f, -0.122836f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1747) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1747) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.259f, -0.645f, -0.09f, 0.709f, 0.729f, -0.023f, -0.792f, 0.03f };
   int lda = 2;
   float B[] = { 0.904f, -0.402f, 0.753f, 0.104f, 0.38f, 0.944f, -0.715f, -0.378f, -0.16f, 0.254f, -0.68f, 0.183f };
   int ldb = 2;
   float B_expected[] = { 0.185924f, -0.0771597f, 0.185827f, -0.0420162f, -0.156592f, 0.373034f, -0.201079f, -0.0256158f, 0.0051007f, 0.152025f, -0.143387f, 0.102908f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1748) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1748) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.938f, 0.25f, -0.509f, 0.377f, -0.063f, 0.166f, 0.227f, -0.24f };
   int lda = 2;
   float B[] = { 0.756f, -0.08f, -0.657f, -0.837f, -0.714f, 0.781f, 0.239f, -0.953f, 0.26f, 0.696f, -0.183f, 0.668f };
   int ldb = 2;
   float B_expected[] = { -0.431623f, 0.111093f, 0.2808f, 0.1854f, 0.007293f, -0.454491f, 0.0236f, 0.3098f, -0.059093f, -0.075968f, -0.0119f, -0.2187f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1749) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1749) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.055f, -0.682f, 0.361f, 0.521f, -0.192f, -0.664f, -0.167f, 0.731f, -0.668f, 0.983f, 0.608f, 0.533f, -0.513f, -0.781f, 0.878f, 0.875f, 0.804f, -0.179f };
   int lda = 3;
   float B[] = { -0.038f, -0.787f, -0.209f, -0.686f, -0.073f, -0.662f, 0.938f, -0.301f, -0.871f, 0.699f, 0.561f, 0.823f };
   int ldb = 3;
   float B_expected[] = { 0.224558f, -0.0087435f, -0.317863f, 0.168822f, 0.105075f, 0.138035f, 0.256887f, 0.377119f, 0.113231f, 0.136832f, -0.235636f, -0.108546f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1750) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1750) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.397f, -0.154f, -0.944f, -0.137f, 0.65f, -0.49f, -0.883f, 0.273f, -0.137f, 0.655f, 0.531f, 0.676f, 0.052f, 0.03f, -0.602f, 0.002f, 0.005f, 0.984f };
   int lda = 3;
   float B[] = { -0.446f, 0.091f, 0.793f, -0.221f, 0.386f, 0.354f, -0.063f, 0.105f, -0.128f, 0.189f, -0.079f, 0.749f };
   int ldb = 3;
   float B_expected[] = { 0.216958f, -0.149634f, -0.25039f, 0.0074932f, -0.1512f, -0.0676f, -0.166784f, -0.100965f, 0.14955f, -0.227622f, -0.0512f, -0.2326f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1751) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1751) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.976f, -0.488f, -0.762f, -0.057f, 0.812f, 0.006f, 0.06f, -0.271f, 0.832f, -0.232f, 0.188f, -0.466f, -0.051f, -0.745f, 0.909f, -0.091f, -0.559f, 0.595f };
   int lda = 3;
   float B[] = { 0.644f, -0.584f, 0.456f, 0.443f, -0.909f, 0.43f, 0.771f, -0.075f, -0.408f, 0.303f, 0.03f, 0.529f };
   int ldb = 3;
   float B_expected[] = { 0.24849f, -0.168067f, -0.114085f, 0.0202884f, 0.0152508f, 0.284926f, 0.267034f, 0.0120048f, 0.0596364f, -0.0643158f, 0.284594f, 0.0837608f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1752) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1752) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.924f, -0.247f, -0.131f, 0.932f, -0.415f, 0.454f, -0.539f, 0.693f, -0.725f, -0.601f, 0.565f, 0.002f, -0.118f, 0.626f, -0.968f, 0.874f, 0.156f, -0.227f };
   int lda = 3;
   float B[] = { 0.793f, -0.15f, -0.967f, 0.821f, 0.37f, -0.572f, -0.156f, 0.106f, -0.877f, -0.297f, 0.448f, -0.576f };
   int ldb = 3;
   float B_expected[] = { -0.2229f, 0.1243f, 0.242003f, -0.564467f, -0.0068716f, 0.568213f, 0.0362f, -0.0474f, 0.306136f, 0.0520352f, -0.336053f, 0.500406f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1753) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1753) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.671f, 0.477f, 0.227f, 0.685f, -0.648f, 0.277f, -0.295f, -0.632f, 0.509f, -0.798f, 0.875f, 0.89f, -0.34f, -0.786f, -0.453f, 0.511f, -0.189f, 0.385f };
   int lda = 3;
   float B[] = { -0.895f, -0.148f, 0.934f, 0.229f, 0.958f, -0.55f, 0.49f, 0.586f, -0.871f, 0.618f, -0.0f, -0.543f };
   int ldb = 2;
   float B_expected[] = { 0.162976f, 0.110656f, -0.12507f, -0.0587256f, 0.138701f, 0.543589f, -0.313677f, 0.0534812f, 0.067207f, 0.12831f, -0.0729792f, -0.0098826f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1754) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1754) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.438f, -0.618f, 0.524f, 0.525f, -0.268f, -0.502f, -0.685f, 0.28f, 0.508f, 0.664f, -0.492f, 0.772f, -0.997f, 0.693f, 0.63f, -0.328f, -0.521f, -0.869f };
   int lda = 3;
   float B[] = { 0.527f, 0.999f, -0.078f, 0.599f, 0.004f, -0.615f, -0.281f, -0.328f, 0.456f, -0.666f, 0.309f, -0.69f };
   int ldb = 2;
   float B_expected[] = { -0.45115f, -0.650085f, -0.277633f, -0.456478f, 0.0965652f, 0.362528f, 0.1802f, 0.227951f, -0.0702f, 0.2454f, -0.0237f, 0.2379f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1755) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1755) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.454f, 0.517f, -0.794f, -0.181f, 0.292f, 0.954f, -0.93f, -0.128f, 0.123f, -0.997f, 0.325f, -0.317f, -0.988f, 0.732f, 0.637f, 0.457f, -0.665f, 0.529f };
   int lda = 3;
   float B[] = { -0.055f, 0.803f, -0.981f, -0.627f, 0.147f, -0.656f, -0.824f, -0.366f, -0.445f, -0.151f, 0.686f, -0.368f };
   int ldb = 2;
   float B_expected[] = { 0.156354f, 0.078881f, -0.208608f, 0.143709f, 0.219569f, 0.211768f, -0.204943f, -0.415655f, 0.191227f, 0.0071854f, 0.136999f, 0.0773624f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1756) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1756) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.623f, -0.129f, -0.419f, -0.006f, 0.21f, -0.165f, 0.218f, 0.915f, 0.736f, 0.07f, 0.502f, -0.809f, 0.242f, -0.015f, 0.67f, -0.956f, 0.153f, 0.365f };
   int lda = 3;
   float B[] = { -0.927f, 0.383f, -0.471f, 0.443f, -0.731f, -0.949f, -0.142f, -0.65f, 0.159f, -0.624f, -0.822f, 0.107f };
   int ldb = 2;
   float B_expected[] = { 0.2398f, -0.2076f, 0.097f, -0.18f, 0.212478f, 0.297146f, 0.065877f, 0.255638f, 0.359717f, -0.0280276f, 0.426852f, -0.164392f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1757) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1757) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.628f, -0.771f, 0.827f, -0.979f, 0.395f, -0.166f, 0.88f, 0.958f };
   int lda = 2;
   float B[] = { 0.297f, 0.49f, 0.425f, -0.386f, 0.672f, 0.992f, -0.077f, 0.761f, 0.393f, -0.605f, -0.273f, 0.725f };
   int ldb = 3;
   float B_expected[] = { 0.177165f, -0.0328107f, -0.0662201f, -0.167954f, 0.366541f, -0.0872256f, -0.2721f, -0.389113f, -0.0674816f, 0.293174f, -0.249446f, -0.709453f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1758) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1758) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.876f, 0.752f, -0.148f, 0.972f, -0.508f, -0.752f, -0.861f, 0.074f };
   int lda = 2;
   float B[] = { 0.878f, -0.987f, -0.896f, 0.519f, -0.355f, -0.117f, 0.329f, 0.068f, -0.644f, 0.344f, -0.187f, -0.343f };
   int ldb = 3;
   float B_expected[] = { -0.1647f, 0.3839f, 0.2169f, -0.2453f, 0.1182f, -0.0004f, 0.292026f, 0.115771f, -0.111733f, -0.342122f, 0.0725176f, -0.0306312f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1759) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1759) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.072f, -0.966f, 0.648f, 0.43f, -0.623f, -0.221f, -0.622f, 0.977f };
   int lda = 2;
   float B[] = { 0.0f, 0.028f, 0.857f, -0.171f, -0.933f, 0.159f, 0.315f, -0.297f, -0.864f, 0.519f, -0.601f, -0.119f };
   int ldb = 3;
   float B_expected[] = { 0.0216306f, -0.0927642f, -0.225266f, -0.0253344f, 0.0408658f, 0.302549f, 0.158132f, -0.0117036f, -0.365472f, -0.0519459f, -0.143387f, -0.172603f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1760) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1760) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.903f, -0.972f, -0.812f, 0.605f, 0.085f, -0.025f, -0.443f, 0.518f };
   int lda = 2;
   float B[] = { -0.725f, -0.451f, 0.779f, 0.969f, 0.25f, 0.021f, 0.029f, -0.382f, 0.022f, 0.957f, 0.704f, 0.832f };
   int ldb = 3;
   float B_expected[] = { 0.26217f, 0.073525f, -0.332173f, -0.239574f, -0.097644f, -0.003892f, 0.0295f, 0.1175f, -0.1023f, -0.2849f, -0.2944f, -0.1792f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1761) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1761) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.322f, -0.981f, 0.193f, -0.604f, 0.87f, -0.384f, 0.463f, -0.502f };
   int lda = 2;
   float B[] = { -0.447f, 0.21f, 0.928f, -0.496f, 0.889f, -0.354f, -0.258f, -0.149f, 0.98f, -0.958f, 0.106f, -0.579f };
   int ldb = 2;
   float B_expected[] = { 0.0692355f, 0.14563f, -0.0874638f, -0.0532654f, -0.116915f, -0.289728f, -0.242902f, 0.136003f, -0.314257f, -0.318533f, -0.400862f, 0.357622f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1762) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1762) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.924f, -0.553f, 0.985f, -0.793f, 0.406f, 0.741f, -0.956f, 0.945f };
   int lda = 2;
   float B[] = { 0.736f, -0.81f, 0.028f, 0.474f, 0.14f, -0.03f, -0.756f, 0.923f, -0.515f, 0.532f, -0.321f, 0.326f };
   int ldb = 2;
   float B_expected[] = { -0.1398f, 0.3166f, 0.122042f, 0.0927314f, -0.039f, 0.023f, 0.135709f, -0.314263f, 0.1013f, -0.2111f, -0.0515973f, -0.29067f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1763) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1763) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.04f, -0.41f, -0.643f, 0.988f, 0.86f, -0.281f, -0.017f, 0.389f };
   int lda = 2;
   float B[] = { 0.204f, 0.524f, -0.558f, -0.736f, 0.26f, -0.202f, -0.757f, 0.346f, 0.917f, 0.541f, -0.108f, -0.965f };
   int ldb = 2;
   float B_expected[] = { 0.059601f, -0.396251f, 0.060088f, -0.096554f, -0.338942f, -0.0950055f, -0.073098f, -0.071831f, 0.208251f, -0.444353f, 0.106223f, -0.05488f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1764) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1764) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.375f, 0.153f, -0.343f, -0.742f, 0.563f, 0.473f, 0.451f, -0.433f };
   int lda = 2;
   float B[] = { -0.804f, -0.016f, -0.715f, -0.902f, -0.89f, 0.155f, -0.408f, 0.419f, 0.078f, -0.691f, -0.717f, -0.637f };
   int ldb = 2;
   float B_expected[] = { -0.0094443f, 0.0821961f, 0.3047f, 0.1991f, 0.347432f, -0.0186595f, 0.0805f, -0.1665f, -0.138523f, 0.381015f, 0.2788f, 0.1194f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1765) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1765) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.867f, -0.597f, -0.577f, 0.735f, 0.827f, -0.104f, -0.861f, -0.802f, -0.288f, 0.293f, 0.593f, 0.228f, -0.469f, 0.942f, 0.193f, 0.591f, 0.241f, 0.382f };
   int lda = 3;
   float B[] = { -0.812f, -0.874f, -0.18f, -0.81f, 0.023f, 0.352f, 0.559f, 0.237f, -0.835f, 0.037f, -0.762f, 0.782f };
   int ldb = 3;
   float B_expected[] = { -0.331628f, -0.278177f, -0.0214727f, -0.156013f, -0.0496067f, -0.0088131f, 0.119788f, -0.469291f, -0.0804714f, -0.263663f, -0.0824792f, -0.132356f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1766) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1766) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.258f, -0.812f, -0.858f, -0.107f, -0.151f, 0.785f, 0.717f, 0.992f, -0.649f, -0.242f, -0.454f, 0.916f, 0.86f, 0.834f, -0.244f, 0.391f, 0.818f, -0.714f };
   int lda = 3;
   float B[] = { 0.163f, 0.441f, 0.54f, 0.679f, 0.071f, -0.76f, 0.345f, -0.956f, 0.654f, -0.217f, -0.892f, 0.106f };
   int ldb = 3;
   float B_expected[] = { 0.296566f, -0.0905963f, -0.0393822f, -0.306541f, 0.0547f, 0.2351f, -0.0059345f, 0.0071855f, -0.402014f, -0.049978f, 0.257f, -0.121f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1767) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1767) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.525f, 0.182f, 0.851f, -0.348f, -0.046f, 0.839f, -0.045f, -0.149f, -0.992f, 0.588f, -0.01f, -0.409f, 0.527f, 0.263f, -0.509f, -0.026f, 0.284f, 0.507f };
   int lda = 3;
   float B[] = { 0.909f, 0.216f, 0.38f, 0.198f, -0.412f, -0.102f, -0.456f, 0.079f, 0.504f, -0.782f, -0.88f, 0.079f };
   int ldb = 3;
   float B_expected[] = { -0.149757f, 0.0672651f, 0.129501f, 0.054878f, -0.0469462f, 0.0277224f, 0.0550599f, -0.0598423f, 0.244521f, -0.217471f, 0.0955519f, -0.37895f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1768) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1768) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.893f, -0.758f, 0.145f, 0.623f, -0.018f, -0.733f, -0.144f, -0.192f, 0.53f, 0.773f, -0.771f, 0.36f, 0.932f, -0.771f, 0.997f, -0.671f, 0.574f, -0.771f };
   int lda = 3;
   float B[] = { 0.592f, 0.985f, -0.62f, -0.095f, -0.344f, -0.607f, 0.759f, 0.085f, -0.609f, 0.068f, -0.084f, -0.575f };
   int ldb = 3;
   float B_expected[] = { -0.2761f, -0.2363f, 0.280628f, -0.052484f, 0.306154f, -0.187624f, -0.2362f, 0.0504f, 0.200236f, -0.133908f, 0.0536278f, 0.0659354f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1769) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1769) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.503f, -0.057f, -0.581f, -0.77f, -0.907f, -0.843f, 0.56f, -0.554f, 0.054f, 0.988f, 0.868f, -0.627f, 0.645f, -0.246f, -0.958f, 0.66f, 0.956f, 0.99f };
   int lda = 3;
   float B[] = { 0.282f, -0.442f, 0.564f, -0.691f, -0.743f, 0.113f, -0.395f, 0.312f, -0.167f, -0.568f, 0.508f, 0.912f };
   int ldb = 2;
   float B_expected[] = { 0.180092f, 0.260648f, -0.045069f, -0.102868f, -0.0964434f, -0.432702f, -0.0404678f, 0.280779f, 0.254359f, 0.0411062f, -0.453454f, 0.0281672f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1770) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1770) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { -0.851f, 0.296f, -0.683f, -0.53f, 0.38f, -0.837f, 0.977f, 0.189f, -0.624f, -0.664f, 0.73f, -0.882f, 0.105f, -0.868f, 0.362f, -0.006f, -0.435f, 0.757f };
   int lda = 3;
   float B[] = { -0.259f, -0.091f, 0.606f, -0.983f, -0.238f, 0.057f, 0.358f, 0.18f, -0.71f, 0.058f, 0.511f, 0.717f };
   int ldb = 2;
   float B_expected[] = { 0.241746f, 0.119591f, -0.0907286f, 0.148899f, 0.141237f, -0.0716576f, -0.205866f, -0.078918f, 0.2072f, -0.0884f, -0.225f, -0.164f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1771) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1771) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.956f, 0.972f, 0.771f, 0.187f, 0.948f, 0.303f, -0.854f, 0.123f, 0.704f, 0.152f, 0.347f, 0.595f, -0.865f, 0.75f, -0.041f, -0.572f, 0.749f, 0.216f };
   int lda = 3;
   float B[] = { -0.821f, -0.098f, 0.347f, -0.639f, 0.314f, -0.009f, -0.725f, 0.45f, 0.536f, 0.801f, 0.431f, 0.936f };
   int ldb = 2;
   float B_expected[] = { 0.193607f, -0.29931f, 0.18163f, 0.255513f, 0.127098f, -0.0503344f, 0.101243f, 0.0097718f, -0.0060322f, -0.148016f, -0.251411f, -0.0777231f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1772) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1772) imag");
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
   float alpha[2] = {-0.3f, 0.1f};
   float A[] = { 0.78f, -0.205f, 0.073f, -0.859f, 0.568f, -0.599f, -0.947f, -0.514f, 0.835f, 0.176f, 0.27f, -0.617f, 0.171f, -0.074f, 0.939f, -0.469f, -0.471f, 0.25f };
   int lda = 3;
   float B[] = { -0.279f, 0.16f, -0.495f, 0.658f, 0.071f, 0.557f, -0.116f, 0.095f, -0.104f, 0.503f, -0.775f, -0.03f };
   int ldb = 2;
   float B_expected[] = { 0.0677f, -0.0759f, 0.0827f, -0.2469f, -0.0068598f, -0.107386f, 0.243424f, 0.0129156f, 0.142748f, -0.254568f, 0.461939f, -0.154419f };
   cblas_ctrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], flteps, "ctrmm(case 1773) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], flteps, "ctrmm(case 1773) imag");
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
   double A[] = { 0.463, 0.033, -0.929, 0.949, 0.864, 0.986, 0.393, 0.885 };
   int lda = 2;
   double B[] = { -0.321, -0.852, -0.337, -0.175, 0.607, -0.613, 0.688, 0.973, -0.331, -0.35, 0.719, -0.553 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1774) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1774) imag");
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
   double A[] = { 0.608, -0.393, 0.921, 0.282, -0.857, -0.286, -0.31, -0.057 };
   int lda = 2;
   double B[] = { -0.548, 0.728, 0.391, -0.506, 0.186, 0.97, -0.435, 0.375, -0.995, -0.496, 0.99, 0.186 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1775) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1775) imag");
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
   double A[] = { 0.253, 0.969, 0.654, -0.016, -0.774, -0.11, -0.101, -0.287 };
   int lda = 2;
   double B[] = { -0.34, -0.268, -0.52, 0.021, -0.875, 0.98, 0.255, 0.564, -0.478, -0.818, -0.043, 0.224 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1776) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1776) imag");
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
   double A[] = { -0.64, -0.222, 0.922, 0.417, -0.724, 0.012, 0.418, 0.39 };
   int lda = 2;
   double B[] = { 0.619, -0.024, -0.068, 0.219, 0.374, -0.937, 0.79, 0.166, -0.92, 0.753, -0.017, 0.076 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1777) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1777) imag");
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
   double A[] = { 0.57, 0.987, 0.116, -0.691, -0.603, -0.778, 0.14, -0.073 };
   int lda = 2;
   double B[] = { 0.421, -0.055, 0.92, 0.664, 0.835, 0.861, -0.392, -0.897, -0.346, 0.516, -0.068, -0.156 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1778) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1778) imag");
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
   double A[] = { -0.754, 0.904, 0.089, 0.206, 0.974, -0.946, -0.55, -0.675 };
   int lda = 2;
   double B[] = { -0.42, -0.372, 0.628, 0.148, 0.344, -0.924, -0.802, -0.307, 0.427, 0.116, 0.916, -0.384 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1779) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1779) imag");
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
   double A[] = { 0.152, -0.898, -0.024, 0.719, 0.992, -0.841, 0.901, 0.202 };
   int lda = 2;
   double B[] = { 0.243, -0.811, 0.68, 0.118, 0.946, -0.632, 0.729, -0.942, 0.308, 0.507, -0.838, 0.594 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1780) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1780) imag");
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
   double A[] = { 0.119, -0.849, 0.425, -0.273, -0.918, 0.196, -0.871, -0.39 };
   int lda = 2;
   double B[] = { 0.709, 0.33, -0.207, 0.012, -0.017, 0.787, -0.385, 0.739, -0.874, 0.188, -0.039, 0.692 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1781) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1781) imag");
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
   double A[] = { 0.837, -0.603, 0.755, -0.92, 0.892, -0.009, -0.741, 0.271, -0.325, -0.861, 0.902, -0.088, 0.091, 0.256, 0.209, -0.724, 0.28, -0.604 };
   int lda = 3;
   double B[] = { 0.455, -0.215, -0.668, 0.917, -0.985, 0.477, 0.564, -0.524, -0.202, -0.53, -0.88, -0.688 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1782) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1782) imag");
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
   double A[] = { -0.991, 0.253, 0.813, 0.497, -0.268, 0.623, 0.82, -0.946, -0.883, 0.333, -0.265, -0.371, 0.131, -0.812, -0.365, 0.45, 0.929, -0.704 };
   int lda = 3;
   double B[] = { 0.783, -0.756, 0.635, 0.56, 0.434, -0.831, -0.34, -0.531, -0.277, 0.874, 0.986, 0.157 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1783) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1783) imag");
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
   double A[] = { 0.265, -0.592, -0.721, -0.838, -0.952, 0.115, -0.34, -0.789, -0.265, -0.779, -0.676, 0.048, 0.78, -0.272, -0.651, 0.272, 0.8, -0.693 };
   int lda = 3;
   double B[] = { -0.609, 0.028, -0.818, 0.289, -0.41, -0.25, -0.917, 0.463, 0.942, 0.692, -0.516, 0.378 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1784) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1784) imag");
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
   double A[] = { 0.962, 0.945, -0.58, -0.358, -0.769, 0.751, -0.068, -0.321, 0.938, 0.183, -0.17, 0.251, -0.248, -0.092, -0.818, 0.928, -0.059, -0.222 };
   int lda = 3;
   double B[] = { 0.015, -0.852, -0.565, 0.16, -0.095, 0.073, 0.405, 0.509, 0.082, -0.478, -0.365, 0.824 };
   int ldb = 3;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1785) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1785) imag");
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
   double A[] = { 0.616, 0.669, 0.323, -0.238, 0.153, 0.891, -0.4, 0.996, 0.689, -0.736, -0.259, -0.707, 0.993, 0.13, -0.829, -0.564, -0.09, 0.118 };
   int lda = 3;
   double B[] = { 0.113, 0.724, 0.148, -0.309, -0.833, -0.791, 0.354, -0.528, 0.313, 0.421, 0.28, 0.371 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1786) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1786) imag");
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
   double A[] = { 0.957, -0.713, 0.976, 0.183, -0.145, -0.858, -0.497, -0.605, -0.742, 0.686, 0.272, 0.83, -0.606, -0.099, -0.807, 0.767, 0.254, 0.244 };
   int lda = 3;
   double B[] = { -0.124, -0.19, 0.665, -0.74, 0.505, -0.194, 0.588, -0.421, -0.727, 0.308, -0.802, -0.278 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1787) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1787) imag");
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
   double A[] = { -0.649, 0.856, 0.969, 0.382, 0.963, 0.567, 0.599, 0.018, -0.924, 0.578, -0.531, -0.091, -0.454, -0.834, 0.97, -0.126, -0.859, 0.879 };
   int lda = 3;
   double B[] = { 0.35, 0.824, -0.084, 0.662, -0.752, 0.872, 0.129, 0.969, -0.539, 0.907, 0.316, -0.675 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1788) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1788) imag");
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
   double A[] = { -0.315, -0.459, 0.327, -0.132, -0.283, 0.173, -0.356, -0.427, 0.508, 0.347, -0.804, -0.849, 0.779, 0.673, 0.019, -0.869, 0.999, -0.338 };
   int lda = 3;
   double B[] = { 0.678, -0.171, 0.136, -0.268, -0.578, -0.431, 0.978, -0.749, 0.333, -0.757, 0.658, 0.456 };
   int ldb = 2;
   double B_expected[] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1789) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1789) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { -0.532, -0.877, 0.308, -0.807, 0.013, 0.891, 0.077, -0.004 };
   int lda = 2;
   double B[] = { 0.634, -0.969, 0.228, -0.097, 0.419, 0.903, 0.21, 0.313, -0.819, -0.028, 0.574, -0.762 };
   int ldb = 3;
   double B_expected[] = { 0.004051, -0.1187101, 0.0148352, -0.0206365, 0.0847859, 0.0569023, 0.0786829, -0.0569289, 0.0212752, -0.007123, 0.0120979, 0.0898923 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1790) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1790) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { -0.77, -0.037, -0.021, -0.831, -0.663, -0.241, -0.273, -0.023 };
   int lda = 2;
   double B[] = { 0.354, -0.95, -0.944, -0.497, 0.741, 0.084, -0.3, 0.023, -0.056, 0.063, -0.117, -0.498 };
   int ldb = 3;
   double B_expected[] = { 0.095, 0.0354, 0.0497, -0.0944, -0.0084, 0.0741, 0.0251224, -0.1096884, -0.0857901, -0.0449183, 0.1115535, -0.0062757 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1791) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1791) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { -0.623, 0.379, 0.903, -0.378, -0.088, 0.24, -0.964, 0.558 };
   int lda = 2;
   double B[] = { -0.137, 0.706, 0.457, 0.399, -0.69, -0.7, 0.34, 0.479, 0.539, -0.133, 0.876, -0.347 };
   int ldb = 3;
   double B_expected[] = { 0.0452313, -0.0327103, -0.006569, -0.0451444, -0.0415366, 0.0701362, 0.0272036, -0.0595042, -0.0428974, -0.0445382, -0.0823316, -0.0650838 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1792) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1792) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { 0.253, 0.657, 0.636, 0.827, -0.107, 0.353, 0.425, -0.365 };
   int lda = 2;
   double B[] = { -0.402, -0.409, 0.421, -0.333, -0.771, -0.099, 0.697, -0.812, -0.653, 0.823, 0.994, 0.998 };
   int ldb = 3;
   double B_expected[] = { 0.0076075, -0.0189943, 0.065157, 0.0200352, -0.0145096, -0.1229652, 0.0812, 0.0697, -0.0823, -0.0653, -0.0998, 0.0994 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1793) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1793) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { -0.57, -0.805, -0.66, -0.421, 0.643, -0.534, -0.988, -0.581 };
   int lda = 2;
   double B[] = { -0.279, -0.253, 0.976, -0.051, 0.294, 0.451, 0.187, -0.177, 0.31, -0.714, -0.104, -0.177 };
   int ldb = 2;
   double B_expected[] = { -0.0368805, -0.0044635, 0.0530361, -0.1308418, 0.049374, 0.0195475, -0.0199226, 0.0142283, -0.015743, -0.075147, 0.0389342, -0.0182031 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1794) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1794) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { -0.594, 0.273, 0.457, 0.295, 0.434, -0.227, -0.662, 0.623 };
   int lda = 2;
   double B[] = { -0.582, -0.581, 0.259, -0.833, -0.864, -0.284, 0.965, -0.459, -0.539, -0.551, -0.969, 0.09 };
   int ldb = 2;
   double B_expected[] = { 0.0581, -0.0582, 0.095304, -0.0125475, 0.0284, -0.0864, 0.0386128, 0.0525556, 0.0551, -0.0539, 0.0026781, -0.1328003 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1795) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1795) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { 0.398, 0.323, 0.31, 0.718, 0.181, 0.665, 0.402, 0.317 };
   int lda = 2;
   double B[] = { 0.812, -0.244, -0.415, 0.602, 0.901, -0.017, 0.786, -0.119, 0.448, -0.75, 0.851, 0.172 };
   int ldb = 2;
   double B_expected[] = { -0.0053814, -0.0158898, -0.0110449, -0.0357664, -0.0811715, 0.0693191, -0.0201324, 0.0353695, -0.0510542, 0.0560868, -0.0338911, 0.0287578 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1796) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1796) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { 0.265, -0.578, 0.218, -0.093, -0.172, 0.414, 0.448, 0.696 };
   int lda = 2;
   double B[] = { 0.02, -0.254, 0.152, 0.304, 0.289, 0.247, 0.705, 0.419, -0.735, 0.788, -0.942, -0.71 };
   int ldb = 2;
   double B_expected[] = { 0.0201864, 0.0081408, -0.0304, 0.0152, -0.0272777, 0.0481657, -0.0419, 0.0705, -0.0720826, -0.1006386, 0.071, -0.0942 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1797) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1797) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { 0.971, 0.532, 0.175, 0.455, 0.191, 0.493, 0.882, -0.944, 0.358, 0.142, -0.065, 0.632, -0.319, -0.101, 0.578, 0.476, -0.773, 0.912 };
   int lda = 3;
   double B[] = { 0.018, -0.131, 0.964, -0.467, -0.729, -0.794, 0.874, 0.361, 0.744, -0.958, 0.162, 0.555 };
   int ldb = 3;
   double B_expected[] = { 0.0271781, 0.0720558, 0.0439416, 0.0960619, 0.0051086, 0.1287645, -0.117224, 0.0980019, 0.0171007, 0.0041098, 0.0281271, -0.0631386 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1798) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1798) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { -0.506, -0.263, -0.994, 0.681, 0.889, -0.5, -0.912, 0.741, -0.329, -0.912, 0.332, -0.001, -0.484, 0.942, -0.728, -0.104, -0.216, 0.679 };
   int lda = 3;
   double B[] = { 0.562, -0.354, 0.742, -0.177, -0.627, -0.762, 0.476, 0.758, 0.675, -0.504, -0.33, 0.186 };
   int ldb = 3;
   double B_expected[] = { 0.0036678, -0.0993414, 0.0429357, 0.0533074, 0.0762, -0.0627, -0.2049005, -0.0052096, 0.0441918, 0.0565626, -0.0186, -0.033 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1799) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1799) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { -0.341, -0.27, 0.001, 0.939, 0.714, 0.803, -0.508, -0.331, -0.563, -0.725, -0.902, -0.793, 0.461, 0.127, -0.597, -0.498, 0.394, -0.019 };
   int lda = 3;
   double B[] = { 0.015, 0.803, 0.497, 0.667, 0.803, 0.775, 0.026, 0.908, 0.535, -0.111, 0.379, -0.036 };
   int ldb = 3;
   double B_expected[] = { 0.0277873, 0.0211695, 0.1148735, 0.0461937, -0.0016476, 0.0271498, 0.0316648, 0.0236294, 0.0795252, -0.009434, -0.0200342, -0.0329361 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1800) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1800) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { 0.132, 0.903, -0.235, -0.294, -0.09, 0.74, -0.707, -0.855, 0.632, 0.543, -0.558, -0.416, -0.99, -0.088, -0.189, -0.371, -0.844, -0.737 };
   int lda = 3;
   double B[] = { -0.257, 0.159, 0.689, 0.785, 0.398, -0.128, -0.098, -0.735, -0.307, 0.032, 0.517, 0.049 };
   int ldb = 3;
   double B_expected[] = { -0.0159, -0.0257, -0.0892322, 0.1006644, 0.0666778, 0.0827436, 0.0735, -0.0098, -0.0635435, -0.0866139, -0.0893123, 0.0619235 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1801) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1801) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { 0.993, 0.709, 0.225, -0.704, -0.396, 0.656, -0.588, -0.085, -0.024, 0.264, -0.988, -0.67, 0.665, -0.165, -0.778, -0.43, 0.71, -0.35 };
   int lda = 3;
   double B[] = { 0.321, 0.614, 0.058, 0.983, 0.153, -0.647, 0.342, -0.518, -0.071, -0.533, -0.424, 0.283 };
   int ldb = 2;
   double B_expected[] = { -0.0861992, -0.0396692, -0.155091, -0.1119744, -0.0501124, -0.0006816, -0.0064866, 0.0580106, 0.035358, -0.023696, -0.034933, -0.020199 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1802) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1802) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { 0.034, -0.02, -0.401, -0.892, 0.329, -0.799, -0.018, 0.564, 0.095, 0.965, -0.105, 0.756, -0.583, -0.706, -0.436, -0.145, 0.921, 0.416 };
   int lda = 3;
   double B[] = { 0.972, 0.157, -0.029, 0.674, 0.914, 0.434, 0.132, -0.116, -0.907, 0.316, -0.423, 0.321 };
   int ldb = 2;
   double B_expected[] = { -0.1120798, 0.1462649, -0.0862031, 0.0507283, -0.0427739, 0.1355272, 0.0194621, 0.0362973, -0.0316, -0.0907, -0.0321, -0.0423 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1803) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1803) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { -0.195, -0.36, 0.834, -0.505, -0.87, -0.787, 0.997, 0.965, -0.046, -0.591, 0.082, 0.552, 0.414, -0.013, -0.048, -0.766, 0.728, 0.088 };
   int lda = 3;
   double B[] = { -0.916, -0.162, -0.863, 0.67, -0.079, -0.27, -0.191, 0.995, 0.981, -0.25, -0.149, 0.248 };
   int ldb = 2;
   double B_expected[] = { -0.036135, 0.01203, -0.018003, 0.0409485, -0.0386581, -0.100169, -0.1061706, 0.0215439, -0.0700412, 0.1548156, -0.0239871, 0.0582902 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1804) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1804) imag");
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
   double alpha[2] = {0, 0.1};
   double A[] = { 0.553, -0.63, -0.079, 0.351, 0.865, -0.062, 0.165, -0.634, -0.513, 0.216, -0.521, 0.349, 0.54, 0.545, -0.719, -0.306, 0.501, 0.757 };
   int lda = 3;
   double B[] = { -0.311, 0.088, -0.328, 0.977, 0.659, -0.06, -0.276, 0.872, -0.734, -0.01, -0.668, -0.327 };
   int ldb = 2;
   double B_expected[] = { -0.0088, -0.0311, -0.0977, -0.0328, 0.0176113, 0.0652681, -0.0679689, -0.0593015, -0.0346653, -0.1319958, 0.0012195, -0.1051678 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1805) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1805) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { 0.993, -0.018, 0.162, -0.222, 0.188, 0.672, -0.675, -0.345 };
   int lda = 2;
   double B[] = { 0.476, -0.009, 0.725, -0.925, -0.245, 0.308, 0.515, 0.1, -0.072, -0.757, 0.212, 0.571 };
   int ldb = 3;
   double B_expected[] = { 0.000369, 0.47283, 0.905475, 0.736575, -0.301434, -0.248829, -0.214389, -0.303015, -0.497235, 0.632565, 0.316779, -0.448161 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1806) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1806) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { -0.78, 0.346, -0.663, -0.86, -0.496, -0.154, 0.356, 0.228 };
   int lda = 2;
   double B[] = { 0.578, 0.492, 0.775, 0.353, 0.198, -0.519, -0.52, -0.677, -0.438, 0.313, 0.941, -0.56 };
   int ldb = 3;
   double B_expected[] = { -0.492, 0.578, -0.353, 0.775, 0.519, 0.198, 0.506116, -1.326334, -0.745461, -1.255405, 0.045623, 1.256066 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1807) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1807) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { 0.455, 0.442, 0.062, 0.815, 0.03, 0.55, 0.592, -0.487 };
   int lda = 2;
   double B[] = { -0.451, 0.01, 0.174, -0.775, 0.22, -0.644, 0.858, -0.004, 0.59, -0.395, -0.943, 0.824 };
   int ldb = 3;
   double B_expected[] = { 0.268128, -0.177245, 0.765883, -0.46293, -0.15311, 0.240362, -0.415478, 0.509884, -0.05349, 0.541645, -0.028567, -0.959544 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1808) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1808) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { -0.268, -0.886, -0.805, 0.875, 0.481, 0.095, -0.057, 0.605 };
   int lda = 2;
   double B[] = { 0.708, -0.638, 0.408, -0.512, 0.175, 0.181, -0.919, -0.126, 0.708, -0.51, 0.212, 0.114 };
   int ldb = 3;
   double B_expected[] = { 0.611301, 0.253991, 0.82457, 0.700098, -0.215694, 0.287802, 0.126, -0.919, 0.51, 0.708, -0.114, 0.212 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1809) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1809) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { 0.881, 0.555, 0.774, 0.148, -0.915, 0.336, 0.103, 0.381 };
   int lda = 2;
   double B[] = { 0.163, 0.963, -0.017, 0.921, 0.809, 0.846, 0.905, -0.43, 0.894, -0.371, -0.988, -0.487 };
   int ldb = 2;
   double B_expected[] = { -0.757938, 0.678068, 0.834573, 0.523573, -0.296331, 1.182259, 1.435009, -0.526594, 0.823021, 0.581709, -0.365348, -1.229977 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1810) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1810) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { 0.719, -0.513, 0.169, -0.524, 0.352, 0.823, -0.741, -0.355 };
   int lda = 2;
   double B[] = { 0.717, 0.052, -0.777, 0.277, -0.962, 0.894, 0.905, -0.216, -0.707, 0.016, 0.481, 0.935 };
   int ldb = 2;
   double B_expected[] = { -0.052, 0.717, 0.294787, -0.48182, -0.894, -0.962, -0.890414, 1.302138, -0.016, -0.707, -1.522493, 0.245304 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1811) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1811) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { 0.501, -0.136, -0.502, 0.669, -0.498, -0.4, -0.518, 0.833 };
   int lda = 2;
   double B[] = { -0.385, 0.88, 0.726, 0.911, 0.839, 0.573, -0.881, -0.517, -0.861, -0.278, 0.941, 0.822 };
   int ldb = 2;
   double B_expected[] = { 0.554496, -0.067558, 1.076656, 0.382795, -1.2501, 0.4388, -1.001679, 0.025697, 1.298547, -0.316017, 1.209649, 0.197288 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1812) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1812) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { 0.049, 0.641, -0.9, 0.246, -0.734, -0.686, 0.76, -0.869 };
   int lda = 2;
   double B[] = { -0.37, 0.206, -0.731, -0.573, 0.638, -0.417, -0.29, -0.719, 0.107, -0.333, 0.556, 0.124 };
   int ldb = 2;
   double B_expected[] = { -0.901526, 0.146942, 0.573, -0.731, -0.30144, 0.722126, 0.719, -0.29, 0.581376, -0.362896, -0.124, 0.556 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1813) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1813) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { 0.553, 0.338, 0.229, -0.828, -0.594, -0.036, -0.335, -0.249, 0.083, -0.197, 0.995, 0.85, -0.988, 0.596, -0.254, 0.179, 0.441, -0.859 };
   int lda = 3;
   double B[] = { -0.058, -0.225, 0.884, 0.348, 0.123, -0.151, 0.891, 0.711, -0.792, 0.552, 0.033, -0.178 };
   int ldb = 3;
   double B_expected[] = { -0.800945, -0.261458, 0.051763, -0.001149, -0.039066, 0.183952, 0.330423, 0.081423, 0.315368, -0.292945, 0.050151, 0.167455 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1814) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1814) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { -0.257, -0.565, -0.123, 0.129, 0.817, -0.516, -0.613, -0.42, -0.494, 0.122, -0.593, -0.972, -0.695, -0.968, 0.848, -0.2, -0.17, 0.436 };
   int lda = 3;
   double B[] = { -0.274, 0.105, -0.899, -0.33, -0.318, -0.096, -0.237, 0.327, 0.046, 0.584, -0.459, -0.182 };
   int ldb = 3;
   double B_expected[] = { -0.019041, -0.416263, 0.582168, -0.617114, 0.096, -0.318, 0.136304, -0.448413, -0.245778, 0.495091, 0.182, -0.459 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1815) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1815) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { -0.127, 0.025, 0.036, 0.612, 0.773, 0.953, 0.074, -0.006, 0.373, 0.292, -0.052, -0.319, -0.878, -0.401, 0.486, -0.493, -0.316, 0.003 };
   int lda = 3;
   double B[] = { 0.794, -0.666, -0.406, 0.622, -0.512, -0.761, 0.161, -0.137, -0.626, 0.408, 0.536, 0.66 };
   int ldb = 3;
   double B_expected[] = { -0.064732, -0.117488, -0.306038, 0.092938, -1.247288, -0.774519, -0.013374, -0.023872, -0.325804, -0.101626, 0.135651, -0.759197 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1816) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1816) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { -0.724, -0.423, 0.028, 0.043, 0.812, -0.568, 0.294, -0.375, -0.85, -0.119, -0.338, -0.415, 0.976, 0.507, 0.913, 0.697, 0.323, 0.206 };
   int lda = 3;
   double B[] = { 0.427, 0.621, -0.212, -0.942, -0.08, 0.416, 0.465, -0.972, -0.529, -0.252, -0.19, 0.073 };
   int ldb = 3;
   double B_expected[] = { -0.621, 0.427, 0.599301, -0.319337, -0.093325, -0.198531, 0.972, 0.465, 0.363393, -0.02779, 0.97279, -0.887585 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1817) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1817) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { 0.501, -0.632, 0.663, 0.151, -0.523, -0.71, -0.811, 0.8, -0.06, 0.994, -0.962, 0.827, -0.543, 0.719, -0.264, -0.942, 0.365, 0.051 };
   int lda = 3;
   double B[] = { -0.974, 0.094, -0.533, 0.633, -0.982, -0.383, -0.297, 0.734, -0.092, -0.15, 0.215, -0.232 };
   int ldb = 2;
   double B_expected[] = { -0.675337, -0.115274, 0.406006, -0.122575, -0.952024, -0.156194, -0.514956, 0.9092, 0.050058, -0.04123, 0.095645, 0.066643 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1818) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1818) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { 0.669, 0.332, -0.661, 0.611, 0.279, -0.133, -0.033, 0.06, 0.788, -0.407, -0.644, 0.958, 0.247, -0.161, 0.125, -0.184, 0.041, -0.045 };
   int lda = 3;
   double B[] = { -0.603, 0.88, 0.668, -0.152, 0.082, 0.033, 0.733, -0.557, 0.722, 0.024, -0.754, 0.458 };
   int ldb = 2;
   double B_expected[] = { -0.996161, -0.429256, 0.185867, 0.350415, -0.168848, 0.167834, 0.638486, 0.554478, -0.024, 0.722, -0.458, -0.754 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1819) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1819) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { -0.91, 0.05, -0.944, 0.748, -0.712, 0.619, -0.28, -0.906, 0.314, 0.943, -0.719, -0.983, 0.474, -0.115, -0.859, 0.837, 0.364, -0.164 };
   int lda = 3;
   double B[] = { -0.278, -0.34, 0.584, 0.43, -0.794, -0.465, -0.65, 0.461, 0.24, 0.003, 0.948, -0.778 };
   int ldb = 2;
   double B_expected[] = { -0.3233, 0.23598, 0.4205, -0.50994, -1.131636, -0.679699, 0.085048, 0.000967, -0.008447, 1.102325, 1.765785, 0.337213 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1820) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1820) imag");
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
   double alpha[2] = {0, 1};
   double A[] = { -0.39, -0.916, 0.257, -0.082, -0.802, 0.215, -0.155, 0.911, -0.099, 0.41, 0.057, 0.105, 0.94, -0.17, -0.714, -0.861, 0.292, -0.231 };
   int lda = 3;
   double B[] = { -0.453, -0.542, 0.135, 0.518, -0.199, 0.776, 0.784, -0.28, -0.499, -0.377, -0.795, -0.965 };
   int ldb = 2;
   double B_expected[] = { 0.542, -0.453, -0.518, 0.135, -0.59956, -0.270977, 0.135804, 0.776219, -0.220206, -0.182087, 1.507741, -0.776612 };
   cblas_ztrmm(order, side, uplo, trans, diag, M, N, alpha, A, lda, B, ldb);
   {
     int i;
     for (i = 0; i < 6; i++) {
       gsl_test_rel(B[2*i], B_expected[2*i], dbleps, "ztrmm(case 1821) real");
       gsl_test_rel(B[2*i+1], B_expected[2*i+1], dbleps, "ztrmm(case 1821) imag");
     };
   };
  };


}
