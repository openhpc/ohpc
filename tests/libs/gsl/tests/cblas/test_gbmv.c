#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_gbmv (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int trans = 111;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   float alpha = -1.0f;
   float beta = -1.0f;
   float A[] = { 0.423f, -0.143f, -0.182f, -0.076f, -0.855f, 0.599f, 0.389f, -0.473f, 0.493f, -0.902f, -0.889f, -0.256f, 0.112f, 0.128f, -0.277f, -0.777f };
   float X[] = { 0.488f, 0.029f, -0.633f, 0.84f };
   int incX = -1;
   float Y[] = { 0.874f, 0.322f, -0.477f };
   int incY = -1;
   float y_expected[] = { -0.101941f, 0.764086f, 0.481914f };
   cblas_sgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "sgbmv(case 794)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   float alpha = -1.0f;
   float beta = -1.0f;
   float A[] = { 0.423f, -0.143f, -0.182f, -0.076f, -0.855f, 0.599f, 0.389f, -0.473f, 0.493f, -0.902f, -0.889f, -0.256f, 0.112f, 0.128f, -0.277f, -0.777f };
   float X[] = { 0.488f, 0.029f, -0.633f, 0.84f };
   int incX = -1;
   float Y[] = { 0.874f, 0.322f, -0.477f };
   int incY = -1;
   float y_expected[] = { -0.656261f, 0.19575f, 0.055905f };
   cblas_sgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "sgbmv(case 795)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   float alpha = 0.0f;
   float beta = 0.1f;
   float A[] = { -0.066f, -0.153f, -0.619f, 0.174f, 0.777f, 0.543f, 0.614f, -0.446f, -0.138f, -0.767f, 0.725f, 0.222f, 0.165f, -0.063f, -0.047f, 0.267f };
   float X[] = { -0.096f, -0.007f, -0.657f };
   int incX = -1;
   float Y[] = { -0.88f, 0.102f, -0.278f, 0.403f };
   int incY = -1;
   float y_expected[] = { -0.088f, 0.0102f, -0.0278f, 0.0403f };
   cblas_sgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "sgbmv(case 796)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   float alpha = 0.0f;
   float beta = 0.1f;
   float A[] = { -0.066f, -0.153f, -0.619f, 0.174f, 0.777f, 0.543f, 0.614f, -0.446f, -0.138f, -0.767f, 0.725f, 0.222f, 0.165f, -0.063f, -0.047f, 0.267f };
   float X[] = { -0.096f, -0.007f, -0.657f };
   int incX = -1;
   float Y[] = { -0.88f, 0.102f, -0.278f, 0.403f };
   int incY = -1;
   float y_expected[] = { -0.088f, 0.0102f, -0.0278f, 0.0403f };
   cblas_sgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "sgbmv(case 797)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   double alpha = 0.1;
   double beta = 0;
   double A[] = { -0.688, 0.29, 0.442, -0.001, 0.313, -0.073, 0.991, -0.654, -0.12, 0.416, 0.571, 0.932, -0.179, -0.724, 0.492, -0.965 };
   double X[] = { 0.187, -0.338, -0.976, -0.052 };
   int incX = -1;
   double Y[] = { -0.101, 0.8, 0.026 };
   int incY = -1;
   double y_expected[] = { 0.0083289, -0.0279986, -0.0446472 };
   cblas_dgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dgbmv(case 798)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   double alpha = 0.1;
   double beta = 0;
   double A[] = { -0.688, 0.29, 0.442, -0.001, 0.313, -0.073, 0.991, -0.654, -0.12, 0.416, 0.571, 0.932, -0.179, -0.724, 0.492, -0.965 };
   double X[] = { 0.187, -0.338, -0.976, -0.052 };
   int incX = -1;
   double Y[] = { -0.101, 0.8, 0.026 };
   int incY = -1;
   double y_expected[] = { -0.1141297, 0.0088824, -0.0320568 };
   cblas_dgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dgbmv(case 799)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   double alpha = -0.3;
   double beta = -0.3;
   double A[] = { 0.746, 0.262, -0.449, -0.954, -0.093, 0.108, -0.496, 0.927, 0.177, 0.729, -0.92, -0.469, 0.87, -0.877, -0.308, -0.806 };
   double X[] = { 0.662, -0.887, 0.261 };
   int incX = -1;
   double Y[] = { 0.771, 0.637, -0.177, -0.018 };
   int incY = -1;
   double y_expected[] = { -0.048588, -0.467865, 0.0818433, -0.0398619 };
   cblas_dgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dgbmv(case 800)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   double alpha = -0.3;
   double beta = -0.3;
   double A[] = { 0.746, 0.262, -0.449, -0.954, -0.093, 0.108, -0.496, 0.927, 0.177, 0.729, -0.92, -0.469, 0.87, -0.877, -0.308, -0.806 };
   double X[] = { 0.662, -0.887, 0.261 };
   int incX = -1;
   double Y[] = { 0.771, 0.637, -0.177, -0.018 };
   int incY = -1;
   double y_expected[] = { -0.404082, -0.2887797, 0.1876263, -0.1345935 };
   cblas_dgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "dgbmv(case 801)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {0.0f, 0.1f};
   float A[] = { -0.107f, 0.926f, -0.246f, -0.555f, -0.301f, 0.276f, 0.471f, -0.084f, -0.754f, 0.082f, -0.952f, -0.394f, 0.659f, 0.054f, 0.795f, 0.923f, 0.232f, -0.788f, 0.478f, 0.775f, -0.118f, 0.691f, -0.933f, 0.809f, 0.164f, -0.263f, -0.923f, -0.88f, 0.819f, -0.521f, -0.045f, 0.034f };
   float X[] = { 0.407f, 0.895f, 0.301f, 0.769f, -0.269f, -0.465f, 0.455f, -0.628f };
   int incX = -1;
   float Y[] = { -0.116f, -0.744f, -0.936f, -0.064f, -0.232f, -0.665f };
   int incY = -1;
   float y_expected[] = { -0.806176f, -1.559f, -1.57611f, -0.155463f, 0.098816f, -0.274361f };
   cblas_cgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "cgbmv(case 802) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "cgbmv(case 802) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   float alpha[2] = {0.0f, 1.0f};
   float beta[2] = {0.0f, 0.1f};
   float A[] = { -0.107f, 0.926f, -0.246f, -0.555f, -0.301f, 0.276f, 0.471f, -0.084f, -0.754f, 0.082f, -0.952f, -0.394f, 0.659f, 0.054f, 0.795f, 0.923f, 0.232f, -0.788f, 0.478f, 0.775f, -0.118f, 0.691f, -0.933f, 0.809f, 0.164f, -0.263f, -0.923f, -0.88f, 0.819f, -0.521f, -0.045f, 0.034f };
   float X[] = { 0.407f, 0.895f, 0.301f, 0.769f, -0.269f, -0.465f, 0.455f, -0.628f };
   int incX = -1;
   float Y[] = { -0.116f, -0.744f, -0.936f, -0.064f, -0.232f, -0.665f };
   int incY = -1;
   float y_expected[] = { -0.245235f, -0.313725f, -0.798094f, 0.691455f, -0.164015f, -0.242714f };
   cblas_cgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "cgbmv(case 803) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "cgbmv(case 803) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   float alpha[2] = {-1.0f, 0.0f};
   float beta[2] = {0.0f, 0.1f};
   float A[] = { -0.258f, 0.838f, -0.106f, -0.066f, 0.395f, 0.982f, -0.546f, 0.565f, 0.14f, -0.18f, 0.165f, -0.186f, 0.499f, -0.038f, -0.305f, -0.653f, -0.811f, -0.466f, -0.674f, -0.013f, -0.552f, -0.807f, -0.536f, 0.864f, -0.027f, -0.606f, 0.459f, 0.564f, -0.968f, 0.717f, -0.312f, -0.485f };
   float X[] = { -0.399f, 0.459f, 0.398f, 0.358f, -0.161f, -0.359f };
   int incX = -1;
   float Y[] = { 0.572f, 0.293f, -0.813f, -0.096f, -0.611f, -0.717f, 0.736f, 0.259f };
   int incY = -1;
   float y_expected[] = { -0.619961f, -0.011425f, -0.477499f, 0.059361f, -0.886984f, 0.44008f, -0.139432f, 0.04644f };
   cblas_cgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "cgbmv(case 804) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "cgbmv(case 804) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   float alpha[2] = {-1.0f, 0.0f};
   float beta[2] = {0.0f, 0.1f};
   float A[] = { -0.258f, 0.838f, -0.106f, -0.066f, 0.395f, 0.982f, -0.546f, 0.565f, 0.14f, -0.18f, 0.165f, -0.186f, 0.499f, -0.038f, -0.305f, -0.653f, -0.811f, -0.466f, -0.674f, -0.013f, -0.552f, -0.807f, -0.536f, 0.864f, -0.027f, -0.606f, 0.459f, 0.564f, -0.968f, 0.717f, -0.312f, -0.485f };
   float X[] = { -0.399f, 0.459f, 0.398f, 0.358f, -0.161f, -0.359f };
   int incX = -1;
   float Y[] = { 0.572f, 0.293f, -0.813f, -0.096f, -0.611f, -0.717f, 0.736f, 0.259f };
   int incY = -1;
   float y_expected[] = { -0.318227f, -0.172201f, -0.109343f, 0.698685f, 0.208261f, -0.269065f, 0.175074f, -0.507326f };
   cblas_cgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "cgbmv(case 805) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "cgbmv(case 805) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   float alpha[2] = {-1.0f, 0.0f};
   float beta[2] = {0.0f, 0.1f};
   float A[] = { -0.804f, 0.232f, -0.448f, -0.558f, -0.078f, -0.056f, -0.345f, -0.379f, 0.369f, -0.662f, -0.169f, -0.391f, -0.215f, 0.467f, 0.374f, 0.889f, -0.698f, 0.734f, 0.377f, -0.955f, 0.498f, 0.151f, -0.725f, -0.728f, -0.655f, -0.581f, 0.389f, 0.949f, -0.553f, -0.434f, 0.237f, 0.641f };
   float X[] = { -0.262f, -0.823f, -0.357f, -0.994f, -0.347f, -0.375f };
   int incX = -1;
   float Y[] = { -0.683f, -0.87f, -0.708f, 0.071f, 0.575f, -0.575f, 0.845f, 0.032f };
   int incY = -1;
   float y_expected[] = { 0.341749f, 0.301992f, -0.306848f, 0.109252f, -0.018347f, -0.747479f, -0.894201f, 0.713246f };
   cblas_cgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "cgbmv(case 806) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "cgbmv(case 806) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   float alpha[2] = {-1.0f, 0.0f};
   float beta[2] = {0.0f, 0.1f};
   float A[] = { -0.804f, 0.232f, -0.448f, -0.558f, -0.078f, -0.056f, -0.345f, -0.379f, 0.369f, -0.662f, -0.169f, -0.391f, -0.215f, 0.467f, 0.374f, 0.889f, -0.698f, 0.734f, 0.377f, -0.955f, 0.498f, 0.151f, -0.725f, -0.728f, -0.655f, -0.581f, 0.389f, 0.949f, -0.553f, -0.434f, 0.237f, 0.641f };
   float X[] = { -0.262f, -0.823f, -0.357f, -0.994f, -0.347f, -0.375f };
   int incX = -1;
   float Y[] = { -0.683f, -0.87f, -0.708f, 0.071f, 0.575f, -0.575f, 0.845f, 0.032f };
   int incY = -1;
   float y_expected[] = { -0.562773f, -0.455143f, -0.213881f, -0.466169f, -0.183683f, 0.097891f, -0.451416f, 0.052586f };
   cblas_cgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], flteps, "cgbmv(case 807) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], flteps, "cgbmv(case 807) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   double alpha[2] = {0, 0.1};
   double beta[2] = {1, 0};
   double A[] = { -0.919, -0.002, 0.105, -0.338, -0.358, -0.715, -0.157, 0.307, 0.334, 0.121, 0.366, 0.029, -0.006, -0.662, -0.314, 0.061, -0.322, -0.865, -0.586, 0.556, 0.507, 0.581, 0.855, -0.09, 0.836, -0.788, -0.209, -0.694, -0.695, 0.11, -0.234, 0.17 };
   double X[] = { 0.356, -0.76, -0.96, 0.437, -0.849, 0.397, -0.382, -0.826 };
   int incX = -1;
   double Y[] = { 0.288, -0.832, 0.889, 0.576, -0.809, 0.4 };
   int incY = -1;
   double y_expected[] = { 0.3241775, -0.6761577, 0.8458527, 0.5705165, -0.8597295, 0.4268499 };
   cblas_zgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zgbmv(case 808) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zgbmv(case 808) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   double alpha[2] = {0, 0.1};
   double beta[2] = {1, 0};
   double A[] = { -0.919, -0.002, 0.105, -0.338, -0.358, -0.715, -0.157, 0.307, 0.334, 0.121, 0.366, 0.029, -0.006, -0.662, -0.314, 0.061, -0.322, -0.865, -0.586, 0.556, 0.507, 0.581, 0.855, -0.09, 0.836, -0.788, -0.209, -0.694, -0.695, 0.11, -0.234, 0.17 };
   double X[] = { 0.356, -0.76, -0.96, 0.437, -0.849, 0.397, -0.382, -0.826 };
   int incX = -1;
   double Y[] = { 0.288, -0.832, 0.889, 0.576, -0.809, 0.4 };
   int incY = -1;
   double y_expected[] = { 0.4026074, -0.8033768, 0.7510795, 0.5671044, -0.8162255, 0.3349099 };
   cblas_zgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zgbmv(case 809) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zgbmv(case 809) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   double A[] = { 0.511, -0.707, -0.906, 0.345, -0.524, -0.933, 0.154, -0.529, -0.651, -0.851, 0.104, 0.532, -0.297, 0.477, 0.511, 0.469, -0.888, -0.789, 0.656, 0.288, -0.749, 0.961, 0.571, 0.539, 0.465, 0.647, 0.653, -0.994, -0.515, 0.297, 0.35, -0.707 };
   double X[] = { -0.991, 0.658, -0.909, -0.99, -0.517, -0.071 };
   int incX = -1;
   double Y[] = { 0.451, 0.351, -0.113, -0.62, 0.983, 0.511, 0.142, -0.186 };
   int incY = -1;
   double y_expected[] = { 0.560921, -1.094193, -0.210397, -0.613323, 3.018979, 0.641612, 0.384166, 1.11801 };
   cblas_zgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zgbmv(case 810) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zgbmv(case 810) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   double alpha[2] = {1, 0};
   double beta[2] = {1, 0};
   double A[] = { 0.511, -0.707, -0.906, 0.345, -0.524, -0.933, 0.154, -0.529, -0.651, -0.851, 0.104, 0.532, -0.297, 0.477, 0.511, 0.469, -0.888, -0.789, 0.656, 0.288, -0.749, 0.961, 0.571, 0.539, 0.465, 0.647, 0.653, -0.994, -0.515, 0.297, 0.35, -0.707 };
   double X[] = { -0.991, 0.658, -0.909, -0.99, -0.517, -0.071 };
   int incX = -1;
   double Y[] = { 0.451, 0.351, -0.113, -0.62, 0.983, 0.511, 0.142, -0.186 };
   int incY = -1;
   double y_expected[] = { -0.435541, 0.015793, -0.926518, 1.122561, 1.671751, -0.257493, 0.187543, 1.066818 };
   cblas_zgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zgbmv(case 811) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zgbmv(case 811) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   double alpha[2] = {0, 0.1};
   double beta[2] = {-0.3, 0.1};
   double A[] = { 0.534, 0.67, -0.621, 0.143, -0.794, 0.073, 0.414, -0.9, 0.155, -0.368, 0.122, -0.583, 0.03, 0.646, -0.768, -0.892, -0.741, -0.397, 0.626, 0.004, -0.515, 0.355, 0.196, -0.989, -0.982, 0.985, 0.445, 0.63, -0.849, -0.528, 0.146, -0.319 };
   double X[] = { -0.199, -0.259, 0.386, -0.131, -0.867, 0.888 };
   int incX = -1;
   double Y[] = { 0.106, 0.874, 0.962, 0.636, -0.759, 0.415, -0.053, 0.315 };
   int incY = -1;
   double y_expected[] = { -0.139603, -0.250546, -0.3107376, -0.1144656, 0.2181809, -0.0877031, 0.0149724, -0.0224571 };
   cblas_zgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zgbmv(case 812) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zgbmv(case 812) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int M = 3;
   int N = 4;
   int KL = 1;
   int KU = 1;
   int lda = 4;
   double alpha[2] = {0, 0.1};
   double beta[2] = {-0.3, 0.1};
   double A[] = { 0.534, 0.67, -0.621, 0.143, -0.794, 0.073, 0.414, -0.9, 0.155, -0.368, 0.122, -0.583, 0.03, 0.646, -0.768, -0.892, -0.741, -0.397, 0.626, 0.004, -0.515, 0.355, 0.196, -0.989, -0.982, 0.985, 0.445, 0.63, -0.849, -0.528, 0.146, -0.319 };
   double X[] = { -0.199, -0.259, 0.386, -0.131, -0.867, 0.888 };
   int incX = -1;
   double Y[] = { 0.106, 0.874, 0.962, 0.636, -0.759, 0.415, -0.053, 0.315 };
   int incY = -1;
   double y_expected[] = { -0.1642353, -0.2575697, -0.3610975, -0.1305629, 0.1713576, -0.2514988, 0.0195631, -0.0648656 };
   cblas_zgbmv(order, trans, M, N, KU, KL, alpha, A,                                  lda, X, incX, beta, Y, incY);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(Y[2*i], y_expected[2*i], dbleps, "zgbmv(case 813) real");
       gsl_test_rel(Y[2*i+1], y_expected[2*i+1], dbleps, "zgbmv(case 813) imag");
     };
   };
  };


}
