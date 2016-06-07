#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_tbsv (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.681f, 0.209f, 0.436f, -0.369f, 0.786f, -0.84f, 0.86f, -0.233f, 0.734f };
   float X[] = { -0.305f, 0.61f, -0.831f };
   int incX = -1;
   float x_expected[] = { -0.354651f, -2.40855f, 0.481076f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1230)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.681f, 0.209f, 0.436f, -0.369f, 0.786f, -0.84f, 0.86f, -0.233f, 0.734f };
   float X[] = { -0.305f, 0.61f, -0.831f };
   int incX = -1;
   float x_expected[] = { -0.305f, 0.84973f, -1.00859f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1231)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.681f, 0.209f, 0.436f, -0.369f, 0.786f, -0.84f, 0.86f, -0.233f, 0.734f };
   float X[] = { -0.305f, 0.61f, -0.831f };
   int incX = -1;
   float x_expected[] = { -2.71619f, -1.09055f, -3.97608f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1232)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.681f, 0.209f, 0.436f, -0.369f, 0.786f, -0.84f, 0.86f, -0.233f, 0.734f };
   float X[] = { -0.305f, 0.61f, -0.831f };
   int incX = -1;
   float x_expected[] = { -0.56589f, 0.303361f, -0.831f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1233)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.681f, 0.209f, 0.436f, -0.369f, 0.786f, -0.84f, 0.86f, -0.233f, 0.734f };
   float X[] = { -0.305f, 0.61f, -0.831f };
   int incX = -1;
   float x_expected[] = { 1.30901f, -0.656172f, -5.13458f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1234)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.681f, 0.209f, 0.436f, -0.369f, 0.786f, -0.84f, 0.86f, -0.233f, 0.734f };
   float X[] = { -0.305f, 0.61f, -0.831f };
   int incX = -1;
   float x_expected[] = { -0.305f, 0.8723f, -0.509121f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1235)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.681f, 0.209f, 0.436f, -0.369f, 0.786f, -0.84f, 0.86f, -0.233f, 0.734f };
   float X[] = { -0.305f, 0.61f, -0.831f };
   int incX = -1;
   float x_expected[] = { 0.524539f, -0.961964f, 1.22026f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1236)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.681f, 0.209f, 0.436f, -0.369f, 0.786f, -0.84f, 0.86f, -0.233f, 0.734f };
   float X[] = { -0.305f, 0.61f, -0.831f };
   int incX = -1;
   float x_expected[] = { -0.920972f, 0.783679f, -0.831f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1237)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.022f, 0.795f, -0.389f, -0.205f, -0.121f, 0.323f, 0.133f, 0.679f, 0.742f };
   float X[] = { 0.144f, 0.635f, 0.116f };
   int incX = -1;
   float x_expected[] = { 16.8676f, 17.3503f, 5.27273f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1238)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.022f, 0.795f, -0.389f, -0.205f, -0.121f, 0.323f, 0.133f, 0.679f, 0.742f };
   float X[] = { 0.144f, 0.635f, 0.116f };
   int incX = -1;
   float x_expected[] = { 0.209676f, 0.54278f, 0.116f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1239)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.022f, 0.795f, -0.389f, -0.205f, -0.121f, 0.323f, 0.133f, 0.679f, 0.742f };
   float X[] = { 0.144f, 0.635f, 0.116f };
   int incX = -1;
   float x_expected[] = { 0.212077f, -5.01482f, -1.14722f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1240)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.022f, 0.795f, -0.389f, -0.205f, -0.121f, 0.323f, 0.133f, 0.679f, 0.742f };
   float X[] = { 0.144f, 0.635f, 0.116f };
   int incX = -1;
   float x_expected[] = { 0.144f, 0.615848f, 0.242249f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1241)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.022f, 0.795f, -0.389f, -0.205f, -0.121f, 0.323f, 0.133f, 0.679f, 0.742f };
   float X[] = { 0.144f, 0.635f, 0.116f };
   int incX = -1;
   float x_expected[] = { 1.28844f, -5.49514f, 0.145912f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1242)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.022f, 0.795f, -0.389f, -0.205f, -0.121f, 0.323f, 0.133f, 0.679f, 0.742f };
   float X[] = { 0.144f, 0.635f, 0.116f };
   int incX = -1;
   float x_expected[] = { 0.0563823f, 0.65878f, 0.116f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1243)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.022f, 0.795f, -0.389f, -0.205f, -0.121f, 0.323f, 0.133f, 0.679f, 0.742f };
   float X[] = { 0.144f, 0.635f, 0.116f };
   int incX = -1;
   float x_expected[] = { 1.08271f, -3.73662f, 140.301f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1244)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.022f, 0.795f, -0.389f, -0.205f, -0.121f, 0.323f, 0.133f, 0.679f, 0.742f };
   float X[] = { 0.144f, 0.635f, 0.116f };
   int incX = -1;
   float x_expected[] = { 0.144f, 0.652424f, -0.402677f };
   cblas_stbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbsv(case 1245)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.619, -0.443, 0.957, -0.633, -0.698, 0.783, -0.343, -0.603, 0.735 };
   double X[] = { 0.332, 0.588, 0.252 };
   int incX = -1;
   double x_expected[] = { -0.967930029155, 0.138412575592, 0.506166027443 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1246)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.619, -0.443, 0.957, -0.633, -0.698, 0.783, -0.343, -0.603, 0.735 };
   double X[] = { 0.332, 0.588, 0.252 };
   int incX = -1;
   double x_expected[] = { 0.332, 0.819736, 0.615143048 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1247)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.619, -0.443, 0.957, -0.633, -0.698, 0.783, -0.343, -0.603, 0.735 };
   double X[] = { 0.332, 0.588, 0.252 };
   int incX = -1;
   double x_expected[] = { -0.364842154056, -0.326531140246, -0.568848758465 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1248)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.619, -0.443, 0.957, -0.633, -0.698, 0.783, -0.343, -0.603, 0.735 };
   double X[] = { 0.332, 0.588, 0.252 };
   int incX = -1;
   double x_expected[] = { 0.588397988, 0.747516, 0.252 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1249)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.619, -0.443, 0.957, -0.633, -0.698, 0.783, -0.343, -0.603, 0.735 };
   double X[] = { 0.332, 0.588, 0.252 };
   int incX = -1;
   double x_expected[] = { -0.550580431177, -0.571849444278, 0.248263427151 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1250)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.619, -0.443, 0.957, -0.633, -0.698, 0.783, -0.343, -0.603, 0.735 };
   double X[] = { 0.332, 0.588, 0.252 };
   int incX = -1;
   double x_expected[] = { 0.332, 0.701876, 0.696287508 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1251)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.619, -0.443, 0.957, -0.633, -0.698, 0.783, -0.343, -0.603, 0.735 };
   double X[] = { 0.332, 0.588, 0.252 };
   int incX = -1;
   double x_expected[] = { 1.50217883761, -1.21382140588, 0.407108239095 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1252)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.619, -0.443, 0.957, -0.633, -0.698, 0.783, -0.343, -0.603, 0.735 };
   double X[] = { 0.332, 0.588, 0.252 };
   int incX = -1;
   double x_expected[] = { 0.820345928, 0.699636, 0.252 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1253)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.199, 0.303, -0.705, -0.013, -0.678, 0.547, 0.756, -0.177, -0.079 };
   double X[] = { 0.58, 0.558, -0.54 };
   int incX = -1;
   double x_expected[] = { 18.994209959, 20.323927329, 2.7135678392 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1254)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.199, 0.303, -0.705, -0.013, -0.678, 0.547, 0.756, -0.177, -0.079 };
   double X[] = { 0.58, 0.558, -0.54 };
   int incX = -1;
   double x_expected[] = { 1.06925836, 0.72162, -0.54 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1255)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.199, 0.303, -0.705, -0.013, -0.678, 0.547, 0.756, -0.177, -0.079 };
   double X[] = { 0.58, 0.558, -0.54 };
   int incX = -1;
   double x_expected[] = { -3.27683615819, -4.47682615869, -1.97425326753 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1256)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.199, 0.303, -0.705, -0.013, -0.678, 0.547, 0.756, -0.177, -0.079 };
   double X[] = { 0.58, 0.558, -0.54 };
   int incX = -1;
   double x_expected[] = { 0.58, 0.11952, -0.53844624 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1257)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.199, 0.303, -0.705, -0.013, -0.678, 0.547, 0.756, -0.177, -0.079 };
   double X[] = { 0.58, 0.558, -0.54 };
   int incX = -1;
   double x_expected[] = { -6.6461072986, -0.788837290809, -1.78217821782 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1258)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.199, 0.303, -0.705, -0.013, -0.678, 0.547, 0.756, -0.177, -0.079 };
   double X[] = { 0.58, 0.558, -0.54 };
   int incX = -1;
   double x_expected[] = { 0.16345912, 0.55098, -0.54 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1259)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.199, 0.303, -0.705, -0.013, -0.678, 0.547, 0.756, -0.177, -0.079 };
   double X[] = { 0.58, 0.558, -0.54 };
   int incX = -1;
   double x_expected[] = { 0.767195767196, -82.9352869353, -123.564783625 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1260)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.199, 0.303, -0.705, -0.013, -0.678, 0.547, 0.756, -0.177, -0.079 };
   double X[] = { 0.58, 0.558, -0.54 };
   int incX = -1;
   double x_expected[] = { 0.58, 0.95124, -0.82822572 };
   cblas_dtbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbsv(case 1261)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.975f, -0.667f, 0.813f, -0.962f, -0.961f, 0.226f, -0.503f, 0.809f, 0.81f, -0.162f, -0.027f, -0.044f, 0.212f, 0.563f, 0.446f, -0.392f, 0.798f, -0.07f };
   float X[] = { 0.11f, 0.787f, -0.826f, 0.809f, -0.437f, 0.592f };
   int incX = -1;
   float x_expected[] = { 1.28871f, 0.289887f, 1.76043f, 1.27481f, 1.56506f, -2.35181f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1262) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1262) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.975f, -0.667f, 0.813f, -0.962f, -0.961f, 0.226f, -0.503f, 0.809f, 0.81f, -0.162f, -0.027f, -0.044f, 0.212f, 0.563f, 0.446f, -0.392f, 0.798f, -0.07f };
   float X[] = { 0.11f, 0.787f, -0.826f, 0.809f, -0.437f, 0.592f };
   int incX = -1;
   float x_expected[] = { 0.11f, 0.787f, -1.04259f, 0.18935f, 0.228474f, -0.564917f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1263) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1263) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.975f, -0.667f, 0.813f, -0.962f, -0.961f, 0.226f, -0.503f, 0.809f, 0.81f, -0.162f, -0.027f, -0.044f, 0.212f, 0.563f, 0.446f, -0.392f, 0.798f, -0.07f };
   float X[] = { 0.11f, 0.787f, -0.826f, 0.809f, -0.437f, 0.592f };
   int incX = -1;
   float x_expected[] = { -0.0906249f, 3.09442f, -1.60036f, 1.28475f, -0.582941f, 0.0383898f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1264) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1264) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.975f, -0.667f, 0.813f, -0.962f, -0.961f, 0.226f, -0.503f, 0.809f, 0.81f, -0.162f, -0.027f, -0.044f, 0.212f, 0.563f, 0.446f, -0.392f, 0.798f, -0.07f };
   float X[] = { 0.11f, 0.787f, -0.826f, 0.809f, -0.437f, 0.592f };
   int incX = -1;
   float x_expected[] = { 1.05233f, 0.79657f, -0.566883f, 1.46031f, -0.437f, 0.592f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1265) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1265) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.975f, -0.667f, 0.813f, -0.962f, -0.961f, 0.226f, -0.503f, 0.809f, 0.81f, -0.162f, -0.027f, -0.044f, 0.212f, 0.563f, 0.446f, -0.392f, 0.798f, -0.07f };
   float X[] = { 0.11f, 0.787f, -0.826f, 0.809f, -0.437f, 0.592f };
   int incX = -1;
   float x_expected[] = { -0.735844f, 1.11782f, -0.28244f, 1.16117f, -0.66707f, 0.938302f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1266) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1266) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.975f, -0.667f, 0.813f, -0.962f, -0.961f, 0.226f, -0.503f, 0.809f, 0.81f, -0.162f, -0.027f, -0.044f, 0.212f, 0.563f, 0.446f, -0.392f, 0.798f, -0.07f };
   float X[] = { 0.11f, 0.787f, -0.826f, 0.809f, -0.437f, 0.592f };
   int incX = -1;
   float x_expected[] = { 0.11f, 0.787f, -0.406239f, 0.580226f, -0.171935f, 1.2125f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1267) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1267) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.975f, -0.667f, 0.813f, -0.962f, -0.961f, 0.226f, -0.503f, 0.809f, 0.81f, -0.162f, -0.027f, -0.044f, 0.212f, 0.563f, 0.446f, -0.392f, 0.798f, -0.07f };
   float X[] = { 0.11f, 0.787f, -0.826f, 0.809f, -0.437f, 0.592f };
   int incX = -1;
   float x_expected[] = { 1.70081f, 2.20477f, 1.32753f, -0.522112f, 0.0223652f, -0.62248f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1268) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1268) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { -0.975f, -0.667f, 0.813f, -0.962f, -0.961f, 0.226f, -0.503f, 0.809f, 0.81f, -0.162f, -0.027f, -0.044f, 0.212f, 0.563f, 0.446f, -0.392f, 0.798f, -0.07f };
   float X[] = { 0.11f, 0.787f, -0.826f, 0.809f, -0.437f, 0.592f };
   int incX = -1;
   float x_expected[] = { 0.967596f, 0.693563f, -1.04022f, -0.09269f, -0.437f, 0.592f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1269) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1269) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.33f, -0.236f, 0.267f, -0.139f, 0.25f, 0.509f, 0.86f, -0.089f, -0.018f, -0.847f, 0.424f, -0.573f, 0.097f, -0.663f, 0.65f, -0.811f, 0.283f, 0.032f };
   float X[] = { -0.509f, 0.608f, 0.021f, -0.848f, -0.134f, 0.179f };
   int incX = -1;
   float x_expected[] = { -1.11985f, 0.801655f, 0.273814f, -1.09438f, -0.52531f, 0.166748f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1270) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1270) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.33f, -0.236f, 0.267f, -0.139f, 0.25f, 0.509f, 0.86f, -0.089f, -0.018f, -0.847f, 0.424f, -0.573f, 0.097f, -0.663f, 0.65f, -0.811f, 0.283f, 0.032f };
   float X[] = { -0.509f, 0.608f, 0.021f, -0.848f, -0.134f, 0.179f };
   int incX = -1;
   float x_expected[] = { 0.266087f, 0.618557f, 0.031897f, -0.914419f, -0.134f, 0.179f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1271) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1271) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.33f, -0.236f, 0.267f, -0.139f, 0.25f, 0.509f, 0.86f, -0.089f, -0.018f, -0.847f, 0.424f, -0.573f, 0.097f, -0.663f, 0.65f, -0.811f, 0.283f, 0.032f };
   float X[] = { -0.509f, 0.608f, 0.021f, -0.848f, -0.134f, 0.179f };
   int incX = -1;
   float x_expected[] = { -0.762749f, -0.016292f, 1.59299f, 0.158751f, -4.75603f, -1.78591f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1272) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1272) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.33f, -0.236f, 0.267f, -0.139f, 0.25f, 0.509f, 0.86f, -0.089f, -0.018f, -0.847f, 0.424f, -0.573f, 0.097f, -0.663f, 0.65f, -0.811f, 0.283f, 0.032f };
   float X[] = { -0.509f, 0.608f, 0.021f, -0.848f, -0.134f, 0.179f };
   int incX = -1;
   float x_expected[] = { -0.509f, 0.608f, -0.332731f, -1.24444f, 0.262904f, 1.21961f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1273) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1273) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.33f, -0.236f, 0.267f, -0.139f, 0.25f, 0.509f, 0.86f, -0.089f, -0.018f, -0.847f, 0.424f, -0.573f, 0.097f, -0.663f, 0.65f, -0.811f, 0.283f, 0.032f };
   float X[] = { -0.509f, 0.608f, 0.021f, -0.848f, -0.134f, 0.179f };
   int incX = -1;
   float x_expected[] = { -1.76046f, 0.0455463f, 1.38348f, 0.700097f, -0.669451f, 0.321896f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1274) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1274) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.33f, -0.236f, 0.267f, -0.139f, 0.25f, 0.509f, 0.86f, -0.089f, -0.018f, -0.847f, 0.424f, -0.573f, 0.097f, -0.663f, 0.65f, -0.811f, 0.283f, 0.032f };
   float X[] = { -0.509f, 0.608f, 0.021f, -0.848f, -0.134f, 0.179f };
   int incX = -1;
   float x_expected[] = { 0.151523f, 0.78611f, 0.120309f, -1.01387f, -0.134f, 0.179f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1275) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1275) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.33f, -0.236f, 0.267f, -0.139f, 0.25f, 0.509f, 0.86f, -0.089f, -0.018f, -0.847f, 0.424f, -0.573f, 0.097f, -0.663f, 0.65f, -0.811f, 0.283f, 0.032f };
   float X[] = { -0.509f, 0.608f, 0.021f, -0.848f, -0.134f, 0.179f };
   int incX = -1;
   float x_expected[] = { -1.00779f, -0.620278f, 0.81164f, -1.90759f, -1.32022f, 1.48356f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1276) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1276) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.33f, -0.236f, 0.267f, -0.139f, 0.25f, 0.509f, 0.86f, -0.089f, -0.018f, -0.847f, 0.424f, -0.573f, 0.097f, -0.663f, 0.65f, -0.811f, 0.283f, 0.032f };
   float X[] = { -0.509f, 0.608f, 0.021f, -0.848f, -0.134f, 0.179f };
   int incX = -1;
   float x_expected[] = { -0.509f, 0.608f, -0.503138f, -1.26818f, 0.176615f, 0.447668f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1277) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1277) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.041f, -0.61f, 0.099f, -0.393f, 0.357f, -0.984f, -0.576f, -0.342f, -0.903f, -0.083f, -0.157f, -0.694f, 0.768f, 0.688f, 0.203f, -0.079f, 0.298f, -0.424f };
   float X[] = { -0.037f, -0.599f, 0.959f, -0.499f, 0.296f, 0.034f };
   int incX = -1;
   float x_expected[] = { -0.613838f, -1.13321f, -1.34847f, 0.0432903f, 0.0879552f, -0.479334f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1278) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1278) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.041f, -0.61f, 0.099f, -0.393f, 0.357f, -0.984f, -0.576f, -0.342f, -0.903f, -0.083f, -0.157f, -0.694f, 0.768f, 0.688f, 0.203f, -0.079f, 0.298f, -0.424f };
   float X[] = { -0.037f, -0.599f, 0.959f, -0.499f, 0.296f, 0.034f };
   int incX = -1;
   float x_expected[] = { 0.76323f, -1.23595f, 0.943058f, -0.618694f, 0.296f, 0.034f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1279) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1279) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.041f, -0.61f, 0.099f, -0.393f, 0.357f, -0.984f, -0.576f, -0.342f, -0.903f, -0.083f, -0.157f, -0.694f, 0.768f, 0.688f, 0.203f, -0.079f, 0.298f, -0.424f };
   float X[] = { -0.037f, -0.599f, 0.959f, -0.499f, 0.296f, 0.034f };
   int incX = -1;
   float x_expected[] = { -1.15557f, -2.50103f, -3.85402f, -1.04833f, 0.414582f, 5.91218f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1280) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1280) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.041f, -0.61f, 0.099f, -0.393f, 0.357f, -0.984f, -0.576f, -0.342f, -0.903f, -0.083f, -0.157f, -0.694f, 0.768f, 0.688f, 0.203f, -0.079f, 0.298f, -0.424f };
   float X[] = { -0.037f, -0.599f, 0.959f, -0.499f, 0.296f, 0.034f };
   int incX = -1;
   float x_expected[] = { -0.037f, -0.599f, 1.39953f, -0.064424f, 1.0801f, -0.481747f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1281) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1281) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.041f, -0.61f, 0.099f, -0.393f, 0.357f, -0.984f, -0.576f, -0.342f, -0.903f, -0.083f, -0.157f, -0.694f, 0.768f, 0.688f, 0.203f, -0.079f, 0.298f, -0.424f };
   float X[] = { -0.037f, -0.599f, 0.959f, -0.499f, 0.296f, 0.034f };
   int incX = -1;
   float x_expected[] = { -3.0802f, -9.09377f, -1.05845f, 0.99239f, 0.259763f, -0.687744f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1282) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1282) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.041f, -0.61f, 0.099f, -0.393f, 0.357f, -0.984f, -0.576f, -0.342f, -0.903f, -0.083f, -0.157f, -0.694f, 0.768f, 0.688f, 0.203f, -0.079f, 0.298f, -0.424f };
   float X[] = { -0.037f, -0.599f, 0.959f, -0.499f, 0.296f, 0.034f };
   int incX = -1;
   float x_expected[] = { -0.513897f, 0.632031f, 1.14112f, -0.580648f, 0.296f, 0.034f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1283) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1283) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.041f, -0.61f, 0.099f, -0.393f, 0.357f, -0.984f, -0.576f, -0.342f, -0.903f, -0.083f, -0.157f, -0.694f, 0.768f, 0.688f, 0.203f, -0.079f, 0.298f, -0.424f };
   float X[] = { -0.037f, -0.599f, 0.959f, -0.499f, 0.296f, 0.034f };
   int incX = -1;
   float x_expected[] = { 0.360899f, -0.456643f, -2.31803f, 0.257877f, 1.56928f, -0.922115f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1284) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1284) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.041f, -0.61f, 0.099f, -0.393f, 0.357f, -0.984f, -0.576f, -0.342f, -0.903f, -0.083f, -0.157f, -0.694f, 0.768f, 0.688f, 0.203f, -0.079f, 0.298f, -0.424f };
   float X[] = { -0.037f, -0.599f, 0.959f, -0.499f, 0.296f, 0.034f };
   int incX = -1;
   float x_expected[] = { -0.037f, -0.599f, 0.875872f, -1.03683f, -0.198184f, -0.207572f };
   cblas_ctbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbsv(case 1285) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbsv(case 1285) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.474, 0.715, 0.061, 0.532, 0.004, -0.318, 0.37, -0.692, -0.166, 0.039, -0.946, 0.857, -0.922, -0.491, 0.012, -0.217, -0.674, -0.429 };
   double X[] = { -0.123, 0.122, 0.981, 0.321, 0.942, 0.98 };
   int incX = -1;
   double x_expected[] = { 0.0490338308139, -0.158433417494, 0.261604043488, 1.28058846321, 1.77633350191, -1.07039599422 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1286) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1286) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.474, 0.715, 0.061, 0.532, 0.004, -0.318, 0.37, -0.692, -0.166, 0.039, -0.946, 0.857, -0.922, -0.491, 0.012, -0.217, -0.674, -0.429 };
   double X[] = { -0.123, 0.122, 0.981, 0.321, 0.942, 0.98 };
   int incX = -1;
   double x_expected[] = { -0.123, 0.122, 0.96534, 0.346049, 1.067212328, 0.445330131 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1287) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1287) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.474, 0.715, 0.061, 0.532, 0.004, -0.318, 0.37, -0.692, -0.166, 0.039, -0.946, 0.857, -0.922, -0.491, 0.012, -0.217, -0.674, -0.429 };
   double X[] = { -0.123, 0.122, 0.981, 0.321, 0.942, 0.98 };
   int incX = -1;
   double x_expected[] = { 72.7437666278, 10.4206532927, -4.34946941374, -14.8012581742, 2.01859491883, -1.53922125931 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1288) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1288) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.474, 0.715, 0.061, 0.532, 0.004, -0.318, 0.37, -0.692, -0.166, 0.039, -0.946, 0.857, -0.922, -0.491, 0.012, -0.217, -0.674, -0.429 };
   double X[] = { -0.123, 0.122, 0.981, 0.321, 0.942, 0.98 };
   int incX = -1;
   double x_expected[] = { -0.464775024, 0.662224708, -0.0457, 0.610264, 0.942, 0.98 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1289) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1289) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.474, 0.715, 0.061, 0.532, 0.004, -0.318, 0.37, -0.692, -0.166, 0.039, -0.946, 0.857, -0.922, -0.491, 0.012, -0.217, -0.674, -0.429 };
   double X[] = { -0.123, 0.122, 0.981, 0.321, 0.942, 0.98 };
   int incX = -1;
   double x_expected[] = { -0.591747295323, -0.534096923761, -4.60251824353, 1.70172936273, -4.94687072873, -3.32536493524 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1290) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1290) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.474, 0.715, 0.061, 0.532, 0.004, -0.318, 0.37, -0.692, -0.166, 0.039, -0.946, 0.857, -0.922, -0.491, 0.012, -0.217, -0.674, -0.429 };
   double X[] = { -0.123, 0.122, 0.981, 0.321, 0.942, 0.98 };
   int incX = -1;
   double x_expected[] = { -0.123, 0.122, 0.807692, 0.373091, 0.384974988, 1.400879194 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1291) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1291) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.474, 0.715, 0.061, 0.532, 0.004, -0.318, 0.37, -0.692, -0.166, 0.039, -0.946, 0.857, -0.922, -0.491, 0.012, -0.217, -0.674, -0.429 };
   double X[] = { -0.123, 0.122, 0.981, 0.321, 0.942, 0.98 };
   int incX = -1;
   double x_expected[] = { -0.129998778267, -0.116630230861, 0.993340886904, 0.530739563688, 1.55891621291, -0.284019181928 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1292) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1292) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.474, 0.715, 0.061, 0.532, 0.004, -0.318, 0.37, -0.692, -0.166, 0.039, -0.946, 0.857, -0.922, -0.491, 0.012, -0.217, -0.674, -0.429 };
   double X[] = { -0.123, 0.122, 0.981, 0.321, 0.942, 0.98 };
   int incX = -1;
   double x_expected[] = { 0.107496032, 0.025821594, 1.444898, -0.239924, 0.942, 0.98 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1293) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1293) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.872, -0.841, 0.108, -0.744, 0.231, -0.513, -0.973, 0.087, 0.348, 0.196, 0.447, 0.307, 0.632, -0.949, 0.322, 0.277, 0.282, 0.831 };
   double X[] = { -0.373, 0.566, 0.92, 0.627, 0.293, -0.434 };
   int incX = -1;
   double x_expected[] = { -0.825842176606, 0.212941473892, -0.548817434511, -0.703261551538, 0.0746069436827, 0.425751789407 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1294) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1294) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.872, -0.841, 0.108, -0.744, 0.231, -0.513, -0.973, 0.087, 0.348, 0.196, 0.447, 0.307, 0.632, -0.949, 0.322, 0.277, 0.282, 0.831 };
   double X[] = { -0.373, 0.566, 0.92, 0.627, 0.293, -0.434 };
   int incX = -1;
   double x_expected[] = { -0.619710352, 0.018225936, 1.211252, 0.891864, 0.293, -0.434 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1295) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1295) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.872, -0.841, 0.108, -0.744, 0.231, -0.513, -0.973, 0.087, 0.348, 0.196, 0.447, 0.307, 0.632, -0.949, 0.322, 0.277, 0.282, 0.831 };
   double X[] = { -0.373, 0.566, 0.92, 0.627, 0.293, -0.434 };
   int incX = -1;
   double x_expected[] = { 0.203289119964, 1.58288482537, -1.7720160159, 0.479463518178, -0.511241930019, -1.79333888299 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1296) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1296) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.872, -0.841, 0.108, -0.744, 0.231, -0.513, -0.973, 0.087, 0.348, 0.196, 0.447, 0.307, 0.632, -0.949, 0.322, 0.277, 0.282, 0.831 };
   double X[] = { -0.373, 0.566, 0.92, 0.627, 0.293, -0.434 };
   int incX = -1;
   double x_expected[] = { -0.373, 0.566, 0.618602, -0.084689, 0.887531803, -0.570220771 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1297) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1297) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.872, -0.841, 0.108, -0.744, 0.231, -0.513, -0.973, 0.087, 0.348, 0.196, 0.447, 0.307, 0.632, -0.949, 0.322, 0.277, 0.282, 0.831 };
   double X[] = { -0.373, 0.566, 0.92, 0.627, 0.293, -0.434 };
   int incX = -1;
   double x_expected[] = { 1.72799012007, 13.4612400765, 4.46126528205, -0.0212528722047, 0.627282377919, 0.302760084926 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1298) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1298) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.872, -0.841, 0.108, -0.744, 0.231, -0.513, -0.973, 0.087, 0.348, 0.196, 0.447, 0.307, 0.632, -0.949, 0.322, 0.277, 0.282, 0.831 };
   double X[] = { -0.373, 0.566, 0.92, 0.627, 0.293, -0.434 };
   int incX = -1;
   double x_expected[] = { -1.280839615, 1.560525655, 1.167331, 0.179227, 0.293, -0.434 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1299) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1299) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.872, -0.841, 0.108, -0.744, 0.231, -0.513, -0.973, 0.087, 0.348, 0.196, 0.447, 0.307, 0.632, -0.949, 0.322, 0.277, 0.282, 0.831 };
   double X[] = { -0.373, 0.566, 0.92, 0.627, 0.293, -0.434 };
   int incX = -1;
   double x_expected[] = { -0.594503951847, 0.00287302167266, -1.08185265666, -0.859860374254, 0.0331027077244, 1.28233265933 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1300) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1300) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { -0.872, -0.841, 0.108, -0.744, 0.231, -0.513, -0.973, 0.087, 0.348, 0.196, 0.447, 0.307, 0.632, -0.949, 0.322, 0.277, 0.282, 0.831 };
   double X[] = { -0.373, 0.566, 0.92, 0.627, 0.293, -0.434 };
   int incX = -1;
   double x_expected[] = { -0.373, 0.566, 1.16074, 0.50314, -0.20669608, 0.37525144 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1301) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1301) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.404, 0.667, 0.861, 0.22, 0.298, -0.858, -0.682, -0.969, 0.327, -0.86, 0.125, 0.606, -0.143, -0.865, -0.036, 0.23, -0.776, 0.079 };
   double X[] = { 0.028, -0.804, 0.582, -0.078, -0.126, 0.459 };
   int incX = -1;
   double x_expected[] = { 0.0654496252357, 0.224007771015, -0.752486084395, -0.554870892947, -0.587163401057, 0.166737652215 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1302) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1302) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.404, 0.667, 0.861, 0.22, 0.298, -0.858, -0.682, -0.969, 0.327, -0.86, 0.125, 0.606, -0.143, -0.865, -0.036, 0.23, -0.776, 0.079 };
   double X[] = { 0.028, -0.804, 0.582, -0.078, -0.126, 0.459 };
   int incX = -1;
   double x_expected[] = { -0.595558802, -1.147174647, 0.589506, -0.500919, -0.126, 0.459 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1303) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1303) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.404, 0.667, 0.861, 0.22, 0.298, -0.858, -0.682, -0.969, 0.327, -0.86, 0.125, 0.606, -0.143, -0.865, -0.036, 0.23, -0.776, 0.079 };
   double X[] = { 0.028, -0.804, 0.582, -0.078, -0.126, 0.459 };
   int incX = -1;
   double x_expected[] = { 3.39346077201, 0.652889512141, -2.33602680355, -2.7859245153, -5.04672104102, -0.334110541026 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1304) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1304) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.404, 0.667, 0.861, 0.22, 0.298, -0.858, -0.682, -0.969, 0.327, -0.86, 0.125, 0.606, -0.143, -0.865, -0.036, 0.23, -0.776, 0.079 };
   double X[] = { 0.028, -0.804, 0.582, -0.078, -0.126, 0.459 };
   int incX = -1;
   double x_expected[] = { 0.028, -0.804, -0.109456, -0.217192, -0.41110804, 0.41693792 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1305) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1305) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.404, 0.667, 0.861, 0.22, 0.298, -0.858, -0.682, -0.969, 0.327, -0.86, 0.125, 0.606, -0.143, -0.865, -0.036, 0.23, -0.776, 0.079 };
   double X[] = { 0.028, -0.804, 0.582, -0.078, -0.126, 0.459 };
   int incX = -1;
   double x_expected[] = { 7.16970224467, -0.772071373678, 0.833386981173, -0.673826630129, -0.26524050899, 0.465327628365 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1306) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1306) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.404, 0.667, 0.861, 0.22, 0.298, -0.858, -0.682, -0.969, 0.327, -0.86, 0.125, 0.606, -0.143, -0.865, -0.036, 0.23, -0.776, 0.079 };
   double X[] = { 0.028, -0.804, 0.582, -0.078, -0.126, 0.459 };
   int incX = -1;
   double x_expected[] = { 0.471459157, -1.566755859, 0.940839, 0.357132, -0.126, 0.459 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1307) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1307) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.404, 0.667, 0.861, 0.22, 0.298, -0.858, -0.682, -0.969, 0.327, -0.86, 0.125, 0.606, -0.143, -0.865, -0.036, 0.23, -0.776, 0.079 };
   double X[] = { 0.028, -0.804, 0.582, -0.078, -0.126, 0.459 };
   int incX = -1;
   double x_expected[] = { -0.909961830373, 0.118063054039, -0.0169425582229, -1.00055409731, -1.37205489923, 0.994032418785 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1308) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1308) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 3;
   int K = 1;
   int lda = 3;
   double A[] = { 0.404, 0.667, 0.861, 0.22, 0.298, -0.858, -0.682, -0.969, 0.327, -0.86, 0.125, 0.606, -0.143, -0.865, -0.036, 0.23, -0.776, 0.079 };
   double X[] = { 0.028, -0.804, 0.582, -0.078, -0.126, 0.459 };
   int incX = -1;
   double x_expected[] = { 0.028, -0.804, -0.118596, 0.160828, -0.059271004, 0.294435972 };
   cblas_ztbsv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbsv(case 1309) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbsv(case 1309) imag");
     };
   };
  };


}
