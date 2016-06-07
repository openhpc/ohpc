#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_herk (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int uplo = 121;
   int trans = 111;
   int N = 2;
   int K = 1;
   float alpha = 0.0f;
   float beta = 0.1f;
   float A[] = { -0.617f, 0.179f, -0.626f, 0.334f };
   int lda = 1;
   float C[] = { 0.346f, -0.903f, 0.022f, -0.839f, -0.715f, 0.049f, -0.338f, 0.149f };
   int ldc = 2;
   float C_expected[] = { 0.0346f, 0.0f, 0.0022f, -0.0839f, -0.715f, 0.049f, -0.0338f, 0.0f };
   cblas_cherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cherk(case 1598) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cherk(case 1598) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 111;
   int N = 2;
   int K = 1;
   float alpha = 0.0f;
   float beta = 0.1f;
   float A[] = { -0.356f, -0.308f, 0.493f, -0.351f };
   int lda = 2;
   float C[] = { -0.898f, -0.905f, 0.002f, -0.219f, 0.881f, 0.879f, 0.275f, -0.351f };
   int ldc = 2;
   float C_expected[] = { -0.0898f, 0.0f, 0.002f, -0.219f, 0.0881f, 0.0879f, 0.0275f, 0.0f };
   cblas_cherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cherk(case 1599) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cherk(case 1599) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 113;
   int N = 2;
   int K = 1;
   float alpha = 0.1f;
   float beta = 1.0f;
   float A[] = { -0.103f, -0.951f, -0.601f, -0.041f };
   int lda = 2;
   float C[] = { -0.918f, -0.018f, 0.991f, -0.789f, -0.698f, -0.067f, 0.956f, -0.599f };
   int ldc = 2;
   float C_expected[] = { -0.826499f, 0.0f, 1.00109f, -0.845733f, -0.698f, -0.067f, 0.992288f, 0.0f };
   cblas_cherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cherk(case 1600) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cherk(case 1600) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 113;
   int N = 2;
   int K = 1;
   float alpha = 0.1f;
   float beta = 1.0f;
   float A[] = { -0.237f, 0.925f, -0.904f, -0.091f };
   int lda = 1;
   float C[] = { -0.572f, 0.915f, 0.398f, 0.222f, 0.016f, 0.288f, -0.078f, -0.507f };
   int ldc = 2;
   float C_expected[] = { -0.480821f, 0.0f, 0.398f, 0.222f, 0.0290073f, 0.373777f, 0.0045497f, 0.0f };
   cblas_cherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cherk(case 1601) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cherk(case 1601) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 111;
   int N = 2;
   int K = 1;
   float alpha = -0.3f;
   float beta = 0.0f;
   float A[] = { 0.963f, -0.23f, -0.435f, 0.289f };
   int lda = 1;
   float C[] = { 0.282f, -0.272f, -0.516f, -0.594f, -0.001f, 0.155f, -0.39f, -0.354f };
   int ldc = 2;
   float C_expected[] = { -0.294081f, 0.0f, -0.516f, -0.594f, 0.145613f, -0.0534771f, -0.0818238f, 0.0f };
   cblas_cherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cherk(case 1602) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cherk(case 1602) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 111;
   int N = 2;
   int K = 1;
   float alpha = -0.3f;
   float beta = 0.0f;
   float A[] = { 0.674f, 0.1f, -0.098f, 0.552f };
   int lda = 2;
   float C[] = { 0.089f, -0.523f, -0.551f, 0.618f, 0.67f, 0.247f, 0.975f, -0.714f };
   int ldc = 2;
   float C_expected[] = { -0.139283f, 0.0f, 0.0032556f, -0.114554f, 0.67f, 0.247f, -0.0942924f, 0.0f };
   cblas_cherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cherk(case 1603) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cherk(case 1603) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 113;
   int N = 2;
   int K = 1;
   float alpha = 1.0f;
   float beta = 0.1f;
   float A[] = { 0.033f, -0.864f, 0.168f, 0.524f };
   int lda = 2;
   float C[] = { 0.788f, 0.016f, -0.436f, 0.749f, -0.89f, -0.87f, 0.421f, -0.203f };
   int ldc = 2;
   float C_expected[] = { 0.826385f, 0.0f, -0.436f, 0.749f, -0.536192f, -0.249444f, 0.3449f, 0.0f };
   cblas_cherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cherk(case 1604) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cherk(case 1604) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 113;
   int N = 2;
   int K = 1;
   float alpha = 1.0f;
   float beta = 0.1f;
   float A[] = { 0.957f, -0.079f, 0.935f, 0.232f };
   int lda = 1;
   float C[] = { -0.744f, -0.061f, 0.195f, -0.574f, 0.551f, 0.478f, -0.337f, 0.1f };
   int ldc = 2;
   float C_expected[] = { 0.84769f, 0.0f, 0.895967f, -0.353289f, 0.551f, 0.478f, 0.894349f, 0.0f };
   cblas_cherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], flteps, "cherk(case 1605) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], flteps, "cherk(case 1605) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 111;
   int N = 2;
   int K = 1;
   double alpha = 1;
   double beta = 1;
   double A[] = { 0.934, 0.664, 0.426, 0.263 };
   int lda = 1;
   double C[] = { 0.251, -0.97, 0.76, -0.349, 0.152, -0.899, -0.17, 0.707 };
   int ldc = 2;
   double C_expected[] = { 1.564252, 0.0, 1.332516, -0.311778, 0.152, -0.899, 0.080645, 0.0 };
   cblas_zherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zherk(case 1606) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zherk(case 1606) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 111;
   int N = 2;
   int K = 1;
   double alpha = 1;
   double beta = 1;
   double A[] = { 0.16, 0.464, -0.623, 0.776 };
   int lda = 2;
   double C[] = { 0.771, -0.449, 0.776, 0.112, -0.134, 0.317, 0.547, -0.551 };
   int ldc = 2;
   double C_expected[] = { 1.011896, 0.0, 0.776, 0.112, 0.126384, -0.096232, 1.537305, 0.0 };
   cblas_zherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zherk(case 1607) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zherk(case 1607) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 121;
   int trans = 113;
   int N = 2;
   int K = 1;
   double alpha = 0.1;
   double beta = 1;
   double A[] = { 0.787, 0.057, -0.49, 0.47 };
   int lda = 2;
   double C[] = { -0.758, 0.912, 0.992, -0.356, 0.584, 0.806, 0.965, 0.674 };
   int ldc = 2;
   double C_expected[] = { -0.6957382, 0.0, 0.956116, -0.316218, 0.584, 0.806, 1.0111, 0.0 };
   cblas_zherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zherk(case 1608) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zherk(case 1608) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 121;
   int trans = 113;
   int N = 2;
   int K = 1;
   double alpha = 0.1;
   double beta = 1;
   double A[] = { 0.961, -0.384, 0.165, 0.395 };
   int lda = 1;
   double C[] = { -0.186, 0.404, -0.873, 0.09, -0.451, -0.972, -0.203, -0.304 };
   int ldc = 2;
   double C_expected[] = { -0.0789023, 0.0, -0.873, 0.09, -0.4503115, -0.9277045, -0.184675, 0.0 };
   cblas_zherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zherk(case 1609) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zherk(case 1609) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 111;
   int N = 2;
   int K = 1;
   double alpha = 0;
   double beta = -0.3;
   double A[] = { 0.04, 0.608, 0.21, -0.44 };
   int lda = 1;
   double C[] = { 0.285, -0.943, 0.581, -0.56, 0.112, 0.529, 0.16, -0.913 };
   int ldc = 2;
   double C_expected[] = { -0.0855, 0.0, 0.581, -0.56, -0.0336, -0.1587, -0.048, 0.0 };
   cblas_zherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zherk(case 1610) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zherk(case 1610) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 111;
   int N = 2;
   int K = 1;
   double alpha = 0;
   double beta = -0.3;
   double A[] = { -0.984, -0.398, -0.379, 0.919 };
   int lda = 2;
   double C[] = { -0.44, -0.087, 0.156, -0.945, -0.943, -0.355, 0.577, 0.053 };
   int ldc = 2;
   double C_expected[] = { 0.132, 0.0, -0.0468, 0.2835, -0.943, -0.355, -0.1731, 0.0 };
   cblas_zherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zherk(case 1611) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zherk(case 1611) imag");
     };
   };
  };


  {
   int order = 101;
   int uplo = 122;
   int trans = 113;
   int N = 2;
   int K = 1;
   double alpha = 1;
   double beta = -1;
   double A[] = { 0.269, -0.428, -0.029, 0.964 };
   int lda = 2;
   double C[] = { 0.473, -0.932, -0.689, -0.072, -0.952, -0.862, 0.001, 0.282 };
   int ldc = 2;
   double C_expected[] = { -0.217455, 0.0, -0.689, -0.072, 0.531607, 0.615096, 0.929137, 0.0 };
   cblas_zherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zherk(case 1612) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zherk(case 1612) imag");
     };
   };
  };


  {
   int order = 102;
   int uplo = 122;
   int trans = 113;
   int N = 2;
   int K = 1;
   double alpha = 1;
   double beta = -1;
   double A[] = { -0.303, -0.037, -0.411, -0.243 };
   int lda = 1;
   double C[] = { 0.652, -0.227, -0.849, 0.87, -0.051, -0.535, 0.418, -0.681 };
   int ldc = 2;
   double C_expected[] = { -0.558822, 0.0, 0.982524, -0.928422, -0.051, -0.535, -0.19003, 0.0 };
   cblas_zherk(order, uplo, trans, N, K, alpha, A, lda, beta, C, ldc);
   {
     int i;
     for (i = 0; i < 4; i++) {
       gsl_test_rel(C[2*i], C_expected[2*i], dbleps, "zherk(case 1613) real");
       gsl_test_rel(C[2*i+1], C_expected[2*i+1], dbleps, "zherk(case 1613) imag");
     };
   };
  };


}
