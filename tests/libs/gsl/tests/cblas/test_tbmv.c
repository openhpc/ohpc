#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_tbmv (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 3;
   int K = 1;
   int lda = 3;
   float A[] = { 0.439f, -0.484f, -0.952f, -0.508f, 0.381f, -0.889f, -0.192f, -0.279f, -0.155f };
   float X[] = { -0.089f, -0.688f, -0.203f };
   int incX = -1;
   float x_expected[] = { 0.017088f, 0.315595f, 0.243875f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 894)");
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
   float A[] = { 0.439f, -0.484f, -0.952f, -0.508f, 0.381f, -0.889f, -0.192f, -0.279f, -0.155f };
   float X[] = { -0.089f, -0.688f, -0.203f };
   int incX = -1;
   float x_expected[] = { -0.089f, -0.721909f, 0.129992f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 895)");
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
   float A[] = { 0.439f, -0.484f, -0.952f, -0.508f, 0.381f, -0.889f, -0.192f, -0.279f, -0.155f };
   float X[] = { -0.089f, -0.688f, -0.203f };
   int incX = -1;
   float x_expected[] = { 0.156927f, -0.159004f, 0.098252f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 896)");
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
   float A[] = { 0.439f, -0.484f, -0.952f, -0.508f, 0.381f, -0.889f, -0.192f, -0.279f, -0.155f };
   float X[] = { -0.089f, -0.688f, -0.203f };
   int incX = -1;
   float x_expected[] = { 0.043096f, -0.584876f, -0.203f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 897)");
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
   float A[] = { 0.439f, -0.484f, -0.952f, -0.508f, 0.381f, -0.889f, -0.192f, -0.279f, -0.155f };
   float X[] = { -0.089f, -0.688f, -0.203f };
   int incX = -1;
   float x_expected[] = { 0.024831f, -0.24504f, 0.447756f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 898)");
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
   float A[] = { 0.439f, -0.484f, -0.952f, -0.508f, 0.381f, -0.889f, -0.192f, -0.279f, -0.155f };
   float X[] = { -0.089f, -0.688f, -0.203f };
   int incX = -1;
   float x_expected[] = { -0.089f, -0.670912f, 0.146504f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 899)");
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
   float A[] = { 0.439f, -0.484f, -0.952f, -0.508f, 0.381f, -0.889f, -0.192f, -0.279f, -0.155f };
   float X[] = { -0.089f, -0.688f, -0.203f };
   int incX = -1;
   float x_expected[] = { -0.24504f, 0.447756f, -0.089117f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 900)");
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
   float A[] = { 0.439f, -0.484f, -0.952f, -0.508f, 0.381f, -0.889f, -0.192f, -0.279f, -0.155f };
   float X[] = { -0.089f, -0.688f, -0.203f };
   int incX = -1;
   float x_expected[] = { -0.351128f, -0.589748f, -0.203f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 901)");
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
   float A[] = { 0.94f, -0.091f, 0.984f, -0.276f, -0.342f, -0.484f, -0.665f, -0.2f, 0.349f };
   float X[] = { 0.023f, -0.501f, -0.562f };
   int incX = -1;
   float x_expected[] = { 0.156047f, 0.189418f, -0.52828f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 902)");
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
   float A[] = { 0.94f, -0.091f, 0.984f, -0.276f, -0.342f, -0.484f, -0.665f, -0.2f, 0.349f };
   float X[] = { 0.023f, -0.501f, -0.562f };
   int incX = -1;
   float x_expected[] = { 0.194342f, -0.449858f, -0.562f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 903)");
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
   float A[] = { 0.94f, -0.091f, 0.984f, -0.276f, -0.342f, -0.484f, -0.665f, -0.2f, 0.349f };
   float X[] = { 0.023f, -0.501f, -0.562f };
   int incX = -1;
   float x_expected[] = { -0.0046f, 0.156047f, 0.189418f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 904)");
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
   float A[] = { 0.94f, -0.091f, 0.984f, -0.276f, -0.342f, -0.484f, -0.665f, -0.2f, 0.349f };
   float X[] = { 0.023f, -0.501f, -0.562f };
   int incX = -1;
   float x_expected[] = { 0.023f, -0.516295f, -0.423724f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 905)");
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
   float A[] = { 0.94f, -0.091f, 0.984f, -0.276f, -0.342f, -0.484f, -0.665f, -0.2f, 0.349f };
   float X[] = { 0.023f, -0.501f, -0.562f };
   int incX = -1;
   float x_expected[] = { 0.328565f, 0.326454f, 0.051142f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 906)");
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
   float A[] = { 0.94f, -0.091f, 0.984f, -0.276f, -0.342f, -0.484f, -0.665f, -0.2f, 0.349f };
   float X[] = { 0.023f, -0.501f, -0.562f };
   int incX = -1;
   float x_expected[] = { 0.356165f, -0.345888f, -0.562f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 907)");
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
   float A[] = { 0.94f, -0.091f, 0.984f, -0.276f, -0.342f, -0.484f, -0.665f, -0.2f, 0.349f };
   float X[] = { 0.023f, -0.501f, -0.562f };
   int incX = -1;
   float x_expected[] = { -0.015295f, 0.13041f, -0.482689f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 908)");
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
   float A[] = { 0.94f, -0.091f, 0.984f, -0.276f, -0.342f, -0.484f, -0.665f, -0.2f, 0.349f };
   float X[] = { 0.023f, -0.501f, -0.562f };
   int incX = -1;
   float x_expected[] = { 0.023f, -0.508866f, -0.516409f };
   cblas_stbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stbmv(case 909)");
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
   double A[] = { 0.566, 0.955, -0.086, -0.856, 0.177, 0.974, -0.652, -0.884, 0.77 };
   double X[] = { -0.77, -0.818, 0.337 };
   int incX = -1;
   double x_expected[] = { 0.50204, 0.563918, -0.590448 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 910)");
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
   double A[] = { 0.566, 0.955, -0.086, -0.856, 0.177, 0.974, -0.652, -0.884, 0.77 };
   double X[] = { -0.77, -0.818, 0.337 };
   int incX = -1;
   double x_expected[] = { -0.77, -0.95429, -0.44419 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 911)");
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
   double A[] = { 0.566, 0.955, -0.086, -0.856, 0.177, 0.974, -0.652, -0.884, 0.77 };
   double X[] = { -0.77, -0.818, 0.337 };
   int incX = -1;
   double x_expected[] = { 1.214016, -0.433258, 0.321835 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 912)");
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
   double A[] = { 0.566, 0.955, -0.086, -0.856, 0.177, 0.974, -0.652, -0.884, 0.77 };
   double X[] = { -0.77, -0.818, 0.337 };
   int incX = -1;
   double x_expected[] = { -0.236664, -1.106472, 0.337 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 913)");
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
   double A[] = { 0.566, 0.955, -0.086, -0.856, 0.177, 0.974, -0.652, -0.884, 0.77 };
   double X[] = { -0.77, -0.818, 0.337 };
   int incX = -1;
   double x_expected[] = { 0.68068, 0.357254, 1.022043 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 914)");
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
   double A[] = { 0.566, 0.955, -0.086, -0.856, 0.177, 0.974, -0.652, -0.884, 0.77 };
   double X[] = { -0.77, -0.818, 0.337 };
   int incX = -1;
   double x_expected[] = { -0.77, -0.31596, 1.037208 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 915)");
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
   double A[] = { 0.566, 0.955, -0.086, -0.856, 0.177, 0.974, -0.652, -0.884, 0.77 };
   double X[] = { -0.77, -0.818, 0.337 };
   int incX = -1;
   double x_expected[] = { 0.357254, 1.022043, 0.190742 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 916)");
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
   double A[] = { 0.566, 0.955, -0.086, -0.856, 0.177, 0.974, -0.652, -0.884, 0.77 };
   double X[] = { -0.77, -0.818, 0.337 };
   int incX = -1;
   double x_expected[] = { -0.914786, -0.496165, 0.337 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 917)");
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
   double A[] = { 0.188, 0.6, -0.743, -0.803, 0.449, -0.681, -0.464, -0.029, 0.553 };
   double X[] = { -0.851, 0.481, 0.155 };
   int incX = -1;
   double x_expected[] = { 0.610833, -0.293243, 0.02914 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 918)");
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
   double A[] = { 0.188, 0.6, -0.743, -0.803, 0.449, -0.681, -0.464, -0.029, 0.553 };
   double X[] = { -0.851, 0.481, 0.155 };
   int incX = -1;
   double x_expected[] = { -0.635031, 0.574, 0.155 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 919)");
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
   double A[] = { 0.188, 0.6, -0.743, -0.803, 0.449, -0.681, -0.464, -0.029, 0.553 };
   double X[] = { -0.851, 0.481, 0.155 };
   int incX = -1;
   double x_expected[] = { 0.024679, 0.610833, -0.293243 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 920)");
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
   double A[] = { 0.188, 0.6, -0.743, -0.803, 0.449, -0.681, -0.464, -0.029, 0.553 };
   double X[] = { -0.851, 0.481, 0.155 };
   int incX = -1;
   double x_expected[] = { -0.851, 0.875864, -0.231243 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 921)");
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
   double A[] = { 0.188, 0.6, -0.743, -0.803, 0.449, -0.681, -0.464, -0.029, 0.553 };
   double X[] = { -0.851, 0.481, 0.155 };
   int incX = -1;
   double x_expected[] = { -0.198505, 0.091504, 0.093 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 922)");
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
   double A[] = { 0.188, 0.6, -0.743, -0.803, 0.449, -0.681, -0.464, -0.029, 0.553 };
   double X[] = { -0.851, 0.481, 0.155 };
   int incX = -1;
   double x_expected[] = { -1.074184, 0.356535, 0.155 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 923)");
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
   double A[] = { 0.188, 0.6, -0.743, -0.803, 0.449, -0.681, -0.464, -0.029, 0.553 };
   double X[] = { -0.851, 0.481, 0.155 };
   int incX = -1;
   double x_expected[] = { 0.394864, -0.768342, 0.31774 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 924)");
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
   double A[] = { 0.188, 0.6, -0.743, -0.803, 0.449, -0.681, -0.464, -0.029, 0.553 };
   double X[] = { -0.851, 0.481, 0.155 };
   int incX = -1;
   double x_expected[] = { -0.851, 0.098901, 0.4436 };
   cblas_dtbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtbmv(case 925)");
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
   float A[] = { 0.824f, -0.45f, -0.987f, 0.758f, 0.42f, -0.357f, 0.147f, -0.191f, 0.88f, 0.63f, 0.155f, -0.573f, 0.224f, 0.146f, 0.501f, -0.889f, 0.456f, 0.796f };
   float X[] = { -0.46f, 0.069f, 0.308f, -0.003f, -0.236f, 0.605f };
   int incX = -1;
   float x_expected[] = { -0.113114f, -0.051704f, -0.403567f, -0.288349f, -0.223936f, 0.841145f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 926) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 926) imag");
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
   float A[] = { 0.824f, -0.45f, -0.987f, 0.758f, 0.42f, -0.357f, 0.147f, -0.191f, 0.88f, 0.63f, 0.155f, -0.573f, 0.224f, 0.146f, 0.501f, -0.889f, 0.456f, 0.796f };
   float X[] = { -0.46f, 0.069f, 0.308f, -0.003f, -0.236f, 0.605f };
   int incX = -1;
   float x_expected[] = { -0.46f, 0.069f, -0.14027f, -0.23208f, -0.537722f, 0.841425f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 927) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 927) imag");
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
   float A[] = { 0.824f, -0.45f, -0.987f, 0.758f, 0.42f, -0.357f, 0.147f, -0.191f, 0.88f, 0.63f, 0.155f, -0.573f, 0.224f, 0.146f, 0.501f, -0.889f, 0.456f, 0.796f };
   float X[] = { -0.46f, 0.069f, 0.308f, -0.003f, -0.236f, 0.605f };
   int incX = -1;
   float x_expected[] = { -0.099689f, 0.487805f, 0.353793f, 0.325411f, -0.225658f, -0.776023f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 928) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 928) imag");
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
   float A[] = { 0.824f, -0.45f, -0.987f, 0.758f, 0.42f, -0.357f, 0.147f, -0.191f, 0.88f, 0.63f, 0.155f, -0.573f, 0.224f, 0.146f, 0.501f, -0.889f, 0.456f, 0.796f };
   float X[] = { -0.46f, 0.069f, 0.308f, -0.003f, -0.236f, 0.605f };
   int incX = -1;
   float x_expected[] = { -0.39057f, 0.113296f, 0.388863f, 0.131011f, -0.236f, 0.605f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 929) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 929) imag");
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
   float A[] = { 0.824f, -0.45f, -0.987f, 0.758f, 0.42f, -0.357f, 0.147f, -0.191f, 0.88f, 0.63f, 0.155f, -0.573f, 0.224f, 0.146f, 0.501f, -0.889f, 0.456f, 0.796f };
   float X[] = { -0.46f, 0.069f, 0.308f, -0.003f, -0.236f, 0.605f };
   int incX = -1;
   float x_expected[] = { -0.169119f, 0.443509f, 0.159816f, 0.139696f, -0.180955f, -0.835292f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 930) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 930) imag");
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
   float A[] = { 0.824f, -0.45f, -0.987f, 0.758f, 0.42f, -0.357f, 0.147f, -0.191f, 0.88f, 0.63f, 0.155f, -0.573f, 0.224f, 0.146f, 0.501f, -0.889f, 0.456f, 0.796f };
   float X[] = { -0.46f, 0.069f, 0.308f, -0.003f, -0.236f, 0.605f };
   int incX = -1;
   float x_expected[] = { -0.46f, 0.069f, 0.194886f, -0.054704f, -0.191297f, 0.545731f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 931) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 931) imag");
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
   float A[] = { 0.824f, -0.45f, -0.987f, 0.758f, 0.42f, -0.357f, 0.147f, -0.191f, 0.88f, 0.63f, 0.155f, -0.573f, 0.224f, 0.146f, 0.501f, -0.889f, 0.456f, 0.796f };
   float X[] = { -0.46f, 0.069f, 0.308f, -0.003f, -0.236f, 0.605f };
   int incX = -1;
   float x_expected[] = { 0.159816f, 0.139696f, -0.180955f, -0.835292f, 0.077786f, 0.60472f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 932) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 932) imag");
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
   float A[] = { 0.824f, -0.45f, -0.987f, 0.758f, 0.42f, -0.357f, 0.147f, -0.191f, 0.88f, 0.63f, 0.155f, -0.573f, 0.224f, 0.146f, 0.501f, -0.889f, 0.456f, 0.796f };
   float X[] = { -0.46f, 0.069f, 0.308f, -0.003f, -0.236f, 0.605f };
   int incX = -1;
   float x_expected[] = { -0.18707f, 0.2604f, 0.082342f, -0.779023f, -0.236f, 0.605f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 933) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 933) imag");
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
   float A[] = { -0.814f, 0.043f, -0.755f, -0.094f, 0.876f, 0.257f, 0.406f, 0.491f, -0.27f, -0.787f, 0.545f, 0.732f, -0.512f, -0.085f, 0.234f, 0.001f, -0.225f, -0.002f };
   float X[] = { 0.411f, -0.308f, -0.912f, 0.811f, -0.894f, -0.116f };
   int incX = -1;
   float x_expected[] = { 0.647885f, 0.621535f, -0.104407f, 0.05309f, 0.732704f, 0.055982f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 934) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 934) imag");
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
   float A[] = { -0.814f, 0.043f, -0.755f, -0.094f, 0.876f, 0.257f, 0.406f, 0.491f, -0.27f, -0.787f, 0.545f, 0.732f, -0.512f, -0.085f, 0.234f, 0.001f, -0.225f, -0.002f };
   float X[] = { 0.411f, -0.308f, -0.912f, 0.811f, -0.894f, -0.116f };
   int incX = -1;
   float x_expected[] = { 1.2955f, 0.190774f, -0.247934f, 0.982616f, -0.894f, -0.116f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 935) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 935) imag");
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
   float A[] = { -0.814f, 0.043f, -0.755f, -0.094f, 0.876f, 0.257f, 0.406f, 0.491f, -0.27f, -0.787f, 0.545f, 0.732f, -0.512f, -0.085f, 0.234f, 0.001f, -0.225f, -0.002f };
   float X[] = { 0.411f, -0.308f, -0.912f, 0.811f, -0.894f, -0.116f };
   int incX = -1;
   float x_expected[] = { 0.096482f, -0.071661f, 0.647885f, 0.621535f, -0.104407f, 0.05309f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 936) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 936) imag");
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
   float A[] = { -0.814f, 0.043f, -0.755f, -0.094f, 0.876f, 0.257f, 0.406f, 0.491f, -0.27f, -0.787f, 0.545f, 0.732f, -0.512f, -0.085f, 0.234f, 0.001f, -0.225f, -0.002f };
   float X[] = { 0.411f, -0.308f, -0.912f, 0.811f, -0.894f, -0.116f };
   int incX = -1;
   float x_expected[] = { 0.411f, -0.308f, -1.14861f, 0.933761f, -1.66247f, -0.234526f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 937) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 937) imag");
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
   float A[] = { -0.814f, 0.043f, -0.755f, -0.094f, 0.876f, 0.257f, 0.406f, 0.491f, -0.27f, -0.787f, 0.545f, 0.732f, -0.512f, -0.085f, 0.234f, 0.001f, -0.225f, -0.002f };
   float X[] = { 0.411f, -0.308f, -0.912f, 0.811f, -0.894f, -0.116f };
   int incX = -1;
   float x_expected[] = { 0.632361f, -0.409373f, 0.578489f, 0.012724f, 0.664066f, 0.171616f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 938) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 938) imag");
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
   float A[] = { -0.814f, 0.043f, -0.755f, -0.094f, 0.876f, 0.257f, 0.406f, 0.491f, -0.27f, -0.787f, 0.545f, 0.732f, -0.512f, -0.085f, 0.234f, 0.001f, -0.225f, -0.002f };
   float X[] = { 0.411f, -0.308f, -0.912f, 0.811f, -0.894f, -0.116f };
   int incX = -1;
   float x_expected[] = { 0.946879f, -0.645712f, -1.21801f, 0.32495f, -0.894f, -0.116f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 939) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 939) imag");
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
   float A[] = { -0.814f, 0.043f, -0.755f, -0.094f, 0.876f, 0.257f, 0.406f, 0.491f, -0.27f, -0.787f, 0.545f, 0.732f, -0.512f, -0.085f, 0.234f, 0.001f, -0.225f, -0.002f };
   float X[] = { 0.411f, -0.308f, -0.912f, 0.811f, -0.894f, -0.116f };
   int incX = -1;
   float x_expected[] = { -0.236612f, 0.122761f, -1.12184f, -0.358823f, 1.4975f, -0.470595f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 940) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 940) imag");
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
   float A[] = { -0.814f, 0.043f, -0.755f, -0.094f, 0.876f, 0.257f, 0.406f, 0.491f, -0.27f, -0.787f, 0.545f, 0.732f, -0.512f, -0.085f, 0.234f, 0.001f, -0.225f, -0.002f };
   float X[] = { 0.411f, -0.308f, -0.912f, 0.811f, -0.894f, -0.116f };
   int incX = -1;
   float x_expected[] = { 0.411f, -0.308f, -1.26537f, 0.570703f, -0.129206f, -0.642577f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 941) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 941) imag");
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
   float A[] = { -0.675f, 0.047f, 0.695f, 0.724f, -0.438f, 0.991f, -0.188f, -0.06f, -0.093f, 0.302f, 0.842f, -0.753f, 0.465f, -0.972f, -0.058f, 0.988f, 0.093f, 0.164f };
   float X[] = { 0.065f, -0.082f, -0.746f, 0.775f, 0.694f, -0.954f };
   int incX = -1;
   float x_expected[] = { 0.413357f, 0.178267f, -0.114618f, -1.35595f, -0.513288f, 0.611332f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 942) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 942) imag");
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
   float A[] = { -0.675f, 0.047f, 0.695f, 0.724f, -0.438f, 0.991f, -0.188f, -0.06f, -0.093f, 0.302f, 0.842f, -0.753f, 0.465f, -0.972f, -0.058f, 0.988f, 0.093f, 0.164f };
   float X[] = { 0.065f, -0.082f, -0.746f, 0.775f, 0.694f, -0.954f };
   int incX = -1;
   float x_expected[] = { 0.368428f, 0.071217f, -0.954366f, -0.390486f, 0.694f, -0.954f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 943) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 943) imag");
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
   float A[] = { -0.675f, 0.047f, 0.695f, 0.724f, -0.438f, 0.991f, -0.188f, -0.06f, -0.093f, 0.302f, 0.842f, -0.753f, 0.465f, -0.972f, -0.058f, 0.988f, 0.093f, 0.164f };
   float X[] = { 0.065f, -0.082f, -0.746f, 0.775f, 0.694f, -0.954f };
   int incX = -1;
   float x_expected[] = { -0.084786f, -0.059464f, 0.413357f, 0.178267f, -0.114618f, -1.35595f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 944) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 944) imag");
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
   float A[] = { -0.675f, 0.047f, 0.695f, 0.724f, -0.438f, 0.991f, -0.188f, -0.06f, -0.093f, 0.302f, 0.842f, -0.753f, 0.465f, -0.972f, -0.058f, 0.988f, 0.093f, 0.164f };
   float X[] = { 0.065f, -0.082f, -0.746f, 0.775f, 0.694f, -0.954f };
   int incX = -1;
   float x_expected[] = { 0.065f, -0.082f, -0.636071f, 0.80005f, 0.787748f, -1.14446f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 945) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 945) imag");
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
   float A[] = { -0.675f, 0.047f, 0.695f, 0.724f, -0.438f, 0.991f, -0.188f, -0.06f, -0.093f, 0.302f, 0.842f, -0.753f, 0.465f, -0.972f, -0.058f, 0.988f, 0.093f, 0.164f };
   float X[] = { 0.065f, -0.082f, -0.746f, 0.775f, 0.694f, -0.954f };
   int incX = -1;
   float x_expected[] = { -1.18498f, -0.424201f, 0.230196f, 0.374209f, -0.208366f, -1.16549f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 946) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 946) imag");
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
   float A[] = { -0.675f, 0.047f, 0.695f, 0.724f, -0.438f, 0.991f, -0.188f, -0.06f, -0.093f, 0.302f, 0.842f, -0.753f, 0.465f, -0.972f, -0.058f, 0.988f, 0.093f, 0.164f };
   float X[] = { 0.065f, -0.082f, -0.746f, 0.775f, 0.694f, -0.954f };
   int incX = -1;
   float x_expected[] = { -1.03519f, -0.446737f, -0.819232f, 0.995992f, 0.694f, -0.954f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 947) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 947) imag");
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
   float A[] = { -0.675f, 0.047f, 0.695f, 0.724f, -0.438f, 0.991f, -0.188f, -0.06f, -0.093f, 0.302f, 0.842f, -0.753f, 0.465f, -0.972f, -0.058f, 0.988f, 0.093f, 0.164f };
   float X[] = { 0.065f, -0.082f, -0.746f, 0.775f, 0.694f, -0.954f };
   int incX = -1;
   float x_expected[] = { 0.109929f, 0.02505f, 0.062939f, -0.202464f, -0.470658f, 1.69006f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 948) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 948) imag");
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
   float A[] = { -0.675f, 0.047f, 0.695f, 0.724f, -0.438f, 0.991f, -0.188f, -0.06f, -0.093f, 0.302f, 0.842f, -0.753f, 0.465f, -0.972f, -0.058f, 0.988f, 0.093f, 0.164f };
   float X[] = { 0.065f, -0.082f, -0.746f, 0.775f, 0.694f, -0.954f };
   int incX = -1;
   float x_expected[] = { 0.065f, -0.082f, -0.776809f, 0.762996f, 0.73663f, 0.124729f };
   cblas_ctbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctbmv(case 949) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctbmv(case 949) imag");
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
   double A[] = { -0.212, 0.612, 0.189, -0.046, -0.124, 0.82, 0.753, 0.727, 0.331, 0.116, 0.504, -0.673, -0.888, -0.277, -0.361, -0.909, 0.982, -0.124 };
   double X[] = { 0.064, 0.169, -0.81, -0.779, -0.641, -0.103 };
   int incX = -1;
   double x_expected[] = { -0.010019, -0.1678, -0.042017, -1.112094, 0.010004, -0.480427 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 950) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 950) imag");
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
   double A[] = { -0.212, 0.612, 0.189, -0.046, -0.124, 0.82, 0.753, 0.727, 0.331, 0.116, 0.504, -0.673, -0.888, -0.277, -0.361, -0.909, 0.982, -0.124 };
   double X[] = { 0.064, 0.169, -0.81, -0.779, -0.641, -0.103 };
   int incX = -1;
   double x_expected[] = { 0.064, 0.169, -0.80842, -0.715637, -0.829924, -0.212971 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 951) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 951) imag");
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
   double A[] = { -0.212, 0.612, 0.189, -0.046, -0.124, 0.82, 0.753, 0.727, 0.331, 0.116, 0.504, -0.673, -0.888, -0.277, -0.361, -0.909, 0.982, -0.124 };
   double X[] = { 0.064, 0.169, -0.81, -0.779, -0.641, -0.103 };
   int incX = -1;
   double x_expected[] = { 0.634014, 0.796937, -0.585538, -0.895375, -0.125887, 0.010019 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 952) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 952) imag");
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
   double A[] = { -0.212, 0.612, 0.189, -0.046, -0.124, 0.82, 0.753, 0.727, 0.331, 0.116, 0.504, -0.673, -0.888, -0.277, -0.361, -0.909, 0.982, -0.124 };
   double X[] = { 0.064, 0.169, -0.81, -0.779, -0.641, -0.103 };
   int incX = -1;
   double x_expected[] = { 0.567497, 1.085122, -1.217792, -1.322566, -0.641, -0.103 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 953) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 953) imag");
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
   double A[] = { -0.212, 0.612, 0.189, -0.046, -0.124, 0.82, 0.753, 0.727, 0.331, 0.116, 0.504, -0.673, -0.888, -0.277, -0.361, -0.909, 0.982, -0.124 };
   double X[] = { 0.064, 0.169, -0.81, -0.779, -0.641, -0.103 };
   int incX = -1;
   double x_expected[] = { 0.130517, -0.119185, -0.187765, -0.519609, -0.169484, -1.165438 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 954) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 954) imag");
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
   double A[] = { -0.212, 0.612, 0.189, -0.046, -0.124, 0.82, 0.753, 0.727, 0.331, 0.116, 0.504, -0.673, -0.888, -0.277, -0.361, -0.909, 0.982, -0.124 };
   double X[] = { 0.064, 0.169, -0.81, -0.779, -0.641, -0.103 };
   int incX = -1;
   double x_expected[] = { 0.064, 0.169, -0.820019, -0.9468, -0.684597, -1.278457 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 955) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 955) imag");
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
   double A[] = { -0.212, 0.612, 0.189, -0.046, -0.124, 0.82, 0.753, 0.727, 0.331, 0.116, 0.504, -0.673, -0.888, -0.277, -0.361, -0.909, 0.982, -0.124 };
   double X[] = { 0.064, 0.169, -0.81, -0.779, -0.641, -0.103 };
   int incX = -1;
   double x_expected[] = { -0.187765, -0.519609, -0.169484, -1.165438, 0.198928, -0.370456 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 956) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 956) imag");
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
   double A[] = { -0.212, 0.612, 0.189, -0.046, -0.124, 0.82, 0.753, 0.727, 0.331, 0.116, 0.504, -0.673, -0.888, -0.277, -0.361, -0.909, 0.982, -0.124 };
   double X[] = { 0.064, 0.169, -0.81, -0.779, -0.641, -0.103 };
   int incX = -1;
   double x_expected[] = { -0.113746, -0.182809, -0.935887, -0.768981, -0.641, -0.103 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 957) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 957) imag");
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
   double A[] = { -0.374, -0.308, 0.792, 0.884, -0.794, -0.055, -0.281, 0.527, 0.246, 0.762, 0.853, 0.891, -0.231, 0.384, 0.373, -0.717, -0.957, -0.338 };
   double X[] = { 0.884, 0.636, 0.921, 0.282, -0.649, 0.188 };
   int incX = -1;
   double x_expected[] = { -0.436746, 0.963714, -1.087615, -0.018695, 0.30063, 0.12958 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 958) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 958) imag");
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
   double A[] = { -0.374, -0.308, 0.792, 0.884, -0.794, -0.055, -0.281, 0.527, 0.246, 0.762, 0.853, 0.891, -0.231, 0.384, 0.373, -0.717, -0.957, -0.338 };
   double X[] = { 0.884, 0.636, 0.921, 0.282, -0.649, 0.188 };
   int incX = -1;
   double x_expected[] = { 0.895682, 1.407174, 0.2408, -0.14282, -0.649, 0.188 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 959) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 959) imag");
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
   double A[] = { -0.374, -0.308, 0.792, 0.884, -0.794, -0.055, -0.281, 0.527, 0.246, 0.762, 0.853, 0.891, -0.231, 0.384, 0.373, -0.717, -0.957, -0.338 };
   double X[] = { 0.884, 0.636, 0.921, 0.282, -0.649, 0.188 };
   int incX = -1;
   double x_expected[] = { 0.785744, -0.3966, -0.436746, 0.963714, -1.087615, -0.018695 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 960) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 960) imag");
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
   double A[] = { -0.374, -0.308, 0.792, 0.884, -0.794, -0.055, -0.281, 0.527, 0.246, 0.762, 0.853, 0.891, -0.231, 0.384, 0.373, -0.717, -0.957, -0.338 };
   double X[] = { 0.884, 0.636, 0.921, 0.282, -0.649, 0.188 };
   int incX = -1;
   double x_expected[] = { 0.884, 0.636, 0.472572, 0.47454, -1.056415, 0.594125 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 961) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 961) imag");
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
   double A[] = { -0.374, -0.308, 0.792, 0.884, -0.794, -0.055, -0.281, 0.527, 0.246, 0.762, 0.853, 0.891, -0.231, 0.384, 0.373, -0.717, -0.957, -0.338 };
   double X[] = { 0.884, 0.636, 0.921, 0.282, -0.649, 0.188 };
   int incX = -1;
   double x_expected[] = { 0.464705, -0.108078, 0.094975, 0.376323, -0.6802, -0.42482 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 962) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 962) imag");
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
   double A[] = { -0.374, -0.308, 0.792, 0.884, -0.794, -0.055, -0.281, 0.527, 0.246, 0.762, 0.853, 0.891, -0.231, 0.384, 0.373, -0.717, -0.957, -0.338 };
   double X[] = { 0.884, 0.636, 0.921, 0.282, -0.649, 0.188 };
   int incX = -1;
   double x_expected[] = { 0.562961, 0.924522, 1.004293, -0.112851, -0.649, 0.188 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 963) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 963) imag");
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
   double A[] = { -0.374, -0.308, 0.792, 0.884, -0.794, -0.055, -0.281, 0.527, 0.246, 0.762, 0.853, 0.891, -0.231, 0.384, 0.373, -0.717, -0.957, -0.338 };
   double X[] = { 0.884, 0.636, 0.921, 0.282, -0.649, 0.188 };
   int incX = -1;
   double x_expected[] = { -0.448428, 0.19254, -0.674583, 1.236189, 0.780774, 1.167088 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 964) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 964) imag");
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
   double A[] = { -0.374, -0.308, 0.792, 0.884, -0.794, -0.055, -0.281, 0.527, 0.246, 0.762, 0.853, 0.891, -0.231, 0.384, 0.373, -0.717, -0.957, -0.338 };
   double X[] = { 0.884, 0.636, 0.921, 0.282, -0.649, 0.188 };
   int incX = -1;
   double x_expected[] = { 0.884, 0.636, 0.653832, 1.112064, -0.168856, 1.225508 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 965) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 965) imag");
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
   double A[] = { 0.002, 0.95, -0.363, 0.084, -0.646, 0.816, -0.407, 0.099, -0.02, -0.906, -0.874, 0.191, -0.328, -0.968, 0.79, 0.826, -0.795, 0.277 };
   double X[] = { -0.591, -0.084, 0.707, 0.945, 0.326, 0.428 };
   int incX = -1;
   double x_expected[] = { -0.59515, 0.077106, -0.27658, -0.637356, 0.407252, -0.308844 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 966) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 966) imag");
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
   double A[] = { 0.002, 0.95, -0.363, 0.084, -0.646, 0.816, -0.407, 0.099, -0.02, -0.906, -0.874, 0.191, -0.328, -0.968, 0.79, 0.826, -0.795, 0.277 };
   double X[] = { -0.591, -0.084, 0.707, 0.945, 0.326, 0.428 };
   int incX = -1;
   double x_expected[] = { -1.46131, 0.537642, 0.624614, 0.762252, 0.326, 0.428 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 967) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 967) imag");
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
   double A[] = { 0.002, 0.95, -0.363, 0.084, -0.646, 0.816, -0.407, 0.099, -0.02, -0.906, -0.874, 0.191, -0.328, -0.968, 0.79, 0.826, -0.795, 0.277 };
   double X[] = { -0.591, -0.084, 0.707, 0.945, 0.326, 0.428 };
   int incX = -1;
   double x_expected[] = { -0.536274, 0.421806, -0.59515, 0.077106, -0.27658, -0.637356 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 968) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 968) imag");
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
   double A[] = { 0.002, 0.95, -0.363, 0.084, -0.646, 0.816, -0.407, 0.099, -0.02, -0.906, -0.874, 0.191, -0.328, -0.968, 0.79, 0.826, -0.795, 0.277 };
   double X[] = { -0.591, -0.084, 0.707, 0.945, 0.326, 0.428 };
   int incX = -1;
   double x_expected[] = { -0.591, -0.084, 0.98216, 0.400464, 0.131806, -0.026608 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 969) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 969) imag");
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
   double A[] = { 0.002, 0.95, -0.363, 0.084, -0.646, 0.816, -0.407, 0.099, -0.02, -0.906, -0.874, 0.191, -0.328, -0.968, 0.79, 0.826, -0.795, 0.277 };
   double X[] = { -0.591, -0.084, 0.707, 0.945, 0.326, 0.428 };
   int incX = -1;
   double x_expected[] = { -1.68293, 0.796222, -0.96062, 0.415172, -0.082386, -0.182748 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 970) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 970) imag");
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
   double A[] = { 0.002, 0.95, -0.363, 0.084, -0.646, 0.816, -0.407, 0.099, -0.02, -0.906, -0.874, 0.191, -0.328, -0.968, 0.79, 0.826, -0.795, 0.277 };
   double X[] = { -0.591, -0.084, 0.707, 0.945, 0.326, 0.428 };
   int incX = -1;
   double x_expected[] = { -1.737656, 0.290416, 0.61669, 0.73853, 0.326, 0.428 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 971) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 971) imag");
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
   double A[] = { 0.002, 0.95, -0.363, 0.084, -0.646, 0.816, -0.407, 0.099, -0.02, -0.906, -0.874, 0.191, -0.328, -0.968, 0.79, 0.826, -0.795, 0.277 };
   double X[] = { -0.591, -0.084, 0.707, 0.945, 0.326, 0.428 };
   int incX = -1;
   double x_expected[] = { 0.27516, -0.544536, -0.10627, -0.988374, 0.229991, -0.711267 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 972) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 972) imag");
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
   double A[] = { 0.002, 0.95, -0.363, 0.084, -0.646, 0.816, -0.407, 0.099, -0.02, -0.906, -0.874, 0.191, -0.328, -0.968, 0.79, 0.826, -0.795, 0.277 };
   double X[] = { -0.591, -0.084, 0.707, 0.945, 0.326, 0.428 };
   int incX = -1;
   double x_expected[] = { -0.591, -0.084, 0.794924, 0.411234, 0.148739, 0.025577 };
   cblas_ztbmv(order, uplo, trans, diag, N, K, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 3; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztbmv(case 973) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztbmv(case 973) imag");
     };
   };
  };


}
