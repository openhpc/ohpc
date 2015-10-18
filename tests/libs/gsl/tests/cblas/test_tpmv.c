#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_tpmv (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.587f, 0.14f, 0.841f };
   float X[] = { -0.213f, 0.885f };
   int incX = -1;
   float x_expected[] = { -0.179133f, -0.549315f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 974)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.587f, 0.14f, 0.841f };
   float X[] = { -0.213f, 0.885f };
   int incX = -1;
   float x_expected[] = { -0.213f, 0.85518f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 975)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.587f, 0.14f, 0.841f };
   float X[] = { -0.213f, 0.885f };
   int incX = -1;
   float x_expected[] = { -0.055233f, -0.519495f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 976)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.587f, 0.14f, 0.841f };
   float X[] = { -0.213f, 0.885f };
   int incX = -1;
   float x_expected[] = { -0.0891f, 0.885f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 977)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.587f, 0.14f, 0.841f };
   float X[] = { -0.213f, 0.885f };
   int incX = -1;
   float x_expected[] = { -0.179133f, -0.549315f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 978)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.587f, 0.14f, 0.841f };
   float X[] = { -0.213f, 0.885f };
   int incX = -1;
   float x_expected[] = { -0.213f, 0.85518f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 979)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.587f, 0.14f, 0.841f };
   float X[] = { -0.213f, 0.885f };
   int incX = -1;
   float x_expected[] = { -0.055233f, -0.519495f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 980)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.587f, 0.14f, 0.841f };
   float X[] = { -0.213f, 0.885f };
   int incX = -1;
   float x_expected[] = { -0.0891f, 0.885f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 981)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.765f, 0.968f, -0.956f };
   float X[] = { 0.243f, -0.274f };
   int incX = -1;
   float x_expected[] = { -0.49754f, 0.20961f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 982)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.765f, 0.968f, -0.956f };
   float X[] = { 0.243f, -0.274f };
   int incX = -1;
   float x_expected[] = { -0.022232f, -0.274f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 983)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.765f, 0.968f, -0.956f };
   float X[] = { 0.243f, -0.274f };
   int incX = -1;
   float x_expected[] = { -0.232308f, 0.444834f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 984)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.765f, 0.968f, -0.956f };
   float X[] = { 0.243f, -0.274f };
   int incX = -1;
   float x_expected[] = { 0.243f, -0.038776f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 985)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.765f, 0.968f, -0.956f };
   float X[] = { 0.243f, -0.274f };
   int incX = -1;
   float x_expected[] = { -0.49754f, 0.20961f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 986)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.765f, 0.968f, -0.956f };
   float X[] = { 0.243f, -0.274f };
   int incX = -1;
   float x_expected[] = { -0.022232f, -0.274f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 987)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.765f, 0.968f, -0.956f };
   float X[] = { 0.243f, -0.274f };
   int incX = -1;
   float x_expected[] = { -0.232308f, 0.444834f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 988)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.765f, 0.968f, -0.956f };
   float X[] = { 0.243f, -0.274f };
   int incX = -1;
   float x_expected[] = { 0.243f, -0.038776f };
   cblas_stpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpmv(case 989)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.393, -0.221, 0.356 };
   double X[] = { -0.062, -0.221 };
   int incX = -1;
   double x_expected[] = { -0.022072, -0.073151 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 990)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.393, -0.221, 0.356 };
   double X[] = { -0.062, -0.221 };
   int incX = -1;
   double x_expected[] = { -0.062, -0.207298 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 991)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.393, -0.221, 0.356 };
   double X[] = { -0.062, -0.221 };
   int incX = -1;
   double x_expected[] = { 0.026769, -0.086853 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 992)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.393, -0.221, 0.356 };
   double X[] = { -0.062, -0.221 };
   int incX = -1;
   double x_expected[] = { -0.013159, -0.221 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 993)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.393, -0.221, 0.356 };
   double X[] = { -0.062, -0.221 };
   int incX = -1;
   double x_expected[] = { -0.022072, -0.073151 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 994)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.393, -0.221, 0.356 };
   double X[] = { -0.062, -0.221 };
   int incX = -1;
   double x_expected[] = { -0.062, -0.207298 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 995)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.393, -0.221, 0.356 };
   double X[] = { -0.062, -0.221 };
   int incX = -1;
   double x_expected[] = { 0.026769, -0.086853 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 996)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.393, -0.221, 0.356 };
   double X[] = { -0.062, -0.221 };
   int incX = -1;
   double x_expected[] = { -0.013159, -0.221 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 997)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.694, 0.501, 0.019 };
   double X[] = { -0.928, 0.365 };
   int incX = -1;
   double x_expected[] = { 0.165233, 0.25331 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 998)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.694, 0.501, 0.019 };
   double X[] = { -0.928, 0.365 };
   int incX = -1;
   double x_expected[] = { -0.745135, 0.365 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 999)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.694, 0.501, 0.019 };
   double X[] = { -0.928, 0.365 };
   int incX = -1;
   double x_expected[] = { -0.017632, -0.211618 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 1000)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.694, 0.501, 0.019 };
   double X[] = { -0.928, 0.365 };
   int incX = -1;
   double x_expected[] = { -0.928, -0.099928 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 1001)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.694, 0.501, 0.019 };
   double X[] = { -0.928, 0.365 };
   int incX = -1;
   double x_expected[] = { 0.165233, 0.25331 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 1002)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.694, 0.501, 0.019 };
   double X[] = { -0.928, 0.365 };
   int incX = -1;
   double x_expected[] = { -0.745135, 0.365 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 1003)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.694, 0.501, 0.019 };
   double X[] = { -0.928, 0.365 };
   int incX = -1;
   double x_expected[] = { -0.017632, -0.211618 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 1004)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.694, 0.501, 0.019 };
   double X[] = { -0.928, 0.365 };
   int incX = -1;
   double x_expected[] = { -0.928, -0.099928 };
   cblas_dtpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpmv(case 1005)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { 0.362f, -0.849f, -0.612f, -0.718f, 0.503f, -0.923f };
   float X[] = { 0.904f, 0.461f, -0.367f, 0.153f };
   int incX = -1;
   float x_expected[] = { 0.880215f, -0.602509f, -0.225207f, -0.564235f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1006) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1006) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { 0.362f, -0.849f, -0.612f, -0.718f, 0.503f, -0.923f };
   float X[] = { 0.904f, 0.461f, -0.367f, 0.153f };
   int incX = -1;
   float x_expected[] = { 0.904f, 0.461f, -0.58925f, -0.778204f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1007) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1007) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { 0.362f, -0.849f, -0.612f, -0.718f, 0.503f, -0.923f };
   float X[] = { 0.904f, 0.461f, -0.367f, 0.153f };
   int incX = -1;
   float x_expected[] = { 1.21467f, -0.432639f, -0.002957f, 0.366969f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1008) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1008) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { 0.362f, -0.849f, -0.612f, -0.718f, 0.503f, -0.923f };
   float X[] = { 0.904f, 0.461f, -0.367f, 0.153f };
   int incX = -1;
   float x_expected[] = { 1.23846f, 0.63087f, -0.367f, 0.153f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1009) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1009) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { 0.362f, -0.849f, -0.612f, -0.718f, 0.503f, -0.923f };
   float X[] = { 0.904f, 0.461f, -0.367f, 0.153f };
   int incX = -1;
   float x_expected[] = { 0.880215f, -0.602509f, -0.225207f, -0.564235f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1010) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1010) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { 0.362f, -0.849f, -0.612f, -0.718f, 0.503f, -0.923f };
   float X[] = { 0.904f, 0.461f, -0.367f, 0.153f };
   int incX = -1;
   float x_expected[] = { 0.904f, 0.461f, -0.58925f, -0.778204f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1011) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1011) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { 0.362f, -0.849f, -0.612f, -0.718f, 0.503f, -0.923f };
   float X[] = { 0.904f, 0.461f, -0.367f, 0.153f };
   int incX = -1;
   float x_expected[] = { 1.21467f, -0.432639f, -0.002957f, 0.366969f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1012) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1012) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { 0.362f, -0.849f, -0.612f, -0.718f, 0.503f, -0.923f };
   float X[] = { 0.904f, 0.461f, -0.367f, 0.153f };
   int incX = -1;
   float x_expected[] = { 1.23846f, 0.63087f, -0.367f, 0.153f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1013) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1013) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.876f, -0.697f, -0.519f, -0.223f, 0.526f, -0.077f };
   float X[] = { 0.338f, -0.807f, 0.444f, -0.748f };
   int incX = -1;
   float x_expected[] = { -0.281591f, -0.161308f, -0.9103f, 0.34578f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1014) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1014) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.876f, -0.697f, -0.519f, -0.223f, 0.526f, -0.077f };
   float X[] = { 0.338f, -0.807f, 0.444f, -0.748f };
   int incX = -1;
   float x_expected[] = { -0.05924f, -0.5178f, 0.444f, -0.748f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1015) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1015) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.876f, -0.697f, -0.519f, -0.223f, 0.526f, -0.077f };
   float X[] = { 0.338f, -0.807f, 0.444f, -0.748f };
   int incX = -1;
   float x_expected[] = { 0.115649f, -0.450508f, -1.26568f, 0.689239f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1016) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1016) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.876f, -0.697f, -0.519f, -0.223f, 0.526f, -0.077f };
   float X[] = { 0.338f, -0.807f, 0.444f, -0.748f };
   int incX = -1;
   float x_expected[] = { 0.338f, -0.807f, 0.088617f, -0.404541f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1017) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1017) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.876f, -0.697f, -0.519f, -0.223f, 0.526f, -0.077f };
   float X[] = { 0.338f, -0.807f, 0.444f, -0.748f };
   int incX = -1;
   float x_expected[] = { -0.281591f, -0.161308f, -0.9103f, 0.34578f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1018) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1018) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.876f, -0.697f, -0.519f, -0.223f, 0.526f, -0.077f };
   float X[] = { 0.338f, -0.807f, 0.444f, -0.748f };
   int incX = -1;
   float x_expected[] = { -0.05924f, -0.5178f, 0.444f, -0.748f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1019) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1019) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.876f, -0.697f, -0.519f, -0.223f, 0.526f, -0.077f };
   float X[] = { 0.338f, -0.807f, 0.444f, -0.748f };
   int incX = -1;
   float x_expected[] = { 0.115649f, -0.450508f, -1.26568f, 0.689239f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1020) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1020) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.876f, -0.697f, -0.519f, -0.223f, 0.526f, -0.077f };
   float X[] = { 0.338f, -0.807f, 0.444f, -0.748f };
   int incX = -1;
   float x_expected[] = { 0.338f, -0.807f, 0.088617f, -0.404541f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1021) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1021) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { 0.869f, -0.091f, -0.859f, 0.008f, -0.921f, -0.321f };
   float X[] = { -0.122f, -0.364f, 0.602f, -0.96f };
   int incX = -1;
   float x_expected[] = { -0.295592f, 1.11591f, 0.610498f, -0.779458f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1022) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1022) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { 0.869f, -0.091f, -0.859f, 0.008f, -0.921f, -0.321f };
   float X[] = { -0.122f, -0.364f, 0.602f, -0.96f };
   int incX = -1;
   float x_expected[] = { -0.646798f, 0.455824f, 0.602f, -0.96f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1023) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1023) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { 0.869f, -0.091f, -0.859f, 0.008f, -0.921f, -0.321f };
   float X[] = { -0.122f, -0.364f, 0.602f, -0.96f };
   int incX = -1;
   float x_expected[] = { 0.229206f, 0.296082f, 0.712384f, -0.465806f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1024) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1024) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { 0.869f, -0.091f, -0.859f, 0.008f, -0.921f, -0.321f };
   float X[] = { -0.122f, -0.364f, 0.602f, -0.96f };
   int incX = -1;
   float x_expected[] = { -0.122f, -0.364f, 0.703886f, -0.646348f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1025) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1025) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { 0.869f, -0.091f, -0.859f, 0.008f, -0.921f, -0.321f };
   float X[] = { -0.122f, -0.364f, 0.602f, -0.96f };
   int incX = -1;
   float x_expected[] = { -0.295592f, 1.11591f, 0.610498f, -0.779458f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1026) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1026) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { 0.869f, -0.091f, -0.859f, 0.008f, -0.921f, -0.321f };
   float X[] = { -0.122f, -0.364f, 0.602f, -0.96f };
   int incX = -1;
   float x_expected[] = { -0.646798f, 0.455824f, 0.602f, -0.96f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1027) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1027) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { 0.869f, -0.091f, -0.859f, 0.008f, -0.921f, -0.321f };
   float X[] = { -0.122f, -0.364f, 0.602f, -0.96f };
   int incX = -1;
   float x_expected[] = { 0.229206f, 0.296082f, 0.712384f, -0.465806f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1028) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1028) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { 0.869f, -0.091f, -0.859f, 0.008f, -0.921f, -0.321f };
   float X[] = { -0.122f, -0.364f, 0.602f, -0.96f };
   int incX = -1;
   float x_expected[] = { -0.122f, -0.364f, 0.703886f, -0.646348f };
   cblas_ctpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpmv(case 1029) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpmv(case 1029) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.254, 0.263, -0.271, -0.595, -0.182, -0.672 };
   double X[] = { -0.042, -0.705, -0.255, -0.854 };
   int incX = -1;
   double x_expected[] = { -0.466116, 0.156534, -0.248261, -0.067936 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1030) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1030) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.254, 0.263, -0.271, -0.595, -0.182, -0.672 };
   double X[] = { -0.042, -0.705, -0.255, -0.854 };
   int incX = -1;
   double x_expected[] = { -0.042, -0.705, -0.663093, -0.637955 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1031) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1031) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.254, 0.263, -0.271, -0.595, -0.182, -0.672 };
   double X[] = { -0.042, -0.705, -0.255, -0.854 };
   int incX = -1;
   double x_expected[] = { -0.905141, 0.539693, 0.159832, -0.283981 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1032) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1032) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.254, 0.263, -0.271, -0.595, -0.182, -0.672 };
   double X[] = { -0.042, -0.705, -0.255, -0.854 };
   int incX = -1;
   double x_expected[] = { -0.481025, -0.321841, -0.255, -0.854 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1033) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1033) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.254, 0.263, -0.271, -0.595, -0.182, -0.672 };
   double X[] = { -0.042, -0.705, -0.255, -0.854 };
   int incX = -1;
   double x_expected[] = { -0.466116, 0.156534, -0.248261, -0.067936 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1034) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1034) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.254, 0.263, -0.271, -0.595, -0.182, -0.672 };
   double X[] = { -0.042, -0.705, -0.255, -0.854 };
   int incX = -1;
   double x_expected[] = { -0.042, -0.705, -0.663093, -0.637955 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1035) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1035) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.254, 0.263, -0.271, -0.595, -0.182, -0.672 };
   double X[] = { -0.042, -0.705, -0.255, -0.854 };
   int incX = -1;
   double x_expected[] = { -0.905141, 0.539693, 0.159832, -0.283981 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1036) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1036) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.254, 0.263, -0.271, -0.595, -0.182, -0.672 };
   double X[] = { -0.042, -0.705, -0.255, -0.854 };
   int incX = -1;
   double x_expected[] = { -0.481025, -0.321841, -0.255, -0.854 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1037) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1037) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.421, -0.407, -0.595, -0.387, 0.884, -0.498 };
   double X[] = { -0.008, 0.904, -0.689, -0.679 };
   int incX = -1;
   double x_expected[] = { 0.590302, 1.473768, -0.566422, -0.005436 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1038) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1038) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.421, -0.407, -0.595, -0.387, 0.884, -0.498 };
   double X[] = { -0.008, 0.904, -0.689, -0.679 };
   int incX = -1;
   double x_expected[] = { 0.139182, 1.574648, -0.689, -0.679 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1039) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1039) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.421, -0.407, -0.595, -0.387, 0.884, -0.498 };
   double X[] = { -0.008, 0.904, -0.689, -0.679 };
   int incX = -1;
   double x_expected[] = { 0.44312, 0.80312, -0.211814, -0.54022 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1040) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1040) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.421, -0.407, -0.595, -0.387, 0.884, -0.498 };
   double X[] = { -0.008, 0.904, -0.689, -0.679 };
   int incX = -1;
   double x_expected[] = { -0.008, 0.904, -0.334392, -1.213784 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1041) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1041) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.421, -0.407, -0.595, -0.387, 0.884, -0.498 };
   double X[] = { -0.008, 0.904, -0.689, -0.679 };
   int incX = -1;
   double x_expected[] = { 0.590302, 1.473768, -0.566422, -0.005436 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1042) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1042) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.421, -0.407, -0.595, -0.387, 0.884, -0.498 };
   double X[] = { -0.008, 0.904, -0.689, -0.679 };
   int incX = -1;
   double x_expected[] = { 0.139182, 1.574648, -0.689, -0.679 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1043) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1043) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.421, -0.407, -0.595, -0.387, 0.884, -0.498 };
   double X[] = { -0.008, 0.904, -0.689, -0.679 };
   int incX = -1;
   double x_expected[] = { 0.44312, 0.80312, -0.211814, -0.54022 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1044) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1044) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.421, -0.407, -0.595, -0.387, 0.884, -0.498 };
   double X[] = { -0.008, 0.904, -0.689, -0.679 };
   int incX = -1;
   double x_expected[] = { -0.008, 0.904, -0.334392, -1.213784 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1045) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1045) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { -0.743, -0.078, 0.77, 0.505, 0.157, -0.986 };
   double X[] = { -0.641, 0.565, -0.406, -0.948 };
   int incX = -1;
   double x_expected[] = { -1.449087, -1.068251, 0.375602, 0.672696 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1046) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1046) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { -0.743, -0.078, 0.77, 0.505, 0.157, -0.986 };
   double X[] = { -0.641, 0.565, -0.406, -0.948 };
   int incX = -1;
   double x_expected[] = { -1.43236, 0.04007, -0.406, -0.948 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1047) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1047) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { -0.743, -0.078, 0.77, 0.505, 0.157, -0.986 };
   double X[] = { -0.641, 0.565, -0.406, -0.948 };
   int incX = -1;
   double x_expected[] = { -0.657727, -0.543321, 0.167357, 1.431451 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1048) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1048) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { -0.743, -0.078, 0.77, 0.505, 0.157, -0.986 };
   double X[] = { -0.641, 0.565, -0.406, -0.948 };
   int incX = -1;
   double x_expected[] = { -0.641, 0.565, -0.614245, -0.189245 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1049) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1049) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { -0.743, -0.078, 0.77, 0.505, 0.157, -0.986 };
   double X[] = { -0.641, 0.565, -0.406, -0.948 };
   int incX = -1;
   double x_expected[] = { -1.449087, -1.068251, 0.375602, 0.672696 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1050) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1050) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { -0.743, -0.078, 0.77, 0.505, 0.157, -0.986 };
   double X[] = { -0.641, 0.565, -0.406, -0.948 };
   int incX = -1;
   double x_expected[] = { -1.43236, 0.04007, -0.406, -0.948 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1051) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1051) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { -0.743, -0.078, 0.77, 0.505, 0.157, -0.986 };
   double X[] = { -0.641, 0.565, -0.406, -0.948 };
   int incX = -1;
   double x_expected[] = { -0.657727, -0.543321, 0.167357, 1.431451 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1052) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1052) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { -0.743, -0.078, 0.77, 0.505, 0.157, -0.986 };
   double X[] = { -0.641, 0.565, -0.406, -0.948 };
   int incX = -1;
   double x_expected[] = { -0.641, 0.565, -0.614245, -0.189245 };
   cblas_ztpmv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpmv(case 1053) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpmv(case 1053) imag");
     };
   };
  };


}
