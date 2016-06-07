#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_trsv (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.995f };
   float X[] = { 0.348f };
   int incX = -1;
   float x_expected[] = { 0.349749f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1150)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.995f };
   float X[] = { 0.348f };
   int incX = -1;
   float x_expected[] = { 0.348f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1151)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.995f };
   float X[] = { 0.348f };
   int incX = -1;
   float x_expected[] = { 0.349749f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1152)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.995f };
   float X[] = { 0.348f };
   int incX = -1;
   float x_expected[] = { 0.348f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1153)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.995f };
   float X[] = { 0.348f };
   int incX = -1;
   float x_expected[] = { 0.349749f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1154)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.995f };
   float X[] = { 0.348f };
   int incX = -1;
   float x_expected[] = { 0.348f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1155)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.995f };
   float X[] = { 0.348f };
   int incX = -1;
   float x_expected[] = { 0.349749f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1156)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.995f };
   float X[] = { 0.348f };
   int incX = -1;
   float x_expected[] = { 0.348f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1157)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.793f };
   float X[] = { 0.338f };
   int incX = -1;
   float x_expected[] = { 0.42623f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1158)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.793f };
   float X[] = { 0.338f };
   int incX = -1;
   float x_expected[] = { 0.338f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1159)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.793f };
   float X[] = { 0.338f };
   int incX = -1;
   float x_expected[] = { 0.42623f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1160)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.793f };
   float X[] = { 0.338f };
   int incX = -1;
   float x_expected[] = { 0.338f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1161)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.793f };
   float X[] = { 0.338f };
   int incX = -1;
   float x_expected[] = { 0.42623f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1162)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.793f };
   float X[] = { 0.338f };
   int incX = -1;
   float x_expected[] = { 0.338f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1163)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.793f };
   float X[] = { 0.338f };
   int incX = -1;
   float x_expected[] = { 0.42623f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1164)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.793f };
   float X[] = { 0.338f };
   int incX = -1;
   float x_expected[] = { 0.338f };
   cblas_strsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "strsv(case 1165)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { -0.21 };
   double X[] = { 0.473 };
   int incX = -1;
   double x_expected[] = { -2.25238095238 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1166)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { -0.21 };
   double X[] = { 0.473 };
   int incX = -1;
   double x_expected[] = { 0.473 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1167)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { -0.21 };
   double X[] = { 0.473 };
   int incX = -1;
   double x_expected[] = { -2.25238095238 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1168)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { -0.21 };
   double X[] = { 0.473 };
   int incX = -1;
   double x_expected[] = { 0.473 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1169)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { -0.21 };
   double X[] = { 0.473 };
   int incX = -1;
   double x_expected[] = { -2.25238095238 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1170)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { -0.21 };
   double X[] = { 0.473 };
   int incX = -1;
   double x_expected[] = { 0.473 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1171)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { -0.21 };
   double X[] = { 0.473 };
   int incX = -1;
   double x_expected[] = { -2.25238095238 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1172)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { -0.21 };
   double X[] = { 0.473 };
   int incX = -1;
   double x_expected[] = { 0.473 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1173)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.748 };
   double X[] = { 0.979 };
   int incX = -1;
   double x_expected[] = { 1.30882352941 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1174)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.748 };
   double X[] = { 0.979 };
   int incX = -1;
   double x_expected[] = { 0.979 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1175)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.748 };
   double X[] = { 0.979 };
   int incX = -1;
   double x_expected[] = { 1.30882352941 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1176)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.748 };
   double X[] = { 0.979 };
   int incX = -1;
   double x_expected[] = { 0.979 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1177)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.748 };
   double X[] = { 0.979 };
   int incX = -1;
   double x_expected[] = { 1.30882352941 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1178)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.748 };
   double X[] = { 0.979 };
   int incX = -1;
   double x_expected[] = { 0.979 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1179)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.748 };
   double X[] = { 0.979 };
   int incX = -1;
   double x_expected[] = { 1.30882352941 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1180)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.748 };
   double X[] = { 0.979 };
   int incX = -1;
   double x_expected[] = { 0.979 };
   cblas_dtrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtrsv(case 1181)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.529f, -0.348f };
   float X[] = { -0.95f, 0.343f };
   int incX = -1;
   float x_expected[] = { -1.55112f, -0.372004f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1182) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1182) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.529f, -0.348f };
   float X[] = { -0.95f, 0.343f };
   int incX = -1;
   float x_expected[] = { -0.95f, 0.343f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1183) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1183) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.529f, -0.348f };
   float X[] = { -0.95f, 0.343f };
   int incX = -1;
   float x_expected[] = { -1.55112f, -0.372004f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1184) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1184) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.529f, -0.348f };
   float X[] = { -0.95f, 0.343f };
   int incX = -1;
   float x_expected[] = { -0.95f, 0.343f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1185) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1185) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.529f, -0.348f };
   float X[] = { -0.95f, 0.343f };
   int incX = -1;
   float x_expected[] = { -1.55112f, -0.372004f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1186) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1186) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.529f, -0.348f };
   float X[] = { -0.95f, 0.343f };
   int incX = -1;
   float x_expected[] = { -0.95f, 0.343f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1187) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1187) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.529f, -0.348f };
   float X[] = { -0.95f, 0.343f };
   int incX = -1;
   float x_expected[] = { -1.55112f, -0.372004f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1188) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1188) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.529f, -0.348f };
   float X[] = { -0.95f, 0.343f };
   int incX = -1;
   float x_expected[] = { -0.95f, 0.343f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1189) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1189) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.6f, 0.041f };
   float X[] = { 0.896f, -0.447f };
   int incX = -1;
   float x_expected[] = { 1.43572f, -0.843108f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1190) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1190) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.6f, 0.041f };
   float X[] = { 0.896f, -0.447f };
   int incX = -1;
   float x_expected[] = { 0.896f, -0.447f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1191) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1191) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.6f, 0.041f };
   float X[] = { 0.896f, -0.447f };
   int incX = -1;
   float x_expected[] = { 1.43572f, -0.843108f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1192) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1192) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.6f, 0.041f };
   float X[] = { 0.896f, -0.447f };
   int incX = -1;
   float x_expected[] = { 0.896f, -0.447f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1193) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1193) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.6f, 0.041f };
   float X[] = { 0.896f, -0.447f };
   int incX = -1;
   float x_expected[] = { 1.43572f, -0.843108f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1194) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1194) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.6f, 0.041f };
   float X[] = { 0.896f, -0.447f };
   int incX = -1;
   float x_expected[] = { 0.896f, -0.447f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1195) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1195) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.6f, 0.041f };
   float X[] = { 0.896f, -0.447f };
   int incX = -1;
   float x_expected[] = { 1.43572f, -0.843108f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1196) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1196) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.6f, 0.041f };
   float X[] = { 0.896f, -0.447f };
   int incX = -1;
   float x_expected[] = { 0.896f, -0.447f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1197) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1197) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.397f, 0.683f };
   float X[] = { 0.765f, 0.18f };
   int incX = -1;
   float x_expected[] = { 0.289642f, 0.951701f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1198) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1198) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.397f, 0.683f };
   float X[] = { 0.765f, 0.18f };
   int incX = -1;
   float x_expected[] = { 0.765f, 0.18f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1199) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1199) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.397f, 0.683f };
   float X[] = { 0.765f, 0.18f };
   int incX = -1;
   float x_expected[] = { 0.289642f, 0.951701f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1200) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1200) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.397f, 0.683f };
   float X[] = { 0.765f, 0.18f };
   int incX = -1;
   float x_expected[] = { 0.765f, 0.18f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1201) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1201) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.397f, 0.683f };
   float X[] = { 0.765f, 0.18f };
   int incX = -1;
   float x_expected[] = { 0.289642f, 0.951701f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1202) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1202) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.397f, 0.683f };
   float X[] = { 0.765f, 0.18f };
   int incX = -1;
   float x_expected[] = { 0.765f, 0.18f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1203) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1203) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   float A[] = { 0.397f, 0.683f };
   float X[] = { 0.765f, 0.18f };
   int incX = -1;
   float x_expected[] = { 0.289642f, 0.951701f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1204) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1204) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   float A[] = { 0.397f, 0.683f };
   float X[] = { 0.765f, 0.18f };
   int incX = -1;
   float x_expected[] = { 0.765f, 0.18f };
   cblas_ctrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctrsv(case 1205) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctrsv(case 1205) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.977, -0.955 };
   double X[] = { -0.627, 0.281 };
   int incX = -1;
   double x_expected[] = { -0.471957414573, -0.173714770642 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1206) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1206) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.977, -0.955 };
   double X[] = { -0.627, 0.281 };
   int incX = -1;
   double x_expected[] = { -0.627, 0.281 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1207) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1207) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.977, -0.955 };
   double X[] = { -0.627, 0.281 };
   int incX = -1;
   double x_expected[] = { -0.471957414573, -0.173714770642 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1208) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1208) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.977, -0.955 };
   double X[] = { -0.627, 0.281 };
   int incX = -1;
   double x_expected[] = { -0.627, 0.281 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1209) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1209) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.977, -0.955 };
   double X[] = { -0.627, 0.281 };
   int incX = -1;
   double x_expected[] = { -0.471957414573, -0.173714770642 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1210) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1210) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.977, -0.955 };
   double X[] = { -0.627, 0.281 };
   int incX = -1;
   double x_expected[] = { -0.627, 0.281 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1211) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1211) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.977, -0.955 };
   double X[] = { -0.627, 0.281 };
   int incX = -1;
   double x_expected[] = { -0.471957414573, -0.173714770642 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1212) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1212) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.977, -0.955 };
   double X[] = { -0.627, 0.281 };
   int incX = -1;
   double x_expected[] = { -0.627, 0.281 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1213) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1213) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.076, -0.16 };
   double X[] = { 0.3, -0.874 };
   int incX = -1;
   double x_expected[] = { 5.18357980622, -0.587200407955 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1214) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1214) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.076, -0.16 };
   double X[] = { 0.3, -0.874 };
   int incX = -1;
   double x_expected[] = { 0.3, -0.874 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1215) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1215) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.076, -0.16 };
   double X[] = { 0.3, -0.874 };
   int incX = -1;
   double x_expected[] = { 5.18357980622, -0.587200407955 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1216) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1216) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.076, -0.16 };
   double X[] = { 0.3, -0.874 };
   int incX = -1;
   double x_expected[] = { 0.3, -0.874 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1217) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1217) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.076, -0.16 };
   double X[] = { 0.3, -0.874 };
   int incX = -1;
   double x_expected[] = { 5.18357980622, -0.587200407955 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1218) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1218) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.076, -0.16 };
   double X[] = { 0.3, -0.874 };
   int incX = -1;
   double x_expected[] = { 0.3, -0.874 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1219) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1219) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.076, -0.16 };
   double X[] = { 0.3, -0.874 };
   int incX = -1;
   double x_expected[] = { 5.18357980622, -0.587200407955 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1220) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1220) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.076, -0.16 };
   double X[] = { 0.3, -0.874 };
   int incX = -1;
   double x_expected[] = { 0.3, -0.874 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1221) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1221) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.372, -0.745 };
   double X[] = { -0.085, -0.303 };
   int incX = -1;
   double x_expected[] = { -0.371144591432, -0.0712292456544 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1222) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1222) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.372, -0.745 };
   double X[] = { -0.085, -0.303 };
   int incX = -1;
   double x_expected[] = { -0.085, -0.303 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1223) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1223) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.372, -0.745 };
   double X[] = { -0.085, -0.303 };
   int incX = -1;
   double x_expected[] = { -0.371144591432, -0.0712292456544 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1224) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1224) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.372, -0.745 };
   double X[] = { -0.085, -0.303 };
   int incX = -1;
   double x_expected[] = { -0.085, -0.303 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1225) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1225) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.372, -0.745 };
   double X[] = { -0.085, -0.303 };
   int incX = -1;
   double x_expected[] = { -0.371144591432, -0.0712292456544 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1226) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1226) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.372, -0.745 };
   double X[] = { -0.085, -0.303 };
   int incX = -1;
   double x_expected[] = { -0.085, -0.303 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1227) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1227) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 1;
   int lda = 1;
   double A[] = { 0.372, -0.745 };
   double X[] = { -0.085, -0.303 };
   int incX = -1;
   double x_expected[] = { -0.371144591432, -0.0712292456544 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1228) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1228) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 1;
   int lda = 1;
   double A[] = { 0.372, -0.745 };
   double X[] = { -0.085, -0.303 };
   int incX = -1;
   double x_expected[] = { -0.085, -0.303 };
   cblas_ztrsv(order, uplo, trans, diag, N, A, lda, X, incX);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztrsv(case 1229) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztrsv(case 1229) imag");
     };
   };
  };


}
