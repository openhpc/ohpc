#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_tpsv (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.31929f, 0.360168f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1310)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.144f, -0.04432f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1311)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.417992f, -0.0839895f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1312)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.12704f, 0.032f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1313)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.31929f, 0.360168f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1314)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.144f, -0.04432f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1315)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.417992f, -0.0839895f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1316)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.12704f, 0.032f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1317)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.417992f, -0.0839895f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1318)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.12704f, 0.032f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1319)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.31929f, 0.360168f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1320)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.144f, -0.04432f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1321)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.417992f, -0.0839895f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1322)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.12704f, 0.032f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1323)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.31929f, 0.360168f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1324)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.381f, 0.53f, 0.451f };
   float X[] = { 0.144f, 0.032f };
   int incX = -1;
   float x_expected[] = { 0.144f, -0.04432f };
   cblas_stpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "stpsv(case 1325)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { 1.67142857143, 1.42438631791 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1326)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { -0.702, -1.150996 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1327)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { 4.76584842388, -1.86197183099 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1328)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { -1.163378, -0.661 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1329)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { 1.67142857143, 1.42438631791 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1330)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { -0.702, -1.150996 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1331)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { 4.76584842388, -1.86197183099 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1332)");
     }
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { -1.163378, -0.661 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1333)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { 4.76584842388, -1.86197183099 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1334)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { -1.163378, -0.661 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1335)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { 1.67142857143, 1.42438631791 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1336)");
     }
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { -0.702, -1.150996 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1337)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { 4.76584842388, -1.86197183099 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1338)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { -1.163378, -0.661 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1339)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { 1.67142857143, 1.42438631791 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1340)");
     }
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.355, -0.698, -0.42 };
   double X[] = { -0.702, -0.661 };
   int incX = -1;
   double x_expected[] = { -0.702, -1.150996 };
   cblas_dtpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "dtpsv(case 1341)");
     }
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -1.05533f, 0.0529057f, -3.93625f, 1.36003f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1342) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1342) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -0.133f, 0.998f, 0.818576f, 0.163438f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1343) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1343) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -2.48362f, 1.13085f, -1.67581f, -0.273264f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1344) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1344) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { 0.431924f, 0.679112f, -0.072f, 0.642f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1345) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1345) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -1.05533f, 0.0529057f, -3.93625f, 1.36003f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1346) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1346) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -0.133f, 0.998f, 0.818576f, 0.163438f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1347) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1347) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -2.48362f, 1.13085f, -1.67581f, -0.273264f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1348) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1348) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { 0.431924f, 0.679112f, -0.072f, 0.642f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1349) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1349) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -2.48362f, 1.13085f, -1.67581f, -0.273264f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1350) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1350) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { 0.431924f, 0.679112f, -0.072f, 0.642f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1351) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1351) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -1.05533f, 0.0529057f, -3.93625f, 1.36003f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1352) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1352) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -0.133f, 0.998f, 0.818576f, 0.163438f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1353) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1353) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -2.48362f, 1.13085f, -1.67581f, -0.273264f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1354) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1354) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { 0.431924f, 0.679112f, -0.072f, 0.642f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1355) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1355) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -1.05533f, 0.0529057f, -3.93625f, 1.36003f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1356) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1356) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -0.133f, 0.998f, 0.818576f, 0.163438f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1357) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1357) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { 2.15867f, 1.69498f, 1.69471f, 0.104738f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1358) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1358) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -0.613252f, 0.561896f, -0.072f, 0.642f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1359) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1359) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { 1.00465f, 0.327432f, 3.44853f, 2.273f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1360) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1360) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -0.133f, 0.998f, -0.806168f, -0.053086f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1361) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1361) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { 2.15867f, 1.69498f, 1.69471f, 0.104738f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1362) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1362) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -0.613252f, 0.561896f, -0.072f, 0.642f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1363) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1363) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { 1.00465f, 0.327432f, 3.44853f, 2.273f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1364) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1364) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   float A[] = { -0.019f, -0.38f, 0.588f, 0.814f, 0.173f, -0.937f };
   float X[] = { -0.133f, 0.998f, -0.072f, 0.642f };
   int incX = -1;
   float x_expected[] = { -0.133f, 0.998f, -0.806168f, -0.053086f };
   cblas_ctpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], flteps, "ctpsv(case 1365) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], flteps, "ctpsv(case 1365) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 0.430509772467, -0.0927067365535, -0.611144484555, 0.999982608216 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1366) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1366) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 0.344, -0.143, -0.795928, -0.523879 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1367) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1367) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 1.00136463678, -1.84591534629, -1.12140892769, 0.696784840869 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1368) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1368) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 1.707508, -0.042521, -0.668, -0.945 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1369) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1369) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 0.430509772467, -0.0927067365535, -0.611144484555, 0.999982608216 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1370) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1370) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 0.344, -0.143, -0.795928, -0.523879 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1371) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1371) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 1.00136463678, -1.84591534629, -1.12140892769, 0.696784840869 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1372) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1372) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 111;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 1.707508, -0.042521, -0.668, -0.945 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1373) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1373) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 1.00136463678, -1.84591534629, -1.12140892769, 0.696784840869 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1374) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1374) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 1.707508, -0.042521, -0.668, -0.945 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1375) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1375) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 0.430509772467, -0.0927067365535, -0.611144484555, 0.999982608216 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1376) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1376) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 0.344, -0.143, -0.795928, -0.523879 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1377) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1377) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 1.00136463678, -1.84591534629, -1.12140892769, 0.696784840869 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1378) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1378) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 1.707508, -0.042521, -0.668, -0.945 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1379) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1379) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 0.430509772467, -0.0927067365535, -0.611144484555, 0.999982608216 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1380) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1380) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 112;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 0.344, -0.143, -0.795928, -0.523879 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1381) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1381) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { -1.47384781823, -0.286556198408, 1.03098932879, -0.824698794397 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1382) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1382) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { -0.016172, 1.175911, -0.668, -0.945 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1383) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1383) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 0.369363905801, -0.239798891331, 1.1759505739, -1.40027235656 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1384) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1384) imag");
     };
   };
  };


  {
   int order = 101;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 0.344, -0.143, -1.05676, -1.151335 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1385) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1385) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 131;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { -1.47384781823, -0.286556198408, 1.03098932879, -0.824698794397 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1386) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1386) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 121;
   int diag = 132;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { -0.016172, 1.175911, -0.668, -0.945 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1387) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1387) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 131;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 0.369363905801, -0.239798891331, 1.1759505739, -1.40027235656 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1388) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1388) imag");
     };
   };
  };


  {
   int order = 102;
   int trans = 113;
   int uplo = 122;
   int diag = 132;
   int N = 2;
   double A[] = { 0.052, 0.875, 0.751, -0.912, 0.832, -0.153 };
   double X[] = { 0.344, -0.143, -0.668, -0.945 };
   int incX = -1;
   double x_expected[] = { 0.344, -0.143, -1.05676, -1.151335 };
   cblas_ztpsv(order, uplo, trans, diag, N, A, X, incX);
   {
     int i;
     for (i = 0; i < 2; i++) {
       gsl_test_rel(X[2*i], x_expected[2*i], dbleps, "ztpsv(case 1389) real");
       gsl_test_rel(X[2*i+1], x_expected[2*i+1], dbleps, "ztpsv(case 1389) imag");
     };
   };
  };


}
