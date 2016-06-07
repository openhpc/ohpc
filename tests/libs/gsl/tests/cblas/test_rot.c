#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_rot (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   int N = 1;
   float c = 0.0f;
   float s = 0.0f;
   float X[] = { -0.314f };
   int incX = 1;
   float Y[] = { -0.406f };
   int incY = -1;
   float x_expected[] = { 0.0f };
   float y_expected[] = { 0.0f };
   cblas_srot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "srot(case 558)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "srot(case 559)");
     }
   };
  };


  {
   int N = 1;
   float c = 0.866025403784f;
   float s = 0.5f;
   float X[] = { -0.314f };
   int incX = 1;
   float Y[] = { -0.406f };
   int incY = -1;
   float x_expected[] = { -0.474932f };
   float y_expected[] = { -0.194606f };
   cblas_srot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "srot(case 560)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "srot(case 561)");
     }
   };
  };


  {
   int N = 1;
   float c = 0.0f;
   float s = -1.0f;
   float X[] = { -0.314f };
   int incX = 1;
   float Y[] = { -0.406f };
   int incY = -1;
   float x_expected[] = { 0.406f };
   float y_expected[] = { -0.314f };
   cblas_srot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "srot(case 562)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "srot(case 563)");
     }
   };
  };


  {
   int N = 1;
   float c = -1.0f;
   float s = 0.0f;
   float X[] = { -0.314f };
   int incX = 1;
   float Y[] = { -0.406f };
   int incY = -1;
   float x_expected[] = { 0.314f };
   float y_expected[] = { 0.406f };
   cblas_srot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "srot(case 564)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "srot(case 565)");
     }
   };
  };


  {
   int N = 1;
   double c = 0;
   double s = 0;
   double X[] = { -0.493 };
   int incX = 1;
   double Y[] = { -0.014 };
   int incY = -1;
   double x_expected[] = { 0.0 };
   double y_expected[] = { 0.0 };
   cblas_drot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "drot(case 566)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "drot(case 567)");
     }
   };
  };


  {
   int N = 1;
   double c = 0.866025403784;
   double s = 0.5;
   double X[] = { -0.493 };
   int incX = 1;
   double Y[] = { -0.014 };
   int incY = -1;
   double x_expected[] = { -0.433950524066 };
   double y_expected[] = { 0.234375644347 };
   cblas_drot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "drot(case 568)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "drot(case 569)");
     }
   };
  };


  {
   int N = 1;
   double c = 0;
   double s = -1;
   double X[] = { -0.493 };
   int incX = 1;
   double Y[] = { -0.014 };
   int incY = -1;
   double x_expected[] = { 0.014 };
   double y_expected[] = { -0.493 };
   cblas_drot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "drot(case 570)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "drot(case 571)");
     }
   };
  };


  {
   int N = 1;
   double c = -1;
   double s = 0;
   double X[] = { -0.493 };
   int incX = 1;
   double Y[] = { -0.014 };
   int incY = -1;
   double x_expected[] = { 0.493 };
   double y_expected[] = { 0.014 };
   cblas_drot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "drot(case 572)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "drot(case 573)");
     }
   };
  };


  {
   int N = 1;
   float c = 0.0f;
   float s = 0.0f;
   float X[] = { -0.808f };
   int incX = -1;
   float Y[] = { -0.511f };
   int incY = 1;
   float x_expected[] = { 0.0f };
   float y_expected[] = { 0.0f };
   cblas_srot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "srot(case 574)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "srot(case 575)");
     }
   };
  };


  {
   int N = 1;
   float c = 0.866025403784f;
   float s = 0.5f;
   float X[] = { -0.808f };
   int incX = -1;
   float Y[] = { -0.511f };
   int incY = 1;
   float x_expected[] = { -0.955249f };
   float y_expected[] = { -0.038539f };
   cblas_srot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "srot(case 576)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "srot(case 577)");
     }
   };
  };


  {
   int N = 1;
   float c = 0.0f;
   float s = -1.0f;
   float X[] = { -0.808f };
   int incX = -1;
   float Y[] = { -0.511f };
   int incY = 1;
   float x_expected[] = { 0.511f };
   float y_expected[] = { -0.808f };
   cblas_srot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "srot(case 578)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "srot(case 579)");
     }
   };
  };


  {
   int N = 1;
   float c = -1.0f;
   float s = 0.0f;
   float X[] = { -0.808f };
   int incX = -1;
   float Y[] = { -0.511f };
   int incY = 1;
   float x_expected[] = { 0.808f };
   float y_expected[] = { 0.511f };
   cblas_srot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "srot(case 580)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "srot(case 581)");
     }
   };
  };


  {
   int N = 1;
   double c = 0;
   double s = 0;
   double X[] = { -0.176 };
   int incX = -1;
   double Y[] = { -0.165 };
   int incY = 1;
   double x_expected[] = { 0.0 };
   double y_expected[] = { 0.0 };
   cblas_drot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "drot(case 582)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "drot(case 583)");
     }
   };
  };


  {
   int N = 1;
   double c = 0.866025403784;
   double s = 0.5;
   double X[] = { -0.176 };
   int incX = -1;
   double Y[] = { -0.165 };
   int incY = 1;
   double x_expected[] = { -0.234920471066 };
   double y_expected[] = { -0.0548941916244 };
   cblas_drot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "drot(case 584)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "drot(case 585)");
     }
   };
  };


  {
   int N = 1;
   double c = 0;
   double s = -1;
   double X[] = { -0.176 };
   int incX = -1;
   double Y[] = { -0.165 };
   int incY = 1;
   double x_expected[] = { 0.165 };
   double y_expected[] = { -0.176 };
   cblas_drot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "drot(case 586)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "drot(case 587)");
     }
   };
  };


  {
   int N = 1;
   double c = -1;
   double s = 0;
   double X[] = { -0.176 };
   int incX = -1;
   double Y[] = { -0.165 };
   int incY = 1;
   double x_expected[] = { 0.176 };
   double y_expected[] = { 0.165 };
   cblas_drot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "drot(case 588)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "drot(case 589)");
     }
   };
  };


  {
   int N = 1;
   float c = 0.0f;
   float s = 0.0f;
   float X[] = { -0.201f };
   int incX = -1;
   float Y[] = { 0.087f };
   int incY = -1;
   float x_expected[] = { 0.0f };
   float y_expected[] = { 0.0f };
   cblas_srot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "srot(case 590)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "srot(case 591)");
     }
   };
  };


  {
   int N = 1;
   float c = 0.866025403784f;
   float s = 0.5f;
   float X[] = { -0.201f };
   int incX = -1;
   float Y[] = { 0.087f };
   int incY = -1;
   float x_expected[] = { -0.130571f };
   float y_expected[] = { 0.175844f };
   cblas_srot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "srot(case 592)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "srot(case 593)");
     }
   };
  };


  {
   int N = 1;
   float c = 0.0f;
   float s = -1.0f;
   float X[] = { -0.201f };
   int incX = -1;
   float Y[] = { 0.087f };
   int incY = -1;
   float x_expected[] = { -0.087f };
   float y_expected[] = { -0.201f };
   cblas_srot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "srot(case 594)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "srot(case 595)");
     }
   };
  };


  {
   int N = 1;
   float c = -1.0f;
   float s = 0.0f;
   float X[] = { -0.201f };
   int incX = -1;
   float Y[] = { 0.087f };
   int incY = -1;
   float x_expected[] = { 0.201f };
   float y_expected[] = { -0.087f };
   cblas_srot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], flteps, "srot(case 596)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], flteps, "srot(case 597)");
     }
   };
  };


  {
   int N = 1;
   double c = 0;
   double s = 0;
   double X[] = { -0.464 };
   int incX = -1;
   double Y[] = { 0.7 };
   int incY = -1;
   double x_expected[] = { 0.0 };
   double y_expected[] = { 0.0 };
   cblas_drot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "drot(case 598)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "drot(case 599)");
     }
   };
  };


  {
   int N = 1;
   double c = 0.866025403784;
   double s = 0.5;
   double X[] = { -0.464 };
   int incX = -1;
   double Y[] = { 0.7 };
   int incY = -1;
   double x_expected[] = { -0.051835787356 };
   double y_expected[] = { 0.838217782649 };
   cblas_drot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "drot(case 600)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "drot(case 601)");
     }
   };
  };


  {
   int N = 1;
   double c = 0;
   double s = -1;
   double X[] = { -0.464 };
   int incX = -1;
   double Y[] = { 0.7 };
   int incY = -1;
   double x_expected[] = { -0.7 };
   double y_expected[] = { -0.464 };
   cblas_drot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "drot(case 602)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "drot(case 603)");
     }
   };
  };


  {
   int N = 1;
   double c = -1;
   double s = 0;
   double X[] = { -0.464 };
   int incX = -1;
   double Y[] = { 0.7 };
   int incY = -1;
   double x_expected[] = { 0.464 };
   double y_expected[] = { -0.7 };
   cblas_drot(N, X, incX, Y, incY, c, s);
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(X[i], x_expected[i], dbleps, "drot(case 604)");
     }
   };
   {
     int i;
     for (i = 0; i < 1; i++) {
       gsl_test_rel(Y[i], y_expected[i], dbleps, "drot(case 605)");
     }
   };
  };


}
