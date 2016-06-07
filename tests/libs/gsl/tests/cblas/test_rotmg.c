#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_rotmg (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   float d1 = -1630.28519312f;
   float d2 = 44320.1964703f;
   float b1 = 1274.7681352f;
   float b2 = 0.983006912864f;
   float h[] = { -999.0f, -999.1f, -999.2f, -999.3f, -999.4f };
   float d1_expected = 0.0f;
   float d2_expected = 0.0f;
   float b1_expected = 0.0f;
   float h0_expected = -1.0f;
   float h11_expected = 0.0f;
   float h21_expected = 0.0f;
   float h12_expected = 0.0f;
   float h22_expected = 0.0f;
   cblas_srotmg(&d1, &d2, &b1, b2, h);
   gsl_test_rel(d1, d1_expected, flteps, "srotmg(case 606)");
   gsl_test_rel(d2, d2_expected, flteps, "srotmg(case 607)");
   gsl_test_rel(b1, b1_expected, flteps, "srotmg(case 608)");
   gsl_test_rel(h[0], h0_expected, flteps, "srotmg(case 609)");
   gsl_test_rel(h[1], h11_expected, flteps, "srotmg(case 610)");
   gsl_test_rel(h[2], h21_expected, flteps, "srotmg(case 611)");
   gsl_test_rel(h[3], h12_expected, flteps, "srotmg(case 612)");
   gsl_test_rel(h[4], h22_expected, flteps, "srotmg(case 613)");
  };


  {
   double d1 = 0.0890831089656;
   double d2 = 24998.3892082;
   double b1 = 34657.8864443;
   double b2 = 1.27708980357;
   double h[] = { -999.0, -999.1, -999.2, -999.3, -999.4 };
   double d1_expected = 0.0890491788526;
   double d2_expected = 24988.8677829;
   double b1_expected = 34671.0920237;
   double h0_expected = 0;
   double h11_expected = -999.1;
   double h21_expected = -3.6848461767e-05;
   double h12_expected = 10.34036867;
   double h22_expected = -999.4;
   cblas_drotmg(&d1, &d2, &b1, b2, h);
   gsl_test_rel(d1, d1_expected, dbleps, "drotmg(case 614)");
   gsl_test_rel(d2, d2_expected, dbleps, "drotmg(case 615)");
   gsl_test_rel(b1, b1_expected, dbleps, "drotmg(case 616)");
   gsl_test_rel(h[0], h0_expected, dbleps, "drotmg(case 617)");
   gsl_test_rel(h[1], h11_expected, dbleps, "drotmg(case 618)");
   gsl_test_rel(h[2], h21_expected, dbleps, "drotmg(case 619)");
   gsl_test_rel(h[3], h12_expected, dbleps, "drotmg(case 620)");
   gsl_test_rel(h[4], h22_expected, dbleps, "drotmg(case 621)");
  };


  {
   float d1 = 0.00100326116366f;
   float d2 = -1.20359225232f;
   float b1 = -7.45489498808f;
   float b2 = 0.159616854019f;
   float h[] = { -999.0f, -999.1f, -999.2f, -999.3f, -999.4f };
   float d1_expected = 0.00222932574734f;
   float d2_expected = -2.67447728926f;
   float b1_expected = -3.35491869218f;
   float h0_expected = 0.0f;
   float h11_expected = -999.1f;
   float h21_expected = 0.0214110130692f;
   float h12_expected = 25.6863620142f;
   float h22_expected = -999.4f;
   cblas_srotmg(&d1, &d2, &b1, b2, h);
   gsl_test_rel(d1, d1_expected, flteps, "srotmg(case 622)");
   gsl_test_rel(d2, d2_expected, flteps, "srotmg(case 623)");
   gsl_test_rel(b1, b1_expected, flteps, "srotmg(case 624)");
   gsl_test_rel(h[0], h0_expected, flteps, "srotmg(case 625)");
   gsl_test_rel(h[1], h11_expected, flteps, "srotmg(case 626)");
   gsl_test_rel(h[2], h21_expected, flteps, "srotmg(case 627)");
   gsl_test_rel(h[3], h12_expected, flteps, "srotmg(case 628)");
   gsl_test_rel(h[4], h22_expected, flteps, "srotmg(case 629)");
  };


  {
   double d1 = -49.1978123005;
   double d2 = 0.228703451277;
   double b1 = 1.8901039144;
   double b2 = 7081.47754386;
   double h[] = { -999.0, -999.1, -999.2, -999.3, -999.4 };
   double d1_expected = 0;
   double d2_expected = 0;
   double b1_expected = 0;
   double h0_expected = -1;
   double h11_expected = 0;
   double h21_expected = 0;
   double h12_expected = 0;
   double h22_expected = 0;
   cblas_drotmg(&d1, &d2, &b1, b2, h);
   gsl_test_rel(d1, d1_expected, dbleps, "drotmg(case 630)");
   gsl_test_rel(d2, d2_expected, dbleps, "drotmg(case 631)");
   gsl_test_rel(b1, b1_expected, dbleps, "drotmg(case 632)");
   gsl_test_rel(h[0], h0_expected, dbleps, "drotmg(case 633)");
   gsl_test_rel(h[1], h11_expected, dbleps, "drotmg(case 634)");
   gsl_test_rel(h[2], h21_expected, dbleps, "drotmg(case 635)");
   gsl_test_rel(h[3], h12_expected, dbleps, "drotmg(case 636)");
   gsl_test_rel(h[4], h22_expected, dbleps, "drotmg(case 637)");
  };


  {
   float d1 = 0.00760694276009f;
   float d2 = -1.07649167228f;
   float b1 = -22584.0076391f;
   float b2 = -0.00305597817159f;
   float h[] = { -999.0f, -999.1f, -999.2f, -999.3f, -999.4f };
   float d1_expected = 0.00760694276011f;
   float d2_expected = -1.07649167228f;
   float b1_expected = -22584.007639f;
   float h0_expected = 0.0f;
   float h11_expected = -999.1f;
   float h21_expected = -1.35316026298e-07f;
   float h12_expected = -1.91491615001e-05f;
   float h22_expected = -999.4f;
   cblas_srotmg(&d1, &d2, &b1, b2, h);
   gsl_test_rel(d1, d1_expected, flteps, "srotmg(case 638)");
   gsl_test_rel(d2, d2_expected, flteps, "srotmg(case 639)");
   gsl_test_rel(b1, b1_expected, flteps, "srotmg(case 640)");
   gsl_test_rel(h[0], h0_expected, flteps, "srotmg(case 641)");
   gsl_test_rel(h[1], h11_expected, flteps, "srotmg(case 642)");
   gsl_test_rel(h[2], h21_expected, flteps, "srotmg(case 643)");
   gsl_test_rel(h[3], h12_expected, flteps, "srotmg(case 644)");
   gsl_test_rel(h[4], h22_expected, flteps, "srotmg(case 645)");
  };


  {
   double d1 = 0.000283076346391;
   double d2 = 20.1907649901;
   double b1 = -0.274927034914;
   double b2 = 18.6645358259;
   double h[] = { -999.0, -999.1, -999.2, -999.3, -999.4 };
   double d1_expected = 20.1907649287;
   double d2_expected = 0.00028307634553;
   double b1_expected = 18.6645358827;
   double h0_expected = 1;
   double h11_expected = -2.06514743478e-07;
   double h21_expected = -999.2;
   double h12_expected = -999.3;
   double h22_expected = -0.0147299154652;
   cblas_drotmg(&d1, &d2, &b1, b2, h);
   gsl_test_rel(d1, d1_expected, dbleps, "drotmg(case 646)");
   gsl_test_rel(d2, d2_expected, dbleps, "drotmg(case 647)");
   gsl_test_rel(b1, b1_expected, dbleps, "drotmg(case 648)");
   gsl_test_rel(h[0], h0_expected, dbleps, "drotmg(case 649)");
   gsl_test_rel(h[1], h11_expected, dbleps, "drotmg(case 650)");
   gsl_test_rel(h[2], h21_expected, dbleps, "drotmg(case 651)");
   gsl_test_rel(h[3], h12_expected, dbleps, "drotmg(case 652)");
   gsl_test_rel(h[4], h22_expected, dbleps, "drotmg(case 653)");
  };


}
