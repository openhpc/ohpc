#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_cblas.h>

#include "tests.h"

void
test_rotg (void) {
const double flteps = 1e-4, dbleps = 1e-6;
  {
   float a = -1.5f;
   float b = -1.5f;
   float c;
   float s;
   float r_expected = -2.12132034356f;
   float z_expected = 1.41421356237f;
   float c_expected = 0.707106781187f;
   float s_expected = 0.707106781187f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 166)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 167)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 168)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 169)");
  };


  {
   float a = -1.5f;
   float b = -1.0f;
   float c;
   float s;
   float r_expected = -1.80277563773f;
   float z_expected = 0.554700196225f;
   float c_expected = 0.832050294338f;
   float s_expected = 0.554700196225f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 170)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 171)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 172)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 173)");
  };


  {
   float a = -1.5f;
   float b = -0.1f;
   float c;
   float s;
   float r_expected = -1.50332963784f;
   float z_expected = 0.0665190105238f;
   float c_expected = 0.997785157857f;
   float s_expected = 0.0665190105238f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 174)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 175)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 176)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 177)");
  };


  {
   float a = -1.5f;
   float b = 0.0f;
   float c;
   float s;
   float r_expected = -1.5f;
   float z_expected = -0.0f;
   float c_expected = 1.0f;
   float s_expected = -0.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 178)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 179)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 180)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 181)");
  };


  {
   float a = -1.5f;
   float b = 0.1f;
   float c;
   float s;
   float r_expected = -1.50332963784f;
   float z_expected = -0.0665190105238f;
   float c_expected = 0.997785157857f;
   float s_expected = -0.0665190105238f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 182)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 183)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 184)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 185)");
  };


  {
   float a = -1.5f;
   float b = 1.0f;
   float c;
   float s;
   float r_expected = -1.80277563773f;
   float z_expected = -0.554700196225f;
   float c_expected = 0.832050294338f;
   float s_expected = -0.554700196225f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 186)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 187)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 188)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 189)");
  };


  {
   float a = -1.5f;
   float b = 1.5f;
   float c;
   float s;
   float r_expected = 2.12132034356f;
   float z_expected = -1.41421356237f;
   float c_expected = -0.707106781187f;
   float s_expected = 0.707106781187f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 190)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 191)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 192)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 193)");
  };


  {
   float a = -1.0f;
   float b = -1.5f;
   float c;
   float s;
   float r_expected = -1.80277563773f;
   float z_expected = 1.80277563773f;
   float c_expected = 0.554700196225f;
   float s_expected = 0.832050294338f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 194)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 195)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 196)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 197)");
  };


  {
   float a = -1.0f;
   float b = -1.0f;
   float c;
   float s;
   float r_expected = -1.41421356237f;
   float z_expected = 1.41421356237f;
   float c_expected = 0.707106781187f;
   float s_expected = 0.707106781187f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 198)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 199)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 200)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 201)");
  };


  {
   float a = -1.0f;
   float b = -0.1f;
   float c;
   float s;
   float r_expected = -1.00498756211f;
   float z_expected = 0.099503719021f;
   float c_expected = 0.99503719021f;
   float s_expected = 0.099503719021f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 202)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 203)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 204)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 205)");
  };


  {
   float a = -1.0f;
   float b = 0.0f;
   float c;
   float s;
   float r_expected = -1.0f;
   float z_expected = -0.0f;
   float c_expected = 1.0f;
   float s_expected = -0.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 206)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 207)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 208)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 209)");
  };


  {
   float a = -1.0f;
   float b = 0.1f;
   float c;
   float s;
   float r_expected = -1.00498756211f;
   float z_expected = -0.099503719021f;
   float c_expected = 0.99503719021f;
   float s_expected = -0.099503719021f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 210)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 211)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 212)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 213)");
  };


  {
   float a = -1.0f;
   float b = 1.0f;
   float c;
   float s;
   float r_expected = 1.41421356237f;
   float z_expected = -1.41421356237f;
   float c_expected = -0.707106781187f;
   float s_expected = 0.707106781187f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 214)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 215)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 216)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 217)");
  };


  {
   float a = -1.0f;
   float b = 1.5f;
   float c;
   float s;
   float r_expected = 1.80277563773f;
   float z_expected = -1.80277563773f;
   float c_expected = -0.554700196225f;
   float s_expected = 0.832050294338f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 218)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 219)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 220)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 221)");
  };


  {
   float a = -0.1f;
   float b = -1.5f;
   float c;
   float s;
   float r_expected = -1.50332963784f;
   float z_expected = 15.0332963784f;
   float c_expected = 0.0665190105238f;
   float s_expected = 0.997785157857f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 222)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 223)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 224)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 225)");
  };


  {
   float a = -0.1f;
   float b = -1.0f;
   float c;
   float s;
   float r_expected = -1.00498756211f;
   float z_expected = 10.0498756211f;
   float c_expected = 0.099503719021f;
   float s_expected = 0.99503719021f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 226)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 227)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 228)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 229)");
  };


  {
   float a = -0.1f;
   float b = -0.1f;
   float c;
   float s;
   float r_expected = -0.141421356237f;
   float z_expected = 1.41421356237f;
   float c_expected = 0.707106781187f;
   float s_expected = 0.707106781187f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 230)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 231)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 232)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 233)");
  };


  {
   float a = -0.1f;
   float b = 0.0f;
   float c;
   float s;
   float r_expected = -0.1f;
   float z_expected = -0.0f;
   float c_expected = 1.0f;
   float s_expected = -0.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 234)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 235)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 236)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 237)");
  };


  {
   float a = -0.1f;
   float b = 0.1f;
   float c;
   float s;
   float r_expected = 0.141421356237f;
   float z_expected = -1.41421356237f;
   float c_expected = -0.707106781187f;
   float s_expected = 0.707106781187f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 238)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 239)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 240)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 241)");
  };


  {
   float a = -0.1f;
   float b = 1.0f;
   float c;
   float s;
   float r_expected = 1.00498756211f;
   float z_expected = -10.0498756211f;
   float c_expected = -0.099503719021f;
   float s_expected = 0.99503719021f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 242)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 243)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 244)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 245)");
  };


  {
   float a = -0.1f;
   float b = 1.5f;
   float c;
   float s;
   float r_expected = 1.50332963784f;
   float z_expected = -15.0332963784f;
   float c_expected = -0.0665190105238f;
   float s_expected = 0.997785157857f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 246)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 247)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 248)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 249)");
  };


  {
   float a = 0.0f;
   float b = -1.5f;
   float c;
   float s;
   float r_expected = -1.5f;
   float z_expected = 1.0f;
   float c_expected = -0.0f;
   float s_expected = 1.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 250)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 251)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 252)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 253)");
  };


  {
   float a = 0.0f;
   float b = -1.0f;
   float c;
   float s;
   float r_expected = -1.0f;
   float z_expected = 1.0f;
   float c_expected = -0.0f;
   float s_expected = 1.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 254)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 255)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 256)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 257)");
  };


  {
   float a = 0.0f;
   float b = -0.1f;
   float c;
   float s;
   float r_expected = -0.1f;
   float z_expected = 1.0f;
   float c_expected = -0.0f;
   float s_expected = 1.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 258)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 259)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 260)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 261)");
  };


  {
   float a = 0.0f;
   float b = 0.0f;
   float c;
   float s;
   float r_expected = 0.0f;
   float z_expected = 0.0f;
   float c_expected = 1.0f;
   float s_expected = 0.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 262)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 263)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 264)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 265)");
  };


  {
   float a = 0.0f;
   float b = 0.1f;
   float c;
   float s;
   float r_expected = 0.1f;
   float z_expected = 1.0f;
   float c_expected = 0.0f;
   float s_expected = 1.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 266)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 267)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 268)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 269)");
  };


  {
   float a = 0.0f;
   float b = 1.0f;
   float c;
   float s;
   float r_expected = 1.0f;
   float z_expected = 1.0f;
   float c_expected = 0.0f;
   float s_expected = 1.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 270)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 271)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 272)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 273)");
  };


  {
   float a = 0.0f;
   float b = 1.5f;
   float c;
   float s;
   float r_expected = 1.5f;
   float z_expected = 1.0f;
   float c_expected = 0.0f;
   float s_expected = 1.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 274)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 275)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 276)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 277)");
  };


  {
   float a = 0.1f;
   float b = -1.5f;
   float c;
   float s;
   float r_expected = -1.50332963784f;
   float z_expected = -15.0332963784f;
   float c_expected = -0.0665190105238f;
   float s_expected = 0.997785157857f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 278)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 279)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 280)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 281)");
  };


  {
   float a = 0.1f;
   float b = -1.0f;
   float c;
   float s;
   float r_expected = -1.00498756211f;
   float z_expected = -10.0498756211f;
   float c_expected = -0.099503719021f;
   float s_expected = 0.99503719021f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 282)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 283)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 284)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 285)");
  };


  {
   float a = 0.1f;
   float b = -0.1f;
   float c;
   float s;
   float r_expected = -0.141421356237f;
   float z_expected = -1.41421356237f;
   float c_expected = -0.707106781187f;
   float s_expected = 0.707106781187f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 286)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 287)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 288)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 289)");
  };


  {
   float a = 0.1f;
   float b = 0.0f;
   float c;
   float s;
   float r_expected = 0.1f;
   float z_expected = 0.0f;
   float c_expected = 1.0f;
   float s_expected = 0.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 290)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 291)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 292)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 293)");
  };


  {
   float a = 0.1f;
   float b = 0.1f;
   float c;
   float s;
   float r_expected = 0.141421356237f;
   float z_expected = 1.41421356237f;
   float c_expected = 0.707106781187f;
   float s_expected = 0.707106781187f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 294)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 295)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 296)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 297)");
  };


  {
   float a = 0.1f;
   float b = 1.0f;
   float c;
   float s;
   float r_expected = 1.00498756211f;
   float z_expected = 10.0498756211f;
   float c_expected = 0.099503719021f;
   float s_expected = 0.99503719021f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 298)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 299)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 300)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 301)");
  };


  {
   float a = 0.1f;
   float b = 1.5f;
   float c;
   float s;
   float r_expected = 1.50332963784f;
   float z_expected = 15.0332963784f;
   float c_expected = 0.0665190105238f;
   float s_expected = 0.997785157857f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 302)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 303)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 304)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 305)");
  };


  {
   float a = 1.0f;
   float b = -1.5f;
   float c;
   float s;
   float r_expected = -1.80277563773f;
   float z_expected = -1.80277563773f;
   float c_expected = -0.554700196225f;
   float s_expected = 0.832050294338f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 306)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 307)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 308)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 309)");
  };


  {
   float a = 1.0f;
   float b = -1.0f;
   float c;
   float s;
   float r_expected = -1.41421356237f;
   float z_expected = -1.41421356237f;
   float c_expected = -0.707106781187f;
   float s_expected = 0.707106781187f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 310)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 311)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 312)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 313)");
  };


  {
   float a = 1.0f;
   float b = -0.1f;
   float c;
   float s;
   float r_expected = 1.00498756211f;
   float z_expected = -0.099503719021f;
   float c_expected = 0.99503719021f;
   float s_expected = -0.099503719021f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 314)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 315)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 316)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 317)");
  };


  {
   float a = 1.0f;
   float b = 0.0f;
   float c;
   float s;
   float r_expected = 1.0f;
   float z_expected = 0.0f;
   float c_expected = 1.0f;
   float s_expected = 0.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 318)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 319)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 320)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 321)");
  };


  {
   float a = 1.0f;
   float b = 0.1f;
   float c;
   float s;
   float r_expected = 1.00498756211f;
   float z_expected = 0.099503719021f;
   float c_expected = 0.99503719021f;
   float s_expected = 0.099503719021f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 322)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 323)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 324)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 325)");
  };


  {
   float a = 1.0f;
   float b = 1.0f;
   float c;
   float s;
   float r_expected = 1.41421356237f;
   float z_expected = 1.41421356237f;
   float c_expected = 0.707106781187f;
   float s_expected = 0.707106781187f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 326)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 327)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 328)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 329)");
  };


  {
   float a = 1.0f;
   float b = 1.5f;
   float c;
   float s;
   float r_expected = 1.80277563773f;
   float z_expected = 1.80277563773f;
   float c_expected = 0.554700196225f;
   float s_expected = 0.832050294338f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 330)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 331)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 332)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 333)");
  };


  {
   float a = 1.5f;
   float b = -1.5f;
   float c;
   float s;
   float r_expected = -2.12132034356f;
   float z_expected = -1.41421356237f;
   float c_expected = -0.707106781187f;
   float s_expected = 0.707106781187f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 334)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 335)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 336)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 337)");
  };


  {
   float a = 1.5f;
   float b = -1.0f;
   float c;
   float s;
   float r_expected = 1.80277563773f;
   float z_expected = -0.554700196225f;
   float c_expected = 0.832050294338f;
   float s_expected = -0.554700196225f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 338)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 339)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 340)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 341)");
  };


  {
   float a = 1.5f;
   float b = -0.1f;
   float c;
   float s;
   float r_expected = 1.50332963784f;
   float z_expected = -0.0665190105238f;
   float c_expected = 0.997785157857f;
   float s_expected = -0.0665190105238f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 342)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 343)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 344)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 345)");
  };


  {
   float a = 1.5f;
   float b = 0.0f;
   float c;
   float s;
   float r_expected = 1.5f;
   float z_expected = 0.0f;
   float c_expected = 1.0f;
   float s_expected = 0.0f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 346)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 347)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 348)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 349)");
  };


  {
   float a = 1.5f;
   float b = 0.1f;
   float c;
   float s;
   float r_expected = 1.50332963784f;
   float z_expected = 0.0665190105238f;
   float c_expected = 0.997785157857f;
   float s_expected = 0.0665190105238f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 350)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 351)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 352)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 353)");
  };


  {
   float a = 1.5f;
   float b = 1.0f;
   float c;
   float s;
   float r_expected = 1.80277563773f;
   float z_expected = 0.554700196225f;
   float c_expected = 0.832050294338f;
   float s_expected = 0.554700196225f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 354)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 355)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 356)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 357)");
  };


  {
   float a = 1.5f;
   float b = 1.5f;
   float c;
   float s;
   float r_expected = 2.12132034356f;
   float z_expected = 1.41421356237f;
   float c_expected = 0.707106781187f;
   float s_expected = 0.707106781187f;
   cblas_srotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, flteps, "srotg(case 358)");
   gsl_test_rel(b, z_expected, flteps, "srotg(case 359)");
   gsl_test_rel(c, c_expected, flteps, "srotg(case 360)");
   gsl_test_rel(s, s_expected, flteps, "srotg(case 361)");
  };


  {
   double a = -1.5;
   double b = -1.5;
   double c;
   double s;
   double r_expected = -2.12132034356;
   double z_expected = 1.41421356237;
   double c_expected = 0.707106781187;
   double s_expected = 0.707106781187;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 362)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 363)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 364)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 365)");
  };


  {
   double a = -1.5;
   double b = -1;
   double c;
   double s;
   double r_expected = -1.80277563773;
   double z_expected = 0.554700196225;
   double c_expected = 0.832050294338;
   double s_expected = 0.554700196225;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 366)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 367)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 368)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 369)");
  };


  {
   double a = -1.5;
   double b = -0.1;
   double c;
   double s;
   double r_expected = -1.50332963784;
   double z_expected = 0.0665190105238;
   double c_expected = 0.997785157857;
   double s_expected = 0.0665190105238;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 370)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 371)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 372)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 373)");
  };


  {
   double a = -1.5;
   double b = 0;
   double c;
   double s;
   double r_expected = -1.5;
   double z_expected = -0;
   double c_expected = 1;
   double s_expected = -0;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 374)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 375)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 376)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 377)");
  };


  {
   double a = -1.5;
   double b = 0.1;
   double c;
   double s;
   double r_expected = -1.50332963784;
   double z_expected = -0.0665190105238;
   double c_expected = 0.997785157857;
   double s_expected = -0.0665190105238;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 378)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 379)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 380)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 381)");
  };


  {
   double a = -1.5;
   double b = 1;
   double c;
   double s;
   double r_expected = -1.80277563773;
   double z_expected = -0.554700196225;
   double c_expected = 0.832050294338;
   double s_expected = -0.554700196225;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 382)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 383)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 384)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 385)");
  };


  {
   double a = -1.5;
   double b = 1.5;
   double c;
   double s;
   double r_expected = 2.12132034356;
   double z_expected = -1.41421356237;
   double c_expected = -0.707106781187;
   double s_expected = 0.707106781187;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 386)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 387)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 388)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 389)");
  };


  {
   double a = -1;
   double b = -1.5;
   double c;
   double s;
   double r_expected = -1.80277563773;
   double z_expected = 1.80277563773;
   double c_expected = 0.554700196225;
   double s_expected = 0.832050294338;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 390)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 391)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 392)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 393)");
  };


  {
   double a = -1;
   double b = -1;
   double c;
   double s;
   double r_expected = -1.41421356237;
   double z_expected = 1.41421356237;
   double c_expected = 0.707106781187;
   double s_expected = 0.707106781187;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 394)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 395)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 396)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 397)");
  };


  {
   double a = -1;
   double b = -0.1;
   double c;
   double s;
   double r_expected = -1.00498756211;
   double z_expected = 0.099503719021;
   double c_expected = 0.99503719021;
   double s_expected = 0.099503719021;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 398)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 399)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 400)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 401)");
  };


  {
   double a = -1;
   double b = 0;
   double c;
   double s;
   double r_expected = -1;
   double z_expected = -0;
   double c_expected = 1;
   double s_expected = -0;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 402)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 403)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 404)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 405)");
  };


  {
   double a = -1;
   double b = 0.1;
   double c;
   double s;
   double r_expected = -1.00498756211;
   double z_expected = -0.099503719021;
   double c_expected = 0.99503719021;
   double s_expected = -0.099503719021;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 406)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 407)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 408)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 409)");
  };


  {
   double a = -1;
   double b = 1;
   double c;
   double s;
   double r_expected = 1.41421356237;
   double z_expected = -1.41421356237;
   double c_expected = -0.707106781187;
   double s_expected = 0.707106781187;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 410)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 411)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 412)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 413)");
  };


  {
   double a = -1;
   double b = 1.5;
   double c;
   double s;
   double r_expected = 1.80277563773;
   double z_expected = -1.80277563773;
   double c_expected = -0.554700196225;
   double s_expected = 0.832050294338;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 414)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 415)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 416)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 417)");
  };


  {
   double a = -0.1;
   double b = -1.5;
   double c;
   double s;
   double r_expected = -1.50332963784;
   double z_expected = 15.0332963784;
   double c_expected = 0.0665190105238;
   double s_expected = 0.997785157857;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 418)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 419)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 420)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 421)");
  };


  {
   double a = -0.1;
   double b = -1;
   double c;
   double s;
   double r_expected = -1.00498756211;
   double z_expected = 10.0498756211;
   double c_expected = 0.099503719021;
   double s_expected = 0.99503719021;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 422)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 423)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 424)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 425)");
  };


  {
   double a = -0.1;
   double b = -0.1;
   double c;
   double s;
   double r_expected = -0.141421356237;
   double z_expected = 1.41421356237;
   double c_expected = 0.707106781187;
   double s_expected = 0.707106781187;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 426)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 427)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 428)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 429)");
  };


  {
   double a = -0.1;
   double b = 0;
   double c;
   double s;
   double r_expected = -0.1;
   double z_expected = -0;
   double c_expected = 1;
   double s_expected = -0;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 430)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 431)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 432)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 433)");
  };


  {
   double a = -0.1;
   double b = 0.1;
   double c;
   double s;
   double r_expected = 0.141421356237;
   double z_expected = -1.41421356237;
   double c_expected = -0.707106781187;
   double s_expected = 0.707106781187;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 434)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 435)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 436)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 437)");
  };


  {
   double a = -0.1;
   double b = 1;
   double c;
   double s;
   double r_expected = 1.00498756211;
   double z_expected = -10.0498756211;
   double c_expected = -0.099503719021;
   double s_expected = 0.99503719021;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 438)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 439)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 440)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 441)");
  };


  {
   double a = -0.1;
   double b = 1.5;
   double c;
   double s;
   double r_expected = 1.50332963784;
   double z_expected = -15.0332963784;
   double c_expected = -0.0665190105238;
   double s_expected = 0.997785157857;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 442)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 443)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 444)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 445)");
  };


  {
   double a = 0;
   double b = -1.5;
   double c;
   double s;
   double r_expected = -1.5;
   double z_expected = 1;
   double c_expected = -0;
   double s_expected = 1;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 446)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 447)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 448)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 449)");
  };


  {
   double a = 0;
   double b = -1;
   double c;
   double s;
   double r_expected = -1;
   double z_expected = 1;
   double c_expected = -0;
   double s_expected = 1;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 450)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 451)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 452)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 453)");
  };


  {
   double a = 0;
   double b = -0.1;
   double c;
   double s;
   double r_expected = -0.1;
   double z_expected = 1;
   double c_expected = -0;
   double s_expected = 1;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 454)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 455)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 456)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 457)");
  };


  {
   double a = 0;
   double b = 0;
   double c;
   double s;
   double r_expected = 0;
   double z_expected = 0;
   double c_expected = 1;
   double s_expected = 0;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 458)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 459)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 460)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 461)");
  };


  {
   double a = 0;
   double b = 0.1;
   double c;
   double s;
   double r_expected = 0.1;
   double z_expected = 1;
   double c_expected = 0;
   double s_expected = 1;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 462)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 463)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 464)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 465)");
  };


  {
   double a = 0;
   double b = 1;
   double c;
   double s;
   double r_expected = 1;
   double z_expected = 1;
   double c_expected = 0;
   double s_expected = 1;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 466)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 467)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 468)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 469)");
  };


  {
   double a = 0;
   double b = 1.5;
   double c;
   double s;
   double r_expected = 1.5;
   double z_expected = 1;
   double c_expected = 0;
   double s_expected = 1;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 470)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 471)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 472)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 473)");
  };


  {
   double a = 0.1;
   double b = -1.5;
   double c;
   double s;
   double r_expected = -1.50332963784;
   double z_expected = -15.0332963784;
   double c_expected = -0.0665190105238;
   double s_expected = 0.997785157857;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 474)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 475)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 476)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 477)");
  };


  {
   double a = 0.1;
   double b = -1;
   double c;
   double s;
   double r_expected = -1.00498756211;
   double z_expected = -10.0498756211;
   double c_expected = -0.099503719021;
   double s_expected = 0.99503719021;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 478)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 479)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 480)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 481)");
  };


  {
   double a = 0.1;
   double b = -0.1;
   double c;
   double s;
   double r_expected = -0.141421356237;
   double z_expected = -1.41421356237;
   double c_expected = -0.707106781187;
   double s_expected = 0.707106781187;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 482)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 483)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 484)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 485)");
  };


  {
   double a = 0.1;
   double b = 0;
   double c;
   double s;
   double r_expected = 0.1;
   double z_expected = 0;
   double c_expected = 1;
   double s_expected = 0;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 486)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 487)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 488)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 489)");
  };


  {
   double a = 0.1;
   double b = 0.1;
   double c;
   double s;
   double r_expected = 0.141421356237;
   double z_expected = 1.41421356237;
   double c_expected = 0.707106781187;
   double s_expected = 0.707106781187;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 490)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 491)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 492)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 493)");
  };


  {
   double a = 0.1;
   double b = 1;
   double c;
   double s;
   double r_expected = 1.00498756211;
   double z_expected = 10.0498756211;
   double c_expected = 0.099503719021;
   double s_expected = 0.99503719021;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 494)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 495)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 496)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 497)");
  };


  {
   double a = 0.1;
   double b = 1.5;
   double c;
   double s;
   double r_expected = 1.50332963784;
   double z_expected = 15.0332963784;
   double c_expected = 0.0665190105238;
   double s_expected = 0.997785157857;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 498)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 499)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 500)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 501)");
  };


  {
   double a = 1;
   double b = -1.5;
   double c;
   double s;
   double r_expected = -1.80277563773;
   double z_expected = -1.80277563773;
   double c_expected = -0.554700196225;
   double s_expected = 0.832050294338;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 502)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 503)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 504)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 505)");
  };


  {
   double a = 1;
   double b = -1;
   double c;
   double s;
   double r_expected = -1.41421356237;
   double z_expected = -1.41421356237;
   double c_expected = -0.707106781187;
   double s_expected = 0.707106781187;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 506)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 507)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 508)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 509)");
  };


  {
   double a = 1;
   double b = -0.1;
   double c;
   double s;
   double r_expected = 1.00498756211;
   double z_expected = -0.099503719021;
   double c_expected = 0.99503719021;
   double s_expected = -0.099503719021;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 510)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 511)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 512)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 513)");
  };


  {
   double a = 1;
   double b = 0;
   double c;
   double s;
   double r_expected = 1;
   double z_expected = 0;
   double c_expected = 1;
   double s_expected = 0;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 514)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 515)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 516)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 517)");
  };


  {
   double a = 1;
   double b = 0.1;
   double c;
   double s;
   double r_expected = 1.00498756211;
   double z_expected = 0.099503719021;
   double c_expected = 0.99503719021;
   double s_expected = 0.099503719021;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 518)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 519)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 520)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 521)");
  };


  {
   double a = 1;
   double b = 1;
   double c;
   double s;
   double r_expected = 1.41421356237;
   double z_expected = 1.41421356237;
   double c_expected = 0.707106781187;
   double s_expected = 0.707106781187;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 522)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 523)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 524)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 525)");
  };


  {
   double a = 1;
   double b = 1.5;
   double c;
   double s;
   double r_expected = 1.80277563773;
   double z_expected = 1.80277563773;
   double c_expected = 0.554700196225;
   double s_expected = 0.832050294338;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 526)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 527)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 528)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 529)");
  };


  {
   double a = 1.5;
   double b = -1.5;
   double c;
   double s;
   double r_expected = -2.12132034356;
   double z_expected = -1.41421356237;
   double c_expected = -0.707106781187;
   double s_expected = 0.707106781187;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 530)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 531)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 532)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 533)");
  };


  {
   double a = 1.5;
   double b = -1;
   double c;
   double s;
   double r_expected = 1.80277563773;
   double z_expected = -0.554700196225;
   double c_expected = 0.832050294338;
   double s_expected = -0.554700196225;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 534)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 535)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 536)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 537)");
  };


  {
   double a = 1.5;
   double b = -0.1;
   double c;
   double s;
   double r_expected = 1.50332963784;
   double z_expected = -0.0665190105238;
   double c_expected = 0.997785157857;
   double s_expected = -0.0665190105238;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 538)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 539)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 540)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 541)");
  };


  {
   double a = 1.5;
   double b = 0;
   double c;
   double s;
   double r_expected = 1.5;
   double z_expected = 0;
   double c_expected = 1;
   double s_expected = 0;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 542)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 543)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 544)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 545)");
  };


  {
   double a = 1.5;
   double b = 0.1;
   double c;
   double s;
   double r_expected = 1.50332963784;
   double z_expected = 0.0665190105238;
   double c_expected = 0.997785157857;
   double s_expected = 0.0665190105238;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 546)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 547)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 548)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 549)");
  };


  {
   double a = 1.5;
   double b = 1;
   double c;
   double s;
   double r_expected = 1.80277563773;
   double z_expected = 0.554700196225;
   double c_expected = 0.832050294338;
   double s_expected = 0.554700196225;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 550)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 551)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 552)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 553)");
  };


  {
   double a = 1.5;
   double b = 1.5;
   double c;
   double s;
   double r_expected = 2.12132034356;
   double z_expected = 1.41421356237;
   double c_expected = 0.707106781187;
   double s_expected = 0.707106781187;
   cblas_drotg(&a, &b, &c, &s);
   gsl_test_rel(a, r_expected, dbleps, "drotg(case 554)");
   gsl_test_rel(b, z_expected, dbleps, "drotg(case 555)");
   gsl_test_rel(c, c_expected, dbleps, "drotg(case 556)");
   gsl_test_rel(s, s_expected, dbleps, "drotg(case 557)");
  };


}
