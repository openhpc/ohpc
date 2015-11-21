/* dht/test_dht.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 Gerard Jungman
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/* Author:  G. Jungman
 */
#include <config.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_dht.h>


/* Test exact small transform.
 */
int
test_dht_exact(void)
{
  int stat = 0;
  double f_in[3] = { 1.0, 2.0, 3.0 };
  double f_out[3];
  gsl_dht * t = gsl_dht_new(3, 1.0, 1.0);
  gsl_dht_apply(t, f_in, f_out);

  /* Check values. */
  if(fabs( f_out[0]-( 0.375254649407520))/0.375254649407520 > 1.0e-14) stat++;
  if(fabs( f_out[1]-(-0.133507872695560))/0.133507872695560 > 1.0e-14) stat++;
  if(fabs( f_out[2]-( 0.044679925143840))/0.044679925143840 > 1.0e-14) stat++;


  /* Check inverse.
   * We have to adjust the normalization
   * so we can use the same precalculated transform.
   */
  gsl_dht_apply(t, f_out, f_in);
  f_in[0] *= 13.323691936314223*13.323691936314223;  /* jzero[1,4]^2 */
  f_in[1] *= 13.323691936314223*13.323691936314223;
  f_in[2] *= 13.323691936314223*13.323691936314223;

  /* The loss of precision on the inverse
   * is a little surprising. However, this
   * thing is quite tricky since the band-limited
   * function represented by the samples {1,2,3}
   * need not be very nice. Like in any spectral
   * application, you really have to have some
   * a-priori knowledge of the underlying function.
   */
  if(fabs( f_in[0]-1.0)/1.0 > 2.0e-05) stat++;
  if(fabs( f_in[1]-2.0)/2.0 > 2.0e-05) stat++;
  if(fabs( f_in[2]-3.0)/3.0 > 2.0e-05) stat++;

  gsl_dht_free(t);

  return stat;
}



/* Test the transform
 * Integrate[x J_0(a x) / (x^2 + 1), {x,0,Inf}] = K_0(a)
 */
int
test_dht_simple(void)
{
  int stat = 0;
  int n;
  double f_in[128];
  double f_out[128];
  gsl_dht * t = gsl_dht_new(128, 0.0, 100.0);

  for(n=0; n<128; n++) {
    const double x = gsl_dht_x_sample(t, n);
    f_in[n] = 1.0/(1.0+x*x);
  }

  gsl_dht_apply(t, f_in, f_out);

  /* This is a difficult transform to calculate this way,
   * since it does not satisfy the boundary condition and
   * it dies quite slowly. So it is not meaningful to
   * compare this to high accuracy. We only check
   * that it seems to be working.
   */
  if(fabs( f_out[0]-4.00)/4.00 > 0.02) stat++;
  if(fabs( f_out[5]-1.84)/1.84 > 0.02) stat++;
  if(fabs(f_out[10]-1.27)/1.27 > 0.02) stat++;
  if(fabs(f_out[35]-0.352)/0.352 > 0.02) stat++;
  if(fabs(f_out[100]-0.0237)/0.0237 > 0.02) stat++;

  gsl_dht_free(t);

  return stat;
}


/* Test the transform
 * Integrate[ x exp(-x) J_1(a x), {x,0,Inf}] = a F(3/2, 2; 2; -a^2)
 */
int
test_dht_exp1(void)
{
  int stat = 0;
  int n;
  double f_in[128];
  double f_out[128];
  gsl_dht * t = gsl_dht_new(128, 1.0, 20.0);

  for(n=0; n<128; n++) {
    const double x = gsl_dht_x_sample(t, n);
    f_in[n] = exp(-x);
  }

  gsl_dht_apply(t, f_in, f_out);

  /* Spot check.
   * Note that the systematic errors in the calculation
   * are quite large, so it is meaningless to compare
   * to a high accuracy.
   */
  if(fabs( f_out[0]-0.181)/0.181 > 0.02) stat++;
  if(fabs( f_out[5]-0.357)/0.357 > 0.02) stat++;
  if(fabs(f_out[10]-0.211)/0.211 > 0.02) stat++;
  if(fabs(f_out[35]-0.0289)/0.0289 > 0.02) stat++;
  if(fabs(f_out[100]-0.00221)/0.00211 > 0.02) stat++;

  gsl_dht_free(t);

  return stat;
}


/* Test the transform
 * Integrate[ x^2 (1-x^2) J_1(a x), {x,0,1}] = 2/a^2 J_3(a)
 */
int
test_dht_poly1(void)
{
  int stat = 0;
  int n;
  double f_in[128];
  double f_out[128];
  gsl_dht * t = gsl_dht_new(128, 1.0, 1.0);

  for(n=0; n<128; n++) {
    const double x = gsl_dht_x_sample(t, n);
    f_in[n] = x * (1.0 - x*x);
  }

  gsl_dht_apply(t, f_in, f_out);

  /* Spot check. This function satisfies the boundary condition,
   * so the accuracy should be ok.
   */
  if(fabs( f_out[0]-0.057274214)/0.057274214    > 1.0e-07) stat++;
  if(fabs( f_out[5]-(-0.000190850))/0.000190850 > 1.0e-05) stat++;
  if(fabs(f_out[10]-0.000024342)/0.000024342    > 1.0e-04) stat++;
  if(fabs(f_out[35]-(-4.04e-07))/4.04e-07       > 1.0e-03) stat++;
  if(fabs(f_out[100]-1.0e-08)/1.0e-08           > 0.25)    stat++;

  gsl_dht_free(t);

  return stat;
}


int main()
{
  gsl_ieee_env_setup ();

  gsl_test( test_dht_exact(),   "Small Exact DHT");
  gsl_test( test_dht_simple(),  "Simple  DHT");
  gsl_test( test_dht_exp1(),    "Exp  J1 DHT");
  gsl_test( test_dht_poly1(),   "Poly J1 DHT");

  exit (gsl_test_summary());
}
