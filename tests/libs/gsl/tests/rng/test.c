/* rng/test.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 James Theiler, Brian Gough
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

#include <config.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>

void rng_test (const gsl_rng_type * T, unsigned long int seed, unsigned int n,
               unsigned long int result);
void rng_float_test (const gsl_rng_type * T);
void generic_rng_test (const gsl_rng_type * T);
void rng_state_test (const gsl_rng_type * T);
void rng_parallel_state_test (const gsl_rng_type * T);
void rng_read_write_test (const gsl_rng_type * T);
int rng_max_test (gsl_rng * r, unsigned long int *kmax, unsigned long int ran_max) ;
int rng_min_test (gsl_rng * r, unsigned long int *kmin, unsigned long int ran_min, unsigned long int ran_max) ;
int rng_sum_test (gsl_rng * r, double *sigma);
int rng_bin_test (gsl_rng * r, double *sigma);
void rng_seed_test (const gsl_rng_type * T);

#define N  10000
#define N2 200000

int
main (void)
{
  const gsl_rng_type ** rngs = gsl_rng_types_setup();  /* get all rng types */

  const gsl_rng_type ** r ;

  gsl_ieee_env_setup ();

  gsl_rng_env_setup ();

  /* specific tests of known results for 10000 iterations with seed = 1 */

  rng_test (gsl_rng_rand, 1, 10000, 1910041713);
  rng_test (gsl_rng_randu, 1, 10000, 1623524161);
  rng_test (gsl_rng_cmrg, 1, 10000, 719452880);
  rng_test (gsl_rng_minstd, 1, 10000, 1043618065);
  rng_test (gsl_rng_mrg, 1, 10000, 2064828650);
  rng_test (gsl_rng_taus, 1, 10000, 2733957125UL);
  rng_test (gsl_rng_taus2, 1, 10000, 2733957125UL);
  rng_test (gsl_rng_taus113, 1, 1000, 1925420673UL);
  rng_test (gsl_rng_transputer, 1, 10000, 1244127297UL);
  rng_test (gsl_rng_vax, 1, 10000, 3051034865UL);

  /* Borosh13 test value from PARI: (1812433253^10000)%(2^32) */
  rng_test (gsl_rng_borosh13, 1, 10000, 2513433025UL);

  /* Fishman18 test value from PARI: (62089911^10000)%(2^31-1) */
  rng_test (gsl_rng_fishman18, 1, 10000, 330402013UL);

  /* Fishman2x test value from PARI: 
     ((48271^10000)%(2^31-1) - (40692^10000)%(2^31-249))%(2^31-1) */
  rng_test (gsl_rng_fishman2x, 1, 10000, 540133597UL);

  /* Knuthran2 test value from PARI: 
     { xn1=1; xn2=1; for (n=1,10000, 
            xn = (271828183*xn1 - 314159269*xn2)%(2^31-1);
            xn2=xn1; xn1=xn; print(xn); ) } */
  rng_test (gsl_rng_knuthran2, 1, 10000, 1084477620UL);

  /* Knuthran test value taken from p188 in Knuth Vol 2. 3rd Ed */
  rng_test (gsl_rng_knuthran, 310952, 1009 * 2009 + 1, 461390032);

  /* Knuthran improved test value from Knuth's source */
  rng_test (gsl_rng_knuthran2002, 310952, 1, 708622036);
  rng_test (gsl_rng_knuthran2002, 310952, 2, 1005450560);
  rng_test (gsl_rng_knuthran2002, 310952, 100 * 2009 + 1, 995235265);
  rng_test (gsl_rng_knuthran2002, 310952, 1009 * 2009 + 1, 704987132);

  /* Lecuyer21 test value from PARI: (40692^10000)%(2^31-249) */
  rng_test (gsl_rng_lecuyer21, 1, 10000, 2006618587UL);

  /* Waterman14 test value from PARI: (1566083941^10000)%(2^32) */
  rng_test (gsl_rng_waterman14, 1, 10000, 3776680385UL);

  /* specific tests of known results for 10000 iterations with seed = 6 */

  /* Coveyou test value from PARI:
     x=6; for(n=1,10000,x=(x*(x+1))%(2^32);print(x);) */

  rng_test (gsl_rng_coveyou, 6, 10000, 1416754246UL);

  /* Fishman20 test value from PARI: (6*48271^10000)%(2^31-1) */
  rng_test (gsl_rng_fishman20, 6, 10000, 248127575UL);

  /* FIXME: the ranlux tests below were made by running the fortran code and
     getting the expected value from that. An analytic calculation
     would be preferable. */

  rng_test (gsl_rng_ranlux, 314159265, 10000, 12077992);
  rng_test (gsl_rng_ranlux389, 314159265, 10000, 165942);

  rng_test (gsl_rng_ranlxs0, 1, 10000, 11904320);
  /* 0.709552764892578125 * ldexp(1.0,24) */

  rng_test (gsl_rng_ranlxs1, 1, 10000, 8734328);
  /* 0.520606517791748047 * ldexp(1.0,24) */

  rng_test (gsl_rng_ranlxs2, 1, 10000, 6843140); 
  /* 0.407882928848266602 * ldexp(1.0,24) */

  rng_test (gsl_rng_ranlxd1, 1, 10000, 1998227290UL);
  /* 0.465248546261094020 * ldexp(1.0,32) */

  rng_test (gsl_rng_ranlxd2, 1, 10000, 3949287736UL);
  /* 0.919515205581550532 * ldexp(1.0,32) */

  /* FIXME: the tests below were made by running the original code in
     the ../random directory and getting the expected value from
     that. An analytic calculation would be preferable. */

  rng_test (gsl_rng_slatec, 1, 10000, 45776);
  rng_test (gsl_rng_uni, 1, 10000, 9214);
  rng_test (gsl_rng_uni32, 1, 10000, 1155229825);
  rng_test (gsl_rng_zuf, 1, 10000, 3970);

  /* The tests below were made by running the original code and
     getting the expected value from that. An analytic calculation
     would be preferable. */

  rng_test (gsl_rng_r250, 1, 10000, 1100653588);
  rng_test (gsl_rng_mt19937, 4357, 1000, 1186927261);
  rng_test (gsl_rng_mt19937_1999, 4357, 1000, 1030650439);
  rng_test (gsl_rng_mt19937_1998, 4357, 1000, 1309179303);
  rng_test (gsl_rng_tt800, 0, 10000, 2856609219UL);

  rng_test (gsl_rng_ran0, 0, 10000, 1115320064);
  rng_test (gsl_rng_ran1, 0, 10000, 1491066076);
  rng_test (gsl_rng_ran2, 0, 10000, 1701364455);
  rng_test (gsl_rng_ran3, 0, 10000, 186340785);

  rng_test (gsl_rng_ranmar, 1, 10000, 14428370);

  rng_test (gsl_rng_rand48, 0, 10000, 0xDE095043UL);
  rng_test (gsl_rng_rand48, 1, 10000, 0xEDA54977UL);

  rng_test (gsl_rng_random_glibc2, 0, 10000, 1908609430);
  rng_test (gsl_rng_random8_glibc2, 0, 10000, 1910041713);
  rng_test (gsl_rng_random32_glibc2, 0, 10000, 1587395585);
  rng_test (gsl_rng_random64_glibc2, 0, 10000, 52848624);
  rng_test (gsl_rng_random128_glibc2, 0, 10000, 1908609430);
  rng_test (gsl_rng_random256_glibc2, 0, 10000, 179943260);

  rng_test (gsl_rng_random_bsd, 0, 10000, 1457025928);
  rng_test (gsl_rng_random8_bsd, 0, 10000, 1910041713);
  rng_test (gsl_rng_random32_bsd, 0, 10000, 1663114331);
  rng_test (gsl_rng_random64_bsd, 0, 10000, 864469165);
  rng_test (gsl_rng_random128_bsd, 0, 10000, 1457025928);
  rng_test (gsl_rng_random256_bsd, 0, 10000, 1216357476);

  rng_test (gsl_rng_random_libc5, 0, 10000, 428084942);
  rng_test (gsl_rng_random8_libc5, 0, 10000, 1910041713);
  rng_test (gsl_rng_random32_libc5, 0, 10000, 1967452027);
  rng_test (gsl_rng_random64_libc5, 0, 10000, 2106639801);
  rng_test (gsl_rng_random128_libc5, 0, 10000, 428084942);
  rng_test (gsl_rng_random256_libc5, 0, 10000, 116367984);

  rng_test (gsl_rng_ranf, 0, 10000, 2152890433UL);
  rng_test (gsl_rng_ranf, 2, 10000, 339327233);

  /* Test constant relationship between int and double functions */

  for (r = rngs ; *r != 0; r++)
    rng_float_test (*r);

  /* Test save/restore functions */

  for (r = rngs ; *r != 0; r++)
    rng_state_test (*r);

  for (r = rngs ; *r != 0; r++)
    rng_parallel_state_test (*r);

  for (r = rngs ; *r != 0; r++)
    rng_read_write_test (*r);

  /* generic statistical tests (these are just to make sure that we
     don't get any crazy results back from the generator, i.e. they
     aren't a test of the algorithm, just the implementation) */

  for (r = rngs ; *r != 0; r++)
    generic_rng_test (*r);

#ifdef TEST_SEEDS
  rng_seed_test (gsl_rng_mt19937);
  rng_seed_test (gsl_rng_ranlxs0);
  rng_seed_test (gsl_rng_ranlxs1);
  rng_seed_test (gsl_rng_ranlxs2);

  rng_seed_test (gsl_rng_ranlxd1);
  rng_seed_test (gsl_rng_ranlxd2);
  rng_seed_test (gsl_rng_ranlux);
  rng_seed_test (gsl_rng_ranlux389);

  rng_seed_test (gsl_rng_cmrg);
  rng_seed_test (gsl_rng_mrg);
  rng_seed_test (gsl_rng_taus);
  rng_seed_test (gsl_rng_taus2);

  rng_seed_test (gsl_rng_gfsr4);
#endif

  exit (gsl_test_summary ());
}


void
rng_test (const gsl_rng_type * T, unsigned long int seed, unsigned int n,
          unsigned long int result)
{
  gsl_rng *r = gsl_rng_alloc (T);
  unsigned int i;
  unsigned long int k = 0;
  int status;

  if (seed != 0)
    {
      gsl_rng_set (r, seed);
    }

  for (i = 0; i < n; i++)
    {
      k = gsl_rng_get (r);
    }

  status = (k != result);
  gsl_test (status, "%s, %u steps (%u observed vs %u expected)",
            gsl_rng_name (r), n, k, result);

  gsl_rng_free (r);
}

void
rng_float_test (const gsl_rng_type * T)
{
  gsl_rng *ri = gsl_rng_alloc (T);
  gsl_rng *rf = gsl_rng_alloc (T);

  double u, c ; 
  unsigned int i;
  unsigned long int k = 0;
  int status = 0 ;

  do 
    {
      k = gsl_rng_get (ri);
      u = gsl_rng_get (rf);
    } 
  while (k == 0) ;

  c = k / u ;
  for (i = 0; i < N2; i++)
    {
      k = gsl_rng_get (ri);
      u = gsl_rng_get (rf);
      if (c*k != u)
        {
          status = 1 ;
          break ;
        }
    }

  gsl_test (status, "%s, ratio of int to double (%g observed vs %g expected)",
            gsl_rng_name (ri), c, k/u);

  gsl_rng_free (ri);
  gsl_rng_free (rf);
}


void
rng_state_test (const gsl_rng_type * T)
{
  unsigned long int test_a[N], test_b[N];

  int i;

  gsl_rng *r = gsl_rng_alloc (T);
  gsl_rng *r_save = gsl_rng_alloc (T);

  for (i = 0; i < N; ++i)
    {
      gsl_rng_get (r);  /* throw away N iterations */
    }

  gsl_rng_memcpy (r_save, r);   /* save the intermediate state */

  for (i = 0; i < N; ++i)
    {
      test_a[i] = gsl_rng_get (r);
    }

  gsl_rng_memcpy (r, r_save);   /* restore the intermediate state */
  gsl_rng_free (r_save);

  for (i = 0; i < N; ++i)
    {
      test_b[i] = gsl_rng_get (r);
    }

  {
    int status = 0;
    for (i = 0; i < N; ++i)
      {
        status |= (test_b[i] != test_a[i]);
      }
    gsl_test (status, "%s, random number state consistency",
              gsl_rng_name (r));
  }

  gsl_rng_free (r);
}


void
rng_parallel_state_test (const gsl_rng_type * T)
{
  unsigned long int test_a[N], test_b[N];
  unsigned long int test_c[N], test_d[N];
  double test_e[N], test_f[N];

  int i;

  gsl_rng *r1 = gsl_rng_alloc (T);
  gsl_rng *r2 = gsl_rng_alloc (T);

  for (i = 0; i < N; ++i)
    {
      gsl_rng_get (r1);         /* throw away N iterations */
    }

  gsl_rng_memcpy (r2, r1);              /* save the intermediate state */

  for (i = 0; i < N; ++i)
    {
      /* check that there is no hidden state intermixed between r1 and r2 */
      test_a[i] = gsl_rng_get (r1);     
      test_b[i] = gsl_rng_get (r2);
      test_c[i] = gsl_rng_uniform_int (r1, 1234);       
      test_d[i] = gsl_rng_uniform_int (r2, 1234);
      test_e[i] = gsl_rng_uniform (r1); 
      test_f[i] = gsl_rng_uniform (r2);
    }

  {
    int status = 0;
    for (i = 0; i < N; ++i)
      {
        status |= (test_b[i] != test_a[i]);
        status |= (test_c[i] != test_d[i]);
        status |= (test_e[i] != test_f[i]);
      }
    gsl_test (status, "%s, parallel random number state consistency",
              gsl_rng_name (r1));
  }

  gsl_rng_free (r1);
  gsl_rng_free (r2);

}

void
rng_read_write_test (const gsl_rng_type * T)
{
  unsigned long int test_a[N], test_b[N];

  int i;

  gsl_rng *r = gsl_rng_alloc (T);

  for (i = 0; i < N; ++i)
    {
      gsl_rng_get (r);  /* throw away N iterations */
    }

  { /* save the state to a binary file */
    FILE *f = fopen("test.dat", "wb"); 
    gsl_rng_fwrite(f, r);
    fclose(f);
  }

  for (i = 0; i < N; ++i)
    {
      test_a[i] = gsl_rng_get (r);
    }

  { /* read the state from a binary file */
    FILE *f = fopen("test.dat", "rb"); 
    gsl_rng_fread(f, r);
    fclose(f);
  }

  for (i = 0; i < N; ++i)
    {
      test_b[i] = gsl_rng_get (r);
    }

  {
    int status = 0;
    for (i = 0; i < N; ++i)
      {
        status |= (test_b[i] != test_a[i]);
      }
    gsl_test (status, "%s, random number generator read and write",
              gsl_rng_name (r));
  }

  gsl_rng_free (r);
}

void
generic_rng_test (const gsl_rng_type * T)
{
  gsl_rng *r = gsl_rng_alloc (T);
  const char *name = gsl_rng_name (r);
  unsigned long int kmax = 0, kmin = 1000;
  double sigma = 0;
  const unsigned long int ran_max = gsl_rng_max (r);
  const unsigned long int ran_min = gsl_rng_min (r);

  int status = rng_max_test (r, &kmax, ran_max);

  gsl_test (status,
            "%s, observed vs theoretical maximum (%lu vs %lu)",
            name, kmax, ran_max);

  status = rng_min_test (r, &kmin, ran_min, ran_max);

  gsl_test (status,
            "%s, observed vs theoretical minimum (%lu vs %lu)",
            name, kmin, ran_min);

  status = rng_sum_test (r, &sigma);

  gsl_test (status,
            "%s, sum test within acceptable sigma (observed %.2g sigma)",
            name, sigma);

  status = rng_bin_test (r, &sigma);

  gsl_test (status,
            "%s, bin test within acceptable chisq (observed %.2g sigma)",
            name, sigma);

  gsl_rng_set (r, 1);   /* set seed to 1 */
  status = rng_max_test (r, &kmax, ran_max);

  gsl_rng_set (r, 1);   /* set seed to 1 */
  status |= rng_min_test (r, &kmin, ran_min, ran_max);

  gsl_rng_set (r, 1);   /* set seed to 1 */
  status |= rng_sum_test (r, &sigma);

  gsl_test (status, "%s, max, min and sum tests for seed=1", name);

  gsl_rng_set (r, 12345);       /* set seed to a "typical" value */
  status = rng_max_test (r, &kmax, ran_max);

  gsl_rng_set (r, 12345);       /* set seed to a "typical" value */
  status |= rng_min_test (r, &kmin, ran_min, ran_max);

  gsl_rng_set (r, 12345);       /* set seed to a "typical" value */
  status |= rng_sum_test (r, &sigma);

  gsl_test (status, "%s, max, min and sum tests for non-default seeds", name);

  gsl_rng_free (r);
}

int
rng_max_test (gsl_rng * r, unsigned long int *kmax, unsigned long int ran_max)
{
  unsigned long int actual_uncovered;
  double expect_uncovered;
  int status;
  unsigned long int max = 0;
  int i;

  for (i = 0; i < N2; ++i)
    {
      unsigned long int k = gsl_rng_get (r);
      if (k > max)
        max = k;
    }

  *kmax = max;

  actual_uncovered = ran_max - max;
  expect_uncovered = (double) ran_max / (double) N2;

  status = (max > ran_max) || (actual_uncovered > 7 * expect_uncovered) ;

  return status;
}

int
rng_min_test (gsl_rng * r, unsigned long int *kmin, 
              unsigned long int ran_min, unsigned long int ran_max)
{
  unsigned long int actual_uncovered;
  double expect_uncovered;
  int status;
  unsigned long int min = 1000000000UL;
  int i;

  for (i = 0; i < N2; ++i)
    {
      unsigned long int k = gsl_rng_get (r);
      if (k < min)
        min = k;
    }

  *kmin = min;

  actual_uncovered = min - ran_min;
  expect_uncovered = (double) ran_max / (double) N2;

  status = (min < ran_min) || (actual_uncovered > 7 * expect_uncovered);

  return status;
}

int
rng_sum_test (gsl_rng * r, double *sigma)
{
  double sum = 0;
  int i, status;

  for (i = 0; i < N2; ++i)
    {
      double x = gsl_rng_uniform (r) - 0.5;
      sum += x;
    }

  sum /= N2;

  /* expect the average to have a variance of 1/(12 n) */

  *sigma = sum * sqrt (12.0 * N2);

  /* more than 3 sigma is an error (increased to 3.1 to avoid false alarms) */
  
  status = (fabs (*sigma) > 3.1 || fabs(*sigma) < 0.003);

  if (status) {
      fprintf(stderr,"sum=%g, sigma=%g\n",sum,*sigma);
  }

  return status;
}

#define BINS 17
#define EXTRA 10
int
rng_bin_test (gsl_rng * r, double *sigma)
{
  int count[BINS+EXTRA];
  double chisq = 0;
  int i, status;

  for (i = 0; i < BINS+EXTRA; i++)
      count[i] = 0 ;


  for (i = 0; i < N2; i++)
    {
      int j = gsl_rng_uniform_int (r, BINS);
      count[j]++ ;
    }

  chisq = 0 ;
  for (i = 0; i < BINS; i++)
    {
      double x = (double)N2/(double)BINS ;
      double d = (count[i] - x) ;
      chisq += (d*d) / x;
    }

  *sigma = sqrt(chisq/BINS) ;

  /* more than 3 sigma is an error */
  
  status = (fabs (*sigma) > 3 || fabs(*sigma) < 0.003);

  for (i = BINS; i < BINS+EXTRA; i++)
    {
      if (count[i] != 0)
        {
          status = 1 ;
          gsl_test (status, 
                    "%s, wrote outside range in bin test "
                    "(%d observed vs %d expected)",
                    gsl_rng_name(r), i, BINS - 1);
        }
    }

  return status;
}

int
rng_sanity_test (gsl_rng * r)
{
  double sum = 0, sigma;
  int i, status = 0;

  for (i = 0; i < N2; ++i)
    {
      double x = gsl_rng_uniform (r);
      sum += x;
      if (x < 0.0 || x > 1.0) {
        fprintf(stderr,"x=%g out of range\n", x);
        return 1;
      }
    }

  sum /= N2;

  /* expect the average to have a variance of 1/(12 n) */

  sigma = (sum - 0.5)* sqrt (12.0 * N2);

  /* more than 5 sigma is an error */
  
  status = (fabs (sigma) > 5 || fabs(sigma) < 0.0001);

  if (status) {
      fprintf(stderr,"sum=%g, sigma=%g\n",sum,sigma);
  }

  return status;
}

void
rng_seed_test (const gsl_rng_type * T)
{
  gsl_rng *r = gsl_rng_alloc (T);
  const char *name = gsl_rng_name (r);
  unsigned long int i;
  int j;
  int status;

  for (i = 0xFFFFFFFFUL ; i > 0; i >>= 1) {
    for (j = 1; j >= -1 ; j--) {
      unsigned long int seed = i + j;
      if (j > 0 && seed < i) continue;
      gsl_rng_set (r, seed);
      status = rng_sanity_test (r);
      if (status) 
        gsl_test (status, "%s, sanity tests for seed = %#lx", name, seed);
    }
  }
}
