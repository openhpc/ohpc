/* multifit/test_linear.c
 * 
 * Copyright (C) 2007, 2013 Brian Gough, Patrick Alken
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

static void
test_random_vector(gsl_vector *v, const gsl_rng *r,
                   const double lower, const double upper)
{
  size_t i;
  size_t N = v->size;

  for (i = 0; i < N; ++i)
    {
      gsl_vector_set(v, i,
                     gsl_rng_uniform(r) * (upper - lower) + lower);
    }
}

static void
test_random_matrix(gsl_matrix *m, const gsl_rng *r,
                   const double lower, const double upper)
{
  size_t i, j;
  size_t M = m->size1;
  size_t N = m->size2;

  for (i = 0; i < M; ++i)
    {
      for (j = 0; j < N; ++j)
      {
        gsl_matrix_set(m, i, j,
                       gsl_rng_uniform(r) * (upper - lower) + lower);
      }
    }
}

static void
test_random_vector_noise(const gsl_rng *r, gsl_vector *y)
{
  size_t i;

  for (i = 0; i < y->size; ++i)
    {
      double *ptr = gsl_vector_ptr(y, i);
      *ptr += 1.0e-3 * gsl_rng_uniform(r);
    }
}

#include "test_longley.c"
#include "test_filip.c"
#include "test_pontius.c"
#include "test_estimator.c"
#include "test_reg.c"
#include "test_shaw.c"

/* test linear regression */

void
test_linear(void)
{
  test_longley();
  test_filip();
  test_pontius();
  test_estimator();
  test_reg();
  test_shaw();
}
