/* matrix/test_source.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 Gerard Jungman, Brian Gough
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

void FUNCTION (test, func) (const size_t M, const size_t N);
void FUNCTION (test, ops) (const size_t M, const size_t N);
void FUNCTION (test, trap) (const size_t M, const size_t N);
void FUNCTION (test, text) (const size_t M, const size_t N);
void FUNCTION (test, binary) (const size_t M, const size_t N);
void FUNCTION (test, binary_noncontiguous) (const size_t M, const size_t N);

#define TEST(expr,desc) gsl_test((expr), NAME(gsl_matrix) desc " M=%d, N=%d", M, N)

void
FUNCTION (test, func) (const size_t M, const size_t N)
{
  TYPE (gsl_vector) * v;
  size_t i, j;
  size_t k = 0;

  TYPE (gsl_matrix) * m = FUNCTION (gsl_matrix, alloc) (M, N);

  gsl_test (m->data == 0, NAME (gsl_matrix) "_alloc returns valid pointer");
  gsl_test (m->size1 != M, NAME (gsl_matrix) "_alloc returns valid size1");
  gsl_test (m->size2 != N, NAME (gsl_matrix) "_alloc returns valid size2");
  gsl_test (m->tda != N, NAME (gsl_matrix) "_alloc returns valid tda");

  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
          k++;
          FUNCTION (gsl_matrix, set) (m, i, j, (BASE) k);
        }
    }

  {
    status = 0;
    k = 0;
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            k++;
            if (m->data[i * N + j] != (BASE) k)
              status = 1;
          };
      };

    gsl_test (status, NAME (gsl_matrix) "_set writes into array");
  }

  {
    status = 0;
    k = 0;
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            k++;
            if (FUNCTION (gsl_matrix, get) (m, i, j) != (BASE) k)
              status = 1;
          };
      };
    gsl_test (status, NAME (gsl_matrix) "_get reads from array");
  }


  FUNCTION (gsl_matrix, free) (m);      /* free whatever is in m */

  m = FUNCTION (gsl_matrix, calloc) (M, N);
  v = FUNCTION (gsl_vector, calloc) (N);

  {
    int status = (FUNCTION(gsl_matrix,isnull)(m) != 1);
    TEST (status, "_isnull" DESC " on calloc matrix");
    
    status = (FUNCTION(gsl_matrix,ispos)(m) != 0);
    TEST (status, "_ispos" DESC " on calloc matrix");
    
    status = (FUNCTION(gsl_matrix,isneg)(m) != 0);
    TEST (status, "_isneg" DESC " on calloc matrix");

    status = (FUNCTION(gsl_matrix,isnonneg)(m) != 1);
    TEST (status, "_isnonneg" DESC " on calloc matrix");
  }


  k = 0;
  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
          k++;
          FUNCTION (gsl_matrix, set) (m, i, j, (BASE) k);
        }
    }


  {
    status = 0;
    k = 0;
    for (i = 0; i < M; i++)
      {
        FUNCTION (gsl_matrix, get_row) (v, m, i);

        for (j = 0; j < N; j++)
          {
            k++;
            if (v->data[j] != (BASE) k)
              status = 1;
          }
      }

    gsl_test (status, NAME (gsl_matrix) "_get_row extracts row");
  }

  {
    BASE exp_max = FUNCTION(gsl_matrix, get) (m, 0, 0);
    BASE exp_min = FUNCTION(gsl_matrix, get) (m, 0, 0);
    size_t exp_imax = 0, exp_jmax = 0, exp_imin = 0, exp_jmin = 0;

    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            BASE k = FUNCTION(gsl_matrix, get) (m, i, j);
            if (k > exp_max) {
              exp_max =  FUNCTION(gsl_matrix, get) (m, i, j);
              exp_imax = i;
              exp_jmax = j;
            }
            if (k < exp_min) {
              exp_min =  FUNCTION(gsl_matrix, get) (m, i, j);
              exp_imin = i;
              exp_jmin = j;
            }
          }
      }

    {
      BASE max = FUNCTION(gsl_matrix, max) (m) ;

      gsl_test (max != exp_max, NAME(gsl_matrix) "_max returns correct maximum value");
    }

    {
      BASE min = FUNCTION(gsl_matrix, min) (m) ;
      
      gsl_test (min != exp_min, NAME(gsl_matrix) "_min returns correct minimum value");
    }

    {
      BASE min, max;
      FUNCTION(gsl_matrix, minmax) (m, &min, &max);

      gsl_test (max != exp_max, NAME(gsl_matrix) "_minmax returns correct maximum value");
      gsl_test (min != exp_min, NAME(gsl_matrix) "_minmax returns correct minimum value");
    }


    {
      size_t imax, jmax;
      FUNCTION(gsl_matrix, max_index) (m, &imax, &jmax) ;

      gsl_test (imax != exp_imax, NAME(gsl_matrix) "_max_index returns correct maximum i");
      gsl_test (jmax != exp_jmax, NAME(gsl_matrix) "_max_index returns correct maximum j");
    }

    {
      size_t imin, jmin;
      FUNCTION(gsl_matrix, min_index) (m, &imin, &jmin) ;

      gsl_test (imin != exp_imin, NAME(gsl_matrix) "_min_index returns correct minimum i");
      gsl_test (jmin != exp_jmin, NAME(gsl_matrix) "_min_index returns correct minimum j");
    }

    {
      size_t imin, jmin, imax, jmax;

      FUNCTION(gsl_matrix, minmax_index) (m,  &imin, &jmin, &imax, &jmax);

      gsl_test (imax != exp_imax, NAME(gsl_matrix) "_minmax_index returns correct maximum i");
      gsl_test (jmax != exp_jmax, NAME(gsl_matrix) "_minmax_index returns correct maximum j");

      gsl_test (imin != exp_imin, NAME(gsl_matrix) "_minmax_index returns correct minimum i");
      gsl_test (jmin != exp_jmin, NAME(gsl_matrix) "_minmax_index returns correct minimum j");
    }

#if FP
    FUNCTION(gsl_matrix,set)(m, 2, 3, GSL_NAN);
    exp_min = GSL_NAN; exp_max = GSL_NAN;
    exp_imin = 2; exp_jmin = 3;
    exp_imax = 2; exp_jmax = 3;

    {
      BASE max = FUNCTION(gsl_matrix, max) (m) ;

      gsl_test_abs (max,exp_max, 0, NAME(gsl_matrix) "_max returns correct maximum value for NaN");
    }

    {
      BASE min = FUNCTION(gsl_matrix, min) (m) ;
      
      gsl_test_abs (min, exp_min, 0, NAME(gsl_matrix) "_min returns correct minimum value for NaN");
    }

    {
      BASE min, max;
      FUNCTION(gsl_matrix, minmax) (m, &min, &max);

      gsl_test_abs (max, exp_max, 0, NAME(gsl_matrix) "_minmax returns correct maximum value for NaN");
      gsl_test_abs (min, exp_min, 0, NAME(gsl_matrix) "_minmax returns correct minimum value for NaN");
    }


    {
      size_t imax, jmax;
      FUNCTION(gsl_matrix, max_index) (m, &imax, &jmax) ;

      gsl_test (imax != exp_imax, NAME(gsl_matrix) "_max_index returns correct maximum i for NaN");
      gsl_test (jmax != exp_jmax, NAME(gsl_matrix) "_max_index returns correct maximum j for NaN");
    }

    {
      size_t imin, jmin;
      FUNCTION(gsl_matrix, min_index) (m, &imin, &jmin) ;

      gsl_test (imin != exp_imin, NAME(gsl_matrix) "_min_index returns correct minimum i for NaN");
      gsl_test (jmin != exp_jmin, NAME(gsl_matrix) "_min_index returns correct minimum j for NaN");
    }

    {
      size_t imin, jmin, imax, jmax;

      FUNCTION(gsl_matrix, minmax_index) (m,  &imin, &jmin, &imax, &jmax);

      gsl_test (imax != exp_imax, NAME(gsl_matrix) "_minmax_index returns correct maximum i for NaN");
      gsl_test (jmax != exp_jmax, NAME(gsl_matrix) "_minmax_index returns correct maximum j for NaN");

      gsl_test (imin != exp_imin, NAME(gsl_matrix) "_minmax_index returns correct minimum i for NaN");
      gsl_test (jmin != exp_jmin, NAME(gsl_matrix) "_minmax_index returns correct minimum j for NaN");
    }
#endif 


  }


  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
          FUNCTION (gsl_matrix, set) (m, i, j, (ATOMIC) 0);
        }
    }

  {
    status = (FUNCTION(gsl_matrix,isnull)(m) != 1);
    TEST (status, "_isnull" DESC " on null matrix") ;

    status = (FUNCTION(gsl_matrix,ispos)(m) != 0);
    TEST (status, "_ispos" DESC " on null matrix") ;

    status = (FUNCTION(gsl_matrix,isneg)(m) != 0);
    TEST (status, "_isneg" DESC " on null matrix") ;

    status = (FUNCTION(gsl_matrix,isnonneg)(m) != 1);
    TEST (status, "_isnonneg" DESC " on null matrix") ;
  }


  k = 0;
  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
          k++;
          FUNCTION (gsl_matrix, set) (m, i, j, (ATOMIC) (k % 10));
        }
    }

  {
    status = (FUNCTION(gsl_matrix,isnull)(m) != 0);
    TEST (status, "_isnull" DESC " on non-negative matrix") ;

    status = (FUNCTION(gsl_matrix,ispos)(m) != 0);
    TEST (status, "_ispos" DESC " on non-negative matrix") ;

    status = (FUNCTION(gsl_matrix,isneg)(m) != 0);
    TEST (status, "_isneg" DESC " on non-negative matrix") ;

    status = (FUNCTION(gsl_matrix,isnonneg)(m) != 1);
    TEST (status, "_isnonneg" DESC " on non-negative matrix") ;
  }

#ifndef UNSIGNED
  k = 0;
  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
          ATOMIC mij = ((++k) % 10)  - (ATOMIC) 5;
          FUNCTION (gsl_matrix, set) (m, i, j, mij);
        }
    }

  {
    status = (FUNCTION(gsl_matrix,isnull)(m) != 0);
    TEST (status, "_isnull" DESC " on mixed matrix") ;

    status = (FUNCTION(gsl_matrix,ispos)(m) != 0);
    TEST (status, "_ispos" DESC " on mixed matrix") ;

    status = (FUNCTION(gsl_matrix,isneg)(m) != 0);
    TEST (status, "_isneg" DESC " on mixed matrix") ;

    status = (FUNCTION(gsl_matrix,isnonneg)(m) != 0);
    TEST (status, "_isnonneg" DESC " on mixed matrix") ;
  }

  k = 0;
  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
          k++;
          FUNCTION (gsl_matrix, set) (m, i, j, -(ATOMIC) (k % 10));
        }
    }

  {
    status = (FUNCTION(gsl_matrix,isnull)(m) != 0);
    TEST (status, "_isnull" DESC " on non-positive matrix") ;

    status = (FUNCTION(gsl_matrix,ispos)(m) != 0);
    TEST (status, "_ispos" DESC " on non-positive matrix") ;

    status = (FUNCTION(gsl_matrix,isneg)(m) != 0);
    TEST (status, "_isneg" DESC " on non-positive matrix") ;

    status = (FUNCTION(gsl_matrix,isnonneg)(m) != 0);
    TEST (status, "_isnonneg" DESC " on non-positive matrix") ;
  }
#endif

  k = 0;
  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
          k++;
          FUNCTION (gsl_matrix, set) (m, i, j, (ATOMIC) (k % 10 + 1));
        }
    }

  {
    status = (FUNCTION(gsl_matrix,isnull)(m) != 0);
    TEST (status, "_isnull" DESC " on positive matrix") ;

    status = (FUNCTION(gsl_matrix,ispos)(m) != 1);
    TEST (status, "_ispos" DESC " on positive matrix") ;

    status = (FUNCTION(gsl_matrix,isneg)(m) != 0);
    TEST (status, "_isneg" DESC " on positive matrix") ;

    status = (FUNCTION(gsl_matrix,isnonneg)(m) != 1);
    TEST (status, "_isnonneg" DESC " on positive matrix") ;
  }

#if (!defined(UNSIGNED) && !defined(BASE_CHAR))
  k = 0;
  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
          k++;
          FUNCTION (gsl_matrix, set) (m, i, j, -(ATOMIC) (k % 10 + 1));
        }
    }

  {
    status = (FUNCTION(gsl_matrix,isnull)(m) != 0);
    TEST (status, "_isnull" DESC " on negative matrix") ;

    status = (FUNCTION(gsl_matrix,ispos)(m) != 0);
    TEST (status, "_ispos" DESC " on negative matrix") ;

    status = (FUNCTION(gsl_matrix,isneg)(m) != 1);
    TEST (status, "_isneg" DESC " on negative matrix") ;

    status = (FUNCTION(gsl_matrix,isnonneg)(m) != 0);
    TEST (status, "_isnonneg" DESC " on negative matrix") ;
  }
#endif

  FUNCTION (gsl_matrix, free) (m);
  FUNCTION (gsl_vector, free) (v);
}


void
FUNCTION (test, ops) (const size_t M, const size_t N)
{
  size_t i, j;
  size_t k = 0;
  
  TYPE (gsl_matrix) * a = FUNCTION (gsl_matrix, calloc) (M, N);
  TYPE (gsl_matrix) * b = FUNCTION (gsl_matrix, calloc) (M, N);
  TYPE (gsl_matrix) * m = FUNCTION (gsl_matrix, alloc) (M, N);

  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
          FUNCTION (gsl_matrix, set) (a, i, j, (BASE)(3 + i +  5 * j));
          FUNCTION (gsl_matrix, set) (b, i, j, (BASE)(3 + 2 * i + 4 * j));
        }
    }
  
  {
    int status = (FUNCTION(gsl_matrix,equal) (a,b) != 0);
    gsl_test (status, NAME (gsl_matrix) "_equal matrix unequal");
  }
  
  FUNCTION (gsl_matrix, memcpy) (m, a);
  
  {
    int status = (FUNCTION(gsl_matrix,equal) (a,m) != 1);
    gsl_test (status, NAME (gsl_matrix) "_equal matrix equal");
    }
  
  FUNCTION (gsl_matrix, add) (m, b);
    
  {
    int status = 0;
    
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            BASE r = FUNCTION(gsl_matrix,get) (m,i,j);
            BASE x = FUNCTION(gsl_matrix,get) (a,i,j);
            BASE y = FUNCTION(gsl_matrix,get) (b,i,j);
            BASE z = x + y;
            if (r != z)
              status = 1;
          }
      }
    gsl_test (status, NAME (gsl_matrix) "_add matrix addition");
  }
  
  
  FUNCTION(gsl_matrix, memcpy) (m, a);
  FUNCTION(gsl_matrix, sub) (m, b);
  
  {
    int status = 0;
    
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            BASE r = FUNCTION(gsl_matrix,get) (m,i,j);
            BASE x = FUNCTION(gsl_matrix,get) (a,i,j);
            BASE y = FUNCTION(gsl_matrix,get) (b,i,j);
            BASE z = x - y;
            if (r != z)
              status = 1;
          }
      }
    gsl_test (status, NAME (gsl_matrix) "_sub matrix subtraction");
  }
  
  FUNCTION(gsl_matrix, memcpy) (m, a);
  FUNCTION(gsl_matrix, mul_elements) (m, b);
  
  {
    int status = 0;
    
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            BASE r = FUNCTION(gsl_matrix,get) (m,i,j);
            BASE x = FUNCTION(gsl_matrix,get) (a,i,j);
            BASE y = FUNCTION(gsl_matrix,get) (b,i,j);
            BASE z = x * y;
            if (r != z)
              status = 1;
          }
      }
    gsl_test (status, NAME (gsl_matrix) "_mul_elements multiplication");
  }

  FUNCTION(gsl_matrix, memcpy) (m, a);
  FUNCTION(gsl_matrix, div_elements) (m, b);
    
  {
    int status = 0;
      
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            BASE r = FUNCTION(gsl_matrix,get) (m,i,j);
            BASE x = FUNCTION(gsl_matrix,get) (a,i,j);
            BASE y = FUNCTION(gsl_matrix,get) (b,i,j);
            BASE z = x / y;
            if (fabs(r - z) > 2 * GSL_FLT_EPSILON * fabs(z))
              status = 1;
          }
      }
    gsl_test (status, NAME (gsl_matrix) "_div_elements division");
  }


  FUNCTION(gsl_matrix, memcpy) (m, a);
  FUNCTION(gsl_matrix, scale) (m, 2.0);

  {
    int status = 0;

    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            BASE r = FUNCTION(gsl_matrix,get) (m,i,j);
            BASE x = FUNCTION(gsl_matrix,get) (a,i,j);
            if (r !=  (ATOMIC)(2*x))
              status = 1;
          }
      }
    gsl_test (status, NAME (gsl_matrix) "_scale");
  }

  FUNCTION(gsl_matrix, memcpy) (m, a);
  FUNCTION(gsl_matrix, add_constant) (m, 3.0);

  {
    int status = 0;

    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            BASE r = FUNCTION(gsl_matrix,get) (m,i,j);
            BASE x = FUNCTION(gsl_matrix,get) (a,i,j);
            BASE y = x + 3.0;
            if (fabs(r - y) > 2 * GSL_FLT_EPSILON * fabs(y))
              status = 1;
          }
      }
    gsl_test (status, NAME (gsl_matrix) "_add_constant");
  }

  FUNCTION(gsl_matrix, memcpy) (m, a);
  FUNCTION(gsl_matrix, add_diagonal) (m, 5.0);

  {
    int status = 0;

    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            BASE r = FUNCTION(gsl_matrix,get) (m,i,j);
            BASE x = FUNCTION(gsl_matrix,get) (a,i,j);
            BASE y = (i == j) ? (x + 5.0) : x;
            if (fabs(r - y) > 2 * GSL_FLT_EPSILON * fabs(y))
              status = 1;
          }
      }
    gsl_test (status, NAME (gsl_matrix) "_add_diagonal");
  }


  FUNCTION(gsl_matrix, swap) (a, b);

  {
    int status = 0;

    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            BASE x = FUNCTION(gsl_matrix,get) (a,i,j);
            BASE y = FUNCTION(gsl_matrix,get) (b,i,j);
            if (y != (BASE)(3 + i +  5 * j) || x != (BASE)(3 + 2 * i + 4 * j))
              status = 1;
          }
      }
    gsl_test (status, NAME (gsl_matrix) "_swap");
  }
      

  FUNCTION(gsl_matrix, free) (a);
  FUNCTION(gsl_matrix, free) (b);
  FUNCTION(gsl_matrix, free) (m);
}

#if !(USES_LONGDOUBLE && !HAVE_PRINTF_LONGDOUBLE)
void
FUNCTION (test, text) (const size_t M, const size_t N)
{
  TYPE (gsl_matrix) * m = FUNCTION (gsl_matrix, alloc) (M, N);

  size_t i, j;
  int k = 0;

  {
    FILE *f = fopen ("test.txt", "w");

    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            k++;
            FUNCTION (gsl_matrix, set) (m, i, j, (BASE) k);
          }
      }

    FUNCTION (gsl_matrix, fprintf) (f, m, OUT_FORMAT);
    fclose (f);
  }

  {
    FILE *f = fopen ("test.txt", "r");
    TYPE (gsl_matrix) * mm = FUNCTION (gsl_matrix, alloc) (M, N);
    status = 0;

    FUNCTION (gsl_matrix, fscanf) (f, mm);
    k = 0;
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            k++;
            if (mm->data[i * N + j] != (BASE) k)
              status = 1;
          }
      }

    gsl_test (status, NAME (gsl_matrix) "_fprintf and fscanf");

    fclose (f);
    FUNCTION (gsl_matrix, free) (mm);
  }

  FUNCTION (gsl_matrix, free) (m);
}
#endif

void
FUNCTION (test, binary) (const size_t M, const size_t N)
{
  TYPE (gsl_matrix) * m = FUNCTION (gsl_matrix, calloc) (M, N);

  size_t i, j;
  size_t k = 0;

  {
    FILE *f = fopen ("test.dat", "wb");
    k = 0;
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            k++;
            FUNCTION (gsl_matrix, set) (m, i, j, (BASE) k);
          }
      }

    FUNCTION (gsl_matrix, fwrite) (f, m);
    fclose (f);
  }

  {
    FILE *f = fopen ("test.dat", "rb");
    TYPE (gsl_matrix) * mm = FUNCTION (gsl_matrix, alloc) (M, N);
    status = 0;

    FUNCTION (gsl_matrix, fread) (f, mm);
    k = 0;
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            k++;
            if (mm->data[i * N + j] != (BASE) k)
              status = 1;
          }
      }

    gsl_test (status, NAME (gsl_matrix) "_write and read");

    fclose (f);
    FUNCTION (gsl_matrix, free) (mm);
  }

  FUNCTION (gsl_matrix, free) (m);
}

void
FUNCTION (test, binary_noncontiguous) (const size_t M, const size_t N)
{
  TYPE (gsl_matrix) * l = FUNCTION (gsl_matrix, calloc) (M+1, N+1);
  VIEW (gsl_matrix, view) m = FUNCTION (gsl_matrix, submatrix) (l, 0, 0, M, N);

  size_t i, j;
  size_t k = 0;

  {
    FILE *f = fopen ("test.dat", "wb");
    k = 0;
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            k++;
            FUNCTION (gsl_matrix, set) (&m.matrix, i, j, (BASE) k);
          }
      }

    FUNCTION (gsl_matrix, fwrite) (f, &m.matrix);
    fclose (f);
  }

  {
    FILE *f = fopen ("test.dat", "rb");
    TYPE (gsl_matrix) * ll = FUNCTION (gsl_matrix, alloc) (M+1, N+1);
    VIEW (gsl_matrix, view) mm = FUNCTION (gsl_matrix, submatrix) (ll, 0, 0, M, N);
    status = 0;

    FUNCTION (gsl_matrix, fread) (f, &mm.matrix);
    k = 0;
    for (i = 0; i < M; i++)
      {
        for (j = 0; j < N; j++)
          {
            k++;
            if (FUNCTION (gsl_matrix, get) (&mm.matrix, i, j) != (BASE) k)
              status = 1;
          }
      }

    gsl_test (status, NAME (gsl_matrix) "_write and read (noncontiguous)");

    fclose (f);
    FUNCTION (gsl_matrix, free) (ll);
  }

  FUNCTION (gsl_matrix, free) (l);
}

void
FUNCTION (test, trap) (const size_t M, const size_t N)
{
  TYPE (gsl_matrix) * m = FUNCTION (gsl_matrix, alloc) (M, N);

  size_t i = 0, j = 0;
  double x;

  status = 0;
  FUNCTION (gsl_matrix, set) (m, M + 1, 0, (BASE) 1.2);
  gsl_test (!status,
            NAME (gsl_matrix) "_set traps 1st index above upper bound");

  status = 0;
  FUNCTION (gsl_matrix, set) (m, 0, N + 1, (BASE) 1.2);
  gsl_test (!status,
            NAME (gsl_matrix) "_set traps 2nd index above upper bound");

  status = 0;
  FUNCTION (gsl_matrix, set) (m, M, 0, (BASE) 1.2);
  gsl_test (!status,
            NAME (gsl_matrix) "_set traps 1st index at upper bound");

  status = 0;
  FUNCTION (gsl_matrix, set) (m, 0, N, (BASE) 1.2);
  gsl_test (!status,
            NAME (gsl_matrix) "_set traps 2nd index at upper bound");

  status = 0;
  x = FUNCTION (gsl_matrix, get) (m, i - 1, 0);
  gsl_test (!status,
            NAME (gsl_matrix) "_get traps 1st index below lower bound");
  gsl_test (x != 0,
     NAME (gsl_matrix) "_get returns zero for 1st index below lower bound");

  status = 0;
  x = FUNCTION (gsl_matrix, get) (m, 0, j - 1);
  gsl_test (!status,
            NAME (gsl_matrix) "_get traps 2nd index below lower bound");
  gsl_test (x != 0,
     NAME (gsl_matrix) "_get returns zero for 2nd index below lower bound");

  status = 0;
  x = FUNCTION (gsl_matrix, get) (m, M + 1, 0);
  gsl_test (!status,
            NAME (gsl_matrix) "_get traps 1st index above upper bound");
  gsl_test (x != 0,
     NAME (gsl_matrix) "_get returns zero for 1st index above upper bound");

  status = 0;
  x = FUNCTION (gsl_matrix, get) (m, 0, N + 1);
  gsl_test (!status,
            NAME (gsl_matrix) "_get traps 2nd index above upper bound");
  gsl_test (x != 0,
     NAME (gsl_matrix) "_get returns zero for 2nd index above upper bound");

  status = 0;
  x = FUNCTION (gsl_matrix, get) (m, M, 0);
  gsl_test (!status,
            NAME (gsl_matrix) "_get traps 1st index at upper bound");
  gsl_test (x != 0,
        NAME (gsl_matrix) "_get returns zero for 1st index at upper bound");

  status = 0;
  x = FUNCTION (gsl_matrix, get) (m, 0, N);
  gsl_test (!status,
            NAME (gsl_matrix) "_get traps 2nd index at upper bound");
  gsl_test (x != 0,
        NAME (gsl_matrix) "_get returns zero for 2nd index at upper bound");

  FUNCTION (gsl_matrix, free) (m);
}
