/* vector/test_source.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007, 2010 Gerard Jungman, Brian Gough
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

void FUNCTION (test, func) (size_t stride, size_t N);
void FUNCTION (test, ops) (size_t stride1, size_t stride2, size_t N);
void FUNCTION (test, file) (size_t stride, size_t N);
void FUNCTION (test, text) (size_t stride, size_t N);
void FUNCTION (test, trap) (size_t stride, size_t N);
TYPE (gsl_vector) * FUNCTION(create, vector) (size_t stride, size_t N);

#define TEST(expr,desc) gsl_test((expr), NAME(gsl_vector) desc " stride=%d, N=%d", stride, N)
#define TEST2(expr,desc) gsl_test((expr), NAME(gsl_vector) desc " stride1=%d, stride2=%d, N=%d", stride1, stride2, N)

TYPE (gsl_vector) *
FUNCTION(create, vector) (size_t stride, size_t N)
{
    TYPE (gsl_vector) * v = FUNCTION (gsl_vector, calloc) (N*stride);
    v->stride = stride;
    v->size = N;
    return v;
}

void
FUNCTION (test, func) (size_t stride, size_t N)
{
  TYPE (gsl_vector) * v0;
  TYPE (gsl_vector) * v;
  QUALIFIED_VIEW(gsl_vector,view) view;

  size_t i, j;

  if (stride == 1) 
    {
      v = FUNCTION (gsl_vector, calloc) (N);
      
      TEST(v->data == 0, "_calloc pointer");
      TEST(v->size != N, "_calloc size");
      TEST(v->stride != 1, "_calloc stride");

      {
        int status = (FUNCTION(gsl_vector,isnull)(v) != 1);
        TEST (status, "_isnull" DESC " on calloc vector");
        
        status = (FUNCTION(gsl_vector,ispos)(v) != 0);
        TEST (status, "_ispos" DESC " on calloc vector");

        status = (FUNCTION(gsl_vector,isneg)(v) != 0);
        TEST (status, "_isneg" DESC " on calloc vector");

        status = (FUNCTION(gsl_vector,isnonneg)(v) != 1);
        TEST (status, "_isnonneg" DESC " on calloc vector");

      }

      FUNCTION (gsl_vector, free) (v);      /* free whatever is in v */
    }

  if (stride == 1) 
    {
      v = FUNCTION (gsl_vector, alloc) (N);
      
      TEST(v->data == 0, "_alloc pointer");
      TEST(v->size != N, "_alloc size");
      TEST(v->stride != 1, "_alloc stride");

      FUNCTION (gsl_vector, free) (v);      /* free whatever is in v */
    }

  if (stride == 1)
    {
      v0 = FUNCTION (gsl_vector, alloc) (N);
      view = FUNCTION (gsl_vector, subvector) (v0, 0, N);
      v = &view.vector;
    }
  else
    {
      v0 = FUNCTION (gsl_vector, alloc) (N * stride);

      for (i = 0; i < N*stride; i++)
        {
          v0->data[i] = i;
        }
      
      view = FUNCTION (gsl_vector, subvector_with_stride) (v0, 0, stride, N);
      v = &view.vector;
    }
      
  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        FUNCTION (gsl_vector, set) (v, i, (ATOMIC) i);
      }

    for (i = 0; i < N; i++)
      {
        if (v->data[i*stride] != (ATOMIC) (i))
          status = 1;
      };
  
    TEST(status,"_set" DESC " writes into array");
  }


  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        if (FUNCTION (gsl_vector, get) (v, i) != (ATOMIC) (i))
          status = 1;
      };

    TEST (status, "_get" DESC " reads from array");
  }
  
  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        if (FUNCTION (gsl_vector, ptr) (v, i) != v->data + i*stride)
          status = 1;
      };

    TEST (status, "_ptr" DESC " access to array");
  }


  {
    int status = 0;
    
    for (i = 0; i < N; i++)
      {
        if (FUNCTION (gsl_vector, const_ptr) (v, i) != v->data + i*stride)
          status = 1;
      };
    
    TEST (status, "_const_ptr" DESC " access to array");
  }


  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        FUNCTION (gsl_vector, set) (v, i, (ATOMIC) 0);
      }
    
    status = (FUNCTION(gsl_vector,isnull)(v) != 1);
    TEST (status, "_isnull" DESC " on null vector") ;

    status = (FUNCTION(gsl_vector,ispos)(v) != 0);
    TEST (status, "_ispos" DESC " on null vector") ;

    status = (FUNCTION(gsl_vector,isneg)(v) != 0);
    TEST (status, "_isneg" DESC " on null vector") ;

    status = (FUNCTION(gsl_vector,isnonneg)(v) != 1);
    TEST (status, "_isnonneg" DESC " on null vector") ;

  }

  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        FUNCTION (gsl_vector, set) (v, i, (ATOMIC) (i % 10));
      }
    
    status = (FUNCTION(gsl_vector,isnull)(v) != 0);
    TEST (status, "_isnull" DESC " on non-negative vector") ;

    status = (FUNCTION(gsl_vector,ispos)(v) != 0);
    TEST (status, "_ispos" DESC " on non-negative vector") ;

    status = (FUNCTION(gsl_vector,isneg)(v) != 0);
    TEST (status, "_isneg" DESC " on non-negative vector") ;

    status = (FUNCTION(gsl_vector,isnonneg)(v) != 1);
    TEST (status, "_isnonneg" DESC " on non-negative vector") ;
  }


#ifndef UNSIGNED
  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        ATOMIC vi = (i % 10) - (ATOMIC) 5;
        FUNCTION (gsl_vector, set) (v, i, vi);
      }
    
    status = (FUNCTION(gsl_vector,isnull)(v) != 0);
    TEST (status, "_isnull" DESC " on mixed vector") ;

    status = (FUNCTION(gsl_vector,ispos)(v) != 0);
    TEST (status, "_ispos" DESC " on mixed vector") ;

    status = (FUNCTION(gsl_vector,isneg)(v) != 0);
    TEST (status, "_isneg" DESC " on mixed vector") ;

    status = (FUNCTION(gsl_vector,isnonneg)(v) != 0);
    TEST (status, "_isnonneg" DESC " on mixed vector") ;
  }

  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        FUNCTION (gsl_vector, set) (v, i, -(ATOMIC) (i % 10));
      }
    
    status = (FUNCTION(gsl_vector,isnull)(v) != 0);
    TEST (status, "_isnull" DESC " on non-positive vector") ;

    status = (FUNCTION(gsl_vector,ispos)(v) != 0);
    TEST (status, "_ispos" DESC " on non-positive vector") ;

    status = (FUNCTION(gsl_vector,isneg)(v) != 0);
    TEST (status, "_isneg" DESC " on non-positive non-null vector") ;

    status = (FUNCTION(gsl_vector,isnonneg)(v) != 0);
    TEST (status, "_isnonneg" DESC " on non-positive non-null vector") ;
  }
#endif

  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        FUNCTION (gsl_vector, set) (v, i, (ATOMIC) (i % 10 + 1));
      }
    
    status = (FUNCTION(gsl_vector,isnull)(v) != 0);
    TEST (status, "_isnull" DESC " on positive vector") ;

    status = (FUNCTION(gsl_vector,ispos)(v) != 1);
    TEST (status, "_ispos" DESC " on positive vector") ;

    status = (FUNCTION(gsl_vector,isneg)(v) != 0);
    TEST (status, "_isneg" DESC " on positive vector") ;

    status = (FUNCTION(gsl_vector,isnonneg)(v) != 1);
    TEST (status, "_isnonneg" DESC " on positive vector") ;
  }


#if (!defined(UNSIGNED) && !defined(BASE_CHAR))
  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        FUNCTION (gsl_vector, set) (v, i, -(ATOMIC) (i % 10 + 1));
      }
    
    status = (FUNCTION(gsl_vector,isnull)(v) != 0);
    TEST (status, "_isnull" DESC " on negative vector") ;

    status = (FUNCTION(gsl_vector,ispos)(v) != 0);
    TEST (status, "_ispos" DESC " on negative vector") ;

    status = (FUNCTION(gsl_vector,isneg)(v) != 1);
    TEST (status, "_isneg" DESC " on negative vector") ;

    status = (FUNCTION(gsl_vector,isnonneg)(v) != 0);
    TEST (status, "_isnonneg" DESC " on negative vector") ;
  }
#endif

  {
    int status = 0;
    
    FUNCTION (gsl_vector, set_zero) (v);

    for (i = 0; i < N; i++)
      {
        if (FUNCTION (gsl_vector, get) (v, i) != (ATOMIC)0)
          status = 1;
      };

    TEST (status, "_setzero" DESC " on non-null vector") ;
  }

  {
    int status = 0;
    
    FUNCTION (gsl_vector, set_all) (v, (ATOMIC)27);

    for (i = 0; i < N; i++)
      {
        if (FUNCTION (gsl_vector, get) (v, i) != (ATOMIC) (27))
          status = 1;
      };

    TEST (status, "_setall" DESC " to non-zero value") ;
  }


  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        FUNCTION (gsl_vector, set_basis) (v, i);

        for (j = 0; j < N; j++)
          {
            if (i == j)
              {
                if (FUNCTION (gsl_vector, get) (v, j) != (ATOMIC)1)
                  status = 1 ;
              }
            else 
              {
                if (FUNCTION (gsl_vector, get) (v, j) != (ATOMIC)(0))
                  status = 1;
              }
          };
      }

    TEST (status, "_setbasis" DESC " over range") ;
  }

  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        FUNCTION (gsl_vector, set) (v, i, (ATOMIC) i);
      }

    FUNCTION (gsl_vector, scale) (v, 2.0);

    for (i = 0; i < N; i++)
      {
        if (FUNCTION (gsl_vector, get) (v, i) != (ATOMIC) (i*2.0))
          status = 1;
      };

    TEST (status, "_scale" DESC " by 2") ;
  }

  {
    int status = 0;

    FUNCTION (gsl_vector, add_constant) (v, (ATOMIC)7);

    for (i = 0; i < N; i++)
      {
        if (FUNCTION (gsl_vector, get) (v, i) != (ATOMIC) (i*2.0 + 7))
          status = 1;
      };

    TEST (status, "_add_constant" DESC) ;
  }
    
  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        FUNCTION (gsl_vector, set) (v, i, (ATOMIC) i);
      }

    FUNCTION (gsl_vector,swap_elements) (v, 2, 5) ;
    
    status = (FUNCTION(gsl_vector,get)(v,2) != 5) ;
    status |= (FUNCTION(gsl_vector,get)(v,5) != 2) ;
    
    FUNCTION (gsl_vector,swap_elements) (v, 2, 5) ;
    
    status |= (FUNCTION(gsl_vector,get)(v,2) != 2) ;
    status |= (FUNCTION(gsl_vector,get)(v,5) != 5) ;
    
    TEST (status, "_swap_elements" DESC " (2,5)") ;
  }

  {
    int status = 0;

    FUNCTION (gsl_vector,reverse) (v) ;
    
    for (i = 0; i < N; i++)
      {
        status |= (FUNCTION (gsl_vector, get) (v, i) !=  (ATOMIC) (N - i - 1));
      }
    
    TEST (status, "_reverse" DESC " reverses elements") ;
  }


  {
    int status = 0;
    
    QUALIFIED_VIEW(gsl_vector,view) v1 = FUNCTION(gsl_vector, view_array) (v->data, N*stride);
    
    for (i = 0; i < N; i++)
      {
        if (FUNCTION (gsl_vector, get) (&v1.vector, i*stride) != FUNCTION (gsl_vector, get) (v, i)) 
          status = 1;
      };

    TEST (status, "_view_array" DESC);
  }

  {
    int status = 0;
    
    QUALIFIED_VIEW(gsl_vector,view) v1 = FUNCTION(gsl_vector, view_array_with_stride) (v->data, stride, N*stride);
    
    for (i = 0; i < N; i++)
      {
        if (FUNCTION (gsl_vector, get) (&v1.vector, i) != FUNCTION (gsl_vector, get) (v, i)) 
          status = 1;
      };

    TEST (status, "_view_array_with_stride" DESC);
  }


  {
    int status = 0;
    
    QUALIFIED_VIEW(gsl_vector,view) v1 = FUNCTION(gsl_vector, subvector) (v, N/3, N/2);
    
    for (i = 0; i < N/2; i++)
      {
        if (FUNCTION (gsl_vector, get) (&v1.vector, i) != FUNCTION (gsl_vector, get) (v, (N/3) + i)) 
          status = 1;
      };

    TEST (status, "_view_subvector" DESC);
  }

  {
    int status = 0;
    
    QUALIFIED_VIEW(gsl_vector,view) v1 = FUNCTION(gsl_vector, subvector_with_stride) (v, N/5, 3, N/4);
    
    for (i = 0; i < N/4; i++)
      {
        if (FUNCTION (gsl_vector, get) (&v1.vector, i) != FUNCTION (gsl_vector, get) (v, (N/5) + 3*i)) 
          status = 1;
      };

    TEST (status, "_view_subvector_with_stride" DESC);
  }


  {
    BASE exp_max = FUNCTION(gsl_vector,get)(v, 0);
    BASE exp_min = FUNCTION(gsl_vector,get)(v, 0);
    size_t exp_imax = 0, exp_imin = 0;

    for (i = 0; i < N; i++)
      {
        BASE k = FUNCTION(gsl_vector, get) (v, i) ;
        if (k < exp_min) {
          exp_min = FUNCTION(gsl_vector, get) (v, i);
          exp_imin = i;
        }
      }

    for (i = 0; i < N; i++)
      {
        BASE k = FUNCTION(gsl_vector, get) (v, i) ;
        if (k > exp_max) {
          exp_max = FUNCTION(gsl_vector, get) (v, i) ;
          exp_imax = i;
        } 
      }

    {
      BASE max = FUNCTION(gsl_vector, max) (v) ;
      TEST (max != exp_max, "_max returns correct maximum value");
    }

    {
      BASE min = FUNCTION(gsl_vector, min) (v) ;
      TEST (min != exp_min, "_min returns correct minimum value");
    }

    {
      BASE min, max;
      FUNCTION(gsl_vector, minmax) (v, &min, &max);

      TEST (max != exp_max, "_minmax returns correct maximum value");
      TEST (min != exp_min, "_minmax returns correct minimum value");
    }


    {
      size_t imax =  FUNCTION(gsl_vector, max_index) (v) ;
      TEST (imax != exp_imax, "_max_index returns correct maximum i");
    }

    {
      size_t imin = FUNCTION(gsl_vector, min_index) (v) ;
      TEST (imin != exp_imin, "_min_index returns correct minimum i");
    }

    {
      size_t imin, imax;

      FUNCTION(gsl_vector, minmax_index) (v,  &imin, &imax);

      TEST (imax != exp_imax, "_minmax_index returns correct maximum i");
      TEST (imin != exp_imin, "_minmax_index returns correct minimum i");
    }
    
#if FP
    i = N/2;
    FUNCTION(gsl_vector, set) (v, i, GSL_NAN);
    exp_max = GSL_NAN; exp_min = GSL_NAN;
    exp_imax = i; exp_imin = i;

    {
      BASE max = FUNCTION(gsl_vector, max) (v) ;
      gsl_test_abs (max, exp_max, 0, "_max returns correct maximum value for NaN");
    }

    {
      BASE min = FUNCTION(gsl_vector, min) (v) ;
      gsl_test_abs (min, exp_min, 0, "_min returns correct minimum value for NaN");
    }

    {
      BASE min, max;
      FUNCTION(gsl_vector, minmax) (v, &min, &max);

      gsl_test_abs (max, exp_max, 0, "_minmax returns correct maximum value for NaN");
      gsl_test_abs (min, exp_min, 0, "_minmax returns correct minimum value for NaN");
    }


    {
      size_t imax =  FUNCTION(gsl_vector, max_index) (v) ;
      TEST (imax != exp_imax, "_max_index returns correct maximum i for NaN");
    }

    {
      size_t imin = FUNCTION(gsl_vector, min_index) (v) ;
      TEST (imin != exp_imin, "_min_index returns correct minimum i for NaN");
    }

    {
      size_t imin, imax;

      FUNCTION(gsl_vector, minmax_index) (v,  &imin, &imax);

      TEST (imax != exp_imax, "_minmax_index returns correct maximum i for NaN");
      TEST (imin != exp_imin, "_minmax_index returns correct minimum i for NaN");
    }
#endif

  }


  FUNCTION (gsl_vector, free) (v0);      /* free whatever is in v */
}

void
FUNCTION (test, ops) (size_t stride1, size_t stride2, size_t N)
{
  size_t i;
  TYPE (gsl_vector) * a = FUNCTION (create, vector) (stride1, N);
  TYPE (gsl_vector) * b = FUNCTION (create, vector) (stride2, N);
  TYPE (gsl_vector) * v = FUNCTION (create, vector) (stride1, N);
  
  for (i = 0; i < N; i++)
    {
      FUNCTION (gsl_vector, set) (a, i, (BASE)(3 + i));
      FUNCTION (gsl_vector, set) (b, i, (BASE)(3 + 2 * i));
    }
  
  {
    int status = (FUNCTION(gsl_vector,equal) (a,b) != 0);
    TEST2 (status, "_equal vectors unequal");
  }

  FUNCTION(gsl_vector, memcpy) (v, a);

  {
    int status = (FUNCTION(gsl_vector,equal) (a,v) != 1);
    TEST2 (status, "_equal vectors equal");
  }

  FUNCTION(gsl_vector, add) (v, b);
  
  {
    int status = 0;
    
    for (i = 0; i < N; i++)
      {
        BASE r = FUNCTION(gsl_vector,get) (v,i);
        BASE x = FUNCTION(gsl_vector,get) (a,i);
        BASE y = FUNCTION(gsl_vector,get) (b,i);
        BASE z = x + y;
        if (r != z)
          status = 1;
      }
    TEST2 (status, "_add vector addition");
  }

  {
    int status = 0;
    
    FUNCTION(gsl_vector, swap) (a, b);

    for (i = 0; i < N; i++)
      {
        status |= (FUNCTION (gsl_vector, get) (a, i) != (BASE)(3 + 2 * i));
        status |= (FUNCTION (gsl_vector, get) (b, i) != (BASE)(3 + i));
      }

    FUNCTION(gsl_vector, swap) (a, b);

    for (i = 0; i < N; i++)
      {
        status |= (FUNCTION (gsl_vector, get) (a, i) != (BASE)(3 + i));
        status |= (FUNCTION (gsl_vector, get) (b, i) != (BASE)(3 + 2 * i));
      }

    TEST2 (status, "_swap exchange vectors");
  }
  
  FUNCTION(gsl_vector, memcpy) (v, a);
  FUNCTION(gsl_vector, sub) (v, b);
  
  {
    int status = 0;
    
    for (i = 0; i < N; i++)
      {
        BASE r = FUNCTION(gsl_vector,get) (v,i);
        BASE x = FUNCTION(gsl_vector,get) (a,i);
        BASE y = FUNCTION(gsl_vector,get) (b,i);
        BASE z = x - y;
        if (r != z)
          status = 1;
      }

    TEST2 (status, "_sub vector subtraction");
  }
  
  FUNCTION(gsl_vector, memcpy) (v, a);
  FUNCTION(gsl_vector, mul) (v, b);
  
  {
    int status = 0;
    
    for (i = 0; i < N; i++)
      {
        BASE r = FUNCTION(gsl_vector,get) (v,i);
        BASE x = FUNCTION(gsl_vector,get) (a,i);
        BASE y = FUNCTION(gsl_vector,get) (b,i);
        BASE z = x * y;
        if (r != z)
          status = 1;
      }

    TEST2 (status, "_mul multiplication");
  }
  
  FUNCTION(gsl_vector, memcpy) (v, a);
  FUNCTION(gsl_vector, div) (v, b);
  
  {
    int status = 0;
    
    for (i = 0; i < N; i++)
      {
        BASE r = FUNCTION(gsl_vector,get) (v,i);
        BASE x = FUNCTION(gsl_vector,get) (a,i);
        BASE y = FUNCTION(gsl_vector,get) (b,i);
        BASE z = x / y;
        if (fabs(r - z) > 2 * GSL_FLT_EPSILON * fabs(z))
          status = 1;
      }
    TEST2 (status, "_div division");
  }

  FUNCTION(gsl_vector, free) (a);
  FUNCTION(gsl_vector, free) (b);
  FUNCTION(gsl_vector, free) (v);
}


void
FUNCTION (test, file) (size_t stride, size_t N)
{
  TYPE (gsl_vector) * v = FUNCTION (create, vector) (stride, N);
  TYPE (gsl_vector) * w = FUNCTION (create, vector) (stride, N);

  size_t i;

  {
    FILE *f = fopen ("test.dat", "wb");

    for (i = 0; i < N; i++)
      {
        FUNCTION (gsl_vector, set) (v, i, (ATOMIC) (N - i));
      };

    FUNCTION (gsl_vector, fwrite) (f, v);

    fclose (f);
  }

  {
    FILE *f = fopen ("test.dat", "rb");

    FUNCTION (gsl_vector, fread) (f, w);

    status = 0;
    for (i = 0; i < N; i++)
      {
        if (w->data[i*stride] != (ATOMIC) (N - i))
          status = 1;
      };

    TEST (status, "_write and read");

    fclose (f);
  }

  FUNCTION (gsl_vector, free) (v);      /* free whatever is in v */
  FUNCTION (gsl_vector, free) (w);      /* free whatever is in w */
}

#if USES_LONGDOUBLE && ! HAVE_PRINTF_LONGDOUBLE
/* skip this test */
#else
void
FUNCTION (test, text) (size_t stride, size_t N)
{
  TYPE (gsl_vector) * v = FUNCTION (create, vector) (stride, N);
  TYPE (gsl_vector) * w = FUNCTION (create, vector) (stride, N);

  size_t i;

  {
    FILE *f = fopen ("test.txt", "w");

    for (i = 0; i < N; i++)
      {
        FUNCTION (gsl_vector, set) (v, i, (ATOMIC) i);
      };

    FUNCTION (gsl_vector, fprintf) (f, v, OUT_FORMAT);

    fclose (f);
  }

  {
    FILE *f = fopen ("test.txt", "r");

    FUNCTION (gsl_vector, fscanf) (f, w);

    status = 0;
    for (i = 0; i < N; i++)
      {
        if (w->data[i*stride] != (ATOMIC) i)
          status = 1;
      };

    gsl_test (status, NAME (gsl_vector) "_fprintf and fscanf");

    fclose (f);
  }

  FUNCTION (gsl_vector, free) (v);
  FUNCTION (gsl_vector, free) (w);
}
#endif

void
FUNCTION (test, trap) (size_t stride, size_t N)
{
  double x;
  size_t j = 0;
  TYPE (gsl_vector) * v = FUNCTION (create, vector) (stride, N);
  v->size = N;
  v->stride = stride;

  status = 0;
  FUNCTION (gsl_vector, set) (v, j - 1, (ATOMIC)0);
  TEST (!status, "_set traps index below lower bound");

  status = 0;
  FUNCTION (gsl_vector, set) (v, N + 1, (ATOMIC)0);
  TEST (!status, "_set traps index above upper bound");

  status = 0;
  FUNCTION (gsl_vector, set) (v, N, (ATOMIC)0);
  TEST (!status, "_set traps index at upper bound");

  status = 0;
  x = FUNCTION (gsl_vector, get) (v, j - 1);
  TEST (!status, "_get traps index below lower bound");
  TEST (x != 0, "_get returns zero for index below lower bound");

  status = 0;
  x = FUNCTION (gsl_vector, get) (v, N + 1);
  TEST (!status, "_get traps index above upper bound");
  TEST (x != 0, "_get returns zero for index above upper bound");

  status = 0;
  x = FUNCTION (gsl_vector, get) (v, N);
  TEST (!status, "_get traps index at upper bound");
  TEST (x != 0, "_get returns zero for index at upper bound");

  FUNCTION (gsl_vector, free) (v);      /* free whatever is in v */
}





