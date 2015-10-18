/* vector/test_complex_source.c
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
          BASE x = ZERO;
          GSL_REAL (x) = (ATOMIC)i;
          GSL_IMAG (x) = (ATOMIC)(i + 1234);
          FUNCTION (gsl_vector, set) (v0, i, x);
        }
      
      view = FUNCTION (gsl_vector, subvector_with_stride) (v0, 0, stride, N);
      v = &view.vector;
    }
      
  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        BASE x = ZERO;
        GSL_REAL (x) = (ATOMIC)i;
        GSL_IMAG (x) = (ATOMIC)(i + 1234);
        FUNCTION (gsl_vector, set) (v, i, x);
      }

    for (i = 0; i < N; i++)
      {
        if (v->data[2*i*stride] != (ATOMIC) (i) || v->data[2 * i * stride + 1] != (ATOMIC) (i + 1234))
          status = 1;
      };
  
    TEST(status,"_set" DESC " writes into array");
  }


  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        BASE x, y;
        GSL_REAL (x) = (ATOMIC)i;
        GSL_IMAG (x) = (ATOMIC)(i + 1234);
        y = FUNCTION (gsl_vector, get) (v, i);
        if (!GSL_COMPLEX_EQ (x, y))
          status = 1;
      };

    TEST (status, "_get" DESC " reads from array");
  }
  
  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        if (FUNCTION (gsl_vector, ptr) (v, i) != (BASE *)v->data + i*stride)
          status = 1;
      };

    TEST (status, "_ptr" DESC " access to array");
  }


  {
    int status = 0;
    
    for (i = 0; i < N; i++)
      {
        if (FUNCTION (gsl_vector, const_ptr) (v, i) != (BASE *)v->data + i*stride)
          status = 1;
      };
    
    TEST (status, "_const_ptr" DESC " access to array");
  }
  
  {
    int status = 0;
    
    for (i = 0; i < N; i++)
      {
        BASE x = ZERO;
        FUNCTION (gsl_vector, set) (v, i, x);
      }
    
    status = (FUNCTION(gsl_vector,isnull)(v) != 1);
    TEST (status, "_isnull" DESC " on null vector") ;

    status = (FUNCTION(gsl_vector,ispos)(v) != 0);
    TEST (status, "_ispos" DESC " on null vector") ;

    status = (FUNCTION(gsl_vector,isneg)(v) != 0);
    TEST (status, "_isneg" DESC " on null vector") ;
  }

  {
    int status = 0;

    for (i = 0; i < N; i++)
      {
        BASE x = ZERO;
        GSL_REAL (x) = (ATOMIC)i;
        GSL_IMAG (x) = (ATOMIC)(i + 1234);
        FUNCTION (gsl_vector, set) (v, i, x);
      }
    
    status = (FUNCTION(gsl_vector,isnull)(v) != 0);
    TEST (status, "_isnull" DESC " on non-null vector") ;

    status = (FUNCTION(gsl_vector,ispos)(v) != 0);
    TEST (status, "_ispos" DESC " on non-null vector") ;

    status = (FUNCTION(gsl_vector,ispos)(v) != 0);
    TEST (status, "_isneg" DESC " on non-null vector") ;
  }

  {
    int status = 0;
    
    FUNCTION (gsl_vector, set_zero) (v);

    for (i = 0; i < N; i++)
      {
        BASE x, y = ZERO;
        x = FUNCTION (gsl_vector, get) (v, i);
        if (!GSL_COMPLEX_EQ (x, y))
          status = 1;
      };

    TEST (status, "_setzero" DESC " on non-null vector") ;
  }

  {
    int status = 0;

    BASE x;
    GSL_REAL (x) = (ATOMIC)27;
    GSL_IMAG (x) = (ATOMIC)(27 + 1234);

    FUNCTION (gsl_vector, set_all) (v, x);

    for (i = 0; i < N; i++)
      {
        BASE y = FUNCTION (gsl_vector, get) (v, i);
        if (!GSL_COMPLEX_EQ (x, y))
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
            BASE x = FUNCTION (gsl_vector, get) (v, j);
            BASE one = ONE;
            BASE zero = ZERO;
              
            if (i == j)
              {
                if (!GSL_COMPLEX_EQ (x, one))
                  status = 1 ;
              }
            else 
              {
                if (!GSL_COMPLEX_EQ (x, zero))
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
        BASE x = ZERO;
        GSL_REAL (x) = (ATOMIC)i;
        GSL_IMAG (x) = (ATOMIC)(i + 1234);
        FUNCTION (gsl_vector, set) (v, i, x);
      }

    {
      BASE x = ZERO;
      GSL_REAL(x) = 2.0;
      GSL_IMAG(x) = 3.0;
      FUNCTION (gsl_vector, scale) (v, x);
    }

    for (i = 0; i < N; i++)
      {
        BASE r = FUNCTION(gsl_vector,get) (v,i);
        ATOMIC real = -(ATOMIC)i-(ATOMIC)3702;
        ATOMIC imag = 5*(ATOMIC)i+(ATOMIC)2468;
        if (GSL_REAL(r) != real || GSL_IMAG(r) != imag)
          status = 1;
      };

    TEST (status, "_scale" DESC " by 2") ;
  }

  {
    int status = 0;

    {
      BASE x = ZERO;
      GSL_REAL(x) = 7.0;
      GSL_IMAG(x) = 13.0;
      FUNCTION (gsl_vector, add_constant) (v, x);
    }


    for (i = 0; i < N; i++)
      {
        BASE r = FUNCTION(gsl_vector,get) (v,i);
        ATOMIC real = -(ATOMIC)i-(ATOMIC)3695;
        ATOMIC imag = 5*(ATOMIC)i+(ATOMIC)2481;

        if (GSL_REAL(r) != real || GSL_IMAG(r) != imag)
          status = 1;
      };

    TEST (status, "_add_constant" DESC) ;
  }

  for (i = 0; i < N; i++)
    {
      BASE x = ZERO;
      GSL_REAL (x) = (ATOMIC)i;
      GSL_IMAG (x) = (ATOMIC)(i + 1234);
      FUNCTION (gsl_vector, set) (v, i, x);
    }

  {
    int status;
    BASE x, y, r, s ;
    GSL_REAL(x) = 2 ;
    GSL_IMAG(x) = 2 + 1234;
    GSL_REAL(y) = 5 ;
    GSL_IMAG(y) = 5 + 1234;

    FUNCTION (gsl_vector,swap_elements) (v, 2, 5) ;
    
    r = FUNCTION(gsl_vector,get)(v,2);
    s = FUNCTION(gsl_vector,get)(v,5);

    status = ! GSL_COMPLEX_EQ(r,y) ;
    status |= ! GSL_COMPLEX_EQ(s,x) ;
    
    FUNCTION (gsl_vector,swap_elements) (v, 2, 5) ;
    
    r = FUNCTION(gsl_vector,get)(v,2);
    s = FUNCTION(gsl_vector,get)(v,5);

    status |= ! GSL_COMPLEX_EQ(r,x) ;
    status |= ! GSL_COMPLEX_EQ(s,y) ;
  
    TEST (status, "_swap_elements" DESC " exchanges elements") ;
  }

  { 
    int status = 0;
    
    FUNCTION (gsl_vector,reverse) (v) ;
    
    for (i = 0; i < N; i++)
      {
        BASE x,r ;
        GSL_REAL(x) = (ATOMIC)(N - i - 1) ;
        GSL_IMAG(x) = (ATOMIC)(N - i - 1 + 1234);
        
        r = FUNCTION (gsl_vector, get) (v, i);
        
        status |= !GSL_COMPLEX_EQ(r,x);
      }
    
    gsl_test (status, NAME(gsl_vector) "_reverse" DESC " reverses elements") ;
  }
    
  {
    int status = 0;
    
    QUALIFIED_VIEW(gsl_vector,view) v1 = FUNCTION(gsl_vector, view_array) (v->data, N*stride);
    
    for (i = 0; i < N; i++)
      {
        BASE x = FUNCTION (gsl_vector, get) (&v1.vector, i*stride) ;
        BASE y = FUNCTION (gsl_vector, get) (v, i);
        if (!GSL_COMPLEX_EQ(x,y)) 
          status = 1;
      };

    TEST (status, "_view_array" DESC);
  }

  {
    int status = 0;
    
    QUALIFIED_VIEW(gsl_vector,view) v1 = FUNCTION(gsl_vector, view_array_with_stride) (v->data, stride, N*stride);
    
    for (i = 0; i < N; i++)
      {
        BASE x = FUNCTION (gsl_vector, get) (&v1.vector, i) ;
        BASE y = FUNCTION (gsl_vector, get) (v, i);
        if (!GSL_COMPLEX_EQ(x,y)) 
          status = 1;
      };

    TEST (status, "_view_array_with_stride" DESC);
  }


  {
    int status = 0;
    
    QUALIFIED_VIEW(gsl_vector,view) v1 = FUNCTION(gsl_vector, subvector) (v, N/3, N/2);
    
    for (i = 0; i < N/2; i++)
      {
        BASE x = FUNCTION (gsl_vector, get) (&v1.vector, i) ;
        BASE y = FUNCTION (gsl_vector, get) (v, (N/3)+i);
        if (!GSL_COMPLEX_EQ(x,y)) 
          status = 1;
      };

    TEST (status, "_view_subvector" DESC);
  }

  {
    int status = 0;
    
    QUALIFIED_VIEW(gsl_vector,view) v1 = FUNCTION(gsl_vector, subvector_with_stride) (v, N/5, 3, N/4);
    
    for (i = 0; i < N/4; i++)
      {
        BASE x = FUNCTION (gsl_vector, get) (&v1.vector, i) ;
        BASE y = FUNCTION (gsl_vector, get) (v, (N/5)+3*i);
        if (!GSL_COMPLEX_EQ(x,y)) 
          status = 1;
      };

    TEST (status, "_view_subvector_with_stride" DESC);
  }


  {
    int status = 0;
    
    QUALIFIED_REAL_VIEW(gsl_vector,view) vv = FUNCTION(gsl_vector, real) (v);
    
    for (i = 0; i < N; i++)
      {
        ATOMIC xr = REAL_VIEW (gsl_vector, get) (&vv.vector, i) ;
        BASE y = FUNCTION (gsl_vector, get) (v, i);
        ATOMIC yr = GSL_REAL(y);

        if (xr != yr) 
          status = 1;
      };

    TEST (status, "_real" DESC);
  }

  {
    int status = 0;
    
    QUALIFIED_REAL_VIEW(gsl_vector,view) vv = FUNCTION(gsl_vector, imag) (v);
    
    for (i = 0; i < N; i++)
      {
        ATOMIC xr = REAL_VIEW (gsl_vector, get) (&vv.vector, i) ;
        BASE y = FUNCTION (gsl_vector, get) (v, i);
        ATOMIC yr = GSL_IMAG(y);

        if (xr != yr) 
          status = 1;
      };

    TEST (status, "_imag" DESC);
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
      BASE z, z1;
      GSL_REAL (z) = (ATOMIC) 3+i;
      GSL_IMAG (z) = (ATOMIC) (3+i + 10);
      GSL_REAL (z1) = (ATOMIC) (3 + 2*i + 5);
      GSL_IMAG (z1) = (ATOMIC) (3 + 2*i + 20);

      FUNCTION (gsl_vector, set) (a, i, z);
      FUNCTION (gsl_vector, set) (b, i, z1);
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
        if (GSL_REAL(r) != (ATOMIC) (3*i+11) 
            || GSL_IMAG(r) != (ATOMIC) (3*i+36))
          status = 1;
      }
    TEST2 (status, "_add vector addition");
  }

  {
    int status = 0;
    
    FUNCTION(gsl_vector, swap) (a, b);

    for (i = 0; i < N; i++)
      {
        BASE z, z1;

        BASE x = FUNCTION (gsl_vector, get) (a, i);
        BASE y = FUNCTION (gsl_vector, get) (b, i);
          
        GSL_REAL (z) = (ATOMIC) 3+i;
        GSL_IMAG (z) = (ATOMIC) (3+i + 10);
        GSL_REAL (z1) = (ATOMIC) (3 + 2*i + 5);
        GSL_IMAG (z1) = (ATOMIC) (3 + 2*i + 20);

        status |= !GSL_COMPLEX_EQ(z,y);
        status |= !GSL_COMPLEX_EQ(z1,x);
      }

    FUNCTION(gsl_vector, swap) (a, b);

    for (i = 0; i < N; i++)
      {
        BASE z, z1;

        BASE x = FUNCTION (gsl_vector, get) (a, i);
        BASE y = FUNCTION (gsl_vector, get) (b, i);
          
        GSL_REAL (z) = (ATOMIC) 3+i;
        GSL_IMAG (z) = (ATOMIC) (3+i + 10);
        GSL_REAL (z1) = (ATOMIC) (3 + 2*i + 5);
        GSL_IMAG (z1) = (ATOMIC) (3 + 2*i + 20);

        status |= !GSL_COMPLEX_EQ(z,x);
        status |= !GSL_COMPLEX_EQ(z1,y);
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
        if (GSL_REAL(r) != (-(ATOMIC)i-(ATOMIC)5) || GSL_IMAG(r) != (-(ATOMIC)i-(ATOMIC)10))
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
        ATOMIC real = (-35*(ATOMIC)i-275);
        ATOMIC imag = (173+((ATOMIC)i)*(63+4*(ATOMIC)i));
        if (fabs(GSL_REAL(r) - real) > 100 * BASE_EPSILON ||
            fabs(GSL_IMAG(r) - imag) > 100 * BASE_EPSILON)
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
        ATOMIC denom = 593 + ((ATOMIC)i)*(124+((ATOMIC)i)*8);
        ATOMIC real = (323+((ATOMIC)i)*(63+4*((ATOMIC)i))) / denom;
        ATOMIC imag = (35 +((ATOMIC)i)*5) / denom;
        if (fabs(GSL_REAL(r) - real) > 100 * BASE_EPSILON)
          status = 1;
        if (fabs(GSL_IMAG(r) - imag) > 100 * BASE_EPSILON)
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
        BASE x = ZERO;
        GSL_REAL (x) = (ATOMIC)(N - i);
        GSL_IMAG (x) = (ATOMIC)(N - i + 1);
        FUNCTION (gsl_vector, set) (v, i, x);
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
        if (w->data[2 * i * stride] != (ATOMIC) (N - i) || w->data[2 * i * stride + 1] != (ATOMIC) (N - i + 1))
          status = 1;
      };
    fclose (f);
  }

  FUNCTION (gsl_vector, free) (v);
  FUNCTION (gsl_vector, free) (w);

  gsl_test (status, NAME (gsl_vector) "_write and read work");

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
        BASE x;
        GSL_REAL (x) = (ATOMIC)i;
        GSL_IMAG (x) = (ATOMIC)(i + 1);
        FUNCTION (gsl_vector, set) (v, i, x);
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
        if (w->data[2 * i * stride] != (ATOMIC) i || w->data[2 * i * stride + 1] != (ATOMIC) (i + 1))
          status = 1;
      };
    fclose (f);
  }

  FUNCTION (gsl_vector, free) (v);
  FUNCTION (gsl_vector, free) (w);

  gsl_test (status, NAME (gsl_vector) "_fprintf and fscanf");
}
#endif

void
FUNCTION (test, trap) (size_t stride, size_t N)
{
  TYPE (gsl_vector) * vc = FUNCTION (create, vector) (stride, N);

  BASE z = {{(ATOMIC)1.2, (ATOMIC)3.4}};
  BASE z1 = {{(ATOMIC)4.5, (ATOMIC)6.7}};

  size_t j = 0;

  status = 0;
  FUNCTION (gsl_vector, set) (vc, j - 1, z);
  gsl_test (!status,
            NAME (gsl_vector) "_set traps index below lower bound");

  status = 0;
  FUNCTION (gsl_vector, set) (vc, N + 1, z);
  gsl_test (!status,
            NAME (gsl_vector) "_set traps index above upper bound");

  status = 0;
  FUNCTION (gsl_vector, set) (vc, N, z);
  gsl_test (!status, NAME (gsl_vector) "_set traps index at upper bound");

  status = 0;
  z1 = FUNCTION (gsl_vector, get) (vc, j - 1);
  gsl_test (!status,
            NAME (gsl_vector) "_get traps index below lower bound");

  gsl_test (GSL_REAL (z1) != 0,
            NAME (gsl_vector) "_get returns zero real below lower bound");
  gsl_test (GSL_IMAG (z1) != 0,
            NAME (gsl_vector) "_get returns zero imag below lower bound");

  status = 0;
  z1 = FUNCTION (gsl_vector, get) (vc, N + 1);
  gsl_test (!status,
            NAME (gsl_vector) "_get traps index above upper bound");
  gsl_test (GSL_REAL (z1) != 0,
            NAME (gsl_vector) "_get returns zero real above upper bound");
  gsl_test (GSL_IMAG (z1) != 0,
            NAME (gsl_vector) "_get returns zero imag above upper bound");

  status = 0;
  z1 = FUNCTION (gsl_vector, get) (vc, N);
  gsl_test (!status, NAME (gsl_vector) "_get traps index at upper bound");
  gsl_test (GSL_REAL (z1) != 0,
            NAME (gsl_vector) "_get returns zero real at upper bound");
  gsl_test (GSL_IMAG (z1) != 0,
            NAME (gsl_vector) "_get returns zero imag at upper bound");

  FUNCTION (gsl_vector, free) (vc);
}




