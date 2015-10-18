/* fft/test_complex.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 Brian Gough
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

#include "bitreverse.h"
#include "signals.h"
#include "compare.h"

void FUNCTION(test_complex,func) (size_t stride, size_t n);
int FUNCTION(test,offset) (const BASE data[], size_t stride, 
                           size_t n, size_t offset);
void FUNCTION(test_complex,bitreverse_order) (size_t stride, size_t n) ;
void FUNCTION(test_complex,radix2) (size_t stride, size_t n);

int FUNCTION(test,offset) (const BASE data[], size_t stride, 
                           size_t n, size_t offset)
{
  int status = 0 ;
  size_t i, j, k = 0 ;

  for (i = 0; i < n; i++) 
    {
      k += 2 ;
      
      for (j = 1; j < stride; j++)
        {
          status |= data[k] != k + offset ;
          k++ ;
          status |= data[k] != k + offset ;
          k++ ;
        }
    }
  return status ;
}


void FUNCTION(test_complex,func) (size_t stride, size_t n) 
{
  size_t i ;
  int status ;

  TYPE(gsl_fft_complex_wavetable) * cw ;
  TYPE(gsl_fft_complex_workspace) * cwork ;

  BASE * complex_data = (BASE *) malloc (2 * n * stride * sizeof (BASE));
  BASE * complex_tmp = (BASE *) malloc (2 * n * stride * sizeof (BASE));
  BASE * fft_complex_data = (BASE *) malloc (2 * n * stride * sizeof (BASE));
  BASE * fft_complex_tmp = (BASE *) malloc (2 * n * stride * sizeof (BASE));

  for (i = 0 ; i < 2 * n * stride ; i++)
    {
      complex_data[i] = (BASE)i ;
      complex_tmp[i] = (BASE)(i + 1000.0) ;
      fft_complex_data[i] = (BASE)(i + 2000.0) ;
      fft_complex_tmp[i] = (BASE)(i + 3000.0) ;
    }

  gsl_set_error_handler (NULL); /* abort on any errors */

  /* Test allocation */

  {
    cw = FUNCTION(gsl_fft_complex_wavetable,alloc) (n);
    gsl_test (cw == 0, NAME(gsl_fft_complex_wavetable) 
              "_alloc, n = %d, stride = %d", n, stride);
  }

  {
    cwork = FUNCTION(gsl_fft_complex_workspace,alloc) (n);
    gsl_test (cwork == 0, NAME(gsl_fft_complex_workspace) 
              "_alloc, n = %d", n);
  }


  /* Test mixed radix fft with noise */

  {
    FUNCTION(fft_signal,complex_noise) (n, stride, complex_data, fft_complex_data);
    for (i = 0 ; i < n ; i++)
      {
        REAL(complex_tmp,stride,i) = REAL(complex_data,stride,i) ;
        IMAG(complex_tmp,stride,i) = IMAG(complex_data,stride,i) ;
      }
    
    FUNCTION(gsl_fft_complex,forward) (complex_data, stride, n, cw, cwork);

    for (i = 0 ; i < n ; i++)
      {
        REAL(fft_complex_tmp,stride,i) = REAL(complex_data,stride,i) ;
        IMAG(fft_complex_tmp,stride,i) = IMAG(complex_data,stride,i) ;
      }

    status = FUNCTION(compare_complex,results) ("dft", fft_complex_data,
                                                "fft of noise", complex_data,
                                                stride, n, 1e6);
    gsl_test (status, NAME(gsl_fft_complex) 
              "_forward with signal_noise, n = %d, stride = %d",  n, stride);

    if (stride > 1) 
      {
        status = FUNCTION(test, offset) (complex_data, stride, n, 0) ;
        
        gsl_test (status, NAME(gsl_fft_complex) 
                  "_forward avoids unstrided data, n = %d, stride = %d",
                  n, stride);
      }
  }
  
  /* Test the inverse fft */

  {
    status = FUNCTION(gsl_fft_complex,inverse) (complex_data, stride, n, cw, cwork);
    status = FUNCTION(compare_complex,results) ("orig", complex_tmp,
                                                "fft inverse", complex_data,
                                                stride, n, 1e6);
    gsl_test (status, NAME(gsl_fft_complex) 
              "_inverse with signal_noise, n = %d, stride = %d", n, stride);

    if (stride > 1) 
      {
        status = FUNCTION(test, offset) (complex_data, stride, n, 0) ;
        
        gsl_test (status, NAME(gsl_fft_complex) 
                  "_inverse other data untouched, n = %d, stride = %d",
                  n, stride);
      }

  }

  /* Test the backward fft */

  {
    status = FUNCTION(gsl_fft_complex,backward) (fft_complex_tmp, stride, n, cw, cwork);

    for (i = 0; i < n; i++)
      {
        REAL(complex_tmp,stride,i) *= n;
        IMAG(complex_tmp,stride,i) *= n;
      }
    status = FUNCTION(compare_complex,results) ("orig", 
                                                complex_tmp,
                                                "fft backward", 
                                                fft_complex_tmp,
                                                stride, n, 1e6);

    gsl_test (status, NAME(gsl_fft_complex) 
              "_backward with signal_noise, n = %d, stride = %d", n, stride);

    if (stride > 1) 
      {
        status = FUNCTION(test, offset) (fft_complex_tmp, stride, n, 3000) ;
        
        gsl_test (status, NAME(gsl_fft_complex) 
                  "_backward avoids unstrided data, n = %d, stride = %d",
                  n, stride);
      }

  }

  /* Test a pulse signal */
  
  {
    FUNCTION(fft_signal,complex_pulse) (1, n, stride, 1.0, 0.0, complex_data,
                                        fft_complex_data);
    FUNCTION(gsl_fft_complex,forward) (complex_data, stride, n, cw, cwork);
    status = FUNCTION(compare_complex,results) ("analytic", fft_complex_data,
                                                "fft of pulse", complex_data, 
                                                stride, n, 1e6);
    gsl_test (status, NAME(gsl_fft_complex) 
              "_forward with signal_pulse, n = %d, stride = %d", n, stride);

  }


  /* Test a constant signal */

  {
    FUNCTION(fft_signal,complex_constant) (n, stride, 1.0, 0.0, complex_data,
                                           fft_complex_data);
    FUNCTION(gsl_fft_complex,forward) (complex_data, stride, n, cw, cwork);
    status = FUNCTION(compare_complex,results) ("analytic", fft_complex_data,
                                                "fft of constant", 
                                                complex_data,
                                                stride, n, 1e6);
    gsl_test (status, NAME(gsl_fft_complex) 
              "_forward with signal_constant, n = %d, stride = %d", n, stride);
  }

  /* Test an exponential (cos/sin) signal */
  
  {
    status = 0;
    for (i = 0; i < n; i++)
      {
        FUNCTION(fft_signal,complex_exp) ((int)i, n, stride, 1.0, 0.0, complex_data,
                                          fft_complex_data);
        FUNCTION(gsl_fft_complex,forward) (complex_data, stride, n, cw, cwork);
        status |= FUNCTION(compare_complex,results) ("analytic", 
                                                     fft_complex_data,
                                                     "fft of exp", 
                                                     complex_data,
                                                     stride, n, 1e6);
      }
    gsl_test (status, NAME(gsl_fft_complex) 
              "_forward with signal_exp, n = %d, stride = %d", n, stride);
  }

  FUNCTION(gsl_fft_complex_wavetable,free) (cw);
  FUNCTION(gsl_fft_complex_workspace,free) (cwork);
  
  free (complex_data);
  free (complex_tmp);
  free (fft_complex_data);
  free (fft_complex_tmp);
}


void 
FUNCTION(test_complex,bitreverse_order) (size_t stride, size_t n) 
{
  int status ;
  size_t logn, i ;

  BASE * tmp = (BASE *) malloc (2 * n * stride * sizeof (BASE));
  BASE * data = (BASE *) malloc (2 * n * stride * sizeof (BASE));
  BASE * reversed_data = (BASE *) malloc (2 * n * stride * sizeof (BASE));
  
  for (i = 0; i <  2 * stride * n; i++) 
    {
      data[i] = (BASE)i ;
    }

  memcpy (tmp, data, 2 * n * stride * sizeof(BASE)) ;

  logn = 0 ; while (n > (1U<<logn)) {logn++ ; } ;

  /* do a naive bit reversal as a baseline for testing the other routines */

  for (i = 0; i < n; i++) 
    {
      size_t i_tmp = i ;
      size_t j = 0 ;
      size_t bit ;

      for (bit = 0; bit < logn; bit++)
        {
          j <<= 1;              /* reverse shift i into j */
          j |= i_tmp & 1;
          i_tmp >>= 1;
        }

      reversed_data[2*j*stride] = data[2*i*stride] ;
      reversed_data[2*j*stride+1] = data[2*i*stride+1] ;
    }

  FUNCTION(fft_complex,bitreverse_order) (data, stride, n, logn);

  status = FUNCTION(compare_complex,results) ("naive bit reverse", 
                                              reversed_data,
                                              "fft_complex_bitreverse_order", 
                                              data,
                                              stride, n, 1e6);

  gsl_test (status, "fft_complex_bitreverse_order, n = %d", n);

  free (reversed_data) ;
  free (data) ;
  free (tmp) ;
}

void FUNCTION(test_complex,radix2) (size_t stride, size_t n) 
{
  size_t i ;
  int status ;

  BASE * complex_data = (BASE *) malloc (2 * n * stride * sizeof (BASE));
  BASE * complex_tmp = (BASE *) malloc (2 * n * stride * sizeof (BASE));
  BASE * fft_complex_data = (BASE *) malloc (2 * n * stride * sizeof (BASE));
  BASE * fft_complex_tmp = (BASE *) malloc (2 * n * stride * sizeof (BASE));

  for (i = 0 ; i < 2 * n * stride ; i++)
    {
      complex_data[i] = (BASE)i ;
      complex_tmp[i] = (BASE)(i + 1000.0) ;
      fft_complex_data[i] = (BASE)(i + 2000.0) ;
      fft_complex_tmp[i] = (BASE)(i + 3000.0) ;
    }

  gsl_set_error_handler (NULL); /* abort on any errors */

  /* Test radix-2 fft with noise */

  {
    FUNCTION(fft_signal,complex_noise) (n, stride, complex_data, 
                                        fft_complex_data);
    for (i = 0 ; i < n ; i++)
      {
        REAL(complex_tmp,stride,i) = REAL(complex_data,stride,i) ;
        IMAG(complex_tmp,stride,i) = IMAG(complex_data,stride,i) ;
      }
    
    FUNCTION(gsl_fft_complex,radix2_forward) (complex_data, stride, n);

    for (i = 0 ; i < n ; i++)
      {
        REAL(fft_complex_tmp,stride,i) = REAL(complex_data,stride,i) ;
        IMAG(fft_complex_tmp,stride,i) = IMAG(complex_data,stride,i) ;
      }

    status = FUNCTION(compare_complex,results) ("dft", fft_complex_data,
                                                "fft of noise", complex_data,
                                                stride, n, 1e6);
    gsl_test (status, NAME(gsl_fft_complex) 
              "_radix2_forward with signal_noise, n = %d, stride = %d",  
              n, stride);

    if (stride > 1) 
      {
        status = FUNCTION(test, offset) (complex_data, stride, n, 0) ;
        
        gsl_test (status, NAME(gsl_fft_complex) 
                  "_radix2_forward avoids unstrided data, n = %d, stride = %d",
                  n, stride);
      }
  }
  
  /* Test the inverse fft */

  {
    status = FUNCTION(gsl_fft_complex,radix2_inverse) (complex_data, stride, n);
    status = FUNCTION(compare_complex,results) ("orig", complex_tmp,
                                                "fft inverse", complex_data,
                                                stride, n, 1e6);
    gsl_test (status, NAME(gsl_fft_complex) 
              "_radix2_inverse with signal_noise, n = %d, stride = %d", n, stride);

    if (stride > 1) 
      {
        status = FUNCTION(test, offset) (complex_data, stride, n, 0) ;
        
        gsl_test (status, NAME(gsl_fft_complex) 
                  "_radix2_inverse other data untouched, n = %d, stride = %d",
                  n, stride);
      }

  }

  /* Test the backward fft */

  {
    status = FUNCTION(gsl_fft_complex,radix2_backward) (fft_complex_tmp, stride, n);

    for (i = 0; i < n; i++)
      {
        REAL(complex_tmp,stride,i) *= n;
        IMAG(complex_tmp,stride,i) *= n;
      }
    status = FUNCTION(compare_complex,results) ("orig", 
                                                complex_tmp,
                                                "fft backward", 
                                                fft_complex_tmp,
                                                stride, n, 1e6);

    gsl_test (status, NAME(gsl_fft_complex) 
              "_radix2_backward with signal_noise, n = %d, stride = %d", n, stride);

    if (stride > 1) 
      {
        status = FUNCTION(test, offset) (fft_complex_tmp, stride, n, 3000) ;
        
        gsl_test (status, NAME(gsl_fft_complex) 
                  "_radix2_backward avoids unstrided data, n = %d, stride = %d",
                  n, stride);
      }

  }

  /* Test a pulse signal */
  
  {
    FUNCTION(fft_signal,complex_pulse) (1, n, stride, 1.0, 0.0, complex_data,
                                        fft_complex_data);
    FUNCTION(gsl_fft_complex,radix2_forward) (complex_data, stride, n);
    status = FUNCTION(compare_complex,results) ("analytic", fft_complex_data,
                                                "fft of pulse", complex_data, 
                                                stride, n, 1e6);
    gsl_test (status, NAME(gsl_fft_complex) 
              "_radix2_forward with signal_pulse, n = %d, stride = %d", n, stride);

  }


  /* Test a constant signal */

  {
    FUNCTION(fft_signal,complex_constant) (n, stride, 1.0, 0.0, complex_data,
                                           fft_complex_data);
    FUNCTION(gsl_fft_complex,radix2_forward) (complex_data, stride, n);
    status = FUNCTION(compare_complex,results) ("analytic", fft_complex_data,
                                                "fft of constant", 
                                                complex_data,
                                                stride, n, 1e6);
    gsl_test (status, NAME(gsl_fft_complex) 
              "_radix2_forward with signal_constant, n = %d, stride = %d", 
              n, stride);
  }

  /* Test an exponential (cos/sin) signal */
  
  {
    status = 0;
    for (i = 0; i < n; i++)
      {
        FUNCTION(fft_signal,complex_exp) ((int)i, n, stride, 1.0, 0.0, complex_data,
                                          fft_complex_data);
        FUNCTION(gsl_fft_complex,radix2_forward) (complex_data, stride, n);
        status |= FUNCTION(compare_complex,results) ("analytic", 
                                                     fft_complex_data,
                                                     "fft of exp", 
                                                     complex_data,
                                                     stride, n, 1e6);
      }
    gsl_test (status, NAME(gsl_fft_complex) 
              "_radix2_forward with signal_exp, n = %d, stride = %d", n, stride);
  }

  
  free (complex_data);
  free (complex_tmp);
  free (fft_complex_data);
  free (fft_complex_tmp);
}

