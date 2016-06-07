#include <config.h>
#include <stdio.h>
#include <math.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_test.h>

#include <gsl/gsl_wavelet.h>
#include <gsl/gsl_wavelet2d.h>

#define N_BS 11

double urand (void);

double
urand (void)
{
  static unsigned long int x = 1;
  x = (1103515245 * x + 12345) & 0x7fffffffUL;
  return x / 2147483648.0;
}

const size_t member[N_BS] =
  { 309, 307, 305, 303, 301, 208, 206, 204, 202, 105, 103 };

void
test_1d (size_t N, size_t stride, const gsl_wavelet_type * T, size_t member);

void
test_2d (size_t N, size_t tda, const gsl_wavelet_type * T, size_t member, int type);

int
main (int argc, char **argv)
{
  size_t i, N, stride, tda;
  const int S = 1, NS = 2;  /* Standard & Non-standard transforms */

  /* One-dimensional tests */

  for (N = 1; N <= 16384; N *= 2)
    {
      for (stride = 1; stride <= 5; stride++)
        {
          for (i = 0; i < N_BS; i++)
            {
              test_1d (N, stride, gsl_wavelet_bspline, member[i]);
              test_1d (N, stride, gsl_wavelet_bspline_centered, member[i]);
            }

          for (i = 4; i <= 20; i += 2)
            {
              test_1d (N, stride, gsl_wavelet_daubechies, i);
              test_1d (N, stride, gsl_wavelet_daubechies_centered, i);
            }

          test_1d (N, stride, gsl_wavelet_haar, 2);
          test_1d (N, stride, gsl_wavelet_haar_centered, 2);
        }
    }

  /* Two-dimensional tests */

  for (N = 1; N <= 64; N *= 2)
    {
      for (tda = N; tda <= N + 5; tda++)
        {
          for (i = 0; i < N_BS; i++)
            {
              test_2d (N, tda, gsl_wavelet_bspline, member[i], S);
              test_2d (N, tda, gsl_wavelet_bspline_centered, member[i], S);

              test_2d (N, tda, gsl_wavelet_bspline, member[i], NS);
              test_2d (N, tda, gsl_wavelet_bspline_centered, member[i], NS);
            }
          
          for (i = 4; i <= 20; i += 2)
            {
              test_2d (N, tda, gsl_wavelet_daubechies, i, S);
              test_2d (N, tda, gsl_wavelet_daubechies_centered, i, S);

              test_2d (N, tda, gsl_wavelet_daubechies, i, NS);
              test_2d (N, tda, gsl_wavelet_daubechies_centered, i, NS);
            }
          
          test_2d (N, tda, gsl_wavelet_haar, 2, S);
          test_2d (N, tda, gsl_wavelet_haar_centered, 2, S);

          test_2d (N, tda, gsl_wavelet_haar, 2, NS);
          test_2d (N, tda, gsl_wavelet_haar_centered, 2, NS);
        }
    }

  exit (gsl_test_summary ());
}


void
test_1d (size_t N, size_t stride, const gsl_wavelet_type * T, size_t member)
{
  gsl_wavelet_workspace *work;
  gsl_vector *v1, *v2, *vdelta;
  gsl_vector_view v;
  gsl_wavelet *w;

  size_t i;
  double *data = (double *)malloc (N * stride * sizeof (double));

  for (i = 0; i < N * stride; i++)
    data[i] = 12345.0 + i;

  v = gsl_vector_view_array_with_stride (data, stride, N);
  v1 = &(v.vector);

  for (i = 0; i < N; i++)
    {
      gsl_vector_set (v1, i, urand ());
    }

  v2 = gsl_vector_alloc (N);
  gsl_vector_memcpy (v2, v1);

  vdelta = gsl_vector_alloc (N);

  work = gsl_wavelet_workspace_alloc (N);

  w = gsl_wavelet_alloc (T, member);

  gsl_wavelet_transform_forward (w, v2->data, v2->stride, v2->size, work);
  gsl_wavelet_transform_inverse (w, v2->data, v2->stride, v2->size, work);

  for (i = 0; i < N; i++)
    {
      double x1 = gsl_vector_get (v1, i);
      double x2 = gsl_vector_get (v2, i);
      gsl_vector_set (vdelta, i, fabs (x1 - x2));
    }

  {
    double x1, x2;
    i = gsl_vector_max_index (vdelta);
    x1 = gsl_vector_get (v1, i);
    x2 = gsl_vector_get (v2, i);

    gsl_test (fabs (x2 - x1) > N * 1e-15,
              "%s(%d), n = %d, stride = %d, maxerr = %g",
              gsl_wavelet_name (w), member, N, stride, fabs (x2 - x1));
  }

  if (stride > 1)
    {
      int status = 0;

      for (i = 0; i < N * stride; i++)
        {
          if (i % stride == 0)
            continue;

          status |= (data[i] != (12345.0 + i));
        }

      gsl_test (status, "%s(%d) other data untouched, n = %d, stride = %d",
                gsl_wavelet_name (w), member, N, stride);
    }

  gsl_wavelet_workspace_free (work);
  gsl_wavelet_free (w);
  gsl_vector_free (vdelta);
  gsl_vector_free (v2);
  free (data);
}


void
test_2d (size_t N, size_t tda, const gsl_wavelet_type * T, size_t member, int type)
{
  gsl_wavelet_workspace *work;
  gsl_matrix *m2;
  gsl_wavelet *w;
  gsl_matrix *m1;
  gsl_matrix *mdelta;
  gsl_matrix_view m;
  size_t i;
  size_t j;

  double *data = (double *)malloc (N * tda * sizeof (double));

  const char * name;

  name = (type == 1) ? "standard" : "nonstd" ;

  for (i = 0; i < N * tda; i++)
    data[i] = 12345.0 + i;

  m = gsl_matrix_view_array_with_tda (data, N, N, tda);
  m1 = &(m.matrix);

  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
            gsl_matrix_set (m1, i, j, urand());
        }
    }

  m2 = gsl_matrix_alloc (N, N);
  gsl_matrix_memcpy (m2, m1);
  
  mdelta = gsl_matrix_alloc (N, N);

  work = gsl_wavelet_workspace_alloc (N);

  w = gsl_wavelet_alloc (T, member);

  switch (type) 
    {
    case 1:
      gsl_wavelet2d_transform_matrix_forward (w, m2, work);
      gsl_wavelet2d_transform_matrix_inverse (w, m2, work);
      break;
    case 2:
      gsl_wavelet2d_nstransform_matrix_forward (w, m2, work);
      gsl_wavelet2d_nstransform_matrix_inverse (w, m2, work);
      break;
    }

  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
          double x1 = gsl_matrix_get (m1, i, j);
          double x2 = gsl_matrix_get (m2, i, j );
          gsl_matrix_set (mdelta, i, j, fabs (x1 - x2));
        }
    }

  {
    double x1, x2;
    gsl_matrix_max_index (mdelta, &i, &j);
    x1 = gsl_matrix_get (m1, i, j);
    x2 = gsl_matrix_get (m2, i, j);

    gsl_test (fabs (x2 - x1) > N * 1e-15,
              "%s(%d)-2d %s, n = %d, tda = %d, maxerr = %g",
              gsl_wavelet_name (w), member, name, N, tda, fabs (x2 - x1));
  }

  if (tda > N)
    {
      int status = 0;

      for (i = 0; i < N ; i++)
        {
          for (j = N; j < tda; j++)
            {
              status |= (data[i*tda+j] != (12345.0 + (i*tda+j)));
            }
        }

      gsl_test (status, "%s(%d)-2d %s other data untouched, n = %d, tda = %d",
                gsl_wavelet_name (w), member, name, N, tda);
    }
  
  free (data);
  gsl_wavelet_workspace_free (work);
  gsl_wavelet_free (w);
  gsl_matrix_free (m2);
  gsl_matrix_free (mdelta);
}
