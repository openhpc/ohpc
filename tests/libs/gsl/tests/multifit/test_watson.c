#define watson_N         31
#define watson_P         6

#define watson_NTRIES    4

static double watson_x0[watson_P] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
static double watson_epsrel = 1.0e-6;

static void
watson_checksol(const double x[], const double sumsq,
                const double epsrel, const char *sname,
                const char *pname)
{
  size_t i;
  const double sumsq_exact = 2.287670053552372e-03;
  const double watson_x[watson_P] = {
    -1.572508640629858e-02,  1.012434869366059e+00, -2.329916259263380e-01,
     1.260430087686035e+00, -1.513728922580576e+00,  9.929964323646112e-01 };

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < watson_P; ++i)
    {
      gsl_test_rel(x[i], watson_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}

static int
watson_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  const double x1 = gsl_vector_get(x, 0);
  const double x2 = gsl_vector_get(x, 1);
  size_t i, j;

  for (i = 0; i < watson_N - 2; ++i)
    {
      double ti = (i + 1) / 29.0;
      double tjm1 = 1.0, tjm2 = 1.0;
      double sum1 = 0.0, sum2 = 0.0;

      for (j = 0; j < watson_P; ++j)
        {
          double xj = gsl_vector_get(x, j);

          sum1 += xj * tjm1;
          tjm1 *= ti;

          if (j > 0)
            {
              sum2 += j * xj * tjm2;
              tjm2 *= ti;
            }
        }

      gsl_vector_set (f, i, sum2 - sum1*sum1 - 1.0);
    }

  gsl_vector_set(f, watson_N - 2, x1);
  gsl_vector_set(f, watson_N - 1, x2 - x1*x1 - 1.0);

  return GSL_SUCCESS;
}

static int
watson_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  double x1 = gsl_vector_get (x, 0);
  size_t i, j;

  gsl_matrix_set_zero(J);

  for (i = 0; i < watson_N - 2; ++i)
    {
      double ti = (i + 1) / 29.0;
      double tjm1 = 1.0, tjm2 = 1.0;
      double sum1 = 0.0;

      for (j = 0; j < watson_P; ++j)
        {
          double xj = gsl_vector_get(x, j);
          sum1 += xj * tjm1;
          tjm1 *= ti;
        }

      tjm1 = 1.0;
      tjm2 = 1.0;
      for (j = 0; j < watson_P; ++j)
        {
          gsl_matrix_set(J, i, j, j * tjm2 - 2.0*sum1*tjm1);
          tjm1 *= ti;

          if (j > 0)
            tjm2 *= ti;
        }
    }

  gsl_matrix_set(J, watson_N - 2, 0, 1.0);
  gsl_matrix_set(J, watson_N - 1, 0, -2.0*x1);
  gsl_matrix_set(J, watson_N - 1, 1, 1.0);

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf watson_func =
{
  &watson_f,
  &watson_df,
  NULL,
  watson_N,
  watson_P,
  NULL,
  0,
  0
};

static test_fdf_problem watson_problem =
{
  "watson",
  watson_x0,
  NULL,
  &watson_epsrel,
  watson_NTRIES,
  &watson_checksol,
  &watson_func
};
