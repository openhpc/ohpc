#define exp1_N         45
#define exp1_P         4

#define exp1_NTRIES    3

static double exp1_x0[exp1_P] = { -1.0, -2.0, 1.0, -1.0 };
static double exp1_epsrel = 1.0e-4;

static double exp1_Y[exp1_N] = {
0.090542, 0.124569, 0.179367, 0.195654, 0.269707,
0.286027, 0.289892, 0.317475, 0.308191, 0.336995,
0.348371, 0.321337, 0.299423, 0.338972, 0.304763,
0.288903, 0.300820, 0.303974, 0.283987, 0.262078,
0.281593, 0.267531, 0.218926, 0.225572, 0.200594,
0.197375, 0.182440, 0.183892, 0.152285, 0.174028,
0.150874, 0.126220, 0.126266, 0.106384, 0.118923,
0.091868, 0.128926, 0.119273, 0.115997, 0.105831,
0.075261, 0.068387, 0.090823, 0.085205, 0.067203
};

static void
exp1_checksol(const double x[], const double sumsq,
              const double epsrel, const char *sname,
              const char *pname)
{
  size_t i;
  const double sumsq_exact = 1.0e-2;
  const double exp1_x[exp1_P] = { -4.0, -5.0, 4.0, -4.0 }; /* approx */

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < exp1_P; ++i)
    {
      gsl_test_rel(x[i], exp1_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}

static int
exp1_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  double x3 = gsl_vector_get(x, 2);
  double x4 = gsl_vector_get(x, 3);
  size_t i;

  for (i = 0; i < exp1_N; ++i)
    {
      double ti = 0.02*(i + 1.0);
      double yi = exp1_Y[i];
      double fi = yi - (x3*exp(x1*ti) + x4*exp(x2*ti));
      gsl_vector_set(f, i, fi);
    }

  return GSL_SUCCESS;
}

static int
exp1_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  double x3 = gsl_vector_get(x, 2);
  double x4 = gsl_vector_get(x, 3);
  size_t i;

  for (i = 0; i < exp1_N; ++i)
    {
      double ti = 0.02*(i + 1.0);
      double term1 = exp(x1*ti);
      double term2 = exp(x2*ti);

      gsl_matrix_set(J, i, 0, -x3*ti*term1);
      gsl_matrix_set(J, i, 1, -x4*ti*term2);
      gsl_matrix_set(J, i, 2, -term1);
      gsl_matrix_set(J, i, 3, -term2);
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf exp1_func =
{
  &exp1_f,
  &exp1_df,
  NULL,
  exp1_N,
  exp1_P,
  NULL,
  0,
  0
};

static test_fdf_problem exp1_problem =
{
  "expfit1",
  exp1_x0,
  NULL,
  &exp1_epsrel,
  exp1_NTRIES,
  &exp1_checksol,
  &exp1_func
};
