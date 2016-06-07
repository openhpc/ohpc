#define meyerscal_N         16
#define meyerscal_P         3

#define meyerscal_NTRIES    1

static double meyerscal_x0[meyerscal_P] = { 8.85, 4.0, 2.5 };
static double meyerscal_epsrel = 1.0e-8;

static double meyerscal_Y[meyerscal_N] = {
34780., 28610., 23650., 19630., 16370., 13720., 11540.,
9744.,  8261.,  7030.,  6005., 5147., 4427., 3820.,
3307.,  2872.
};

static void
meyerscal_checksol(const double x[], const double sumsq,
                   const double epsrel, const char *sname,
                   const char *pname)
{
  size_t i;
  const double sumsq_exact = 8.794585517003888e-05;
  const double meyerscal_x[meyerscal_P] = { 2.481778312286695e+00,
                                            6.181346341853554e+00,
                                            3.452236344749865e+00 };

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < meyerscal_P; ++i)
    {
      gsl_test_rel(x[i], meyerscal_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}

static int
meyerscal_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  double x3 = gsl_vector_get(x, 2);
  size_t i;

  for (i = 0; i < meyerscal_N; ++i)
    {
      double ti = 0.45 + 0.05*(i + 1.0);
      double yi = meyerscal_Y[i];
      double fi = x1 * exp(10.0*x2 / (ti + x3) - 13.0) - 1.0e-3*yi;
      gsl_vector_set(f, i, fi);
    }

  return GSL_SUCCESS;
}

static int
meyerscal_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  double x3 = gsl_vector_get(x, 2);
  size_t i;

  for (i = 0; i < meyerscal_N; ++i)
    {
      double ti = 0.45 + 0.05*(i + 1.0);
      double term1 = ti + x3;
      double term2 = exp(10.0*x2/term1 - 13.0);

      gsl_matrix_set(J, i, 0, term2);
      gsl_matrix_set(J, i, 1, 10.0*x1*term2/term1);
      gsl_matrix_set(J, i, 2, -10.0*x1*x2*term2/(term1*term1));
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf meyerscal_func =
{
  &meyerscal_f,
  &meyerscal_df,
  NULL,
  meyerscal_N,
  meyerscal_P,
  NULL,
  0,
  0
};

static test_fdf_problem meyerscal_problem =
{
  "meyerscal",
  meyerscal_x0,
  NULL,
  &meyerscal_epsrel,
  meyerscal_NTRIES,
  &meyerscal_checksol,
  &meyerscal_func
};
