#define bard_N         15
#define bard_P         3

#define bard_NTRIES    3

static double bard_x0[bard_P] = { 1.0, 1.0, 1.0 };

static double bard_epsrel = 1.0e-8;

static double bard_Y[bard_N] = {
0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 0.37,
0.58, 0.73, 0.96, 1.34, 2.10, 4.39
};

static void
bard_checksol(const double x[], const double sumsq,
              const double epsrel, const char *sname,
              const char *pname)
{
  size_t i;
  const double sumsq_exact1 = 8.214877306578963e-03;
  const double bard_x1[bard_P] = { 8.241055975623580e-02,
                                   1.133036092245175,
                                   2.343695178435405 };
  const double sumsq_exact2 = 17.42869333333333;
  const double bard_x2[bard_P] = { 8.406666666666666e-01,
                                   GSL_NAN,    /* -inf */
                                   GSL_NAN };  /* -inf */
  const double *bard_x;
  double sumsq_exact;

  if (fabs(x[1]) < 10.0 && fabs(x[2]) < 10.0)
    {
      bard_x = bard_x1;
      sumsq_exact = sumsq_exact1;
    }
  else
    {
      bard_x = bard_x2;
      sumsq_exact = sumsq_exact2;
    }

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < bard_P; ++i)
    {
      if (!gsl_finite(bard_x[i]))
        continue;

      gsl_test_rel(x[i], bard_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}

static int
bard_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  double x3 = gsl_vector_get(x, 2);
  size_t i;

  for (i = 0; i < bard_N; ++i)
    {
      double ui = i + 1.0;
      double vi = 16.0 - i - 1.0;
      double wi = GSL_MIN(ui, vi);
      double yi = bard_Y[i];
      double fi = yi - (x1 + (ui / (x2*vi + x3*wi)));

      gsl_vector_set(f, i, fi);
    }

  return GSL_SUCCESS;
}

static int
bard_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  double x2 = gsl_vector_get(x, 1);
  double x3 = gsl_vector_get(x, 2);
  size_t i;

  for (i = 0; i < bard_N; ++i)
    {
      double ui = i + 1.0;
      double vi = 16.0 - i - 1.0;
      double wi = GSL_MIN(ui, vi);
      double term = x2 * vi + x3 * wi;

      gsl_matrix_set(J, i, 0, -1.0);
      gsl_matrix_set(J, i, 1, ui * vi / (term * term));
      gsl_matrix_set(J, i, 2, ui * wi / (term * term));
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf bard_func =
{
  &bard_f,
  &bard_df,
  NULL,
  bard_N,
  bard_P,
  NULL,
  0,
  0
};

static test_fdf_problem bard_problem =
{
  "bard",
  bard_x0,
  NULL,
  &bard_epsrel,
  bard_NTRIES,
  &bard_checksol,
  &bard_func
};
