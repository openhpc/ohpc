#define rosenbrocke_N         8 /* = p */
#define rosenbrocke_P         8 /* must be even */

#define rosenbrocke_NTRIES    4

static double rosenbrocke_x0[rosenbrocke_P] = { -1.2, 1.0, -1.2, 1.0,
                                                -1.2, 1.0, -1.2, 1.0 };

static double rosenbrocke_epsrel = 1.0e-12;

static void
rosenbrocke_checksol(const double x[], const double sumsq,
                     const double epsrel, const char *sname,
                     const char *pname)
{
  size_t i;
  const double sumsq_exact = 0.0;
  const double rosenbrocke_x[rosenbrocke_P] = { 1.0, 1.0, 1.0, 1.0,
                                                1.0, 1.0, 1.0, 1.0 };

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < rosenbrocke_P; ++i)
    {
      gsl_test_rel(x[i], rosenbrocke_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}

static int
rosenbrocke_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  size_t i;

  for (i = 0; i < rosenbrocke_N / 2; ++i)
    {
      double x2i = gsl_vector_get(x, 2*i + 1);
      double x2im1 = gsl_vector_get(x, 2*i);

      gsl_vector_set(f, 2*i, 10.0 * (x2i - x2im1*x2im1));
      gsl_vector_set(f, 2*i + 1, 1.0 - x2im1);
    }

  return GSL_SUCCESS;
}

static int
rosenbrocke_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  size_t i;

  gsl_matrix_set_zero(J);

  for (i = 0; i < rosenbrocke_N / 2; ++i)
    {
      double x2im1 = gsl_vector_get(x, 2*i);

      gsl_matrix_set(J, 2*i, 2*i, -20.0*x2im1);
      gsl_matrix_set(J, 2*i, 2*i + 1, 10.0);
      gsl_matrix_set(J, 2*i + 1, 2*i, -1.0);
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf rosenbrocke_func =
{
  &rosenbrocke_f,
  &rosenbrocke_df,
  NULL,
  rosenbrocke_N,
  rosenbrocke_P,
  NULL,
  0,
  0
};

static test_fdf_problem rosenbrocke_problem =
{
  "rosenbrock_extended",
  rosenbrocke_x0,
  NULL,
  &rosenbrocke_epsrel,
  rosenbrocke_NTRIES,
  &rosenbrocke_checksol,
  &rosenbrocke_func
};
