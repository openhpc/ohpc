#define lin1_N         11  /* can be anything >= p */
#define lin1_P         5

#define lin1_NTRIES    3

static double lin1_x0[lin1_P] = { 1.0, 1.0, 1.0, 1.0, 1.0 };
static double lin1_epsrel = 1.0e-10;

static void
lin1_checksol(const double x[], const double sumsq,
              const double epsrel, const char *sname,
              const char *pname)
{
  size_t i;
  const double sumsq_exact = (double) (lin1_N - lin1_P);

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < lin1_P; ++i)
    {
      gsl_test_rel(x[i], -1.0, epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}

static int
lin1_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  size_t i, j;

  for (i = 0; i < lin1_N; ++i)
    {
      double fi = 0.0;

      for (j = 0; j < lin1_P; ++j)
        {
          double xj = gsl_vector_get(x, j);
          double Aij = (i == j) ? 1.0 : 0.0;
          Aij -= 2.0 / lin1_N;
          fi += Aij * xj;
        }

      fi -= 1.0;
      gsl_vector_set(f, i, fi);
    }

  return GSL_SUCCESS;
}

static int
lin1_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  size_t i, j;

  for (i = 0; i < lin1_N; ++i)
    {
      for (j = 0; j < lin1_P; ++j)
        {
          double Jij = (i == j) ? 1.0 : 0.0;
          Jij -= 2.0 / lin1_N;
          gsl_matrix_set(J, i, j, Jij);
        }
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf lin1_func =
{
  &lin1_f,
  &lin1_df,
  NULL,
  lin1_N,
  lin1_P,
  NULL,
  0,
  0
};

static test_fdf_problem lin1_problem =
{
  "linear_full",
  lin1_x0,
  NULL,
  &lin1_epsrel,
  lin1_NTRIES,
  &lin1_checksol,
  &lin1_func
};
