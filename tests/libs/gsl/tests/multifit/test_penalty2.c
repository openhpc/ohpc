#define penalty2_N         8 /* 2*p */
#define penalty2_P         4

#define penalty2_NTRIES    3

static double penalty2_x0[penalty2_P] = { 0.5, 0.5, 0.5, 0.5 };
static double penalty2_epsrel = 1.0e-12;

static void
penalty2_checksol(const double x[], const double sumsq,
                  const double epsrel, const char *sname,
                  const char *pname)
{
  const double sumsq_exact = 9.37629300735544219e-06;

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);
}

static int
penalty2_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  const double alpha = 1.0e-5;
  const double sqrt_alpha = sqrt(alpha);
  double x1 = gsl_vector_get(x, 0);
  size_t i;
  double sum = penalty2_P * x1 * x1;

  gsl_vector_set(f, 0, x1 - 0.2);

  /* rows [2:p] */
  for (i = 1; i < penalty2_P; ++i)
    {
      double xi = gsl_vector_get(x, i);
      double xim1 = gsl_vector_get(x, i - 1);
      double yi = exp(0.1*(i + 1.0)) + exp(0.1*i);

      gsl_vector_set(f, i, sqrt_alpha*(exp(0.1*xi) + exp(0.1*xim1) - yi));

      sum += (penalty2_P - i) * xi * xi;
    }

  /* rows [p+1:2p-1] */
  for (i = penalty2_P; i < penalty2_N - 1; ++i)
    {
      double xi = gsl_vector_get(x, i - penalty2_P + 1);

      gsl_vector_set(f, i, sqrt_alpha*(exp(0.1*xi) - exp(-0.1)));
    }

  /* row 2p */
  gsl_vector_set(f, penalty2_N - 1, sum - 1.0);

  return GSL_SUCCESS;
}

static int
penalty2_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  const double alpha = 1.0e-5;
  const double sqrt_alpha = sqrt(alpha);
  size_t i, j;

  for (j = 0; j < penalty2_P; ++j)
    {
      double xj = gsl_vector_get(x, j);
      double delta1j = (j == 0) ? 1.0 : 0.0;

      /* first and last rows */
      gsl_matrix_set(J, 0, j, delta1j);
      gsl_matrix_set(J, penalty2_N - 1, j, 2.0 * (penalty2_P - j) * xj);

      /* rows [2:p] */
      for (i = 1; i < penalty2_P; ++i)
        {
          double xi = gsl_vector_get(x, i);
          double xim1 = gsl_vector_get(x, i - 1);
          double Jij;

          if (i == j)
            Jij = exp(0.1 * xi);
          else if (i - 1 == j)
            Jij = exp(0.1 * xim1);
          else
            Jij = 0.0;

          Jij *= 0.1 * sqrt_alpha;

          gsl_matrix_set(J, i, j, Jij);
        }

      /* rows [p+1:2p-1] */
      for (i = penalty2_P; i < penalty2_N - 1; ++i)
        {
          double xi = gsl_vector_get(x, i - penalty2_P + 1);

          if (i - penalty2_P + 1 == j)
            gsl_matrix_set(J, i, j, 0.1 * sqrt_alpha * exp(0.1*xi));
          else
            gsl_matrix_set(J, i, j, 0.0);
        }
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf penalty2_func =
{
  &penalty2_f,
  &penalty2_df,
  NULL,
  penalty2_N,
  penalty2_P,
  NULL,
  0,
  0
};

static test_fdf_problem penalty2_problem =
{
  "penalty2",
  penalty2_x0,
  NULL,
  &penalty2_epsrel,
  penalty2_NTRIES,
  &penalty2_checksol,
  &penalty2_func
};
