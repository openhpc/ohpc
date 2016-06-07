#define enso_N       168
#define enso_P       9

#define enso_NTRIES  1

static double enso_x0[enso_P] = { 10.0, 3.0, 0.5, 44.0, -1.5, 0.5, 26.0, 0.1, 1.5 };


static double enso_epsrel = 1.0e-3;

static double enso_sigma[enso_P] = {
 1.7488832467E-01,
 2.4310052139E-01,
 2.4354686618E-01,
 9.4408025976E-01,
 2.8078369611E-01,
 4.8073701119E-01,
 4.1612939130E-01,
 5.1460022911E-01,
 2.5434468893E-01
};

static double enso_F[enso_N] = {
    12.90000, 
    11.30000, 
    10.60000, 
    11.20000, 
    10.90000, 
    7.500000, 
    7.700000, 
    11.70000, 
    12.90000, 
    14.30000, 
    10.90000, 
    13.70000, 
    17.10000, 
    14.00000, 
    15.30000, 
    8.500000, 
    5.700000, 
    5.500000, 
    7.600000, 
    8.600000, 
    7.300000, 
    7.600000, 
    12.70000, 
    11.00000, 
    12.70000, 
    12.90000, 
    13.00000, 
    10.90000, 
   10.400000, 
   10.200000, 
    8.000000, 
    10.90000, 
    13.60000, 
   10.500000, 
    9.200000, 
    12.40000, 
    12.70000, 
    13.30000, 
   10.100000, 
    7.800000, 
    4.800000, 
    3.000000, 
    2.500000, 
    6.300000, 
    9.700000, 
    11.60000, 
    8.600000, 
    12.40000, 
   10.500000, 
    13.30000, 
   10.400000, 
    8.100000, 
    3.700000, 
    10.70000, 
    5.100000, 
   10.400000, 
    10.90000, 
    11.70000, 
    11.40000, 
    13.70000, 
    14.10000, 
    14.00000, 
    12.50000, 
    6.300000, 
    9.600000, 
    11.70000, 
    5.000000, 
    10.80000, 
    12.70000, 
    10.80000, 
    11.80000, 
    12.60000, 
    15.70000, 
    12.60000, 
    14.80000, 
    7.800000, 
    7.100000, 
    11.20000, 
    8.100000, 
    6.400000, 
    5.200000, 
    12.00000, 
   10.200000, 
    12.70000, 
   10.200000, 
    14.70000, 
    12.20000, 
    7.100000, 
    5.700000, 
    6.700000, 
    3.900000, 
    8.500000, 
    8.300000, 
    10.80000, 
    16.70000, 
    12.60000, 
    12.50000, 
    12.50000, 
    9.800000, 
    7.200000, 
    4.100000, 
    10.60000, 
   10.100000, 
   10.100000, 
    11.90000, 
    13.60000, 
    16.30000, 
    17.60000, 
    15.50000, 
    16.00000, 
    15.20000, 
    11.20000, 
    14.30000, 
    14.50000, 
    8.500000, 
    12.00000, 
    12.70000, 
    11.30000, 
    14.50000, 
    15.10000, 
   10.400000, 
    11.50000, 
    13.40000, 
    7.500000, 
   0.6000000, 
   0.3000000, 
    5.500000, 
    5.000000, 
    4.600000, 
    8.200000, 
    9.900000, 
    9.200000, 
    12.50000, 
    10.90000, 
    9.900000, 
    8.900000, 
    7.600000, 
    9.500000, 
    8.400000, 
    10.70000, 
    13.60000, 
    13.70000, 
    13.70000, 
    16.50000, 
    16.80000, 
    17.10000, 
    15.40000, 
    9.500000, 
    6.100000, 
   10.100000, 
    9.300000, 
    5.300000, 
    11.20000, 
    16.60000, 
    15.60000, 
    12.00000, 
    11.50000, 
    8.600000, 
    13.80000, 
    8.700000, 
    8.600000, 
    8.600000, 
    8.700000, 
    12.80000, 
    13.20000, 
    14.00000, 
    13.40000, 
    14.80000
};

static void
enso_checksol(const double x[], const double sumsq,
              const double epsrel, const char *sname,
              const char *pname)
{
  size_t i;
  const double sumsq_exact = 7.8853978668E+02;
  const double enso_x[enso_P] = {
    1.0510749193E+01, 3.0762128085E+00, 5.3280138227E-01,
    4.4311088700E+01, -1.6231428586E+00, 5.2554493756E-01,
    2.6887614440E+01, 2.1232288488E-01, 1.4966870418E+00 };

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < enso_P; ++i)
    {
      gsl_test_rel(x[i], enso_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}


static int
enso_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double b[enso_P];
  size_t i;

  for (i = 0; i < enso_P; i++)
    {
      b[i] = gsl_vector_get(x, i);
    }

  for (i = 0; i < enso_N; i++)
    {
      double t = (i + 1.0);
      double y;
      y = b[0];
      y += b[1] * cos(2*M_PI*t/12);
      y += b[2] * sin(2*M_PI*t/12);
      y += b[4] * cos(2*M_PI*t/b[3]);
      y += b[5] * sin(2*M_PI*t/b[3]);
      y += b[7] * cos(2*M_PI*t/b[6]);
      y += b[8] * sin(2*M_PI*t/b[6]);

      gsl_vector_set (f, i, enso_F[i] - y);
    }

  return GSL_SUCCESS;
}

static int
enso_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double b[enso_P];
  size_t i;

  for (i = 0; i < enso_P; i++)
    {
      b[i] = gsl_vector_get(x, i);
    }

  for (i = 0; i < enso_N; i++)
    {
      double t = (i + 1.0);

      gsl_matrix_set (df, i, 0, -1.0);
      gsl_matrix_set (df, i, 1, -cos(2*M_PI*t/12));
      gsl_matrix_set (df, i, 2, -sin(2*M_PI*t/12));
      gsl_matrix_set (df, i, 3, 
                      -b[4]*(2*M_PI*t/(b[3]*b[3]))*sin(2*M_PI*t/b[3])
                      +b[5]*(2*M_PI*t/(b[3]*b[3]))*cos(2*M_PI*t/b[3]));
      gsl_matrix_set (df, i, 4, -cos(2*M_PI*t/b[3]));
      gsl_matrix_set (df, i, 5, -sin(2*M_PI*t/b[3]));
      gsl_matrix_set (df, i, 6, 
                     -b[7] * (2*M_PI*t/(b[6]*b[6])) * sin(2*M_PI*t/b[6])
                     +b[8] * (2*M_PI*t/(b[6]*b[6])) * cos(2*M_PI*t/b[6]));
      gsl_matrix_set (df, i, 7, -cos(2*M_PI*t/b[6]));
      gsl_matrix_set (df, i, 8, -sin(2*M_PI*t/b[6]));
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf enso_func =
{
  &enso_f,
  &enso_df,
  NULL,
  enso_N,
  enso_P,
  NULL,
  0,
  0
};

static test_fdf_problem enso_problem =
{
  "nist-ENSO",
  enso_x0,
  enso_sigma,
  &enso_epsrel,
  enso_NTRIES,
  &enso_checksol,
  &enso_func
};
