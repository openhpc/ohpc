void
test_estimator ()
{
  gsl_vector_view c;
  gsl_matrix_view cov;
  gsl_vector_view x;
  double y, y_err;
  
  double cov_ij[25] = {    
    4.271520, -0.526675,  0.957930,  0.267750, -0.103610,
    -0.526675,  5.701680, -0.098080,  0.641845,  0.429780,
    0.957930, -0.098080,  4.584790,  0.375865,  1.510810,
    0.267750,  0.641845,  0.375865,  4.422720,  0.392210,
  -0.103610,  0.429780,  1.510810,  0.392210,  5.782750

 };
    
  double c_i[5] = {  
    -0.627020,   0.848674,   0.216877,  -0.057883,   0.596668
  };

  double x_i[5] = {
    0.99932,   0.23858,   0.19797,   1.44008,  -0.15335
  };

  double y_expected = -5.56037032230000e-01;
  double yerr_expected = 3.91891123349318e+00;

  cov = gsl_matrix_view_array(cov_ij, 5, 5);
  c = gsl_vector_view_array(c_i, 5);
  x = gsl_vector_view_array(x_i, 5);

  gsl_multifit_linear_est(&x.vector , &c.vector, &cov.matrix, &y, &y_err);

  gsl_test_rel (y, y_expected, 256*GSL_DBL_EPSILON, "gsl_multifit_linear_est y");
  gsl_test_rel (y_err, yerr_expected, 256*GSL_DBL_EPSILON, "gsl_multifit_linear_est yerr");
}
