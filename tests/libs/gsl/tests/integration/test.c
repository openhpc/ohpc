/* integration/test.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 Brian Gough
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <config.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>

#include "tests.h"

gsl_function make_function (double (* f) (double, void *), double * p);

gsl_function make_function (double (* f) (double, void *), double * p)
{
  gsl_function f_new;

  f_new.function = f ;
  f_new.params = p ;

  return f_new;
}

struct counter_params {
  gsl_function * f;
  int neval;
} ;

double counter (double x, void * params);
gsl_function make_counter (gsl_function * f, struct counter_params * p);

double 
counter (double x, void * params)
{
  struct counter_params * p = (struct counter_params *) params;
  p->neval++ ; /* increment counter */
  return GSL_FN_EVAL(p->f, x);
}

gsl_function make_counter (gsl_function * f, struct counter_params * p)
{
  gsl_function f_new;

  p->f = f;
  p->neval = 0 ;
  
  f_new.function = &counter ;
  f_new.params = p ;

  return f_new;
}

void my_error_handler (const char *reason, const char *file,
                       int line, int err);


int main (void)
{
  gsl_ieee_env_setup ();
  gsl_set_error_handler (&my_error_handler); 

  /* Test the basic Gauss-Kronrod rules with a smooth positive function. */

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result = 7.716049357767090777E-02;
    double exp_abserr = 2.990224871000550874E-06;
    double exp_resabs = 7.716049357767090777E-02;
    double exp_resasc = 4.434273814139995384E-02;

    double alpha = 2.6 ;
    gsl_function f = make_function(&f1, &alpha) ;

    gsl_integration_qk15 (&f, 0.0, 1.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk15(f1) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk15(f1) smooth abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk15(f1) smooth resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk15(f1) smooth resasc") ;

    gsl_integration_qk15 (&f, 1.0, 0.0, 
                                  &result, &abserr, &resabs, &resasc) ;

    gsl_test_rel(result,-exp_result,1e-15,"qk15(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk15(f1) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk15(f1) reverse resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk15(f1) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result = 7.716049379303084599E-02;
    double exp_abserr = 9.424302194248481445E-08;
    double exp_resabs = 7.716049379303084599E-02;
    double exp_resasc = 4.434311425038358484E-02;

    double alpha = 2.6 ;
    gsl_function f = make_function(&f1, &alpha);

    gsl_integration_qk21 (&f, 0.0, 1.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk21(f1) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk21(f1) smooth abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk21(f1) smooth resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk21(f1) smooth resasc") ;

    gsl_integration_qk21 (&f, 1.0, 0.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk21(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk21(f1) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk21(f1) reverse resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk21(f1) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result = 7.716049382494900855E-02;
    double exp_abserr = 1.713503193600029893E-09;
    double exp_resabs = 7.716049382494900855E-02;
    double exp_resasc = 4.427995051868838933E-02;

    double alpha = 2.6 ;
    gsl_function f = make_function(&f1, &alpha);

    gsl_integration_qk31 (&f, 0.0, 1.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk31(f1) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk31(f1) smooth abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk31(f1) smooth resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk31(f1) smooth resasc") ;

    gsl_integration_qk31 (&f, 1.0, 0.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk31(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk31(f1) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk31(f1) reverse resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk31(f1) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result = 7.716049382681375302E-02;
    double exp_abserr = 9.576386660975511224E-11;
    double exp_resabs = 7.716049382681375302E-02;
    double exp_resasc = 4.421521169637691873E-02;

    double alpha = 2.6 ;
    gsl_function f = make_function(&f1, &alpha);

    gsl_integration_qk41 (&f, 0.0, 1.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk41(f1) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk41(f1) smooth abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk41(f1) smooth resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk41(f1) smooth resasc") ;

    gsl_integration_qk41 (&f, 1.0, 0.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk41(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk41(f1) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk41(f1) reverse resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk41(f1) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result = 7.716049382708510540E-02;
    double exp_abserr = 1.002079980317363772E-11;
    double exp_resabs = 7.716049382708510540E-02;
    double exp_resasc = 4.416474291216854892E-02;

    double alpha = 2.6 ;
    gsl_function f = make_function(&f1, &alpha);

    gsl_integration_qk51 (&f, 0.0, 1.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk51(f1) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qk51(f1) smooth abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk51(f1) smooth resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk51(f1) smooth resasc") ;

    gsl_integration_qk51 (&f, 1.0, 0.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk51(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qk51(f1) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk51(f1) reverse resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk51(f1) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result = 7.716049382713800753E-02;
    double exp_abserr = 1.566060362296155616E-12;
    double exp_resabs = 7.716049382713800753E-02;
    double exp_resasc = 4.419287685934316506E-02;

    double alpha = 2.6 ;
    gsl_function f = make_function(&f1, &alpha);

    gsl_integration_qk61 (&f, 0.0, 1.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk61(f1) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qk61(f1) smooth abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk61(f1) smooth resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk61(f1) smooth resasc") ;

    gsl_integration_qk61 (&f, 1.0, 0.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk61(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qk61(f1) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk61(f1) reverse resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk61(f1) reverse resasc") ;
  }

  /* Now test the basic rules with a positive function that has a
     singularity. This should give large values of abserr which would
     find discrepancies in the abserr calculation. */

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result = 1.555688196612745777E+01;
    double exp_abserr = 2.350164577239293706E+01;
    double exp_resabs = 1.555688196612745777E+01;
    double exp_resasc = 2.350164577239293706E+01;

    double alpha = -0.9 ;
    gsl_function f = make_function(&f1, &alpha);

    gsl_integration_qk15 (&f, 0.0, 1.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk15(f1) singular result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk15(f1) singular abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk15(f1) singular resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk15(f1) singular resasc") ;

    gsl_integration_qk15 (&f, 1.0, 0.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk15(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk15(f1) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk15(f1) reverse resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk15(f1) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result = 1.799045317938126232E+01;
    double exp_abserr = 2.782360287710622515E+01;
    double exp_resabs = 1.799045317938126232E+01;
    double exp_resasc = 2.782360287710622515E+01;

    double alpha = -0.9 ;
    gsl_function f = make_function(&f1, &alpha);

    gsl_integration_qk21 (&f, 0.0, 1.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk21(f1) singular result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk21(f1) singular abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk21(f1) singular resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk21(f1) singular resasc") ;

    gsl_integration_qk21 (&f, 1.0, 0.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk21(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk21(f1) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk21(f1) reverse resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk21(f1) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result = 2.081873305159121657E+01;
    double exp_abserr = 3.296500137482590276E+01;
    double exp_resabs = 2.081873305159121301E+01;
    double exp_resasc = 3.296500137482590276E+01;

    double alpha = -0.9 ;
    gsl_function f = make_function(&f1, &alpha);

    gsl_integration_qk31 (&f, 0.0, 1.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk31(f1) singular result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk31(f1) singular abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk31(f1) singular resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk31(f1) singular resasc") ;

    gsl_integration_qk31 (&f, 1.0, 0.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk31(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk31(f1) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk31(f1) reverse resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk31(f1) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result = 2.288677623903126701E+01;
    double exp_abserr = 3.671538820274916048E+01;
    double exp_resabs = 2.288677623903126701E+01;
    double exp_resasc = 3.671538820274916048E+01;

    double alpha = -0.9 ;
    gsl_function f = make_function(&f1, &alpha);

    gsl_integration_qk41 (&f, 0.0, 1.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk41(f1) singular result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk41(f1) singular abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk41(f1) singular resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk41(f1) singular resasc") ;

    gsl_integration_qk41 (&f, 1.0, 0.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk41(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk41(f1) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk41(f1) reverse resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk41(f1) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result = 2.449953612016972215E+01;
    double exp_abserr = 3.967771249391228849E+01;
    double exp_resabs = 2.449953612016972215E+01;
    double exp_resasc = 3.967771249391228849E+01;

    double alpha = -0.9 ;
    gsl_function f = make_function(&f1, &alpha);

    gsl_integration_qk51 (&f, 0.0, 1.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk51(f1) singular result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk51(f1) singular abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk51(f1) singular resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk51(f1) singular resasc") ;

    gsl_integration_qk51 (&f, 1.0, 0.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk51(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk51(f1) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk51(f1) reverse resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk51(f1) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result = 2.583030240976628988E+01;
    double exp_abserr = 4.213750493076978643E+01;
    double exp_resabs = 2.583030240976628988E+01;
    double exp_resasc = 4.213750493076978643E+01;

    double alpha = -0.9 ;
    gsl_function f = make_function(&f1, &alpha);

    gsl_integration_qk61 (&f, 0.0, 1.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk61(f1) singular result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk61(f1) singular abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk61(f1) singular resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk61(f1) singular resasc") ;

    gsl_integration_qk61 (&f, 1.0, 0.0, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk61(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk61(f1) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk61(f1) reverse resabs") ;    
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk61(f1) reverse resasc") ;
  }

  /* Test the basic Gauss-Kronrod rules with a smooth oscillating
     function, over an unsymmetric range. This should find any
     discrepancies in the abscissae. */

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result =-7.238969575483799046E-01;
    double exp_abserr = 8.760080200939757174E-06;
    double exp_resabs = 1.165564172429140788E+00;
    double exp_resasc = 9.334560307787327371E-01;

    double alpha = 1.3 ;
    gsl_function f = make_function(&f3, &alpha);

    gsl_integration_qk15 (&f, 0.3, 2.71, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk15(f3) oscill result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk15(f3) oscill abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk15(f3) oscill resabs") ;
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk15(f3) oscill resasc") ;

    gsl_integration_qk15 (&f, 2.71, 0.3, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk15(f3) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk15(f3) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk15(f3) reverse resabs") ;
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk15(f3) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result =-7.238969575482959717E-01;
    double exp_abserr = 7.999213141433641888E-11;
    double exp_resabs = 1.150829032708484023E+00;
    double exp_resasc = 9.297591249133687619E-01;

    double alpha = 1.3 ;
    gsl_function f = make_function(&f3, &alpha);
    
    gsl_integration_qk21 (&f, 0.3, 2.71, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk21(f3) oscill result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qk21(f3) oscill abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk21(f3) oscill resabs") ;
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk21(f3) oscill resasc") ;

    gsl_integration_qk21 (&f, 2.71, 0.3,
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk21(f3) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qk21(f3) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk21(f3) reverse resabs") ;
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk21(f3) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result =-7.238969575482959717E-01;
    double exp_abserr = 1.285805464427459261E-14;
    double exp_resabs = 1.158150602093290571E+00;
    double exp_resasc = 9.277828092501518853E-01;

    double alpha = 1.3 ;
    gsl_function f = make_function(&f3, &alpha);

    gsl_integration_qk31 (&f, 0.3, 2.71, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk31(f3) oscill result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk31(f3) oscill abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk31(f3) oscill resabs") ;
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk31(f3) oscill resasc") ;

    gsl_integration_qk31 (&f, 2.71, 0.3, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk31(f3) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk31(f3) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk31(f3) reverse resabs") ;
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk31(f3) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result =-7.238969575482959717E-01;
    double exp_abserr = 1.286535726271015626E-14;
    double exp_resabs = 1.158808363486595328E+00;
    double exp_resasc = 9.264382258645686985E-01;

    double alpha = 1.3 ;
    gsl_function f = make_function(&f3, &alpha);

    gsl_integration_qk41 (&f, 0.3, 2.71, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk41(f3) oscill result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk41(f3) oscill abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk41(f3) oscill resabs") ;
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk41(f3) oscill resasc") ;

    gsl_integration_qk41 (&f, 2.71, 0.3,
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk41(f3) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk41(f3) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk41(f3) reverse resabs") ;
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk41(f3) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result =-7.238969575482961938E-01;
    double exp_abserr = 1.285290995039385778E-14;
    double exp_resabs = 1.157687209264406381E+00;
    double exp_resasc = 9.264666884071264263E-01;

    double alpha = 1.3 ;
    gsl_function f = make_function(&f3, &alpha);

    gsl_integration_qk51 (&f, 0.3, 2.71, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk51(f3) oscill result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk51(f3) oscill abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk51(f3) oscill resabs") ;
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk51(f3) oscill resasc") ;

    gsl_integration_qk51 (&f, 2.71, 0.3,
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk51(f3) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk51(f3) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk51(f3) reverse resabs") ;
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk51(f3) reverse resasc") ;
  }

  {
    double result = 0, abserr = 0, resabs = 0, resasc = 0 ;
    double exp_result =-7.238969575482959717E-01;
    double exp_abserr = 1.286438572027470736E-14;
    double exp_resabs = 1.158720854723590099E+00;
    double exp_resasc = 9.270469641771273972E-01;

    double alpha = 1.3 ;
    gsl_function f = make_function(&f3, &alpha);

    gsl_integration_qk61 (&f, 0.3, 2.71, 
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,exp_result,1e-15,"qk61(f3) oscill result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk61(f3) oscill abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk61(f3) oscill resabs") ;
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk61(f3) oscill resasc") ;

    gsl_integration_qk61 (&f, 2.71, 0.3,
                                  &result, &abserr, &resabs, &resasc) ;
    gsl_test_rel(result,-exp_result,1e-15,"qk61(f3) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qk61(f3) reverse abserr") ;
    gsl_test_rel(resabs,exp_resabs,1e-15,"qk61(f3) reverse resabs") ;
    gsl_test_rel(resasc,exp_resasc,1e-15,"qk61(f3) reverse resasc") ;
  }

  /* Test the non-adaptive gaussian integrator QNG */

  {
    int status = 0; size_t neval = 0 ;
    double result = 0, abserr = 0 ;
    double exp_result = 7.716049379303083211E-02;
    double exp_abserr = 9.424302199601294244E-08;
    int exp_neval  =  21;
    int exp_ier    =   0;

    double alpha = 2.6 ;
    gsl_function f = make_function(&f1, &alpha);
    
    status = gsl_integration_qng (&f, 0.0, 1.0, 1e-1, 0.0,
                                  &result, &abserr, &neval) ;
    gsl_test_rel(result,exp_result,1e-15,"qng(f1) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qng(f1) smooth abserr") ;
    gsl_test_int((int)neval,exp_neval,"qng(f1) smooth neval") ;  
    gsl_test_int(status,exp_ier,"qng(f1) smooth status") ;

    status = gsl_integration_qng (&f, 1.0, 0.0, 1e-1, 0.0,
                                  &result, &abserr, &neval) ;
    gsl_test_rel(result,-exp_result,1e-15,"qng(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qng(f1) reverse abserr") ;
    gsl_test_int((int)neval,exp_neval,"qng(f1) reverse neval") ;  
    gsl_test_int(status,exp_ier,"qng(f1) reverse status") ;
  }

  {
    int status = 0; size_t neval = 0 ;
    double result = 0, abserr = 0 ;

    double exp_result = 7.716049382706505200E-02;
    double exp_abserr = 2.666893044866214501E-12;
    int exp_neval  =  43;
    int exp_ier    =   0;

    double alpha = 2.6 ;
    gsl_function f = make_function(&f1, &alpha);

    status = gsl_integration_qng (&f, 0.0, 1.0, 0.0, 1e-9,
                                  &result, &abserr, &neval) ;
    gsl_test_rel(result,exp_result,1e-15,"qng(f1) smooth 43pt result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qng(f1) smooth 43pt abserr") ;
    gsl_test_int((int)neval,exp_neval,"qng(f1) smooth 43pt neval") ;  
    gsl_test_int(status,exp_ier,"qng(f1) smooth 43pt status") ;

    status = gsl_integration_qng (&f, 1.0, 0.0, 0.0, 1e-9,
                                  &result, &abserr, &neval) ;
    gsl_test_rel(result,-exp_result,1e-15,"qng(f1) reverse 43pt result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qng(f1) reverse 43pt abserr") ;
    gsl_test_int((int)neval,exp_neval,"qng(f1) reverse 43pt neval") ;  
    gsl_test_int(status,exp_ier,"qng(f1) reverse 43pt status") ;
  }

  {
    int status; size_t neval = 0 ;
    double result = 0, abserr = 0 ;
    double exp_result =-7.238969575482961938E-01;
    double exp_abserr = 1.277676889520056369E-14;
    int exp_neval  =  43;
    int exp_ier    =   0;

    double alpha = 1.3 ;
    gsl_function f = make_function(&f3, &alpha);

    status = gsl_integration_qng (&f, 0.3, 2.71, 0.0, 1e-12,
                                  &result, &abserr, &neval) ;
    gsl_test_rel(result,exp_result,1e-15,"qnq(f3) oscill result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qng(f3) oscill abserr") ;
    gsl_test_int((int)neval,exp_neval,"qng(f3) oscill neval") ;
    gsl_test_int(status,exp_ier,"qng(f3) oscill status") ;

    status = gsl_integration_qng (&f, 2.71, 0.3, 0.0, 1e-12,
                                  &result, &abserr, &neval) ;
    gsl_test_rel(result,-exp_result,1e-15,"qnq(f3) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qng(f3) reverse abserr") ;
    gsl_test_int((int)neval,exp_neval,"qng(f3) reverse neval") ;
    gsl_test_int(status,exp_ier,"qng(f3) reverse status") ;
  }

  {
    int status = 0; size_t neval = 0 ;
    double result = 0, abserr = 0 ;

    double exp_result = 7.716049382716029525E-02;
    double exp_abserr = 8.566535680046930668E-16;
    int exp_neval  =  87;
    int exp_ier    =   0;

    double alpha = 2.6 ;
    gsl_function f = make_function(&f1, &alpha);

    status = gsl_integration_qng (&f, 0.0, 1.0, 0.0, 1e-13,
                                  &result, &abserr, &neval) ;
    gsl_test_rel(result,exp_result,1e-15,"qng(f1) 87pt smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qng(f1) 87pt smooth abserr") ;
    gsl_test_int((int)neval,exp_neval,"qng(f1) 87pt smooth neval") ;  
    gsl_test_int(status,exp_ier,"qng(f1) 87pt smooth status") ;

    status = gsl_integration_qng (&f, 1.0, 0.0, 0.0, 1e-13,
                                  &result, &abserr, &neval) ;
    gsl_test_rel(result,-exp_result,1e-15,"qng(f1) 87pt reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-7,"qng(f1) 87pt reverse abserr") ;
    gsl_test_int((int)neval,exp_neval,"qng(f1) 87pt reverse neval") ;  
    gsl_test_int(status,exp_ier,"qng(f1) 87pt reverse status") ;
  }

  {
    int status = 0; size_t neval = 0 ;
    double result = 0, abserr = 0 ;

    double exp_result = 3.222948711817264211E+01;
    double exp_abserr = 2.782360287710622870E+01;
    int exp_neval  =  87;
    int exp_ier    =  GSL_ETOL;

    double alpha = -0.9 ;
    gsl_function f = make_function(&f1, &alpha);

    status = gsl_integration_qng (&f, 0.0, 1.0, 0.0, 1e-3,
                                  &result, &abserr, &neval) ;
    gsl_test_rel(result,exp_result,1e-15,"qng(f1) sing beyond 87pt result");
    gsl_test_rel(abserr,exp_abserr,1e-7,"qng(f1) sing beyond 87pt abserr");
    gsl_test_int((int)neval,exp_neval,"qng(f1) sing beyond 87pt neval") ;  
    gsl_test_int(status,exp_ier,"qng(f1) sing beyond 87pt status") ;

    status = gsl_integration_qng (&f, 1.0, 0.0, 0.0, 1e-3,
                                  &result, &abserr, &neval) ;
    gsl_test_rel(result,-exp_result,1e-15,"qng(f1) reverse beyond 87pt result");
    gsl_test_rel(abserr,exp_abserr,1e-7,"qng(f1) rev beyond 87pt abserr");
    gsl_test_int((int)neval,exp_neval,"qng(f1) rev beyond 87pt neval") ;  
    gsl_test_int(status,exp_ier,"qng(f1) rev beyond 87pt status") ;
  }

  /* Test the adaptive integrator QAG */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    double exp_result = 7.716049382715854665E-02 ;
    double exp_abserr = 6.679384885865053037E-12 ;
    int exp_neval  =     165;
    int exp_ier    =       0;
    int exp_last   =       6;

    double a[6] = { 0, 0.5, 0.25, 0.125, 0.0625, 0.03125 } ;
    double b[6] = { 0.03125, 1, 0.5, 0.25, 0.125, 0.0625 } ;
    double r[6] = { 3.966769831709074375E-06, 5.491842501998222409E-02,
                    1.909827770934243926E-02, 2.776531175604360531E-03,
                    3.280661030752063693E-04, 3.522704932261797744E-05 } ;
    double e[6] = { 6.678528276336181873E-12, 6.097169993333454062E-16,
                    2.120334764359736934E-16, 3.082568839745514608E-17,
                    3.642265412331439511E-18, 3.910988124757650942E-19 } ;
    int order[6] = { 1, 2, 3, 4, 5, 6 } ;

    double alpha = 2.6 ;
    gsl_function f = make_function(&f1, &alpha) ;

    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qag (&fc, 0.0, 1.0, 0.0, 1e-10, w->limit,
                                  GSL_INTEG_GAUSS15, w,
                                  &result, &abserr) ;

    gsl_test_rel(result,exp_result,1e-15,"qag(f1) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qag(f1) smooth abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qag(f1) smooth neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qag(f1) smooth last") ;  
    gsl_test_int(status,exp_ier,"qag(f1) smooth status") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qag(f1) smooth alist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qag(f1) smooth blist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-15,"qag(f1) smooth rlist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-6,"qag(f1) smooth elist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qag(f1) smooth order") ;

    p.neval = 0;

    status = gsl_integration_qag (&fc, 1.0, 0.0, 0.0, 1e-10, w->limit,
                                  GSL_INTEG_GAUSS15, w,
                                  &result, &abserr) ;

    gsl_test_rel(result,-exp_result,1e-15,"qag(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qag(f1) reverse abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qag(f1) reverse neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qag(f1) reverse last") ;  
    gsl_test_int(status,exp_ier,"qag(f1) reverse status") ;

    gsl_integration_workspace_free (w) ;

  }

  /* Test the same function using an absolute error bound and the
     21-point rule */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    double exp_result = 7.716049382716050342E-02 ;
    double exp_abserr = 2.227969521869139532E-15 ;
    int exp_neval  =     315;
    int exp_ier    =       0;
    int exp_last   =       8;

    double a[8] = { 0, 0.5, 0.25, 0.125, 0.0625, 0.03125, 0.015625,
                    0.0078125 } ;
    double b[8] = { 0.0078125, 1, 0.5, 0.25, 0.125, 0.0625, 0.03125,
                    0.015625 } ;
    double r[8] = { 3.696942726831556522E-08, 5.491842501998223103E-02,
                    1.909827770934243579E-02, 2.776531175604360097E-03,
                    3.280661030752062609E-04, 3.522704932261797744E-05,
                    3.579060884684503576E-06, 3.507395216921808047E-07 } ;
    double e[8] = { 1.371316364034059572E-15, 6.097169993333454062E-16,
                    2.120334764359736441E-16, 3.082568839745514608E-17,
                    3.642265412331439511E-18, 3.910988124757650460E-19,
                    3.973555800712018091E-20, 3.893990926286736620E-21 } ;
    int order[8] = { 1, 2, 3, 4, 5, 6, 7, 8 } ;

    double alpha = 2.6 ;
    gsl_function f = make_function(&f1, &alpha);

    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qag (&fc, 0.0, 1.0, 1e-14, 0.0, w->limit,
                                  GSL_INTEG_GAUSS21, w,
                                  &result, &abserr) ;

    gsl_test_rel(result,exp_result,1e-15,"qag(f1,21pt) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qag(f1,21pt) smooth abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qag(f1,21pt) smooth neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qag(f1,21pt) smooth last") ;  
    gsl_test_int(status,exp_ier,"qag(f1,21pt) smooth status") ;

    for (i = 0; i < 8 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qag(f1,21pt) smooth alist") ;

    for (i = 0; i < 8 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qag(f1,21pt) smooth blist") ;

    for (i = 0; i < 8 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-15,"qag(f1,21pt) smooth rlist") ;

    for (i = 0; i < 8 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-6,"qag(f1,21pt) smooth elist") ;

    for (i = 0; i < 8 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qag(f1,21pt) smooth order");


    p.neval = 0;
    status = gsl_integration_qag (&fc, 1.0, 0.0, 1e-14, 0.0, w->limit,
                                  GSL_INTEG_GAUSS21, w,
                                  &result, &abserr) ;

    gsl_test_rel(result,-exp_result,1e-15,"qag(f1,21pt) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qag(f1,21pt) reverse abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qag(f1,21pt) reverse neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qag(f1,21pt) reverse last") ;  
    gsl_test_int(status,exp_ier,"qag(f1,21pt) reverse status") ;

    gsl_integration_workspace_free (w) ;

  }

  /* Adaptive integration of an oscillatory function which terminates because
     of roundoff error, uses the 31-pt rule */

  {
    int status = 0; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    double exp_result = -7.238969575482959717E-01;
    double exp_abserr =  1.285805464427459261E-14;
    int exp_neval   =     31;
    int exp_ier     =     GSL_EROUND;
    int exp_last    =     1;

    double alpha = 1.3 ;
    gsl_function f = make_function(&f3, &alpha);

    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qag (&fc, 0.3, 2.71, 1e-14, 0.0, w->limit, 
                                  GSL_INTEG_GAUSS31, w, 
                                  &result, &abserr) ;

    gsl_test_rel(result,exp_result,1e-15,"qag(f3,31pt) oscill result");
    gsl_test_rel(abserr,exp_abserr,1e-6,"qag(f3,31pt) oscill abserr");
    gsl_test_int((int)(p.neval),exp_neval,"qag(f3,31pt) oscill neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qag(f3,31pt) oscill last") ;  
    gsl_test_int(status,exp_ier,"qag(f3,31pt) oscill status") ;

    p.neval = 0;
    status = gsl_integration_qag (&fc, 2.71, 0.3, 1e-14, 0.0, w->limit, 
                                  GSL_INTEG_GAUSS31, w, 
                                  &result, &abserr) ;

    gsl_test_rel(result,-exp_result,1e-15,"qag(f3,31pt) reverse result");
    gsl_test_rel(abserr,exp_abserr,1e-6,"qag(f3,31pt) reverse abserr");
    gsl_test_int((int)(p.neval),exp_neval,"qag(f3,31pt) reverse neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qag(f3,31pt) reverse last") ;  
    gsl_test_int(status,exp_ier,"qag(f3,31pt) reverse status") ;

    gsl_integration_workspace_free (w) ;

  }

  /* Check the singularity detection (singularity at x=-0.1 in this example) */

  {
    int status = 0; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    int exp_neval  =     5151;
    int exp_ier    =     GSL_ESING;
    int exp_last   =     51;

    double alpha = 2.0 ;
    gsl_function f = make_function(&f16, &alpha);

    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qag (&fc, -1.0, 1.0, 1e-14, 0.0, w->limit,
                                  GSL_INTEG_GAUSS51, w, 
                                  &result, &abserr) ;

    gsl_test_int((int)(p.neval),exp_neval,"qag(f16,51pt) sing neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qag(f16,51pt) sing last") ;  
    gsl_test_int(status,exp_ier,"qag(f16,51pt) sing status") ;

    p.neval = 0;
    status = gsl_integration_qag (&fc, 1.0, -1.0, 1e-14, 0.0, w->limit,
                                  GSL_INTEG_GAUSS51, w, 
                                  &result, &abserr) ;

    gsl_test_int((int)(p.neval),exp_neval,"qag(f16,51pt) rev neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qag(f16,51pt) rev last") ;  
    gsl_test_int(status,exp_ier,"qag(f16,51pt) rev status") ;

    gsl_integration_workspace_free (w) ;

  }

  /* Check for hitting the iteration limit */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (3) ;

    double exp_result =  9.565151449233894709 ;
    double exp_abserr =  1.570369823891028460E+01;
    int exp_neval  =     305;
    int exp_ier    =     GSL_EMAXITER;
    int exp_last   =     3;

    double a[3] = { -5.000000000000000000E-01,
                    0.000000000000000000,
                    -1.000000000000000000 } ;
    
    double b[3] = { 0.000000000000000000,
                    1.000000000000000000,
                    -5.000000000000000000E-01 } ;
    
    double r[3] = { 9.460353469435913709,
                    9.090909090909091161E-02,
                    1.388888888888888812E-02 } ;
    
    double e[3] = { 1.570369823891028460E+01,
                    1.009293658750142399E-15,
                    1.541976423090495140E-16 } ;
    
    int order[3] = { 1, 2, 3 } ;

    double alpha = 1.0 ;
    gsl_function f = make_function(&f16, &alpha);
    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qag (&fc, -1.0, 1.0, 1e-14, 0.0, w->limit, 
                                  GSL_INTEG_GAUSS61, w, 
                                  &result, &abserr) ;

    gsl_test_rel(result,exp_result,1e-15,"qag(f16,61pt) limit result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qag(f16,61pt) limit abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qag(f16,61pt) limit neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qag(f16,61pt) limit last") ;  
    gsl_test_int(status,exp_ier,"qag(f16,61pt) limit status") ;

    for (i = 0; i < 3 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qag(f16,61pt) limit alist") ;

    for (i = 0; i < 3 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qag(f16,61pt) limit blist") ;

    for (i = 0; i < 3 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-15,"qag(f16,61pt) limit rlist") ;

    for (i = 0; i < 3 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-6,"qag(f16,61pt) limit elist") ;

    for (i = 0; i < 3 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qag(f16,61pt) limit order");

    p.neval = 0;
    status = gsl_integration_qag (&fc, 1.0, -1.0, 1e-14, 0.0, w->limit, 
                                  GSL_INTEG_GAUSS61, w, 
                                  &result, &abserr) ;

    gsl_test_rel(result,-exp_result,1e-15,"qag(f16,61pt) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qag(f16,61pt) reverse abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qag(f16,61pt) reverse neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qag(f16,61pt) reverse last") ;  
    gsl_test_int(status,exp_ier,"qag(f16,61pt) reverse status") ;

    gsl_integration_workspace_free (w) ;

  }

  /* Test the adaptive integrator with extrapolation QAGS */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    double exp_result = 7.716049382715789440E-02 ;
    double exp_abserr = 2.216394961010438404E-12 ;
    int exp_neval  =     189;
    int exp_ier    =       0;
    int exp_last   =       5;

    double a[5] = { 0, 0.5, 0.25, 0.125, 0.0625 } ;
    double b[5] = { 0.0625, 1, 0.5, 0.25, 0.125 } ;
    double r[5] = { 3.919381915366914693E-05,
                    5.491842501998223103E-02,
                    1.909827770934243579E-02,
                    2.776531175604360097E-03,
                    3.280661030752062609E-04 } ;
    double e[5] = { 2.215538742580964735E-12,
                    6.097169993333454062E-16,
                    2.120334764359736441E-16,
                    3.082568839745514608E-17,
                    3.642265412331439511E-18 } ;
    int order[5] = { 1, 2, 3, 4, 5 } ;

    double alpha = 2.6 ;
    gsl_function f = make_function(&f1, &alpha);
    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qags (&fc, 0.0, 1.0, 0.0, 1e-10, w->limit,
                                   w, 
                                   &result, &abserr) ;

    gsl_test_rel(result,exp_result,1e-15,"qags(f1) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qags(f1) smooth abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qags(f1) smooth neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qags(f1) smooth last") ;  
    gsl_test_int(status,exp_ier,"qags(f1) smooth status") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qags(f1) smooth alist") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qags(f1) smooth blist") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-15,"qags(f1) smooth rlist") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-6,"qags(f1) smooth elist") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qags(f1) smooth order") ;

    p.neval = 0;
    status = gsl_integration_qags (&fc, 1.0, 0.0, 0.0, 1e-10, w->limit,
                                   w, 
                                   &result, &abserr) ;

    gsl_test_rel(result,-exp_result,1e-15,"qags(f1) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qags(f1) reverse abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qags(f1) reverse neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qags(f1) reverse last") ;  
    gsl_test_int(status,exp_ier,"qags(f1) reverse status") ;

    gsl_integration_workspace_free (w) ;

  }

  /* Test f11 using an absolute error bound */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    /* All results are for GSL_IEEE_MODE=double-precision */

    double exp_result = -5.908755278982136588E+03 ;
    double exp_abserr = 1.299646281053874554E-10 ;
    int exp_neval  =     357;
    int exp_ier    =       0;
    int exp_last   =       9;

    double a[9] = { 1.000000000000000000E+00,
                    5.005000000000000000E+02,
                    2.507500000000000000E+02,
                    1.258750000000000000E+02,
                    6.343750000000000000E+01,
                    3.221875000000000000E+01,
                    1.660937500000000000E+01,
                    8.804687500000000000E+00,
                    4.902343750000000000E+00 } ;
    double b[9] = { 4.902343750000000000E+00,
                    1.000000000000000000E+03,
                    5.005000000000000000E+02,
                    2.507500000000000000E+02,
                    1.258750000000000000E+02,
                    6.343750000000000000E+01,
                    3.221875000000000000E+01,
                    1.660937500000000000E+01,
                    8.804687500000000000E+00 } ;
    double r[9] = { -3.890977835520834649E+00,
                    -3.297343675805121620E+03,
                    -1.475904154146372775E+03,
                    -6.517404019686431411E+02,
                    -2.829354222635842007E+02,
                    -1.201692001973227519E+02,
                    -4.959999906099650246E+01,
                    -1.971441499411640308E+01,
                    -7.457032710459004399E+00 } ;
    double e[9] = { 6.448276035006137169E-11,
                    3.660786868980994028E-11,
                    1.638582774073219226E-11,
                    7.235772003440423011E-12,
                    3.141214202790722909E-12,
                    1.334146129098576244E-12,
                    5.506706097890446534E-13,
                    2.188739744348345039E-13,
                    8.278969410534525339E-14 } ;
    int order[9] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 } ;

    double alpha = 2.0 ;
    gsl_function f = make_function(&f11, &alpha);
    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qags (&fc, 1.0, 1000.0, 1e-7, 0.0, w->limit,
                                   w, 
                                   &result, &abserr) ;
    
    gsl_test_rel(result,exp_result,1e-15,"qags(f11) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-3,"qags(f11) smooth abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qags(f11) smooth neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qags(f11) smooth last") ;  
    gsl_test_int(status,exp_ier,"qags(f11) smooth status") ;

    for (i = 0; i < 9 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qags(f11) smooth alist") ;

    for (i = 0; i < 9 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qags(f11) smooth blist") ;

    for (i = 0; i < 9 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-15,"qags(f11) smooth rlist") ;

    for (i = 0; i < 9 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-5,"qags(f11) smooth elist") ;

    for (i = 0; i < 9 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qags(f11) smooth order");

    p.neval = 0;
    status = gsl_integration_qags (&fc, 1000.0, 1.0, 1e-7, 0.0, w->limit,
                                   w, 
                                   &result, &abserr) ;
    
    gsl_test_rel(result,-exp_result,1e-15,"qags(f11) reverse result") ;
    gsl_test_rel(abserr,exp_abserr,1e-3,"qags(f11) reverse abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qags(f11) reverse neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qags(f11) reverse last") ;  
    gsl_test_int(status,exp_ier,"qags(f11) reverse status") ;

    gsl_integration_workspace_free (w) ;

  }

  /* Test infinite range integral f455 using a relative error bound */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    /* All results are for GSL_IEEE_MODE=double-precision */

    double exp_result = -3.616892186127022568E-01 ;
    double exp_abserr = 3.016716913328831851E-06;
    int exp_neval  =      285;
    int exp_ier    =        0;
    int exp_last   =       10;

    double a[10] = { 9.687500000000000000E-01,
                     0.000000000000000000E+00,
                     5.000000000000000000E-01,
                     2.500000000000000000E-01,
                     7.500000000000000000E-01,
                     1.250000000000000000E-01,
                     8.750000000000000000E-01,
                     6.250000000000000000E-02,
                     9.375000000000000000E-01,
                     3.125000000000000000E-02 } ;
    double b[10] = { 1.000000000000000000E+00,
                     3.125000000000000000E-02,
                     7.500000000000000000E-01,
                     5.000000000000000000E-01,
                     8.750000000000000000E-01,
                     2.500000000000000000E-01,
                     9.375000000000000000E-01,
                     1.250000000000000000E-01,
                     9.687500000000000000E-01,
                     6.250000000000000000E-02 } ;
    double r[10] = { -1.390003415539725340E-01,
                     1.429785306003466313E-03,
                     -1.229943369113085765E-02,
                     2.995321156568048898E-03,
                     -4.980050133751051655E-02,
                     2.785385934678596704E-03,
                     -8.653752279614615461E-02,
                     1.736218164975512294E-03,
                     -8.398745675010892142E-02,
                     1.041689192004495576E-03 } ;
    double e[10] = { 2.395037249893453013E-02,
                     2.161214992172538524E-04,
                     5.720644840858777846E-14,
                     3.325474514168701167E-17,
                     3.147380432198176412E-14,
                     3.092399597147240624E-17,
                     9.607595030230581153E-16,
                     1.927589382528252344E-17,
                     9.324480826368044019E-16,
                     1.156507325466566521E-17 } ;
    int order[10] = { 1, 2, 3, 5, 7, 9, 4, 6, 8, 10 } ;

    gsl_function f = make_function(&f455, 0);
    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qagiu (&fc, 0.0, 0.0, 1.0e-3, w->limit,
                                    w, 
                                    &result, &abserr) ;
    
    gsl_test_rel(result,exp_result,1e-14,"qagiu(f455) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qagiu(f455) smooth abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qagiu(f455) smooth neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qagiu(f455) smooth last") ;  
    gsl_test_int(status,exp_ier,"qagiu(f455) smooth status") ;

    for (i = 0; i < 10 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qagiu(f455) smooth alist") ;

    for (i = 0; i < 10 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qagiu(f455) smooth blist") ;

    for (i = 0; i < 10 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-15,"qagiu(f455) smooth rlist") ;

    for (i = 0; i < 10 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-4,"qagiu(f455) smooth elist") ;

    for (i = 0; i < 10 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qagiu(f455) smooth order");

    gsl_integration_workspace_free (w) ;

  }

  /* Test infinite range integral f15 using a relative error bound */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    /* All results are for GSL_IEEE_MODE=double-precision */

    double exp_result = 6.553600000000024738E+04;
    double exp_abserr = 7.121667111456009280E-04;
    int exp_neval  =      285;
    int exp_ier    =        0;
    int exp_last   =       10;

    double a[10] = { 0.000000000000000000E+00,
                     5.000000000000000000E-01,
                     2.500000000000000000E-01,
                     1.250000000000000000E-01,
                     6.250000000000000000E-02,
                     3.125000000000000000E-02,
                     1.562500000000000000E-02,
                     7.812500000000000000E-03,
                     3.906250000000000000E-03,
                     1.953125000000000000E-03 } ;
    double b[10] = { 1.953125000000000000E-03,
                     1.000000000000000000E+00,
                     5.000000000000000000E-01,
                     2.500000000000000000E-01,
                     1.250000000000000000E-01,
                     6.250000000000000000E-02,
                     3.125000000000000000E-02,
                     1.562500000000000000E-02,
                     7.812500000000000000E-03,
                     3.906250000000000000E-03 } ;
    double r[10] = { 1.099297665754340292E+00,
                     3.256176475185617591E-01,
                     8.064694554185326325E+00,
                     8.873128656118993263E+01,
                     6.977679035845269482E+02,
                     4.096981198511257389E+03,
                     1.574317583220441520E+04,
                     2.899418134793237914E+04,
                     1.498314766425578091E+04,
                     9.225251570832365360E+02 } ;
    double e[10] = { 7.101865971621337814E-04,
                     1.912660677170175771E-08,
                     9.167763417119923333E-08,
                     3.769501719163865578E-07,
                     6.973493131275552509E-07,
                     1.205653952340679711E-07,
                     1.380003928453846583E-07,
                     1.934652413547325474E-07,
                     3.408933028357320364E-07,
                     2.132473175465897029E-09 } ;
    int order[10] = { 1, 5, 4, 9, 8, 7, 6, 3, 2, 10 } ;

    double alpha = 5.0;

    gsl_function f = make_function(&f15, &alpha);
    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qagiu (&fc, 0.0, 0.0, 1.0e-7, w->limit,
                                    w, 
                                    &result, &abserr) ;
    
    gsl_test_rel(result,exp_result,1e-14,"qagiu(f15) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qagiu(f15) smooth abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qagiu(f15) smooth neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qagiu(f15) smooth last") ;  
    gsl_test_int(status,exp_ier,"qagiu(f15) smooth status") ;

    for (i = 0; i < 10 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qagiu(f15) smooth alist") ;

    for (i = 0; i < 10 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qagiu(f15) smooth blist") ;

    for (i = 0; i < 10 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-15,"qagiu(f15) smooth rlist") ;

    for (i = 0; i < 10 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-4,"qagiu(f15) smooth elist") ;

    for (i = 0; i < 10 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qagiu(f15) smooth order");

    gsl_integration_workspace_free (w) ;

  }

  /* Test infinite range integral f16 using an absolute error bound */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    /* All results are for GSL_IEEE_MODE=double-precision */

    double exp_result = 1.000000000006713292E-04;
    double exp_abserr = 3.084062020905636316E-09;
    int exp_neval  =      165;
    int exp_ier    =        0;
    int exp_last   =        6;

    double a[6] = { 0.000000000000000000E+00,
                    5.000000000000000000E-01,
                    2.500000000000000000E-01,
                    1.250000000000000000E-01,
                    6.250000000000000000E-02,
                    3.125000000000000000E-02 } ;
    double b[6] = { 3.125000000000000000E-02,
                    1.000000000000000000E+00,
                    5.000000000000000000E-01,
                    2.500000000000000000E-01,
                    1.250000000000000000E-01,
                    6.250000000000000000E-02 } ;
    double r[6] = { 7.633587786326674618E-05,
                    9.900990099009899620E-07,
                    1.922522349322310737E-06,
                    3.629434715543053753E-06,
                    6.501422186103209199E-06,
                    1.062064387653501389E-05 } ;
    double e[6] = { 3.084061858351569051E-09,
                    3.112064814755089674E-17,
                    4.543453652226561245E-17,
                    4.908618166361344548E-17,
                    3.014338672269481784E-17,
                    6.795996738013555461E-18 } ;
    int order[6] = { 1, 4, 3, 2, 5, 6 } ;

    double alpha = 1.0;

    gsl_function f = make_function(&f16, &alpha);
    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qagiu (&fc, 99.9, 1.0e-7, 0.0, w->limit,
                                    w, 
                                    &result, &abserr) ;
    
    gsl_test_rel(result,exp_result,1e-14,"qagiu(f16) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qagiu(f16) smooth abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qagiu(f16) smooth neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qagiu(f16) smooth last") ;  
    gsl_test_int(status,exp_ier,"qagiu(f16) smooth status") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qagiu(f16) smooth alist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qagiu(f16) smooth blist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-15,"qagiu(f16) smooth rlist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-4,"qagiu(f16) smooth elist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qagiu(f16) smooth order");

    gsl_integration_workspace_free (w) ;

  }

  /* Test infinite range integral myfn1 using an absolute error bound */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    /* All results are for GSL_IEEE_MODE=double-precision */

    double exp_result = 2.275875794468747770E+00;
    double exp_abserr = 7.436490118267390744E-09;
    int exp_neval  =      270;
    int exp_ier    =        0;
    int exp_last   =        5;

    double a[5] = { 1.250000000000000000E-01,
                    5.000000000000000000E-01,
                    2.500000000000000000E-01,
                    0.000000000000000000E+00,
                    3.750000000000000000E-01 } ;
    double b[5] = { 2.500000000000000000E-01,
                    1.000000000000000000E+00,
                    3.750000000000000000E-01,
                    1.250000000000000000E-01,
                    5.000000000000000000E-01 } ;
    double r[5] = { 4.639317228058405717E-04,
                    1.691664195356748834E+00,
                    1.146307471900291086E-01,
                    4.379392477350953574E-20,
                    4.691169201991640669E-01 } ;
    double e[5] = { 3.169263960393051137E-09,
                    4.265988974874425043E-09,
                    1.231954072964969637E-12,
                    8.360902986775307673E-20,
                    5.208244060463541433E-15 } ;
    int order[5] = { 2, 1, 3, 5, 4 } ;

    gsl_function f = make_function(&myfn1, 0);
    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qagi (&fc, 1.0e-7, 0.0, w->limit,
                                   w, 
                                   &result, &abserr) ;
    
    gsl_test_rel(result,exp_result,1e-14,"qagiu(myfn1) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qagiu(myfn1) smooth abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qagiu(myfn1) smooth neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qagiu(myfn1) smooth last") ;  
    gsl_test_int(status,exp_ier,"qagiu(myfn1) smooth status") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qagiu(myfn1) smooth alist") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qagiu(myfn1) smooth blist") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-14,"qagiu(myfn1) smooth rlist") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-4,"qagiu(myfn1) smooth elist") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qagiu(myfn1) smooth order");

    gsl_integration_workspace_free (w) ;

  }

  /* Test infinite range integral myfn1 using an absolute error bound */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    /* All results are for GSL_IEEE_MODE=double-precision */

    double exp_result = 2.718281828459044647E+00;
    double exp_abserr = 1.588185109253204805E-10;
    int exp_neval  =      135;
    int exp_ier    =        0;
    int exp_last   =        5;

    double a[5] = { 0.000000000000000000E+00,
                    5.000000000000000000E-01,
                    2.500000000000000000E-01,
                    1.250000000000000000E-01,
                    6.250000000000000000E-02 } ;
    double b[5] = { 6.250000000000000000E-02,
                    1.000000000000000000E+00,
                    5.000000000000000000E-01,
                    2.500000000000000000E-01,
                    1.250000000000000000E-01 } ;
    double r[5] = { 8.315287189746029816E-07,
                    1.718281828459045091E+00,
                    8.646647167633871867E-01,
                    1.328565310599463256E-01,
                    2.477920647947255521E-03 } ;
    double e[5] = { 1.533437090413525935E-10,
                    4.117868247943567505E-12,
                    7.802455785301941044E-13,
                    5.395586026138397182E-13,
                    3.713312434866150125E-14 } ;
    int order[5] = { 1, 2, 3, 4, 5 } ;

    double alpha = 1.0 ;
    gsl_function f = make_function(&myfn2, &alpha);
    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qagil (&fc, 1.0, 1.0e-7, 0.0, w->limit,
                                    w, 
                                    &result, &abserr) ;
    
    gsl_test_rel(result,exp_result,1e-14,"qagiu(myfn2) smooth result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qagiu(myfn2) smooth abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qagiu(myfn2) smooth neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qagiu(myfn2) smooth last") ;  
    gsl_test_int(status,exp_ier,"qagiu(myfn2) smooth status") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qagiu(myfn2) smooth alist") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qagiu(myfn2) smooth blist") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-14,"qagiu(myfn2) smooth rlist") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-4,"qagiu(myfn2) smooth elist") ;

    for (i = 0; i < 5 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qagiu(myfn2) smooth order");

    gsl_integration_workspace_free (w) ;

  }

  /* Test integral f454 with integrable singular points */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    /* All results are for GSL_IEEE_MODE=double-precision */

    double exp_result = 5.274080611672716401E+01;
    double exp_abserr = 1.755703848687062418E-04;
    int exp_neval  =        777;
    int exp_ier    =          0;
    int exp_last   =         20;

    double a[20] = { 9.687500000000000000E-01,
                    1.401269388548935790E+00,
                    1.414213562373095145E+00,
                    1.000000000000000000E+00,
                    0.000000000000000000E+00,
                    2.207106781186547462E+00,
                    1.810660171779821415E+00,
                    1.207106781186547462E+00,
                    5.000000000000000000E-01,
                    1.103553390593273731E+00,
                    1.612436867076458391E+00,
                    1.310660171779821415E+00,
                    7.500000000000000000E-01,
                    1.051776695296636976E+00,
                    1.513325214724776657E+00,
                    1.362436867076458391E+00,
                    8.750000000000000000E-01,
                    1.463769388548935790E+00,
                    1.388325214724776657E+00,
                    9.375000000000000000E-01} ;
    double b[20] = { 1.000000000000000000E+00,
                     1.414213562373095145E+00,
                     1.463769388548935790E+00,
                     1.051776695296636976E+00,
                     5.000000000000000000E-01,
                     3.000000000000000000E+00,
                     2.207106781186547462E+00,
                     1.310660171779821415E+00,
                     7.500000000000000000E-01,
                     1.207106781186547462E+00,
                     1.810660171779821415E+00,
                     1.362436867076458391E+00,
                     8.750000000000000000E-01,
                     1.103553390593273731E+00,
                     1.612436867076458391E+00,
                     1.388325214724776657E+00,
                     9.375000000000000000E-01,
                     1.513325214724776657E+00,
                     1.401269388548935790E+00,
                     9.687500000000000000E-01} ;
    double r[20] = { -1.125078814079027711E-01,
                     -1.565132123531515207E-01,
                     -4.225328513207429193E-01,
                     -1.830392049835374568E-01,
                     6.575875041899758092E-03,
                     4.873920540843067783E+01,
                     6.032891565603589079E+00,
                     -2.991531901645863023E-01,
                     -7.326282608704996063E-03,
                     -2.431894410706912923E-01,
                     5.911661670635662835E-01,
                     -2.236786562536174916E-01,
                     -5.647871991778510847E-02,
                     -1.305470403178642658E-01,
                     -1.721363984401322045E-01,
                     -1.589345454585119055E-01,
                     -7.406626263352669715E-02,
                     -2.208730668000830344E-01,
                     -1.048692749517999567E-01,
                     -6.302287584527696551E-02} ;
    double e[20] = { 2.506431410088378817E-02,
                     2.730454695485963826E-02,
                     1.017446081816190118E-01,
                     3.252808038935910834E-02,
                     7.300687878575027348E-17,
                     5.411138804637469780E-13,
                     6.697855121200013106E-14,
                     3.321267596107916554E-15,
                     1.417509685426979386E-16,
                     2.699945168224041491E-15,
                     6.573952690524728748E-15,
                     2.483331942899818875E-15,
                     6.270397525408045936E-16,
                     1.449363299575615261E-15,
                     1.911097929242846383E-15,
                     1.764527917763735212E-15,
                     8.223007012367522077E-16,
                     2.452183642810224359E-15,
                     1.164282836272345215E-15,
                     6.996944784151910810E-16} ;
    int order[20] = { 3, 4, 2, 1, 6, 7, 11, 8, 10, 12, 18,
                     15, 16, 14, 19, 17, 20, 13, 9, 5 } ;

    gsl_function f = make_function(&f454, 0);
    gsl_function fc = make_counter(&f, &p) ;

    double pts[4] ;

    pts[0] = 0.0;
    pts[1] = 1.0;
    pts[2] = sqrt(2.0);
    pts[3] = 3.0;

    status = gsl_integration_qagp (&fc, pts, 4,
                                   0.0, 1.0e-3, w->limit,
                                   w, 
                                   &result, &abserr) ;
    
    gsl_test_rel(result,exp_result,1e-14,"qagp(f454) singular result") ;
    gsl_test_rel(abserr,exp_abserr,1e-5,"qagp(f454) singular abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qagp(f454) singular neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qagp(f454) singular last") ;  
    gsl_test_int(status,exp_ier,"qagp(f454) singular status") ;

    for (i = 0; i < 20 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qagp(f454) singular alist") ;

    for (i = 0; i < 20 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qagp(f454) singular blist") ;

    for (i = 0; i < 20 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-14,"qagp(f454) singular rlist") ;

    for (i = 0; i < 20 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-4,"qagp(f454) singular elist") ;

    for (i = 0; i < 20 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qagp(f454) singular order");

    gsl_integration_workspace_free (w) ;

  }


  /* Test cauchy integration using a relative error bound */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    /* All results are for GSL_IEEE_MODE=double-precision */

    double exp_result = -8.994400695837000137E-02;
    double exp_abserr =  1.185290176227023727E-06;
    int exp_neval  =      215;
    int exp_ier    =        0;
    int exp_last   =        6;

    double a[6] = { -1.000000000000000000E+00,
                    2.500000000000000000E+00,
                    1.250000000000000000E+00,
                    6.250000000000000000E-01,
                    -5.000000000000000000E-01,
                    -7.500000000000000000E-01} ;
    double b[6] = { -7.500000000000000000E-01,
                    5.000000000000000000E+00,
                    2.500000000000000000E+00,
                    1.250000000000000000E+00,
                    6.250000000000000000E-01,
                    -5.000000000000000000E-01} ;
    double r[6] = { -1.234231128040012976E-01,
                    3.579970394639702888E-03,
                    2.249831615049339983E-02,
                    7.214232992127905808E-02,
                    2.079093855884046535E-02,
                    -8.553244917962132821E-02} ;
    double e[6] = { 1.172832717970022565E-06,
                    9.018232896137375412E-13,
                    1.815172652101790755E-12,
                    1.006998195150956048E-13,
                    1.245463873006391609E-08,
                    1.833082948207153514E-15 } ;
    int order[6] = { 1, 5, 3, 2, 4, 6 } ;

    double alpha = 1.0 ;
    gsl_function f = make_function(&f459, &alpha);
    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qawc (&fc, -1.0, 5.0, 0.0, 0.0, 1.0e-3, w->limit,
                                   w, 
                                   &result, &abserr) ;
    
    gsl_test_rel(result,exp_result,1e-14,"qawc(f459) result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qawc(f459) abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qawc(f459) neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qawc(f459) last") ;  
    gsl_test_int(status,exp_ier,"qawc(f459) status") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qawc(f459) alist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qawc(f459) blist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-14,"qawc(f459) rlist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-4,"qawc(f459) elist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qawc(f459) order");

    p.neval = 0;
    status = gsl_integration_qawc (&fc, 5.0, -1.0, 0.0, 0.0, 1.0e-3, w->limit,
                                   w, 
                                   &result, &abserr) ;
    
    gsl_test_rel(result,-exp_result,1e-14,"qawc(f459) rev result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qawc(f459) rev abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qawc(f459) rev neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qawc(f459) rev last") ;  
    gsl_test_int(status,exp_ier,"qawc(f459) rev status") ;

    gsl_integration_workspace_free (w) ;

  }

  /* Test QAWS singular integration using a relative error bound */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_qaws_table * t 
      = gsl_integration_qaws_table_alloc (0.0, 0.0, 1, 0);

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;

    /* All results are for GSL_IEEE_MODE=double-precision */

    double exp_result = -1.892751853489401670E-01;
    double exp_abserr = 1.129133712015747658E-08;
    int exp_neval  =      280;
    int exp_ier    =        0;
    int exp_last   =        8;

    double a[8] = { 0.000000000000000000E+00,
                    5.000000000000000000E-01,
                    2.500000000000000000E-01,
                    1.250000000000000000E-01,
                    6.250000000000000000E-02,
                    3.125000000000000000E-02,
                    1.562500000000000000E-02,
                    7.812500000000000000E-03} ;
    double b[8] = { 7.812500000000000000E-03,
                    1.000000000000000000E+00,
                    5.000000000000000000E-01,
                    2.500000000000000000E-01,
                    1.250000000000000000E-01,
                    6.250000000000000000E-02,
                    3.125000000000000000E-02,
                    1.562500000000000000E-02} ;
    double r[8] = { -4.126317299834445824E-05,
                    -1.076283950172247789E-01,
                    -6.240573216173390947E-02,
                    -1.456169844189576269E-02,
                    -3.408925115926728436E-03,
                    -8.914083918175634211E-04,
                    -2.574191402137795482E-04,
                    -8.034390712936630608E-05} ;
    double e[8] = { 1.129099387465713953E-08,
                    3.423394967694403596E-13,
                    6.928428071454762659E-16,
                    1.616673288784094320E-16,
                    3.784667152924835070E-17,
                    9.896621209399419425E-18,
                    2.857926564445496100E-18,
                    8.919965558336773736E-19} ;
    int order[8] = { 1, 2, 3, 4, 5, 6, 7, 8 } ;

    double alpha = 1.0 ;
    gsl_function f = make_function(&f458, &alpha);
    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qaws (&fc, 0.0, 1.0, t, 0.0, 1.0e-7, w->limit,
                                   w, 
                                   &result, &abserr) ;
    
    gsl_test_rel(result,exp_result,1e-14,"qaws(f458) ln(x-a) result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qaws(f458) ln(x-a) abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qaws(f458) ln(x-a) neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qaws(f458) ln(x-a) last") ;  
    gsl_test_int(status,exp_ier,"qaws(f458) ln(x-a) status") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qaws(f458) ln(x-a) alist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qaws(f458) ln(x-a) blist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-14,"qaws(f458) ln(x-a) rlist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-4,"qaws(f458) ln(x-a) elist") ;

    for (i = 0; i < 6 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qaws(f458) ln(x-a) order");
    
    /* Test without logs */
    
    gsl_integration_qaws_table_set (t, -0.5, -0.3, 0, 0);
    
    status = gsl_integration_qaws (&fc, 0.0, 1.0, t, 0.0, 1.0e-7, w->limit,
                                   w, &result, &abserr) ;

    exp_result = 9.896686656601706433E-01;
    exp_abserr = 5.888032513201251628E-08;

    gsl_test_rel(result,exp_result,1e-14,"qaws(f458) AB result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qaws(f458) AB abserr") ;

    /* Test with ln(x - a) */

    gsl_integration_qaws_table_set (t, -0.5, -0.3, 1, 0);
    
    status = gsl_integration_qaws (&fc, 0.0, 1.0, t, 0.0, 1.0e-7, w->limit,
                                   w, &result, &abserr) ;

    exp_result = -3.636679470586539620E-01;
    exp_abserr = 2.851348775257054093E-08;

    gsl_test_rel(result,exp_result,1e-14,"qaws(f458) AB ln(x-a) result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qaws(f458) AB ln(x-a) abserr") ;

    /* Test with ln(b - x) */

    gsl_integration_qaws_table_set (t, -0.5, -0.3, 0, 1);
    
    status = gsl_integration_qaws (&fc, 0.0, 1.0, t, 0.0, 1.0e-7, w->limit,
                                   w, &result, &abserr) ;

    exp_result = -1.911489253363409802E+00;
    exp_abserr = 9.854016753016499034E-09;

    gsl_test_rel(result,exp_result,1e-14,"qaws(f458) AB ln(b-x) result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qaws(f458) AB ln(b-x) abserr") ;

    /* Test with ln(x - a) ln(b - x) */

    gsl_integration_qaws_table_set (t, -0.5, -0.3, 1, 1);
    
    status = gsl_integration_qaws (&fc, 0.0, 1.0, t, 0.0, 1.0e-7, w->limit,
                                   w, &result, &abserr) ;

    exp_result = 3.159922862811048172E-01;
    exp_abserr = 2.336183482198144595E-08;

    gsl_test_rel(result,exp_result,1e-14,"qaws(f458) AB ln(x-a)ln(b-x) result") ;
    gsl_test_rel(abserr,exp_abserr,1e-6,"qaws(f458) AB ln(x-a)ln(b-x) abserr") ;

    gsl_integration_workspace_free (w) ;
    gsl_integration_qaws_table_free (t) ;

  }


  /* Test oscillatory integration using a relative error bound */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;
    gsl_integration_qawo_table * wo 
      = gsl_integration_qawo_table_alloc (10.0 * M_PI, 1.0,
                                              GSL_INTEG_SINE, 1000) ;

    /* All results are for GSL_IEEE_MODE=double-precision */

    double exp_result = -1.281368483991674190E-01;
    double exp_abserr =  6.875028324415666248E-12;
    int exp_neval  =      305;
    int exp_ier    =        0;
    int exp_last   =        9;

    double a[9] = { 0.000000000000000000E+00,
                    5.000000000000000000E-01,
                    2.500000000000000000E-01,
                    1.250000000000000000E-01,
                    6.250000000000000000E-02,
                    3.125000000000000000E-02,
                    1.562500000000000000E-02,
                    7.812500000000000000E-03,
                    3.906250000000000000E-03 } ;
    double b[9] = { 3.906250000000000000E-03,
                    1.000000000000000000E+00,
                    5.000000000000000000E-01,
                    2.500000000000000000E-01,
                    1.250000000000000000E-01,
                    6.250000000000000000E-02,
                    3.125000000000000000E-02,
                    1.562500000000000000E-02,
                    7.812500000000000000E-03 } ;
    double r[9] = { -1.447193692377651136E-03,
                    2.190541162282139478E-02,
                    -2.587726479625663753E-02,
                    5.483209176363500886E-02,
                    -3.081695575172510582E-02,
                    -9.178321994387816929E-02,
                    -3.886716016498160953E-02,
                    -1.242306301902117854E-02,
                    -3.659495117871544145E-03} ;
    double e[9] = { 8.326506625798146465E-07,
                    1.302638552580516100E-13,
                    7.259224351945759794E-15,
                    1.249770395036711102E-14,
                    7.832180081562836579E-16,
                    1.018998440559284116E-15,
                    4.315121611695628020E-16,
                    1.379237060008662177E-16,
                    4.062855738364339357E-17 } ;
    int order[9] = { 1, 2, 4, 3, 6, 5, 7, 8, 9 } ;

    double alpha = 1.0 ;
    gsl_function f = make_function(&f456, &alpha);
    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qawo (&fc, 0.0, 0.0, 1e-7, w->limit,
                                   w, wo, &result, &abserr) ;
    
    gsl_test_rel(result,exp_result,1e-14,"qawo(f456) result") ;
    gsl_test_rel(abserr,exp_abserr,1e-3,"qawo(f456) abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qawo(f456) neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qawo(f456) last") ;  
    gsl_test_int(status,exp_ier,"qawo(f456) status") ;

    for (i = 0; i < 9 ; i++) 
        gsl_test_rel(w->alist[i],a[i],1e-15,"qawo(f456) alist") ;

    for (i = 0; i < 9 ; i++) 
        gsl_test_rel(w->blist[i],b[i],1e-15,"qawo(f456) blist") ;

    for (i = 0; i < 9 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-14,"qawo(f456) rlist") ;

    for (i = 0; i < 9 ; i++) 
        gsl_test_rel(w->elist[i],e[i],1e-2,"qawo(f456) elist") ;

    for (i = 0; i < 9 ; i++) 
        gsl_test_int((int)w->order[i],order[i]-1,"qawo(f456) order");


    /* In reverse, flip limit and sign of length */

    gsl_integration_qawo_table_set_length (wo, -1.0);

    p.neval = 0; 
    status = gsl_integration_qawo (&fc, 1.0, 0.0, 1e-7, w->limit,
                                   w, wo, &result, &abserr) ;
    
    gsl_test_rel(result,-exp_result,1e-14,"qawo(f456) rev result") ;
    gsl_test_rel(abserr,exp_abserr,1e-3,"qawo(f456) rev abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qawo(f456) rev neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qawo(f456) rev last") ;  
    gsl_test_int(status,exp_ier,"qawo(f456) rev status") ;


    gsl_integration_qawo_table_free (wo) ;
    gsl_integration_workspace_free (w) ;

  }

  /* Test fourier integration using an absolute error bound */

  {
    int status = 0, i; struct counter_params p;
    double result = 0, abserr=0;

    gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000) ;
    gsl_integration_workspace * wc = gsl_integration_workspace_alloc (1000) ;
    gsl_integration_qawo_table * wo 
      = gsl_integration_qawo_table_alloc (M_PI / 2.0, 1.0,
                                              GSL_INTEG_COSINE, 1000) ;

    /* All results are for GSL_IEEE_MODE=double-precision */

    double exp_result = 9.999999999279802765E-01;
    double exp_abserr = 1.556289974669056164E-08;
    int exp_neval  =      590;
    int exp_ier    =        0;
    int exp_last   =       12;

    double r[12] = { 1.013283128125232802E+00,
                    -1.810857954748607349E-02,
                    7.466754034900931897E-03,
                    -4.360312526786496237E-03,
                    2.950184068216192904E-03,
                    -2.168238443073697373E-03,
                    1.680910783140869081E-03,
                    -1.352797860944863345E-03,
                    1.119354921991485901E-03,
                    -9.462367583691360827E-04,
                    8.136341270731781887E-04,
                    -7.093931338504278145E-04 } ;
    double e[12] = { 1.224798040766472695E-12,
                    1.396565155187268456E-13,
                    1.053844511655910310E-16,
                    6.505213034913026604E-19,
                    7.155734338404329264E-18,
                    1.105886215935214523E-17,
                    9.757819552369539906E-18,
                    5.854691731421723944E-18,
                    4.553649124439220312E-18,
                    7.643625316022806260E-18,
                    2.439454888092388058E-17,
                    2.130457268934021451E-17 } ;

    double alpha = 1.0 ;
    gsl_function f = make_function(&f457, &alpha);
    gsl_function fc = make_counter(&f, &p) ;

    status = gsl_integration_qawf (&fc, 0.0, 1e-7, w->limit,
                                   w, wc, wo, &result, &abserr) ;
    
    gsl_test_rel(result,exp_result,1e-14,"qawf(f457) result") ;
    gsl_test_rel(abserr,exp_abserr,1e-3,"qawf(f457) abserr") ;
    gsl_test_int((int)(p.neval),exp_neval,"qawf(f457) neval") ;  
    gsl_test_int((int)(w->size),exp_last,"qawf(f457) last") ;  
    gsl_test_int(status,exp_ier,"qawf(f457) status") ;

    for (i = 0; i < 9 ; i++) 
        gsl_test_rel(w->rlist[i],r[i],1e-12,"qawf(f457) rlist") ;

    /* We can only get within two orders of magnitude on the error
       here, which is very sensitive to the floating point precision */

    for (i = 0; i < 9 ; i++) 
        gsl_test_rel(w->elist[i],e[i],50.0,"qawf(f457) elist") ;


    gsl_integration_qawo_table_free (wo) ;
    gsl_integration_workspace_free (wc) ;
    gsl_integration_workspace_free (w) ;

  }

  /* Sanity check monomial test function for fixed Gauss-Legendre rules */
  {
    struct monomial_params params;
    gsl_function f;
    
    f.function = &f_monomial;
    f.params = &params;

    params.degree   = 2;
    params.constant = 1.0;
    gsl_test_abs(GSL_FN_EVAL(&f, 2.0), 4.0, 8*GSL_DBL_EPSILON,
        "f_monomial sanity check 1");

    params.degree   = 1;
    params.constant = 2.0;
    gsl_test_abs(GSL_FN_EVAL(&f, 2.0), 4.0, 8*GSL_DBL_EPSILON,
        "f_monomial sanity check 2");

    params.degree   = 2;
    params.constant = 2.0;
    gsl_test_abs(integ_f_monomial(1.0, 2.0, &params),
        (2.0/3.0)*(2.0*2.0*2.0 - 1.0*1.0*1.0), 8*GSL_DBL_EPSILON,
        "integ_f_monomial sanity check");
  }

  /* Test the fixed-order Gauss-Legendre rules with a monomial. */
  {
    int n;
    struct monomial_params params;
    gsl_function f;
    const double a   = 0.0, b = 1.2;

    f.function = &f_monomial;
    f.params = &params;

    params.constant = 1.0;

    for (n = 1; n < 1025; ++n)
      {
        double expected, result;

        gsl_integration_glfixed_table * tbl =
          gsl_integration_glfixed_table_alloc(n);

        params.degree = 2*n-1; /* n point rule exact for 2n-1 degree poly */
        expected      = integ_f_monomial(a, b, &params);
        result        = gsl_integration_glfixed(&f, a, b, tbl);

        if (tbl->precomputed)
          {
            gsl_test_rel(result, expected, 1.0e-12,
                "glfixed %d-point: Integrating (%g*x^%d) over [%g,%g]",
                n, params.constant, params.degree, a, b);
          }
        else
          {
            gsl_test_rel(result, expected, 1.0e-7,
                "glfixed %d-point: Integrating (%g*x^%d) over [%g,%g]",
                n, params.constant, params.degree, a, b);
          }

        gsl_integration_glfixed_table_free(tbl);
      }
  }

  /* Sanity check sin(x) test function for fixed Gauss-Legendre rules */
  {
    gsl_function f = { f_sin, NULL };

    gsl_test_abs(GSL_FN_EVAL(&f, 2.0), sin(2.0), 0.0, "f_sin sanity check 1");
    gsl_test_abs(GSL_FN_EVAL(&f, 7.0), sin(7.0), 0.0, "f_sin sanity check 2");
    gsl_test_abs(integ_f_sin(0.0, M_PI), 2.0, GSL_DBL_EPSILON,
        "integ_f_sin sanity check");
  }

  /* Test the fixed-order Gauss-Legendre rules against sin(x) on [0, pi] */
  {
    const int n_max = 1024;
    const gsl_function f = { f_sin, NULL };
    const double a = 0.0, b = M_PI;
    const double expected = integ_f_sin(a, b);
    double result, abserr, prev_abserr = 0.0;
    int n;

    for (n = 1; n <= n_max; ++n)
      {
        gsl_integration_glfixed_table * const tbl =
          gsl_integration_glfixed_table_alloc(n);

        result = gsl_integration_glfixed(&f, a, b, tbl);
        abserr = fabs(expected - result);

        if (n == 1)
          {
            gsl_test_abs(result, GSL_FN_EVAL(&f,(b+a)/2)*(b-a), 0.0,
                "glfixed %d-point: behavior for n == 1", n);
          }
        else if (n < 9)
          {
            gsl_test(! (abserr < prev_abserr),
                "glfixed %d-point: observed drop in absolute error versus %d-points",
                n, n-1);
          }
        else if (tbl->precomputed)
          {
            gsl_test_abs(result, expected, 2.0 * n * GSL_DBL_EPSILON,
                "glfixed %d-point: very low absolute error for high precision coefficients",
                n);
          }
        else
          {
            gsl_test_abs(result, expected, 1.0e6 * GSL_DBL_EPSILON,
                "glfixed %d-point: acceptable absolute error for on-the-fly coefficients",
                n);
          }

        prev_abserr = abserr;
        gsl_integration_glfixed_table_free(tbl);
      }
  }

  /* Test some fixed-order Gauss-Legendre rule points and weights on [-1, 1] */
  /* This verifies the (point, weight) retrieval API behaves sanely */
  {
    const double eps = GSL_DBL_EPSILON;
    gsl_integration_glfixed_table *tbl;
    int n, i;
    double xi, wi;

    /* Analytical results for points and weights on [-1, 1]
       Pulled from http://en.wikipedia.org/wiki/Gaussian_quadrature
       Sorted in increasing order of Gauss points */
    const double e1[1][2] = {
      {0, 2 }
    };
    const double e2[2][2] = {
      {-1/sqrt(3), 1},
      { 1/sqrt(3), 1}
    };
    const double e3[3][2] = {
      {-sqrt(15)/5, 5./9},
      {          0, 8./9},
      { sqrt(15)/5, 5./9}
    };
    const double e4[4][2] = {
      {-sqrt((3+2*sqrt(6./5))/7), (18-sqrt(30))/36},
      {-sqrt((3-2*sqrt(6./5))/7), (18+sqrt(30))/36},
      { sqrt((3-2*sqrt(6./5))/7), (18+sqrt(30))/36},
      { sqrt((3+2*sqrt(6./5))/7), (18-sqrt(30))/36}
    };
    const double e5[5][2] = {
      {-sqrt((5+2*sqrt(10./7)))/3, (322-13*sqrt(70))/900},
      {-sqrt((5-2*sqrt(10./7)))/3, (322+13*sqrt(70))/900},
      {                         0, 128./225             },
      { sqrt((5-2*sqrt(10./7)))/3, (322+13*sqrt(70))/900},
      { sqrt((5+2*sqrt(10./7)))/3, (322-13*sqrt(70))/900}
    };

    n = 1;
    tbl = gsl_integration_glfixed_table_alloc(n);
    for (i = 0; i < n; ++i)
      {
        gsl_integration_glfixed_point(-1, 1, i, &xi, &wi, tbl);
        gsl_test_abs(xi, e1[i][0], eps, "glfixed %d-point lookup: x(%d)", n, i);
        gsl_test_abs(wi, e1[i][1], eps, "glfixed %d-point lookup: w(%d)", n, i);
      }
    gsl_integration_glfixed_table_free(tbl);

    n = 2;
    tbl = gsl_integration_glfixed_table_alloc(n);
    for (i = 0; i < n; ++i)
      {
        gsl_integration_glfixed_point(-1, 1, i, &xi, &wi, tbl);
        gsl_test_abs(xi, e2[i][0], eps, "glfixed %d-point lookup: x(%d)", n, i);
        gsl_test_abs(wi, e2[i][1], eps, "glfixed %d-point lookup: w(%d)", n, i);
      }
    gsl_integration_glfixed_table_free(tbl);

    n = 3;
    tbl = gsl_integration_glfixed_table_alloc(n);
    for (i = 0; i < n; ++i)
      {
        gsl_integration_glfixed_point(-1, 1, i, &xi, &wi, tbl);
        gsl_test_abs(xi, e3[i][0], eps, "glfixed %d-point lookup: x(%d)", n, i);
        gsl_test_abs(wi, e3[i][1], eps, "glfixed %d-point lookup: w(%d)", n, i);
      }
    gsl_integration_glfixed_table_free(tbl);

    n = 4;
    tbl = gsl_integration_glfixed_table_alloc(n);
    for (i = 0; i < n; ++i)
      {
        gsl_integration_glfixed_point(-1, 1, i, &xi, &wi, tbl);
        gsl_test_abs(xi, e4[i][0], eps, "glfixed %d-point lookup: x(%d)", n, i);
        gsl_test_abs(wi, e4[i][1], eps, "glfixed %d-point lookup: w(%d)", n, i);
      }
    gsl_integration_glfixed_table_free(tbl);

    n = 5;
    tbl = gsl_integration_glfixed_table_alloc(n);
    for (i = 0; i < n; ++i)
      {
        gsl_integration_glfixed_point(-1, 1, i, &xi, &wi, tbl);
        gsl_test_abs(xi, e5[i][0], eps, "glfixed %d-point lookup: x(%d)", n, i);
        gsl_test_abs(wi, e5[i][1], eps, "glfixed %d-point lookup: w(%d)", n, i);
      }
    gsl_integration_glfixed_table_free(tbl);
  }

  /* Test some fixed-order Gauss-Legendre rule points and weights on [-2, 3] */
  /* This verifies the (point, weight) retrieval API is okay on non-[-1,1] */
  {
    gsl_integration_glfixed_table *tbl;
    double result, x, w;
    int i;

    /* Odd n = 3, f(x) = x**5 + x**4 + x**3 + x**2 + x**1 + 1 */
    result = 0;
    tbl = gsl_integration_glfixed_table_alloc(3);
    for (i = 0; i < 3; ++i)
      {
        gsl_integration_glfixed_point(-2, 3, i, &x, &w, tbl);
        result += w * (1 + x*(1 + x*(1 + x*(1 + x*(1 + x)))));
      }
    gsl_test_rel(result, 805./4, 1e-8,
        "glfixed %d-point xi,wi eval", 3);
    gsl_integration_glfixed_table_free(tbl);

    /* Even n = 4, f(x) = x**7 + x**6 + x**5 + x**4 + x**3 + x**2 + x**1 + 1 */
    result = 0;
    tbl = gsl_integration_glfixed_table_alloc(4);
    for (i = 0; i < 4; ++i)
      {
        gsl_integration_glfixed_point(-2, 3, i, &x, &w, tbl);
        result += w * (1 + x*(1 + x*(1 + x*(1 + x*(1 + x*(1 + x*(1 + x)))))));
      }
    gsl_test_rel(result, 73925./56, 1e-8,
        "glfixed %d-point xi,wi eval", 4);
    gsl_integration_glfixed_table_free(tbl);
  }

  {
    typedef double (*fptr) ( double , void * );
    
    const static fptr funs[25] = { &cqf1 , &cqf2 , &cqf3 , &cqf4 , &cqf5 , &cqf6 , &cqf7 , 
                                   &cqf8 , &cqf9 , &cqf10 , &cqf11 , &cqf12 , &cqf13 , &cqf14 , &cqf15 , &cqf16 , &cqf17 ,
                                   &cqf18 , &cqf19 , &cqf20 , &cqf21 , &cqf22 , &cqf23 , &cqf24 , &cqf25 };
    
    const static double ranges[50] = { 0, 1 , 0, 1 , 0, 1 , -1, 1 , -1, 1 , 0, 1 , 0, 1 , 0, 1 , 0, 1 , 
                                       0, 1 , 0, 1 , 0, 1 , 0, 1 , 0, 10 , 0, 10 , 0, 10 , 0, 1 , 0, M_PI ,
                                       0, 1 , -1, 1 , 0, 1 , 0, 1 , 0, 1 , 0, 3 , 0, 5 };
    const static double f_exact[25] = { 1.7182818284590452354 , 0.7 , 2.0/3 , 0.4794282266888016674 , 
                                        1.5822329637296729331 , 0.4 , 2 , 0.86697298733991103757 , 
                                        1.1547005383792515290 , 0.69314718055994530942 , 0.3798854930417224753 , 
                                        0.77750463411224827640 , 0.49898680869304550249 , 
                                        0.5 , 1 , 0.13263071079267703209e+08 , 0.49898680869304550249 , 
                                        0.83867634269442961454 , -1 , 1.5643964440690497731 , 
                                        0.16349494301863722618 , -0.63466518254339257343 , 
                                        0.013492485649467772692 , 17.664383539246514971 , 7.5 };
    
    double result, abserr;
    size_t neval;
    int fid;
        
    /* Loop over the functions... */
    for ( fid = 0 ; fid < 25 ; fid++ ) {
      gsl_integration_cquad_workspace *ws = gsl_integration_cquad_workspace_alloc ( 200 );
      gsl_function f = make_function(funs[fid], NULL);
      double exact = f_exact[fid];

      /* Call our quadrature routine. */
      int status = gsl_integration_cquad (&f, ranges[2*fid] , ranges[2*fid+1] , 0.0 , 1.0e-12 , ws , &result , &abserr , &neval);
      
      gsl_test_rel (result, exact, 1e-12, "cquad f%d", fid);
      gsl_test (fabs(result - exact) > 5.0 * abserr, "cquad f%d error (%g actual vs %g estimated)", fid, fabs(result-exact), abserr);
      gsl_test_int (status, GSL_SUCCESS, "cquad return code");

      gsl_integration_cquad_workspace_free(ws);
    }
  }        


  exit (gsl_test_summary());
} 

void
my_error_handler (const char *reason, const char *file, int line, int err)
{
  if (0) printf ("(caught [%s:%d: %s (%d)])\n", file, line, reason, err) ;
}
