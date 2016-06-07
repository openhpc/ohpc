/* const/test.c
 * 
 * Copyright (C) 2003, 2007 Brian Gough
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
#include <math.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_const.h>
#include <gsl/gsl_test.h>

#include <gsl/gsl_ieee_utils.h>

int
main (void)
{
  gsl_ieee_env_setup ();

  /* Basic check to make sure the header files are functioning */

  {
    double c = GSL_CONST_MKS_SPEED_OF_LIGHT;
    double eps = GSL_CONST_MKS_VACUUM_PERMITTIVITY;
    double mu = GSL_CONST_MKS_VACUUM_PERMEABILITY;

    gsl_test_rel (c, 1.0/sqrt(eps*mu), 1e-6, "speed of light (mks)");
  }

  {
    double ly = GSL_CONST_CGS_LIGHT_YEAR;
    double c = GSL_CONST_CGS_SPEED_OF_LIGHT;
    double y = 365.2425 * GSL_CONST_CGS_DAY;
    
    gsl_test_rel (ly, c * y, 1e-6, "light year (cgs)");
  }

  {
    double c = GSL_CONST_MKSA_SPEED_OF_LIGHT;
    double eps = GSL_CONST_MKSA_VACUUM_PERMITTIVITY;
    double mu = GSL_CONST_MKSA_VACUUM_PERMEABILITY;

    gsl_test_rel (c, 1.0/sqrt(eps*mu), 1e-6, "speed of light (mksa)");
  }

  {
    double ly = GSL_CONST_CGSM_LIGHT_YEAR;
    double c = GSL_CONST_CGSM_SPEED_OF_LIGHT;
    double y = 365.2425 * GSL_CONST_CGSM_DAY;
    
    gsl_test_rel (ly, c * y, 1e-6, "light year (cgsm)");
  }

  {
    double micro = GSL_CONST_NUM_MICRO;
    double mega = GSL_CONST_NUM_MEGA;
    double kilo = GSL_CONST_NUM_KILO;

    gsl_test_rel (mega/kilo, 1/(micro*kilo), 1e-10, "kilo (mega/kilo, 1/(micro*kilo))");
  }

  {
    double d = GSL_CONST_MKSA_DEBYE;
    double c = GSL_CONST_MKSA_SPEED_OF_LIGHT;
    double desu = d * c * 1000.0;
    
    gsl_test_rel (desu, 1e-18, 1e-10, "debye (esu)");
  }

  {
    double k = GSL_CONST_MKSA_BOLTZMANN;
    double c = GSL_CONST_MKSA_SPEED_OF_LIGHT;
    double h = GSL_CONST_MKSA_PLANCKS_CONSTANT_H;
    double s = 2 * pow(M_PI, 5.0) * pow(k, 4.0) / (15 * pow(c, 2.0) * pow(h, 3.0));
    double sigma = GSL_CONST_MKSA_STEFAN_BOLTZMANN_CONSTANT;
    
    gsl_test_rel(s, sigma, 1e-10, "stefan boltzmann constant");
  }


  exit (gsl_test_summary ());
}

