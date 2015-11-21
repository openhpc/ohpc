/* statistics/test_float_source.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 Jim Davies, Brian Gough
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

void FUNCTION (test, func) (const size_t stridea, const size_t strideb);

void
FUNCTION (test, func) (const size_t stridea, const size_t strideb)
{
  /* sample sets of doubles */
  size_t i;
  const size_t na = 14, nb = 14;

  const double rawa[] =
  {.0421, .0941, .1064, .0242, .1331,
   .0773, .0243, .0815, .1186, .0356,
   .0728, .0999, .0614, .0479};

  const double rawb[] =
  {.1081, .0986, .1566, .1961, .1125,
   .1942, .1079, .1021, .1583, .1673,
   .1675, .1856, .1688, .1512};

  const double raww[] = 
  {.0000, .0000, .0000, 3.000, .0000,
   1.000, 1.000, 1.000, 0.000, .5000,
   7.000, 5.000, 4.000, 0.123};

  BASE * sorted ;

  BASE * groupa = (BASE *) malloc (stridea * na * sizeof(BASE));
  BASE * groupb = (BASE *) malloc (strideb * nb * sizeof(BASE));
  BASE * w = (BASE *) malloc (strideb * na * sizeof(BASE));

#ifdef BASE_FLOAT
  double rel = 1e-6;
#else
  double rel = 1e-10;
#endif

  for (i = 0 ; i < na ; i++)
    groupa[i * stridea] = (BASE) rawa[i] ;

  for (i = 0 ; i < na ; i++)
    w[i * strideb] = (BASE) raww[i] ;

  for (i = 0 ; i < nb ; i++)
    groupb[i * strideb] = (BASE) rawb[i] ;


  {
    double mean = FUNCTION(gsl_stats,mean) (groupa, stridea, na);
    double expected = 0.0728;
    gsl_test_rel (mean, expected, rel, NAME(gsl_stats) "_mean");
  }

  {
    double mean = FUNCTION(gsl_stats,mean) (groupa, stridea, na);
    double var = FUNCTION(gsl_stats,variance_with_fixed_mean) (groupa, stridea, na, mean);
    double expected = 0.00113837428571429;
    gsl_test_rel (var, expected, rel, NAME(gsl_stats) "_variance_with_fixed_mean");
  }


  {
    double mean = FUNCTION(gsl_stats,mean) (groupa, stridea, na);
    double var = FUNCTION(gsl_stats,sd_with_fixed_mean) (groupa, stridea, na, mean);
    double expected = 0.0337398026922845;
    gsl_test_rel (var, expected, rel, NAME(gsl_stats) "_sd_with_fixed_mean");
  }


  {
    double var = FUNCTION(gsl_stats,variance) (groupb, strideb, nb);
    double expected = 0.00124956615384615;
    gsl_test_rel (var, expected, rel, NAME(gsl_stats) "_variance");
  }

  {
    double sd = FUNCTION(gsl_stats,sd) (groupa, stridea, na);
    double expected = 0.0350134479659107;
    gsl_test_rel (sd, expected, rel, NAME(gsl_stats) "_sd");
  }

  {
    double ss = FUNCTION(gsl_stats,tss) (groupb, strideb, nb);
    double expected = 0.01624436;
    gsl_test_rel (ss, expected, rel, NAME(gsl_stats) "_ss");
  }

  {
    double mean = FUNCTION(gsl_stats,mean) (groupa, stridea, na);
    double ss = FUNCTION(gsl_stats,tss_m) (groupa, stridea, na, mean);
    double expected = 1.59372400000000e-02;
    gsl_test_rel (ss, expected, rel, NAME(gsl_stats) "_ss_m");
  }

  {
    double absdev = FUNCTION(gsl_stats,absdev) (groupa, stridea, na);
    double expected = 0.0287571428571429;
    gsl_test_rel (absdev, expected, rel, NAME(gsl_stats) "_absdev");
  }

  {
    double skew = FUNCTION(gsl_stats,skew) (groupa, stridea, na);
    double expected = 0.0954642051479004;
    gsl_test_rel (skew, expected, rel, NAME(gsl_stats) "_skew");
  }

  {
    double kurt = FUNCTION(gsl_stats,kurtosis) (groupa, stridea, na);
    double expected = -1.38583851548909 ;
    gsl_test_rel (kurt, expected, rel, NAME(gsl_stats) "_kurtosis");
  }

  {
    double wmean = FUNCTION(gsl_stats,wmean) (w, strideb, groupa, stridea, na);
    double expected = 0.0678111523670601;
    gsl_test_rel (wmean, expected, rel, NAME(gsl_stats) "_wmean");
  }

  {
    double wmean = FUNCTION(gsl_stats,wmean) (w, strideb, groupa, stridea, na);
    double wvar = FUNCTION(gsl_stats,wvariance_with_fixed_mean) (w, strideb, groupa, stridea, na, wmean);
    double expected = 0.000615793060878654;
    gsl_test_rel (wvar, expected, rel, NAME(gsl_stats) "_wvariance_with_fixed_mean");
  }

  {
    double est_wvar = FUNCTION(gsl_stats,wvariance) (w, strideb, groupa, stridea, na);
    double expected = 0.000769562962860317;
    gsl_test_rel (est_wvar, expected, rel, NAME(gsl_stats) "_wvariance");
  }

  {
    double wsd = FUNCTION(gsl_stats,wsd) (w, strideb, groupa, stridea, na);
    double expected = 0.0277409978706664;
    gsl_test_rel (wsd, expected, rel, NAME(gsl_stats) "_wsd");
  }


  {
    double wtss = FUNCTION(gsl_stats,wtss) (w, strideb, groupa, stridea, na);
    double expected =  1.39310864162578e-02;
    gsl_test_rel (wtss, expected, rel, NAME(gsl_stats) "_wtss");
  }

  {
    double wmean = FUNCTION(gsl_stats,wmean) (w, strideb, groupa, stridea, na);
    double wtss = FUNCTION(gsl_stats,wtss_m) (w, strideb, groupa, stridea, na, wmean);
    double expected =  1.39310864162578e-02;
    gsl_test_rel (wtss, expected, rel, NAME(gsl_stats) "_wtss_m");
  }



  {
    double wabsdev = FUNCTION(gsl_stats,wabsdev) (w, strideb, groupa, stridea, na);
    double expected = 0.0193205027504008;
    gsl_test_rel (wabsdev, expected, rel, NAME(gsl_stats) "_wabsdev");
  }

  {
    double wskew = FUNCTION(gsl_stats,wskew) (w, strideb, groupa, stridea, na);
    double expected = -0.373631000307076;
    gsl_test_rel (wskew, expected, rel, NAME(gsl_stats) "_wskew");
  }

  {
    double wkurt = FUNCTION(gsl_stats,wkurtosis) (w, strideb, groupa, stridea, na);
    double expected = -1.48114233353963;
    gsl_test_rel (wkurt, expected, rel, NAME(gsl_stats) "_wkurtosis");
  }

  {
    double c = FUNCTION(gsl_stats,covariance) (groupa, stridea, groupb, strideb, nb);
    double expected = -0.000139021538461539;
    gsl_test_rel (c, expected, rel, NAME(gsl_stats) "_covariance");
  }

  {
    double r = FUNCTION(gsl_stats,correlation) (groupa, stridea, groupb, strideb, nb);
    double expected = -0.112322712666074171;
    gsl_test_rel (r, expected, rel, NAME(gsl_stats) "_correlation");
  }

  {
    double *work = malloc(2 * na * sizeof(double));
    double r = FUNCTION(gsl_stats,spearman) (groupa, stridea, groupb, strideb, nb, work);
    double expected = -0.1604395604395604396;
    gsl_test_rel (r, expected, rel, NAME(gsl_stats) "_spearman");
    free(work);
  }

  {
    double pv = FUNCTION(gsl_stats,pvariance) (groupa, stridea, na, groupb, strideb, nb);
    double expected = 0.00123775384615385;
    gsl_test_rel (pv, expected, rel, NAME(gsl_stats) "_pvariance");
  }

  {
    double t = FUNCTION(gsl_stats,ttest) (groupa, stridea, na, groupb, strideb, nb);
    double expected = -5.67026326985851;
    gsl_test_rel (t, expected, rel, NAME(gsl_stats) "_ttest");
  }

  {
    BASE expected = (BASE)0.1331;
    gsl_test  (FUNCTION(gsl_stats,max) (groupa, stridea, na) != expected,
               NAME(gsl_stats) "_max (" OUT_FORMAT " observed vs " OUT_FORMAT " expected)", 
               FUNCTION(gsl_stats,max) (groupa, stridea, na), expected);
  }

  {
    BASE min = FUNCTION(gsl_stats,min) (groupa, stridea, na);
    BASE expected = (BASE)0.0242;
    gsl_test (min != expected,
              NAME(gsl_stats) "_min (" OUT_FORMAT " observed vs " OUT_FORMAT " expected)", 
              min, expected);
  }

  {
    BASE min, max;
    BASE expected_max = (BASE)0.1331;
    BASE expected_min = (BASE)0.0242;
    
    FUNCTION(gsl_stats,minmax) (&min, &max, groupa, stridea, na);
 
    gsl_test  (max != expected_max,
               NAME(gsl_stats) "_minmax max (" OUT_FORMAT " observed vs " OUT_FORMAT " expected)", 
               max, expected_max);
    gsl_test  (min != expected_min,
               NAME(gsl_stats) "_minmax min (" OUT_FORMAT " observed vs " OUT_FORMAT " expected)", 
               min, expected_min);
  }

  {
    int max_index = FUNCTION(gsl_stats,max_index) (groupa, stridea, na);
    int expected = 4;
    gsl_test (max_index != expected,
              NAME(gsl_stats) "_max_index (%d observed vs %d expected)",
              max_index, expected);
  }

  {
    int min_index = FUNCTION(gsl_stats,min_index) (groupa, stridea, na);
    int expected = 3;
    gsl_test (min_index != expected,
              NAME(gsl_stats) "_min_index (%d observed vs %d expected)",
              min_index, expected);
  }

  {
    size_t min_index, max_index;
    size_t expected_max_index = 4;
    size_t expected_min_index = 3;

    FUNCTION(gsl_stats,minmax_index) (&min_index, &max_index, groupa, stridea, na);

    gsl_test  (max_index != expected_max_index,
               NAME(gsl_stats) "_minmax_index max (%u observed vs %u expected)", 
               max_index, expected_max_index);
    gsl_test  (min_index != expected_min_index,
               NAME(gsl_stats) "_minmax_index min (%u observed vs %u expected)", 
               min_index, expected_min_index);
  }


  sorted = (BASE *) malloc(stridea * na * sizeof(BASE)) ;
  
  for (i = 0 ; i < na ; i++)
    sorted[stridea * i] = groupa[stridea * i] ;
  
  TYPE(gsl_sort)(sorted, stridea, na) ;

  {
    double median = FUNCTION(gsl_stats,median_from_sorted_data)(sorted, stridea, na) ;
    double expected = 0.07505;
    gsl_test_rel  (median,expected, rel,
                   NAME(gsl_stats) "_median_from_sorted_data (even)");
  }

  {
    double median = FUNCTION(gsl_stats,median_from_sorted_data)(sorted, stridea, na - 1) ;
    double expected = 0.0728;
    gsl_test_rel  (median,expected, rel,
                   NAME(gsl_stats) "_median_from_sorted_data");
  }


  {
    double zeroth = FUNCTION(gsl_stats,quantile_from_sorted_data)(sorted, stridea, na, 0.0) ;
    double expected = 0.0242;
    gsl_test_rel  (zeroth,expected, rel,
                   NAME(gsl_stats) "_quantile_from_sorted_data (0)");
  }

  {
    double top = FUNCTION(gsl_stats,quantile_from_sorted_data)(sorted, stridea, na, 1.0) ;
    double expected = 0.1331;
    gsl_test_rel  (top,expected, rel,
                   NAME(gsl_stats) "_quantile_from_sorted_data (100)");
  }

  {
    double median = FUNCTION(gsl_stats,quantile_from_sorted_data)(sorted, stridea, na, 0.5) ;
    double expected = 0.07505;
    gsl_test_rel  (median,expected, rel,
                   NAME(gsl_stats) "_quantile_from_sorted_data (50even)");
  }

  {
    double median = FUNCTION(gsl_stats,quantile_from_sorted_data)(sorted, stridea, na - 1, 0.5);
    double expected = 0.0728;
    gsl_test_rel  (median,expected, rel,
                   NAME(gsl_stats) "_quantile_from_sorted_data (50odd)");

  }

  /* Test for IEEE handling - set third element to NaN */

  groupa [3*stridea] = GSL_NAN;

  {
    BASE max = FUNCTION(gsl_stats,max) (groupa, stridea, na);
    BASE expected = GSL_NAN;
    gsl_test  (!isnan(max),
               NAME(gsl_stats) "_max NaN (" OUT_FORMAT " observed vs " OUT_FORMAT " expected)", 
               max, expected);
  }

  {
    BASE min = FUNCTION(gsl_stats,min) (groupa, stridea, na);
    BASE expected = GSL_NAN;
    gsl_test (!isnan(min),
              NAME(gsl_stats) "_min NaN (" OUT_FORMAT " observed vs " OUT_FORMAT " expected)", 
              min, expected);
  }

  {
    BASE min, max;
    BASE expected_max = GSL_NAN;
    BASE expected_min = GSL_NAN;
    
    FUNCTION(gsl_stats,minmax) (&min, &max, groupa, stridea, na);
 
    gsl_test  (!isnan(max),
               NAME(gsl_stats) "_minmax max NaN (" OUT_FORMAT " observed vs " OUT_FORMAT " expected)", 
               max, expected_max);
    gsl_test  (!isnan(min),
               NAME(gsl_stats) "_minmax min NaN (" OUT_FORMAT " observed vs " OUT_FORMAT " expected)", 
               min, expected_min);
  }

#ifdef FAST
  {
    BASE min, max;
    BASE expected_max = GSL_NAN;
    BASE expected_min = GSL_NAN;
    
    FUNCTION(gsl_stats,minmax) (&min, &max, groupa, stridea, na-1);
 
    gsl_test  (!isnan(max),
               NAME(gsl_stats) "_minmax(-1) max NaN (" OUT_FORMAT " observed vs " OUT_FORMAT " expected)", 
               max, expected_max);
    gsl_test  (!isnan(min),
               NAME(gsl_stats) "_minmax(-1) min NaN (" OUT_FORMAT " observed vs " OUT_FORMAT " expected)", 
               min, expected_min);
  }
#endif


  {
    int max_index = FUNCTION(gsl_stats,max_index) (groupa, stridea, na);
    int expected = 3;
    gsl_test (max_index != expected,
              NAME(gsl_stats) "_max_index NaN (%d observed vs %d expected)",
              max_index, expected);
  }

  {
    int min_index = FUNCTION(gsl_stats,min_index) (groupa, stridea, na);
    int expected = 3;
    gsl_test (min_index != expected,
              NAME(gsl_stats) "_min_index NaN (%d observed vs %d expected)",
              min_index, expected);
  }

  {
    size_t min_index, max_index;
    size_t expected_max_index = 3;
    size_t expected_min_index = 3;

    FUNCTION(gsl_stats,minmax_index) (&min_index, &max_index, groupa, stridea, na);

    gsl_test  (max_index != expected_max_index,
               NAME(gsl_stats) "_minmax_index max NaN (%u observed vs %u expected)", 
               max_index, expected_max_index);
    gsl_test  (min_index != expected_min_index,
               NAME(gsl_stats) "_minmax_index min NaN (%u observed vs %u expected)", 
               min_index, expected_min_index);
  }

  free (sorted);
  free (groupa);
  free (groupb);
  free (w);

}
