#include <config.h>
#include <math.h>
#include <stdlib.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_complex.h>
#include <gsl/gsl_errno.h>

#include <gsl/gsl_dft_complex.h>
#include <gsl/gsl_dft_complex_float.h>

#include "complex_internal.h"

#include "urand.c"

#define BASE_DOUBLE
#include "templates_on.h"
#include "signals_source.c"
#include "templates_off.h"
#undef  BASE_DOUBLE

#define BASE_FLOAT
#include "templates_on.h"
#include "signals_source.c"
#include "templates_off.h"
#undef  BASE_FLOAT

