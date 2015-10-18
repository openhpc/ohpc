/*********************************************************************
 *   Copyright 1996, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: util.c,v 1.27 2006/12/10 13:59:56 ed Exp $
 *********************************************************************/

#include "tests.h"
#include <math.h>

void
print_nok(int nok)
{
    if (verbose || nfails > 0)
        print("\n");
    print(" %d good comparisons. ", nok);
}


/* Is value within external type range? */
int
inRange(const double value, const nc_type datatype)
{
    double min, max;

    switch (datatype) {
	case NC_CHAR:   min = X_CHAR_MIN;   max = X_CHAR_MAX; break;
	case NC_BYTE:   min = X_BYTE_MIN;   max = X_BYTE_MAX; break;
	case NC_SHORT:  min = X_SHORT_MIN;  max = X_SHORT_MAX; break;
	case NC_INT:   min = X_INT_MIN;   max = X_INT_MAX; break;
	case NC_FLOAT:  min = X_FLOAT_MIN;  max = X_FLOAT_MAX; break;
	case NC_DOUBLE: min = X_DOUBLE_MIN; max = X_DOUBLE_MAX; break;
	default:  assert(0);
    }
    return value >= min && value <= max;
}

static int
inRange_uchar(const double value, const nc_type datatype)
{
    if (datatype == NC_BYTE) {
	return(value >= 0 && value <= 255);
    }
    /* else */
    return inRange(value, datatype);
}

static int
inRange_float(const double value, const nc_type datatype)
{
    double min, max;

    switch (datatype) {
	case NC_CHAR:   min = X_CHAR_MIN;   max = X_CHAR_MAX; break;
	case NC_BYTE:   min = X_BYTE_MIN;   max = X_BYTE_MAX; break;
	case NC_SHORT:  min = X_SHORT_MIN;  max = X_SHORT_MAX; break;
	case NC_INT:   min = X_INT_MIN;   max = X_INT_MAX; break;
	case NC_FLOAT:
		if(FLT_MAX < X_FLOAT_MAX) {
			min = (-FLT_MAX);
			max = FLT_MAX;
		} else {
			min = X_FLOAT_MIN;
			max = X_FLOAT_MAX;
		}
		break;
	case NC_DOUBLE:
		if(FLT_MAX < X_DOUBLE_MAX) {
			min = (-FLT_MAX);
			max = FLT_MAX;
		} else {
			min = X_DOUBLE_MIN;
			max = X_DOUBLE_MAX;
		}
		break;
	default:  assert(0);
    }
    if(!( value >= min && value <= max)) {
#if 0	/* DEBUG */
	if(datatype == NC_FLOAT) {
	fprintf(stderr, "\n");
	fprintf(stderr, "min   % .17e\n", min);
	fprintf(stderr, "value % .17e\n", value);
	fprintf(stderr, "max   % .17e\n", max);
	}
#endif
	return 0;
    }
#if FLT_MANT_DIG != DBL_MANT_DIG
    /* else */
    {
	const float fvalue = value;
	return fvalue >= min && fvalue <= max;
    }
#else
    return 1;
#endif
}

/* wrapper for inRange to handle special NC_BYTE/uchar adjustment */
int
inRange3(
    const double value, 
    const nc_type datatype,
    const nct_itype itype)
{
    switch (itype) {
    case NCT_UCHAR:
	return inRange_uchar(value, datatype);
    case NCT_FLOAT:
	return inRange_float(value, datatype);
    default:
	break;
    }
    return inRange(value, datatype);
}


/* 
 *  Does x == y, where one is internal and other external (netCDF)?  
 *  Use tolerant comparison based on IEEE FLT_EPSILON or DBL_EPSILON.
 */
int
equal(
    const double x, 
    const double y, 
    nc_type extType, 	/* external data type */
    nct_itype itype)
{
    const double flt_epsilon = 1.19209290E-07;
    const double dbl_epsilon = 2.2204460492503131E-16;
    double epsilon;

    epsilon = extType == NC_FLOAT || itype == NCT_FLOAT ? flt_epsilon : dbl_epsilon;
    return ABS(x-y) <= epsilon * MAX( ABS(x), ABS(y));
}

/* Test whether two int vectors are equal. If so return 1, else 0  */
int
int_vec_eq(const int *v1, const int *v2, const int n)
{
    int i;
    for (i= 0; i < n && v1[i] == v2[i]; i++)
	;
    return i == n;
}


/*
 *  Generate random integer from 0 to n-1
 *  Like throwing an n-sided dice marked 0, 1, 2, ..., n-1
 */
int roll( int n )
{
    int  r;

    do
	/*
	 * Compute a pseudo-random value between 0.0 and 1.0, multiply
	 * it by n-1, and then find the nearest integer.
	 *
	 * We don't use RAND_MAX here because not all compilation
	 * environments define it (e.g. gcc(1) under SunOS 4.1.4).
	 */
	r = ((rand() % 32768) / 32767.0) * (n - 1) + 0.5;
    while (r >= n);

    return r;
}


/*
 *      Convert number to mixed base
 *
 *      E.g. to convert 41 inches to yards, feet and inches:
 *      size_t base[] = {1, 3, 12};
 *      size_t result[3];
 *      status = toMixedBase(41, 3, base, result);
 *
 *      Author: Harvey Davies, Unidata/UCAR, Boulder, Colorado
 */
int
toMixedBase(
    size_t number,        /* number to be converted to mixed base */
    size_t length,
    const size_t base[],        /* dimensioned [length], base[0] ignored */
    size_t result[])      /* dimensioned [length] */
{
    size_t i;

    if (length > 0) {
	for (i = length - 1; i > 0; i--) {
	    if (base[i] == 0)
		return 1;
	    result[i] = number % base[i];
	    number = number / base[i];
	}
        result[0] = number;
    }
    return 0;
}

/*
 *      Convert number from mixed base
 *
 *      E.g. to convert 1 yard, 0 feet, 5 inches to inches:
 *      size_t number[] = {1, 0, 5};
 *      size_t base[] = {1, 3, 12};
 *      inches = fromMixedBase(3, number, base);
 *
 *      Author: Harvey Davies, Unidata/UCAR, Boulder, Colorado
 */
size_t
fromMixedBase(
    size_t length,
    size_t number[],      /* dimensioned [length] */
    size_t base[])        /* dimensioned [length], base[0] ignored */
{
    size_t i;
    size_t result = 0;

    for (i = 1; i < length; i++) {
        result += number[i-1];
        result *= base[i];
    }
    if (length > 0)
        result += number[i-1];
    return result;
}


/* Convert any nc_type to double */
int nc2dbl ( const nc_type datatype, const void *p, double *result)
{
    if ( ! p ) return 2;
    if ( ! result ) return 3;
    switch (datatype) {
        case NC_BYTE: *result = *((signed char *) p); break;
        case NC_CHAR: *result = *((char *) p); break;
        case NC_SHORT: *result = *((short *) p); break;
        case NC_INT:
#if INT_MAX >= X_INT_MAX
		*result = *((int *) p);
#else
		*result = *((long *) p);
#endif
		break;
        case NC_FLOAT: *result = *((float *) p); break;
        case NC_DOUBLE: *result = *((double *) p); break;
        default: return 1;
    }
    return 0;
}


/* Convert double to any nc_type */
int dbl2nc ( const double d, const nc_type datatype, void *p)
{
    double r;   /* rounded value */

    if (p) {
        switch (datatype) {
            case NC_BYTE:
                r = floor(0.5+d);
                if ( r < schar_min  ||  r > schar_max )  return 2;
                *((signed char *) p) = r;
                break;
            case NC_CHAR:
                r = floor(0.5+d);
                if ( r < text_min  ||  r > text_max )  return 2;
                *((char   *) p) = r;
                break;
            case NC_SHORT:
                r = floor(0.5+d);
                if ( r < short_min  ||  r > short_max )  return 2;
                *((short  *) p) = r;
                break;
            case NC_INT:
                r = floor(0.5+d);
                if ( r < long_min  ||  r > long_max )  return 2;
#if INT_MAX >= X_INT_MAX
                *((int   *) p) = r;
#else
                *((long   *) p) = r;
#endif
                break;
            case NC_FLOAT:
                if ( fabs(d) > float_max )  return 2;
                *((float  *) p) = d;
                break;
            case NC_DOUBLE:
                *((double *) p) = d;
                break;
            default:
                return 1;
        }
	return 0;
    } else {
	return 1;
    }
}

#define FUZZ (1.19209290E-07)

#ifdef USE_EXTREME_NUMBERS
/* Generate data values as function of type, rank (-1 for attribute), index */
double
hash( const nc_type type, const int rank, const size_t *index ) 
{
    double base;
    double result;
    int  d;       /* index of dimension */

	/* If vector then elements 0 & 1 are min & max. Elements 2 & 3 are */
	/* just < min & > max (except for NC_CHAR & NC_DOUBLE) */
    if (abs(rank) == 1 && index[0] <= 3) {
	switch (index[0]) {
	    case 0:
		switch (type) {
		    case NC_CHAR:   return X_CHAR_MIN;
		    case NC_BYTE:   return X_BYTE_MIN;
		    case NC_SHORT:  return X_SHORT_MIN;
		    case NC_INT:   return X_INT_MIN;
		    case NC_FLOAT:  return X_FLOAT_MIN;
		    case NC_DOUBLE: return X_DOUBLE_MIN;
		    default:  assert(0);
		}
	    case 1:
		switch (type) {
		    case NC_CHAR:   return X_CHAR_MAX;
		    case NC_BYTE:   return X_BYTE_MAX;
		    case NC_SHORT:  return X_SHORT_MAX;
		    case NC_INT:   return X_INT_MAX;
		    case NC_FLOAT:  return X_FLOAT_MAX;
		    case NC_DOUBLE: return X_DOUBLE_MAX;
		    default:  assert(0);
		}
	    case 2:
		switch (type) {
		    case NC_CHAR:   return 'A';
		    case NC_BYTE:   return X_BYTE_MIN-1.0;
		    case NC_SHORT:  return X_SHORT_MIN-1.0;
		    case NC_INT:   return X_INT_MIN-1.0;
		    case NC_FLOAT:  return X_FLOAT_MIN * (1.0 + FUZZ);
		    case NC_DOUBLE: return -1.0;
		    default:  assert(0);
		}
	    case 3:
		switch (type) {
		    case NC_CHAR:   return 'Z';
		    case NC_BYTE:   return X_BYTE_MAX+1.0;
		    case NC_SHORT:  return X_SHORT_MAX+1.0;
		    case NC_INT:   return X_INT_MAX+1.0;
		    case NC_FLOAT:  return X_FLOAT_MAX * (1.0 + FUZZ);
		    case NC_DOUBLE: return 1.0;
		    default:  assert(0);
		}
	}
    } else {
	switch (type) {
	    case NC_CHAR: base = 2; break;
	    case NC_BYTE: base = -2; break;
	    case NC_SHORT: base = -5; break;
	    case NC_INT: base = -20; break;
	    case NC_FLOAT: base = -9; break;
	    case NC_DOUBLE: base = -10; break;
	    default:  assert(0);
	}
	result = rank < 0 ? base * 7 : base * (rank + 1);
	for (d = 0; d < abs(rank); d++)
	    result = base * (result + index[d]);
    }
    return result;
}
#else /* USE_EXTREME_NUMBERS */
#define SANE_SHORT 3333
#define SANE_INT 2222
#define SANE_FLOAT 300.0
#define SANE_DOUBLE 1000.0

/* Generate data values as function of type, rank (-1 for attribute), index */
double
hash( const nc_type type, const int rank, const size_t *index ) 
{
    double base;
    double result;
    int  d;       /* index of dimension */

	/* If vector then elements 0 & 1 are min & max. Elements 2 & 3 are */
	/* just < min & > max (except for NC_CHAR & NC_DOUBLE) */
    if (abs(rank) == 1 && index[0] <= 3) {
	switch (index[0]) {
	    case 0:
		switch (type) {
		    case NC_CHAR:   return X_CHAR_MIN;
		    case NC_BYTE:   return X_BYTE_MIN;
		    case NC_SHORT:  return SANE_SHORT;
		    case NC_INT:   return SANE_INT;
		    case NC_FLOAT:  return SANE_FLOAT;
		    case NC_DOUBLE: return SANE_DOUBLE;
		    default:  assert(0);
		}
	    case 1:
		switch (type) {
		    case NC_CHAR:   return X_CHAR_MAX;
		    case NC_BYTE:   return X_BYTE_MAX;
		    case NC_SHORT:  return SANE_SHORT;
		    case NC_INT:   return SANE_INT;
		    case NC_FLOAT:  return SANE_FLOAT;
		    case NC_DOUBLE: return SANE_DOUBLE;
		    default:  assert(0);
		}
	    case 2:
		switch (type) {
		    case NC_CHAR:   return 'A';
		    case NC_BYTE:   return X_BYTE_MIN-1.0;
		    case NC_SHORT:  return SANE_SHORT-1.0;
		    case NC_INT:   return SANE_INT-1.0;
		    case NC_FLOAT:  return SANE_FLOAT * (1.0 + FUZZ);
		    case NC_DOUBLE: return -1.0;
		    default:  assert(0);
		}
	    case 3:
		switch (type) {
		    case NC_CHAR:   return 'Z';
		    case NC_BYTE:   return X_BYTE_MAX+1.0;
		    case NC_SHORT:  return SANE_SHORT+1.0;
		    case NC_INT:   return SANE_INT+1.0;
		    case NC_FLOAT:  return SANE_FLOAT * (1.0 + FUZZ);
		    case NC_DOUBLE: return 1.0;
		    default:  assert(0);
		}
	}
    } else {
	switch (type) {
	    case NC_CHAR: base = 2; break;
	    case NC_BYTE: base = -2; break;
	    case NC_SHORT: base = -5; break;
	    case NC_INT: base = -20; break;
	    case NC_FLOAT: base = -9; break;
	    case NC_DOUBLE: base = -10; break;
	    default:  assert(0);
	}
	result = rank < 0 ? base * 7 : base * (rank + 1);
	for (d = 0; d < abs(rank); d++)
	    result = base * (result + index[d]);
    }
    return result;
}
#endif
/* wrapper for hash to handle special NC_BYTE/uchar adjustment */
double
hash4(
    const nc_type type, 
    const int rank, 
    const size_t *index, 
    const nct_itype itype)
{
    double result;

    result = hash( type, rank, index );
    if (itype == NCT_UCHAR && type == NC_BYTE && result >= -128 && result < 0)
	result += 256;
    return result;
}

static nc_type
char2type(char letter) {
    switch (letter) {
        case 'c': return NC_CHAR;
        case 'b': return NC_BYTE;
        case 's': return NC_SHORT;
        case 'i': return NC_INT;
        case 'f': return NC_FLOAT;
        case 'd': return NC_DOUBLE;
        default:  assert(0);
    }
    return NC_CHAR;  /* Just to keep compiler happy */
}


static void
init_dims(const char *digit)
{
	int dimid;			/* index of dimension */
	for (dimid = 0; dimid < NDIMS; dimid++)
	{
		dim_len[dimid] = dimid == 0 ? NRECS : dimid;
		dim_name[dimid][0] = 'D';
		dim_name[dimid][1] = digit[dimid];
		dim_name[dimid][2] = '\0';
	}
}

static void
init_gatts(const char *type_letter)
{
	int attid;
	for (attid = 0; attid < NGATTS; attid++)
	{
		gatt_name[attid][0] = 'G';
		gatt_name[attid][1] = type_letter[attid];
		gatt_name[attid][2] = '\0';
		gatt_len[attid] = 1 + attid;
		gatt_type[attid] = char2type (type_letter[attid]);
	}
}

static size_t
product(size_t nn, const size_t *sp)
{
	size_t result = 1;
	while(nn-- > 0)
		result *= *sp++;
	return result;
}

/* 
   define global variables:
   dim_name, dim_len, 
   var_name, var_type, var_rank, var_shape, var_natts, var_dimid, var_nels
   att_name, gatt_name, att_type, gatt_type, att_len, gatt_len
 */
void
init_gvars (void)
{
	const size_t max_dim_len[MAX_RANK] = {
		MAX_DIM_LEN +1,
		MAX_DIM_LEN,
		MAX_DIM_LEN
	};
	const char type_letter[] = "cbsifd";
	const char digit[] = "r123456789";

	size_t rank;
	int vn;			/* var number */
	int xtype;		/* index of type */
	int an;			/* attribute number */

	assert(sizeof(max_dim_len)/sizeof(max_dim_len[0]) >= MAX_RANK);

	init_dims(digit);

	for (rank = 0, vn = 0, xtype = 0, an = 0;  rank <= MAX_RANK; rank++)
	{
			/* number variables of a type and rank */
		const size_t nvars = product(rank, max_dim_len);
		int jj;

		for (jj = 0; jj < nvars; jj++)
		{
				/* number types of this shape */
			const int ntypes = rank < 2 ? NTYPES : 1;

			int tc;
			for (tc = 0; tc < ntypes;
			     tc++, vn++, xtype = (xtype + 1) % NTYPES)
			{
				size_t tmp[MAX_RANK];

				var_name[vn][0] = type_letter[xtype];
				var_type[vn] = char2type (type_letter[xtype]);
				var_rank[vn] = rank;
				var_natts[vn] = rank == 0 ? vn % (MAX_NATTS + 1) : 0;
				{
					int ac;
					for (ac = 0; ac < var_natts[vn]; ac++, an++)
					{
						att_name[vn][ac][0] = type_letter[an % NTYPES];
						att_name[vn][ac][1] = '\0';
						att_len[vn][ac] = an;
						att_type[vn][ac] = char2type (type_letter[an % NTYPES]);
					}
				} /* ac block */
#ifndef NDEBUG
				assert(toMixedBase (jj, rank, max_dim_len, tmp) == 0);
#else
				(void) toMixedBase (jj, rank, max_dim_len, tmp);
#endif
				{
					int dn; /* dimension number */
					for (dn = 0; dn < rank; dn++)
						var_dimid[vn][dn] = (int)tmp[dn];
					for (dn = 0, var_nels[vn] = 1; dn < rank; dn++)
					{
						var_dimid[vn][dn] += dn > 0;
						assert (var_dimid[vn][dn] <= 9);
						var_name[vn][dn + 1] = digit[var_dimid[vn][dn]];
						var_shape[vn][dn] = var_dimid[vn][dn] ?
							var_dimid[vn][dn] : NRECS;
						var_nels[vn] *= var_shape[vn][dn];
					}
				} /* dn block */
			}
		}
	}

	init_gatts(type_letter);
}


/* define dims defined by global variables */
void                                                        
def_dims(int ncid)
{
    int  err;             /* status */
    int  i;
    int  dimid;		/* dimension id */

    for (i = 0; i < NDIMS; i++) {
	err = nc_def_dim(ncid, dim_name[i], i==0 ? NC_UNLIMITED : dim_len[i],
	    &dimid);
	IF (err) error("nc_def_dim: %s", nc_strerror(err));
    }
}


/* define vars defined by global variables */
void                                                        
def_vars(int ncid)
{
    int  err;             /* status */
    int  i;
    int var_id;

    for (i = 0; i < NVARS; i++) {
	err = nc_def_var(ncid, var_name[i], var_type[i], var_rank[i],
	    var_dimid[i], &var_id);
	IF (err) error("nc_def_var: %s", nc_strerror(err));
    }
}


/* put attributes defined by global variables */
void                                                        
put_atts(int ncid)
{
    int  err;             /* status */
    int  i;
    size_t  k;
    int  j;		/* index of attribute */
    int  allInRange;
    double att[MAX_NELS];
    char catt[MAX_NELS];

    for (i = -1; i < NVARS; i++) {
	for (j = 0; j < NATTS(i); j++) {
	    if (ATT_TYPE(i,j) == NC_CHAR) {
		for (k = 0; k < ATT_LEN(i,j); k++) {
		    catt[k] = hash(ATT_TYPE(i,j), -1, &k);
		}
		err = nc_put_att_text(ncid, i, ATT_NAME(i,j),
		    ATT_LEN(i,j), catt);
		IF (err) 
		    error("nc_put_att_text: %s", nc_strerror(err));
	    } else {
		for (allInRange = 1, k = 0; k < ATT_LEN(i,j); k++) {
		    att[k] = hash(ATT_TYPE(i,j), -1, &k);
		    allInRange = allInRange && inRange(att[k], ATT_TYPE(i,j));
		}
		err = nc_put_att_double(ncid, i, ATT_NAME(i,j),
		    ATT_TYPE(i,j), ATT_LEN(i,j), att);
                if (allInRange) {
                    IF (err)
                        error("nc_put_att_double: %s", nc_strerror(err));
                } else {
                    IF (err != NC_ERANGE)
			error("type-conversion range error: status = %d", err);
                }
	    }
        }
    }
}

/* put variables defined by global variables */
void                                                        
put_vars(int ncid)
{
    size_t start[MAX_RANK];
    size_t index[MAX_RANK];
    int  err;             /* status */
    int  i;
    size_t  j;
    double value[MAX_NELS];
    char text[MAX_NELS];
    int  allInRange;

    for (j = 0; j < MAX_RANK; j++)
	start[j] = 0;
    for (i = 0; i < NVARS; i++) {
	for (allInRange = 1, j = 0; j < var_nels[i]; j++) {
	    err = toMixedBase(j, var_rank[i], var_shape[i], index);
	    IF (err) error("toMixedBase");
	    if (var_name[i][0] == 'c') {
		text[j] = hash(var_type[i], var_rank[i], index);
	    } else {
		value[j]  = hash(var_type[i], var_rank[i], index);
		allInRange = allInRange && inRange(value[j], var_type[i]);
	    }
	}
	if (var_name[i][0] == 'c') {
	    err = nc_put_vara_text(ncid, i, start, var_shape[i], text);
	    IF (err)
		error("nc_put_vara_text: %s", nc_strerror(err));
	} else {
	    err = nc_put_vara_double(ncid, i, start, var_shape[i], value);
	    if (allInRange) {
		IF (err)
		    error("nc_put_vara_double: %s", nc_strerror(err));
	    } else {
		IF (err != NC_ERANGE)
		    error("type-conversion range error: status = %d", err);
	    }
	}
    }
}


/* Create & write all of specified file using global variables */
void
write_file(char *filename) 
{
    int  ncid;			/* netCDF id */
    int  err;		/* status */

    err = nc_create(filename, NC_CLOBBER, &ncid);
    IF (err) 
	error("nc_create: %s", nc_strerror(err));

    def_dims(ncid);
    def_vars(ncid);
    put_atts(ncid);
    err = nc_enddef(ncid);
    IF (err) 
	error("nc_enddef: %s", nc_strerror(err));
    put_vars(ncid);

    err = nc_close (ncid);
    IF (err) 
	error("nc_close: %s", nc_strerror(err));
}


/*
 * check dimensions of specified file have expected name & length
 */
void
check_dims(int  ncid)
{
    char name[NC_MAX_NAME];
    size_t length;
    int  i;
    int  err;           /* status */

    for (i = 0; i < NDIMS; i++) {
	err = nc_inq_dim(ncid, i, name, &length);
	IF (err)
	    error("nc_inq_dim: %s", nc_strerror(err));
	IF (strcmp(name, dim_name[i]) != 0)
	    error("Unexpected name of dimension %d: '%s', expected: '%s'", i, name, dim_name[i]);
	IF (length != dim_len[i])
	    error("Unexpected length %d of dimension %d, expected %zu", length, i, dim_len[i]);
    }
}


/*
 * check variables of specified file have expected name, type, shape & values
 */
void
check_vars(int  ncid)
{
    size_t index[MAX_RANK];
    int  err;		/* status */
    int  i;
    size_t  j;
    char  text;
    double value;
    nc_type datatype;
    int ndims;
    int dimids[MAX_RANK];
    int isChar;
    double expect;
    char name[NC_MAX_NAME];
    size_t length;
    int nok = 0;      /* count of valid comparisons */

    for (i = 0; i < NVARS; i++) {
        isChar = var_type[i] == NC_CHAR;
	err = nc_inq_var(ncid, i, name, &datatype, &ndims, dimids, NULL);
	IF (err) 
	    error("nc_inq_var: %s", nc_strerror(err));
	IF (strcmp(name, var_name[i]) != 0) 
	    error("Unexpected var_name");
	IF (datatype != var_type[i]) 
	    error("Unexpected type");
	IF (ndims != var_rank[i]) 
	    error("Unexpected rank");
	for (j = 0; j < ndims; j++) {
	    err = nc_inq_dim(ncid, dimids[j], 0, &length);
	    IF (err) 
		error("nc_inq_dim: %s", nc_strerror(err));
	    IF (length != var_shape[i][j]) 
		error("Unexpected shape");
	}
	for (j = 0; j < var_nels[i]; j++) {
	    err = toMixedBase(j, var_rank[i], var_shape[i], index);
	    IF (err) 
		error("error in toMixedBase 2");
	    expect = hash( var_type[i], var_rank[i], index );
	    if (isChar) {
		err = nc_get_var1_text(ncid, i, index, &text);
		IF (err)
		    error("nc_get_var1_text: %s", nc_strerror(err));
		IF (text != expect) {
		    error("Var %s value read 0x%02x not that expected 0x%02x ",
			var_name[i], text, (char)expect);
			print_n_size_t(var_rank[i], index);
		} else {
#if 0
			print("\nOk %s ", var_name[i]);
			print_n_size_t(var_rank[i], index);
#endif
		    nok++;
		}
	    } else {
		err = nc_get_var1_double(ncid, i, index, &value);
		if (inRange(expect,var_type[i])) {
		    IF (err) {
			error("nc_get_var1_double: %s", nc_strerror(err));
		    } else {
			IF (!equal(value,expect,var_type[i], NCT_DOUBLE)) {
	error("Var %s value read % 12.5e not that expected % 12.7e ",
		var_name[i], value, expect);
		print_n_size_t(var_rank[i], index);
			} else {
#if 0
			print("\nOk %s ", var_name[i]);
			print_n_size_t(var_rank[i], index);
#endif
			    nok++;
			}
		    }
		}
	    }
	}
    }
    print_nok(nok);
}


/*
 * check attributes of specified file have expected name, type, length & values
 */
void
check_atts(int  ncid) 
{
    int  err;		/* status */
    int  i;
    int  j;
    size_t  k;
    nc_type datatype;
    char name[NC_MAX_NAME];
    size_t length;
    char  text[MAX_NELS];
    double value[MAX_NELS];
    double expect;
    int nok = 0;      /* count of valid comparisons */

    for (i = -1; i < NVARS; i++) {
	for (j = 0; j < NATTS(i); j++) {
            err = nc_inq_attname(ncid, i, j, name);
            IF (err) 
                error("nc_inq_attname: %s", nc_strerror(err));
            IF (strcmp(name, ATT_NAME(i,j)) != 0)
                error("nc_inq_attname: unexpected name");
	    err = nc_inq_att(ncid, i, name, &datatype, &length);
	    IF (err) 
		error("nc_inq_att: %s", nc_strerror(err));
	    IF (datatype != ATT_TYPE(i,j))
		error("nc_inq_att: unexpected type");
	    IF (length != ATT_LEN(i,j))
		error("nc_inq_att: unexpected length");
	    if (datatype == NC_CHAR) {
		err = nc_get_att_text(ncid, i, name, text);
		IF (err)
		    error("nc_get_att_text: %s", nc_strerror(err));
		for (k = 0; k < ATT_LEN(i,j); k++) {
		    IF (text[k] != hash(datatype, -1, &k)) {
			error("nc_get_att_text: unexpected value");
                    } else {
                        nok++;
                    }
		}
	    } else {
		err = nc_get_att_double(ncid, i, name, value);
		for (k = 0; k < ATT_LEN(i,j); k++) {
		    expect = hash(datatype, -1, &k);
		    if (inRange(expect,ATT_TYPE(i,j))) {
			IF (err)
			    error("nc_get_att_double: %s", nc_strerror(err));
			IF (!equal(value[k], expect, ATT_TYPE(i,j), NCT_DOUBLE)) {
			    error("Att value read not that expected");
			} else {
			    nok++;
			}
		    }
		}
	    }
	}
    }
    print_nok(nok);
}


/* Check file (dims, vars, atts) corresponds to global variables */
void
check_file(char *filename) 
{
    int  ncid;		/* netCDF id */
    int  err;		/* status */

    err = nc_open(filename, NC_NOWRITE, &ncid);
    IF (err) {
        error("nc_open: %s", nc_strerror(err));
    } else {
	check_dims(ncid);
	check_vars(ncid);
	check_atts(ncid);
	err = nc_close (ncid);
	IF (err) 
	    error("nc_close: %s", nc_strerror(err));
    }
}

/* TODO: Maybe this function belongs in the netcdf library. */
const char *
s_nc_type(nc_type type)
{
	switch((int)type){
	case NC_BYTE:
		return "NC_BYTE";
	case NC_CHAR:
		return "NC_CHAR";
	case NC_SHORT:
		return "NC_SHORT";
	case NC_INT:
		return "NC_INT";
	case NC_FLOAT:
		return "NC_FLOAT";
	case NC_DOUBLE:
		return "NC_DOUBLE";
	}
	return "";
}
