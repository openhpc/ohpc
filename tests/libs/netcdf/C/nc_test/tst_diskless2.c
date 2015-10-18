#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>


#define Clear ((unsigned char)0)
#define Cumulonimbus ((unsigned char)1)
#define Stratus ((unsigned char)2)
typedef unsigned char enum_t;
typedef unsigned char opaque_t[11];
typedef nc_vlen_t vlen_t;
typedef struct g_cmpd_t {
    vlen_t f1;
    enum_t f2;
} g_cmpd_t;


void
check_err(const int stat, const int line, const char *file) {
    if (stat != NC_NOERR) {
        (void)fprintf(stderr,"line %d of %s: %s\n", line, file, nc_strerror(stat));
        fflush(stderr);
        exit(1);
    }
}

int
main() {/* create tst_diskless2.nc */

    int  stat;  /* return status */
    int  ncid;  /* netCDF id */

    /* group ids */
    int root_grp;
    int g_grp;
    int h_grp;

    /* type ids */
    int enum_t_typ;
    int opaque_t_typ;
    int vlen_t_typ;
    int g_cmpd_t_typ;

    /* dimension ids */
    int lat_dim;
    int lon_dim;
    int time_dim;

    /* dimension lengths */
    size_t lat_len = 10;
    size_t lon_len = 5;
    size_t time_len = NC_UNLIMITED;

    /* variable ids */
    int lat_id;
    int lon_id;
    int time_id;
    int Z_id;
    int t_id;
    int p_id;
    int rh_id;
    int country_id;
    int tag_id;
    int h_compoundvar_id;

    /* rank (number of dimensions) for each variable */
#   define RANK_lat 1
#   define RANK_lon 1
#   define RANK_time 1
#   define RANK_Z 3
#   define RANK_t 3
#   define RANK_p 3
#   define RANK_rh 3
#   define RANK_country 3
#   define RANK_tag 0
#   define RANK_h_compoundvar 0

    /* variable shapes */
    int lat_dims[RANK_lat];
    int lon_dims[RANK_lon];
    int time_dims[RANK_time];
    int Z_dims[RANK_Z];
    int t_dims[RANK_t];
    int p_dims[RANK_p];
    int rh_dims[RANK_rh];
    int country_dims[RANK_country];

    /* enter define mode */
    stat = nc_create("tst_diskless2.nc", NC_DISKLESS|NC_WRITE|NC_CLOBBER|NC_NETCDF4, &ncid);
    check_err(stat,__LINE__,__FILE__);
    root_grp = ncid;
    stat = nc_def_grp(root_grp, "g", &g_grp);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_grp(root_grp, "h", &h_grp);
    check_err(stat,__LINE__,__FILE__);

    {
    unsigned char econst;
    stat = nc_def_enum(root_grp, NC_UBYTE, "enum_t", &enum_t_typ);
    check_err(stat,__LINE__,__FILE__);
    econst = 0;
    stat = nc_insert_enum(root_grp, enum_t_typ, "Clear", &econst);
    check_err(stat,__LINE__,__FILE__);
    econst = 1;
    stat = nc_insert_enum(root_grp, enum_t_typ, "Cumulonimbus", &econst);
    check_err(stat,__LINE__,__FILE__);
    econst = 2;
    stat = nc_insert_enum(root_grp, enum_t_typ, "Stratus", &econst);
    check_err(stat,__LINE__,__FILE__);
    }

    stat = nc_def_opaque(root_grp, 11, "opaque_t", &opaque_t_typ);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_vlen(root_grp, "vlen_t", NC_INT, &vlen_t_typ);    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_compound(g_grp, sizeof(g_cmpd_t), "cmpd_t", &g_cmpd_t_typ);    check_err(stat,__LINE__,__FILE__);
    {
    stat = nc_insert_compound(g_grp, g_cmpd_t_typ, "f1", NC_COMPOUND_OFFSET(g_cmpd_t,f1), vlen_t_typ);    check_err(stat,__LINE__,__FILE__);
    stat = nc_insert_compound(g_grp, g_cmpd_t_typ, "f2", NC_COMPOUND_OFFSET(g_cmpd_t,f2), enum_t_typ);    check_err(stat,__LINE__,__FILE__);
    }


    /* define dimensions */
    stat = nc_def_dim(root_grp, "lat", lat_len, &lat_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(root_grp, "lon", lon_len, &lon_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(root_grp, "time", time_len, &time_dim);
    check_err(stat,__LINE__,__FILE__);

    /* define variables */

    lat_dims[0] = lat_dim;
    stat = nc_def_var(root_grp, "lat", NC_INT, RANK_lat, lat_dims, &lat_id);
    check_err(stat,__LINE__,__FILE__);

    lon_dims[0] = lon_dim;
    stat = nc_def_var(root_grp, "lon", NC_INT, RANK_lon, lon_dims, &lon_id);
    check_err(stat,__LINE__,__FILE__);

    time_dims[0] = time_dim;
    stat = nc_def_var(root_grp, "time", NC_INT, RANK_time, time_dims, &time_id);
    check_err(stat,__LINE__,__FILE__);

    Z_dims[0] = time_dim;
    Z_dims[1] = lat_dim;
    Z_dims[2] = lon_dim;
    stat = nc_def_var(root_grp, "Z", NC_FLOAT, RANK_Z, Z_dims, &Z_id);
    check_err(stat,__LINE__,__FILE__);

    t_dims[0] = time_dim;
    t_dims[1] = lat_dim;
    t_dims[2] = lon_dim;
    stat = nc_def_var(root_grp, "t", NC_FLOAT, RANK_t, t_dims, &t_id);
    check_err(stat,__LINE__,__FILE__);

    p_dims[0] = time_dim;
    p_dims[1] = lat_dim;
    p_dims[2] = lon_dim;
    stat = nc_def_var(root_grp, "p", NC_DOUBLE, RANK_p, p_dims, &p_id);
    check_err(stat,__LINE__,__FILE__);

    rh_dims[0] = time_dim;
    rh_dims[1] = lat_dim;
    rh_dims[2] = lon_dim;
    stat = nc_def_var(root_grp, "rh", NC_INT, RANK_rh, rh_dims, &rh_id);
    check_err(stat,__LINE__,__FILE__);

    country_dims[0] = time_dim;
    country_dims[1] = lat_dim;
    country_dims[2] = lon_dim;
    stat = nc_def_var(root_grp, "country", NC_STRING, RANK_country, country_dims, &country_id);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(root_grp, "tag", NC_UBYTE, RANK_tag, 0, &tag_id);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(h_grp, "compoundvar", g_cmpd_t_typ, RANK_h_compoundvar, 0, &h_compoundvar_id);
    check_err(stat,__LINE__,__FILE__);

    /* assign global attributes */

    {
    static const int vlen_2[] = {17, 18, 19} ;
    static const vlen_t globalatt_att[1] = {{3, (void*)vlen_2}} ;
    stat = nc_put_att(root_grp, NC_GLOBAL, "globalatt", vlen_t_typ, 1, globalatt_att);
    check_err(stat,__LINE__,__FILE__);
    }


    /* assign per-variable attributes */

    {
    stat = nc_put_att_text(root_grp, lat_id, "long_name", 8, "latitude");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(root_grp, lat_id, "units", 13, "degrees_north");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(root_grp, lon_id, "long_name", 9, "longitude");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(root_grp, lon_id, "units", 12, "degrees_east");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(root_grp, time_id, "units", 31, "seconds since 1992-1-1 00:00:00");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const char* Z_units_att[1] = {"geopotential meters"} ;
    stat = nc_put_att_string(root_grp, Z_id, "units", 1, Z_units_att);    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const float Z_valid_range_att[2] = {((float)0), ((float)5000)} ;
    stat = nc_put_att_float(root_grp, Z_id, "valid_range", NC_FLOAT, 2, Z_valid_range_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double p_FillValue_att[1] = {((double)-9999)} ;
    stat = nc_put_att_double(root_grp, p_id, "_FillValue", NC_DOUBLE, 1, p_FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const int rh_FillValue_att[1] = {-1} ;
    stat = nc_put_att_int(root_grp, rh_id, "_FillValue", NC_INT, 1, rh_FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }


    /* leave define mode */
    stat = nc_enddef (root_grp);
    check_err(stat,__LINE__,__FILE__);

    /* assign variable data */

    {
    int lat_data[10] = {0, 10, 20, 30, 40, 50, 60, 70, 80, 90} ;
    size_t lat_startset[1] = {0} ;
    size_t lat_countset[1] = {10};
    stat = nc_put_vara(root_grp, lat_id, lat_startset, lat_countset, lat_data);
    check_err(stat,__LINE__,__FILE__);
    }


    {
    int lon_data[5] = {-140, -118, -96, -84, -52} ;
    size_t lon_startset[1] = {0} ;
    size_t lon_countset[1] = {5};
    stat = nc_put_vara(root_grp, lon_id, lon_startset, lon_countset, lon_data);
    check_err(stat,__LINE__,__FILE__);
    }


    {
    static const int vlen_10[] = {3, 4, 5} ;
    size_t zero = 0;
    static g_cmpd_t h_compoundvar_data[1] = {{{3, (void*)vlen_10}, 2}};
    stat = nc_put_var1(h_grp, h_compoundvar_id, &zero, h_compoundvar_data);
    check_err(stat,__LINE__,__FILE__);
    }


    stat = nc_close(root_grp);
    check_err(stat,__LINE__,__FILE__);
    return 0;
}
