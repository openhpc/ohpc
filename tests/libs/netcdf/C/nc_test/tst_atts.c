/* This is part of the netCDF package.
   Copyright 2006 University Corporation for Atmospheric Research/Unidata.
   See COPYRIGHT file for conditions of use.

   This is a very simple example which writes a netCDF file with
   Unicode names encoded with UTF-8. It is the NETCDF3 equivalent
   of tst_unicode.c

   $Id$
*/

#include <nc_tests.h>
#include <stdlib.h>
#include <string.h>

/* The data file we will create. */
#define FILE_NAME "tst_atts.nc"

void
check_err(const int stat, const int line, const char *file) {
   if (stat != NC_NOERR) {
      (void)fprintf(stderr,"line %d of %s: %s\n", line, file, nc_strerror(stat));
      fflush(stderr);
      exit(1);
   }
}

int
create_file()
{
    int  stat;  /* return status */
    int  ncid;  /* netCDF id */

    /* dimension ids */
    int Dr_dim;
    int D1_dim;
    int D2_dim;
    int D3_dim;
    int D4_dim;

    /* dimension lengths */
    size_t Dr_len = NC_UNLIMITED;
    size_t D1_len = 1;
    size_t D2_len = 2;
    size_t D3_len = 3;
    size_t D4_len = 4;

    /* variable ids */
    int c_id;
    int b_id;
    int s_id;
    int i_id;
    int f_id;
    int d_id;
    int cr_id;
    int br_id;
    int sr_id;
    int ir_id;
    int fr_id;
    int dr_id;
    int c1_id;
    int b1_id;
    int s1_id;
    int i1_id;
    int f1_id;
    int d1_id;
    int c2_id;
    int b2_id;
    int s2_id;
    int i2_id;
    int f2_id;
    int d2_id;
    int c3_id;
    int b3_id;
    int s3_id;
    int i3_id;
    int f3_id;
    int d3_id;
    int c4_id;
    int b4_id;
    int s4_id;
    int i4_id;
    int f4_id;
    int d4_id;
    int cr1_id;
    int br2_id;
    int sr3_id;
    int ir4_id;
    int f11_id;
    int d12_id;
    int c13_id;
    int b14_id;
    int s21_id;
    int i22_id;
    int f23_id;
    int d24_id;
    int c31_id;
    int b32_id;
    int s33_id;
    int i34_id;
    int f41_id;
    int d42_id;
    int c43_id;
    int b44_id;
    int sr11_id;
    int ir12_id;
    int fr13_id;
    int dr14_id;
    int cr21_id;
    int br22_id;
    int sr23_id;
    int ir24_id;
    int fr31_id;
    int dr32_id;
    int cr33_id;
    int br34_id;
    int sr41_id;
    int ir42_id;
    int fr43_id;
    int dr44_id;
    int c111_id;
    int b112_id;
    int s113_id;
    int i114_id;
    int f121_id;
    int d122_id;
    int c123_id;
    int b124_id;
    int s131_id;
    int i132_id;
    int f133_id;
    int d134_id;
    int c141_id;
    int b142_id;
    int s143_id;
    int i144_id;
    int f211_id;
    int d212_id;
    int c213_id;
    int b214_id;
    int s221_id;
    int i222_id;
    int f223_id;
    int d224_id;
    int c231_id;
    int b232_id;
    int s233_id;
    int i234_id;
    int f241_id;
    int d242_id;
    int c243_id;
    int b244_id;
    int s311_id;
    int i312_id;
    int f313_id;
    int d314_id;
    int c321_id;
    int b322_id;
    int s323_id;
    int i324_id;
    int f331_id;
    int d332_id;
    int c333_id;
    int b334_id;
    int s341_id;
    int i342_id;
    int f343_id;
    int d344_id;
    int c411_id;
    int b412_id;
    int s413_id;
    int i414_id;
    int f421_id;
    int d422_id;
    int c423_id;
    int b424_id;
    int s431_id;
    int i432_id;
    int f433_id;
    int d434_id;
    int c441_id;
    int b442_id;
    int s443_id;
    int i444_id;

    /* rank (number of dimensions) for each variable */
#   define RANK_c 0
#   define RANK_b 0
#   define RANK_s 0
#   define RANK_i 0
#   define RANK_f 0
#   define RANK_d 0
#   define RANK_cr 1
#   define RANK_br 1
#   define RANK_sr 1
#   define RANK_ir 1
#   define RANK_fr 1
#   define RANK_dr 1
#   define RANK_c1 1
#   define RANK_b1 1
#   define RANK_s1 1
#   define RANK_i1 1
#   define RANK_f1 1
#   define RANK_d1 1
#   define RANK_c2 1
#   define RANK_b2 1
#   define RANK_s2 1
#   define RANK_i2 1
#   define RANK_f2 1
#   define RANK_d2 1
#   define RANK_c3 1
#   define RANK_b3 1
#   define RANK_s3 1
#   define RANK_i3 1
#   define RANK_f3 1
#   define RANK_d3 1
#   define RANK_c4 1
#   define RANK_b4 1
#   define RANK_s4 1
#   define RANK_i4 1
#   define RANK_f4 1
#   define RANK_d4 1
#   define RANK_cr1 2
#   define RANK_br2 2
#   define RANK_sr3 2
#   define RANK_ir4 2
#   define RANK_f11 2
#   define RANK_d12 2
#   define RANK_c13 2
#   define RANK_b14 2
#   define RANK_s21 2
#   define RANK_i22 2
#   define RANK_f23 2
#   define RANK_d24 2
#   define RANK_c31 2
#   define RANK_b32 2
#   define RANK_s33 2
#   define RANK_i34 2
#   define RANK_f41 2
#   define RANK_d42 2
#   define RANK_c43 2
#   define RANK_b44 2
#   define RANK_sr11 3
#   define RANK_ir12 3
#   define RANK_fr13 3
#   define RANK_dr14 3
#   define RANK_cr21 3
#   define RANK_br22 3
#   define RANK_sr23 3
#   define RANK_ir24 3
#   define RANK_fr31 3
#   define RANK_dr32 3
#   define RANK_cr33 3
#   define RANK_br34 3
#   define RANK_sr41 3
#   define RANK_ir42 3
#   define RANK_fr43 3
#   define RANK_dr44 3
#   define RANK_c111 3
#   define RANK_b112 3
#   define RANK_s113 3
#   define RANK_i114 3
#   define RANK_f121 3
#   define RANK_d122 3
#   define RANK_c123 3
#   define RANK_b124 3
#   define RANK_s131 3
#   define RANK_i132 3
#   define RANK_f133 3
#   define RANK_d134 3
#   define RANK_c141 3
#   define RANK_b142 3
#   define RANK_s143 3
#   define RANK_i144 3
#   define RANK_f211 3
#   define RANK_d212 3
#   define RANK_c213 3
#   define RANK_b214 3
#   define RANK_s221 3
#   define RANK_i222 3
#   define RANK_f223 3
#   define RANK_d224 3
#   define RANK_c231 3
#   define RANK_b232 3
#   define RANK_s233 3
#   define RANK_i234 3
#   define RANK_f241 3
#   define RANK_d242 3
#   define RANK_c243 3
#   define RANK_b244 3
#   define RANK_s311 3
#   define RANK_i312 3
#   define RANK_f313 3
#   define RANK_d314 3
#   define RANK_c321 3
#   define RANK_b322 3
#   define RANK_s323 3
#   define RANK_i324 3
#   define RANK_f331 3
#   define RANK_d332 3
#   define RANK_c333 3
#   define RANK_b334 3
#   define RANK_s341 3
#   define RANK_i342 3
#   define RANK_f343 3
#   define RANK_d344 3
#   define RANK_c411 3
#   define RANK_b412 3
#   define RANK_s413 3
#   define RANK_i414 3
#   define RANK_f421 3
#   define RANK_d422 3
#   define RANK_c423 3
#   define RANK_b424 3
#   define RANK_s431 3
#   define RANK_i432 3
#   define RANK_f433 3
#   define RANK_d434 3
#   define RANK_c441 3
#   define RANK_b442 3
#   define RANK_s443 3
#   define RANK_i444 3

    /* variable shapes */
    int cr_dims[RANK_cr];
    int br_dims[RANK_br];
    int sr_dims[RANK_sr];
    int ir_dims[RANK_ir];
    int fr_dims[RANK_fr];
    int dr_dims[RANK_dr];
    int c1_dims[RANK_c1];
    int b1_dims[RANK_b1];
    int s1_dims[RANK_s1];
    int i1_dims[RANK_i1];
    int f1_dims[RANK_f1];
    int d1_dims[RANK_d1];
    int c2_dims[RANK_c2];
    int b2_dims[RANK_b2];
    int s2_dims[RANK_s2];
    int i2_dims[RANK_i2];
    int f2_dims[RANK_f2];
    int d2_dims[RANK_d2];
    int c3_dims[RANK_c3];
    int b3_dims[RANK_b3];
    int s3_dims[RANK_s3];
    int i3_dims[RANK_i3];
    int f3_dims[RANK_f3];
    int d3_dims[RANK_d3];
    int c4_dims[RANK_c4];
    int b4_dims[RANK_b4];
    int s4_dims[RANK_s4];
    int i4_dims[RANK_i4];
    int f4_dims[RANK_f4];
    int d4_dims[RANK_d4];
    int cr1_dims[RANK_cr1];
    int br2_dims[RANK_br2];
    int sr3_dims[RANK_sr3];
    int ir4_dims[RANK_ir4];
    int f11_dims[RANK_f11];
    int d12_dims[RANK_d12];
    int c13_dims[RANK_c13];
    int b14_dims[RANK_b14];
    int s21_dims[RANK_s21];
    int i22_dims[RANK_i22];
    int f23_dims[RANK_f23];
    int d24_dims[RANK_d24];
    int c31_dims[RANK_c31];
    int b32_dims[RANK_b32];
    int s33_dims[RANK_s33];
    int i34_dims[RANK_i34];
    int f41_dims[RANK_f41];
    int d42_dims[RANK_d42];
    int c43_dims[RANK_c43];
    int b44_dims[RANK_b44];
    int sr11_dims[RANK_sr11];
    int ir12_dims[RANK_ir12];
    int fr13_dims[RANK_fr13];
    int dr14_dims[RANK_dr14];
    int cr21_dims[RANK_cr21];
    int br22_dims[RANK_br22];
    int sr23_dims[RANK_sr23];
    int ir24_dims[RANK_ir24];
    int fr31_dims[RANK_fr31];
    int dr32_dims[RANK_dr32];
    int cr33_dims[RANK_cr33];
    int br34_dims[RANK_br34];
    int sr41_dims[RANK_sr41];
    int ir42_dims[RANK_ir42];
    int fr43_dims[RANK_fr43];
    int dr44_dims[RANK_dr44];
    int c111_dims[RANK_c111];
    int b112_dims[RANK_b112];
    int s113_dims[RANK_s113];
    int i114_dims[RANK_i114];
    int f121_dims[RANK_f121];
    int d122_dims[RANK_d122];
    int c123_dims[RANK_c123];
    int b124_dims[RANK_b124];
    int s131_dims[RANK_s131];
    int i132_dims[RANK_i132];
    int f133_dims[RANK_f133];
    int d134_dims[RANK_d134];
    int c141_dims[RANK_c141];
    int b142_dims[RANK_b142];
    int s143_dims[RANK_s143];
    int i144_dims[RANK_i144];
    int f211_dims[RANK_f211];
    int d212_dims[RANK_d212];
    int c213_dims[RANK_c213];
    int b214_dims[RANK_b214];
    int s221_dims[RANK_s221];
    int i222_dims[RANK_i222];
    int f223_dims[RANK_f223];
    int d224_dims[RANK_d224];
    int c231_dims[RANK_c231];
    int b232_dims[RANK_b232];
    int s233_dims[RANK_s233];
    int i234_dims[RANK_i234];
    int f241_dims[RANK_f241];
    int d242_dims[RANK_d242];
    int c243_dims[RANK_c243];
    int b244_dims[RANK_b244];
    int s311_dims[RANK_s311];
    int i312_dims[RANK_i312];
    int f313_dims[RANK_f313];
    int d314_dims[RANK_d314];
    int c321_dims[RANK_c321];
    int b322_dims[RANK_b322];
    int s323_dims[RANK_s323];
    int i324_dims[RANK_i324];
    int f331_dims[RANK_f331];
    int d332_dims[RANK_d332];
    int c333_dims[RANK_c333];
    int b334_dims[RANK_b334];
    int s341_dims[RANK_s341];
    int i342_dims[RANK_i342];
    int f343_dims[RANK_f343];
    int d344_dims[RANK_d344];
    int c411_dims[RANK_c411];
    int b412_dims[RANK_b412];
    int s413_dims[RANK_s413];
    int i414_dims[RANK_i414];
    int f421_dims[RANK_f421];
    int d422_dims[RANK_d422];
    int c423_dims[RANK_c423];
    int b424_dims[RANK_b424];
    int s431_dims[RANK_s431];
    int i432_dims[RANK_i432];
    int f433_dims[RANK_f433];
    int d434_dims[RANK_d434];
    int c441_dims[RANK_c441];
    int b442_dims[RANK_b442];
    int s443_dims[RANK_s443];
    int i444_dims[RANK_i444];

    /* enter define mode */
    stat = nc_create(FILE_NAME, NC_CLOBBER, &ncid);
    check_err(stat,__LINE__,__FILE__);

    /* define dimensions */
    stat = nc_def_dim(ncid, "Dr", Dr_len, &Dr_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(ncid, "D1", D1_len, &D1_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(ncid, "D2", D2_len, &D2_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(ncid, "D3", D3_len, &D3_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(ncid, "D4", D4_len, &D4_dim);
    check_err(stat,__LINE__,__FILE__);

    /* define variables */

    stat = nc_def_var(ncid, "c", NC_CHAR, RANK_c, 0, &c_id);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(ncid, "b", NC_BYTE, RANK_b, 0, &b_id);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(ncid, "s", NC_SHORT, RANK_s, 0, &s_id);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(ncid, "i", NC_INT, RANK_i, 0, &i_id);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(ncid, "f", NC_FLOAT, RANK_f, 0, &f_id);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(ncid, "d", NC_DOUBLE, RANK_d, 0, &d_id);
    check_err(stat,__LINE__,__FILE__);

    cr_dims[0] = Dr_dim;
    stat = nc_def_var(ncid, "cr", NC_CHAR, RANK_cr, cr_dims, &cr_id);
    check_err(stat,__LINE__,__FILE__);

    br_dims[0] = Dr_dim;
    stat = nc_def_var(ncid, "br", NC_BYTE, RANK_br, br_dims, &br_id);
    check_err(stat,__LINE__,__FILE__);

    sr_dims[0] = Dr_dim;
    stat = nc_def_var(ncid, "sr", NC_SHORT, RANK_sr, sr_dims, &sr_id);
    check_err(stat,__LINE__,__FILE__);

    ir_dims[0] = Dr_dim;
    stat = nc_def_var(ncid, "ir", NC_INT, RANK_ir, ir_dims, &ir_id);
    check_err(stat,__LINE__,__FILE__);

    fr_dims[0] = Dr_dim;
    stat = nc_def_var(ncid, "fr", NC_FLOAT, RANK_fr, fr_dims, &fr_id);
    check_err(stat,__LINE__,__FILE__);

    dr_dims[0] = Dr_dim;
    stat = nc_def_var(ncid, "dr", NC_DOUBLE, RANK_dr, dr_dims, &dr_id);
    check_err(stat,__LINE__,__FILE__);

    c1_dims[0] = D1_dim;
    stat = nc_def_var(ncid, "c1", NC_CHAR, RANK_c1, c1_dims, &c1_id);
    check_err(stat,__LINE__,__FILE__);

    b1_dims[0] = D1_dim;
    stat = nc_def_var(ncid, "b1", NC_BYTE, RANK_b1, b1_dims, &b1_id);
    check_err(stat,__LINE__,__FILE__);

    s1_dims[0] = D1_dim;
    stat = nc_def_var(ncid, "s1", NC_SHORT, RANK_s1, s1_dims, &s1_id);
    check_err(stat,__LINE__,__FILE__);

    i1_dims[0] = D1_dim;
    stat = nc_def_var(ncid, "i1", NC_INT, RANK_i1, i1_dims, &i1_id);
    check_err(stat,__LINE__,__FILE__);

    f1_dims[0] = D1_dim;
    stat = nc_def_var(ncid, "f1", NC_FLOAT, RANK_f1, f1_dims, &f1_id);
    check_err(stat,__LINE__,__FILE__);

    d1_dims[0] = D1_dim;
    stat = nc_def_var(ncid, "d1", NC_DOUBLE, RANK_d1, d1_dims, &d1_id);
    check_err(stat,__LINE__,__FILE__);

    c2_dims[0] = D2_dim;
    stat = nc_def_var(ncid, "c2", NC_CHAR, RANK_c2, c2_dims, &c2_id);
    check_err(stat,__LINE__,__FILE__);

    b2_dims[0] = D2_dim;
    stat = nc_def_var(ncid, "b2", NC_BYTE, RANK_b2, b2_dims, &b2_id);
    check_err(stat,__LINE__,__FILE__);

    s2_dims[0] = D2_dim;
    stat = nc_def_var(ncid, "s2", NC_SHORT, RANK_s2, s2_dims, &s2_id);
    check_err(stat,__LINE__,__FILE__);

    i2_dims[0] = D2_dim;
    stat = nc_def_var(ncid, "i2", NC_INT, RANK_i2, i2_dims, &i2_id);
    check_err(stat,__LINE__,__FILE__);

    f2_dims[0] = D2_dim;
    stat = nc_def_var(ncid, "f2", NC_FLOAT, RANK_f2, f2_dims, &f2_id);
    check_err(stat,__LINE__,__FILE__);

    d2_dims[0] = D2_dim;
    stat = nc_def_var(ncid, "d2", NC_DOUBLE, RANK_d2, d2_dims, &d2_id);
    check_err(stat,__LINE__,__FILE__);

    c3_dims[0] = D3_dim;
    stat = nc_def_var(ncid, "c3", NC_CHAR, RANK_c3, c3_dims, &c3_id);
    check_err(stat,__LINE__,__FILE__);

    b3_dims[0] = D3_dim;
    stat = nc_def_var(ncid, "b3", NC_BYTE, RANK_b3, b3_dims, &b3_id);
    check_err(stat,__LINE__,__FILE__);

    s3_dims[0] = D3_dim;
    stat = nc_def_var(ncid, "s3", NC_SHORT, RANK_s3, s3_dims, &s3_id);
    check_err(stat,__LINE__,__FILE__);

    i3_dims[0] = D3_dim;
    stat = nc_def_var(ncid, "i3", NC_INT, RANK_i3, i3_dims, &i3_id);
    check_err(stat,__LINE__,__FILE__);

    f3_dims[0] = D3_dim;
    stat = nc_def_var(ncid, "f3", NC_FLOAT, RANK_f3, f3_dims, &f3_id);
    check_err(stat,__LINE__,__FILE__);

    d3_dims[0] = D3_dim;
    stat = nc_def_var(ncid, "d3", NC_DOUBLE, RANK_d3, d3_dims, &d3_id);
    check_err(stat,__LINE__,__FILE__);

    c4_dims[0] = D4_dim;
    stat = nc_def_var(ncid, "c4", NC_CHAR, RANK_c4, c4_dims, &c4_id);
    check_err(stat,__LINE__,__FILE__);

    b4_dims[0] = D4_dim;
    stat = nc_def_var(ncid, "b4", NC_BYTE, RANK_b4, b4_dims, &b4_id);
    check_err(stat,__LINE__,__FILE__);

    s4_dims[0] = D4_dim;
    stat = nc_def_var(ncid, "s4", NC_SHORT, RANK_s4, s4_dims, &s4_id);
    check_err(stat,__LINE__,__FILE__);

    i4_dims[0] = D4_dim;
    stat = nc_def_var(ncid, "i4", NC_INT, RANK_i4, i4_dims, &i4_id);
    check_err(stat,__LINE__,__FILE__);

    f4_dims[0] = D4_dim;
    stat = nc_def_var(ncid, "f4", NC_FLOAT, RANK_f4, f4_dims, &f4_id);
    check_err(stat,__LINE__,__FILE__);

    d4_dims[0] = D4_dim;
    stat = nc_def_var(ncid, "d4", NC_DOUBLE, RANK_d4, d4_dims, &d4_id);
    check_err(stat,__LINE__,__FILE__);

    cr1_dims[0] = Dr_dim;
    cr1_dims[1] = D1_dim;
    stat = nc_def_var(ncid, "cr1", NC_CHAR, RANK_cr1, cr1_dims, &cr1_id);
    check_err(stat,__LINE__,__FILE__);

    br2_dims[0] = Dr_dim;
    br2_dims[1] = D2_dim;
    stat = nc_def_var(ncid, "br2", NC_BYTE, RANK_br2, br2_dims, &br2_id);
    check_err(stat,__LINE__,__FILE__);

    sr3_dims[0] = Dr_dim;
    sr3_dims[1] = D3_dim;
    stat = nc_def_var(ncid, "sr3", NC_SHORT, RANK_sr3, sr3_dims, &sr3_id);
    check_err(stat,__LINE__,__FILE__);

    ir4_dims[0] = Dr_dim;
    ir4_dims[1] = D4_dim;
    stat = nc_def_var(ncid, "ir4", NC_INT, RANK_ir4, ir4_dims, &ir4_id);
    check_err(stat,__LINE__,__FILE__);

    f11_dims[0] = D1_dim;
    f11_dims[1] = D1_dim;
    stat = nc_def_var(ncid, "f11", NC_FLOAT, RANK_f11, f11_dims, &f11_id);
    check_err(stat,__LINE__,__FILE__);

    d12_dims[0] = D1_dim;
    d12_dims[1] = D2_dim;
    stat = nc_def_var(ncid, "d12", NC_DOUBLE, RANK_d12, d12_dims, &d12_id);
    check_err(stat,__LINE__,__FILE__);

    c13_dims[0] = D1_dim;
    c13_dims[1] = D3_dim;
    stat = nc_def_var(ncid, "c13", NC_CHAR, RANK_c13, c13_dims, &c13_id);
    check_err(stat,__LINE__,__FILE__);

    b14_dims[0] = D1_dim;
    b14_dims[1] = D4_dim;
    stat = nc_def_var(ncid, "b14", NC_BYTE, RANK_b14, b14_dims, &b14_id);
    check_err(stat,__LINE__,__FILE__);

    s21_dims[0] = D2_dim;
    s21_dims[1] = D1_dim;
    stat = nc_def_var(ncid, "s21", NC_SHORT, RANK_s21, s21_dims, &s21_id);
    check_err(stat,__LINE__,__FILE__);

    i22_dims[0] = D2_dim;
    i22_dims[1] = D2_dim;
    stat = nc_def_var(ncid, "i22", NC_INT, RANK_i22, i22_dims, &i22_id);
    check_err(stat,__LINE__,__FILE__);

    f23_dims[0] = D2_dim;
    f23_dims[1] = D3_dim;
    stat = nc_def_var(ncid, "f23", NC_FLOAT, RANK_f23, f23_dims, &f23_id);
    check_err(stat,__LINE__,__FILE__);

    d24_dims[0] = D2_dim;
    d24_dims[1] = D4_dim;
    stat = nc_def_var(ncid, "d24", NC_DOUBLE, RANK_d24, d24_dims, &d24_id);
    check_err(stat,__LINE__,__FILE__);

    c31_dims[0] = D3_dim;
    c31_dims[1] = D1_dim;
    stat = nc_def_var(ncid, "c31", NC_CHAR, RANK_c31, c31_dims, &c31_id);
    check_err(stat,__LINE__,__FILE__);

    b32_dims[0] = D3_dim;
    b32_dims[1] = D2_dim;
    stat = nc_def_var(ncid, "b32", NC_BYTE, RANK_b32, b32_dims, &b32_id);
    check_err(stat,__LINE__,__FILE__);

    s33_dims[0] = D3_dim;
    s33_dims[1] = D3_dim;
    stat = nc_def_var(ncid, "s33", NC_SHORT, RANK_s33, s33_dims, &s33_id);
    check_err(stat,__LINE__,__FILE__);

    i34_dims[0] = D3_dim;
    i34_dims[1] = D4_dim;
    stat = nc_def_var(ncid, "i34", NC_INT, RANK_i34, i34_dims, &i34_id);
    check_err(stat,__LINE__,__FILE__);

    f41_dims[0] = D4_dim;
    f41_dims[1] = D1_dim;
    stat = nc_def_var(ncid, "f41", NC_FLOAT, RANK_f41, f41_dims, &f41_id);
    check_err(stat,__LINE__,__FILE__);

    d42_dims[0] = D4_dim;
    d42_dims[1] = D2_dim;
    stat = nc_def_var(ncid, "d42", NC_DOUBLE, RANK_d42, d42_dims, &d42_id);
    check_err(stat,__LINE__,__FILE__);

    c43_dims[0] = D4_dim;
    c43_dims[1] = D3_dim;
    stat = nc_def_var(ncid, "c43", NC_CHAR, RANK_c43, c43_dims, &c43_id);
    check_err(stat,__LINE__,__FILE__);

    b44_dims[0] = D4_dim;
    b44_dims[1] = D4_dim;
    stat = nc_def_var(ncid, "b44", NC_BYTE, RANK_b44, b44_dims, &b44_id);
    check_err(stat,__LINE__,__FILE__);

    sr11_dims[0] = Dr_dim;
    sr11_dims[1] = D1_dim;
    sr11_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "sr11", NC_SHORT, RANK_sr11, sr11_dims, &sr11_id);
    check_err(stat,__LINE__,__FILE__);

    ir12_dims[0] = Dr_dim;
    ir12_dims[1] = D1_dim;
    ir12_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "ir12", NC_INT, RANK_ir12, ir12_dims, &ir12_id);
    check_err(stat,__LINE__,__FILE__);

    fr13_dims[0] = Dr_dim;
    fr13_dims[1] = D1_dim;
    fr13_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "fr13", NC_FLOAT, RANK_fr13, fr13_dims, &fr13_id);
    check_err(stat,__LINE__,__FILE__);

    dr14_dims[0] = Dr_dim;
    dr14_dims[1] = D1_dim;
    dr14_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "dr14", NC_DOUBLE, RANK_dr14, dr14_dims, &dr14_id);
    check_err(stat,__LINE__,__FILE__);

    cr21_dims[0] = Dr_dim;
    cr21_dims[1] = D2_dim;
    cr21_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "cr21", NC_CHAR, RANK_cr21, cr21_dims, &cr21_id);
    check_err(stat,__LINE__,__FILE__);

    br22_dims[0] = Dr_dim;
    br22_dims[1] = D2_dim;
    br22_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "br22", NC_BYTE, RANK_br22, br22_dims, &br22_id);
    check_err(stat,__LINE__,__FILE__);

    sr23_dims[0] = Dr_dim;
    sr23_dims[1] = D2_dim;
    sr23_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "sr23", NC_SHORT, RANK_sr23, sr23_dims, &sr23_id);
    check_err(stat,__LINE__,__FILE__);

    ir24_dims[0] = Dr_dim;
    ir24_dims[1] = D2_dim;
    ir24_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "ir24", NC_INT, RANK_ir24, ir24_dims, &ir24_id);
    check_err(stat,__LINE__,__FILE__);

    fr31_dims[0] = Dr_dim;
    fr31_dims[1] = D3_dim;
    fr31_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "fr31", NC_FLOAT, RANK_fr31, fr31_dims, &fr31_id);
    check_err(stat,__LINE__,__FILE__);

    dr32_dims[0] = Dr_dim;
    dr32_dims[1] = D3_dim;
    dr32_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "dr32", NC_DOUBLE, RANK_dr32, dr32_dims, &dr32_id);
    check_err(stat,__LINE__,__FILE__);

    cr33_dims[0] = Dr_dim;
    cr33_dims[1] = D3_dim;
    cr33_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "cr33", NC_CHAR, RANK_cr33, cr33_dims, &cr33_id);
    check_err(stat,__LINE__,__FILE__);

    br34_dims[0] = Dr_dim;
    br34_dims[1] = D3_dim;
    br34_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "br34", NC_BYTE, RANK_br34, br34_dims, &br34_id);
    check_err(stat,__LINE__,__FILE__);

    sr41_dims[0] = Dr_dim;
    sr41_dims[1] = D4_dim;
    sr41_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "sr41", NC_SHORT, RANK_sr41, sr41_dims, &sr41_id);
    check_err(stat,__LINE__,__FILE__);

    ir42_dims[0] = Dr_dim;
    ir42_dims[1] = D4_dim;
    ir42_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "ir42", NC_INT, RANK_ir42, ir42_dims, &ir42_id);
    check_err(stat,__LINE__,__FILE__);

    fr43_dims[0] = Dr_dim;
    fr43_dims[1] = D4_dim;
    fr43_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "fr43", NC_FLOAT, RANK_fr43, fr43_dims, &fr43_id);
    check_err(stat,__LINE__,__FILE__);

    dr44_dims[0] = Dr_dim;
    dr44_dims[1] = D4_dim;
    dr44_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "dr44", NC_DOUBLE, RANK_dr44, dr44_dims, &dr44_id);
    check_err(stat,__LINE__,__FILE__);

    c111_dims[0] = D1_dim;
    c111_dims[1] = D1_dim;
    c111_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "c111", NC_CHAR, RANK_c111, c111_dims, &c111_id);
    check_err(stat,__LINE__,__FILE__);

    b112_dims[0] = D1_dim;
    b112_dims[1] = D1_dim;
    b112_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "b112", NC_BYTE, RANK_b112, b112_dims, &b112_id);
    check_err(stat,__LINE__,__FILE__);

    s113_dims[0] = D1_dim;
    s113_dims[1] = D1_dim;
    s113_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "s113", NC_SHORT, RANK_s113, s113_dims, &s113_id);
    check_err(stat,__LINE__,__FILE__);

    i114_dims[0] = D1_dim;
    i114_dims[1] = D1_dim;
    i114_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "i114", NC_INT, RANK_i114, i114_dims, &i114_id);
    check_err(stat,__LINE__,__FILE__);

    f121_dims[0] = D1_dim;
    f121_dims[1] = D2_dim;
    f121_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "f121", NC_FLOAT, RANK_f121, f121_dims, &f121_id);
    check_err(stat,__LINE__,__FILE__);

    d122_dims[0] = D1_dim;
    d122_dims[1] = D2_dim;
    d122_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "d122", NC_DOUBLE, RANK_d122, d122_dims, &d122_id);
    check_err(stat,__LINE__,__FILE__);

    c123_dims[0] = D1_dim;
    c123_dims[1] = D2_dim;
    c123_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "c123", NC_CHAR, RANK_c123, c123_dims, &c123_id);
    check_err(stat,__LINE__,__FILE__);

    b124_dims[0] = D1_dim;
    b124_dims[1] = D2_dim;
    b124_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "b124", NC_BYTE, RANK_b124, b124_dims, &b124_id);
    check_err(stat,__LINE__,__FILE__);

    s131_dims[0] = D1_dim;
    s131_dims[1] = D3_dim;
    s131_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "s131", NC_SHORT, RANK_s131, s131_dims, &s131_id);
    check_err(stat,__LINE__,__FILE__);

    i132_dims[0] = D1_dim;
    i132_dims[1] = D3_dim;
    i132_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "i132", NC_INT, RANK_i132, i132_dims, &i132_id);
    check_err(stat,__LINE__,__FILE__);

    f133_dims[0] = D1_dim;
    f133_dims[1] = D3_dim;
    f133_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "f133", NC_FLOAT, RANK_f133, f133_dims, &f133_id);
    check_err(stat,__LINE__,__FILE__);

    d134_dims[0] = D1_dim;
    d134_dims[1] = D3_dim;
    d134_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "d134", NC_DOUBLE, RANK_d134, d134_dims, &d134_id);
    check_err(stat,__LINE__,__FILE__);

    c141_dims[0] = D1_dim;
    c141_dims[1] = D4_dim;
    c141_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "c141", NC_CHAR, RANK_c141, c141_dims, &c141_id);
    check_err(stat,__LINE__,__FILE__);

    b142_dims[0] = D1_dim;
    b142_dims[1] = D4_dim;
    b142_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "b142", NC_BYTE, RANK_b142, b142_dims, &b142_id);
    check_err(stat,__LINE__,__FILE__);

    s143_dims[0] = D1_dim;
    s143_dims[1] = D4_dim;
    s143_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "s143", NC_SHORT, RANK_s143, s143_dims, &s143_id);
    check_err(stat,__LINE__,__FILE__);

    i144_dims[0] = D1_dim;
    i144_dims[1] = D4_dim;
    i144_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "i144", NC_INT, RANK_i144, i144_dims, &i144_id);
    check_err(stat,__LINE__,__FILE__);

    f211_dims[0] = D2_dim;
    f211_dims[1] = D1_dim;
    f211_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "f211", NC_FLOAT, RANK_f211, f211_dims, &f211_id);
    check_err(stat,__LINE__,__FILE__);

    d212_dims[0] = D2_dim;
    d212_dims[1] = D1_dim;
    d212_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "d212", NC_DOUBLE, RANK_d212, d212_dims, &d212_id);
    check_err(stat,__LINE__,__FILE__);

    c213_dims[0] = D2_dim;
    c213_dims[1] = D1_dim;
    c213_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "c213", NC_CHAR, RANK_c213, c213_dims, &c213_id);
    check_err(stat,__LINE__,__FILE__);

    b214_dims[0] = D2_dim;
    b214_dims[1] = D1_dim;
    b214_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "b214", NC_BYTE, RANK_b214, b214_dims, &b214_id);
    check_err(stat,__LINE__,__FILE__);

    s221_dims[0] = D2_dim;
    s221_dims[1] = D2_dim;
    s221_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "s221", NC_SHORT, RANK_s221, s221_dims, &s221_id);
    check_err(stat,__LINE__,__FILE__);

    i222_dims[0] = D2_dim;
    i222_dims[1] = D2_dim;
    i222_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "i222", NC_INT, RANK_i222, i222_dims, &i222_id);
    check_err(stat,__LINE__,__FILE__);

    f223_dims[0] = D2_dim;
    f223_dims[1] = D2_dim;
    f223_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "f223", NC_FLOAT, RANK_f223, f223_dims, &f223_id);
    check_err(stat,__LINE__,__FILE__);

    d224_dims[0] = D2_dim;
    d224_dims[1] = D2_dim;
    d224_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "d224", NC_DOUBLE, RANK_d224, d224_dims, &d224_id);
    check_err(stat,__LINE__,__FILE__);

    c231_dims[0] = D2_dim;
    c231_dims[1] = D3_dim;
    c231_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "c231", NC_CHAR, RANK_c231, c231_dims, &c231_id);
    check_err(stat,__LINE__,__FILE__);

    b232_dims[0] = D2_dim;
    b232_dims[1] = D3_dim;
    b232_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "b232", NC_BYTE, RANK_b232, b232_dims, &b232_id);
    check_err(stat,__LINE__,__FILE__);

    s233_dims[0] = D2_dim;
    s233_dims[1] = D3_dim;
    s233_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "s233", NC_SHORT, RANK_s233, s233_dims, &s233_id);
    check_err(stat,__LINE__,__FILE__);

    i234_dims[0] = D2_dim;
    i234_dims[1] = D3_dim;
    i234_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "i234", NC_INT, RANK_i234, i234_dims, &i234_id);
    check_err(stat,__LINE__,__FILE__);

    f241_dims[0] = D2_dim;
    f241_dims[1] = D4_dim;
    f241_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "f241", NC_FLOAT, RANK_f241, f241_dims, &f241_id);
    check_err(stat,__LINE__,__FILE__);

    d242_dims[0] = D2_dim;
    d242_dims[1] = D4_dim;
    d242_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "d242", NC_DOUBLE, RANK_d242, d242_dims, &d242_id);
    check_err(stat,__LINE__,__FILE__);

    c243_dims[0] = D2_dim;
    c243_dims[1] = D4_dim;
    c243_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "c243", NC_CHAR, RANK_c243, c243_dims, &c243_id);
    check_err(stat,__LINE__,__FILE__);

    b244_dims[0] = D2_dim;
    b244_dims[1] = D4_dim;
    b244_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "b244", NC_BYTE, RANK_b244, b244_dims, &b244_id);
    check_err(stat,__LINE__,__FILE__);

    s311_dims[0] = D3_dim;
    s311_dims[1] = D1_dim;
    s311_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "s311", NC_SHORT, RANK_s311, s311_dims, &s311_id);
    check_err(stat,__LINE__,__FILE__);

    i312_dims[0] = D3_dim;
    i312_dims[1] = D1_dim;
    i312_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "i312", NC_INT, RANK_i312, i312_dims, &i312_id);
    check_err(stat,__LINE__,__FILE__);

    f313_dims[0] = D3_dim;
    f313_dims[1] = D1_dim;
    f313_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "f313", NC_FLOAT, RANK_f313, f313_dims, &f313_id);
    check_err(stat,__LINE__,__FILE__);

    d314_dims[0] = D3_dim;
    d314_dims[1] = D1_dim;
    d314_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "d314", NC_DOUBLE, RANK_d314, d314_dims, &d314_id);
    check_err(stat,__LINE__,__FILE__);

    c321_dims[0] = D3_dim;
    c321_dims[1] = D2_dim;
    c321_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "c321", NC_CHAR, RANK_c321, c321_dims, &c321_id);
    check_err(stat,__LINE__,__FILE__);

    b322_dims[0] = D3_dim;
    b322_dims[1] = D2_dim;
    b322_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "b322", NC_BYTE, RANK_b322, b322_dims, &b322_id);
    check_err(stat,__LINE__,__FILE__);

    s323_dims[0] = D3_dim;
    s323_dims[1] = D2_dim;
    s323_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "s323", NC_SHORT, RANK_s323, s323_dims, &s323_id);
    check_err(stat,__LINE__,__FILE__);

    i324_dims[0] = D3_dim;
    i324_dims[1] = D2_dim;
    i324_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "i324", NC_INT, RANK_i324, i324_dims, &i324_id);
    check_err(stat,__LINE__,__FILE__);

    f331_dims[0] = D3_dim;
    f331_dims[1] = D3_dim;
    f331_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "f331", NC_FLOAT, RANK_f331, f331_dims, &f331_id);
    check_err(stat,__LINE__,__FILE__);

    d332_dims[0] = D3_dim;
    d332_dims[1] = D3_dim;
    d332_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "d332", NC_DOUBLE, RANK_d332, d332_dims, &d332_id);
    check_err(stat,__LINE__,__FILE__);

    c333_dims[0] = D3_dim;
    c333_dims[1] = D3_dim;
    c333_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "c333", NC_CHAR, RANK_c333, c333_dims, &c333_id);
    check_err(stat,__LINE__,__FILE__);

    b334_dims[0] = D3_dim;
    b334_dims[1] = D3_dim;
    b334_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "b334", NC_BYTE, RANK_b334, b334_dims, &b334_id);
    check_err(stat,__LINE__,__FILE__);

    s341_dims[0] = D3_dim;
    s341_dims[1] = D4_dim;
    s341_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "s341", NC_SHORT, RANK_s341, s341_dims, &s341_id);
    check_err(stat,__LINE__,__FILE__);

    i342_dims[0] = D3_dim;
    i342_dims[1] = D4_dim;
    i342_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "i342", NC_INT, RANK_i342, i342_dims, &i342_id);
    check_err(stat,__LINE__,__FILE__);

    f343_dims[0] = D3_dim;
    f343_dims[1] = D4_dim;
    f343_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "f343", NC_FLOAT, RANK_f343, f343_dims, &f343_id);
    check_err(stat,__LINE__,__FILE__);

    d344_dims[0] = D3_dim;
    d344_dims[1] = D4_dim;
    d344_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "d344", NC_DOUBLE, RANK_d344, d344_dims, &d344_id);
    check_err(stat,__LINE__,__FILE__);

    c411_dims[0] = D4_dim;
    c411_dims[1] = D1_dim;
    c411_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "c411", NC_CHAR, RANK_c411, c411_dims, &c411_id);
    check_err(stat,__LINE__,__FILE__);

    b412_dims[0] = D4_dim;
    b412_dims[1] = D1_dim;
    b412_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "b412", NC_BYTE, RANK_b412, b412_dims, &b412_id);
    check_err(stat,__LINE__,__FILE__);

    s413_dims[0] = D4_dim;
    s413_dims[1] = D1_dim;
    s413_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "s413", NC_SHORT, RANK_s413, s413_dims, &s413_id);
    check_err(stat,__LINE__,__FILE__);

    i414_dims[0] = D4_dim;
    i414_dims[1] = D1_dim;
    i414_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "i414", NC_INT, RANK_i414, i414_dims, &i414_id);
    check_err(stat,__LINE__,__FILE__);

    f421_dims[0] = D4_dim;
    f421_dims[1] = D2_dim;
    f421_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "f421", NC_FLOAT, RANK_f421, f421_dims, &f421_id);
    check_err(stat,__LINE__,__FILE__);

    d422_dims[0] = D4_dim;
    d422_dims[1] = D2_dim;
    d422_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "d422", NC_DOUBLE, RANK_d422, d422_dims, &d422_id);
    check_err(stat,__LINE__,__FILE__);

    c423_dims[0] = D4_dim;
    c423_dims[1] = D2_dim;
    c423_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "c423", NC_CHAR, RANK_c423, c423_dims, &c423_id);
    check_err(stat,__LINE__,__FILE__);

    b424_dims[0] = D4_dim;
    b424_dims[1] = D2_dim;
    b424_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "b424", NC_BYTE, RANK_b424, b424_dims, &b424_id);
    check_err(stat,__LINE__,__FILE__);

    s431_dims[0] = D4_dim;
    s431_dims[1] = D3_dim;
    s431_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "s431", NC_SHORT, RANK_s431, s431_dims, &s431_id);
    check_err(stat,__LINE__,__FILE__);

    i432_dims[0] = D4_dim;
    i432_dims[1] = D3_dim;
    i432_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "i432", NC_INT, RANK_i432, i432_dims, &i432_id);
    check_err(stat,__LINE__,__FILE__);

    f433_dims[0] = D4_dim;
    f433_dims[1] = D3_dim;
    f433_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "f433", NC_FLOAT, RANK_f433, f433_dims, &f433_id);
    check_err(stat,__LINE__,__FILE__);

    d434_dims[0] = D4_dim;
    d434_dims[1] = D3_dim;
    d434_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "d434", NC_DOUBLE, RANK_d434, d434_dims, &d434_id);
    check_err(stat,__LINE__,__FILE__);

    c441_dims[0] = D4_dim;
    c441_dims[1] = D4_dim;
    c441_dims[2] = D1_dim;
    stat = nc_def_var(ncid, "c441", NC_CHAR, RANK_c441, c441_dims, &c441_id);
    check_err(stat,__LINE__,__FILE__);

    b442_dims[0] = D4_dim;
    b442_dims[1] = D4_dim;
    b442_dims[2] = D2_dim;
    stat = nc_def_var(ncid, "b442", NC_BYTE, RANK_b442, b442_dims, &b442_id);
    check_err(stat,__LINE__,__FILE__);

    s443_dims[0] = D4_dim;
    s443_dims[1] = D4_dim;
    s443_dims[2] = D3_dim;
    stat = nc_def_var(ncid, "s443", NC_SHORT, RANK_s443, s443_dims, &s443_id);
    check_err(stat,__LINE__,__FILE__);

    i444_dims[0] = D4_dim;
    i444_dims[1] = D4_dim;
    i444_dims[2] = D4_dim;
    stat = nc_def_var(ncid, "i444", NC_INT, RANK_i444, i444_dims, &i444_id);
    check_err(stat,__LINE__,__FILE__);

    /* assign global attributes */
    { /* Gc */
    stat = nc_put_att_text(ncid, NC_GLOBAL, "Gc", 1, "€");
    check_err(stat,__LINE__,__FILE__);
    }
    { /* Gb */
    static const signed char Gb_att[2] = {-128, 127} ;
    stat = nc_put_att_schar(ncid, NC_GLOBAL, "Gb", NC_BYTE, 2, Gb_att);
    check_err(stat,__LINE__,__FILE__);
    }
    { /* Gs */
    static const short Gs_att[3] = {-32768, 32767, 32767} ;
    stat = nc_put_att_short(ncid, NC_GLOBAL, "Gs", NC_SHORT, 3, Gs_att);
    check_err(stat,__LINE__,__FILE__);
    }
    { /* Gi */
    static const int Gi_att[4] = {-2147483648, 2147483647, -2147483648, -2147483648} ;
    stat = nc_put_att_int(ncid, NC_GLOBAL, "Gi", NC_INT, 4, Gi_att);
    check_err(stat,__LINE__,__FILE__);
    }
    { /* Gf */
    static const float Gf_att[5] = {-3.4028231e+38, 3.4028231e+38, -9.96921e+36, 9.96921e+36, 531} ;
    stat = nc_put_att_float(ncid, NC_GLOBAL, "Gf", NC_FLOAT, 5, Gf_att);
    check_err(stat,__LINE__,__FILE__);
    }
    { /* Gd */
    static const double Gd_att[6] = {-42, 42, -1, 1, 660, 650} ;
    stat = nc_put_att_double(ncid, NC_GLOBAL, "Gd", NC_DOUBLE, 6, Gd_att);
    check_err(stat,__LINE__,__FILE__);
    }


    /* assign per-variable attributes */
    { /* c */
    stat = nc_put_att_text(ncid, b_id, "c", 0, "");
    check_err(stat,__LINE__,__FILE__);
    }
    { /* b */
    static const signed char s_b_att[1] = {-128} ;
    stat = nc_put_att_schar(ncid, s_id, "b", NC_BYTE, 1, s_b_att);
    check_err(stat,__LINE__,__FILE__);
    }
    { /* s */
    static const short s_s_att[2] = {-32768, 32767} ;
    stat = nc_put_att_short(ncid, s_id, "s", NC_SHORT, 2, s_s_att);
    check_err(stat,__LINE__,__FILE__);
    }
    { /* i */
    static const int i_i_att[3] = {-2147483648, 2147483647, -2147483648} ;
    stat = nc_put_att_int(ncid, i_id, "i", NC_INT, 3, i_i_att);
    check_err(stat,__LINE__,__FILE__);
    }
    { /* f */
    static const float i_f_att[4] = {-3.4028231e+38, 3.4028231e+38, -9.96921e+36, 9.96921e+36} ;
    stat = nc_put_att_float(ncid, i_id, "f", NC_FLOAT, 4, i_f_att);
    check_err(stat,__LINE__,__FILE__);
    }
    { /* d */
    static const double i_d_att[5] = {-42, 42, -1, 1, 660} ;
    stat = nc_put_att_double(ncid, i_id, "d", NC_DOUBLE, 5, i_d_att);
    check_err(stat,__LINE__,__FILE__);
    }
    { /* c */
    stat = nc_put_att_text(ncid, d_id, "c", 6, "blahhh");
    check_err(stat,__LINE__,__FILE__);
    }


    /* leave define mode */
    stat = nc_enddef (ncid);
    check_err(stat,__LINE__,__FILE__);

    /* assign variable data */
    {
    size_t zero = 0;
    static char c_data[1] = {'\002'};
    stat = nc_put_var1(ncid, c_id, &zero, c_data);    check_err(stat,__LINE__,__FILE__);
    }
    {
    size_t zero = 0;
    static signed char b_data[1] = {-2};
    stat = nc_put_var1(ncid, b_id, &zero, b_data);    check_err(stat,__LINE__,__FILE__);
    }
    {
    size_t zero = 0;
    static short s_data[1] = {-5};
    stat = nc_put_var1(ncid, s_id, &zero, s_data);    check_err(stat,__LINE__,__FILE__);
    }
    {
    size_t zero = 0;
    static int i_data[1] = {-20};
    stat = nc_put_var1(ncid, i_id, &zero, i_data);    check_err(stat,__LINE__,__FILE__);
    }
    {
    size_t zero = 0;
    static float f_data[1] = {-9};
    stat = nc_put_var1(ncid, f_id, &zero, f_data);    check_err(stat,__LINE__,__FILE__);
    }
    {
    size_t zero = 0;
    static double d_data[1] = {-10};
    stat = nc_put_var1(ncid, d_id, &zero, d_data);    check_err(stat,__LINE__,__FILE__);
    }
    {
    char cr_data[2] = "€\177" ;
    size_t cr_startset[1] = {0} ;
    size_t cr_countset[1] = {2} ;
    stat = nc_put_vara(ncid, cr_id, cr_startset, cr_countset, cr_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char br_data[2] = {-128, 127} ;
    size_t br_startset[1] = {0} ;
    size_t br_countset[1] = {2} ;
    stat = nc_put_vara(ncid, br_id, br_startset, br_countset, br_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short sr_data[2] = {-32768, 32767} ;
    size_t sr_startset[1] = {0} ;
    size_t sr_countset[1] = {2} ;
    stat = nc_put_vara(ncid, sr_id, sr_startset, sr_countset, sr_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int ir_data[2] = {-2147483648, 2147483647} ;
    size_t ir_startset[1] = {0} ;
    size_t ir_countset[1] = {2} ;
    stat = nc_put_vara(ncid, ir_id, ir_startset, ir_countset, ir_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float fr_data[2] = {-3.4028231e+38, 3.4028231e+38} ;
    size_t fr_startset[1] = {0} ;
    size_t fr_countset[1] = {2} ;
    stat = nc_put_vara(ncid, fr_id, fr_startset, fr_countset, fr_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double dr_data[2] = {-42, 42} ;
    size_t dr_startset[1] = {0} ;
    size_t dr_countset[1] = {2} ;
    stat = nc_put_vara(ncid, dr_id, dr_startset, dr_countset, dr_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c1_data[1] = "€" ;
    size_t c1_startset[1] = {0} ;
    size_t c1_countset[1] = {1} ;
    stat = nc_put_vara(ncid, c1_id, c1_startset, c1_countset, c1_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b1_data[1] = {-128} ;
    size_t b1_startset[1] = {0} ;
    size_t b1_countset[1] = {1} ;
    stat = nc_put_vara(ncid, b1_id, b1_startset, b1_countset, b1_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s1_data[1] = {-32768} ;
    size_t s1_startset[1] = {0} ;
    size_t s1_countset[1] = {1} ;
    stat = nc_put_vara(ncid, s1_id, s1_startset, s1_countset, s1_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i1_data[1] = {-2147483648} ;
    size_t i1_startset[1] = {0} ;
    size_t i1_countset[1] = {1} ;
    stat = nc_put_vara(ncid, i1_id, i1_startset, i1_countset, i1_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f1_data[1] = {-3.4028231e+38} ;
    size_t f1_startset[1] = {0} ;
    size_t f1_countset[1] = {1} ;
    stat = nc_put_vara(ncid, f1_id, f1_startset, f1_countset, f1_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d1_data[1] = {-42} ;
    size_t d1_startset[1] = {0} ;
    size_t d1_countset[1] = {1} ;
    stat = nc_put_vara(ncid, d1_id, d1_startset, d1_countset, d1_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c2_data[2] = "€\177" ;
    size_t c2_startset[1] = {0} ;
    size_t c2_countset[1] = {2} ;
    stat = nc_put_vara(ncid, c2_id, c2_startset, c2_countset, c2_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b2_data[2] = {-128, 127} ;
    size_t b2_startset[1] = {0} ;
    size_t b2_countset[1] = {2} ;
    stat = nc_put_vara(ncid, b2_id, b2_startset, b2_countset, b2_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s2_data[2] = {-32768, 32767} ;
    size_t s2_startset[1] = {0} ;
    size_t s2_countset[1] = {2} ;
    stat = nc_put_vara(ncid, s2_id, s2_startset, s2_countset, s2_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i2_data[2] = {-2147483648, 2147483647} ;
    size_t i2_startset[1] = {0} ;
    size_t i2_countset[1] = {2} ;
    stat = nc_put_vara(ncid, i2_id, i2_startset, i2_countset, i2_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f2_data[2] = {-3.4028231e+38, 3.4028231e+38} ;
    size_t f2_startset[1] = {0} ;
    size_t f2_countset[1] = {2} ;
    stat = nc_put_vara(ncid, f2_id, f2_startset, f2_countset, f2_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d2_data[2] = {-42, 42} ;
    size_t d2_startset[1] = {0} ;
    size_t d2_countset[1] = {2} ;
    stat = nc_put_vara(ncid, d2_id, d2_startset, d2_countset, d2_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c3_data[3] = "€\177A" ;
    size_t c3_startset[1] = {0} ;
    size_t c3_countset[1] = {3} ;
    stat = nc_put_vara(ncid, c3_id, c3_startset, c3_countset, c3_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b3_data[3] = {-128, 127, 127} ;
    size_t b3_startset[1] = {0} ;
    size_t b3_countset[1] = {3} ;
    stat = nc_put_vara(ncid, b3_id, b3_startset, b3_countset, b3_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s3_data[3] = {-32768, 32767, 32767} ;
    size_t s3_startset[1] = {0} ;
    size_t s3_countset[1] = {3} ;
    stat = nc_put_vara(ncid, s3_id, s3_startset, s3_countset, s3_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i3_data[3] = {-2147483648, 2147483647, -2147483648} ;
    size_t i3_startset[1] = {0} ;
    size_t i3_countset[1] = {3} ;
    stat = nc_put_vara(ncid, i3_id, i3_startset, i3_countset, i3_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f3_data[3] = {-3.4028231e+38, 3.4028231e+38, -9.96921e+36} ;
    size_t f3_startset[1] = {0} ;
    size_t f3_countset[1] = {3} ;
    stat = nc_put_vara(ncid, f3_id, f3_startset, f3_countset, f3_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d3_data[3] = {-42, 42, -1} ;
    size_t d3_startset[1] = {0} ;
    size_t d3_countset[1] = {3} ;
    stat = nc_put_vara(ncid, d3_id, d3_startset, d3_countset, d3_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c4_data[4] = "€\177AZ" ;
    size_t c4_startset[1] = {0} ;
    size_t c4_countset[1] = {4} ;
    stat = nc_put_vara(ncid, c4_id, c4_startset, c4_countset, c4_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b4_data[4] = {-128, 127, 127, -128} ;
    size_t b4_startset[1] = {0} ;
    size_t b4_countset[1] = {4} ;
    stat = nc_put_vara(ncid, b4_id, b4_startset, b4_countset, b4_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s4_data[4] = {-32768, 32767, 32767, -32768} ;
    size_t s4_startset[1] = {0} ;
    size_t s4_countset[1] = {4} ;
    stat = nc_put_vara(ncid, s4_id, s4_startset, s4_countset, s4_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i4_data[4] = {-2147483648, 2147483647, -2147483648, -2147483648} ;
    size_t i4_startset[1] = {0} ;
    size_t i4_countset[1] = {4} ;
    stat = nc_put_vara(ncid, i4_id, i4_startset, i4_countset, i4_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f4_data[4] = {-3.4028231e+38, 3.4028231e+38, -9.96921e+36, 9.96921e+36} ;
    size_t f4_startset[1] = {0} ;
    size_t f4_countset[1] = {4} ;
    stat = nc_put_vara(ncid, f4_id, f4_startset, f4_countset, f4_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d4_data[4] = {-42, 42, -1, 1} ;
    size_t d4_startset[1] = {0} ;
    size_t d4_countset[1] = {4} ;
    stat = nc_put_vara(ncid, d4_id, d4_startset, d4_countset, d4_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char cr1_data[2] = "\030\034" ;
    size_t cr1_startset[2] = {0, 0} ;
    size_t cr1_countset[2] = {2, 1} ;
    stat = nc_put_vara(ncid, cr1_id, cr1_startset, cr1_countset, cr1_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char br2_data[4] = {-24, -26, -20, -22} ;
    size_t br2_startset[2] = {0, 0} ;
    size_t br2_countset[2] = {2, 2} ;
    stat = nc_put_vara(ncid, br2_id, br2_startset, br2_countset, br2_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short sr3_data[6] = {-375, -380, -385, -350, -355, -360} ;
    size_t sr3_startset[2] = {0, 0} ;
    size_t sr3_countset[2] = {2, 3} ;
    stat = nc_put_vara(ncid, sr3_id, sr3_startset, sr3_countset, sr3_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int ir4_data[8] = {-24000, -24020, -24040, -24060, -23600, -23620, -23640, -23660} ;
    size_t ir4_startset[2] = {0, 0} ;
    size_t ir4_countset[2] = {2, 4} ;
    stat = nc_put_vara(ncid, ir4_id, ir4_startset, ir4_countset, ir4_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f11_data[1] = {-2187} ;
    size_t f11_startset[2] = {0, 0} ;
    size_t f11_countset[2] = {1, 1} ;
    stat = nc_put_vara(ncid, f11_id, f11_startset, f11_countset, f11_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d12_data[2] = {-3000, -3010} ;
    size_t d12_startset[2] = {0, 0} ;
    size_t d12_countset[2] = {1, 2} ;
    stat = nc_put_vara(ncid, d12_id, d12_startset, d12_countset, d12_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c13_data[3] = "\030\032\034" ;
    size_t c13_startset[2] = {0, 0} ;
    size_t c13_countset[2] = {1, 3} ;
    stat = nc_put_vara(ncid, c13_id, c13_startset, c13_countset, c13_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b14_data[4] = {-24, -26, -28, -30} ;
    size_t b14_startset[2] = {0, 0} ;
    size_t b14_countset[2] = {1, 4} ;
    stat = nc_put_vara(ncid, b14_id, b14_startset, b14_countset, b14_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s21_data[2] = {-375, -350} ;
    size_t s21_startset[2] = {0, 0} ;
    size_t s21_countset[2] = {2, 1} ;
    stat = nc_put_vara(ncid, s21_id, s21_startset, s21_countset, s21_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i22_data[4] = {-24000, -24020, -23600, -23620} ;
    size_t i22_startset[2] = {0, 0} ;
    size_t i22_countset[2] = {2, 2} ;
    stat = nc_put_vara(ncid, i22_id, i22_startset, i22_countset, i22_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f23_data[6] = {-2187, -2196, -2205, -2106, -2115, -2124} ;
    size_t f23_startset[2] = {0, 0} ;
    size_t f23_countset[2] = {2, 3} ;
    stat = nc_put_vara(ncid, f23_id, f23_startset, f23_countset, f23_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d24_data[8] = {-3000, -3010, -3020, -3030, -2900, -2910, -2920, -2930} ;
    size_t d24_startset[2] = {0, 0} ;
    size_t d24_countset[2] = {2, 4} ;
    stat = nc_put_vara(ncid, d24_id, d24_startset, d24_countset, d24_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c31_data[3] = "\030\034 " ;
    size_t c31_startset[2] = {0, 0} ;
    size_t c31_countset[2] = {3, 1} ;
    stat = nc_put_vara(ncid, c31_id, c31_startset, c31_countset, c31_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b32_data[6] = {-24, -26, -20, -22, -16, -18} ;
    size_t b32_startset[2] = {0, 0} ;
    size_t b32_countset[2] = {3, 2} ;
    stat = nc_put_vara(ncid, b32_id, b32_startset, b32_countset, b32_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s33_data[9] = {-375, -380, -385, -350, -355, -360, -325, -330, -335} ;
    size_t s33_startset[2] = {0, 0} ;
    size_t s33_countset[2] = {3, 3} ;
    stat = nc_put_vara(ncid, s33_id, s33_startset, s33_countset, s33_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i34_data[12] = {-24000, -24020, -24040, -24060, -23600, -23620, -23640, -23660, -23200, -23220, -23240, -23260} ;
    size_t i34_startset[2] = {0, 0} ;
    size_t i34_countset[2] = {3, 4} ;
    stat = nc_put_vara(ncid, i34_id, i34_startset, i34_countset, i34_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f41_data[4] = {-2187, -2106, -2025, -1944} ;
    size_t f41_startset[2] = {0, 0} ;
    size_t f41_countset[2] = {4, 1} ;
    stat = nc_put_vara(ncid, f41_id, f41_startset, f41_countset, f41_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d42_data[8] = {-3000, -3010, -2900, -2910, -2800, -2810, -2700, -2710} ;
    size_t d42_startset[2] = {0, 0} ;
    size_t d42_countset[2] = {4, 2} ;
    stat = nc_put_vara(ncid, d42_id, d42_startset, d42_countset, d42_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c43_data[12] = "\030\032\034\034\036  \"$$&(" ;
    size_t c43_startset[2] = {0, 0} ;
    size_t c43_countset[2] = {4, 3} ;
    stat = nc_put_vara(ncid, c43_id, c43_startset, c43_countset, c43_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b44_data[16] = {-24, -26, -28, -30, -20, -22, -24, -26, -16, -18, -20, -22, -12, -14, -16, -18} ;
    size_t b44_startset[2] = {0, 0} ;
    size_t b44_countset[2] = {4, 4} ;
    stat = nc_put_vara(ncid, b44_id, b44_startset, b44_countset, b44_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short sr11_data[2] = {2500, 2375} ;
    size_t sr11_startset[3] = {0, 0, 0} ;
    size_t sr11_countset[3] = {2, 1, 1} ;
    stat = nc_put_vara(ncid, sr11_id, sr11_startset, sr11_countset, sr11_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int ir12_data[4] = {640000, 639980, 632000, 631980} ;
    size_t ir12_startset[3] = {0, 0, 0} ;
    size_t ir12_countset[3] = {2, 1, 2} ;
    stat = nc_put_vara(ncid, ir12_id, ir12_startset, ir12_countset, ir12_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float fr13_data[6] = {26244, 26235, 26226, 25515, 25506, 25497} ;
    size_t fr13_startset[3] = {0, 0, 0} ;
    size_t fr13_countset[3] = {2, 1, 3} ;
    stat = nc_put_vara(ncid, fr13_id, fr13_startset, fr13_countset, fr13_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double dr14_data[8] = {40000, 39990, 39980, 39970, 39000, 38990, 38980, 38970} ;
    size_t dr14_startset[3] = {0, 0, 0} ;
    size_t dr14_countset[3] = {2, 1, 4} ;
    stat = nc_put_vara(ncid, dr14_id, dr14_startset, dr14_countset, dr14_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char cr21_data[4] = "@DHL" ;
    size_t cr21_startset[3] = {0, 0, 0} ;
    size_t cr21_countset[3] = {2, 2, 1} ;
    stat = nc_put_vara(ncid, cr21_id, cr21_startset, cr21_countset, cr21_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char br22_data[8] = {64, 62, 68, 66, 56, 54, 60, 58} ;
    size_t br22_startset[3] = {0, 0, 0} ;
    size_t br22_countset[3] = {2, 2, 2} ;
    stat = nc_put_vara(ncid, br22_id, br22_startset, br22_countset, br22_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short sr23_data[12] = {2500, 2495, 2490, 2525, 2520, 2515, 2375, 2370, 2365, 2400, 2395, 2390} ;
    size_t sr23_startset[3] = {0, 0, 0} ;
    size_t sr23_countset[3] = {2, 2, 3} ;
    stat = nc_put_vara(ncid, sr23_id, sr23_startset, sr23_countset, sr23_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int ir24_data[16] = {640000, 639980, 639960, 639940, 640400, 640380, 640360, 640340, 632000, 631980, 631960, 631940, 632400, 632380, 632360, 632340} ;
    size_t ir24_startset[3] = {0, 0, 0} ;
    size_t ir24_countset[3] = {2, 2, 4} ;
    stat = nc_put_vara(ncid, ir24_id, ir24_startset, ir24_countset, ir24_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float fr31_data[6] = {26244, 26325, 26406, 25515, 25596, 25677} ;
    size_t fr31_startset[3] = {0, 0, 0} ;
    size_t fr31_countset[3] = {2, 3, 1} ;
    stat = nc_put_vara(ncid, fr31_id, fr31_startset, fr31_countset, fr31_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double dr32_data[12] = {40000, 39990, 40100, 40090, 40200, 40190, 39000, 38990, 39100, 39090, 39200, 39190} ;
    size_t dr32_startset[3] = {0, 0, 0} ;
    size_t dr32_countset[3] = {2, 3, 2} ;
    stat = nc_put_vara(ncid, dr32_id, dr32_startset, dr32_countset, dr32_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char cr33_data[18] = "@BDDFHHJLHJLLNPPRT" ;
    size_t cr33_startset[3] = {0, 0, 0} ;
    size_t cr33_countset[3] = {2, 3, 3} ;
    stat = nc_put_vara(ncid, cr33_id, cr33_startset, cr33_countset, cr33_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char br34_data[24] = {64, 62, 60, 58, 68, 66, 64, 62, 72, 70, 68, 66, 56, 54, 52, 50, 60, 58, 56, 54, 64, 62, 60, 58} ;
    size_t br34_startset[3] = {0, 0, 0} ;
    size_t br34_countset[3] = {2, 3, 4} ;
    stat = nc_put_vara(ncid, br34_id, br34_startset, br34_countset, br34_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short sr41_data[8] = {2500, 2525, 2550, 2575, 2375, 2400, 2425, 2450} ;
    size_t sr41_startset[3] = {0, 0, 0} ;
    size_t sr41_countset[3] = {2, 4, 1} ;
    stat = nc_put_vara(ncid, sr41_id, sr41_startset, sr41_countset, sr41_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int ir42_data[16] = {640000, 639980, 640400, 640380, 640800, 640780, 641200, 641180, 632000, 631980, 632400, 632380, 632800, 632780, 633200, 633180} ;
    size_t ir42_startset[3] = {0, 0, 0} ;
    size_t ir42_countset[3] = {2, 4, 2} ;
    stat = nc_put_vara(ncid, ir42_id, ir42_startset, ir42_countset, ir42_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float fr43_data[24] = {26244, 26235, 26226, 26325, 26316, 26307, 26406, 26397, 26388, 26487, 26478, 26469, 25515, 25506, 25497, 25596, 25587, 25578, 25677, 25668, 25659, 25758, 25749, 25740} ;
    size_t fr43_startset[3] = {0, 0, 0} ;
    size_t fr43_countset[3] = {2, 4, 3} ;
    stat = nc_put_vara(ncid, fr43_id, fr43_startset, fr43_countset, fr43_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double dr44_data[32] = {40000, 39990, 39980, 39970, 40100, 40090, 40080, 40070, 40200, 40190, 40180, 40170, 40300, 40290, 40280, 40270, 39000, 38990, 38980, 38970, 39100, 39090, 39080, 39070, 39200, 39190, 39180, 39170, 39300, 39290, 39280, 39270} ;
    size_t dr44_startset[3] = {0, 0, 0} ;
    size_t dr44_countset[3] = {2, 4, 4} ;
    stat = nc_put_vara(ncid, dr44_id, dr44_startset, dr44_countset, dr44_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c111_data[1] = "@" ;
    size_t c111_startset[3] = {0, 0, 0} ;
    size_t c111_countset[3] = {1, 1, 1} ;
    stat = nc_put_vara(ncid, c111_id, c111_startset, c111_countset, c111_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b112_data[2] = {64, 62} ;
    size_t b112_startset[3] = {0, 0, 0} ;
    size_t b112_countset[3] = {1, 1, 2} ;
    stat = nc_put_vara(ncid, b112_id, b112_startset, b112_countset, b112_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s113_data[3] = {2500, 2495, 2490} ;
    size_t s113_startset[3] = {0, 0, 0} ;
    size_t s113_countset[3] = {1, 1, 3} ;
    stat = nc_put_vara(ncid, s113_id, s113_startset, s113_countset, s113_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i114_data[4] = {640000, 639980, 639960, 639940} ;
    size_t i114_startset[3] = {0, 0, 0} ;
    size_t i114_countset[3] = {1, 1, 4} ;
    stat = nc_put_vara(ncid, i114_id, i114_startset, i114_countset, i114_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f121_data[2] = {26244, 26325} ;
    size_t f121_startset[3] = {0, 0, 0} ;
    size_t f121_countset[3] = {1, 2, 1} ;
    stat = nc_put_vara(ncid, f121_id, f121_startset, f121_countset, f121_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d122_data[4] = {40000, 39990, 40100, 40090} ;
    size_t d122_startset[3] = {0, 0, 0} ;
    size_t d122_countset[3] = {1, 2, 2} ;
    stat = nc_put_vara(ncid, d122_id, d122_startset, d122_countset, d122_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c123_data[6] = "@BDDFH" ;
    size_t c123_startset[3] = {0, 0, 0} ;
    size_t c123_countset[3] = {1, 2, 3} ;
    stat = nc_put_vara(ncid, c123_id, c123_startset, c123_countset, c123_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b124_data[8] = {64, 62, 60, 58, 68, 66, 64, 62} ;
    size_t b124_startset[3] = {0, 0, 0} ;
    size_t b124_countset[3] = {1, 2, 4} ;
    stat = nc_put_vara(ncid, b124_id, b124_startset, b124_countset, b124_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s131_data[3] = {2500, 2525, 2550} ;
    size_t s131_startset[3] = {0, 0, 0} ;
    size_t s131_countset[3] = {1, 3, 1} ;
    stat = nc_put_vara(ncid, s131_id, s131_startset, s131_countset, s131_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i132_data[6] = {640000, 639980, 640400, 640380, 640800, 640780} ;
    size_t i132_startset[3] = {0, 0, 0} ;
    size_t i132_countset[3] = {1, 3, 2} ;
    stat = nc_put_vara(ncid, i132_id, i132_startset, i132_countset, i132_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f133_data[9] = {26244, 26235, 26226, 26325, 26316, 26307, 26406, 26397, 26388} ;
    size_t f133_startset[3] = {0, 0, 0} ;
    size_t f133_countset[3] = {1, 3, 3} ;
    stat = nc_put_vara(ncid, f133_id, f133_startset, f133_countset, f133_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d134_data[12] = {40000, 39990, 39980, 39970, 40100, 40090, 40080, 40070, 40200, 40190, 40180, 40170} ;
    size_t d134_startset[3] = {0, 0, 0} ;
    size_t d134_countset[3] = {1, 3, 4} ;
    stat = nc_put_vara(ncid, d134_id, d134_startset, d134_countset, d134_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c141_data[4] = "@DHL" ;
    size_t c141_startset[3] = {0, 0, 0} ;
    size_t c141_countset[3] = {1, 4, 1} ;
    stat = nc_put_vara(ncid, c141_id, c141_startset, c141_countset, c141_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b142_data[8] = {64, 62, 68, 66, 72, 70, 76, 74} ;
    size_t b142_startset[3] = {0, 0, 0} ;
    size_t b142_countset[3] = {1, 4, 2} ;
    stat = nc_put_vara(ncid, b142_id, b142_startset, b142_countset, b142_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s143_data[12] = {2500, 2495, 2490, 2525, 2520, 2515, 2550, 2545, 2540, 2575, 2570, 2565} ;
    size_t s143_startset[3] = {0, 0, 0} ;
    size_t s143_countset[3] = {1, 4, 3} ;
    stat = nc_put_vara(ncid, s143_id, s143_startset, s143_countset, s143_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i144_data[16] = {640000, 639980, 639960, 639940, 640400, 640380, 640360, 640340, 640800, 640780, 640760, 640740, 641200, 641180, 641160, 641140} ;
    size_t i144_startset[3] = {0, 0, 0} ;
    size_t i144_countset[3] = {1, 4, 4} ;
    stat = nc_put_vara(ncid, i144_id, i144_startset, i144_countset, i144_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f211_data[2] = {26244, 25515} ;
    size_t f211_startset[3] = {0, 0, 0} ;
    size_t f211_countset[3] = {2, 1, 1} ;
    stat = nc_put_vara(ncid, f211_id, f211_startset, f211_countset, f211_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d212_data[4] = {40000, 39990, 39000, 38990} ;
    size_t d212_startset[3] = {0, 0, 0} ;
    size_t d212_countset[3] = {2, 1, 2} ;
    stat = nc_put_vara(ncid, d212_id, d212_startset, d212_countset, d212_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c213_data[6] = "@BDHJL" ;
    size_t c213_startset[3] = {0, 0, 0} ;
    size_t c213_countset[3] = {2, 1, 3} ;
    stat = nc_put_vara(ncid, c213_id, c213_startset, c213_countset, c213_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b214_data[8] = {64, 62, 60, 58, 56, 54, 52, 50} ;
    size_t b214_startset[3] = {0, 0, 0} ;
    size_t b214_countset[3] = {2, 1, 4} ;
    stat = nc_put_vara(ncid, b214_id, b214_startset, b214_countset, b214_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s221_data[4] = {2500, 2525, 2375, 2400} ;
    size_t s221_startset[3] = {0, 0, 0} ;
    size_t s221_countset[3] = {2, 2, 1} ;
    stat = nc_put_vara(ncid, s221_id, s221_startset, s221_countset, s221_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i222_data[8] = {640000, 639980, 640400, 640380, 632000, 631980, 632400, 632380} ;
    size_t i222_startset[3] = {0, 0, 0} ;
    size_t i222_countset[3] = {2, 2, 2} ;
    stat = nc_put_vara(ncid, i222_id, i222_startset, i222_countset, i222_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f223_data[12] = {26244, 26235, 26226, 26325, 26316, 26307, 25515, 25506, 25497, 25596, 25587, 25578} ;
    size_t f223_startset[3] = {0, 0, 0} ;
    size_t f223_countset[3] = {2, 2, 3} ;
    stat = nc_put_vara(ncid, f223_id, f223_startset, f223_countset, f223_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d224_data[16] = {40000, 39990, 39980, 39970, 40100, 40090, 40080, 40070, 39000, 38990, 38980, 38970, 39100, 39090, 39080, 39070} ;
    size_t d224_startset[3] = {0, 0, 0} ;
    size_t d224_countset[3] = {2, 2, 4} ;
    stat = nc_put_vara(ncid, d224_id, d224_startset, d224_countset, d224_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c231_data[6] = "@DHHLP" ;
    size_t c231_startset[3] = {0, 0, 0} ;
    size_t c231_countset[3] = {2, 3, 1} ;
    stat = nc_put_vara(ncid, c231_id, c231_startset, c231_countset, c231_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b232_data[12] = {64, 62, 68, 66, 72, 70, 56, 54, 60, 58, 64, 62} ;
    size_t b232_startset[3] = {0, 0, 0} ;
    size_t b232_countset[3] = {2, 3, 2} ;
    stat = nc_put_vara(ncid, b232_id, b232_startset, b232_countset, b232_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s233_data[18] = {2500, 2495, 2490, 2525, 2520, 2515, 2550, 2545, 2540, 2375, 2370, 2365, 2400, 2395, 2390, 2425, 2420, 2415} ;
    size_t s233_startset[3] = {0, 0, 0} ;
    size_t s233_countset[3] = {2, 3, 3} ;
    stat = nc_put_vara(ncid, s233_id, s233_startset, s233_countset, s233_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i234_data[24] = {640000, 639980, 639960, 639940, 640400, 640380, 640360, 640340, 640800, 640780, 640760, 640740, 632000, 631980, 631960, 631940, 632400, 632380, 632360, 632340, 632800, 632780, 632760, 632740} ;
    size_t i234_startset[3] = {0, 0, 0} ;
    size_t i234_countset[3] = {2, 3, 4} ;
    stat = nc_put_vara(ncid, i234_id, i234_startset, i234_countset, i234_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f241_data[8] = {26244, 26325, 26406, 26487, 25515, 25596, 25677, 25758} ;
    size_t f241_startset[3] = {0, 0, 0} ;
    size_t f241_countset[3] = {2, 4, 1} ;
    stat = nc_put_vara(ncid, f241_id, f241_startset, f241_countset, f241_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d242_data[16] = {40000, 39990, 40100, 40090, 40200, 40190, 40300, 40290, 39000, 38990, 39100, 39090, 39200, 39190, 39300, 39290} ;
    size_t d242_startset[3] = {0, 0, 0} ;
    size_t d242_countset[3] = {2, 4, 2} ;
    stat = nc_put_vara(ncid, d242_id, d242_startset, d242_countset, d242_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c243_data[24] = "@BDDFHHJLLNPHJLLNPPRTTVX" ;
    size_t c243_startset[3] = {0, 0, 0} ;
    size_t c243_countset[3] = {2, 4, 3} ;
    stat = nc_put_vara(ncid, c243_id, c243_startset, c243_countset, c243_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b244_data[32] = {64, 62, 60, 58, 68, 66, 64, 62, 72, 70, 68, 66, 76, 74, 72, 70, 56, 54, 52, 50, 60, 58, 56, 54, 64, 62, 60, 58, 68, 66, 64, 62} ;
    size_t b244_startset[3] = {0, 0, 0} ;
    size_t b244_countset[3] = {2, 4, 4} ;
    stat = nc_put_vara(ncid, b244_id, b244_startset, b244_countset, b244_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s311_data[3] = {2500, 2375, 2250} ;
    size_t s311_startset[3] = {0, 0, 0} ;
    size_t s311_countset[3] = {3, 1, 1} ;
    stat = nc_put_vara(ncid, s311_id, s311_startset, s311_countset, s311_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i312_data[6] = {640000, 639980, 632000, 631980, 624000, 623980} ;
    size_t i312_startset[3] = {0, 0, 0} ;
    size_t i312_countset[3] = {3, 1, 2} ;
    stat = nc_put_vara(ncid, i312_id, i312_startset, i312_countset, i312_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f313_data[9] = {26244, 26235, 26226, 25515, 25506, 25497, 24786, 24777, 24768} ;
    size_t f313_startset[3] = {0, 0, 0} ;
    size_t f313_countset[3] = {3, 1, 3} ;
    stat = nc_put_vara(ncid, f313_id, f313_startset, f313_countset, f313_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d314_data[12] = {40000, 39990, 39980, 39970, 39000, 38990, 38980, 38970, 38000, 37990, 37980, 37970} ;
    size_t d314_startset[3] = {0, 0, 0} ;
    size_t d314_countset[3] = {3, 1, 4} ;
    stat = nc_put_vara(ncid, d314_id, d314_startset, d314_countset, d314_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c321_data[6] = "@DHLPT" ;
    size_t c321_startset[3] = {0, 0, 0} ;
    size_t c321_countset[3] = {3, 2, 1} ;
    stat = nc_put_vara(ncid, c321_id, c321_startset, c321_countset, c321_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b322_data[12] = {64, 62, 68, 66, 56, 54, 60, 58, 48, 46, 52, 50} ;
    size_t b322_startset[3] = {0, 0, 0} ;
    size_t b322_countset[3] = {3, 2, 2} ;
    stat = nc_put_vara(ncid, b322_id, b322_startset, b322_countset, b322_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s323_data[18] = {2500, 2495, 2490, 2525, 2520, 2515, 2375, 2370, 2365, 2400, 2395, 2390, 2250, 2245, 2240, 2275, 2270, 2265} ;
    size_t s323_startset[3] = {0, 0, 0} ;
    size_t s323_countset[3] = {3, 2, 3} ;
    stat = nc_put_vara(ncid, s323_id, s323_startset, s323_countset, s323_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i324_data[24] = {640000, 639980, 639960, 639940, 640400, 640380, 640360, 640340, 632000, 631980, 631960, 631940, 632400, 632380, 632360, 632340, 624000, 623980, 623960, 623940, 624400, 624380, 624360, 624340} ;
    size_t i324_startset[3] = {0, 0, 0} ;
    size_t i324_countset[3] = {3, 2, 4} ;
    stat = nc_put_vara(ncid, i324_id, i324_startset, i324_countset, i324_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f331_data[9] = {26244, 26325, 26406, 25515, 25596, 25677, 24786, 24867, 24948} ;
    size_t f331_startset[3] = {0, 0, 0} ;
    size_t f331_countset[3] = {3, 3, 1} ;
    stat = nc_put_vara(ncid, f331_id, f331_startset, f331_countset, f331_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d332_data[18] = {40000, 39990, 40100, 40090, 40200, 40190, 39000, 38990, 39100, 39090, 39200, 39190, 38000, 37990, 38100, 38090, 38200, 38190} ;
    size_t d332_startset[3] = {0, 0, 0} ;
    size_t d332_countset[3] = {3, 3, 2} ;
    stat = nc_put_vara(ncid, d332_id, d332_startset, d332_countset, d332_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c333_data[27] = "@BDDFHHJLHJLLNPPRTPRTTVXXZ\\" ;
    size_t c333_startset[3] = {0, 0, 0} ;
    size_t c333_countset[3] = {3, 3, 3} ;
    stat = nc_put_vara(ncid, c333_id, c333_startset, c333_countset, c333_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b334_data[36] = {64, 62, 60, 58, 68, 66, 64, 62, 72, 70, 68, 66, 56, 54, 52, 50, 60, 58, 56, 54, 64, 62, 60, 58, 48, 46, 44, 42, 52, 50, 48, 46, 56, 54, 52, 50} ;
    size_t b334_startset[3] = {0, 0, 0} ;
    size_t b334_countset[3] = {3, 3, 4} ;
    stat = nc_put_vara(ncid, b334_id, b334_startset, b334_countset, b334_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s341_data[12] = {2500, 2525, 2550, 2575, 2375, 2400, 2425, 2450, 2250, 2275, 2300, 2325} ;
    size_t s341_startset[3] = {0, 0, 0} ;
    size_t s341_countset[3] = {3, 4, 1} ;
    stat = nc_put_vara(ncid, s341_id, s341_startset, s341_countset, s341_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i342_data[24] = {640000, 639980, 640400, 640380, 640800, 640780, 641200, 641180, 632000, 631980, 632400, 632380, 632800, 632780, 633200, 633180, 624000, 623980, 624400, 624380, 624800, 624780, 625200, 625180} ;
    size_t i342_startset[3] = {0, 0, 0} ;
    size_t i342_countset[3] = {3, 4, 2} ;
    stat = nc_put_vara(ncid, i342_id, i342_startset, i342_countset, i342_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f343_data[36] = {26244, 26235, 26226, 26325, 26316, 26307, 26406, 26397, 26388, 26487, 26478, 26469, 25515, 25506, 25497, 25596, 25587, 25578, 25677, 25668, 25659, 25758, 25749, 25740, 24786, 24777, 24768, 24867, 24858, 24849, 24948, 24939, 24930, 25029, 25020, 25011} ;
    size_t f343_startset[3] = {0, 0, 0} ;
    size_t f343_countset[3] = {3, 4, 3} ;
    stat = nc_put_vara(ncid, f343_id, f343_startset, f343_countset, f343_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d344_data[48] = {40000, 39990, 39980, 39970, 40100, 40090, 40080, 40070, 40200, 40190, 40180, 40170, 40300, 40290, 40280, 40270, 39000, 38990, 38980, 38970, 39100, 39090, 39080, 39070, 39200, 39190, 39180, 39170, 39300, 39290, 39280, 39270, 38000, 37990, 37980, 37970, 38100, 38090, 38080, 38070, 38200, 38190, 38180, 38170, 38300, 38290, 38280, 38270} ;
    size_t d344_startset[3] = {0, 0, 0} ;
    size_t d344_countset[3] = {3, 4, 4} ;
    stat = nc_put_vara(ncid, d344_id, d344_startset, d344_countset, d344_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c411_data[4] = "@HPX" ;
    size_t c411_startset[3] = {0, 0, 0} ;
    size_t c411_countset[3] = {4, 1, 1} ;
    stat = nc_put_vara(ncid, c411_id, c411_startset, c411_countset, c411_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b412_data[8] = {64, 62, 56, 54, 48, 46, 40, 38} ;
    size_t b412_startset[3] = {0, 0, 0} ;
    size_t b412_countset[3] = {4, 1, 2} ;
    stat = nc_put_vara(ncid, b412_id, b412_startset, b412_countset, b412_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s413_data[12] = {2500, 2495, 2490, 2375, 2370, 2365, 2250, 2245, 2240, 2125, 2120, 2115} ;
    size_t s413_startset[3] = {0, 0, 0} ;
    size_t s413_countset[3] = {4, 1, 3} ;
    stat = nc_put_vara(ncid, s413_id, s413_startset, s413_countset, s413_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i414_data[16] = {640000, 639980, 639960, 639940, 632000, 631980, 631960, 631940, 624000, 623980, 623960, 623940, 616000, 615980, 615960, 615940} ;
    size_t i414_startset[3] = {0, 0, 0} ;
    size_t i414_countset[3] = {4, 1, 4} ;
    stat = nc_put_vara(ncid, i414_id, i414_startset, i414_countset, i414_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f421_data[8] = {26244, 26325, 25515, 25596, 24786, 24867, 24057, 24138} ;
    size_t f421_startset[3] = {0, 0, 0} ;
    size_t f421_countset[3] = {4, 2, 1} ;
    stat = nc_put_vara(ncid, f421_id, f421_startset, f421_countset, f421_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d422_data[16] = {40000, 39990, 40100, 40090, 39000, 38990, 39100, 39090, 38000, 37990, 38100, 38090, 37000, 36990, 37100, 37090} ;
    size_t d422_startset[3] = {0, 0, 0} ;
    size_t d422_countset[3] = {4, 2, 2} ;
    stat = nc_put_vara(ncid, d422_id, d422_startset, d422_countset, d422_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c423_data[24] = "@BDDFHHJLLNPPRTTVXXZ\\\\^`" ;
    size_t c423_startset[3] = {0, 0, 0} ;
    size_t c423_countset[3] = {4, 2, 3} ;
    stat = nc_put_vara(ncid, c423_id, c423_startset, c423_countset, c423_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b424_data[32] = {64, 62, 60, 58, 68, 66, 64, 62, 56, 54, 52, 50, 60, 58, 56, 54, 48, 46, 44, 42, 52, 50, 48, 46, 40, 38, 36, 34, 44, 42, 40, 38} ;
    size_t b424_startset[3] = {0, 0, 0} ;
    size_t b424_countset[3] = {4, 2, 4} ;
    stat = nc_put_vara(ncid, b424_id, b424_startset, b424_countset, b424_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s431_data[12] = {2500, 2525, 2550, 2375, 2400, 2425, 2250, 2275, 2300, 2125, 2150, 2175} ;
    size_t s431_startset[3] = {0, 0, 0} ;
    size_t s431_countset[3] = {4, 3, 1} ;
    stat = nc_put_vara(ncid, s431_id, s431_startset, s431_countset, s431_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i432_data[24] = {640000, 639980, 640400, 640380, 640800, 640780, 632000, 631980, 632400, 632380, 632800, 632780, 624000, 623980, 624400, 624380, 624800, 624780, 616000, 615980, 616400, 616380, 616800, 616780} ;
    size_t i432_startset[3] = {0, 0, 0} ;
    size_t i432_countset[3] = {4, 3, 2} ;
    stat = nc_put_vara(ncid, i432_id, i432_startset, i432_countset, i432_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    float f433_data[36] = {26244, 26235, 26226, 26325, 26316, 26307, 26406, 26397, 26388, 25515, 25506, 25497, 25596, 25587, 25578, 25677, 25668, 25659, 24786, 24777, 24768, 24867, 24858, 24849, 24948, 24939, 24930, 24057, 24048, 24039, 24138, 24129, 24120, 24219, 24210, 24201} ;
    size_t f433_startset[3] = {0, 0, 0} ;
    size_t f433_countset[3] = {4, 3, 3} ;
    stat = nc_put_vara(ncid, f433_id, f433_startset, f433_countset, f433_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    double d434_data[48] = {40000, 39990, 39980, 39970, 40100, 40090, 40080, 40070, 40200, 40190, 40180, 40170, 39000, 38990, 38980, 38970, 39100, 39090, 39080, 39070, 39200, 39190, 39180, 39170, 38000, 37990, 37980, 37970, 38100, 38090, 38080, 38070, 38200, 38190, 38180, 38170, 37000, 36990, 36980, 36970, 37100, 37090, 37080, 37070, 37200, 37190, 37180, 37170} ;
    size_t d434_startset[3] = {0, 0, 0} ;
    size_t d434_countset[3] = {4, 3, 4} ;
    stat = nc_put_vara(ncid, d434_id, d434_startset, d434_countset, d434_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    char c441_data[16] = "@DHLHLPTPTX\\X\\`d" ;
    size_t c441_startset[3] = {0, 0, 0} ;
    size_t c441_countset[3] = {4, 4, 1} ;
    stat = nc_put_vara(ncid, c441_id, c441_startset, c441_countset, c441_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    signed char b442_data[32] = {64, 62, 68, 66, 72, 70, 76, 74, 56, 54, 60, 58, 64, 62, 68, 66, 48, 46, 52, 50, 56, 54, 60, 58, 40, 38, 44, 42, 48, 46, 52, 50} ;
    size_t b442_startset[3] = {0, 0, 0} ;
    size_t b442_countset[3] = {4, 4, 2} ;
    stat = nc_put_vara(ncid, b442_id, b442_startset, b442_countset, b442_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    short s443_data[48] = {2500, 2495, 2490, 2525, 2520, 2515, 2550, 2545, 2540, 2575, 2570, 2565, 2375, 2370, 2365, 2400, 2395, 2390, 2425, 2420, 2415, 2450, 2445, 2440, 2250, 2245, 2240, 2275, 2270, 2265, 2300, 2295, 2290, 2325, 2320, 2315, 2125, 2120, 2115, 2150, 2145, 2140, 2175, 2170, 2165, 2200, 2195, 2190} ;
    size_t s443_startset[3] = {0, 0, 0} ;
    size_t s443_countset[3] = {4, 4, 3} ;
    stat = nc_put_vara(ncid, s443_id, s443_startset, s443_countset, s443_data);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    int i444_data[64] = {640000, 639980, 639960, 639940, 640400, 640380, 640360, 640340, 640800, 640780, 640760, 640740, 641200, 641180, 641160, 641140, 632000, 631980, 631960, 631940, 632400, 632380, 632360, 632340, 632800, 632780, 632760, 632740, 633200, 633180, 633160, 633140, 624000, 623980, 623960, 623940, 624400, 624380, 624360, 624340, 624800, 624780, 624760, 624740, 625200, 625180, 625160, 625140, 616000, 615980, 615960, 615940, 616400, 616380, 616360, 616340, 616800, 616780, 616760, 616740, 617200, 617180, 617160, 617140} ;
    size_t i444_startset[3] = {0, 0, 0} ;
    size_t i444_countset[3] = {4, 4, 4} ;
    stat = nc_put_vara(ncid, i444_id, i444_startset, i444_countset, i444_data);
    check_err(stat,__LINE__,__FILE__);
    }


    stat = nc_close(ncid);
    check_err(stat,__LINE__,__FILE__);
    return 0;

}

int
main(int argc, char **argv)
{
   printf("\n*** Testing netCDF attributes.\n");
   printf("*** testing attribute renaming for memory leak, like nc_test...");
   {
#define A1_NAME "a"
#define B1_NAME "b"
#define VAR_NAME "var"
      
      int ncid, nvars, v, natts, varid;
      char name_in[NC_MAX_NAME + 1];
      char char_data = 'a';

      /* Create a file with a var with two atts. */
      if (nc_create(FILE_NAME, NC_NETCDF4|NC_CLASSIC_MODEL|NC_CLOBBER, &ncid)) ERR;
      if (nc_def_var(ncid, VAR_NAME, NC_INT, 0, NULL, &varid)) ERR;
      if (nc_put_att(ncid, varid, A1_NAME, NC_CHAR, 1, &char_data)) ERR;
      if (nc_put_att(ncid, varid, B1_NAME, NC_CHAR, 1, &char_data)) ERR;
      
      /* Add a global attribute A1_NAME. */
      if (nc_put_att(ncid, NC_GLOBAL, A1_NAME, NC_CHAR, 1, &char_data)) ERR;

      /* Change the name of the first att of each variable to
       * A1_NAME. Then copy the global att called A1_NAME, overwriting
       * the one we just changed. */
      if (nc_inq_nvars(ncid, &nvars)) ERR;
      if (nvars != 1) ERR_RET;
      if (nc_inq_varnatts(ncid, 0, &natts)) ERR;
      if (natts != 2) ERR;
      if (nc_copy_att(ncid, NC_GLOBAL, A1_NAME, ncid, 0)) ERR;

      /* Also test for fix of another bug, allowing invalid _FillValue
       * attribute (not of same type as variable or with 0 values or more
       * than 1 value) to be created. */
      {
	  static const int var_FillValue_atts[] = {42, -99} ;
	  float var_FillValue_att = -99 ;
	  /* This should return error, because attribute has too many values */
	  if (nc_put_att_int(ncid, varid, "_FillValue", NC_INT, 2, var_FillValue_atts) 
	      != NC_EINVAL) ERR;
	  /* This also should return error, because types don't match */
	  if (nc_put_att_float(ncid, varid, "_FillValue", NC_FLOAT, 1, &var_FillValue_att) 
	      != NC_EBADTYPE) ERR;
	  /* This should succeed, _FillValue is valid */
	  if (nc_put_att_int(ncid, varid, "_FillValue", NC_INT, 1, var_FillValue_atts)) ERR;
      }

      if (nc_close(ncid)) ERR;

      /* Reopen the file and check it. */
      if (nc_open(FILE_NAME, NC_WRITE, &ncid)) ERR;
      if (nc_inq_nvars(ncid, &nvars)) ERR;
      if (nvars != 1) ERR_RET;
      for (v = 0; v < nvars; v++)
      {
      	 if (nc_inq_varnatts(ncid, v, &natts)) ERR_RET;
      	 if (natts)
      	 {
      	    if (nc_inq_attname(ncid, v, 0, name_in)) ERR_RET;
      	    if (strcmp(name_in, A1_NAME)) ERR_RET;
      	 }
      }
      if (nc_close(ncid)) ERR;

   }
   SUMMARIZE_ERR;
   printf("*** testing attribute renaming for memory leak, like nc_test...");
   {
#define NVARS 136
#define A_NAME "a"
      int ncid, nvars, v, natts;
      char name_in[NC_MAX_NAME + 1];
      char char_data = 'a';

      /* Create the same file as nc_test uses (almost). */
      if (create_file()) ERR;

      /* Open the file. */
      if (nc_open(FILE_NAME, NC_WRITE, &ncid)) ERR;
      if (nc_redef(ncid)) ERR;

      /* Add a global attribute A_NAME. */
      if (nc_put_att(ncid, NC_GLOBAL, A_NAME, NC_CHAR, 1,
		     &char_data)) ERR;

      /* Change the name of the first att of each variable to
       * A_NAME. Then copy the global att called A_NAME, overwriting
       * the one we just changed. */
      if (nc_inq_nvars(ncid, &nvars)) ERR;
      if (nvars != NVARS) ERR_RET;
      for (v = 0; v < nvars; v++)
      {
	 if (nc_inq_varnatts(ncid, v, &natts)) ERR_RET;
	 if (natts)
	 {
	    if (nc_inq_attname(ncid, v, 0, name_in)) ERR_RET;
	    if (nc_rename_att(ncid, v, name_in, A_NAME)) ERR_RET;
	    if (nc_copy_att(ncid, NC_GLOBAL, A_NAME, ncid, v)) ERR_RET;
	 }
      }
      if (nc_close(ncid)) ERR;

      /* Reopen the file and check it. */
      if (nc_open(FILE_NAME, NC_WRITE, &ncid)) ERR;
      if (nc_inq_nvars(ncid, &nvars)) ERR;
      if (nvars != NVARS) ERR_RET;
      for (v = 0; v < nvars; v++)
      {
	 if (nc_inq_varnatts(ncid, v, &natts)) ERR_RET;
	 if (natts)
	 {
	    if (nc_inq_attname(ncid, v, 0, name_in)) ERR_RET;
	    if (strcmp(name_in, A_NAME)) ERR_RET;
	 }
      }
      if (nc_close(ncid)) ERR;

   }
   SUMMARIZE_ERR;
   FINAL_RESULTS;
}
