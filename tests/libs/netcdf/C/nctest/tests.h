/*********************************************************************
 *   Copyright 1993 - 2005, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/tests.h,v 1.8 2006/10/31 16:23:47 ed Exp $
 *********************************************************************/

#include <config.h>

#ifdef __cplusplus
extern "C" {
#endif

   extern int test_nccreate(const char*);
   extern int test_ncopen(const char*);
   extern int test_ncredef(const char*);
   extern int test_ncendef(const char*);
   extern int test_ncclose(const char*);
   extern int test_ncinquire(const char*);
   extern int test_ncsync(const char*);
   extern int test_ncabort(const char*);
   extern int test_ncdimdef(const char*);
   extern int test_ncdimid(const char*);
   extern int test_ncdiminq(const char*);
   extern int test_ncdimrename(const char*);
   extern int test_ncvardef(const char*);
   extern int test_ncvarid(const char*);
   extern int test_ncvarinq(const char*);
   extern int test_ncvarput1(const char*);
   extern int test_ncvarget1(const char*);
   extern int test_ncvarput(const char*);
   extern int test_ncvarget(const char*);
   extern int test_ncvarputg(const char*);
   extern int test_ncvargetg(const char*);
   extern int test_ncrecinq(const char*);
   extern int test_ncrecput(const char*);
   extern int test_ncrecget(const char*);
   extern int test_ncvarrename(const char*);
   extern int test_ncattput(const char*);
   extern int test_ncattinq(const char*);
   extern int test_ncattget(const char*);
   extern int test_ncattcopy(const char*, const char*);
   extern int test_ncattname(const char*);
   extern int test_ncattrename(const char*);
   extern int test_ncattdel(const char*);
   extern int test_nctypelen(void);
   extern int test_varputget(int);
   extern int test_varputgetg(int);
   extern int test_slabs(int);

#ifdef __cplusplus
}
#endif
