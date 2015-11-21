/*
 * adios_infocache.h
 *
 * Provides a VARINFO/TRANSINFO cache for internal ADIOS use. Code wishing to avoid redundant
 * calls to common_read_inq_var[_transinfo] should instead use the infocache functions as follows:
 *
 *   ADIOS_VARINFO *raw_varinfo = adios_infocache_inq_varinfo(fp, internals->infocache, varid); //common_read_inq_var_raw_byid(fp, varid);        // Get the *raw* varinfo
 *   ADIOS_TRANSINFO *transinfo = adios_infocache_inq_transinfo(fp, internals->infocache, varid); //common_read_inq_transinfo(fp, raw_varinfo);    // Get the transform info (i.e. original var info)
 *
 *  Created on: Nov 21, 2014
 *      Author: David A. Boyuka II
 */
#ifndef ADIOS_INFOCACHE_H_
#define ADIOS_INFOCACHE_H_

#include "public/adios_types.h"
#include "public/adios_read_v2.h"
#include "transforms/adios_transforms_transinfo.h"

typedef struct {
    int capacity;
    ADIOS_VARINFO **physical_varinfos;
    ADIOS_VARINFO **logical_varinfos;
    ADIOS_TRANSINFO **transinfos;
} adios_infocache;


adios_infocache * adios_infocache_new();
void adios_infocache_invalidate(adios_infocache *cache);
void adios_infocache_free(adios_infocache **cache_ptr);

ADIOS_VARINFO * adios_infocache_inq_varinfo(const ADIOS_FILE *fp, adios_infocache *cache, int varid);
ADIOS_TRANSINFO * adios_infocache_inq_transinfo(const ADIOS_FILE *fp, adios_infocache *cache, int varid);

#endif /* ADIOS_INFOCACHE_H_ */
