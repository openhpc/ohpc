/*
 * adios_infocache.c
 *
 *  Created on: Nov 21, 2014
 *      Author: David A. Boyuka II
 */

#include <stddef.h>
#include <stdlib.h>
#include "core/common_read.h"
#include "core/adios_infocache.h"

// Utilities
static inline int min(int a, int b) { return a < b ? a : b; }
static inline int max(int a, int b) { return a > b ? a : b; }
#define MALLOC_ARRAY(arr,type,len) { (arr) = (type *)malloc((len) * sizeof(type)); }
#define CALLOC_ARRAY(arr,type,len) { (arr) = (type *)calloc((len), sizeof(type)); }
#define REALLOC_ARRAY(arr,type,len) { (arr) = (type *)realloc((arr), (len) * sizeof(type)); }

#define MALLOC(type, var) type *var; MALLOC_ARRAY(var, type, 1);

#define FREE(p) {if (p){free(p); (p)=NULL;}}

#define INITIAL_INFOCACHE_SIZE 16

static void expand_infocache(adios_infocache *cache, int var_capacity) {
    int i;
    const int oldcap = cache->capacity;
    const int newcap = max(max(oldcap * 2, var_capacity), INITIAL_INFOCACHE_SIZE);

    if (oldcap == 0) {
        MALLOC_ARRAY(cache->physical_varinfos, ADIOS_VARINFO*, newcap);
        MALLOC_ARRAY(cache->logical_varinfos, ADIOS_VARINFO*, newcap);
        MALLOC_ARRAY(cache->transinfos, ADIOS_TRANSINFO*, newcap);
    } else {
        REALLOC_ARRAY(cache->physical_varinfos, ADIOS_VARINFO*, newcap);
        REALLOC_ARRAY(cache->logical_varinfos, ADIOS_VARINFO*, newcap);
        REALLOC_ARRAY(cache->transinfos, ADIOS_TRANSINFO*, newcap);
    }

    for (i = oldcap; i < newcap; i++) {
        cache->physical_varinfos[i] = NULL;
        cache->logical_varinfos[i] = NULL;
        cache->transinfos[i] = NULL;
    }

    cache->capacity = newcap;
}

adios_infocache * adios_infocache_new() {
    MALLOC(adios_infocache, cache);
    cache->capacity = 0;
    cache->physical_varinfos = NULL;
    cache->logical_varinfos = NULL;
    cache->transinfos = NULL;

    expand_infocache(cache, INITIAL_INFOCACHE_SIZE);
    return cache;
}

static void invalidate_varinfo(ADIOS_VARINFO **varinfo_ptr) {
	ADIOS_VARINFO *varinfo = *varinfo_ptr;
	if (varinfo) {
		common_read_free_varinfo(varinfo);
		*varinfo_ptr = NULL;
	}
}

static void invalidate_transinfo(const ADIOS_VARINFO *phys_varinfo, ADIOS_TRANSINFO **transinfo_ptr) {
	ADIOS_TRANSINFO *transinfo = *transinfo_ptr;
	if (transinfo) {
		common_read_free_transinfo(phys_varinfo, transinfo);
		*transinfo_ptr = NULL;
	}
}

void adios_infocache_invalidate(adios_infocache *cache) {
    int i;
    for (i = 0; i < cache->capacity; i++) {
    	if (cache->physical_varinfos[i])
        	invalidate_transinfo(cache->physical_varinfos[i], &cache->transinfos[i]);
    	invalidate_varinfo(&cache->physical_varinfos[i]);
    	invalidate_varinfo(&cache->logical_varinfos[i]);
    }
}

void adios_infocache_free(adios_infocache **cache_ptr) {
    adios_infocache *cache = *cache_ptr;

    adios_infocache_invalidate(cache); // Frees all varinfos/transinfos
    FREE(cache->physical_varinfos);
    FREE(cache->logical_varinfos);
    FREE(cache->transinfos);
    cache->capacity = 0;
    FREE(*cache_ptr);
}

ADIOS_VARINFO * adios_infocache_inq_varinfo(const ADIOS_FILE *fp, adios_infocache *cache, int varid) {
    if (varid >= cache->capacity)
        expand_infocache(cache, varid + 1);

    // Choose the varinfo array corresponding to whether this inquiry is
    // in the logical or physical view
    const data_view_t view = common_read_get_data_view(fp);
    ADIOS_VARINFO **varinfos = (view == PHYSICAL_DATA_VIEW) ? cache->physical_varinfos : cache->logical_varinfos;
    ADIOS_VARINFO **varinfo = &varinfos[varid];

    if (*varinfo)
        return *varinfo;
    else
        return *varinfo = common_read_inq_var_byid(fp, varid);
}

ADIOS_TRANSINFO * adios_infocache_inq_transinfo(const ADIOS_FILE *fp, adios_infocache *cache, int varid) {
    if (varid >= cache->capacity)
        expand_infocache(cache, varid + 1);

    if (cache->transinfos[varid]) {
        return cache->transinfos[varid];
    } else {
    	// inq_var in physical view. It probably doesn't matter, but this is the "true"
    	// varinfo as seen by the transport layer, which is the layer to which we
    	// are about to pass the varinfo, so best to make it match.
    	// Note: violate constness temporarily, since we set the view right back again
    	const data_view_t old_view = common_read_set_data_view((ADIOS_FILE*)fp, PHYSICAL_DATA_VIEW);
        ADIOS_VARINFO *vi = adios_infocache_inq_varinfo(fp, cache, varid);
        common_read_set_data_view((ADIOS_FILE*)fp, old_view);

        return cache->transinfos[varid] = common_read_inq_transinfo(fp, vi);
    }
}
