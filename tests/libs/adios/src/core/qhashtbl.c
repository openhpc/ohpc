/******************************************************************************
 * qLibc - http://www.qdecoder.org
 *
 * Copyright (c) 2010-2012 Seungyoung Kim.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************/

/**
 * @file qhashtbl.c Hash-table container implementation.
 *
 * qhashtbl implements a hashtable, which maps keys to values. Key is a unique
 * string and value is any non-null object. The creator qHashtbl() has one
 * parameters that affect its performance: initial hash range. The hash range
 * is the number of slots(pointers) in the hash table. in the case of a hash
 * collision, a single slots stores multiple elements using linked-list
 * structure, which must be searched sequentially. So lower range than the
 * number of elements decreases the space overhead but increases the number of
 * hash collisions and consequently it increases the time cost to look up an
 * element.
 *
 * @code
 *  [Internal Structure Example for 10-slot hash table]
 *
 *  RANGE    NAMED-OBJECT-LIST
 *  =====    =================
 *  [ 0 ] -> [hash=320,key3=value] -> [hash=210,key5=value] -> [hash=110,...]
 *  [ 1 ] -> [hash=1,key1=value]
 *  [ 2 ]
 *  [ 3 ] -> [hash=873,key4=value]
 *  [ 4 ] -> [hash=2674,key11=value] -> [hash=214,key5=value]
 *  [ 5 ] -> [hash=8545,key10=value]
 *  [ 6 ] -> [hash=9226,key9=value]
 *  [ 7 ]
 *  [ 8 ] -> [hash=8,key6=value] -> [hash=88,key8=value]
 *  [ 9 ] -> [hash=12439,key7=value]
 * @endcode
 *
 * @code
 *  // create a hash-table with 10 hash-index range.
 *  // Please beaware, the hash-index range 10 does not mean the number of
 *  // objects which can be stored. You can put as many as you want.
 *  qhashtbl_t *tbl = qHashtbl(10);
 *
 *  // put objects into table.
 *  tbl->put(tbl, "sample1", "binary", 6);
 *  tbl->putstr(tbl, "sample2", "string");
 *  tbl->putint(tbl, "sample3", 1);
 *
 *  // debug print out
 *  tbl->debug(tbl, stdout, true);
 *
 *  // get objects
 *  void *sample1 = tbl->get(tbl, "sample1", &size, true);
 *  char *sample2 = tbl->getstr(tbl, "sample2", false);
 *  int  sample3  = tbl->getint(tbl, "sample3");
 *
 *  // sample1 is memalloced
 *  if(sample1 != NULL) free(sample1);
 *
 *  // release table
 *  tbl->free(tbl);
 * @endcode
 *
 * @note
 *  Use "--enable-threadsafe" configure script option to use under
 *  multi-threaded environments.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include "qhashtbl.h"
#include "core/adios_logger.h"


// member methods
static bool put2(qhashtbl_t *tbl, const char *path, const char *name, const void *data);
static bool put(qhashtbl_t *tbl, const char *path, const void *data);
static void *get(qhashtbl_t *tbl, const char *fullpath);
static void *get2(qhashtbl_t *tbl, const char *path, const char *name);
static bool remove_(qhashtbl_t *tbl, const char *fullpath);
static int size(qhashtbl_t *tbl);
static void clear(qhashtbl_t *tbl);
static void debug(qhashtbl_t *tbl, FILE *out, bool detailed);
static void free_(qhashtbl_t *tbl);

uint32_t qhashmurmur3_32(const void *data, size_t nbytes);

/**
 * Initialize hash table.
 *
 * @param range     hash range.
 *
 * @return a pointer of malloced qhashtbl_t, otherwise returns false
 * @retval errno will be set in error condition.
 *  - EINVAL : Invalid argument.
 *  - ENOMEM : Memory allocation failure.
 *
 * @code
 *  // create a hash-table with hash-index range 1000 (does not mean maximum
 *  // number of objects).
 *  qhashtbl_t *tbl = qhashtbl(1000);
 * @endcode
 */
qhashtbl_t *qhashtbl(int range)
{
    if (range == 0) {
        errno = EINVAL;
        return NULL;
    }

    qhashtbl_t *tbl = (qhashtbl_t *)malloc(sizeof(qhashtbl_t));
    if (tbl == NULL) {
        errno = ENOMEM;
        return NULL;
    }
    memset((void *)tbl, 0, sizeof(qhashtbl_t));

    // allocate table space
    tbl->slots = (qhslot_t *)malloc(sizeof(qhslot_t) * range);
    if (tbl->slots == NULL) {
        errno = ENOMEM;
        free_(tbl);
        return NULL;
    }
    memset((void *)tbl->slots, 0, sizeof(qhslot_t) * range);

    // assign methods
    tbl->put2       = put2;
    tbl->put        = put;
    tbl->get        = get;
    tbl->get2       = get2;
    tbl->remove     = remove_;
    tbl->size       = size;
    tbl->clear      = clear;
    tbl->debug      = debug;
    tbl->free       = free_;

    // now table can be used
    tbl->range = range;
    tbl->num = 0;

    // debug variables
    tbl->nwalks_get = 0;
    tbl->ncalls_get = 0;
    tbl->nwalks_put = 0;
    tbl->ncalls_put = 0;

    return tbl;
}

/**
 * qhashtbl->put(): Put a object into this table.
 *
 * @param tbl       qhashtbl_t container pointer.
 * @param path      key is path+/+name
 * @param name      part of key
 * @param data      data object
 *
 * @return true if successful, otherwise returns false
 * @retval errno will be set in error condition.
 *  - EINVAL : Invalid argument.
 *  - ENOMEM : Memory allocation failure.
 */
static void genkey(const char *path, const char *name, int *keylen, char **key)
{
    // create key
    if (!path || !strcmp (path, "")) {
        *keylen = strlen (name);
        *key = malloc (*keylen+1);
        sprintf (*key, "%s", name);
    } else if (!strcmp (path, "/")) {
        *keylen = strlen (name) + 1;
        *key = malloc (*keylen+1);
        sprintf (*key, "/%s", name);
    } else {
        *keylen = strlen (name) + strlen (path) + 1;
        *key = malloc (*keylen+1);
        sprintf (*key, "%s/%s", path, name);
    }
}

static bool qhput(qhashtbl_t *tbl, char *key, int keylen, const void *data)
{
    // get hash integer
    uint32_t hash = qhashmurmur3_32(key, keylen);
    int idx = hash % tbl->range;
    tbl->ncalls_put++; // debug

    //log_error ("qhastbl:put: key=[%s], keylen=%d hash=%d, idx=%d, d=%x\n", key, keylen, hash, idx, data);

    // find existing key
    qhslot_t *slot = &tbl->slots[idx];
    qhnobj_t *obj;
    for (obj = slot->head; obj != NULL; obj = obj->next) {
        if (obj->hash == hash && !strcmp(obj->key, key)) {
            break;
        }
        tbl->nwalks_put++; // debug: we walk one step in a chain of elements 
    }

    // put into table
    if (obj == NULL) {
        // insert
        obj = (qhnobj_t *)malloc(sizeof(qhnobj_t));
        if (obj == NULL) {
            free(key);
            errno = ENOMEM;
            return false;
        }
        memset((void *)obj, 0, sizeof(qhnobj_t));

        if (slot->tail != NULL) {
            // connect old tail to this new tail 
            slot->tail->next = obj;
        }
        if (slot->head == NULL) {
            // insert as very first element
            slot->head = obj;
        }
        slot->tail = obj;
        obj->next = 0;

        // increase counter
        tbl->num++;

        // set data
        obj->hash  = hash;
        obj->key   = key;
        obj->value = (void *)data;

    } else {
        /* Do not do anything.
         * Keep the first definition in place, because consider this example
         * if we would replace the object here:
         *  def NX
         *  def A[NX] --> A's dimension is the first variable NX (a pointer to that)
         *  def NX    --> hashtable stores this variable reference
         *  def B[NX]
         *  write NX  --> value is stored in the NX variable found in the hash table
         *  write A   --> dimension found (valid first pointer) but value is not found
         *                (stored in the second reference)
         *  At this point, A's dimension variable is first NX, but the value of
         *  write NX goes to the variable found here in the hash table.
         */
        free(key);
    }

    return true;
}

static bool put(qhashtbl_t *tbl, const char *fullpath, const void *data)
{
    if (!fullpath)
        return false;

    int keylen = strlen(fullpath);
    char *key = strdup (fullpath);

    return qhput (tbl, key, keylen, data);
}

static bool put2(qhashtbl_t *tbl, const char *path, const char *name, const void *data)
{
    int keylen;
    char *key;
    genkey (path, name, &keylen, &key);

    return qhput (tbl, key, keylen, data);
}


/**
 * qhashtbl->get(): Get a object from this table.
 *
 * @param tbl       qhashtbl_t container pointer.
 * @param name      key name.
 * @param size      if not NULL, oject size will be stored.
 * @param newmem    whether or not to allocate memory for the data.
 *
 * @return a pointer of data if the key is found, otherwise returns NULL.
 * @retval errno will be set in error condition.
 *  - ENOENT : No such key found.
 *  - EINVAL : Invalid argument.
 *  - ENOMEM : Memory allocation failure.
 *
 * @code
 *  qhashtbl_t *tbl = qHashtbl(1000);
 *  (...codes...)
 *
 *  // with newmem flag unset
 *  int size;
 *  struct myobj *obj = (struct myobj*)tbl->get(tbl, "key_name", &size, false);
 *
 *  // with newmem flag set
 *  int size;
 *  struct myobj *obj = (struct myobj*)tbl->get(tbl, "key_name", &size, true);
 *  if(obj != NULL) free(obj);
 * @endcode
 *
 */
static void *qhget(qhashtbl_t *tbl, char *key, int keylen)
{
    // get hash integer
    uint32_t hash = qhashmurmur3_32(key, keylen);
    int idx = hash % tbl->range;
    tbl->ncalls_get++; // debug

    //log_error ("qhastbl:get: key=[%s], keylen=%d, hash=%d, idx=%d\n", fullpath, strlen(fullpath), hash, idx);

    // find key
    qhslot_t *slot = &tbl->slots[idx];
    qhnobj_t *obj;
    for (obj = slot->head; obj != NULL; obj = obj->next) {
        if (obj->hash == hash && !strcmp(obj->key, key)) {
            break;
        }
        tbl->nwalks_get++; // debug: we walk one step in a chain of elements 
    }

    void *data = NULL;
    if (obj != NULL) {
        data = obj->value;
    }

    if (data == NULL) errno = ENOENT;
    //log_error ("qhastbl:get: data=%x\n", data);
    return data;
}

static void *get(qhashtbl_t *tbl, const char *fullpath)
{
    if (!fullpath)
        return NULL;

    int keylen = strlen(fullpath);
    char *key = strdup (fullpath);

    void * data = qhget (tbl, key, keylen);
    free (key);
    return data;
}

static void *get2(qhashtbl_t *tbl, const char *path, const char *name)
{
    int keylen;
    char *key;
    genkey (path, name, &keylen, &key);

    void * data = qhget (tbl, key, keylen);
    free (key);
    return data;
}


/**
 * qhashtbl->remove(): Remove an object from this table.
 *
 * @param tbl   qhashtbl_t container pointer.
 * @param name  key name
 *
 * @return true if successful, otherwise(not found) returns false
 * @retval errno will be set in error condition.
 *  - ENOENT : No next element.
 *  - EINVAL : Invalid argument.
 */
static bool remove_(qhashtbl_t *tbl, const char *fullpath)
{
    int keylen = strlen (fullpath);
    const char *key = fullpath;

    // get hash integer
    uint32_t hash = qhashmurmur3_32(key, keylen);
    int idx = hash % tbl->range;

    //log_error ("qhastbl:remove: key=%s, hash=%d, idx=%d\n", key, hash, idx);

    // find key
    bool found = false;
    qhslot_t *slot = &tbl->slots[idx];
    qhnobj_t *prev = NULL;
    qhnobj_t *obj;
    for (obj = slot->head; obj != NULL; obj = obj->next) {
        if (obj->hash == hash && !strcmp(obj->key, key)) {
            // adjust links
            if (prev == NULL) {
                // remove as very first
                slot->head = obj->next;
            } else {
                // remove otherwise
                prev->next = obj->next;
            }

            if (obj == slot->tail) {
                // this was last element: update tail
                slot->tail = prev;
            }

            // remove
            free(obj->key);
            free(obj);

            found = true;
            tbl->num--;
            break;
        }

        prev = obj;
    }

    if (found == false) errno = ENOENT;

    return found;
}

/**
 * qhashtbl->size(): Returns the number of keys in this hashtable.
 *
 * @param tbl   qhashtbl_t container pointer.
 *
 * @return number of elements stored
 */
static int size(qhashtbl_t *tbl)
{
    return tbl->num;
}

/**
 * qhashtbl->clear(): Clears this hashtable so that it contains no keys.
 *
 * @param tbl   qhashtbl_t container pointer.
 */
void clear(qhashtbl_t *tbl)
{
    if (!tbl) return;
    //debug(tbl, stdout, 0);
    int idx;
    qhnobj_t *obj;
    for (idx = 0; idx < tbl->range && tbl->num > 0; idx++) {
        obj = tbl->slots[idx].head;
        while (obj != NULL) {
            qhnobj_t *next = obj->next;
            free(obj->key);
            free(obj);
            obj = next;
            tbl->num--;
        }
        tbl->slots[idx].head = NULL;
        tbl->slots[idx].tail = NULL;
    }
}

/**
 * qhashtbl->debug(): Print hash table for debugging purpose
 *
 * @param tbl   qhashtbl_t container pointer.
 * @param out   output stream
 *
 */
void debug(qhashtbl_t *tbl, FILE *out, bool detailed)
{
    if (out == NULL) {
        out = stdout;
    }
    int len, lenmin=1000000, lenmax=0;

    qhnobj_t *obj;
    int idx;
    for (idx = 0; idx < tbl->range && tbl->num > 0; idx++) {
        len = 0;
        if (detailed) fprintf(out, "[%d]:", idx);
        obj = tbl->slots[idx].head;
        while (obj != NULL) {
            qhnobj_t *next = obj->next;
            if (detailed) fprintf(out, "(%s,%p)" , obj->key, obj->value);
            obj = next;
            len++;
        }
        if (detailed) fprintf(out,"\n");
        if (len < lenmin) lenmin = len;
        if (len > lenmax) lenmax = len;
    }
    fprintf(out, "Hash table %p\n", tbl);
    fprintf(out, "Hash table size = %d\n", tbl->range);
    fprintf(out, "Number of elements = %d\n", tbl->num);
    fprintf(out, "Shortest collision list size = %d\n", lenmin);
    fprintf(out, "Longest  collision list size = %d\n", lenmax);
    fprintf(out, "get() calls = %d, walks = %d\n", tbl->ncalls_get, tbl->nwalks_get);
    fprintf(out, "put() calls = %d, walks = %d\n", tbl->ncalls_put, tbl->nwalks_put);
    fflush(out);
}


/**
 * qhashtbl->free(): De-allocate hash table
 *
 * @param tbl   qhashtbl_t container pointer.
 */
void free_(qhashtbl_t *tbl)
{
    if (!tbl) return;
    clear(tbl);
    if (tbl->slots != NULL) free(tbl->slots);
    free(tbl);
}


/**
 * Get 32-bit Murmur3 hash.
 *
 * @param data      source data
 * @param nbytes    size of data
 *
 * @return 32-bit unsigned hash value.
 *
 * @code
 *  uint32_t hashval = qhashmurmur3_32((void*)"hello", 5);
 * @endcode
 *
 * @code
 *  MurmurHash3 was created by Austin Appleby  in 2008. The cannonical
 *  implementations are in C++ and placed in the public.
 *
 *    https://sites.google.com/site/murmurhash/
 *
 *  Seungyoung Kim has ported it's cannonical implementation to C language
 *  in 2012 and published it as a part of qLibc component.
 * @endcode
 */
uint32_t qhashmurmur3_32(const void *data, size_t nbytes)
{
    if (data == NULL || nbytes == 0) return 0;

    const uint32_t c1 = 0xcc9e2d51;
    const uint32_t c2 = 0x1b873593;

    const int nblocks = nbytes / 4;
    const uint32_t *blocks = (const uint32_t *)(data);
    const uint8_t *tail = (const uint8_t *)data + (nblocks * 4);

    uint32_t h = 0;

    int i;
    uint32_t k;
    for (i = 0; i < nblocks; i++) {
        k = blocks[i];

        k *= c1;
        k = (k << 15) | (k >> (32 - 15));
        k *= c2;

        h ^= k;
        h = (h << 13) | (h >> (32 - 13));
        h = (h * 5) + 0xe6546b64;
    }

    k = 0;
    switch (nbytes & 3) {
        case 3:
            k ^= tail[2] << 16;
        case 2:
            k ^= tail[1] << 8;
        case 1:
            k ^= tail[0];
            k *= c1;
            k = (k << 13) | (k >> (32 - 15));
            k *= c2;
            h ^= k;
    };

    h ^= nbytes;

    h ^= h >> 16;
    h *= 0x85ebca6b;
    h ^= h >> 13;
    h *= 0xc2b2ae35;
    h ^= h >> 16;

    return h;
}
