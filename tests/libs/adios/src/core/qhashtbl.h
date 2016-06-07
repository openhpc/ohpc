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
/*
  Modified/simplified by ADIOS team for the purpose of variable/attribute store
*/
#ifndef __HASHTBL_H_
#define __HASHTBL_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>


typedef struct qhnobj_s qhnobj_t;  
typedef struct qhslot_s qhslot_t;  
typedef struct qhashtbl_s qhashtbl_t;

struct qhnobj_s {
    uint32_t hash;     /*!< 32bit-hash value of object name */
    char *key;         /*!< object key */
    void *value;       /*!< object value */
    qhnobj_t *next;    /*!< for chaining next collision object */
};

// Head node in hash table 
struct qhslot_s {
    qhnobj_t *head;    /*!< The first collision object for gets */
    qhnobj_t *tail;    /*!< The last collision object for puts */
};

struct qhashtbl_s {
    /* capsulated member functions */
    bool  (*put)    (qhashtbl_t *tbl, const char *fullpath, const void *data);
    bool  (*put2)   (qhashtbl_t *tbl, const char *path,  const char *name, const void *data);
    void *(*get)    (qhashtbl_t *tbl, const char *fullpath);
    void *(*get2)   (qhashtbl_t *tbl, const char *path,  const char *name);
    bool  (*remove) (qhashtbl_t *tbl, const char *fullpath);

    int   (*size)   (qhashtbl_t *tbl);
    void  (*clear)  (qhashtbl_t *tbl);
    void  (*debug)  (qhashtbl_t *tbl, FILE *out, bool detailed);

    void  (*free)   (qhashtbl_t *tbl);

    /* private variables - do not access directly */
    int num;         /*!< number of objects in this table */
    int range;       /*!< hash range, vertical number of slots */
    qhslot_t *slots; /*!< slot head node */

    /* private debug variables */
    int ncalls_get; // number of calls to get()
    int nwalks_get; // number of walking steps in hash list in get()
    int ncalls_put; // number of calls to put()
    int nwalks_put; // number of walking steps in hash list in put()
};

qhashtbl_t* qhashtbl(int range);

#ifdef __cplusplus
}
#endif

#endif

