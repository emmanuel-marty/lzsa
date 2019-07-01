/*
 * hashmap.h - integer hashmap definitions
 *
 * Copyright (C) 2019 Emmanuel Marty
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 */

/*
 * Uses the libdivsufsort library Copyright (c) 2003-2008 Yuta Mori
 *
 * Inspired by LZ4 by Yann Collet. https://github.com/lz4/lz4
 * With help, ideas, optimizations and speed measurements by spke <zxintrospec@gmail.com>
 * With ideas from Lizard by Przemyslaw Skibinski and Yann Collet. https://github.com/inikep/lizard
 * Also with ideas from smallz4 by Stephan Brumme. https://create.stephan-brumme.com/smallz4/
 *
 */

#ifndef _HASHMAP_H
#define _HASHMAP_H

#include <stdlib.h>

/** Number of hashmap buckets */
#define LZSA_HASH_NBUCKETS 256

/* Forward definitions */
typedef struct _lzsa_hashvalue_t lzsa_hashvalue_t;
typedef struct _lzsa_hashbuffer_t lzsa_hashbuffer_t;

/** One hashmap bucket entry */
typedef struct _lzsa_hashvalue_t {
   lzsa_hashvalue_t *pNext;
   unsigned long long key;
   unsigned int value;
} lzsa_hashvalue_t;

/** One buffer storing hashmap bucket entries */
typedef struct _lzsa_hashbuffer_t {
   lzsa_hashbuffer_t *pNext;
   int nFreeEntryIdx;
   lzsa_hashvalue_t value[255];
} lzsa_hashbuffer_t;

/** Hashmap */
typedef struct {
   lzsa_hashbuffer_t *pBuffer;
   lzsa_hashvalue_t *pBucket[LZSA_HASH_NBUCKETS];
} lzsa_hashmap_t;

/**
 * Initialize hashmap
 *
 * @param pHashMap hashmap
 */
void lzsa_hashmap_init(lzsa_hashmap_t *pHashMap);

/**
 * Set value for key
 *
 * @param pHashMap hashmap
 * @param key key to set value for
 * @param value new value
 */
void lzsa_hashmap_insert(lzsa_hashmap_t *pHashMap, unsigned long long key, unsigned int value);

/**
 * Get value for key
 *
 * @param pHashMap hashmap
 * @param key key to get value for
 * @param pValue pointer to where to store value if found
 *
 * @return 0 if found, nonzero if not found
 */
int lzsa_hashmap_find(lzsa_hashmap_t *pHashMap, unsigned long long key, unsigned int *pValue);

/**
 * Clear hashmap
 *
 * @param pHashMap hashmap
 */
void lzsa_hashmap_clear(lzsa_hashmap_t *pHashMap);

#endif   /* _HASHMAP_H */
