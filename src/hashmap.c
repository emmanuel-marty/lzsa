/*
 * hashmap.c - integer hashmap implementation
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

#include <stdlib.h>
#include <string.h>
#include "hashmap.h"

/**
 * Generate key hash by mixing
 *
 * @param key key to get hash for
 *
 * @return hash
 */
static unsigned int lzsa_hashmap_get_hash(unsigned long long key) {
   key = (~key) + (key << 21);
   key = key ^ (key >> 24);
   key = (key + (key << 3)) + (key << 8);
   key = key ^ (key >> 14);
   key = (key + (key << 2)) + (key << 4);
   key = key ^ (key >> 28);
   key = key + (key << 31);
   return key & (LZSA_HASH_NBUCKETS - 1);
}

/**
 * Initialize hashmap
 *
 * @param pHashMap hashmap
 */
void lzsa_hashmap_init(lzsa_hashmap_t *pHashMap) {
   pHashMap->pBuffer = NULL;
   memset(pHashMap->pBucket, 0, sizeof(lzsa_hashvalue_t *) * LZSA_HASH_NBUCKETS);
}

/**
 * Set value for key
 *
 * @param pHashMap hashmap
 * @param key key to set value for
 * @param value new value
 */
void lzsa_hashmap_insert(lzsa_hashmap_t *pHashMap, unsigned long long key, unsigned int value) {
   unsigned int hash = lzsa_hashmap_get_hash(key);
   lzsa_hashvalue_t **pBucket = &pHashMap->pBucket[hash];
   while (*pBucket) {
      if ((*pBucket)->key == key) {
         (*pBucket)->value = value;
         return;
      }

      pBucket = &((*pBucket)->pNext);
   }

   if (!pHashMap->pBuffer || pHashMap->pBuffer->nFreeEntryIdx >= 255) {
      lzsa_hashbuffer_t *pNewBuffer = (lzsa_hashbuffer_t *)malloc(sizeof(lzsa_hashbuffer_t));
      if (!pNewBuffer) return;

      pNewBuffer->pNext = pHashMap->pBuffer;
      pNewBuffer->nFreeEntryIdx = 0;
      pHashMap->pBuffer = pNewBuffer;
   }

   *pBucket = &pHashMap->pBuffer->value[pHashMap->pBuffer->nFreeEntryIdx++];
   (*pBucket)->pNext = NULL;
   (*pBucket)->key = key;
   (*pBucket)->value = value;
}

/**
 * Get value for key
 *
 * @param pHashMap hashmap
 * @param key key to get value for
 * @param pValue pointer to where to store value if found
 *
 * @return 0 if found, nonzero if not found
 */
int lzsa_hashmap_find(lzsa_hashmap_t *pHashMap, unsigned long long key, unsigned int *pValue) {
   unsigned int hash = lzsa_hashmap_get_hash(key);
   lzsa_hashvalue_t **pBucket = &pHashMap->pBucket[hash];
   while (*pBucket) {
      if ((*pBucket)->key == key) {
         *pValue = (*pBucket)->value;
         return 0;
      }

      pBucket = &((*pBucket)->pNext);
   }

   return -1;
}

/**
 * Clear hashmap
 *
 * @param pHashMap hashmap
 */
void lzsa_hashmap_clear(lzsa_hashmap_t *pHashMap) {
   while (pHashMap->pBuffer) {
      lzsa_hashbuffer_t *pCurBuffer = pHashMap->pBuffer;
      pHashMap->pBuffer = pCurBuffer->pNext;
      free(pCurBuffer);
      pCurBuffer = NULL;
   }

   memset(pHashMap->pBucket, 0, sizeof(lzsa_hashvalue_t *) * LZSA_HASH_NBUCKETS);
}

