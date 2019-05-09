/*
 * expand_v2.c - LZSA2 block decompressor implementation
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "format.h"
#include "expand_v2.h"

#ifdef _MSC_VER
#define FORCE_INLINE __forceinline
#else /* _MSC_VER */
#define FORCE_INLINE __attribute__((always_inline))
#endif /* _MSC_VER */

static inline FORCE_INLINE unsigned int lzsa_get_nibble_v2(const unsigned char **ppInBlock, const unsigned char *pInBlockEnd, int *nCurNibbles, unsigned char *nibbles) {
   unsigned int nValue;

   if ((*nCurNibbles ^= 1) != 0) {
      const unsigned char *pInBlock = *ppInBlock;
      if (pInBlock >= pInBlockEnd) return -1;
      (*nibbles) = *pInBlock++;
      *ppInBlock = pInBlock;
   }

   nValue = ((unsigned int)((*nibbles) & 0xf0)) >> 4;

   (*nibbles) <<= 4;

   return nValue;
}

static inline FORCE_INLINE int lzsa_expand_literals_slow_v2(const unsigned char **ppInBlock, const unsigned char *pInBlockEnd, unsigned int nLiterals, int *nCurNibbles, unsigned char *nibbles,
      unsigned char **ppCurOutData, const unsigned char *pOutDataEnd) {
   const unsigned char *pInBlock = *ppInBlock;
   unsigned char *pCurOutData = *ppCurOutData;

   if (nLiterals == LITERALS_RUN_LEN_V2) {
      nLiterals += lzsa_get_nibble_v2(&pInBlock, pInBlockEnd, nCurNibbles, nibbles);

      if (nLiterals == (LITERALS_RUN_LEN_V2 + 15)) {
         if (pInBlock < pInBlockEnd) {
            nLiterals = ((unsigned int)*pInBlock++);

            if (nLiterals == 0) {
               if ((pInBlock + 1) < pInBlockEnd) {
                  nLiterals = ((unsigned int)*pInBlock++);
                  nLiterals |= (((unsigned int)*pInBlock++) << 8);
               }
               else {
                  return -1;
               }
            }
         }
         else {
            return -1;
         }
      }
   }

   if (nLiterals != 0) {
      if ((pInBlock + nLiterals) <= pInBlockEnd &&
         (pCurOutData + nLiterals) <= pOutDataEnd) {
         memcpy(pCurOutData, pInBlock, nLiterals);
         pInBlock += nLiterals;
         pCurOutData += nLiterals;
      }
      else {
         return -1;
      }
   }

   *ppInBlock = pInBlock;
   *ppCurOutData = pCurOutData;
   return 0;
}

static inline FORCE_INLINE int lzsa_expand_match_slow_v2(const unsigned char **ppInBlock, const unsigned char *pInBlockEnd, const unsigned char *pSrc, unsigned int nMatchLen, int *nCurNibbles, unsigned char *nibbles,
      unsigned char **ppCurOutData, const unsigned char *pOutDataEnd, const unsigned char *pOutDataFastEnd) {
   const unsigned char *pInBlock = *ppInBlock;
   unsigned char *pCurOutData = *ppCurOutData;

   nMatchLen += MIN_MATCH_SIZE_V2;
   if (nMatchLen == (MATCH_RUN_LEN_V2 + MIN_MATCH_SIZE_V2)) {
      nMatchLen += lzsa_get_nibble_v2(&pInBlock, pInBlockEnd, nCurNibbles, nibbles);

      if (nMatchLen == (MATCH_RUN_LEN_V2 + MIN_MATCH_SIZE_V2 + 15)) {
         if (pInBlock < pInBlockEnd) {
            nMatchLen = ((unsigned int)*pInBlock++);

            if (nMatchLen == 0) {
               if ((pInBlock + 1) < pInBlockEnd) {
                  nMatchLen = ((unsigned int)*pInBlock++);
                  nMatchLen |= (((unsigned int)*pInBlock++) << 8);
               }
               else {
                  return -1;
               }
            }
         }
         else {
            return -1;
         }
      }
   }

   if ((pCurOutData + nMatchLen) <= pOutDataEnd) {
      /* Do a deterministic, left to right byte copy instead of memcpy() so as to handle overlaps */

      if ((pCurOutData - pSrc) >= 8 && (pCurOutData + nMatchLen) < (pOutDataFastEnd - 15)) {
         const unsigned char *pCopySrc = pSrc;
         unsigned char *pCopyDst = pCurOutData;
         const unsigned char *pCopyEndDst = pCurOutData + nMatchLen;

         do {
            memcpy(pCopyDst, pCopySrc, 8);
            memcpy(pCopyDst + 8, pCopySrc + 8, 8);
            pCopySrc += 16;
            pCopyDst += 16;
         } while (pCopyDst < pCopyEndDst);

         pCurOutData += nMatchLen;
      }
      else {
         while (nMatchLen >= 4) {
            *pCurOutData++ = *pSrc++;
            *pCurOutData++ = *pSrc++;
            *pCurOutData++ = *pSrc++;
            *pCurOutData++ = *pSrc++;
            nMatchLen -= 4;
         }
         while (nMatchLen) {
            *pCurOutData++ = *pSrc++;
            nMatchLen--;
         }
      }
   }
   else {
      return -1;
   }

   *ppInBlock = pInBlock;
   *ppCurOutData = pCurOutData;
   return 0;
}

/**
 * Decompress one LZSA2 data block
 *
 * @param pInBlock pointer to compressed data
 * @param nInBlockSize size of compressed data, in bytes
 * @param pOutData pointer to output decompression buffer (previously decompressed bytes + room for decompressing this block)
 * @param nOutDataOffset starting index of where to store decompressed bytes in output buffer (and size of previously decompressed bytes)
 * @param nBlockMaxSize total size of output decompression buffer, in bytes
 *
 * @return size of decompressed data in bytes, or -1 for error
 */
int lzsa_expand_block_v2(const unsigned char *pInBlock, int nBlockSize, unsigned char *pOutData, int nOutDataOffset, int nBlockMaxSize) {
   const unsigned char *pInBlockEnd = pInBlock + nBlockSize;
   const unsigned char *pInBlockFastEnd = pInBlock + nBlockSize - 8;
   unsigned char *pCurOutData = pOutData + nOutDataOffset;
   const unsigned char *pOutDataEnd = pCurOutData + nBlockMaxSize;
   const unsigned char *pOutDataFastEnd = pOutDataEnd - 20;
   int nCurNibbles = 0;
   unsigned char nibbles;
   int nMatchOffset = 0;

   /* Fast loop */

   while (pInBlock < pInBlockFastEnd && pCurOutData < pOutDataFastEnd) {
      const unsigned char token = *pInBlock++;
      unsigned int nLiterals = (unsigned int)((token & 0x18) >> 3);

      if (nLiterals < LITERALS_RUN_LEN_V2) {
         memcpy(pCurOutData, pInBlock, 8);
         pInBlock += nLiterals;
         pCurOutData += nLiterals;
      }
      else {
         if (lzsa_expand_literals_slow_v2(&pInBlock, pInBlockEnd, nLiterals, &nCurNibbles, &nibbles, &pCurOutData, pOutDataEnd))
            return -1;
      }

      if ((pInBlock + 1) < pInBlockEnd) { /* The last token in the block does not include match information */
         unsigned char nOffsetMode = token & 0xc0;

         switch (nOffsetMode) {
         case 0x00:
            /* 5 bit offset */
            nMatchOffset = (unsigned int)lzsa_get_nibble_v2(&pInBlock, pInBlockEnd, &nCurNibbles, &nibbles);
            nMatchOffset |= ((token & 0x20) >> 1);
            nMatchOffset |= 0xffffffe0;
            break;

         case 0x40:
            /* 9 bit offset */
            nMatchOffset = (unsigned int)(*pInBlock++);
            nMatchOffset |= (((unsigned int)(token & 0x20)) << 3);
            nMatchOffset |= 0xfffffe00;
            break;

         case 0x80:
            /* 13 bit offset */
            nMatchOffset = (unsigned int)(*pInBlock++);
            nMatchOffset |= (lzsa_get_nibble_v2(&pInBlock, pInBlockEnd, &nCurNibbles, &nibbles) << 8);
            nMatchOffset |= (((unsigned int)(token & 0x20)) << 7);
            nMatchOffset |= 0xffffe000;
            nMatchOffset -= 512;
            break;

         default:
            /* Check if this is a 16 bit offset or a rep-match */
            if ((token & 0x20) == 0) {
               /* 16 bit offset */
               nMatchOffset = (unsigned int)(*pInBlock++);
               nMatchOffset |= (((unsigned int)(*pInBlock++)) << 8);
               nMatchOffset |= 0xffff0000;
            }
            break;
         }

         const unsigned char *pSrc = pCurOutData + nMatchOffset;
         if (pSrc >= pOutData) {
            unsigned int nMatchLen = (unsigned int)(token & 0x07);
            if (nMatchLen < MATCH_RUN_LEN_V2 && nMatchOffset >= 8 && pCurOutData < pOutDataFastEnd) {
               memcpy(pCurOutData, pSrc, 8);
               memcpy(pCurOutData + 8, pSrc + 8, 4);
               pCurOutData += (MIN_MATCH_SIZE_V2 + nMatchLen);
            }
            else {
               if (lzsa_expand_match_slow_v2(&pInBlock, pInBlockEnd, pSrc, nMatchLen, &nCurNibbles, &nibbles, &pCurOutData, pOutDataEnd, pOutDataFastEnd))
                  return -1;
            }
         }
         else {
            return -1;
         }
      }
   }

   /* Slow loop for the remainder of the buffer */

   while (pInBlock < pInBlockEnd) {
      const unsigned char token = *pInBlock++;
      unsigned int nLiterals = (unsigned int)((token & 0x18) >> 3);

      if (lzsa_expand_literals_slow_v2(&pInBlock, pInBlockEnd, nLiterals, &nCurNibbles, &nibbles, &pCurOutData, pOutDataEnd))
         return -1;

      if ((pInBlock + 1) < pInBlockEnd) { /* The last token in the block does not include match information */
         unsigned char nOffsetMode = token & 0xc0;

         switch (nOffsetMode) {
         case 0x00:
            /* 5 bit offset */
            nMatchOffset = (unsigned int)lzsa_get_nibble_v2(&pInBlock, pInBlockEnd, &nCurNibbles, &nibbles);
            nMatchOffset |= ((token & 0x20) >> 1);
            nMatchOffset |= 0xffffffe0;
            break;

         case 0x40:
            /* 9 bit offset */
            nMatchOffset = (unsigned int)(*pInBlock++);
            nMatchOffset |= (((unsigned int)(token & 0x20)) << 3);
            nMatchOffset |= 0xfffffe00;
            break;

         case 0x80:
            /* 13 bit offset */
            nMatchOffset = (unsigned int)(*pInBlock++);
            nMatchOffset |= (lzsa_get_nibble_v2(&pInBlock, pInBlockEnd, &nCurNibbles, &nibbles) << 8);
            nMatchOffset |= (((unsigned int)(token & 0x20)) << 7);
            nMatchOffset |= 0xffffe000;
            nMatchOffset -= 512;
            break;

         default:
            /* Check if this is a 16 bit offset or a rep-match */
            if ((token & 0x20) == 0) {
               /* 16 bit offset */
               nMatchOffset = (unsigned int)(*pInBlock++);
               nMatchOffset |= (((unsigned int)(*pInBlock++)) << 8);
               nMatchOffset |= 0xffff0000;
            }
            break;
         }

         const unsigned char *pSrc = pCurOutData + nMatchOffset;
         if (pSrc >= pOutData) {
            unsigned int nMatchLen = (unsigned int)(token & 0x07);
            if (lzsa_expand_match_slow_v2(&pInBlock, pInBlockEnd, pSrc, nMatchLen, &nCurNibbles, &nibbles, &pCurOutData, pOutDataEnd, pOutDataFastEnd))
               return -1;
         }
         else {
            return -1;
         }
      }
   }

   return (int)(pCurOutData - (pOutData + nOutDataOffset));
}
