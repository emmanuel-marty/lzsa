/*
 * shrink_block_v2.c - LZSA2 block compressor implementation
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
#include "lib.h"
#include "shrink_block_v2.h"
#include "format.h"
#include "hashmap.h"
#include "matchfinder.h"

/**
 * Write 4-bit nibble to output (compressed) buffer
 *
 * @param pOutData pointer to output buffer
 * @param nOutOffset current write index into output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 * @param nCurNibbleOffset write index into output buffer, of current byte being filled with nibbles
 * @param nCurFreeNibbles current number of free nibbles in byte
 * @param nNibbleValue value to write (0..15)
 */
static int lzsa_write_nibble_v2(unsigned char *pOutData, int nOutOffset, const int nMaxOutDataSize, int *nCurNibbleOffset, int *nCurFreeNibbles, int nNibbleValue) {
   if (nOutOffset < 0) return -1;

   if ((*nCurNibbleOffset) == -1) {
      if (nOutOffset >= nMaxOutDataSize) return -1;
      (*nCurNibbleOffset) = nOutOffset;
      (*nCurFreeNibbles) = 2;
      pOutData[nOutOffset++] = 0;
   }

   pOutData[*nCurNibbleOffset] = (pOutData[*nCurNibbleOffset] << 4) | (nNibbleValue & 0x0f);
   (*nCurFreeNibbles)--;
   if ((*nCurFreeNibbles) == 0) {
      (*nCurNibbleOffset) = -1;
   }

   return nOutOffset;
}

/**
 * Get the number of extra bits required to represent a literals length
 *
 * @param nLength literals length
 *
 * @return number of extra bits required
 */
static inline int lzsa_get_literals_varlen_size_v2(const int nLength) {
   if (nLength < LITERALS_RUN_LEN_V2) {
      return 0;
   }
   else {
      if (nLength < (LITERALS_RUN_LEN_V2 + 15)) {
         return 4;
      }
      else {
         if (nLength < 256)
            return 4+8;
         else {
            return 4+24;
         }
      }
   }
}

/**
 * Write extra literals length bytes to output (compressed) buffer. The caller must first check that there is enough
 * room to write the bytes.
 *
 * @param pOutData pointer to output buffer
 * @param nOutOffset current write index into output buffer
 * @param nLength literals length
 */
static inline int lzsa_write_literals_varlen_v2(unsigned char *pOutData, int nOutOffset, const int nMaxOutDataSize, int *nCurNibbleOffset, int *nCurFreeNibbles, int nLength) {
   if (nLength >= LITERALS_RUN_LEN_V2) {
      if (nLength < (LITERALS_RUN_LEN_V2 + 15)) {
         nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, nCurNibbleOffset, nCurFreeNibbles, nLength - LITERALS_RUN_LEN_V2);
      }
      else {
         nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, nCurNibbleOffset, nCurFreeNibbles, 15);
         if (nOutOffset < 0) return -1;

         if (nLength < 256)
            pOutData[nOutOffset++] = nLength - 18;
         else {
            pOutData[nOutOffset++] = 239;
            pOutData[nOutOffset++] = nLength & 0xff;
            pOutData[nOutOffset++] = (nLength >> 8) & 0xff;
         }
      }
   }

   return nOutOffset;
}

/**
 * Get the number of extra bits required to represent an encoded match length
 *
 * @param nLength encoded match length (actual match length - MIN_MATCH_SIZE_V2)
 *
 * @return number of extra bits required
 */
static inline int lzsa_get_match_varlen_size_v2(const int nLength) {
   if (nLength < MATCH_RUN_LEN_V2) {
      return 0;
   }
   else {
      if (nLength < (MATCH_RUN_LEN_V2 + 15))
         return 4;
      else {
         if ((nLength + MIN_MATCH_SIZE_V2) < 256)
            return 4+8;
         else {
            return 4 + 24;
         }
      }
   }
}

/**
 * Write extra encoded match length bytes to output (compressed) buffer. The caller must first check that there is enough
 * room to write the bytes.
 *
 * @param pOutData pointer to output buffer
 * @param nOutOffset current write index into output buffer
 * @param nLength encoded match length (actual match length - MIN_MATCH_SIZE_V2)
 */
static inline int lzsa_write_match_varlen_v2(unsigned char *pOutData, int nOutOffset, const int nMaxOutDataSize, int *nCurNibbleOffset, int *nCurFreeNibbles, int nLength) {
   if (nLength >= MATCH_RUN_LEN_V2) {
      if (nLength < (MATCH_RUN_LEN_V2 + 15)) {
         nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, nCurNibbleOffset, nCurFreeNibbles, nLength - MATCH_RUN_LEN_V2);
      }
      else {
         nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, nCurNibbleOffset, nCurFreeNibbles, 15);
         if (nOutOffset < 0) return -1;

         if ((nLength + MIN_MATCH_SIZE_V2) < 256)
            pOutData[nOutOffset++] = nLength + MIN_MATCH_SIZE_V2 - 24;
         else {
            pOutData[nOutOffset++] = 233;
            pOutData[nOutOffset++] = (nLength + MIN_MATCH_SIZE_V2) & 0xff;
            pOutData[nOutOffset++] = ((nLength + MIN_MATCH_SIZE_V2) >> 8) & 0xff;
         }
      }
   }

   return nOutOffset;
}

/**
 * Attempt to pick optimal matches using a forward arrivals parser, so as to produce the smallest possible output that decompresses to the same input
 *
 * @param pCompressor compression context
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 */
static void lzsa_optimize_forward_v2(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset) {
   lzsa_arrival *arrival = pCompressor->arrival;
   const int nFavorRatio = (pCompressor->flags & LZSA_FLAG_FAVOR_RATIO) ? 1 : 0;
   const int nMinMatchSize = pCompressor->min_match_size;
   int i, j, n;
   lzsa_match match[32];

   memset(arrival + (nStartOffset << MATCHES_PER_OFFSET_SHIFT), 0, sizeof(lzsa_arrival) * ((nEndOffset - nStartOffset) << MATCHES_PER_OFFSET_SHIFT));

   for (i = (nStartOffset << MATCHES_PER_OFFSET_SHIFT); i != (nEndOffset << MATCHES_PER_OFFSET_SHIFT); i++) {
      arrival[i].cost = 0x40000000;
   }

   arrival[nStartOffset << MATCHES_PER_OFFSET_SHIFT].from_slot = -1;

   for (i = nStartOffset; i != (nEndOffset - 1); i++) {
      int m, nMatches;

      for (j = 0; j < NMATCHES_PER_OFFSET && arrival[(i << MATCHES_PER_OFFSET_SHIFT) + j].from_slot; j++) {
         const int nPrevCost = arrival[(i << MATCHES_PER_OFFSET_SHIFT) + j].cost & 0x3fffffff;
         int nCodingChoiceCost = nPrevCost + 8 /* literal */;
         int nNumLiterals = arrival[(i << MATCHES_PER_OFFSET_SHIFT) + j].num_literals + 1;

         if (nNumLiterals == LITERALS_RUN_LEN_V2) {
            nCodingChoiceCost += 4;
         }
         else if (nNumLiterals == (LITERALS_RUN_LEN_V2 + 15)) {
            nCodingChoiceCost += 8;
         }
         else if (nNumLiterals == 256) {
            nCodingChoiceCost += 16;
         }

         if (!nFavorRatio && nNumLiterals == 1)
            nCodingChoiceCost += MODESWITCH_PENALTY;

         lzsa_arrival *pDestSlots = &arrival[(i + 1) << MATCHES_PER_OFFSET_SHIFT];
         if (nCodingChoiceCost <= pDestSlots[NMATCHES_PER_OFFSET - 1].cost) {
            int exists = 0;
            for (n = 0;
               n < NMATCHES_PER_OFFSET && pDestSlots[n].cost <= nCodingChoiceCost;
               n++) {
               if (pDestSlots[n].rep_offset == arrival[(i << MATCHES_PER_OFFSET_SHIFT) + j].rep_offset) {
                  exists = 1;
                  break;
               }
            }

            if (!exists) {
               for (n = 0; n < NMATCHES_PER_OFFSET; n++) {
                  lzsa_arrival *pDestArrival = &pDestSlots[n];
                  if (nCodingChoiceCost <= pDestArrival->cost) {

                     if (pDestArrival->from_slot) {
                        memmove(&pDestSlots[n + 1],
                           &pDestSlots[n],
                           sizeof(lzsa_arrival) * (NMATCHES_PER_OFFSET - n - 1));
                     }

                     pDestArrival->cost = nCodingChoiceCost;
                     pDestArrival->from_pos = i;
                     pDestArrival->from_slot = j + 1;
                     pDestArrival->match_offset = 0;
                     pDestArrival->match_len = 0;
                     pDestArrival->num_literals = nNumLiterals;
                     pDestArrival->rep_offset = arrival[(i << MATCHES_PER_OFFSET_SHIFT) + j].rep_offset;
                     break;
                  }
               }
            }
         }
      }

      nMatches = lzsa_find_matches_at(pCompressor, i, match, 32);

      for (m = 0; m < nMatches; m++) {
         int nMatchLen = match[m].length;
         int nMatchOffset = match[m].offset;
         int nNoRepmatchOffsetCost = (nMatchOffset <= 32) ? 4 : ((nMatchOffset <= 512) ? 8 : ((nMatchOffset <= (8192 + 512)) ? 12 : 16));
         int nStartingMatchLen, k;
         int nMaxRepLen[NMATCHES_PER_OFFSET];

         if ((i + nMatchLen) > (nEndOffset - LAST_LITERALS))
            nMatchLen = nEndOffset - LAST_LITERALS - i;

         for (j = 0; j < NMATCHES_PER_OFFSET && arrival[(i << MATCHES_PER_OFFSET_SHIFT) + j].from_slot; j++) {
            int nRepOffset = arrival[(i << MATCHES_PER_OFFSET_SHIFT) + j].rep_offset;
            int nCurMaxRepLen = 0;

            if (nMatchOffset != nRepOffset &&
               nRepOffset &&
               i >= nRepOffset &&
               (i - nRepOffset + nMatchLen) <= (nEndOffset - LAST_LITERALS)) {
               while (nCurMaxRepLen < nMatchLen && pInWindow[i - nRepOffset + nCurMaxRepLen] == pInWindow[i - nMatchOffset + nCurMaxRepLen])
                  nCurMaxRepLen++;
            }

            nMaxRepLen[j] = nCurMaxRepLen;
         }
         while (j < NMATCHES_PER_OFFSET)
            nMaxRepLen[j++] = 0;

         if (nMatchLen >= LEAVE_ALONE_MATCH_SIZE)
            nStartingMatchLen = nMatchLen;
         else
            nStartingMatchLen = nMinMatchSize;

         for (k = nStartingMatchLen; k <= nMatchLen; k++) {
            int nMatchLenCost = lzsa_get_match_varlen_size_v2(k - MIN_MATCH_SIZE_V2);
            lzsa_arrival *pDestSlots = &arrival[(i + k) << MATCHES_PER_OFFSET_SHIFT];

            for (j = 0; j < NMATCHES_PER_OFFSET && arrival[(i << MATCHES_PER_OFFSET_SHIFT) + j].from_slot; j++) {
               const int nPrevCost = arrival[(i << MATCHES_PER_OFFSET_SHIFT) + j].cost & 0x3fffffff;
               int nRepOffset = arrival[(i << MATCHES_PER_OFFSET_SHIFT) + j].rep_offset;

               int nMatchOffsetCost = (nMatchOffset == nRepOffset) ? 0 : nNoRepmatchOffsetCost;
               int nRepCodingChoiceCost = nPrevCost + 8 /* token */ /* the actual cost of the literals themselves accumulates up the chain */ + nMatchLenCost;
               int nCodingChoiceCost = nRepCodingChoiceCost + nMatchOffsetCost;

               if (!nFavorRatio && !arrival[(i << MATCHES_PER_OFFSET_SHIFT) + j].num_literals)
                  nCodingChoiceCost += MODESWITCH_PENALTY;

               if (nRepCodingChoiceCost <= pDestSlots[NMATCHES_PER_OFFSET - 1].cost) {
                  if (nCodingChoiceCost <= pDestSlots[NMATCHES_PER_OFFSET - 1].cost) {
                     int exists = 0;

                     for (n = 0;
                        n < NMATCHES_PER_OFFSET && pDestSlots[n].cost <= nCodingChoiceCost;
                        n++) {
                        if (pDestSlots[n].rep_offset == nMatchOffset) {
                           exists = 1;
                           break;
                        }
                     }

                     if (!exists) {
                        for (n = 0; n < NMATCHES_PER_OFFSET; n++) {
                           lzsa_arrival *pDestArrival = &pDestSlots[n];

                           if (nCodingChoiceCost <= pDestArrival->cost) {
                              if (pDestArrival->from_slot) {
                                 memmove(&pDestSlots[n + 1],
                                    &pDestSlots[n],
                                    sizeof(lzsa_arrival) * (NMATCHES_PER_OFFSET - n - 1));
                              }

                              pDestArrival->cost = nCodingChoiceCost;
                              pDestArrival->from_pos = i;
                              pDestArrival->from_slot = j + 1;
                              pDestArrival->match_offset = nMatchOffset;
                              pDestArrival->match_len = k;
                              pDestArrival->num_literals = 0;
                              pDestArrival->rep_offset = nMatchOffset;
                              break;
                           }
                        }
                     }
                  }

                  /* If this coding choice doesn't rep-match, see if we still get a match by using the current repmatch offset for this arrival. This can occur (and not have the
                   * matchfinder offer the offset in the first place, or have too many choices with the same cost to retain the repmatchable offset) when compressing regions
                   * of identical bytes, for instance. Checking for this provides a big compression win on some files. */

                  if (nMaxRepLen[j] >= k) {
                     int exists = 0;

                     /* A match is possible at the rep offset; insert the extra coding choice. */

                     for (n = 0;
                        n < NMATCHES_PER_OFFSET && pDestSlots[n].cost <= nRepCodingChoiceCost;
                        n++) {
                        if (pDestSlots[n].rep_offset == nRepOffset) {
                           exists = 1;
                           break;
                        }
                     }

                     if (!exists) {
                        for (n = 0; n < NMATCHES_PER_OFFSET; n++) {
                           lzsa_arrival *pDestArrival = &pDestSlots[n];

                           if (nRepCodingChoiceCost <= pDestArrival->cost) {
                              if (pDestArrival->from_slot) {
                                 memmove(&pDestSlots[n + 1],
                                    &pDestSlots[n],
                                    sizeof(lzsa_arrival) * (NMATCHES_PER_OFFSET - n - 1));
                              }

                              pDestArrival->cost = nRepCodingChoiceCost;
                              pDestArrival->from_pos = i;
                              pDestArrival->from_slot = j + 1;
                              pDestArrival->match_offset = nRepOffset;
                              pDestArrival->match_len = k;
                              pDestArrival->num_literals = 0;
                              pDestArrival->rep_offset = nRepOffset;
                              break;
                           }
                        }
                     }
                  }
               }
            }
         }
      }
   }

   lzsa_arrival *end_arrival = &arrival[(i << MATCHES_PER_OFFSET_SHIFT) + 0];
   pCompressor->best_match[i].length = 0;
   pCompressor->best_match[i].offset = 0;

   while (end_arrival->from_slot > 0 && end_arrival->from_pos >= 0) {
      pCompressor->best_match[end_arrival->from_pos].length = end_arrival->match_len;
      pCompressor->best_match[end_arrival->from_pos].offset = end_arrival->match_offset;
      end_arrival = &arrival[(end_arrival->from_pos << MATCHES_PER_OFFSET_SHIFT) + (end_arrival->from_slot - 1)];
   }
}

/**
 * Attempt to minimize the number of commands issued in the compressed data block, in order to speed up decompression without
 * impacting the compression ratio
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param pBestMatch optimal matches to evaluate and update
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 *
 * @return non-zero if the number of tokens was reduced, 0 if it wasn't
 */
static int lzsa_optimize_command_count_v2(lzsa_compressor *pCompressor, const unsigned char *pInWindow, lzsa_match *pBestMatch, const int nStartOffset, const int nEndOffset) {
   int i;
   int nNumLiterals = 0;
   int nPrevRepMatchOffset = 0;
   int nRepMatchOffset = 0;
   int nRepMatchLen = 0;
   int nRepIndex = 0;
   int nDidReduce = 0;

   for (i = nStartOffset; i < nEndOffset; ) {
      lzsa_match *pMatch = pBestMatch + i;

      if (pMatch->length >= MIN_MATCH_SIZE_V2) {
         if ((i + pMatch->length) < nEndOffset /* Don't consider the last match in the block, we can only reduce a match inbetween other tokens */) {
            int nNextIndex = i + pMatch->length;
            int nNextLiterals = 0;

            while (nNextIndex < nEndOffset && pBestMatch[nNextIndex].length < MIN_MATCH_SIZE_V2) {
               nNextLiterals++;
               nNextIndex++;
            }

            if (nNextIndex < nEndOffset && pBestMatch[nNextIndex].length >= MIN_MATCH_SIZE_V2) {
               /* This command is a match, is followed by 'nNextLiterals' literals and then by another match */

               if (nRepMatchOffset && pMatch->offset != nRepMatchOffset && (pBestMatch[nNextIndex].offset != pMatch->offset || pBestMatch[nNextIndex].offset == nRepMatchOffset ||
                  ((pMatch->offset <= 32) ? 4 : ((pMatch->offset <= 512) ? 8 : ((pMatch->offset <= (8192 + 512)) ? 12 : 16))) >
                  ((pBestMatch[nNextIndex].offset <= 32) ? 4 : ((pBestMatch[nNextIndex].offset <= 512) ? 8 : ((pBestMatch[nNextIndex].offset <= (8192 + 512)) ? 12 : 16))))) {
                  /* Check if we can change the current match's offset to be the same as the previous match's offset, and get an extra repmatch. This will occur when
                   * matching large regions of identical bytes for instance, where there are too many offsets to be considered by the parser, and when not compressing to favor the
                   * ratio (the forward arrivals parser already has this covered). */
                  if (i >= nRepMatchOffset &&
                     (i - nRepMatchOffset + pMatch->length) <= (nEndOffset - LAST_LITERALS) &&
                     !memcmp(pInWindow + i - nRepMatchOffset, pInWindow + i - pMatch->offset, pMatch->length)) {
                     pMatch->offset = nRepMatchOffset;
                     nDidReduce = 1;
                  }
               }

               if (pBestMatch[nNextIndex].offset && pMatch->offset != pBestMatch[nNextIndex].offset && nRepMatchOffset != pBestMatch[nNextIndex].offset) {
                  /* Otherwise, try to gain a match forward as well */
                  if (i >= pBestMatch[nNextIndex].offset && (i - pBestMatch[nNextIndex].offset + pMatch->length) <= (nEndOffset - LAST_LITERALS)) {
                     int nMaxLen = 0;
                     while (nMaxLen < pMatch->length && pInWindow[i - pBestMatch[nNextIndex].offset + nMaxLen] == pInWindow[i - pMatch->offset + nMaxLen])
                        nMaxLen++;
                     if (nMaxLen >= pMatch->length) {
                        /* Replace */
                        pMatch->offset = pBestMatch[nNextIndex].offset;
                        nDidReduce = 1;
                     }
                     else if (nMaxLen >= 2 && pMatch->offset != nRepMatchOffset) {
                        int nPartialSizeBefore, nPartialSizeAfter;

                        nPartialSizeBefore = lzsa_get_match_varlen_size_v2(pMatch->length - MIN_MATCH_SIZE_V2);
                        nPartialSizeBefore += (pMatch->offset <= 32) ? 4 : ((pMatch->offset <= 512) ? 8 : ((pMatch->offset <= (8192 + 512)) ? 12 : 16));
                        nPartialSizeBefore += lzsa_get_literals_varlen_size_v2(nNextLiterals);

                        nPartialSizeAfter = lzsa_get_match_varlen_size_v2(nMaxLen - MIN_MATCH_SIZE_V2);
                        nPartialSizeAfter += lzsa_get_literals_varlen_size_v2(nNextLiterals + (pMatch->length - nMaxLen)) + ((pMatch->length - nMaxLen) << 3);

                        if (nPartialSizeAfter < nPartialSizeBefore) {
                           int j;

                           /* We gain a repmatch that is shorter than the original match as this is the best we can do, so it is followed by extra literals, but
                            * we have calculated that this is shorter */
                           pMatch->offset = pBestMatch[nNextIndex].offset;
                           for (j = nMaxLen; j < pMatch->length; j++) {
                              pBestMatch[i + j].length = 0;
                           }
                           pMatch->length = nMaxLen;
                           nDidReduce = 1;
                        }
                     }
                  }
               }

               if (pMatch->length < 9 /* Don't waste time considering large matches, they will always win over literals */) {
                  /* Calculate this command's current cost (excluding 'nNumLiterals' bytes) */

                  int nCurCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNumLiterals) + lzsa_get_match_varlen_size_v2(pMatch->length - MIN_MATCH_SIZE_V2);
                  if (pMatch->offset != nRepMatchOffset)
                     nCurCommandSize += (pMatch->offset <= 32) ? 4 : ((pMatch->offset <= 512) ? 8 : ((pMatch->offset <= (8192 + 512)) ? 12 : 16));

                  /* Calculate the next command's current cost */
                  int nNextCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNextLiterals) + (nNextLiterals << 3) + lzsa_get_match_varlen_size_v2(pBestMatch[nNextIndex].length - MIN_MATCH_SIZE_V2);
                  if (pBestMatch[nNextIndex].offset != pMatch->offset)
                     nNextCommandSize += (pBestMatch[nNextIndex].offset <= 32) ? 4 : ((pBestMatch[nNextIndex].offset <= 512) ? 8 : ((pBestMatch[nNextIndex].offset <= (8192 + 512)) ? 12 : 16));

                  int nOriginalCombinedCommandSize = nCurCommandSize + nNextCommandSize;

                  /* Calculate the cost of replacing this match command by literals + the next command with the cost of encoding these literals (excluding 'nNumLiterals' bytes) */
                  int nReducedCommandSize = (pMatch->length << 3) + 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNumLiterals + pMatch->length + nNextLiterals) + (nNextLiterals << 3) + lzsa_get_match_varlen_size_v2(pBestMatch[nNextIndex].length - MIN_MATCH_SIZE_V2);
                  if (pBestMatch[nNextIndex].offset != nRepMatchOffset)
                     nReducedCommandSize += (pBestMatch[nNextIndex].offset <= 32) ? 4 : ((pBestMatch[nNextIndex].offset <= 512) ? 8 : ((pBestMatch[nNextIndex].offset <= (8192 + 512)) ? 12 : 16));

                  int nReplaceRepOffset = 0;
                  if (nRepMatchOffset && nRepMatchOffset != nPrevRepMatchOffset && nRepMatchLen >= MIN_MATCH_SIZE_V2 && nRepMatchOffset != pBestMatch[nNextIndex].offset && nRepIndex >= pBestMatch[nNextIndex].offset &&
                     (nRepIndex - pBestMatch[nNextIndex].offset + nRepMatchLen) <= (nEndOffset - LAST_LITERALS) &&
                     !memcmp(pInWindow + nRepIndex - nRepMatchOffset, pInWindow + nRepIndex - pBestMatch[nNextIndex].offset, nRepMatchLen)) {
                     /* Replacing this match command by literals would let us create a repmatch */
                     nReplaceRepOffset = 1;
                     nReducedCommandSize -= (nRepMatchOffset <= 32) ? 4 : ((nRepMatchOffset <= 512) ? 8 : ((nRepMatchOffset <= (8192 + 512)) ? 12 : 16));
                  }

                  if (nOriginalCombinedCommandSize >= nReducedCommandSize) {
                     /* Reduce */
                     int nMatchLen = pMatch->length;
                     int j;

                     for (j = 0; j < nMatchLen; j++) {
                        pBestMatch[i + j].length = 0;
                     }

                     nDidReduce = 1;

                     if (nReplaceRepOffset) {
                        pBestMatch[nRepIndex].offset = pBestMatch[nNextIndex].offset;
                        nRepMatchOffset = pBestMatch[nNextIndex].offset;
                     }
                     continue;
                  }
               }
            }
         }

         if ((i + pMatch->length) < nEndOffset && pMatch->length >= LCP_MAX &&
            pMatch->offset && pMatch->offset <= 32 && pBestMatch[i + pMatch->length].offset == pMatch->offset && (pMatch->length % pMatch->offset) == 0 &&
            (pMatch->length + pBestMatch[i + pMatch->length].length) <= MAX_VARLEN) {
            int nMatchLen = pMatch->length;

            /* Join */

            pMatch->length += pBestMatch[i + nMatchLen].length;
            pBestMatch[i + nMatchLen].offset = 0;
            pBestMatch[i + nMatchLen].length = -1;
            nDidReduce = 1;
            continue;
         }

         nPrevRepMatchOffset = nRepMatchOffset;
         nRepMatchOffset = pMatch->offset;
         nRepMatchLen = pMatch->length;
         nRepIndex = i;

         i += pMatch->length;
         nNumLiterals = 0;
      }
      else {
         nNumLiterals++;
         i++;
      }
   }

   return nDidReduce;
}

/**
 * Emit block of compressed data
 *
 * @param pCompressor compression context
 * @param pBestMatch optimal matches to emit
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 * @param pOutData pointer to output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 *
 * @return size of compressed data in output buffer, or -1 if the data is uncompressible
 */
static int lzsa_write_block_v2(lzsa_compressor *pCompressor, lzsa_match *pBestMatch, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset, unsigned char *pOutData, const int nMaxOutDataSize) {
   int i;
   int nNumLiterals = 0;
   int nInFirstLiteralOffset = 0;
   int nOutOffset = 0;
   int nCurNibbleOffset = -1, nCurFreeNibbles = 0;
   int nRepMatchOffset = 0;

   for (i = nStartOffset; i < nEndOffset; ) {
      const lzsa_match *pMatch = pBestMatch + i;

      if (pMatch->length >= MIN_MATCH_SIZE_V2) {
         int nMatchOffset = pMatch->offset;
         int nMatchLen = pMatch->length;
         int nEncodedMatchLen = nMatchLen - MIN_MATCH_SIZE_V2;
         int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN_V2) ? LITERALS_RUN_LEN_V2 : nNumLiterals;
         int nTokenMatchLen = (nEncodedMatchLen >= MATCH_RUN_LEN_V2) ? MATCH_RUN_LEN_V2 : nEncodedMatchLen;
         int nTokenOffsetMode;
         int nOffsetSize;

         if (nMatchOffset == nRepMatchOffset) {
            nTokenOffsetMode = 0xe0;
            nOffsetSize = 0;
         }
         else {
            if (nMatchOffset <= 32) {
               nTokenOffsetMode = 0x00 | ((((-nMatchOffset) & 0x01) << 5) ^ 0x20);
               nOffsetSize = 4;
            }
            else if (nMatchOffset <= 512) {
               nTokenOffsetMode = 0x40 | ((((-nMatchOffset) & 0x100) >> 3) ^ 0x20);
               nOffsetSize = 8;
            }
            else if (nMatchOffset <= (8192 + 512)) {
               nTokenOffsetMode = 0x80 | ((((-(nMatchOffset - 512)) & 0x0100) >> 3) ^ 0x20);
               nOffsetSize = 12;
            }
            else {
               nTokenOffsetMode = 0xc0;
               nOffsetSize = 16;
            }
         }

         int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNumLiterals) + (nNumLiterals << 3) + nOffsetSize /* match offset */ + lzsa_get_match_varlen_size_v2(nEncodedMatchLen);

         if ((nOutOffset + ((nCommandSize + 7) >> 3)) > nMaxOutDataSize)
            return -1;
         if (nMatchOffset < MIN_OFFSET || nMatchOffset > MAX_OFFSET)
            return -1;

         pOutData[nOutOffset++] = nTokenOffsetMode | (nTokenLiteralsLen << 3) | nTokenMatchLen;
         nOutOffset = lzsa_write_literals_varlen_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, &nCurFreeNibbles, nNumLiterals);
         if (nOutOffset < 0) return -1;

         if (nNumLiterals < pCompressor->stats.min_literals || pCompressor->stats.min_literals == -1)
            pCompressor->stats.min_literals = nNumLiterals;
         if (nNumLiterals > pCompressor->stats.max_literals)
            pCompressor->stats.max_literals = nNumLiterals;
         pCompressor->stats.total_literals += nNumLiterals;
         pCompressor->stats.literals_divisor++;

         if (nNumLiterals != 0) {
            memcpy(pOutData + nOutOffset, pInWindow + nInFirstLiteralOffset, nNumLiterals);
            nOutOffset += nNumLiterals;
            nNumLiterals = 0;
         }

         if (nTokenOffsetMode == 0x00 || nTokenOffsetMode == 0x20) {
            nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, &nCurFreeNibbles, ((-nMatchOffset) & 0x1e) >> 1);
            if (nOutOffset < 0) return -1;
         }
         else if (nTokenOffsetMode == 0x40 || nTokenOffsetMode == 0x60) {
            pOutData[nOutOffset++] = (-nMatchOffset) & 0xff;
         }
         else if (nTokenOffsetMode == 0x80 || nTokenOffsetMode == 0xa0) {
            nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, &nCurFreeNibbles, ((-(nMatchOffset - 512)) >> 9) & 0x0f);
            if (nOutOffset < 0) return -1;
            pOutData[nOutOffset++] = (-(nMatchOffset - 512)) & 0xff;
         }
         else if (nTokenOffsetMode == 0xc0) {
            pOutData[nOutOffset++] = (-nMatchOffset) >> 8;
            pOutData[nOutOffset++] = (-nMatchOffset) & 0xff;
         }

         if (nMatchOffset == nRepMatchOffset)
            pCompressor->stats.num_rep_offsets++;

         nRepMatchOffset = nMatchOffset;

         nOutOffset = lzsa_write_match_varlen_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, &nCurFreeNibbles, nEncodedMatchLen);
         if (nOutOffset < 0) return -1;

         if (nMatchOffset < pCompressor->stats.min_offset || pCompressor->stats.min_offset == -1)
            pCompressor->stats.min_offset = nMatchOffset;
         if (nMatchOffset > pCompressor->stats.max_offset)
            pCompressor->stats.max_offset = nMatchOffset;
         pCompressor->stats.total_offsets += nMatchOffset;

         if (nMatchLen < pCompressor->stats.min_match_len || pCompressor->stats.min_match_len == -1)
            pCompressor->stats.min_match_len = nMatchLen;
         if (nMatchLen > pCompressor->stats.max_match_len)
            pCompressor->stats.max_match_len = nMatchLen;
         pCompressor->stats.total_match_lens += nMatchLen;
         pCompressor->stats.match_divisor++;

         if (nMatchOffset == 1) {
            if (nMatchLen < pCompressor->stats.min_rle1_len || pCompressor->stats.min_rle1_len == -1)
               pCompressor->stats.min_rle1_len = nMatchLen;
            if (nMatchLen > pCompressor->stats.max_rle1_len)
               pCompressor->stats.max_rle1_len = nMatchLen;
            pCompressor->stats.total_rle1_lens += nMatchLen;
            pCompressor->stats.rle1_divisor++;
         }
         else if (nMatchOffset == 2) {
            if (nMatchLen < pCompressor->stats.min_rle2_len || pCompressor->stats.min_rle2_len == -1)
               pCompressor->stats.min_rle2_len = nMatchLen;
            if (nMatchLen > pCompressor->stats.max_rle2_len)
               pCompressor->stats.max_rle2_len = nMatchLen;
            pCompressor->stats.total_rle2_lens += nMatchLen;
            pCompressor->stats.rle2_divisor++;
         }

         i += nMatchLen;

         if (pCompressor->flags & LZSA_FLAG_RAW_BLOCK) {
            int nCurSafeDist = (i - nStartOffset) - nOutOffset;
            if (nCurSafeDist >= 0 && pCompressor->safe_dist < nCurSafeDist)
               pCompressor->safe_dist = nCurSafeDist;
         }

         pCompressor->num_commands++;
      }
      else {
         if (nNumLiterals == 0)
            nInFirstLiteralOffset = i;
         nNumLiterals++;
         i++;
      }
   }

   {
      int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN_V2) ? LITERALS_RUN_LEN_V2 : nNumLiterals;
      int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNumLiterals) + (nNumLiterals << 3);

      if ((nOutOffset + ((nCommandSize + 7) >> 3)) > nMaxOutDataSize)
         return -1;

      if (pCompressor->flags & LZSA_FLAG_RAW_BLOCK)
         pOutData[nOutOffset++] = (nTokenLiteralsLen << 3) | 0x47;
      else
         pOutData[nOutOffset++] = (nTokenLiteralsLen << 3) | 0x00;
      nOutOffset = lzsa_write_literals_varlen_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, &nCurFreeNibbles, nNumLiterals);
      if (nOutOffset < 0) return -1;

      if (nNumLiterals < pCompressor->stats.min_literals || pCompressor->stats.min_literals == -1)
         pCompressor->stats.min_literals = nNumLiterals;
      if (nNumLiterals > pCompressor->stats.max_literals)
         pCompressor->stats.max_literals = nNumLiterals;
      pCompressor->stats.total_literals += nNumLiterals;
      pCompressor->stats.literals_divisor++;

      if (nNumLiterals != 0) {
         memcpy(pOutData + nOutOffset, pInWindow + nInFirstLiteralOffset, nNumLiterals);
         nOutOffset += nNumLiterals;
         nNumLiterals = 0;
      }

      if (pCompressor->flags & LZSA_FLAG_RAW_BLOCK) {
         int nCurSafeDist = (i - nStartOffset) - nOutOffset;
         if (nCurSafeDist >= 0 && pCompressor->safe_dist < nCurSafeDist)
            pCompressor->safe_dist = nCurSafeDist;
      }

      pCompressor->num_commands++;
   }

   if (pCompressor->flags & LZSA_FLAG_RAW_BLOCK) {
      /* Emit EOD marker for raw block */

      if (nOutOffset >= nMaxOutDataSize)
         return -1;
      pOutData[nOutOffset++] = 0;      /* Match offset */

      nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, &nCurFreeNibbles, 15);   /* Extended match length nibble */
      if (nOutOffset < 0) return -1;

      if ((nOutOffset + 1) > nMaxOutDataSize)
         return -1;

      pOutData[nOutOffset++] = 232;    /* EOD match length byte */
   }

   if (nCurNibbleOffset != -1) {
      nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, &nCurFreeNibbles, 0);
      if (nOutOffset < 0 || nCurNibbleOffset != -1)
         return -1;
   }

   return nOutOffset;
}

/**
 * Emit raw block of uncompressible data
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 * @param pOutData pointer to output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 *
 * @return size of compressed data in output buffer, or -1 if the data is uncompressible
 */
static int lzsa_write_raw_uncompressed_block_v2(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset, unsigned char *pOutData, const int nMaxOutDataSize) {
   int nCurNibbleOffset = -1, nCurFreeNibbles = 0;
   int nNumLiterals = nEndOffset - nStartOffset;
   int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN_V2) ? LITERALS_RUN_LEN_V2 : nNumLiterals;
   int nOutOffset = 0;

   int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNumLiterals) + (nNumLiterals << 3) + 8 + 4 + 8;
   if ((nOutOffset + ((nCommandSize + 7) >> 3)) > nMaxOutDataSize)
      return -1;

   pCompressor->num_commands = 0;
   pOutData[nOutOffset++] = (nTokenLiteralsLen << 3) | 0x47;

   nOutOffset = lzsa_write_literals_varlen_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, &nCurFreeNibbles, nNumLiterals);
   if (nOutOffset < 0) return -1;

   if (nNumLiterals != 0) {
      memcpy(pOutData + nOutOffset, pInWindow + nStartOffset, nNumLiterals);
      nOutOffset += nNumLiterals;
      nNumLiterals = 0;
   }

   /* Emit EOD marker for raw block */

   pOutData[nOutOffset++] = 0;      /* Match offset */

   nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, &nCurFreeNibbles, 15);   /* Extended match length nibble */
   if (nOutOffset < 0) return -1;

   if ((nOutOffset + 1) > nMaxOutDataSize)
      return -1;

   pOutData[nOutOffset++] = 232;    /* EOD match length byte */

   pCompressor->num_commands++;

   if (nCurNibbleOffset != -1) {
      nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, &nCurFreeNibbles, 0);
      if (nOutOffset < 0 || nCurNibbleOffset != -1)
         return -1;
   }

   return nOutOffset;
}

/**
 * Select the most optimal matches, reduce the token count if possible, and then emit a block of compressed LZSA2 data
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nPreviousBlockSize number of previously compressed bytes (or 0 for none)
 * @param nInDataSize number of input bytes to compress
 * @param pOutData pointer to output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 *
 * @return size of compressed data in output buffer, or -1 if the data is uncompressible
 */
int lzsa_optimize_and_write_block_v2(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int nPreviousBlockSize, const int nInDataSize, unsigned char *pOutData, const int nMaxOutDataSize) {
   int nResult;

   lzsa_optimize_forward_v2(pCompressor, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize);

   int nDidReduce;
   int nPasses = 0;
   do {
      nDidReduce = lzsa_optimize_command_count_v2(pCompressor, pInWindow, pCompressor->best_match, nPreviousBlockSize, nPreviousBlockSize + nInDataSize);
      nPasses++;
   } while (nDidReduce && nPasses < 20);

   nResult = lzsa_write_block_v2(pCompressor, pCompressor->best_match, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, pOutData, nMaxOutDataSize);
   if (nResult < 0 && pCompressor->flags & LZSA_FLAG_RAW_BLOCK) {
      nResult = lzsa_write_raw_uncompressed_block_v2(pCompressor, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, pOutData, nMaxOutDataSize);
   }

   return nResult;
}
