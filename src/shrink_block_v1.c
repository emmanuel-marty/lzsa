/*
 * shrink_block_v1.c - LZSA1 block compressor implementation
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
#include "shrink_block_v1.h"
#include "format.h"

/**
 * Get the number of extra bits required to represent a literals length
 *
 * @param nLength literals length
 *
 * @return number of extra bits required
 */
static inline int lzsa_get_literals_varlen_size_v1(const int nLength) {
   if (nLength < LITERALS_RUN_LEN_V1) {
      return 0;
   }
   else {
      if (nLength < 256)
         return 8;
      else {
         if (nLength < 512)
            return 16;
         else
            return 24;
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
static inline int lzsa_write_literals_varlen_v1(unsigned char *pOutData, int nOutOffset, const int nLength) {
   if (nLength >= LITERALS_RUN_LEN_V1) {
      if (nLength < 256)
         pOutData[nOutOffset++] = nLength - LITERALS_RUN_LEN_V1;
      else {
         if (nLength < 512) {
            pOutData[nOutOffset++] = 250;
            pOutData[nOutOffset++] = nLength - 256;
         }
         else {
            pOutData[nOutOffset++] = 249;
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
 * @param nLength encoded match length (actual match length - MIN_MATCH_SIZE_V1)
 *
 * @return number of extra bits required
 */
static inline int lzsa_get_match_varlen_size_v1(const int nLength) {
   if (nLength < MATCH_RUN_LEN_V1) {
      return 0;
   }
   else {
      if ((nLength + MIN_MATCH_SIZE_V1) < 256)
         return 8;
      else {
         if ((nLength + MIN_MATCH_SIZE_V1) < 512)
            return 16;
         else
            return 24;
      }
   }
}

/**
 * Write extra encoded match length bytes to output (compressed) buffer. The caller must first check that there is enough
 * room to write the bytes.
 *
 * @param pOutData pointer to output buffer
 * @param nOutOffset current write index into output buffer
 * @param nLength encoded match length (actual match length - MIN_MATCH_SIZE_V1)
 */
static inline int lzsa_write_match_varlen_v1(unsigned char *pOutData, int nOutOffset, const int nLength) {
   if (nLength >= MATCH_RUN_LEN_V1) {
      if ((nLength + MIN_MATCH_SIZE_V1) < 256)
         pOutData[nOutOffset++] = nLength - MATCH_RUN_LEN_V1;
      else {
         if ((nLength + MIN_MATCH_SIZE_V1) < 512) {
            pOutData[nOutOffset++] = 239;
            pOutData[nOutOffset++] = nLength + MIN_MATCH_SIZE_V1 - 256;
         }
         else {
            pOutData[nOutOffset++] = 238;
            pOutData[nOutOffset++] = (nLength + MIN_MATCH_SIZE_V1) & 0xff;
            pOutData[nOutOffset++] = ((nLength + MIN_MATCH_SIZE_V1) >> 8) & 0xff;
         }
      }
   }

   return nOutOffset;
}

/**
 * Get offset encoding cost in bits
 *
 * @param __nMatchOffset offset to get cost of
 *
 * @return cost in bits
 */
#define lzsa_get_offset_cost_v1(__nMatchOffset) (((__nMatchOffset) <= 256) ? 8 : 16)

/**
 * Attempt to pick optimal matches using a forward arrivals parser, so as to produce the smallest possible output that decompresses to the same input
 *
 * @param pCompressor compression context
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 * @param nReduce non-zero to reduce the number of tokens when the path costs are equal, zero not to
 */
static void lzsa_optimize_forward_v1(lzsa_compressor *pCompressor, const int nStartOffset, const int nEndOffset, const int nReduce) {
   lzsa_arrival *arrival = pCompressor->arrival - (nStartOffset << ARRIVALS_PER_POSITION_SHIFT_V1);
   const int nMinMatchSize = pCompressor->min_match_size;
   const int nFavorRatio = (pCompressor->flags & LZSA_FLAG_FAVOR_RATIO) ? 1 : 0;
   const int nModeSwitchPenalty = nFavorRatio ? 0 : MODESWITCH_PENALTY;
   const int nDisableScore = nReduce ? 0 : (2 * BLOCK_SIZE);
   int i;

   if ((nEndOffset - nStartOffset) > BLOCK_SIZE) return;

   for (i = (nStartOffset << ARRIVALS_PER_POSITION_SHIFT_V1); i != ((nEndOffset + 1) << ARRIVALS_PER_POSITION_SHIFT_V1); i += NARRIVALS_PER_POSITION_V1) {
      lzsa_arrival* cur_arrival = &arrival[i];
      int j;

      memset(cur_arrival, 0, sizeof(lzsa_arrival) * NARRIVALS_PER_POSITION_V1);

      for (j = 0; j < NARRIVALS_PER_POSITION_V1; j++)
         cur_arrival[j].cost = 0x40000000;
   }

   arrival[nStartOffset << ARRIVALS_PER_POSITION_SHIFT_V1].cost = 0;
   arrival[nStartOffset << ARRIVALS_PER_POSITION_SHIFT_V1].from_slot = -1;

   for (i = nStartOffset; i != nEndOffset; i++) {
      lzsa_arrival* cur_arrival = &arrival[i << ARRIVALS_PER_POSITION_SHIFT_V1];
      lzsa_arrival* pDestLiteralSlots = &cur_arrival[1 << ARRIVALS_PER_POSITION_SHIFT_V1];
      int j, m;

      for (j = 0; j < NARRIVALS_PER_POSITION_V1 && cur_arrival[j].from_slot; j++) {
         const int nPrevCost = cur_arrival[j].cost;
         int nCodingChoiceCost = nPrevCost + 8 /* literal */;
         const int nScore = cur_arrival[j].score + 1;
         const int nNumLiterals = cur_arrival[j].num_literals + 1;
         int n;

         if (nNumLiterals == 1)
            nCodingChoiceCost += nModeSwitchPenalty;
         else if (nNumLiterals == LITERALS_RUN_LEN_V1 || nNumLiterals == 256 || nNumLiterals == 512) {
            nCodingChoiceCost += 8;
         }

         for (n = 0; n < NARRIVALS_PER_POSITION_V1 /* we only need the literals + short match cost + long match cost cases */; n++) {
            if (nCodingChoiceCost < pDestLiteralSlots[n].cost ||
               (nCodingChoiceCost == pDestLiteralSlots[n].cost && nScore < (pDestLiteralSlots[n].score + nDisableScore))) {
               memmove(&pDestLiteralSlots[n + 1],
                  &pDestLiteralSlots[n],
                  sizeof(lzsa_arrival) * (NARRIVALS_PER_POSITION_V1 - n - 1));

               lzsa_arrival* pDestArrival = &pDestLiteralSlots[n];
               pDestArrival->cost = nCodingChoiceCost;
               pDestArrival->rep_offset = cur_arrival[j].rep_offset;
               pDestArrival->from_slot = j + 1;
               pDestArrival->from_pos = i - nStartOffset;
               pDestArrival->match_len = 0;
               pDestArrival->num_literals = nNumLiterals;
               pDestArrival->score = nScore;
               break;
            }
         }
      }

      const lzsa_match *match = pCompressor->match + ((i - nStartOffset) << MATCHES_PER_INDEX_SHIFT_V1);
      const int nNumArrivalsForThisPos = j;

      if (nNumArrivalsForThisPos != 0) {
         for (m = 0; m < NMATCHES_PER_INDEX_V1 && match[m].length; m++) {
            int nMatchLen = match[m].length;
            const int nMatchOffsetCost = lzsa_get_offset_cost_v1(match[m].offset);
            int nStartingMatchLen, k;

            if ((i + nMatchLen) > nEndOffset)
               nMatchLen = nEndOffset - i;

            if (nMatchLen >= LEAVE_ALONE_MATCH_SIZE)
               nStartingMatchLen = nMatchLen;
            else
               nStartingMatchLen = nMinMatchSize;
            for (k = nStartingMatchLen; k <= nMatchLen; k++) {
               const int nMatchLenCost = lzsa_get_match_varlen_size_v1(k - MIN_MATCH_SIZE_V1);

               lzsa_arrival* pDestSlots = &cur_arrival[k << ARRIVALS_PER_POSITION_SHIFT_V1];
               int nCodingChoiceCost = cur_arrival[0].cost + 8 /* token */ /* the actual cost of the literals themselves accumulates up the chain */ + nMatchOffsetCost + nMatchLenCost;
               int exists = 0, n;

               if (!cur_arrival[0].num_literals)
                  nCodingChoiceCost += nModeSwitchPenalty;

               for (n = 0;
                  n < NARRIVALS_PER_POSITION_V1 && pDestSlots[n].from_slot && pDestSlots[n].cost <= nCodingChoiceCost;
                  n++) {
                  if (lzsa_get_offset_cost_v1(pDestSlots[n].rep_offset) == nMatchOffsetCost) {
                     exists = 1;
                     break;
                  }
               }

               if (!exists) {
                  const int nScore = cur_arrival[0].score + 5;

                  if (nCodingChoiceCost < pDestSlots[0].cost ||
                     (nCodingChoiceCost == pDestSlots[0].cost && nScore < (pDestSlots[0].score + nDisableScore))) {
                     memmove(&pDestSlots[1],
                        &pDestSlots[0],
                        sizeof(lzsa_arrival) * (NARRIVALS_PER_POSITION_V1 - 1));

                     pDestSlots->cost = nCodingChoiceCost;
                     pDestSlots->rep_offset = match[m].offset;
                     pDestSlots->from_slot = 1;
                     pDestSlots->from_pos = i - nStartOffset;
                     pDestSlots->match_len = k;
                     pDestSlots->num_literals = 0;
                     pDestSlots->score = nScore;
                  }
               }
            }
         }
      }
   }

   const lzsa_arrival *end_arrival = &arrival[i << ARRIVALS_PER_POSITION_SHIFT_V1];
   lzsa_match *pBestMatch = pCompressor->best_match - nStartOffset;

   while (end_arrival->from_slot > 0 && (end_arrival->from_pos + nStartOffset) < nEndOffset) {
      pBestMatch[end_arrival->from_pos + nStartOffset].length = end_arrival->match_len;
      pBestMatch[end_arrival->from_pos + nStartOffset].offset = (end_arrival->match_len) ? end_arrival->rep_offset: 0;
      end_arrival = &arrival[((end_arrival->from_pos + nStartOffset) << ARRIVALS_PER_POSITION_SHIFT_V1) + (end_arrival->from_slot - 1)];
   }
}

/**
 * Attempt to minimize the number of commands issued in the compressed data block, in order to speed up decompression without
 * impacting the compression ratio
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 *
 * @return non-zero if the number of tokens was reduced, 0 if it wasn't
 */
static int lzsa_optimize_command_count_v1(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset) {
   lzsa_match *pBestMatch = pCompressor->best_match - nStartOffset;
   int i;
   int nNumLiterals = 0;
   int nDidReduce = 0;

   for (i = nStartOffset; i < nEndOffset; ) {
      lzsa_match *pMatch = pBestMatch + i;

      if (pMatch->length == 0 &&
         (i + 1) < nEndOffset &&
         pBestMatch[i + 1].length >= MIN_MATCH_SIZE_V1 &&
         pBestMatch[i + 1].length < MAX_VARLEN &&
         pBestMatch[i + 1].offset &&
         i >= pBestMatch[i + 1].offset &&
         (i + pBestMatch[i + 1].length + 1) <= nEndOffset &&
         !memcmp(pInWindow + i - (pBestMatch[i + 1].offset), pInWindow + i, pBestMatch[i + 1].length + 1)) {
         const int nCurLenSize = lzsa_get_match_varlen_size_v1(pBestMatch[i + 1].length - MIN_MATCH_SIZE_V1);
         const int nReducedLenSize = lzsa_get_match_varlen_size_v1(pBestMatch[i + 1].length + 1 - MIN_MATCH_SIZE_V1);

         if ((nReducedLenSize - nCurLenSize) <= 8) {
            /* Merge */
            pBestMatch[i].length = pBestMatch[i + 1].length + 1;
            pBestMatch[i].offset = pBestMatch[i + 1].offset;
            pBestMatch[i + 1].length = 0;
            pBestMatch[i + 1].offset = 0;
            nDidReduce = 1;
            continue;
         }
      }

      if (pMatch->length >= MIN_MATCH_SIZE_V1) {
         if (pMatch->length <= 9 /* Don't waste time considering large matches, they will always win over literals */ &&
            (i + pMatch->length) < nEndOffset /* Don't consider the last token in the block, we can only reduce a match inbetween other tokens */) {
            int nNextIndex = i + pMatch->length;
            int nNextLiterals = 0;

            while (nNextIndex < nEndOffset && pBestMatch[nNextIndex].length < MIN_MATCH_SIZE_V1) {
               nNextLiterals++;
               nNextIndex++;
            }

            /* This command is a match, is followed by 'nNextLiterals' literals and then by another match, or the end of the input. Calculate this command's current cost (excluding 'nNumLiterals' bytes) */
            if ((8 /* token */ + lzsa_get_literals_varlen_size_v1(nNumLiterals) + ((pMatch->offset <= 256) ? 8 : 16) /* match offset */ + lzsa_get_match_varlen_size_v1(pMatch->length - MIN_MATCH_SIZE_V1) +
               8 /* token */ + lzsa_get_literals_varlen_size_v1(nNextLiterals)) >=
               (8 /* token */ + (pMatch->length << 3) + lzsa_get_literals_varlen_size_v1(nNumLiterals + pMatch->length + nNextLiterals))) {
               /* Reduce */
               const int nMatchLen = pMatch->length;
               int j;

               for (j = 0; j < nMatchLen; j++) {
                  pBestMatch[i + j].length = 0;
               }

               nDidReduce = 1;
               continue;
            }
         }

         if ((i + pMatch->length) < nEndOffset && pMatch->offset && pMatch->length >= MIN_MATCH_SIZE_V1 &&
            pBestMatch[i + pMatch->length].offset &&
            pBestMatch[i + pMatch->length].length >= MIN_MATCH_SIZE_V1 &&
            (pMatch->length + pBestMatch[i + pMatch->length].length) <= MAX_VARLEN &&
            (i + pMatch->length) >= pMatch->offset &&
            (i + pMatch->length) >= pBestMatch[i + pMatch->length].offset &&
            (i + pMatch->length + pBestMatch[i + pMatch->length].length) <= nEndOffset &&
            !memcmp(pInWindow + i - pMatch->offset + pMatch->length,
               pInWindow + i + pMatch->length - pBestMatch[i + pMatch->length].offset,
               pBestMatch[i + pMatch->length].length)) {

            int nCurPartialSize = lzsa_get_match_varlen_size_v1(pMatch->length - MIN_MATCH_SIZE_V1);
            nCurPartialSize += 8 /* token */ + /* lzsa_get_literals_varlen_size_v1(0) + */ ((pBestMatch[i + pMatch->length].offset <= 256) ? 8 : 16) /* match offset */ + lzsa_get_match_varlen_size_v1(pBestMatch[i + pMatch->length].length - MIN_MATCH_SIZE_V1);

            const int nReducedPartialSize = lzsa_get_match_varlen_size_v1(pMatch->length + pBestMatch[i + pMatch->length].length - MIN_MATCH_SIZE_V1);

            if (nCurPartialSize >= nReducedPartialSize) {
               const int nMatchLen = pMatch->length;

               /* Join */

               pMatch->length += pBestMatch[i + nMatchLen].length;
               pBestMatch[i + nMatchLen].length = 0;
               pBestMatch[i + nMatchLen].offset = 0;
               nDidReduce = 1;
               continue;
            }
         }

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
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 * @param pOutData pointer to output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 *
 * @return size of compressed data in output buffer, or -1 if the data is uncompressible
 */
static int lzsa_write_block_v1(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset, unsigned char *pOutData, const int nMaxOutDataSize) {
   const lzsa_match *pBestMatch = pCompressor->best_match - nStartOffset;
   int i;
   int nNumLiterals = 0;
   int nInFirstLiteralOffset = 0;
   int nOutOffset = 0;

   for (i = nStartOffset; i < nEndOffset; ) {
      const lzsa_match *pMatch = pBestMatch + i;

      if (pMatch->length >= MIN_MATCH_SIZE_V1) {
         const int nMatchOffset = pMatch->offset;
         const int nMatchLen = pMatch->length;
         const int nEncodedMatchLen = nMatchLen - MIN_MATCH_SIZE_V1;
         const int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN_V1) ? LITERALS_RUN_LEN_V1 : nNumLiterals;
         const int nTokenMatchLen = (nEncodedMatchLen >= MATCH_RUN_LEN_V1) ? MATCH_RUN_LEN_V1 : nEncodedMatchLen;
         const int nTokenLongOffset = (nMatchOffset <= 256) ? 0x00 : 0x80;
         const int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v1(nNumLiterals) + (nNumLiterals << 3) + (nTokenLongOffset ? 16 : 8) /* match offset */ + lzsa_get_match_varlen_size_v1(nEncodedMatchLen);

         if ((nOutOffset + (nCommandSize >> 3)) > nMaxOutDataSize)
            return -1;
         if (nMatchOffset < MIN_OFFSET || nMatchOffset > MAX_OFFSET)
            return -1;

         pOutData[nOutOffset++] = nTokenLongOffset | (nTokenLiteralsLen << 4) | nTokenMatchLen;
         nOutOffset = lzsa_write_literals_varlen_v1(pOutData, nOutOffset, nNumLiterals);

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

         pOutData[nOutOffset++] = (-nMatchOffset) & 0xff;
         if (nTokenLongOffset) {
            pOutData[nOutOffset++] = (-nMatchOffset) >> 8;
         }
         nOutOffset = lzsa_write_match_varlen_v1(pOutData, nOutOffset, nEncodedMatchLen);

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
            const int nCurSafeDist = (i - nStartOffset) - nOutOffset;
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
      const int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN_V1) ? LITERALS_RUN_LEN_V1 : nNumLiterals;
      const int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v1(nNumLiterals) + (nNumLiterals << 3);

      if ((nOutOffset + (nCommandSize >> 3)) > nMaxOutDataSize)
         return -1;

      if (pCompressor->flags & LZSA_FLAG_RAW_BLOCK)
         pOutData[nOutOffset++] = (nTokenLiteralsLen << 4) | 0x0f;
      else
         pOutData[nOutOffset++] = (nTokenLiteralsLen << 4) /* | 0x00 */;
      nOutOffset = lzsa_write_literals_varlen_v1(pOutData, nOutOffset, nNumLiterals);

      if (nNumLiterals < pCompressor->stats.min_literals || pCompressor->stats.min_literals == -1)
         pCompressor->stats.min_literals = nNumLiterals;
      if (nNumLiterals > pCompressor->stats.max_literals)
         pCompressor->stats.max_literals = nNumLiterals;
      pCompressor->stats.total_literals += nNumLiterals;
      pCompressor->stats.literals_divisor++;

      if (nNumLiterals != 0) {
         memcpy(pOutData + nOutOffset, pInWindow + nInFirstLiteralOffset, nNumLiterals);
         nOutOffset += nNumLiterals;
      }

      if (pCompressor->flags & LZSA_FLAG_RAW_BLOCK) {
         const int nCurSafeDist = (i - nStartOffset) - nOutOffset;
         if (nCurSafeDist >= 0 && pCompressor->safe_dist < nCurSafeDist)
            pCompressor->safe_dist = nCurSafeDist;
      }

      pCompressor->num_commands++;
   }

   if (pCompressor->flags & LZSA_FLAG_RAW_BLOCK) {
      /* Emit EOD marker for raw block */

      if ((nOutOffset + 4) > nMaxOutDataSize)
         return -1;

      pOutData[nOutOffset++] = 0;
      pOutData[nOutOffset++] = 238;
      pOutData[nOutOffset++] = 0;
      pOutData[nOutOffset++] = 0;
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
static int lzsa_write_raw_uncompressed_block_v1(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset, unsigned char *pOutData, const int nMaxOutDataSize) {
   const int nNumLiterals = nEndOffset - nStartOffset;
   const int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN_V1) ? LITERALS_RUN_LEN_V1 : nNumLiterals;
   int nOutOffset = 0;

   const int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v1(nNumLiterals) + (nNumLiterals << 3) + 4;
   if ((nOutOffset + (nCommandSize >> 3)) > nMaxOutDataSize)
      return -1;

   pCompressor->num_commands = 0;
   pOutData[nOutOffset++] = (nTokenLiteralsLen << 4) | 0x0f;
   
   nOutOffset = lzsa_write_literals_varlen_v1(pOutData, nOutOffset, nNumLiterals);

   if (nNumLiterals != 0) {
      memcpy(pOutData + nOutOffset, pInWindow + nStartOffset, nNumLiterals);
      nOutOffset += nNumLiterals;
   }

   pCompressor->num_commands++;

   /* Emit EOD marker for raw block */

   pOutData[nOutOffset++] = 0;
   pOutData[nOutOffset++] = 238;
   pOutData[nOutOffset++] = 0;
   pOutData[nOutOffset++] = 0;

   return nOutOffset;
}

/**
 * Select the most optimal matches, reduce the token count if possible, and then emit a block of compressed LZSA1 data
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
int lzsa_optimize_and_write_block_v1(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int nPreviousBlockSize, const int nInDataSize, unsigned char *pOutData, const int nMaxOutDataSize) {
   int nResult;

   /* Compress optimally without breaking ties in favor of less tokens */

   memset(pCompressor->best_match, 0, BLOCK_SIZE * sizeof(lzsa_match));

   if (nInDataSize < 65536) {
      lzsa_optimize_forward_v1(pCompressor, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, 1 /* reduce */);
   }
   else {
      lzsa_optimize_forward_v1(pCompressor, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, 0 /* reduce */);
   }

   int nDidReduce;
   int nPasses = 0;
   do {
      nDidReduce = lzsa_optimize_command_count_v1(pCompressor, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize);
      nPasses++;
   } while (nDidReduce && nPasses < 20);

   nResult = lzsa_write_block_v1(pCompressor, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, pOutData, nMaxOutDataSize);
   if (nResult < 0 && (pCompressor->flags & LZSA_FLAG_RAW_BLOCK)) {
      nResult = lzsa_write_raw_uncompressed_block_v1(pCompressor, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, pOutData, nMaxOutDataSize);
   }

   return nResult;
}
