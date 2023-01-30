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

/**
 * Write 4-bit nibble to output (compressed) buffer
 *
 * @param pOutData pointer to output buffer
 * @param nOutOffset current write index into output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 * @param nCurNibbleOffset write index into output buffer, of current byte being filled with nibbles
 * @param nNibbleValue value to write (0..15)
 * 
 * @return updated write index into output buffer
 */
static int lzsa_write_nibble_v2(unsigned char *pOutData, int nOutOffset, const int nMaxOutDataSize, int *nCurNibbleOffset, const int nNibbleValue) {
   if (nOutOffset >= 0) {
      if ((*nCurNibbleOffset) == -1) {
         if (nOutOffset >= nMaxOutDataSize) return -1;
         (*nCurNibbleOffset) = nOutOffset;
         pOutData[nOutOffset++] = nNibbleValue << 4;
      }
      else {
         pOutData[*nCurNibbleOffset] |= (nNibbleValue & 0x0f);
         (*nCurNibbleOffset) = -1;
      }
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
            return 4 + 8;
         else {
            return 4 + 24;
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
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 * @param nCurNibbleOffset write index into output buffer, of current byte being filled with nibbles
 * @param nLength literals length
 *
 * @return updated write index into output buffer
 */
static inline int lzsa_write_literals_varlen_v2(unsigned char *pOutData, int nOutOffset, const int nMaxOutDataSize, int *nCurNibbleOffset, const int nLength) {
   if (nLength >= LITERALS_RUN_LEN_V2) {
      if (nLength < (LITERALS_RUN_LEN_V2 + 15)) {
         nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, nCurNibbleOffset, nLength - LITERALS_RUN_LEN_V2);
      }
      else {
         nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, nCurNibbleOffset, 15);
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
            return 4 + 8;
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
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 * @param nCurNibbleOffset write index into output buffer, of current byte being filled with nibbles
 * @param nLength encoded match length (actual match length - MIN_MATCH_SIZE_V2)
 *
 * @return updated write index into output buffer
 */
static inline int lzsa_write_match_varlen_v2(unsigned char *pOutData, int nOutOffset, const int nMaxOutDataSize, int *nCurNibbleOffset, const int nLength) {
   if (nLength >= MATCH_RUN_LEN_V2) {
      if (nLength < (MATCH_RUN_LEN_V2 + 15)) {
         nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, nCurNibbleOffset, nLength - MATCH_RUN_LEN_V2);
      }
      else {
         nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, nCurNibbleOffset, 15);
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
 * Insert forward rep candidate
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param i input data window position whose matches are being considered
 * @param nMatchOffset match offset to use as rep candidate
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 * @param nDepth current insertion depth
 */
static void lzsa_insert_forward_match_v2(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int i, const int nMatchOffset, const int nStartOffset, const int nEndOffset, const int nDepth) {
   const lzsa_arrival *arrival = pCompressor->arrival + ((i - nStartOffset) << ARRIVALS_PER_POSITION_SHIFT_V2);
   const int *rle_len = (const int*)pCompressor->intervals /* reuse */;
   lzsa_match* visited = ((lzsa_match*)pCompressor->pos_data) - nStartOffset /* reuse */;
   int j;

   for (j = 0; j < NARRIVALS_PER_POSITION_V2_BIG && arrival[j].from_slot; j++) {
      const int nRepOffset = arrival[j].rep_offset;

      if (nMatchOffset != nRepOffset) {
         const int nRepLen = arrival[j].rep_len;
         const int nRepPos = arrival[j].rep_pos;

         if (nRepPos >= nStartOffset &&
            (nRepPos + nRepLen) <= nEndOffset) {

            if (visited[nRepPos].offset != nMatchOffset || visited[nRepPos].length > nRepLen) {
               visited[nRepPos].length = 0;
               visited[nRepPos].offset = nMatchOffset;

               lzsa_match* fwd_match = pCompressor->match + ((nRepPos - nStartOffset) << MATCHES_PER_INDEX_SHIFT_V2);

               if (fwd_match[NMATCHES_PER_INDEX_V2 - 1].length == 0) {
                  if (nRepPos >= nMatchOffset) {
                     const unsigned char* pInWindowStart = pInWindow + nRepPos;

                     if (!memcmp(pInWindowStart, pInWindowStart - nMatchOffset, 2)) {
                        const int nLen0 = rle_len[nRepPos - nMatchOffset];
                        const int nLen1 = rle_len[nRepPos];
                        const int nMinLen = (nLen0 < nLen1) ? nLen0 : nLen1;

                        if (nMinLen >= nRepLen || !memcmp(pInWindowStart + nMinLen, pInWindowStart + nMinLen - nMatchOffset, nRepLen - nMinLen)) {
                           int r;

                           for (r = 0; fwd_match[r].length; r++) {
                              if (fwd_match[r].offset == nMatchOffset) {
                                 break;
                              }
                           }

                           if (fwd_match[r].length == 0) {
                              if (nRepLen >= MIN_MATCH_SIZE_V2) {
                                 if (nRepOffset) {
                                    int nMaxRepLen = nEndOffset - nRepPos;
                                    if (nMaxRepLen > LCP_MAX)
                                       nMaxRepLen = LCP_MAX;

                                    const int nCurRepLen = (nMinLen > nRepLen) ? nMinLen : nRepLen;
                                    const unsigned char* pInWindowMax = pInWindowStart + nMaxRepLen;
                                    const unsigned char* pInWindowAtRepPos = pInWindowStart + nCurRepLen;

                                    if (pInWindowAtRepPos > pInWindowMax)
                                       pInWindowAtRepPos = pInWindowMax;

                                    while ((pInWindowAtRepPos + 8) < pInWindowMax && !memcmp(pInWindowAtRepPos, pInWindowAtRepPos - nMatchOffset, 8))
                                       pInWindowAtRepPos += 8;
                                    while ((pInWindowAtRepPos + 4) < pInWindowMax && !memcmp(pInWindowAtRepPos, pInWindowAtRepPos - nMatchOffset, 4))
                                       pInWindowAtRepPos += 4;
                                    while (pInWindowAtRepPos < pInWindowMax && pInWindowAtRepPos[0] == pInWindowAtRepPos[-nMatchOffset])
                                       pInWindowAtRepPos++;

                                    fwd_match[r].length = (const unsigned short)(pInWindowAtRepPos - pInWindowStart);
                                    fwd_match[r].offset = nMatchOffset;

                                    if (nDepth < 9)
                                       lzsa_insert_forward_match_v2(pCompressor, pInWindow, nRepPos, nMatchOffset, nStartOffset, nEndOffset, nDepth + 1);
                                 }
                              }
                           }
                        }
                        else {
                           visited[nRepPos].length = nRepLen;
                        }
                     }
                  }
               }
            }
         }
      }
   }
}

/**
 * Attempt to pick optimal matches using a forward arrivals parser, so as to produce the smallest possible output that decompresses to the same input
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 * @param nReduce non-zero to reduce the number of tokens when the path costs are equal, zero not to
 * @param nInsertForwardReps non-zero to insert forward repmatch candidates, zero to use the previously inserted candidates
 * @param nArrivalsPerPosition number of arrivals to record per input buffer position
 */
static void lzsa_optimize_forward_v2(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset, const int nReduce, const int nInsertForwardReps, const int nArrivalsPerPosition) {
   lzsa_arrival *arrival = pCompressor->arrival - (nStartOffset << ARRIVALS_PER_POSITION_SHIFT_V2);
   const int *rle_len = (const int*)pCompressor->intervals /* reuse */;
   lzsa_match *visited = ((lzsa_match*)pCompressor->pos_data) - nStartOffset /* reuse */;
   unsigned char *nRepSlotHandledMask = pCompressor->rep_slot_handled_mask;
   unsigned char *nRepLenHandledMask = pCompressor->rep_len_handled_mask;
   const int nModeSwitchPenalty = (pCompressor->flags & LZSA_FLAG_FAVOR_RATIO) ? 0 : MODESWITCH_PENALTY;
   const int nMinMatchSize = pCompressor->min_match_size;
   const int nDisableScore = nReduce ? 0 : (2 * BLOCK_SIZE);
   const int nMaxRepInsertedLen = nReduce ? LEAVE_ALONE_MATCH_SIZE : 0;
   const int nLeaveAloneMatchSize = (nArrivalsPerPosition == NARRIVALS_PER_POSITION_V2_SMALL) ? LEAVE_ALONE_MATCH_SIZE_SMALL : LEAVE_ALONE_MATCH_SIZE;
   int i;

   if ((nEndOffset - nStartOffset) > BLOCK_SIZE) return;

   for (i = (nStartOffset << ARRIVALS_PER_POSITION_SHIFT_V2); i != ((nEndOffset + 1) << ARRIVALS_PER_POSITION_SHIFT_V2); i += NARRIVALS_PER_POSITION_V2_MAX) {
      lzsa_arrival *cur_arrival = &arrival[i];
      int j;

      memset(cur_arrival, 0, sizeof(lzsa_arrival) * NARRIVALS_PER_POSITION_V2_MAX);

      for (j = 0; j < NARRIVALS_PER_POSITION_V2_MAX; j++)
         cur_arrival[j].cost = 0x40000000;
   }

   arrival[nStartOffset << ARRIVALS_PER_POSITION_SHIFT_V2].cost = 0;
   arrival[nStartOffset << ARRIVALS_PER_POSITION_SHIFT_V2].from_slot = -1;

   if (nInsertForwardReps) {
      memset(visited + nStartOffset, 0, (nEndOffset - nStartOffset) * sizeof(lzsa_match));
   }

   for (i = nStartOffset; i != nEndOffset; i++) {
      lzsa_arrival *cur_arrival = &arrival[i << ARRIVALS_PER_POSITION_SHIFT_V2];
      lzsa_arrival *pDestLiteralSlots = &cur_arrival[NARRIVALS_PER_POSITION_V2_MAX];
      int j, m;

      for (j = 0; j < nArrivalsPerPosition && cur_arrival[j].from_slot; j++) {
         const int nPrevCost = cur_arrival[j].cost;
         int nCodingChoiceCost = nPrevCost + 8 /* literal */;
         const int nScore = cur_arrival[j].score + 1 - nDisableScore;
         const int nNumLiterals = cur_arrival[j].num_literals + 1;
         const int nRepOffset = cur_arrival[j].rep_offset;

         switch (nNumLiterals) {
         case 1:
            nCodingChoiceCost += nModeSwitchPenalty;
            break;

         case LITERALS_RUN_LEN_V2:
            nCodingChoiceCost += 4;
            break;

         case LITERALS_RUN_LEN_V2 + 15:
            nCodingChoiceCost += 8;
            break;

         case 256:
            nCodingChoiceCost += 16;
            break;

         default:
            break;
         }

         if (nCodingChoiceCost < pDestLiteralSlots[nArrivalsPerPosition - 1].cost ||
            (nCodingChoiceCost == pDestLiteralSlots[nArrivalsPerPosition - 1].cost && nScore < pDestLiteralSlots[nArrivalsPerPosition - 1].score &&
               nRepOffset != pDestLiteralSlots[nArrivalsPerPosition - 1].rep_offset)) {
            int exists = 0, n;

            for (n = 0;
               pDestLiteralSlots[n].cost < nCodingChoiceCost;
               n++) {
               if (pDestLiteralSlots[n].rep_offset == nRepOffset) {
                  exists = 1;
                  break;
               }
            }

            if (!exists) {
               for (;
                  n < nArrivalsPerPosition && pDestLiteralSlots[n].cost == nCodingChoiceCost && nScore >= pDestLiteralSlots[n].score;
                  n++) {
                  if (pDestLiteralSlots[n].rep_offset == nRepOffset) {
                     exists = 1;
                     break;
                  }
               }

               if (!exists) {
                  if (n < nArrivalsPerPosition) {
                     int z;

                     for (z = n;
                        z < nArrivalsPerPosition - 1 && pDestLiteralSlots[z].cost == nCodingChoiceCost;
                        z++) {
                        if (pDestLiteralSlots[z].rep_offset == nRepOffset) {
                           exists = 1;
                           break;
                        }
                     }

                     if (!exists) {
                        for (; z < nArrivalsPerPosition - 1 && pDestLiteralSlots[z].from_slot; z++) {
                           if (pDestLiteralSlots[z].rep_offset == nRepOffset)
                              break;
                        }

                        memmove(&pDestLiteralSlots[n + 1],
                           &pDestLiteralSlots[n],
                           sizeof(lzsa_arrival) * (z - n));

                        lzsa_arrival* pDestArrival = &pDestLiteralSlots[n];
                        pDestArrival->cost = nCodingChoiceCost;
                        pDestArrival->rep_offset = nRepOffset;
                        pDestArrival->from_slot = j + 1;
                        pDestArrival->from_pos = i - nStartOffset;
                        pDestArrival->rep_len = cur_arrival[j].rep_len;
                        pDestArrival->match_len = 0;
                        pDestArrival->num_literals = nNumLiterals;
                        pDestArrival->rep_pos = cur_arrival[j].rep_pos;
                        pDestArrival->score = nScore + nDisableScore;
                     }
                  }
               }
            }
         }
      }

      const lzsa_match *match = pCompressor->match + ((i - nStartOffset) << MATCHES_PER_INDEX_SHIFT_V2);
      const int nNumArrivalsForThisPos = j;
      int nMinOverallRepLen = 0, nMaxOverallRepLen = 0;

      int nRepMatchArrivalIdxAndLen[(NARRIVALS_PER_POSITION_V2_MAX * 2) + 1];
      int nNumRepMatchArrivals = 0;

      if ((i + MIN_MATCH_SIZE_V2) <= nEndOffset) {
         int nMaxRepLenForPos = nEndOffset - i;
         if (nMaxRepLenForPos > LCP_MAX)
            nMaxRepLenForPos = LCP_MAX;
         const unsigned char* pInWindowStart = pInWindow + i;
         const unsigned char* pInWindowMax = pInWindowStart + nMaxRepLenForPos;

         for (j = 0; j < nNumArrivalsForThisPos; j++) {
            const int nRepOffset = cur_arrival[j].rep_offset;

            if (i >= nRepOffset) {
               if (!memcmp(pInWindowStart, pInWindowStart - nRepOffset, MIN_MATCH_SIZE_V2)) {
                  if (nRepOffset) {
                     const unsigned char* pInWindowAtPos;

                     const int nLen0 = rle_len[i - nRepOffset];
                     const int nLen1 = rle_len[i];
                     int nMinLen = (nLen0 < nLen1) ? nLen0 : nLen1;

                     if (nMinLen > nMaxRepLenForPos)
                        nMinLen = nMaxRepLenForPos;
                     pInWindowAtPos = pInWindowStart + nMinLen;

                     while ((pInWindowAtPos + 8) < pInWindowMax && !memcmp(pInWindowAtPos, pInWindowAtPos - nRepOffset, 8))
                        pInWindowAtPos += 8;
                     while ((pInWindowAtPos + 4) < pInWindowMax && !memcmp(pInWindowAtPos, pInWindowAtPos - nRepOffset, 4))
                        pInWindowAtPos += 4;
                     while (pInWindowAtPos < pInWindowMax && pInWindowAtPos[0] == pInWindowAtPos[-nRepOffset])
                        pInWindowAtPos++;
                     const int nCurRepLen = (const int)(pInWindowAtPos - pInWindowStart);

                     if (nMaxOverallRepLen < nCurRepLen)
                        nMaxOverallRepLen = nCurRepLen;
                     nRepMatchArrivalIdxAndLen[nNumRepMatchArrivals++] = j;
                     nRepMatchArrivalIdxAndLen[nNumRepMatchArrivals++] = nCurRepLen;
                  }
               }
            }
         }
      }
      nRepMatchArrivalIdxAndLen[nNumRepMatchArrivals] = -1;

      if (!nReduce) {
         memset(nRepSlotHandledMask, 0, nArrivalsPerPosition * ((LCP_MAX + 1) / 8) * sizeof(unsigned char));
      }
      memset(nRepLenHandledMask, 0, ((LCP_MAX + 1) / 8) * sizeof(unsigned char));

      for (m = 0; m < NMATCHES_PER_INDEX_V2 && match[m].length; m++) {
         int nMatchLen = match[m].length & 0x7fff;
         const int nMatchOffset = match[m].offset;
         int nNoRepmatchOffsetCost = 0, nNoRepmatchScore = 0;
         int nStartingMatchLen, k;

         if ((i + nMatchLen) > nEndOffset)
            nMatchLen = nEndOffset - i;

         if (nInsertForwardReps)
            lzsa_insert_forward_match_v2(pCompressor, pInWindow, i, nMatchOffset, nStartOffset, nEndOffset, 0);

         int nNonRepMatchArrivalIdx = -1;
         for (j = 0; j < nNumArrivalsForThisPos; j++) {
            if (nMatchOffset != cur_arrival[j].rep_offset) {
               const int nPrevCost = cur_arrival[j].cost;
               const int nScorePenalty = 3 + (match[m].length >> 15);

               nNoRepmatchOffsetCost = nPrevCost /* the actual cost of the literals themselves accumulates up the chain */;
               if (!cur_arrival[j].num_literals)
                  nNoRepmatchOffsetCost += nModeSwitchPenalty;
               nNoRepmatchOffsetCost += (nMatchOffset <= 32) ? 4 : ((nMatchOffset <= 512) ? 8 : ((nMatchOffset <= (8192 + 512)) ? 12 : 16));
               nNoRepmatchScore = cur_arrival[j].score + nScorePenalty - nDisableScore;

               nNonRepMatchArrivalIdx = j;
               break;
            }
         }

         int nMatchLenCost;
         if (nMatchLen >= nLeaveAloneMatchSize) {
            nStartingMatchLen = nMatchLen;
            nMatchLenCost = 4 + 24 + 8 /* token */;
         }
         else {
            nStartingMatchLen = nMinMatchSize;
            nMatchLenCost = /* 0 + */ 8 /* token */;
         }

         for (k = nStartingMatchLen; k <= nMatchLen; k++) {
            if (k == (MATCH_RUN_LEN_V2 + MIN_MATCH_SIZE_V2)) {
               nMatchLenCost = 4 + 8 /* token */;
            }
            else {
               if (k == (MATCH_RUN_LEN_V2 + 15 + MIN_MATCH_SIZE_V2))
                  nMatchLenCost = 4 + 8 + 8 /* token */;
               else {
                  if (k == 256)
                     nMatchLenCost = 4 + 24 + 8 /* token */;
               }
            }

            lzsa_arrival *pDestSlots = &cur_arrival[k << ARRIVALS_PER_POSITION_SHIFT_V2];

            /* Insert non-repmatch candidate */

            if (nNonRepMatchArrivalIdx >= 0) {
               const int nCodingChoiceCost = nMatchLenCost + nNoRepmatchOffsetCost;

               if (nCodingChoiceCost < pDestSlots[nArrivalsPerPosition - 2].cost ||
                  (nCodingChoiceCost == pDestSlots[nArrivalsPerPosition - 2].cost && nNoRepmatchScore < pDestSlots[nArrivalsPerPosition - 2].score &&
                     (nCodingChoiceCost != pDestSlots[nArrivalsPerPosition - 1].cost || nMatchOffset != pDestSlots[nArrivalsPerPosition - 1].rep_offset))) {
                  int exists = 0, n;

                  for (n = 0;
                     pDestSlots[n].cost < nCodingChoiceCost;
                     n++) {
                     if (pDestSlots[n].rep_offset == nMatchOffset) {
                        exists = 1;
                        break;
                     }
                  }

                  if (!exists) {
                     for (;
                        n < nArrivalsPerPosition && pDestSlots[n].cost == nCodingChoiceCost && nNoRepmatchScore >= pDestSlots[n].score;
                        n++) {
                        if (pDestSlots[n].rep_offset == nMatchOffset) {
                           exists = 1;
                           break;
                        }
                     }

                     if (!exists) {
                        if (n < nArrivalsPerPosition - 1) {
                           int z;

                           for (z = n;
                              z < nArrivalsPerPosition - 1 && pDestSlots[z].cost == nCodingChoiceCost;
                              z++) {
                              if (pDestSlots[z].rep_offset == nMatchOffset) {
                                 if (!nInsertForwardReps || pDestSlots[nArrivalsPerPosition - 1].from_slot || pDestSlots[z].rep_pos >= i) {
                                    exists = 1;
                                 }
                                 break;
                              }
                           }

                           if (!exists) {
                              for (; z < nArrivalsPerPosition - 1 && pDestSlots[z].from_slot; z++) {
                                 if (pDestSlots[z].rep_offset == nMatchOffset)
                                    break;
                              }

                              memmove(&pDestSlots[n + 1],
                                 &pDestSlots[n],
                                 sizeof(lzsa_arrival) * (z - n));

                              lzsa_arrival* pDestArrival = &pDestSlots[n];
                              pDestArrival->cost = nCodingChoiceCost;
                              pDestArrival->rep_offset = nMatchOffset;
                              pDestArrival->from_slot = nNonRepMatchArrivalIdx + 1;
                              pDestArrival->from_pos = i - nStartOffset;
                              pDestArrival->rep_len = k;
                              pDestArrival->match_len = k;
                              pDestArrival->num_literals = 0;
                              pDestArrival->rep_pos = i;
                              pDestArrival->score = nNoRepmatchScore + nDisableScore;
                              nRepLenHandledMask[k >> 3] &= ~((1 ^ nReduce) << (k & 7));
                           }
                        }
                     }
                  }
               }
            }

            /* Insert repmatch candidates */

            if (k > nMinOverallRepLen && k <= nMaxOverallRepLen && (nRepLenHandledMask[k >> 3] & (1 << (k & 7))) == 0) {
               int nCurRepMatchArrival;

               nRepLenHandledMask[k >> 3] |= 1 << (k & 7);

               for (nCurRepMatchArrival = 0; (j = nRepMatchArrivalIdxAndLen[nCurRepMatchArrival]) >= 0; nCurRepMatchArrival += 2) {
                  if (nRepMatchArrivalIdxAndLen[nCurRepMatchArrival + 1] >= k) {
                     const int nMaskOffset = (j << 7) + (k >> 3);

                     if (nReduce || !(nRepSlotHandledMask[nMaskOffset] & (1 << (k & 7)))) {
                        const int nScore = cur_arrival[j].score + 2 - nDisableScore;
                        const int nRepOffset = cur_arrival[j].rep_offset;

                        if (nRepOffset != pDestSlots[nArrivalsPerPosition - 1].rep_offset) {
                           const int nPrevCost = cur_arrival[j].cost;
                           const int nRepCodingChoiceCost = nPrevCost /* the actual cost of the literals themselves accumulates up the chain */ + nMatchLenCost;

                           if (nRepCodingChoiceCost < pDestSlots[nArrivalsPerPosition - 1].cost ||
                              (nRepCodingChoiceCost == pDestSlots[nArrivalsPerPosition - 1].cost && nScore < pDestSlots[nArrivalsPerPosition - 1].score)) {
                              int exists = 0, n;

                              for (n = 0;
                                 pDestSlots[n].cost < nRepCodingChoiceCost;
                                 n++) {
                                 if (pDestSlots[n].rep_offset == nRepOffset) {
                                    exists = 1;
                                    if (!nReduce)
                                       nRepSlotHandledMask[nMaskOffset] |= 1 << (k & 7);
                                    break;
                                 }
                              }

                              if (!exists) {
                                 for (;
                                    n < nArrivalsPerPosition && pDestSlots[n].cost == nRepCodingChoiceCost && nScore >= pDestSlots[n].score;
                                    n++) {
                                    if (pDestSlots[n].rep_offset == nRepOffset) {
                                       exists = 1;
                                       break;
                                    }
                                 }

                                 if (!exists) {
                                    if (n < nArrivalsPerPosition) {
                                       int z;

                                       for (z = n;
                                          z < nArrivalsPerPosition - 1 && pDestSlots[z].cost == nRepCodingChoiceCost;
                                          z++) {
                                          if (pDestSlots[z].rep_offset == nRepOffset) {
                                             exists = 1;
                                             break;
                                          }
                                       }

                                       if (!exists) {
                                          for (; z < nArrivalsPerPosition - 1 && pDestSlots[z].from_slot; z++) {
                                             if (pDestSlots[z].rep_offset == nRepOffset)
                                                break;
                                          }

                                          memmove(&pDestSlots[n + 1],
                                             &pDestSlots[n],
                                             sizeof(lzsa_arrival) * (z - n));

                                          lzsa_arrival* pDestArrival = &pDestSlots[n];
                                          pDestArrival->cost = nRepCodingChoiceCost;
                                          pDestArrival->rep_offset = nRepOffset;
                                          pDestArrival->from_slot = j + 1;
                                          pDestArrival->from_pos = i - nStartOffset;
                                          pDestArrival->rep_len = k;
                                          pDestArrival->match_len = k;
                                          pDestArrival->num_literals = 0;
                                          pDestArrival->rep_pos = i;
                                          pDestArrival->score = nScore + nDisableScore;
                                          nRepLenHandledMask[k >> 3] &= ~((1 ^ nReduce) << (k & 7));
                                       }
                                    }
                                 }
                              }
                           }
                           else {
                              break;
                           }
                        }
                     }
                  }
               }

               if (k < nMaxRepInsertedLen)
                  nMinOverallRepLen = k;
            }
         }

         if (nMatchLen >= LCP_MAX && ((m + 1) >= NMATCHES_PER_INDEX_V2 || (match[m + 1].length & 0x7fff) < LCP_MAX))
            break;
      }
   }

   if (!nInsertForwardReps) {
      const lzsa_arrival* end_arrival = &arrival[i << ARRIVALS_PER_POSITION_SHIFT_V2];
      lzsa_match* pBestMatch = pCompressor->best_match - nStartOffset;

      while (end_arrival->from_slot > 0 && (end_arrival->from_pos + nStartOffset) < nEndOffset) {
         pBestMatch[end_arrival->from_pos + nStartOffset].length = end_arrival->match_len;
         pBestMatch[end_arrival->from_pos + nStartOffset].offset = (end_arrival->match_len) ? end_arrival->rep_offset : 0;
         end_arrival = &arrival[((end_arrival->from_pos + nStartOffset) << ARRIVALS_PER_POSITION_SHIFT_V2) + (end_arrival->from_slot - 1)];
      }
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
static int lzsa_optimize_command_count_v2(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset) {
   lzsa_match* pBestMatch = pCompressor->best_match - nStartOffset;
   int i;
   int nNumLiterals = 0;
   int nPrevRepMatchOffset = 0;
   int nRepMatchOffset = 0;
   int nRepMatchLen = 0;
   int nRepIndex = 0;
   int nDidReduce = 0;

   for (i = nStartOffset; i < nEndOffset; ) {
      lzsa_match *pMatch = pBestMatch + i;

      if (pMatch->length == 0 &&
         (i + 1) < nEndOffset &&
         pBestMatch[i + 1].length >= MIN_MATCH_SIZE_V2 &&
         pBestMatch[i + 1].length < MAX_VARLEN &&
         pBestMatch[i + 1].offset &&
         i >= pBestMatch[i + 1].offset &&
         (i + pBestMatch[i + 1].length + 1) <= nEndOffset &&
         !memcmp(pInWindow + i - (pBestMatch[i + 1].offset), pInWindow + i, pBestMatch[i + 1].length + 1)) {
         const int nCurLenSize = lzsa_get_match_varlen_size_v2(pBestMatch[i + 1].length - MIN_MATCH_SIZE_V2);
         const int nReducedLenSize = lzsa_get_match_varlen_size_v2(pBestMatch[i + 1].length + 1 - MIN_MATCH_SIZE_V2);

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

      if (pMatch->length >= MIN_MATCH_SIZE_V2) {
         if ((i + pMatch->length) < nEndOffset /* Don't consider the last match in the block, we can only reduce a match inbetween other tokens */) {
            int nNextIndex = i + pMatch->length;
            int nNextLiterals = 0;

            while (nNextIndex < nEndOffset && pBestMatch[nNextIndex].length < MIN_MATCH_SIZE_V2) {
               nNextLiterals++;
               nNextIndex++;
            }

            if (nNextIndex < nEndOffset) {
               /* This command is a match, is followed by 'nNextLiterals' literals and then by another match */

               if (nRepMatchOffset && pMatch->offset != nRepMatchOffset && (pBestMatch[nNextIndex].offset != pMatch->offset ||
                  ((pMatch->offset <= 32) ? 4 : ((pMatch->offset <= 512) ? 8 : ((pMatch->offset <= (8192 + 512)) ? 12 : 16))) >
                  ((pBestMatch[nNextIndex].offset <= 32) ? 4 : ((pBestMatch[nNextIndex].offset <= 512) ? 8 : ((pBestMatch[nNextIndex].offset <= (8192 + 512)) ? 12 : 16))))) {
                  /* Check if we can change the current match's offset to be the same as the previous match's offset, and get an extra repmatch. This will occur when
                   * matching large regions of identical bytes for instance, where there are too many offsets to be considered by the parser, and when not compressing to favor the
                   * ratio (the forward arrivals parser already has this covered). */
                  if (i >= nRepMatchOffset &&
                     !memcmp(pInWindow + i - nRepMatchOffset, pInWindow + i, pMatch->length)) {
                     pMatch->offset = nRepMatchOffset;
                     nDidReduce = 1;
                  }
               }

               if (pBestMatch[nNextIndex].offset && pMatch->offset != pBestMatch[nNextIndex].offset) {
                  /* Otherwise, try to gain a match forward as well */
                  if (i >= pBestMatch[nNextIndex].offset && (i + pMatch->length) <= nEndOffset) {
                     int nMaxLen = 0;
                     const unsigned char *pInWindowAtPos = pInWindow + i;
                     while ((nMaxLen + 8) < pMatch->length && !memcmp(pInWindowAtPos + nMaxLen - pBestMatch[nNextIndex].offset, pInWindowAtPos + nMaxLen, 8))
                        nMaxLen += 8;
                     while ((nMaxLen + 4) < pMatch->length && !memcmp(pInWindowAtPos + nMaxLen - pBestMatch[nNextIndex].offset, pInWindowAtPos + nMaxLen, 4))
                        nMaxLen += 4;
                     while (nMaxLen < pMatch->length && pInWindowAtPos[nMaxLen - pBestMatch[nNextIndex].offset] == pInWindowAtPos[nMaxLen])
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
                        nPartialSizeBefore += (pBestMatch[nNextIndex].offset <= 32) ? 4 : ((pBestMatch[nNextIndex].offset <= 512) ? 8 : ((pBestMatch[nNextIndex].offset <= (8192 + 512)) ? 12 : 16));

                        nPartialSizeAfter = lzsa_get_match_varlen_size_v2(nMaxLen - MIN_MATCH_SIZE_V2);
                        nPartialSizeAfter += lzsa_get_literals_varlen_size_v2(nNextLiterals + (pMatch->length - nMaxLen)) + ((pMatch->length - nMaxLen) << 3);
                        if (nRepMatchOffset != pBestMatch[nNextIndex].offset)
                           nPartialSizeAfter += (pBestMatch[nNextIndex].offset <= 32) ? 4 : ((pBestMatch[nNextIndex].offset <= 512) ? 8 : ((pBestMatch[nNextIndex].offset <= (8192 + 512)) ? 12 : 16));

                        if (nPartialSizeAfter < nPartialSizeBefore) {
                           const int nMatchLen = pMatch->length;
                           int j;

                           /* We gain a repmatch that is shorter than the original match as this is the best we can do, so it is followed by extra literals, but
                            * we have calculated that this is shorter */
                           pMatch->length = nMaxLen;
                           pMatch->offset = pBestMatch[nNextIndex].offset;
                           for (j = nMaxLen; j < nMatchLen; j++) {
                              pBestMatch[i + j].length = 0;
                           }
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
                  int nNextCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNextLiterals) + /* (nNextLiterals << 3) + */ lzsa_get_match_varlen_size_v2(pBestMatch[nNextIndex].length - MIN_MATCH_SIZE_V2);
                  if (pBestMatch[nNextIndex].offset != pMatch->offset)
                     nNextCommandSize += (pBestMatch[nNextIndex].offset <= 32) ? 4 : ((pBestMatch[nNextIndex].offset <= 512) ? 8 : ((pBestMatch[nNextIndex].offset <= (8192 + 512)) ? 12 : 16));

                  const int nOriginalCombinedCommandSize = nCurCommandSize + nNextCommandSize;

                  /* Calculate the cost of replacing this match command by literals + the next command with the cost of encoding these literals (excluding 'nNumLiterals' bytes) */
                  int nReducedCommandSize = (pMatch->length << 3) + 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNumLiterals + pMatch->length + nNextLiterals) + /* (nNextLiterals << 3) + */ lzsa_get_match_varlen_size_v2(pBestMatch[nNextIndex].length - MIN_MATCH_SIZE_V2);
                  if (pBestMatch[nNextIndex].offset != nRepMatchOffset)
                     nReducedCommandSize += (pBestMatch[nNextIndex].offset <= 32) ? 4 : ((pBestMatch[nNextIndex].offset <= 512) ? 8 : ((pBestMatch[nNextIndex].offset <= (8192 + 512)) ? 12 : 16));

                  int nReplaceRepOffset = 0;
                  if (nRepMatchOffset && nRepMatchOffset != nPrevRepMatchOffset && nRepMatchLen >= MIN_MATCH_SIZE_V2 && nRepMatchOffset != pBestMatch[nNextIndex].offset && nRepIndex >= pBestMatch[nNextIndex].offset &&
                     (nRepIndex - pBestMatch[nNextIndex].offset + nRepMatchLen) <= nEndOffset &&
                     !memcmp(pInWindow + nRepIndex - nRepMatchOffset, pInWindow + nRepIndex - pBestMatch[nNextIndex].offset, nRepMatchLen)) {
                     /* Replacing this match command by literals would let us create a repmatch */
                     nReplaceRepOffset = 1;
                     nReducedCommandSize -= (nRepMatchOffset <= 32) ? 4 : ((nRepMatchOffset <= 512) ? 8 : ((nRepMatchOffset <= (8192 + 512)) ? 12 : 16));
                  }

                  if (nOriginalCombinedCommandSize >= nReducedCommandSize) {
                     /* Reduce */
                     const int nMatchLen = pMatch->length;
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

         if ((i + pMatch->length) < nEndOffset && pMatch->offset && pMatch->length >= MIN_MATCH_SIZE_V2 &&
            pBestMatch[i + pMatch->length].offset &&
            pBestMatch[i + pMatch->length].length >= MIN_MATCH_SIZE_V2 &&
            (pMatch->length + pBestMatch[i + pMatch->length].length) <= MAX_VARLEN &&
            (i + pMatch->length) >= pMatch->offset &&
            (i + pMatch->length) >= pBestMatch[i + pMatch->length].offset &&
            (i + pMatch->length + pBestMatch[i + pMatch->length].length) <= nEndOffset &&
            !memcmp(pInWindow + i - pMatch->offset + pMatch->length,
               pInWindow + i + pMatch->length - pBestMatch[i + pMatch->length].offset,
               pBestMatch[i + pMatch->length].length)) {

            int nNextIndex = i + pMatch->length;

            while (nNextIndex < nEndOffset && pBestMatch[nNextIndex].length < MIN_MATCH_SIZE_V2) {
               nNextIndex++;
            }

            int nCurPartialSize = lzsa_get_match_varlen_size_v2(pMatch->length - MIN_MATCH_SIZE_V2);

            nCurPartialSize += 8 /* token */ + /* lzsa_get_literals_varlen_size_v2(0) + */ lzsa_get_match_varlen_size_v2(pBestMatch[i + pMatch->length].length - MIN_MATCH_SIZE_V2);
            if (pBestMatch[i + pMatch->length].offset != pMatch->offset)
               nCurPartialSize += (pBestMatch[i + pMatch->length].offset <= 32) ? 4 : ((pBestMatch[i + pMatch->length].offset <= 512) ? 8 : ((pBestMatch[i + pMatch->length].offset <= (8192 + 512)) ? 12 : 16));

            int nReducedPartialSize = lzsa_get_match_varlen_size_v2(pMatch->length + pBestMatch[i + pMatch->length].length - MIN_MATCH_SIZE_V2);

            if (nNextIndex < nEndOffset) {
               const int nNextOffset = pBestMatch[nNextIndex].offset;

               if (nNextOffset != pBestMatch[i + pMatch->length].offset)
                  nCurPartialSize += (nNextOffset <= 32) ? 4 : ((nNextOffset <= 512) ? 8 : ((nNextOffset <= (8192 + 512)) ? 12 : 16));

               if (nNextOffset != pMatch->offset)
                  nReducedPartialSize += (nNextOffset <= 32) ? 4 : ((nNextOffset <= 512) ? 8 : ((nNextOffset <= (8192 + 512)) ? 12 : 16));
            }

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

         nPrevRepMatchOffset = nRepMatchOffset;
         nRepMatchLen = pMatch->length;
         nRepMatchOffset = pMatch->offset;
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
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 * @param pOutData pointer to output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 *
 * @return size of compressed data in output buffer, or -1 if the data is uncompressible
 */
static int lzsa_write_block_v2(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset, unsigned char *pOutData, const int nMaxOutDataSize) {
   const lzsa_match *pBestMatch = pCompressor->best_match - nStartOffset;
   int i;
   int nNumLiterals = 0;
   int nInFirstLiteralOffset = 0;
   int nOutOffset = 0;
   int nCurNibbleOffset = -1;
   int nRepMatchOffset = 0;

   for (i = nStartOffset; i < nEndOffset; ) {
      const lzsa_match *pMatch = pBestMatch + i;

      if (pMatch->length >= MIN_MATCH_SIZE_V2) {
         const int nMatchLen = pMatch->length;
         const int nMatchOffset = pMatch->offset;
         const int nEncodedMatchLen = nMatchLen - MIN_MATCH_SIZE_V2;
         const int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN_V2) ? LITERALS_RUN_LEN_V2 : nNumLiterals;
         const int nTokenMatchLen = (nEncodedMatchLen >= MATCH_RUN_LEN_V2) ? MATCH_RUN_LEN_V2 : nEncodedMatchLen;
         int nTokenOffsetMode;
         int nOffsetSize;

         if (nMatchOffset == nRepMatchOffset) {
            nTokenOffsetMode = 0xe0;
            nOffsetSize = 0;
         }
         else {
            if (nMatchOffset <= 32) {
               nTokenOffsetMode = /* 0x00 | */ ((((-nMatchOffset) & 0x01) << 5) ^ 0x20);
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

         const int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNumLiterals) + (nNumLiterals << 3) + nOffsetSize /* match offset */ + lzsa_get_match_varlen_size_v2(nEncodedMatchLen);

         if ((nOutOffset + ((nCommandSize + 7) >> 3)) > nMaxOutDataSize)
            return -1;
         if (nMatchOffset < MIN_OFFSET || nMatchOffset > MAX_OFFSET)
            return -1;

         pOutData[nOutOffset++] = nTokenOffsetMode | (nTokenLiteralsLen << 3) | nTokenMatchLen;
         nOutOffset = lzsa_write_literals_varlen_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, nNumLiterals);
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
            nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, ((-nMatchOffset) & 0x1e) >> 1);
            if (nOutOffset < 0) return -1;
         }
         else if (nTokenOffsetMode == 0x40 || nTokenOffsetMode == 0x60) {
            pOutData[nOutOffset++] = (-nMatchOffset) & 0xff;
         }
         else if (nTokenOffsetMode == 0x80 || nTokenOffsetMode == 0xa0) {
            nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, ((-(nMatchOffset - 512)) >> 9) & 0x0f);
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

         nOutOffset = lzsa_write_match_varlen_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, nEncodedMatchLen);
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
      const int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN_V2) ? LITERALS_RUN_LEN_V2 : nNumLiterals;
      const int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNumLiterals) + (nNumLiterals << 3);

      if ((nOutOffset + ((nCommandSize + 7) >> 3)) > nMaxOutDataSize)
         return -1;

      if (pCompressor->flags & LZSA_FLAG_RAW_BLOCK)
         pOutData[nOutOffset++] = (nTokenLiteralsLen << 3) | 0xe7;
      else
         pOutData[nOutOffset++] = (nTokenLiteralsLen << 3) /* | 0x00 */;
      nOutOffset = lzsa_write_literals_varlen_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, nNumLiterals);
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

      if (nOutOffset >= nMaxOutDataSize)
         return -1;

      nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, 15);   /* Extended match length nibble */
      if (nOutOffset < 0) return -1;

      if ((nOutOffset + 1) > nMaxOutDataSize)
         return -1;

      pOutData[nOutOffset++] = 232;    /* EOD match length byte */
   }

   if (nCurNibbleOffset != -1) {
      nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, 0);
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
   int nCurNibbleOffset = -1;
   const int nNumLiterals = nEndOffset - nStartOffset;
   const int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN_V2) ? LITERALS_RUN_LEN_V2 : nNumLiterals;
   int nOutOffset = 0;

   const int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNumLiterals) + (nNumLiterals << 3) + 4 + 8;
   if ((nOutOffset + ((nCommandSize + 7) >> 3)) > nMaxOutDataSize)
      return -1;

   pCompressor->num_commands = 0;
   pOutData[nOutOffset++] = (nTokenLiteralsLen << 3) | 0xe7;

   nOutOffset = lzsa_write_literals_varlen_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, nNumLiterals);
   if (nOutOffset < 0) return -1;

   if (nNumLiterals != 0) {
      memcpy(pOutData + nOutOffset, pInWindow + nStartOffset, nNumLiterals);
      nOutOffset += nNumLiterals;
   }

   /* Emit EOD marker for raw block */

   nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, 15);   /* Extended match length nibble */
   if (nOutOffset < 0) return -1;

   if ((nOutOffset + 1) > nMaxOutDataSize)
      return -1;

   pOutData[nOutOffset++] = 232;    /* EOD match length byte */

   pCompressor->num_commands++;

   if (nCurNibbleOffset != -1) {
      nOutOffset = lzsa_write_nibble_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, 0);
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
   const int nEndOffset = nPreviousBlockSize + nInDataSize;
   const int nArrivalsPerPosition = (nInDataSize < 65536) ? NARRIVALS_PER_POSITION_V2_BIG : NARRIVALS_PER_POSITION_V2_SMALL;
   int nResult;
   int *rle_len = (int*)pCompressor->intervals /* reuse */;
   int i, nDidReduce, nPasses;

   i = 0;
   while (i < nEndOffset) {
      int nRangeStartIdx = i;
      const unsigned char c = pInWindow[nRangeStartIdx];

      do {
         i++;
      } while (i < nEndOffset && pInWindow[i] == c);
      while (nRangeStartIdx < i) {
         rle_len[nRangeStartIdx] = i - nRangeStartIdx;
         nRangeStartIdx++;
      }
   }

   /* Compress optimally without breaking ties in favor of less tokens */
   
   memset(pCompressor->best_match, 0, BLOCK_SIZE * sizeof(lzsa_match));
   lzsa_optimize_forward_v2(pCompressor, pInWindow, nPreviousBlockSize, nEndOffset, 0 /* reduce */, (nInDataSize < 65536) ? 1 : 0 /* insert forward reps */, nArrivalsPerPosition);

   if (nInDataSize < 65536) {
      int* first_offset_for_byte = pCompressor->first_offset_for_byte;
      int* next_offset_for_pos = pCompressor->next_offset_for_pos;
      int* offset_cache = pCompressor->offset_cache;
      int nPosition;

      /* Supplement small matches */

      memset(first_offset_for_byte, 0xff, sizeof(int) * 65536);
      memset(next_offset_for_pos, 0xff, sizeof(int) * nInDataSize);

      for (nPosition = nPreviousBlockSize; nPosition < nEndOffset - 1; nPosition++) {
         next_offset_for_pos[nPosition - nPreviousBlockSize] = first_offset_for_byte[((unsigned int)pInWindow[nPosition]) | (((unsigned int)pInWindow[nPosition + 1]) << 8)];
         first_offset_for_byte[((unsigned int)pInWindow[nPosition]) | (((unsigned int)pInWindow[nPosition + 1]) << 8)] = nPosition;
      }

      for (nPosition = nPreviousBlockSize + 1; nPosition < (nEndOffset - 1); nPosition++) {
         const int nMaxMatchLen = ((nPosition + 16) < nEndOffset) ? 16 : (nEndOffset - nPosition);
         lzsa_match* match = pCompressor->match + ((nPosition - nPreviousBlockSize) << MATCHES_PER_INDEX_SHIFT_V2);
         int m = 0, nInserted = 0;
         int nMatchPos;

         while (m < 15 && match[m].length)
            m++;

         for (nMatchPos = next_offset_for_pos[nPosition - nPreviousBlockSize]; m < 15 && nMatchPos >= 0; nMatchPos = next_offset_for_pos[nMatchPos - nPreviousBlockSize]) {
            const int nMatchOffset = nPosition - nMatchPos;
            int nAlreadyExists = 0;
            int nExistingMatchIdx;

            for (nExistingMatchIdx = 0; nExistingMatchIdx < m; nExistingMatchIdx++) {
               if (match[nExistingMatchIdx].offset == nMatchOffset) {
                  nAlreadyExists = 1;
                  break;
               }
            }

            if (!nAlreadyExists) {
               int nMatchLen = 2;
               while ((nMatchLen + 8) < nMaxMatchLen && !memcmp(pInWindow + nPosition + nMatchLen, pInWindow + nMatchPos + nMatchLen, 8))
                  nMatchLen += 8;
               while ((nMatchLen + 4) < nMaxMatchLen && !memcmp(pInWindow + nPosition + nMatchLen, pInWindow + nMatchPos + nMatchLen, 4))
                  nMatchLen += 4;
               while (nMatchLen < nMaxMatchLen && pInWindow[nPosition + nMatchLen] == pInWindow[nMatchPos + nMatchLen])
                  nMatchLen++;
               match[m].length = nMatchLen;
               match[m].offset = nMatchOffset;
               m++;
               nInserted++;
               if (nInserted >= 12)
                  break;
            }
         }
      }

      /* Supplement matches further */

      memset(offset_cache, 0xff, sizeof(int) * 2048);

      for (nPosition = nPreviousBlockSize + 1; nPosition < (nEndOffset - 1); nPosition++) {
         lzsa_match* match = pCompressor->match + ((nPosition - nPreviousBlockSize) << MATCHES_PER_INDEX_SHIFT_V2);

         if (match[0].length < 5) {
            const int nMaxMatchLen = ((nPosition + 16) < nEndOffset) ? 16 : (nEndOffset - nPosition);
            int m = 0, nInserted = 0;
            int nMatchPos;
            int nMaxForwardPos = nPosition + 2 + 1 + 2;

            if (nMaxForwardPos > (nEndOffset - 2))
               nMaxForwardPos = nEndOffset - 2;

            while (m < 46 && match[m].length) {
               offset_cache[match[m].offset & 2047] = nPosition;
               m++;
            }

            for (nMatchPos = next_offset_for_pos[nPosition - nPreviousBlockSize]; m < 46 && nMatchPos >= 0; nMatchPos = next_offset_for_pos[nMatchPos - nPreviousBlockSize]) {
               const int nMatchOffset = nPosition - nMatchPos;

               if (nMatchOffset <= MAX_OFFSET) {
                  int nAlreadyExists = 0;

                  if (offset_cache[nMatchOffset & 2047] == nPosition) {
                     int nExistingMatchIdx;

                     for (nExistingMatchIdx = 0; nExistingMatchIdx < m; nExistingMatchIdx++) {
                        if (match[nExistingMatchIdx].offset == nMatchOffset) {
                           nAlreadyExists = 1;
                           break;
                        }
                     }
                  }

                  if (!nAlreadyExists) {
                     int nForwardPos = nPosition + 2;

                     if (nForwardPos >= nMatchOffset) {
                        int nGotMatch = 0;

                        while (nForwardPos < nMaxForwardPos) {
                           if (!memcmp(pInWindow + nForwardPos, pInWindow + nForwardPos - nMatchOffset, 2)) {
                              nGotMatch = 1;
                              break;
                           }
                           nForwardPos++;
                        }

                        if (nGotMatch) {
                           int nMatchLen = 2;
                           while ((nMatchLen + 8) < nMaxMatchLen && !memcmp(pInWindow + nPosition + nMatchLen, pInWindow + nMatchPos + nMatchLen, 8))
                              nMatchLen += 8;
                           while ((nMatchLen + 4) < nMaxMatchLen && !memcmp(pInWindow + nPosition + nMatchLen, pInWindow + nMatchPos + nMatchLen, 4))
                              nMatchLen += 4;
                           while (nMatchLen < nMaxMatchLen && pInWindow[nPosition + nMatchLen] == pInWindow[nMatchPos + nMatchLen])
                              nMatchLen++;
                           match[m].length = nMatchLen | 0x8000;
                           match[m].offset = nMatchOffset;
                           m++;

                           lzsa_insert_forward_match_v2(pCompressor, pInWindow, nPosition, nMatchOffset, nPreviousBlockSize, nEndOffset, 8);

                           nInserted++;
                           if (nInserted >= 3)
                              break;
                        }
                     }
                  }
               }
               else {
                  break;
               }
            }
         }
      }

      for (nPosition = nPreviousBlockSize + 1; nPosition < (nEndOffset - 1); nPosition++) {
         lzsa_match* match = pCompressor->match + ((nPosition - nPreviousBlockSize) << MATCHES_PER_INDEX_SHIFT_V2);

         if (match[0].length < 8) {
            const int nMaxMatchLen = ((nPosition + 16) < nEndOffset) ? 16 : (nEndOffset - nPosition);
            int m = 0, nInserted = 0;
            int nMatchPos;
            int nMaxForwardPos = nPosition + 2 + 1 + 6;

            if (nMaxForwardPos > (nEndOffset - 2))
               nMaxForwardPos = nEndOffset - 2;

            while (m < 63 && match[m].length) {
               offset_cache[match[m].offset & 2047] = nPosition;
               m++;
            }

            for (nMatchPos = next_offset_for_pos[nPosition - nPreviousBlockSize]; m < 63 && nMatchPos >= 0; nMatchPos = next_offset_for_pos[nMatchPos - nPreviousBlockSize]) {
               const int nMatchOffset = nPosition - nMatchPos;

               if (nMatchOffset <= MAX_OFFSET) {
                  int nAlreadyExists = 0;

                  if (offset_cache[nMatchOffset & 2047] == nPosition) {
                     int nExistingMatchIdx;

                     for (nExistingMatchIdx = 0; nExistingMatchIdx < m; nExistingMatchIdx++) {
                        if (match[nExistingMatchIdx].offset == nMatchOffset) {
                           nAlreadyExists = 1;
                           break;
                        }
                     }
                  }

                  if (!nAlreadyExists) {
                     int nForwardPos = nPosition + 2;

                     if (nForwardPos >= nMatchOffset) {
                        int nGotMatch = 0;

                        while (nForwardPos < nMaxForwardPos) {
                           if (!memcmp(pInWindow + nForwardPos, pInWindow + nForwardPos - nMatchOffset, 2)) {
                              nGotMatch = 1;
                              break;
                           }
                           nForwardPos++;
                        }

                        if (nGotMatch) {
                           int nMatchLen = 2;

                           while ((nMatchLen + 8) < nMaxMatchLen && !memcmp(pInWindow + nPosition + nMatchLen, pInWindow + nMatchPos + nMatchLen, 8))
                              nMatchLen += 8;
                           while ((nMatchLen + 4) < nMaxMatchLen && !memcmp(pInWindow + nPosition + nMatchLen, pInWindow + nMatchPos + nMatchLen, 4))
                              nMatchLen += 4;
                           while (nMatchLen < nMaxMatchLen && pInWindow[nPosition + nMatchLen] == pInWindow[nMatchPos + nMatchLen] )
                              nMatchLen++;

                           match[m].length = nMatchLen;
                           match[m].offset = nMatchOffset;
                           m++;

                           lzsa_insert_forward_match_v2(pCompressor, pInWindow, nPosition, nMatchOffset, nPreviousBlockSize, nEndOffset, 8);

                           nInserted++;
                           if (nInserted >= 12)
                              break;
                        }
                     }
                  }
               }
               else {
                  break;
               }
            }
         }
      }

      /* Compress optimally and do break ties in favor of less tokens */
      lzsa_optimize_forward_v2(pCompressor, pInWindow, nPreviousBlockSize, nEndOffset, 1 /* reduce */, 0 /* use forward reps */, NARRIVALS_PER_POSITION_V2_MAX);
   }

   /* Try to reduce final command set, wherever possible */
   nPasses = 0;
   do {
      nDidReduce = lzsa_optimize_command_count_v2(pCompressor, pInWindow, nPreviousBlockSize, nEndOffset);
      nPasses++;
   } while (nDidReduce && nPasses < 20);

   /* Write compressed block */
   nResult = lzsa_write_block_v2(pCompressor, pInWindow, nPreviousBlockSize, nEndOffset, pOutData, nMaxOutDataSize);
   if (nResult < 0 && (pCompressor->flags & LZSA_FLAG_RAW_BLOCK)) {
      nResult = lzsa_write_raw_uncompressed_block_v2(pCompressor, pInWindow, nPreviousBlockSize, nEndOffset, pOutData, nMaxOutDataSize);
   }

   return nResult;
}
