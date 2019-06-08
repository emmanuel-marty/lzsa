/*
 * shrink_v2.c - LZSA2 block compressor implementation
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
 * Attempt to pick optimal matches, so as to produce the smallest possible output that decompresses to the same input
 *
 * @param pCompressor compression context
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 */
static void lzsa_optimize_matches_v2(lzsa_compressor *pCompressor, const int nStartOffset, const int nEndOffset) {
   int *cost = (int*)pCompressor->pos_data;  /* Reuse */
   int *prev_match = (int*)pCompressor->intervals; /* Reuse */
   lzsa_repmatch_opt *repmatch_opt = pCompressor->repmatch_opt;
   lzsa_match *pBestMatch = pCompressor->best_match;
   int nLastLiteralsOffset;
   int nMinMatchSize = pCompressor->min_match_size;
   const int nFavorRatio = (pCompressor->flags & LZSA_FLAG_FAVOR_RATIO) ? 1 : 0;
   int i;

   cost[nEndOffset - 1] = 8;
   prev_match[nEndOffset - 1] = nEndOffset;
   nLastLiteralsOffset = nEndOffset;

   pCompressor->best_match[nEndOffset - 1].length = 0;
   pCompressor->best_match[nEndOffset - 1].offset = 0;

   repmatch_opt[nEndOffset - 1].best_slot_for_incoming = -1;
   repmatch_opt[nEndOffset - 1].incoming_offset = -1;
   repmatch_opt[nEndOffset - 1].expected_repmatch = 0;

   for (i = nEndOffset - 2; i != (nStartOffset - 1); i--) {
      int nLiteralsCost;

      int nLiteralsLen = nLastLiteralsOffset - i;
      nLiteralsCost = 8 + cost[i + 1];

      /* Add to the cost of encoding literals as their number crosses a variable length encoding boundary.
       * The cost automatically accumulates down the chain. */
      if (nLiteralsLen == LITERALS_RUN_LEN_V2) {
         nLiteralsCost += 4;
      }
      else if (nLiteralsLen == (LITERALS_RUN_LEN_V2 + 15)) {
         nLiteralsCost += 8;
      }
      else if (nLiteralsLen == 256) {
         nLiteralsCost += 16;
      }
      if (pCompressor->best_match[i + 1].length >= MIN_MATCH_SIZE_V2)
         nLiteralsCost += MODESWITCH_PENALTY;

      lzsa_match *pMatch = pCompressor->match + (i << MATCHES_PER_OFFSET_SHIFT);
      int *pSlotCost = pCompressor->slot_cost + (i << MATCHES_PER_OFFSET_SHIFT);
      int m;

      cost[i] = nLiteralsCost;
      pCompressor->best_match[i].length = 0;
      pCompressor->best_match[i].offset = 0;

      repmatch_opt[i].best_slot_for_incoming = -1;
      repmatch_opt[i].incoming_offset = -1;
      repmatch_opt[i].expected_repmatch = 0;

      for (m = 0; m < NMATCHES_PER_OFFSET && pMatch[m].length >= nMinMatchSize; m++) {
         int nBestCost, nBestMatchLen, nBestMatchOffset, nBestUpdatedSlot, nBestUpdatedIndex, nBestExpectedRepMatch;

         nBestCost = nLiteralsCost;
         nBestMatchLen = 0;
         nBestMatchOffset = 0;
         nBestUpdatedSlot = -1;
         nBestUpdatedIndex = -1;
         nBestExpectedRepMatch = 0;

         if (pMatch[m].length >= LEAVE_ALONE_MATCH_SIZE) {
            int nCurCost;
            int nMatchLen = pMatch[m].length;

            if ((i + nMatchLen) > (nEndOffset - LAST_LITERALS))
               nMatchLen = nEndOffset - LAST_LITERALS - i;

            int nCurIndex = prev_match[i + nMatchLen];

            int nMatchOffsetSize = 0;
            int nCurExpectedRepMatch = 1;
            if (nCurIndex >= nEndOffset || pCompressor->best_match[nCurIndex].length < MIN_MATCH_SIZE_V2 ||
                pCompressor->best_match[nCurIndex].offset != pMatch[m].offset) {
               nMatchOffsetSize = (pMatch[m].offset <= 32) ? 4 : ((pMatch[m].offset <= 512) ? 8 : ((pMatch[m].offset <= (8192 + 512)) ? 12 : 16));
               nCurExpectedRepMatch = 0;
            }

            nCurCost = 8 + nMatchOffsetSize + lzsa_get_match_varlen_size_v2(nMatchLen - MIN_MATCH_SIZE_V2);
            nCurCost += cost[i + nMatchLen];
            if (pCompressor->best_match[i + nMatchLen].length >= MIN_MATCH_SIZE_V2)
               nCurCost += MODESWITCH_PENALTY;

            if (nBestCost > (nCurCost - nFavorRatio)) {
               nBestCost = nCurCost;
               nBestMatchLen = nMatchLen;
               nBestMatchOffset = pMatch[m].offset;
               nBestUpdatedSlot = -1;
               nBestUpdatedIndex = -1;
               nBestExpectedRepMatch = nCurExpectedRepMatch;
            }
         }
         else {
            int nMatchLen = pMatch[m].length;
            int k, nMatchRunLen;

            if ((i + nMatchLen) > (nEndOffset - LAST_LITERALS))
               nMatchLen = nEndOffset - LAST_LITERALS - i;

            nMatchRunLen = nMatchLen;
            if (nMatchRunLen > MATCH_RUN_LEN_V2)
               nMatchRunLen = MATCH_RUN_LEN_V2;

            for (k = nMinMatchSize; k < nMatchRunLen; k++) {
               int nCurCost;

               int nCurIndex = prev_match[i + k];
               int nMatchOffsetSize = 0;
               int nCurExpectedRepMatch = 1;
               if (nCurIndex >= nEndOffset || pCompressor->best_match[nCurIndex].length < MIN_MATCH_SIZE_V2 ||
                  pCompressor->best_match[nCurIndex].offset != pMatch[m].offset) {
                  nMatchOffsetSize = (pMatch[m].offset <= 32) ? 4 : ((pMatch[m].offset <= 512) ? 8 : ((pMatch[m].offset <= (8192 + 512)) ? 12 : 16));
                  nCurExpectedRepMatch = 0;
               }

               nCurCost = 8 + nMatchOffsetSize /* no extra match len bytes */;
               nCurCost += cost[i + k];
               if (pCompressor->best_match[i + k].length >= MIN_MATCH_SIZE_V2)
                  nCurCost += MODESWITCH_PENALTY;

               int nCurUpdatedSlot = -1;
               int nCurUpdatedIndex = -1;

               if (nMatchOffsetSize && nCurIndex < nEndOffset && pCompressor->best_match[nCurIndex].length >= MIN_MATCH_SIZE_V2 && !repmatch_opt[nCurIndex].expected_repmatch) {
                  int r;

                  for (r = 0; r < NMATCHES_PER_OFFSET && pCompressor->match[(nCurIndex << MATCHES_PER_OFFSET_SHIFT) + r].length >= MIN_MATCH_SIZE_V2; r++) {
                     if (pCompressor->match[(nCurIndex << MATCHES_PER_OFFSET_SHIFT) + r].offset == pMatch[m].offset) {
                        int nAltCost = nCurCost - nMatchOffsetSize + pCompressor->slot_cost[(nCurIndex << MATCHES_PER_OFFSET_SHIFT) + r] - cost[nCurIndex];

                        if (nAltCost <= nCurCost) {
                           nCurUpdatedSlot = r;
                           nCurUpdatedIndex = nCurIndex;
                           nCurCost = nAltCost;
                           nCurExpectedRepMatch = 2;
                        }
                     }
                  }
               }

               if (nBestCost > (nCurCost - nFavorRatio)) {
                  nBestCost = nCurCost;
                  nBestMatchLen = k;
                  nBestMatchOffset = pMatch[m].offset;
                  nBestUpdatedSlot = nCurUpdatedSlot;
                  nBestUpdatedIndex = nCurUpdatedIndex;
                  nBestExpectedRepMatch = nCurExpectedRepMatch;
               }
            }

            for (; k <= nMatchLen; k++) {
               int nCurCost;

               int nCurIndex = prev_match[i + k];
               int nMatchOffsetSize = 0;
               int nCurExpectedRepMatch = 1;
               if (nCurIndex >= nEndOffset || pCompressor->best_match[nCurIndex].length < MIN_MATCH_SIZE_V2 ||
                  pCompressor->best_match[nCurIndex].offset != pMatch[m].offset) {
                  nMatchOffsetSize = (pMatch[m].offset <= 32) ? 4 : ((pMatch[m].offset <= 512) ? 8 : ((pMatch[m].offset <= (8192 + 512)) ? 12 : 16));
                  nCurExpectedRepMatch = 0;
               }

               nCurCost = 8 + nMatchOffsetSize + lzsa_get_match_varlen_size_v2(k - MIN_MATCH_SIZE_V2);
               nCurCost += cost[i + k];
               if (pCompressor->best_match[i + k].length >= MIN_MATCH_SIZE_V2)
                  nCurCost += MODESWITCH_PENALTY;

               int nCurUpdatedSlot = -1;
               int nCurUpdatedIndex = -1;

               if (nMatchOffsetSize && nCurIndex < nEndOffset && pCompressor->best_match[nCurIndex].length >= MIN_MATCH_SIZE_V2 && !repmatch_opt[nCurIndex].expected_repmatch) {
                  int r;

                  for (r = 0; r < NMATCHES_PER_OFFSET && pCompressor->match[(nCurIndex << MATCHES_PER_OFFSET_SHIFT) + r].length >= MIN_MATCH_SIZE_V2; r++) {
                     if (pCompressor->match[(nCurIndex << MATCHES_PER_OFFSET_SHIFT) + r].offset == pMatch[m].offset) {
                        int nAltCost = nCurCost - nMatchOffsetSize + pCompressor->slot_cost[(nCurIndex << MATCHES_PER_OFFSET_SHIFT) + r] - cost[nCurIndex];

                        if (nAltCost <= nCurCost) {
                           nCurUpdatedSlot = r;
                           nCurUpdatedIndex = nCurIndex;
                           nCurCost = nAltCost;
                           nCurExpectedRepMatch = 2;
                        }
                     }
                  }
               }

               if (nBestCost > (nCurCost - nFavorRatio)) {
                  nBestCost = nCurCost;
                  nBestMatchLen = k;
                  nBestMatchOffset = pMatch[m].offset;
                  nBestUpdatedSlot = nCurUpdatedSlot;
                  nBestUpdatedIndex = nCurUpdatedIndex;
                  nBestExpectedRepMatch = nCurExpectedRepMatch;
               }
            }
         }

         pSlotCost[m] = nBestCost;
         pMatch[m].length = nBestMatchLen;
         pMatch[m].offset = nBestMatchOffset; /* not necessary */

         if (m == 0 || (nBestMatchLen && cost[i] >= nBestCost)) {
            cost[i] = nBestCost;
            pCompressor->best_match[i].length = nBestMatchLen;
            pCompressor->best_match[i].offset = nBestMatchOffset;

            repmatch_opt[i].expected_repmatch = nBestExpectedRepMatch;

            if (nBestUpdatedSlot >= 0 && nBestUpdatedIndex >= 0) {
               repmatch_opt[nBestUpdatedIndex].best_slot_for_incoming = nBestUpdatedSlot;
               repmatch_opt[nBestUpdatedIndex].incoming_offset = i;
            }
         }
      }
      for (; m < NMATCHES_PER_OFFSET; m++) {
         pSlotCost[m] = 0;
      }

      if (pCompressor->best_match[i].length >= MIN_MATCH_SIZE_V2)
         nLastLiteralsOffset = i;

      prev_match[i] = nLastLiteralsOffset;
   }

   int nIncomingOffset = -1;
   for (i = nStartOffset; i < nEndOffset; ) {
      if (pCompressor->best_match[i].length >= MIN_MATCH_SIZE_V2) {
         if (nIncomingOffset >= 0 && repmatch_opt[i].incoming_offset == nIncomingOffset && repmatch_opt[i].best_slot_for_incoming >= 0) {
            lzsa_match *pMatch = pCompressor->match + (i << MATCHES_PER_OFFSET_SHIFT) + repmatch_opt[i].best_slot_for_incoming;
            int *pSlotCost = pCompressor->slot_cost + (i << MATCHES_PER_OFFSET_SHIFT) + repmatch_opt[i].best_slot_for_incoming;

            pCompressor->best_match[i].length = pMatch->length;
            pCompressor->best_match[i].offset = pMatch->offset;
            cost[i] = *pSlotCost;

            if (repmatch_opt[i].expected_repmatch == 2)
               repmatch_opt[i].expected_repmatch = 1;
         }
         else {
            if (repmatch_opt[i].expected_repmatch == 2)
               repmatch_opt[i].expected_repmatch = 0;
         }

         nIncomingOffset = i;
         i += pCompressor->best_match[i].length;
      }
      else {
         i++;
      }
   }
}

/**
 * Attempt to minimize the number of commands issued in the compressed data block, in order to speed up decompression without
 * impacting the compression ratio
 *
 * @param pCompressor compression context
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 *
 * @return non-zero if the number of tokens was reduced, 0 if it wasn't
 */
static int lzsa_optimize_command_count_v2(lzsa_compressor *pCompressor, const int nStartOffset, const int nEndOffset) {
   int i;
   int nNumLiterals = 0;
   int nDidReduce = 0;
   int nPreviousMatchOffset = -1;
   lzsa_repmatch_opt *repmatch_opt = pCompressor->repmatch_opt;

   for (i = nStartOffset; i < nEndOffset; ) {
      lzsa_match *pMatch = pCompressor->best_match + i;

      if (pMatch->length >= MIN_MATCH_SIZE_V2) {
         int nMatchLen = pMatch->length;
         int nReduce = 0;
         int nCurrentMatchOffset = i;

         if (nMatchLen <= 9 && (i + nMatchLen) < nEndOffset) /* max reducable command size: <token> <EE> <ll> <ll> <offset> <offset> <EE> <mm> <mm> */ {
            int nMatchOffset = pMatch->offset;
            int nEncodedMatchLen = nMatchLen - MIN_MATCH_SIZE_V2;
            int nUndoRepMatchCost = (nPreviousMatchOffset < 0 || !repmatch_opt[nPreviousMatchOffset].expected_repmatch) ? 0 : 16;

            if (pCompressor->best_match[i + nMatchLen].length >= MIN_MATCH_SIZE_V2) {
               int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNumLiterals) + lzsa_get_match_varlen_size_v2(nEncodedMatchLen) - nUndoRepMatchCost;

               if (pCompressor->best_match[i + nMatchLen].offset != nMatchOffset) {
                  nCommandSize += (nMatchOffset <= 32) ? 4 : ((nMatchOffset <= 512) ? 8 : ((nMatchOffset <= (8192 + 512)) ? 12 : 16)) /* match offset */;
               }

               if (nCommandSize >= ((nMatchLen << 3) + lzsa_get_literals_varlen_size_v2(nNumLiterals + nMatchLen))) {
                  /* This command is a match; the next command is also a match. The next command currently has no literals; replacing this command by literals will
                   * make the next command eat the cost of encoding the current number of literals, + nMatchLen extra literals. The size of the current match command is
                   * at least as much as the number of literal bytes + the extra cost of encoding them in the next match command, so we can safely replace the current
                   * match command by literals, the output size will not increase and it will remove one command. */
                  nReduce = 1;
               }
            }
            else {
               int nCurIndex = i + nMatchLen;
               int nNextNumLiterals = 0;
               int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v2(nNumLiterals) + lzsa_get_match_varlen_size_v2(nEncodedMatchLen) - nUndoRepMatchCost;;

               do {
                  nCurIndex++;
                  nNextNumLiterals++;
               } while (nCurIndex < nEndOffset && pCompressor->best_match[nCurIndex].length < MIN_MATCH_SIZE_V2);

               if (nCurIndex >= nEndOffset || pCompressor->best_match[nCurIndex].length < MIN_MATCH_SIZE_V2 ||
                  pCompressor->best_match[nCurIndex].offset != nMatchOffset) {
                  nCommandSize += (nMatchOffset <= 32) ? 4 : ((nMatchOffset <= 512) ? 8 : ((nMatchOffset <= (8192 + 512)) ? 12 : 16)) /* match offset */;
               }

               if (nCommandSize >= ((nMatchLen << 3) + lzsa_get_literals_varlen_size_v2(nNumLiterals + nNextNumLiterals + nMatchLen) - lzsa_get_literals_varlen_size_v2(nNextNumLiterals))) {
                  /* This command is a match, and is followed by literals, and then another match or the end of the input data. If encoding this match as literals doesn't take
                   * more room than the match, and doesn't grow the next match command's literals encoding, go ahead and remove the command. */
                  nReduce = 1;
               }
            }
         }

         if (nReduce) {
            int j;

            for (j = 0; j < nMatchLen; j++) {
               pCompressor->best_match[i + j].length = 0;
            }
            nNumLiterals += nMatchLen;
            i += nMatchLen;

            nDidReduce = 1;

            if (nPreviousMatchOffset >= 0) {
               repmatch_opt[nPreviousMatchOffset].expected_repmatch = 0;
               nPreviousMatchOffset = -1;
            }
         }
         else {
            if ((i + nMatchLen) < nEndOffset && nMatchLen >= LCP_MAX &&
               pMatch->offset && pMatch->offset <= 32 && pCompressor->best_match[i + nMatchLen].offset == pMatch->offset && (nMatchLen % pMatch->offset) == 0 &&
               (nMatchLen + pCompressor->best_match[i + nMatchLen].length) <= MAX_VARLEN) {
               /* Join */

               pMatch->length += pCompressor->best_match[i + nMatchLen].length;
               pCompressor->best_match[i + nMatchLen].offset = 0;
               pCompressor->best_match[i + nMatchLen].length = -1;
               continue;
            }

            nNumLiterals = 0;
            i += nMatchLen;
         }

         nPreviousMatchOffset = nCurrentMatchOffset;
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
   int i;
   int nNumLiterals = 0;
   int nInFirstLiteralOffset = 0;
   int nOutOffset = 0;
   int nCurNibbleOffset = -1, nCurFreeNibbles = 0;
   int nRepMatchOffset = 0;

   for (i = nStartOffset; i < nEndOffset; ) {
      lzsa_match *pMatch = pCompressor->best_match + i;

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
         nRepMatchOffset = nMatchOffset;

         nOutOffset = lzsa_write_match_varlen_v2(pOutData, nOutOffset, nMaxOutDataSize, &nCurNibbleOffset, &nCurFreeNibbles, nEncodedMatchLen);
         if (nOutOffset < 0) return -1;

         i += nMatchLen;

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

      if (nNumLiterals != 0) {
         memcpy(pOutData + nOutOffset, pInWindow + nInFirstLiteralOffset, nNumLiterals);
         nOutOffset += nNumLiterals;
         nNumLiterals = 0;
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
 * Select the most optimal matches, reduce the token count if possible, and then emit a block of compressed LZSA2 data
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
int lzsa_optimize_and_write_block_v2(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int nPreviousBlockSize, const int nInDataSize, unsigned char *pOutData, const int nMaxOutDataSize) {
   lzsa_optimize_matches_v2(pCompressor, nPreviousBlockSize, nPreviousBlockSize + nInDataSize);

   int nDidReduce;
   int nPasses = 0;
   do {
      nDidReduce = lzsa_optimize_command_count_v2(pCompressor, nPreviousBlockSize, nPreviousBlockSize + nInDataSize);
      nPasses++;
   } while (nDidReduce && nPasses < 20);

   return lzsa_write_block_v2(pCompressor, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, pOutData, nMaxOutDataSize);
}
