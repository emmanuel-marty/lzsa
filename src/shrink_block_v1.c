/*
 * shrink_v1.c - LZSA1 block compressor implementation
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
static inline int lzsa_write_literals_varlen_v1(unsigned char *pOutData, int nOutOffset, int nLength) {
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
static inline int lzsa_write_match_varlen_v1(unsigned char *pOutData, int nOutOffset, int nLength) {
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
 * Attempt to pick optimal matches, so as to produce the smallest possible output that decompresses to the same input
 *
 * @param pCompressor compression context
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 */
static void lzsa_optimize_matches_v1(lzsa_compressor *pCompressor, const int nStartOffset, const int nEndOffset) {
   int *cost = (int*)pCompressor->pos_data;  /* Reuse */
   int nLastLiteralsOffset;
   int nMinMatchSize = pCompressor->min_match_size;
   const int nFavorRatio = (pCompressor->flags & LZSA_FLAG_FAVOR_RATIO) ? 1 : 0;
   int i;

   cost[nEndOffset - 1] = 8;
   nLastLiteralsOffset = nEndOffset;

   for (i = nEndOffset - 2; i != (nStartOffset - 1); i--) {
      int nBestCost, nBestMatchLen, nBestMatchOffset;

      int nLiteralsLen = nLastLiteralsOffset - i;
      nBestCost = 8 + cost[i + 1];
      if (nLiteralsLen == LITERALS_RUN_LEN_V1 || nLiteralsLen == 256 || nLiteralsLen == 512) {
         /* Add to the cost of encoding literals as their number crosses a variable length encoding boundary.
          * The cost automatically accumulates down the chain. */
         nBestCost += 8;
      }
      if (pCompressor->match[(i + 1) << MATCHES_PER_OFFSET_SHIFT].length >= MIN_MATCH_SIZE_V1)
         nBestCost += MODESWITCH_PENALTY;
      nBestMatchLen = 0;
      nBestMatchOffset = 0;

      lzsa_match *pMatch = pCompressor->match + (i << MATCHES_PER_OFFSET_SHIFT);
      int m;

      for (m = 0; m < NMATCHES_PER_OFFSET && pMatch[m].length >= nMinMatchSize; m++) {
         int nMatchOffsetSize = (pMatch[m].offset <= 256) ? 8 : 16;

         if (pMatch[m].length >= LEAVE_ALONE_MATCH_SIZE) {
            int nCurCost;
            int nMatchLen = pMatch[m].length;

            if ((i + nMatchLen) > (nEndOffset - LAST_LITERALS))
               nMatchLen = nEndOffset - LAST_LITERALS - i;

            nCurCost = 8 + nMatchOffsetSize + lzsa_get_match_varlen_size_v1(nMatchLen - MIN_MATCH_SIZE_V1);
            nCurCost += cost[i + nMatchLen];
            if (pCompressor->match[(i + nMatchLen) << MATCHES_PER_OFFSET_SHIFT].length >= MIN_MATCH_SIZE_V1)
               nCurCost += MODESWITCH_PENALTY;

            if (nBestCost > (nCurCost - nFavorRatio)) {
               nBestCost = nCurCost;
               nBestMatchLen = nMatchLen;
               nBestMatchOffset = pMatch[m].offset;
            }
         }
         else {
            int nMatchLen = pMatch[m].length;
            int k, nMatchRunLen;

            if ((i + nMatchLen) > (nEndOffset - LAST_LITERALS))
               nMatchLen = nEndOffset - LAST_LITERALS - i;

            nMatchRunLen = nMatchLen;
            if (nMatchRunLen > MATCH_RUN_LEN_V1)
               nMatchRunLen = MATCH_RUN_LEN_V1;

            for (k = nMinMatchSize; k < nMatchRunLen; k++) {
               int nCurCost;

               nCurCost = 8 + nMatchOffsetSize /* no extra match len bytes */;
               nCurCost += cost[i + k];
               if (pCompressor->match[(i + k) << MATCHES_PER_OFFSET_SHIFT].length >= MIN_MATCH_SIZE_V1)
                  nCurCost += MODESWITCH_PENALTY;

               if (nBestCost > (nCurCost - nFavorRatio)) {
                  nBestCost = nCurCost;
                  nBestMatchLen = k;
                  nBestMatchOffset = pMatch[m].offset;
               }
            }

            for (; k <= nMatchLen; k++) {
               int nCurCost;

               nCurCost = 8 + nMatchOffsetSize + lzsa_get_match_varlen_size_v1(k - MIN_MATCH_SIZE_V1);
               nCurCost += cost[i + k];
               if (pCompressor->match[(i + k) << MATCHES_PER_OFFSET_SHIFT].length >= MIN_MATCH_SIZE_V1)
                  nCurCost += MODESWITCH_PENALTY;

               if (nBestCost > (nCurCost - nFavorRatio)) {
                  nBestCost = nCurCost;
                  nBestMatchLen = k;
                  nBestMatchOffset = pMatch[m].offset;
               }
            }
         }
      }

      if (nBestMatchLen >= MIN_MATCH_SIZE_V1)
         nLastLiteralsOffset = i;

      cost[i] = nBestCost;
      pMatch->length = nBestMatchLen;
      pMatch->offset = nBestMatchOffset;
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
static int lzsa_optimize_command_count_v1(lzsa_compressor *pCompressor, const int nStartOffset, const int nEndOffset) {
   int i;
   int nNumLiterals = 0;
   int nDidReduce = 0;

   for (i = nStartOffset; i < nEndOffset; ) {
      lzsa_match *pMatch = pCompressor->match + (i << MATCHES_PER_OFFSET_SHIFT);

      if (pMatch->length >= MIN_MATCH_SIZE_V1) {
         int nMatchLen = pMatch->length;
         int nReduce = 0;

         if (nMatchLen <= 9 && (i + nMatchLen) < nEndOffset) /* max reducable command size: <token> <EE> <ll> <ll> <offset> <offset> <EE> <mm> <mm> */ {
            int nMatchOffset = pMatch->offset;
            int nEncodedMatchLen = nMatchLen - MIN_MATCH_SIZE_V1;
            int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v1(nNumLiterals) + ((nMatchOffset <= 256) ? 8 : 16) /* match offset */ + lzsa_get_match_varlen_size_v1(nEncodedMatchLen);

            if (pCompressor->match[(i + nMatchLen) << MATCHES_PER_OFFSET_SHIFT].length >= MIN_MATCH_SIZE_V1) {
               if (nCommandSize >= ((nMatchLen << 3) + lzsa_get_literals_varlen_size_v1(nNumLiterals + nMatchLen))) {
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

               do {
                  nCurIndex++;
                  nNextNumLiterals++;
               } while (nCurIndex < nEndOffset && pCompressor->match[nCurIndex << MATCHES_PER_OFFSET_SHIFT].length < MIN_MATCH_SIZE_V1);

               if (nCommandSize >= ((nMatchLen << 3) + lzsa_get_literals_varlen_size_v1(nNumLiterals + nNextNumLiterals + nMatchLen) - lzsa_get_literals_varlen_size_v1(nNextNumLiterals))) {
                  /* This command is a match, and is followed by literals, and then another match or the end of the input data. If encoding this match as literals doesn't take
                   * more room than the match, and doesn't grow the next match command's literals encoding, go ahead and remove the command. */
                  nReduce = 1;
               }
            }
         }

         if (nReduce) {
            int j;

            for (j = 0; j < nMatchLen; j++) {
               pCompressor->match[(i + j) << MATCHES_PER_OFFSET_SHIFT].length = 0;
            }
            nNumLiterals += nMatchLen;
            i += nMatchLen;

            nDidReduce = 1;
         }
         else {
            if ((i + nMatchLen) < nEndOffset && nMatchLen >= LCP_MAX &&
               pMatch->offset && pMatch->offset <= 32 && pCompressor->match[(i + nMatchLen) << MATCHES_PER_OFFSET_SHIFT].offset == pMatch->offset && (nMatchLen % pMatch->offset) == 0 &&
               (nMatchLen + pCompressor->match[(i + nMatchLen) << MATCHES_PER_OFFSET_SHIFT].length) <= MAX_VARLEN) {
               /* Join */

               pMatch->length += pCompressor->match[(i + nMatchLen) << MATCHES_PER_OFFSET_SHIFT].length;
               pCompressor->match[(i + nMatchLen) << MATCHES_PER_OFFSET_SHIFT].offset = 0;
               pCompressor->match[(i + nMatchLen) << MATCHES_PER_OFFSET_SHIFT].length = -1;
               continue;
            }

            nNumLiterals = 0;
            i += nMatchLen;
         }
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
   int i;
   int nNumLiterals = 0;
   int nInFirstLiteralOffset = 0;
   int nOutOffset = 0;

   for (i = nStartOffset; i < nEndOffset; ) {
      lzsa_match *pMatch = pCompressor->match + (i << MATCHES_PER_OFFSET_SHIFT);

      if (pMatch->length >= MIN_MATCH_SIZE_V1) {
         int nMatchOffset = pMatch->offset;
         int nMatchLen = pMatch->length;
         int nEncodedMatchLen = nMatchLen - MIN_MATCH_SIZE_V1;
         int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN_V1) ? LITERALS_RUN_LEN_V1 : nNumLiterals;
         int nTokenMatchLen = (nEncodedMatchLen >= MATCH_RUN_LEN_V1) ? MATCH_RUN_LEN_V1 : nEncodedMatchLen;
         int nTokenLongOffset = (nMatchOffset <= 256) ? 0x00 : 0x80;
         int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v1(nNumLiterals) + (nNumLiterals << 3) + (nTokenLongOffset ? 16 : 8) /* match offset */ + lzsa_get_match_varlen_size_v1(nEncodedMatchLen);

         if ((nOutOffset + (nCommandSize >> 3)) > nMaxOutDataSize)
            return -1;
         if (nMatchOffset < MIN_OFFSET || nMatchOffset > MAX_OFFSET)
            return -1;

         pOutData[nOutOffset++] = nTokenLongOffset | (nTokenLiteralsLen << 4) | nTokenMatchLen;
         nOutOffset = lzsa_write_literals_varlen_v1(pOutData, nOutOffset, nNumLiterals);

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
      int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN_V1) ? LITERALS_RUN_LEN_V1 : nNumLiterals;
      int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v1(nNumLiterals) + (nNumLiterals << 3);

      if ((nOutOffset + (nCommandSize >> 3)) > nMaxOutDataSize)
         return -1;

      if (pCompressor->flags & LZSA_FLAG_RAW_BLOCK)
         pOutData[nOutOffset++] = (nTokenLiteralsLen << 4) | 0x0f;
      else
         pOutData[nOutOffset++] = (nTokenLiteralsLen << 4) | 0x00;
      nOutOffset = lzsa_write_literals_varlen_v1(pOutData, nOutOffset, nNumLiterals);

      if (nNumLiterals != 0) {
         memcpy(pOutData + nOutOffset, pInWindow + nInFirstLiteralOffset, nNumLiterals);
         nOutOffset += nNumLiterals;
         nNumLiterals = 0;
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
   int nNumLiterals = nEndOffset - nStartOffset;
   int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN_V1) ? LITERALS_RUN_LEN_V1 : nNumLiterals;
   int nOutOffset = 0;

   int nCommandSize = 8 /* token */ + lzsa_get_literals_varlen_size_v1(nNumLiterals) + (nNumLiterals << 3) + 4;
   if ((nOutOffset + (nCommandSize >> 3)) > nMaxOutDataSize)
      return -1;

   pCompressor->num_commands = 0;
   pOutData[nOutOffset++] = (nTokenLiteralsLen << 4) | 0x0f;
   
   nOutOffset = lzsa_write_literals_varlen_v1(pOutData, nOutOffset, nNumLiterals);

   if (nNumLiterals != 0) {
      memcpy(pOutData + nOutOffset, pInWindow + nStartOffset, nNumLiterals);
      nOutOffset += nNumLiterals;
      nNumLiterals = 0;
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

   lzsa_optimize_matches_v1(pCompressor, nPreviousBlockSize, nPreviousBlockSize + nInDataSize);

   int nDidReduce;
   int nPasses = 0;
   do {
      nDidReduce = lzsa_optimize_command_count_v1(pCompressor, nPreviousBlockSize, nPreviousBlockSize + nInDataSize);
      nPasses++;
   } while (nDidReduce && nPasses < 20);

   nResult = lzsa_write_block_v1(pCompressor, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, pOutData, nMaxOutDataSize);
   if (nResult < 0 && pCompressor->flags & LZSA_FLAG_RAW_BLOCK) {
      nResult = lzsa_write_raw_uncompressed_block_v1(pCompressor, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, pOutData, nMaxOutDataSize);
   }

   return nResult;
}
