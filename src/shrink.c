/*
 * shrink.c - block compressor implementation
 *
 * The following copying information applies to this specific source code file:
 *
 * Written in 2019 by Emmanuel Marty <marty.emmanuel@gmail.com>
 * With help, ideas, optimizations and speed measurements by spke <zxintrospec@gmail.com>
 * Portions written in 2014-2015 by Eric Biggers <ebiggers3@gmail.com>
 *
 * To the extent possible under law, the author(s) have dedicated all copyright
 * and related and neighboring rights to this software to the public domain
 * worldwide via the Creative Commons Zero 1.0 Universal Public Domain
 * Dedication (the "CC0").
 *
 * This software is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the CC0 for more details.
 *
 * You should have received a copy of the CC0 along with this software; if not
 * see <http://creativecommons.org/publicdomain/zero/1.0/>.
 */

/*
 * Uses the libdivsufsort library Copyright (c) 2003-2008 Yuta Mori
 *
 * Inspired by LZ4 by Yann Collet. https://github.com/lz4/lz4
 * With ideas from Lizard by Przemyslaw Skibinski and Yann Collet. https://github.com/inikep/lizard
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "shrink.h"
#include "format.h"

#define LCP_BITS 14
#define LCP_MAX ((1<<LCP_BITS) - 1)
#define LCP_SHIFT (32-LCP_BITS)
#define LCP_MASK (LCP_MAX << LCP_SHIFT)
#define POS_MASK ((1<<LCP_SHIFT) - 1)

#define NMATCHES_PER_OFFSET 8
#define MATCHES_PER_OFFSET_SHIFT 3

#define LEAVE_ALONE_MATCH_SIZE 1000

#define LAST_MATCH_OFFSET 4
#define LAST_LITERALS 1

/** One match */
typedef struct _lzsa_match {
   unsigned short length;
   unsigned short offset;
} lzsa_match;

/**
 * Initialize compression context
 *
 * @param pCompressor compression context to initialize
 * @param nMaxWindowSize maximum size of input data window (previously compressed bytes + bytes to compress)
 *
 * @return 0 for success, non-zero for failure
 */
int lzsa_compressor_init(lsza_compressor *pCompressor, const int nMaxWindowSize) {
   int nResult;

   nResult = divsufsort_init(&pCompressor->divsufsort_context);
   pCompressor->intervals = NULL;
   pCompressor->pos_data = NULL;
   pCompressor->open_intervals = NULL;
   pCompressor->match = NULL;
   pCompressor->num_commands = 0;

   if (!nResult) {
      pCompressor->intervals = (unsigned int *)malloc(nMaxWindowSize * sizeof(unsigned int));

      if (pCompressor->intervals) {
         pCompressor->pos_data = (unsigned int *)malloc(nMaxWindowSize * sizeof(unsigned int));

         if (pCompressor->pos_data) {
            pCompressor->open_intervals = (unsigned int *)malloc((LCP_MAX + 1) * sizeof(unsigned int));

            if (pCompressor->open_intervals) {
               pCompressor->match = (lzsa_match *)malloc(nMaxWindowSize * NMATCHES_PER_OFFSET * sizeof(lzsa_match));

               if (pCompressor->match)
                  return 0;
            }
         }
      }
   }

   lzsa_compressor_destroy(pCompressor);
   return 100;
}

/**
 * Clean up compression context and free up any associated resources
 *
 * @param pCompressor compression context to clean up
 */
void lzsa_compressor_destroy(lsza_compressor *pCompressor) {
   divsufsort_destroy(&pCompressor->divsufsort_context);

   if (pCompressor->match) {
      free(pCompressor->match);
      pCompressor->match = NULL;
   }

   if (pCompressor->open_intervals) {
      free(pCompressor->open_intervals);
      pCompressor->open_intervals = NULL;
   }

   if (pCompressor->pos_data) {
      free(pCompressor->pos_data);
      pCompressor->pos_data = NULL;
   }

   if (pCompressor->intervals) {
      free(pCompressor->intervals);
      pCompressor->intervals = NULL;
   }
}

/**
 * Parse input data, build suffix array and overlaid data structures to speed up match finding
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nInWindowSize total input size in bytes (previously compressed bytes + bytes to compress)
 *
 * @return 0 for success, non-zero for failure
 */
static int lzsa_build_suffix_array(lsza_compressor *pCompressor, const unsigned char *pInWindow, const int nInWindowSize) {
   unsigned int *intervals = pCompressor->intervals;

   /* Build suffix array from input data */
   if (divsufsort_build_array(&pCompressor->divsufsort_context, pInWindow, (saidx_t*)intervals, nInWindowSize) != 0) {
      return 100;
   }

   int *PLCP = (int*)pCompressor->pos_data;  /* Use temporarily */
   int *Phi = PLCP;
   int nCurLen = 0;
   int i;

   /* Compute the permuted LCP first (Kärkkäinen method) */
   Phi[intervals[0]] = -1;
   for (i = 1; i < nInWindowSize; i++)
      Phi[intervals[i]] = intervals[i - 1];
   for (i = 0; i < nInWindowSize; i++) {
      if (Phi[i] == -1) {
         PLCP[i] = 0;
         continue;
      }
      int nMaxLen = (i > Phi[i]) ? (nInWindowSize - i) : (nInWindowSize - Phi[i]);
      while (nCurLen < nMaxLen && pInWindow[i + nCurLen] == pInWindow[Phi[i] + nCurLen]) nCurLen++;
      PLCP[i] = nCurLen;
      if (nCurLen > 0)
         nCurLen--;
   }

   /* Rotate permuted LCP into the LCP. This has better cache locality than the direct Kasai LCP method. This also
    * saves us from having to build the inverse suffix array index, as the LCP is calculated without it using this method,
    * and the interval builder below doesn't need it either. */
   intervals[0] &= POS_MASK;
   for (i = 1; i < nInWindowSize - 1; i++) {
      int nIndex = (int)(intervals[i] & POS_MASK);
      int nLen = PLCP[nIndex];
      if (nLen < MIN_MATCH_SIZE)
         nLen = 0;
      if (nLen > LCP_MAX)
         nLen = LCP_MAX;
      intervals[i] = ((unsigned int)nIndex) | (((unsigned int)nLen) << LCP_SHIFT);
   }
   if (i < nInWindowSize)
      intervals[i] &= POS_MASK;

   /**
    * Build intervals for finding matches
    *
    * Methodology and code fragment taken from wimlib (CC0 license):
    * https://wimlib.net/git/?p=wimlib;a=blob_plain;f=src/lcpit_matchfinder.c;h=a2d6a1e0cd95200d1f3a5464d8359d5736b14cbe;hb=HEAD
    */
   unsigned int * const SA_and_LCP = intervals;
   unsigned int *pos_data = pCompressor->pos_data;
   unsigned int next_interval_idx;
   unsigned int *top = pCompressor->open_intervals;
   unsigned int prev_pos = SA_and_LCP[0] & POS_MASK;

   *top = 0;
   intervals[0] = 0;
   next_interval_idx = 1;

   for (int r = 1; r < nInWindowSize; r++) {
      const unsigned int next_pos = SA_and_LCP[r] & POS_MASK;
      const unsigned int next_lcp = SA_and_LCP[r] & LCP_MASK;
      const unsigned int top_lcp = *top & LCP_MASK;

      if (next_lcp == top_lcp) {
         /* Continuing the deepest open interval  */
         pos_data[prev_pos] = *top;
      }
      else if (next_lcp > top_lcp) {
         /* Opening a new interval  */
         *++top = next_lcp | next_interval_idx++;
         pos_data[prev_pos] = *top;
      }
      else {
         /* Closing the deepest open interval  */
         pos_data[prev_pos] = *top;
         for (;;) {
            const unsigned int closed_interval_idx = *top-- & POS_MASK;
            const unsigned int superinterval_lcp = *top & LCP_MASK;

            if (next_lcp == superinterval_lcp) {
               /* Continuing the superinterval */
               intervals[closed_interval_idx] = *top;
               break;
            }
            else if (next_lcp > superinterval_lcp) {
               /* Creating a new interval that is a
                * superinterval of the one being
                * closed, but still a subinterval of
                * its superinterval  */
               *++top = next_lcp | next_interval_idx++;
               intervals[closed_interval_idx] = *top;
               break;
            }
            else {
               /* Also closing the superinterval  */
               intervals[closed_interval_idx] = *top;
            }
         }
      }
      prev_pos = next_pos;
   }

   /* Close any still-open intervals.  */
   pos_data[prev_pos] = *top;
   for (; top > pCompressor->open_intervals; top--)
      intervals[*top & POS_MASK] = *(top - 1);

   /* Success */
   return 0;
}

/**
 * Find matches at the specified offset in the input window
 *
 * @param pCompressor compression context
 * @param nOffset offset to find matches at, in the input window
 * @param pMatches pointer to returned matches
 * @param nMaxMatches maximum number of matches to return (0 for none)
 *
 * @return number of matches
 */
static int lzsa_find_matches_at(lsza_compressor *pCompressor, const int nOffset, lzsa_match *pMatches, const int nMaxMatches) {
   unsigned int *intervals = pCompressor->intervals;
   unsigned int *pos_data = pCompressor->pos_data;
   unsigned int ref;
   unsigned int super_ref;
   unsigned int match_pos;
   lzsa_match *matchptr;

   /**
    * Find matches using intervals
    *
    * Taken from wimlib (CC0 license):
    * https://wimlib.net/git/?p=wimlib;a=blob_plain;f=src/lcpit_matchfinder.c;h=a2d6a1e0cd95200d1f3a5464d8359d5736b14cbe;hb=HEAD
    */

    /* Get the deepest lcp-interval containing the current suffix. */
   ref = pos_data[nOffset];

   pos_data[nOffset] = 0;

   /* Ascend until we reach a visited interval, the root, or a child of the
    * root.  Link unvisited intervals to the current suffix as we go.  */
   while ((super_ref = intervals[ref & POS_MASK]) & LCP_MASK) {
      intervals[ref & POS_MASK] = nOffset;
      ref = super_ref;
   }

   if (super_ref == 0) {
      /* In this case, the current interval may be any of:
       * (1) the root;
       * (2) an unvisited child of the root;
       * (3) an interval last visited by suffix 0
       *
       * We could avoid the ambiguity with (3) by using an lcp
       * placeholder value other than 0 to represent "visited", but
       * it's fastest to use 0.  So we just don't allow matches with
       * position 0.  */

      if (ref != 0)  /* Not the root?  */
         intervals[ref & POS_MASK] = nOffset;
      return 0;
   }

   /* Ascend indirectly via pos_data[] links.  */
   match_pos = super_ref;
   matchptr = pMatches;
   for (;;) {
      while ((super_ref = pos_data[match_pos]) > ref)
         match_pos = intervals[super_ref & POS_MASK];
      intervals[ref & POS_MASK] = nOffset;
      pos_data[match_pos] = ref;

      if ((matchptr - pMatches) < nMaxMatches) {
         int nMatchOffset = (int)(nOffset - match_pos);

         if (nMatchOffset <= MAX_OFFSET) {
            matchptr->length = (unsigned short)(ref >> LCP_SHIFT);
            matchptr->offset = (unsigned short)nMatchOffset;
            matchptr++;
         }
      }

      if (super_ref == 0)
         break;
      ref = super_ref;
      match_pos = intervals[ref & POS_MASK];
   }

   return (int)(matchptr - pMatches);
}

/**
 * Skip previously compressed bytes
 *
 * @param pCompressor compression context
 * @param nStartOffset current offset in input window (typically 0)
 * @param nEndOffset offset to skip to in input window (typically the number of previously compressed bytes)
 */
static void lzsa_skip_matches(lsza_compressor *pCompressor, const int nStartOffset, const int nEndOffset) {
   lzsa_match match;
   int i;

   /* Skipping still requires scanning for matches, as this also performs a lazy update of the intervals. However,
    * we don't store the matches. */
   for (i = nStartOffset; i < nEndOffset; i++) {
      lzsa_find_matches_at(pCompressor, i, &match, 0);
   }
}

/**
 * Find all matches for the data to be compressed. Up to NMATCHES_PER_OFFSET matches are stored for each offset, for
 * the optimizer to look at.
 *
 * @param pCompressor compression context
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 */
static void lzsa_find_all_matches(lsza_compressor *pCompressor, const int nStartOffset, const int nEndOffset) {
   lzsa_match *pMatch = pCompressor->match + (nStartOffset << MATCHES_PER_OFFSET_SHIFT);
   int i;

   for (i = nStartOffset; i < nEndOffset; i++) {
      int nMatches = lzsa_find_matches_at(pCompressor, i, pMatch, NMATCHES_PER_OFFSET);
      int m;

      for (m = 0; m < NMATCHES_PER_OFFSET; m++) {
         if (nMatches <= m || i > (nEndOffset - LAST_MATCH_OFFSET)) {
            pMatch->length = 0;
            pMatch->offset = 0;
         }
         else {
            int nMaxLen = (nEndOffset - LAST_LITERALS) - i;
            if (nMaxLen < 0)
               nMaxLen = 0;
            if (pMatch->length > nMaxLen)
               pMatch->length = (unsigned short)nMaxLen;
         }

         pMatch++;
      }
   }
}

/**
 * Get the number of extra bytes required to represent a literals length
 *
 * @param nLength literals length
 *
 * @return number of extra bytes required
 */
static inline int lzsa_get_literals_varlen_size(const int nLength) {
   if (nLength < LITERALS_RUN_LEN) {
      return 0;
   }
   else {
      if (nLength < (LITERALS_RUN_LEN + 254))
         return 1;
      else {
         if (nLength < (LITERALS_RUN_LEN + 510))
            return 2;
         else
            return 3;
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
static inline int lzsa_write_literals_varlen(unsigned char *pOutData, int nOutOffset, int nLength) {
   if (nLength >= LITERALS_RUN_LEN) {
      if (nLength < (LITERALS_RUN_LEN + 254))
         pOutData[nOutOffset++] = nLength - LITERALS_RUN_LEN;
      else {
         if (nLength < (LITERALS_RUN_LEN + 510)) {
            pOutData[nOutOffset++] = 254;
            pOutData[nOutOffset++] = nLength - LITERALS_RUN_LEN - 254;
         }
         else {
            pOutData[nOutOffset++] = 255;
            pOutData[nOutOffset++] = nLength & 0xff;
            pOutData[nOutOffset++] = (nLength >> 8) & 0xff;
         }
      }
   }

   return nOutOffset;
}

/**
 * Get the number of extra bytes required to represent an encoded match length
 *
 * @param nLength encoded match length (actual match length - MIN_MATCH_SIZE)
 *
 * @return number of extra bytes required
 */
static inline int lzsa_get_match_varlen_size(const int nLength) {
   if (nLength < MATCH_RUN_LEN) {
      return 0;
   }
   else {
      if (nLength < (MATCH_RUN_LEN + 254))
         return 1;
      else {
         if (nLength < (MATCH_RUN_LEN + 510))
            return 2;
         else
            return 3;
      }
   }
}

/**
 * Write extra encoded match length bytes to output (compressed) buffer. The caller must first check that there is enough
 * room to write the bytes.
 *
 * @param pOutData pointer to output buffer
 * @param nOutOffset current write index into output buffer
 * @param nLength encoded match length (actual match length - MIN_MATCH_SIZE)
 */
static inline int lzsa_write_match_varlen(unsigned char *pOutData, int nOutOffset, int nLength) {
   if (nLength >= MATCH_RUN_LEN) {
      if (nLength < (MATCH_RUN_LEN + 254))
         pOutData[nOutOffset++] = nLength - MATCH_RUN_LEN;
      else {
         if (nLength < (MATCH_RUN_LEN + 510)) {
            pOutData[nOutOffset++] = 254;
            pOutData[nOutOffset++] = nLength - MATCH_RUN_LEN - 254;
         }
         else {
            pOutData[nOutOffset++] = 255;
            pOutData[nOutOffset++] = nLength & 0xff;
            pOutData[nOutOffset++] = (nLength >> 8) & 0xff;
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
static void lzsa_optimize_matches(lsza_compressor *pCompressor, const int nStartOffset, const int nEndOffset) {
   int *cost = (int*)pCompressor->pos_data;  /* Reuse */
   int nLastLiteralsOffset;
   int i;

   cost[nEndOffset - 1] = 1;
   nLastLiteralsOffset = nEndOffset - 1;

   for (i = nEndOffset - 2; i != (nStartOffset - 1); i--) {
      int nBestCost, nBestMatchLen, nBestMatchOffset;

      int nLiteralsLen = nLastLiteralsOffset - i;
      nBestCost = 1 + cost[i + 1] + lzsa_get_literals_varlen_size(nLiteralsLen);
      nBestMatchLen = 0;
      nBestMatchOffset = 0;

      lzsa_match *pMatch = pCompressor->match + (i << MATCHES_PER_OFFSET_SHIFT);
      int m;

      for (m = 0; m < NMATCHES_PER_OFFSET; m++) {
         int nMatchOffsetSize = (pMatch[m].offset <= 256) ? 1 : 2;

         if (pMatch[m].length >= LEAVE_ALONE_MATCH_SIZE) {
            int nCurCost;
            int nMatchLen = pMatch[m].length;
            int nRemainingLiteralsLen = nLastLiteralsOffset - (i + nMatchLen);

            if (nRemainingLiteralsLen < 0) nRemainingLiteralsLen = 0;

            nCurCost = 1 + lzsa_get_literals_varlen_size(nRemainingLiteralsLen) + nMatchOffsetSize + lzsa_get_match_varlen_size(nMatchLen - MIN_MATCH_SIZE);
            if ((i + nMatchLen) < nEndOffset)
               nCurCost += cost[i + nMatchLen];

            if (nBestCost >= nCurCost) {
               nBestCost = nCurCost;
               nBestMatchLen = nMatchLen;
               nBestMatchOffset = pMatch[m].offset;
            }
         }
         else {
            if (pMatch[m].length >= MIN_MATCH_SIZE) {
               int k, nMatchRunLen;

               nMatchRunLen = pMatch[m].length;
               if (nMatchRunLen > MATCH_RUN_LEN)
                  nMatchRunLen = MATCH_RUN_LEN;

               for (k = MIN_MATCH_SIZE; k < nMatchRunLen; k++) {
                  int nCurCost;
                  int nRemainingLiteralsLen = nLastLiteralsOffset - (i + k);

                  if (nRemainingLiteralsLen < 0) nRemainingLiteralsLen = 0;

                  nCurCost = 1 + lzsa_get_literals_varlen_size(nRemainingLiteralsLen) + nMatchOffsetSize /* no extra match len bytes */;
                  if ((i + k) < nEndOffset)
                     nCurCost += cost[i + k];

                  if (nBestCost >= nCurCost) {
                     nBestCost = nCurCost;
                     nBestMatchLen = k;
                     nBestMatchOffset = pMatch[m].offset;
                  }
               }

               for (; k <= pMatch[m].length; k++) {
                  int nCurCost;
                  int nRemainingLiteralsLen = nLastLiteralsOffset - (i + k);

                  if (nRemainingLiteralsLen < 0) nRemainingLiteralsLen = 0;

                  nCurCost = 1 + lzsa_get_literals_varlen_size(nRemainingLiteralsLen) + nMatchOffsetSize + lzsa_get_match_varlen_size(k - MIN_MATCH_SIZE);
                  if ((i + k) < nEndOffset)
                     nCurCost += cost[i + k];

                  if (nBestCost >= nCurCost) {
                     nBestCost = nCurCost;
                     nBestMatchLen = k;
                     nBestMatchOffset = pMatch[m].offset;
                  }
               }
            }
         }
      }

      if (nBestMatchLen >= MIN_MATCH_SIZE)
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
 */
static void lzsa_optimize_command_count(lsza_compressor *pCompressor, const int nStartOffset, const int nEndOffset) {
   int i;
   int nNumLiterals = 0;

   for (i = nStartOffset; i < nEndOffset; ) {
      lzsa_match *pMatch = pCompressor->match + (i << MATCHES_PER_OFFSET_SHIFT);

      if (pMatch->length >= MIN_MATCH_SIZE) {
         int nMatchLen = pMatch->length;
         int nReduce = 0;

         if (nMatchLen <= 9) /* max reducable command size: <token> <FF> <ll> <ll> <offset> <FF> <mm> <mm> */ {
            int nMatchOffset = pMatch->offset;
            int nEncodedMatchLen = nMatchLen - MIN_MATCH_SIZE;
            int nTokenLongOffset = (nMatchOffset <= 256) ? 0x00 : 0x80;
            int nCommandSize = 1 /* token */ + lzsa_get_literals_varlen_size(nNumLiterals) + (nTokenLongOffset ? 2 : 1) /* match offset */ + lzsa_get_match_varlen_size(nEncodedMatchLen);

            if ((i + nMatchLen) < nEndOffset && pCompressor->match[(i + nMatchLen) << MATCHES_PER_OFFSET_SHIFT].length >= MIN_MATCH_SIZE &&
               nCommandSize >= (nMatchLen + lzsa_get_literals_varlen_size(nNumLiterals + nMatchLen))) {
               /* This command is a match; the next command is also a match. The next command currently has no literals; replacing this command by literals will
                * make the next command eat the cost of encoding the current number of literals, + nMatchLen extra literals. The size of the current match command is
                * at least as much as the number of literal bytes + the extra cost of encoding them in the next match command, so we can safely replace the current
                * match command by literals, the output size will not increase and it will remove one command. */
               nReduce = 1;
            }
            else if ((i + nMatchLen) < nEndOffset && pCompressor->match[(i + nMatchLen) << MATCHES_PER_OFFSET_SHIFT].length < MIN_MATCH_SIZE) {
               int nCurIndex = i + nMatchLen;
               int nNextNumLiterals = 0;

               do {
                  nCurIndex++;
                  nNextNumLiterals++;
               } while (nCurIndex < nEndOffset && pCompressor->match[nCurIndex << MATCHES_PER_OFFSET_SHIFT].length < MIN_MATCH_SIZE);

               if (nCommandSize >= (nMatchLen + lzsa_get_literals_varlen_size(nNumLiterals + nNextNumLiterals + nMatchLen) - lzsa_get_literals_varlen_size(nNextNumLiterals))) {
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
         }
         else {
            nNumLiterals = 0;
            i += nMatchLen;
         }
      }
      else {
         nNumLiterals++;
         i++;
      }
   }
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
static int lzsa_write_block(lsza_compressor *pCompressor, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset, unsigned char *pOutData, const int nMaxOutDataSize) {
   int i;
   int nNumLiterals = 0;
   int nInFirstLiteralOffset = 0;
   int nOutOffset = 0;

   for (i = nStartOffset; i < nEndOffset; ) {
      lzsa_match *pMatch = pCompressor->match + (i << MATCHES_PER_OFFSET_SHIFT);

      if (pMatch->length >= MIN_MATCH_SIZE) {
         int nMatchOffset = pMatch->offset;
         int nMatchLen = pMatch->length;
         int nEncodedMatchLen = nMatchLen - MIN_MATCH_SIZE;
         int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN) ? LITERALS_RUN_LEN : nNumLiterals;
         int nTokenMatchLen = (nEncodedMatchLen >= MATCH_RUN_LEN) ? MATCH_RUN_LEN : nEncodedMatchLen;
         int nTokenLongOffset = (nMatchOffset <= 256) ? 0x00 : 0x80;
         int nCommandSize = 1 /* token */ + lzsa_get_literals_varlen_size(nNumLiterals) + nNumLiterals + (nTokenLongOffset ? 2 : 1) /* match offset */ + lzsa_get_match_varlen_size(nEncodedMatchLen);

         if ((nOutOffset + nCommandSize) > nMaxOutDataSize)
            return -1;
         if (nMatchOffset < MIN_OFFSET || nMatchOffset > MAX_OFFSET)
            return -1;

         pOutData[nOutOffset++] = nTokenLongOffset | (nTokenLiteralsLen << 4) | nTokenMatchLen;
         nOutOffset = lzsa_write_literals_varlen(pOutData, nOutOffset, nNumLiterals);

         if (nNumLiterals != 0) {
            memcpy(pOutData + nOutOffset, pInWindow + nInFirstLiteralOffset, nNumLiterals);
            nOutOffset += nNumLiterals;
            nNumLiterals = 0;
         }

         pOutData[nOutOffset++] = (nMatchOffset - 1) & 0xff;
         if (nTokenLongOffset) {
            pOutData[nOutOffset++] = (nMatchOffset - 1) >> 8;
         }
         nOutOffset = lzsa_write_match_varlen(pOutData, nOutOffset, nEncodedMatchLen);
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
      int nTokenLiteralsLen = (nNumLiterals >= LITERALS_RUN_LEN) ? LITERALS_RUN_LEN : nNumLiterals;
      int nCommandSize = 1 /* token */ + lzsa_get_literals_varlen_size(nNumLiterals) + nNumLiterals;

      if ((nOutOffset + nCommandSize) > nMaxOutDataSize)
         return -1;

      pOutData[nOutOffset++] = (nTokenLiteralsLen << 4) | 0x0f;
      nOutOffset = lzsa_write_literals_varlen(pOutData, nOutOffset, nNumLiterals);

      if (nNumLiterals != 0) {
         memcpy(pOutData + nOutOffset, pInWindow + nInFirstLiteralOffset, nNumLiterals);
         nOutOffset += nNumLiterals;
         nNumLiterals = 0;
      }

      pCompressor->num_commands++;
   }

   return nOutOffset;
}

/**
 * Compress one block of data
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
int lzsa_shrink_block(lsza_compressor *pCompressor, const unsigned char *pInWindow, const int nPreviousBlockSize, const int nInDataSize, unsigned char *pOutData, const int nMaxOutDataSize) {
   if (lzsa_build_suffix_array(pCompressor, pInWindow, nPreviousBlockSize + nInDataSize))
      return -1;
   if (nPreviousBlockSize) {
      lzsa_skip_matches(pCompressor, 0, nPreviousBlockSize);
   }
   lzsa_find_all_matches(pCompressor, nPreviousBlockSize, nPreviousBlockSize + nInDataSize);
   lzsa_optimize_matches(pCompressor, nPreviousBlockSize, nPreviousBlockSize + nInDataSize);
   lzsa_optimize_command_count(pCompressor, nPreviousBlockSize, nPreviousBlockSize + nInDataSize);

   return lzsa_write_block(pCompressor, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, pOutData, nMaxOutDataSize);
}

/**
 * Get the number of compression commands issued in compressed data blocks
 *
 * @return number of commands
 */
int lzsa_compressor_get_command_count(lsza_compressor *pCompressor) {
   return pCompressor->num_commands;
}
