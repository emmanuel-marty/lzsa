/*
 * lib.c - LZSA library implementation
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
#include "lib.h"
#include "matchfinder.h"
#include "shrink_v1.h"
#include "shrink_v2.h"
#include "expand_v1.h"
#include "expand_v2.h"
#include "format.h"

/**
 * Initialize compression context
 *
 * @param pCompressor compression context to initialize
 * @param nMaxWindowSize maximum size of input data window (previously compressed bytes + bytes to compress)
 * @param nMinMatchSize minimum match size (cannot be less than MIN_MATCH_SIZE)
 * @param nFlags compression flags
 *
 * @return 0 for success, non-zero for failure
 */
int lzsa_compressor_init(lsza_compressor *pCompressor, const int nMaxWindowSize, const int nMinMatchSize, const int nFormatVersion, const int nFlags) {
   int nResult;
   int nMinMatchSizeForFormat = (nFormatVersion == 1) ? MIN_MATCH_SIZE_V1 : MIN_MATCH_SIZE_V2;

   nResult = divsufsort_init(&pCompressor->divsufsort_context);
   pCompressor->intervals = NULL;
   pCompressor->pos_data = NULL;
   pCompressor->open_intervals = NULL;
   pCompressor->match = NULL;
   pCompressor->best_match = NULL;
   pCompressor->slot_cost = NULL;
   pCompressor->repmatch_opt = NULL;
   pCompressor->min_match_size = nMinMatchSize;
   if (pCompressor->min_match_size < nMinMatchSizeForFormat)
      pCompressor->min_match_size = nMinMatchSizeForFormat;
   else if (pCompressor->min_match_size > 5)
      pCompressor->min_match_size = 5;
   pCompressor->format_version = nFormatVersion;
   pCompressor->flags = nFlags;
   pCompressor->num_commands = 0;

   if (!nResult) {
      pCompressor->intervals = (unsigned int *)malloc(nMaxWindowSize * sizeof(unsigned int));

      if (pCompressor->intervals) {
         pCompressor->pos_data = (unsigned int *)malloc(nMaxWindowSize * sizeof(unsigned int));

         if (pCompressor->pos_data) {
            pCompressor->open_intervals = (unsigned int *)malloc((LCP_MAX + 1) * sizeof(unsigned int));

            if (pCompressor->open_intervals) {
               pCompressor->match = (lzsa_match *)malloc(nMaxWindowSize * NMATCHES_PER_OFFSET * sizeof(lzsa_match));

               if (pCompressor->match) {
                  if (pCompressor->format_version == 2) {
                     pCompressor->best_match = (lzsa_match *)malloc(nMaxWindowSize * sizeof(lzsa_match));

                     if (pCompressor->best_match) {
                        pCompressor->slot_cost = (int *)malloc(nMaxWindowSize * NMATCHES_PER_OFFSET * sizeof(int));

                        if (pCompressor->slot_cost) {
                           pCompressor->repmatch_opt = (lzsa_repmatch_opt *)malloc(nMaxWindowSize * sizeof(lzsa_repmatch_opt));

                           if (pCompressor->repmatch_opt)
                              return 0;
                        }
                     }
                  }
                  else {
                     return 0;
                  }
               }
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

   if (pCompressor->repmatch_opt) {
      free(pCompressor->repmatch_opt);
      pCompressor->repmatch_opt = NULL;
   }

   if (pCompressor->slot_cost) {
      free(pCompressor->slot_cost);
      pCompressor->slot_cost = NULL;
   }

   if (pCompressor->best_match) {
      free(pCompressor->best_match);
      pCompressor->best_match = NULL;
   }

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

   if (pCompressor->format_version == 1) {
      return lzsa_optimize_and_write_block_v1(pCompressor, pInWindow, nPreviousBlockSize, nInDataSize, pOutData, nMaxOutDataSize);
   }
   else if (pCompressor->format_version == 2) {
      return lzsa_optimize_and_write_block_v2(pCompressor, pInWindow, nPreviousBlockSize, nInDataSize, pOutData, nMaxOutDataSize);
   }
   else {
      return -1;
   }
}

/**
 * Get the number of compression commands issued in compressed data blocks
 *
 * @return number of commands
 */
int lzsa_compressor_get_command_count(lsza_compressor *pCompressor) {
   return pCompressor->num_commands;
}

/**
 * Decompress one data block
 *
 * @param pInBlock pointer to compressed data
 * @param nInBlockSize size of compressed data, in bytes
 * @param pOutData pointer to output decompression buffer (previously decompressed bytes + room for decompressing this block)
 * @param nOutDataOffset starting index of where to store decompressed bytes in output buffer (and size of previously decompressed bytes)
 * @param nBlockMaxSize total size of output decompression buffer, in bytes
 *
 * @return size of decompressed data in bytes, or -1 for error
 */
int lzsa_expand_block(const int nFormatVersion, const unsigned char *pInBlock, int nBlockSize, unsigned char *pOutData, int nOutDataOffset, int nBlockMaxSize) {
   if (nFormatVersion == 1)
      return lzsa_expand_block_v1(pInBlock, nBlockSize, pOutData, nOutDataOffset, nBlockMaxSize);
   else if (nFormatVersion == 2)
      return lzsa_expand_block_v2(pInBlock, nBlockSize, pOutData, nOutDataOffset, nBlockMaxSize);
   else
      return -1;
}
