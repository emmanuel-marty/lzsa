/*
 * shrink_context.h - compression context definitions
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

#ifndef _SHRINK_CONTEXT_H
#define _SHRINK_CONTEXT_H

#include "divsufsort.h"
#include "hashmap.h"

#ifdef __cplusplus
extern "C" {
#endif

#define LCP_BITS 14
#define LCP_MAX (1U<<(LCP_BITS - 1))
#define LCP_SHIFT (31-LCP_BITS)
#define LCP_MASK (((1U<<LCP_BITS) - 1) << LCP_SHIFT)
#define POS_MASK ((1U<<LCP_SHIFT) - 1)
#define VISITED_FLAG 0x80000000
#define EXCL_VISITED_MASK  0x7fffffff

#define NMATCHES_PER_OFFSET 8
#define MATCHES_PER_OFFSET_SHIFT 3

#define LEAVE_ALONE_MATCH_SIZE 1000

#define LAST_MATCH_OFFSET 4
#define LAST_LITERALS 1

#define MODESWITCH_PENALTY 1

/** One match */
typedef struct _lzsa_match {
   unsigned short length;
   unsigned short offset;
} lzsa_match;

/** One rep-match slot (for LZSA2) */
typedef struct _lzsa_repmatch_opt {
   int incoming_offset;
   short best_slot_for_incoming;
   short expected_repmatch;
} lzsa_repmatch_opt;

/** Compression context */
typedef struct _lzsa_compressor {
   divsufsort_ctx_t divsufsort_context;
   unsigned int *intervals;
   unsigned int *pos_data;
   unsigned int *open_intervals;
   lzsa_match *match;
   lzsa_match *selected_match;
   lzsa_match *best_match;
   lzsa_match *improved_match;
   int *slot_cost;
   lzsa_repmatch_opt *repmatch_opt;
   int min_match_size;
   int max_forward_depth;
   int format_version;
   int flags;
   int num_commands;
   lzsa_hashmap_t cost_map;
} lzsa_compressor;

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
int lzsa_compressor_init(lzsa_compressor *pCompressor, const int nMaxWindowSize, const int nMinMatchSize, const int nFormatVersion, const int nFlags);

/**
 * Clean up compression context and free up any associated resources
 *
 * @param pCompressor compression context to clean up
 */
void lzsa_compressor_destroy(lzsa_compressor *pCompressor);

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
int lzsa_compressor_shrink_block(lzsa_compressor *pCompressor, const unsigned char *pInWindow, const int nPreviousBlockSize, const int nInDataSize, unsigned char *pOutData, const int nMaxOutDataSize);

/**
 * Get the number of compression commands issued in compressed data blocks
 *
 * @return number of commands
 */
int lzsa_compressor_get_command_count(lzsa_compressor *pCompressor);

#ifdef __cplusplus
}
#endif

#endif /* _SHRINK_CONTEXT_H */
