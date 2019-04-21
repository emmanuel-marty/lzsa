/*
 * shrink.h - block compressor definitions
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

#ifndef _SHRINK_H
#define _SHRINK_H

#include "divsufsort.h"

/* Forward declarations */
typedef struct _lzsa_match lzsa_match;

/** Compression context */
typedef struct {
   divsufsort_ctx_t divsufsort_context;
   unsigned int *intervals;
   unsigned int *pos_data;
   unsigned int *open_intervals;
   lzsa_match *match;
   int min_match_size;
   int num_commands;
} lsza_compressor;

/**
 * Initialize compression context
 *
 * @param pCompressor compression context to initialize
 * @param nMaxWindowSize maximum size of input data window (previously compressed bytes + bytes to compress)
 * @param nMinMatchSize minimum match size (cannot be less than MIN_MATCH_SIZE)
 *
 * @return 0 for success, non-zero for failure
 */
int lzsa_compressor_init(lsza_compressor *pCompressor, const int nMaxWindowSize, const int nMinMatchSize);

/**
 * Clean up compression context and free up any associated resources
 *
 * @param pCompressor compression context to clean up
 */
void lzsa_compressor_destroy(lsza_compressor *pCompressor);

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
int lzsa_shrink_block(lsza_compressor *pCompressor, const unsigned char *pInWindow, const int nPreviousBlockSize, const int nInDataSize, unsigned char *pOutData, const int nMaxOutDataSize);

/**
 * Get the number of compression commands issued in compressed data blocks
 *
 * @return number of commands
 */
int lzsa_compressor_get_command_count(lsza_compressor *pCompressor);

#endif /* _SHRINK_H */
