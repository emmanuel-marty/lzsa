/*
 * inmem.h - in-memory decompression for benchmarks
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

#ifndef _INMEM_H
#define _INMEM_H

#include <stdio.h>

/**
 * Get maximum decompressed size of compressed data
 *
 * @param pFileData compressed data
 * @param nFileSize compressed size in bytes
 *
 * @return maximum decompressed size
 */
size_t lzsa_inmem_get_max_decompressed_size(const unsigned char *pFileData, size_t nFileSize);

/**
 * Decompress data in memory
 *
 * @param pFileData compressed data
 * @param pOutBuffer buffer for decompressed data
 * @param nFileSize compressed size in bytes
 * @param nMaxOutBufferSize maximum capacity of decompression buffer
 * @param pFormatVersion pointer to format version, updated if this function is successful
 *
 * @return actual decompressed size, or -1 for error
 */
size_t lzsa_inmem_decompress_stream(const unsigned char *pFileData, unsigned char *pOutBuffer, size_t nFileSize, size_t nMaxOutBufferSize, int *pFormatVersion);

#endif /* _INMEM_H */
