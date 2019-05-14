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
#include "frame.h"

#define BLOCK_SIZE 65536

/*-------------- Top level API -------------- */

/**
 * Compress file
 *
 * @param pszInFilename name of input(source) file to compress
 * @param pszOutFilename name of output(compressed) file to generate
 * @param pszDictionaryFilename name of dictionary file, or NULL for none
 * @param nFlags compression flags (LZSA_FLAG_xxx)
 * @param nMinMatchSize minimum match size
 * @param nFormatVersion version of format to use (1-2)
 * @param progress progress function, called after compressing each block, or NULL for none
 * @param pOriginalSize pointer to returned input(source) size, updated when this function is successful
 * @param pCompressedSize pointer to returned output(compressed) size, updated when this function is successful
 * @param pCommandCount pointer to returned token(compression commands) count, updated when this function is successful
 *
 * @return LZSA_OK for success, or an error value from lzsa_status_t
 */
lzsa_status_t lsza_compress_file(const char *pszInFilename, const char *pszOutFilename, const char *pszDictionaryFilename, const unsigned int nFlags, const int nMinMatchSize, const int nFormatVersion,
      void(*progress)(long long nOriginalSize, long long nCompressedSize), long long *pOriginalSize, long long *pCompressedSize, int *pCommandCount) {
   lzsa_stream_t inStream, outStream;
   void *pDictionaryData = NULL;
   int nDictionaryDataSize = 0;
   lzsa_status_t nStatus;

   if (lzsa_filestream_open(&inStream, pszInFilename, "rb") < 0) {
      return LZSA_ERROR_SRC;
   }

   if (lzsa_filestream_open(&outStream, pszOutFilename, "wb") < 0) {
      inStream.close(&inStream);
      return LZSA_ERROR_DST;
   }

   nStatus = lzsa_dictionary_load(pszDictionaryFilename, &pDictionaryData, &nDictionaryDataSize);

   if (nStatus) {
      outStream.close(&outStream);
      inStream.close(&inStream);
      return nStatus;
   }

   nStatus = lsza_compress_stream(&inStream, &outStream, pDictionaryData, nDictionaryDataSize, nFlags, nMinMatchSize, nFormatVersion, progress, pOriginalSize, pCompressedSize, pCommandCount);

   lzsa_dictionary_free(&pDictionaryData);
   outStream.close(&outStream);
   inStream.close(&inStream);
   return nStatus;
}

/**
 * Decompress file
 *
 * @param pszInFilename name of input(compressed) file to decompress
 * @param pszOutFilename name of output(decompressed) file to generate
 * @param pszDictionaryFilename name of dictionary file, or NULL for none
 * @param nFlags compression flags (LZSA_FLAG_RAW_BLOCK to decompress a raw block, or 0)
 * @param nFormatVersion default version of format to use (1-2). This is used when decompressing a raw block, otherwise the version is extracted from the source file
 * @param pOriginalSize pointer to returned output(decompressed) size, updated when this function is successful
 * @param pCompressedSize pointer to returned input(compressed) size, updated when this function is successful
 *
 * @return LZSA_OK for success, or an error value from lzsa_status_t
 */
lzsa_status_t lzsa_decompress_file(const char *pszInFilename, const char *pszOutFilename, const char *pszDictionaryFilename, const unsigned int nFlags, int nFormatVersion,
                                   long long *pOriginalSize, long long *pCompressedSize) {
   lzsa_stream_t inStream, outStream;
   void *pDictionaryData = NULL;
   int nDictionaryDataSize = 0;
   lzsa_status_t nStatus;

   if (lzsa_filestream_open(&inStream, pszInFilename, "rb") < 0) {
      return LZSA_ERROR_SRC;
   }

   if (lzsa_filestream_open(&outStream, pszOutFilename, "wb") < 0) {
      inStream.close(&inStream);
      return LZSA_ERROR_DST;
   }

   nStatus = lzsa_dictionary_load(pszDictionaryFilename, &pDictionaryData, &nDictionaryDataSize);
   if (nStatus) {
      outStream.close(&outStream);
      inStream.close(&inStream);
      return nStatus;
   }

   nStatus = lzsa_decompress_stream(&inStream, &outStream, pDictionaryData, nDictionaryDataSize, nFlags, nFormatVersion, pOriginalSize, pCompressedSize);

   lzsa_dictionary_free(&pDictionaryData);
   outStream.close(&outStream);
   inStream.close(&inStream);

   return nStatus;
}

/*-------------- Streaming API -------------- */

/**
 * Load dictionary contents
 *
 * @param pszDictionaryFilename name of dictionary file, or NULL for none
 * @param pDictionaryData pointer to returned dictionary contents, or NULL for none
 * @param nDictionaryDataSize pointer to returned size of dictionary contents, or 0
 *
 * @return LZSA_OK for success, or an error value from lzsa_status_t
 */
int lzsa_dictionary_load(const char *pszDictionaryFilename, void **ppDictionaryData, int *pDictionaryDataSize) {
   unsigned char *pDictionaryData = NULL;
   int nDictionaryDataSize = 0;

   if (pszDictionaryFilename) {
      pDictionaryData = (unsigned char *)malloc(BLOCK_SIZE);
      if (!pDictionaryData) {
         return LZSA_ERROR_MEMORY;
      }

      FILE *pDictionaryFile = fopen(pszDictionaryFilename, "rb");
      if (!pDictionaryFile) {
         free(pDictionaryData);
         pDictionaryData = NULL;
         return LZSA_ERROR_DICTIONARY;
      }

      fseek(pDictionaryFile, 0, SEEK_END);
#ifdef _WIN32
      __int64 nDictionaryFileSize = _ftelli64(pDictionaryFile);
#else
      off_t nDictionaryFileSize = ftello(pDictionaryFile);
#endif
      if (nDictionaryFileSize > BLOCK_SIZE) {
         /* Use the last BLOCK_SIZE bytes of the dictionary */
         fseek(pDictionaryFile, -BLOCK_SIZE, SEEK_END);
      }
      else {
         fseek(pDictionaryFile, 0, SEEK_SET);
      }

      nDictionaryDataSize = (int)fread(pDictionaryData, 1, BLOCK_SIZE, pDictionaryFile);
      if (nDictionaryDataSize < 0)
         nDictionaryDataSize = 0;

      fclose(pDictionaryFile);
      pDictionaryFile = NULL;
   }

   *ppDictionaryData = pDictionaryData;
   *pDictionaryDataSize = nDictionaryDataSize;
   return LZSA_OK;
}

/**
 * Free dictionary contents
 *
 * @param pDictionaryData pointer to pointer to dictionary contents
 */
void lzsa_dictionary_free(void **ppDictionaryData) {
   if (*ppDictionaryData) {
      free(*ppDictionaryData);
      ppDictionaryData = NULL;
   }
}

/**
 * Compress stream
 *
 * @param pInStream input(source) stream to compress
 * @param pOutStream output(compressed) stream to write to
 * @param pDictionaryData dictionary contents, or NULL for none
 * @param nDictionaryDataSize size of dictionary contents, or 0
 * @param nFlags compression flags (LZSA_FLAG_xxx)
 * @param nMinMatchSize minimum match size
 * @param nFormatVersion version of format to use (1-2)
 * @param progress progress function, called after compressing each block, or NULL for none
 * @param pOriginalSize pointer to returned input(source) size, updated when this function is successful
 * @param pCompressedSize pointer to returned output(compressed) size, updated when this function is successful
 * @param pCommandCount pointer to returned token(compression commands) count, updated when this function is successful
 *
 * @return LZSA_OK for success, or an error value from lzsa_status_t
 */
lzsa_status_t lsza_compress_stream(lzsa_stream_t *pInStream, lzsa_stream_t *pOutStream, const void *pDictionaryData, int nDictionaryDataSize,
                                   const unsigned int nFlags, const int nMinMatchSize, const int nFormatVersion,
                                   void(*progress)(long long nOriginalSize, long long nCompressedSize), long long *pOriginalSize, long long *pCompressedSize, int *pCommandCount) {
   unsigned char *pInData, *pOutData;
   lsza_compressor compressor;
   long long nStartTime = 0LL, nEndTime = 0LL;
   long long nOriginalSize = 0LL, nCompressedSize = 0LL;
   int nResult;
   unsigned char cFrameData[16];
   int nError = 0;

   pInData = (unsigned char*)malloc(BLOCK_SIZE * 2);
   if (!pInData) {
      return LZSA_ERROR_MEMORY;
   }
   memset(pInData, 0, BLOCK_SIZE * 2);

   pOutData = (unsigned char*)malloc(BLOCK_SIZE);
   if (!pOutData) {
      free(pInData);
      pInData = NULL;

      return LZSA_ERROR_MEMORY;
   }
   memset(pOutData, 0, BLOCK_SIZE);

   nResult = lzsa_compressor_init(&compressor, BLOCK_SIZE * 2, nMinMatchSize, nFormatVersion, nFlags);
   if (nResult != 0) {
      free(pOutData);
      pOutData = NULL;

      free(pInData);
      pInData = NULL;

      return LZSA_ERROR_MEMORY;
   }

   if ((nFlags & LZSA_FLAG_RAW_BLOCK) == 0) {
      int nHeaderSize = lzsa_encode_header(cFrameData, 16, nFormatVersion);
      if (nHeaderSize < 0)
         nError = LZSA_ERROR_COMPRESSION;
      else {
         if (pOutStream->write(pOutStream, cFrameData, nHeaderSize) != nHeaderSize)
            nError = LZSA_ERROR_DST;
         nCompressedSize += (long long)nHeaderSize;
      }
   }

   int nPreviousBlockSize = 0;
   int nNumBlocks = 0;

   while (!pInStream->eof(pInStream) && !nError) {
      int nInDataSize;

      if (nPreviousBlockSize) {
         memcpy(pInData + BLOCK_SIZE - nPreviousBlockSize, pInData + BLOCK_SIZE, nPreviousBlockSize);
      }
      else if (nDictionaryDataSize && pDictionaryData) {
         nPreviousBlockSize = nDictionaryDataSize;
         memcpy(pInData + BLOCK_SIZE - nPreviousBlockSize, pDictionaryData, nPreviousBlockSize);
      }

      nInDataSize = (int)pInStream->read(pInStream, pInData + BLOCK_SIZE, BLOCK_SIZE);
      if (nInDataSize > 0) {
         if ((nFlags & LZSA_FLAG_RAW_BLOCK) != 0 && nNumBlocks) {
            nError = LZSA_ERROR_RAW_TOOLARGE;
            break;
         }
         nDictionaryDataSize = 0;

         int nOutDataSize;

         nOutDataSize = lzsa_compressor_shrink_block(&compressor, pInData + BLOCK_SIZE - nPreviousBlockSize, nPreviousBlockSize, nInDataSize, pOutData, (nInDataSize >= BLOCK_SIZE) ? BLOCK_SIZE : nInDataSize);
         if (nOutDataSize >= 0) {
            /* Write compressed block */

            if ((nFlags & LZSA_FLAG_RAW_BLOCK) == 0) {
               int nBlockheaderSize = lzsa_encode_compressed_block_frame(cFrameData, 16, nOutDataSize);
               if (nBlockheaderSize < 0)
                  nError = LZSA_ERROR_COMPRESSION;
               else {
                  nCompressedSize += (long long)nBlockheaderSize;
                  if (pOutStream->write(pOutStream, cFrameData, nBlockheaderSize) != (size_t)nBlockheaderSize) {
                     nError = LZSA_ERROR_DST;
                  }
               }
            }

            if (!nError) {
               if (pOutStream->write(pOutStream, pOutData, (size_t)nOutDataSize) != (size_t)nOutDataSize) {
                  nError = LZSA_ERROR_DST;
               }
               else {
                  nOriginalSize += (long long)nInDataSize;
                  nCompressedSize += (long long)nOutDataSize;
               }
            }
         }
         else {
            /* Write uncompressible, literal block */

            if ((nFlags & LZSA_FLAG_RAW_BLOCK) != 0) {
               nError = LZSA_ERROR_RAW_UNCOMPRESSED;
               break;
            }

            int nBlockheaderSize = lzsa_encode_uncompressed_block_frame(cFrameData, 16, nInDataSize);
            if (nBlockheaderSize < 0)
               nError = LZSA_ERROR_COMPRESSION;
            else {
               if (pOutStream->write(pOutStream, cFrameData, nBlockheaderSize) != (size_t)nBlockheaderSize) {
                  nError = LZSA_ERROR_DST;
               }
               else {
                  if (pOutStream->write(pOutStream, pInData + BLOCK_SIZE, (size_t)nInDataSize) != (size_t)nInDataSize) {
                     nError = LZSA_ERROR_DST;
                  }
                  else {
                     nOriginalSize += (long long)nInDataSize;
                     nCompressedSize += (long long)nBlockheaderSize + (long long)nInDataSize;
                  }
               }
            }
         }

         nPreviousBlockSize = nInDataSize;
         nNumBlocks++;
      }

      if (!nError && !pInStream->eof(pInStream)) {
         if (progress)
            progress(nOriginalSize, nCompressedSize);
      }
   }

   if (!nError) {
      int nFooterSize;

      if ((nFlags & LZSA_FLAG_RAW_BLOCK) != 0) {
         nFooterSize = 0;
      }
      else {
         nFooterSize = lzsa_encode_footer_frame(cFrameData, 16);
         if (nFooterSize < 0)
            nError = LZSA_ERROR_COMPRESSION;
      }

      if (pOutStream->write(pOutStream, cFrameData, nFooterSize) != nFooterSize)
         nError = LZSA_ERROR_DST;
      nCompressedSize += (long long)nFooterSize;
   }

   if (progress)
      progress(nOriginalSize, nCompressedSize);

   int nCommandCount = lzsa_compressor_get_command_count(&compressor);
   lzsa_compressor_destroy(&compressor);

   free(pOutData);
   pOutData = NULL;

   free(pInData);
   pInData = NULL;

   if (nError) {
      return nError;
   }
   else {
      if (pOriginalSize)
         *pOriginalSize = nOriginalSize;
      if (pCompressedSize)
         *pCompressedSize = nCompressedSize;
      if (pCommandCount)
         *pCommandCount = nCommandCount;
      return LZSA_OK;
   }
}

/**
 * Decompress stream
 *
 * @param pInStream input(compressed) stream to decompress
 * @param pOutStream output(decompressed) stream to write to
 * @param pDictionaryData dictionary contents, or NULL for none
 * @param nDictionaryDataSize size of dictionary contents, or 0
 * @param nFlags compression flags (LZSA_FLAG_RAW_BLOCK to decompress a raw block, or 0)
 * @param nFormatVersion default version of format to use (1-2). This is used when decompressing a raw block, otherwise the version is extracted from the source file
 * @param pOriginalSize pointer to returned output(decompressed) size, updated when this function is successful
 * @param pCompressedSize pointer to returned input(compressed) size, updated when this function is successful
 *
 * @return LZSA_OK for success, or an error value from lzsa_status_t
 */
lzsa_status_t lzsa_decompress_stream(lzsa_stream_t *pInStream, lzsa_stream_t *pOutStream, const void *pDictionaryData, int nDictionaryDataSize, const unsigned int nFlags, int nFormatVersion,
      long long *pOriginalSize, long long *pCompressedSize) {
   long long nStartTime = 0LL, nEndTime = 0LL;
   long long nOriginalSize = 0LL, nCompressedSize = 0LL;
   unsigned char cFrameData[16];
   unsigned char *pInBlock;
   unsigned char *pOutData;

   if ((nFlags & LZSA_FLAG_RAW_BLOCK) == 0) {
      const int nHeaderSize = lzsa_get_header_size();

      memset(cFrameData, 0, 16);
      if (pInStream->read(pInStream, cFrameData, nHeaderSize) != nHeaderSize) {
         return LZSA_ERROR_SRC;
      }

      if (lzsa_decode_header(cFrameData, nHeaderSize, &nFormatVersion) < 0) {
         return LZSA_ERROR_FORMAT;
      }

      nCompressedSize += (long long)nHeaderSize;
   }

   pInBlock = (unsigned char*)malloc(BLOCK_SIZE);
   if (!pInBlock) {
      return LZSA_ERROR_MEMORY;
   }

   pOutData = (unsigned char*)malloc(BLOCK_SIZE * 2);
   if (!pOutData) {
      free(pInBlock);
      pInBlock = NULL;

      return LZSA_ERROR_MEMORY;
   }

   int nDecompressionError = 0;
   int nPrevDecompressedSize = 0;
   int nNumBlocks = 0;

   while (!pInStream->eof(pInStream) && !nDecompressionError) {
      unsigned int nBlockSize = 0;
      int nIsUncompressed = 0;

      if (nPrevDecompressedSize != 0) {
         memcpy(pOutData + BLOCK_SIZE - nPrevDecompressedSize, pOutData + BLOCK_SIZE, nPrevDecompressedSize);
      }
      else if (nDictionaryDataSize && pDictionaryData) {
         nPrevDecompressedSize = nDictionaryDataSize;
         memcpy(pOutData + BLOCK_SIZE - nPrevDecompressedSize, pDictionaryData, nPrevDecompressedSize);
      }

      if ((nFlags & LZSA_FLAG_RAW_BLOCK) == 0) {
         const int nFrameSize = lzsa_get_frame_size();

         memset(cFrameData, 0, 16);
         if (pInStream->read(pInStream, cFrameData, nFrameSize) == nFrameSize) {
            if (lzsa_decode_frame(cFrameData, nFrameSize, &nBlockSize, &nIsUncompressed) < 0) {
               nDecompressionError = LZSA_ERROR_FORMAT;
               nBlockSize = 0;
            }

            nCompressedSize += (long long)nFrameSize;
         }
         else {
            nDecompressionError = LZSA_ERROR_SRC;
            nBlockSize = 0;
         }
      }
      else {
         if (!nNumBlocks)
            nBlockSize = BLOCK_SIZE;
         else
            nBlockSize = 0;
      }

      if (nBlockSize != 0) {
         int nDecompressedSize = 0;

         if ((int)nBlockSize > BLOCK_SIZE) {
            nDecompressionError = LZSA_ERROR_FORMAT;
            break;
         }
         size_t nReadBytes = pInStream->read(pInStream, pInBlock, nBlockSize);
         if (nFlags & LZSA_FLAG_RAW_BLOCK) {
            if (nReadBytes > 4)
               nReadBytes -= 4;
            else
               nReadBytes = 0;
            nBlockSize = (unsigned int)nReadBytes;
         }

         if (nReadBytes == nBlockSize) {
            nCompressedSize += (long long)nReadBytes;

            if (nIsUncompressed) {
               memcpy(pOutData + BLOCK_SIZE, pInBlock, nBlockSize);
               nDecompressedSize = nBlockSize;
            }
            else {
               unsigned int nBlockOffs = 0;

               nDecompressedSize = lzsa_decompressor_expand_block(nFormatVersion, pInBlock, nBlockSize, pOutData, BLOCK_SIZE, BLOCK_SIZE);
               if (nDecompressedSize < 0) {
                  nDecompressionError = LZSA_ERROR_DECOMPRESSION;
                  break;
               }
            }

            if (nDecompressedSize != 0) {
               nOriginalSize += (long long)nDecompressedSize;

               if (pOutStream->write(pOutStream, pOutData + BLOCK_SIZE, nDecompressedSize) != nDecompressedSize)
                  nDecompressionError = LZSA_ERROR_DST;
               nPrevDecompressedSize = nDecompressedSize;
               nDecompressedSize = 0;
            }
         }
         else {
            break;
         }

         nNumBlocks++;
      }
      else {
         break;
      }
   }

   free(pOutData);
   pOutData = NULL;

   free(pInBlock);
   pInBlock = NULL;

   *pOriginalSize = nOriginalSize;
   *pCompressedSize = nCompressedSize;
   return nDecompressionError;
}

/*-------------- Block compression API --------------*/

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
int lzsa_compressor_shrink_block(lsza_compressor *pCompressor, const unsigned char *pInWindow, const int nPreviousBlockSize, const int nInDataSize, unsigned char *pOutData, const int nMaxOutDataSize) {
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
int lzsa_decompressor_expand_block(const int nFormatVersion, const unsigned char *pInBlock, int nBlockSize, unsigned char *pOutData, int nOutDataOffset, int nBlockMaxSize) {
   if (nFormatVersion == 1)
      return lzsa_decompressor_expand_block_v1(pInBlock, nBlockSize, pOutData, nOutDataOffset, nBlockMaxSize);
   else if (nFormatVersion == 2)
      return lzsa_decompressor_expand_block_v2(pInBlock, nBlockSize, pOutData, nOutDataOffset, nBlockMaxSize);
   else
      return -1;
}
