/*
 * main.c - command line compression utility for the LZSA format
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

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <sys/timeb.h>
#else
#include <sys/time.h>
#endif
#include "format.h"
#include "frame.h"
#include "shrink.h"
#include "expand.h"

#define BLOCK_SIZE 65536
#define OPT_VERBOSE     1
#define OPT_RAW         2
#define OPT_FAVOR_RATIO 4

/*---------------------------------------------------------------------------*/

static long long lzsa_get_time() {
   long long nTime;

#ifdef _WIN32
   struct _timeb tb;
   _ftime(&tb);

   nTime = ((long long)tb.time * 1000LL + (long long)tb.millitm) * 1000LL;
#else
   struct timeval tm;
   gettimeofday(&tm, NULL);

   nTime = (long long)tm.tv_sec * 1000000LL + (long long)tm.tv_usec;
#endif
   return nTime;
}

/*---------------------------------------------------------------------------*/

static int lzsa_compress(const char *pszInFilename, const char *pszOutFilename, const char *pszDictionaryFilename, const unsigned int nOptions, const int nMinMatchSize) {
   FILE *f_in, *f_out;
   unsigned char *pInData, *pOutData;
   lsza_compressor compressor;
   long long nStartTime = 0LL, nEndTime = 0LL;
   long long nOriginalSize = 0LL, nCompressedSize = 0LL;
   int nFlags;
   int nResult;
   unsigned char cFrameData[16];
   bool bError = false;

   f_in = fopen(pszInFilename, "rb");
   if (!f_in) {
      fprintf(stderr, "error opening '%s' for reading\n", pszInFilename);
      return 100;
   }

   f_out = fopen(pszOutFilename, "wb");
   if (!f_out) {
      fprintf(stderr, "error opening '%s' for writing\n", pszOutFilename);
      return 100;
   }

   pInData = (unsigned char*)malloc(BLOCK_SIZE * 2);
   if (!pInData) {
      fclose(f_out);
      f_out = NULL;

      fclose(f_in);
      f_in = NULL;

      fprintf(stderr, "out of memory\n");
      return 100;
   }
   memset(pInData, 0, BLOCK_SIZE * 2);

   pOutData = (unsigned char*)malloc(BLOCK_SIZE);
   if (!pOutData) {
      free(pInData);
      pInData = NULL;

      fclose(f_out);
      f_out = NULL;

      fclose(f_in);
      f_in = NULL;

      fprintf(stderr, "out of memory\n");
      return 100;
   }
   memset(pOutData, 0, BLOCK_SIZE);

   int nDictionaryDataSize = 0;

   if (pszDictionaryFilename) {
      FILE *f_dictionary = fopen(pszDictionaryFilename, "rb");
      if (!f_dictionary) {
         free(pOutData);
         pOutData = NULL;

         free(pInData);
         pInData = NULL;

         fclose(f_out);
         f_out = NULL;

         fclose(f_in);
         f_in = NULL;

         fprintf(stderr, "error opening dictionary '%s' for reading\n", pszInFilename);
         return 100;
      }

      nDictionaryDataSize = (int)fread(pInData + BLOCK_SIZE, 1, BLOCK_SIZE - 1, f_dictionary);
      if (nDictionaryDataSize < 0)
         nDictionaryDataSize = 0;

      fclose(f_dictionary);
      f_dictionary = NULL;
   }

   nFlags = 0;
   if (nOptions & OPT_FAVOR_RATIO)
      nFlags |= LZSA_FLAG_FAVOR_RATIO;
   if (nOptions & OPT_RAW)
      nFlags |= LZSA_FLAG_RAW_BLOCK;
   nResult = lzsa_compressor_init(&compressor, BLOCK_SIZE * 2, nMinMatchSize, nFlags);
   if (nResult != 0) {
      free(pOutData);
      pOutData = NULL;

      free(pInData);
      pInData = NULL;

      fclose(f_out);
      f_out = NULL;

      fclose(f_in);
      f_in = NULL;

      fprintf(stderr, "error initializing compressor\n");
      return 100;
   }

   if ((nOptions & OPT_RAW) == 0) {
      int nHeaderSize = lzsa_encode_header(cFrameData, 16);
      if (nHeaderSize < 0)
         bError = true;
      else {
         bError = fwrite(cFrameData, 1, nHeaderSize, f_out) != nHeaderSize;
         nCompressedSize += (long long)nHeaderSize;
      }
   }

   if (nOptions & OPT_VERBOSE) {
      nStartTime = lzsa_get_time();
   }

   int nPreviousBlockSize = 0;

   if (nDictionaryDataSize)
      nPreviousBlockSize = nDictionaryDataSize;

   while (!feof(f_in) && !bError) {
      int nInDataSize;

      if (nPreviousBlockSize) {
         memcpy(pInData + BLOCK_SIZE - nPreviousBlockSize, pInData + BLOCK_SIZE, nPreviousBlockSize);
      }

      nInDataSize = (int)fread(pInData + BLOCK_SIZE, 1, BLOCK_SIZE, f_in);
      if (nInDataSize > 0) {
         if (nPreviousBlockSize && (nOptions & OPT_RAW) != 0 && !nDictionaryDataSize) {
            fprintf(stderr, "error: raw blocks can only be used with files <= 64 Kb\n");
            bError = true;
            break;
         }
         nDictionaryDataSize = 0;

         int nOutDataSize;

         nOutDataSize = lzsa_shrink_block(&compressor, pInData + BLOCK_SIZE - nPreviousBlockSize, nPreviousBlockSize, nInDataSize, pOutData, (nInDataSize >= BLOCK_SIZE) ? BLOCK_SIZE : nInDataSize);
         if (nOutDataSize >= 0) {
            /* Write compressed block */

            if ((nOptions & OPT_RAW) == 0) {
               int nBlockheaderSize = lzsa_encode_compressed_block_frame(cFrameData, 16, nOutDataSize);
               if (nBlockheaderSize < 0)
                  bError = true;
               else {
                  nCompressedSize += (long long)nBlockheaderSize;
                  if (fwrite(cFrameData, 1, nBlockheaderSize, f_out) != (size_t)nBlockheaderSize) {
                     bError = true;
                  }
               }
            }

            if (!bError) {
               if (fwrite(pOutData, 1, (size_t)nOutDataSize, f_out) != (size_t)nOutDataSize) {
                  bError = true;
               }
               else {
                  nOriginalSize += (long long)nInDataSize;
                  nCompressedSize += (long long)nOutDataSize;
               }
            }
         }
         else {
            /* Write uncompressible, literal block */

            if ((nOptions & OPT_RAW) != 0) {
               fprintf(stderr, "error: data is incompressible, raw blocks only support compressed data\n");
               bError = true;
               break;
            }

            int nBlockheaderSize = lzsa_encode_uncompressed_block_frame(cFrameData, 16, nInDataSize);
            if (nBlockheaderSize < 0)
               bError = true;
            else {
               if (fwrite(cFrameData, 1, nBlockheaderSize, f_out) != (size_t)nBlockheaderSize) {
                  bError = true;
               }
               else {
                  if (fwrite(pInData + BLOCK_SIZE, 1, (size_t)nInDataSize, f_out) != (size_t)nInDataSize) {
                     bError = true;
                  }
                  else {
                     nOriginalSize += (long long)nInDataSize;
                     nCompressedSize += (long long)nBlockheaderSize + (long long)nInDataSize;
                  }
               }
            }
         }

         nPreviousBlockSize = nInDataSize;
      }

      if (!bError && !feof(f_in) && nOriginalSize >= 1024 * 1024) {
         fprintf(stdout, "\r%lld => %lld (%g %%)", nOriginalSize, nCompressedSize, (double)(nCompressedSize * 100.0 / nOriginalSize));
         fflush(stdout);
      }
   }

   int nFooterSize;

   if ((nOptions & OPT_RAW) != 0) {
      nFooterSize = 0;
   }
   else {
      nFooterSize = lzsa_encode_footer_frame(cFrameData, 16);
      if (nFooterSize < 0)
         bError = true;
   }

   if (!bError)
      bError = fwrite(cFrameData, 1, nFooterSize, f_out) != nFooterSize;
   nCompressedSize += (long long)nFooterSize;

   if (!bError && (nOptions & OPT_VERBOSE)) {
      nEndTime = lzsa_get_time();

      double fDelta = ((double)(nEndTime - nStartTime)) / 1000000.0;
      double fSpeed = ((double)nOriginalSize / 1048576.0) / fDelta;
      int nCommands = lzsa_compressor_get_command_count(&compressor);
      fprintf(stdout, "\rCompressed '%s' in %g seconds, %.02g Mb/s, %d tokens (%g bytes/token), %lld into %lld bytes ==> %g %%\n",
         pszInFilename, fDelta, fSpeed, nCommands, (double)nOriginalSize / (double)nCommands,
         nOriginalSize, nCompressedSize, (double)(nCompressedSize * 100.0 / nOriginalSize));
   }

   lzsa_compressor_destroy(&compressor);

   free(pOutData);
   pOutData = NULL;

   free(pInData);
   pInData = NULL;

   fclose(f_out);
   f_out = NULL;

   fclose(f_in);
   f_in = NULL;

   if (bError) {
      fprintf(stderr, "\rcompression error for '%s'\n", pszInFilename);
      return 100;
   }
   else {
      return 0;
   }
}

/*---------------------------------------------------------------------------*/

static int lzsa_decompress(const char *pszInFilename, const char *pszOutFilename, const char *pszDictionaryFilename, const unsigned int nOptions) {
   long long nStartTime = 0LL, nEndTime = 0LL;
   long long nOriginalSize = 0LL;
   unsigned int nFileSize = 0;
   unsigned char cFrameData[16];

   FILE *pInFile = fopen(pszInFilename, "rb");
   if (!pInFile) {
      fprintf(stderr, "error opening input file\n");
      return 100;
   }

   if ((nOptions & OPT_RAW) == 0) {
      const int nHeaderSize = lzsa_get_header_size();

      memset(cFrameData, 0, 16);
      if (fread(cFrameData, 1, nHeaderSize, pInFile) != nHeaderSize) {
         fclose(pInFile);
         pInFile = NULL;
         fprintf(stderr, "error reading header in input file\n");
         return 100;
      }

      if (lzsa_decode_header(cFrameData, nHeaderSize) < 0) {
         fclose(pInFile);
         pInFile = NULL;
         fprintf(stderr, "invalid magic number or format version in input file\n");
         return 100;
      }
   }
   else {
      fseek(pInFile, 0, SEEK_END);
      nFileSize = (unsigned int)ftell(pInFile);
      fseek(pInFile, 0, SEEK_SET);

      if (nFileSize < 4) {
         fclose(pInFile);
         pInFile = NULL;
         fprintf(stderr, "invalid file size for raw block mode\n");
         return 100;
      }
   }

   FILE *pOutFile = fopen(pszOutFilename, "wb");
   if (!pOutFile) {
      fclose(pInFile);
      pInFile = NULL;
      fprintf(stderr, "error opening output file\n");
      return 100;
   }

   unsigned char *pInBlock;
   unsigned char *pOutData;

   pInBlock = (unsigned char*)malloc(BLOCK_SIZE);
   if (!pInBlock) {
      fclose(pOutFile);
      pOutFile = NULL;

      fclose(pInFile);
      pInFile = NULL;
      fprintf(stderr, "error opening output file\n");
      return 100;
   }

   pOutData = (unsigned char*)malloc(BLOCK_SIZE * 2);
   if (!pOutData) {
      free(pInBlock);
      pInBlock = NULL;

      fclose(pOutFile);
      pOutFile = NULL;

      fclose(pInFile);
      pInFile = NULL;
      fprintf(stderr, "error opening output file\n");
      return 100;
   }

   int nDictionaryDataSize = 0;
   if (pszDictionaryFilename) {
      FILE *pDictionaryFile = fopen(pszDictionaryFilename, "rb");
      if (!pDictionaryFile) {
         free(pOutData);
         pOutData = NULL;

         free(pInBlock);
         pInBlock = NULL;

         fclose(pOutFile);
         pOutFile = NULL;

         fclose(pInFile);
         pInFile = NULL;

         fprintf(stderr, "error opening dictionary file\n");
         return 100;
      }

      nDictionaryDataSize = (int)fread(pOutData + BLOCK_SIZE, 1, BLOCK_SIZE - 1, pDictionaryFile);
      if (nDictionaryDataSize < 0)
         nDictionaryDataSize = 0;

      fclose(pDictionaryFile);
      pDictionaryFile = NULL;
   }

   if (nOptions & OPT_VERBOSE) {
      nStartTime = lzsa_get_time();
   }

   int nDecompressionError = 0;
   int nPrevDecompressedSize = 0;

   if (nDictionaryDataSize) {
      nPrevDecompressedSize = nDictionaryDataSize;
   }

   while (!feof(pInFile) && !nDecompressionError) {
      unsigned int nBlockSize = 0;
      int nIsUncompressed = 0;

      if (nPrevDecompressedSize != 0) {
         memcpy(pOutData + BLOCK_SIZE - nPrevDecompressedSize, pOutData + BLOCK_SIZE, nPrevDecompressedSize);
      }

      if ((nOptions & OPT_RAW) == 0) {
         const int nFrameSize = lzsa_get_frame_size();

         memset(cFrameData, 0, 16);
         if (fread(cFrameData, 1, nFrameSize, pInFile) == nFrameSize) {
            if (lzsa_decode_frame(cFrameData, nFrameSize, &nBlockSize, &nIsUncompressed) < 0) {
               nDecompressionError = 1;
               nBlockSize = 0;
            }
         }
         else {
            nBlockSize = 0;
         }
      }
      else {
         if (nFileSize >= 4)
            nBlockSize = nFileSize - 4;
         nFileSize = 0;
      }

      if (nBlockSize != 0) {
         int nDecompressedSize = 0;

         if ((int)nBlockSize > BLOCK_SIZE) {
            fprintf(stderr, "block size %d > max size %d\n", nBlockSize, BLOCK_SIZE);
            break;
         }
         if (fread(pInBlock, 1, nBlockSize, pInFile) == nBlockSize) {
            if (nIsUncompressed) {
               memcpy(pOutData + BLOCK_SIZE, pInBlock, nBlockSize);
               nDecompressedSize = nBlockSize;
            }
            else {
               unsigned int nBlockOffs = 0;

               nDecompressedSize = lzsa_expand_block(pInBlock, nBlockSize, pOutData, BLOCK_SIZE, BLOCK_SIZE);
               if (nDecompressedSize < 0) {
                  nDecompressionError = nDecompressedSize;
                  break;
               }
            }

            if (nDecompressedSize != 0) {
               nOriginalSize += (long long)nDecompressedSize;

               fwrite(pOutData + BLOCK_SIZE, 1, nDecompressedSize, pOutFile);
               nPrevDecompressedSize = nDecompressedSize;
               nDecompressedSize = 0;
            }
         }
         else {
            break;
         }
      }
      else {
         break;
      }
   }

   free(pOutData);
   pOutData = NULL;

   free(pInBlock);
   pInBlock = NULL;

   fclose(pOutFile);
   pOutFile = NULL;

   fclose(pInFile);
   pInFile = NULL;

   if (nDecompressionError) {
      fprintf(stderr, "decompression error for '%s'\n", pszInFilename);
      return 100;
   }
   else {
      if (nOptions & OPT_VERBOSE) {
         nEndTime = lzsa_get_time();
         double fDelta = ((double)(nEndTime - nStartTime)) / 1000000.0;
         double fSpeed = ((double)nOriginalSize / 1048576.0) / fDelta;
         fprintf(stdout, "Decompressed '%s' in %g seconds, %g Mb/s\n",
            pszInFilename, fDelta, fSpeed);
      }

      return 0;
   }
}

static int lzsa_compare(const char *pszInFilename, const char *pszOutFilename, const char *pszDictionaryFilename, const unsigned int nOptions) {
   long long nStartTime = 0LL, nEndTime = 0LL;
   long long nOriginalSize = 0LL;
   long long nKnownGoodSize = 0LL;
   unsigned int nFileSize = 0;
   unsigned char cFrameData[16];

   FILE *pInFile = fopen(pszInFilename, "rb");
   if (!pInFile) {
      fprintf(stderr, "error opening compressed input file\n");
      return 100;
   }

   if ((nOptions & OPT_RAW) == 0) {
      const int nHeaderSize = lzsa_get_header_size();

      memset(cFrameData, 0, 16);
      if (fread(cFrameData, 1, nHeaderSize, pInFile) != nHeaderSize) {
         fclose(pInFile);
         pInFile = NULL;
         fprintf(stderr, "error reading header in compressed input file\n");
         return 100;
      }

      if (lzsa_decode_header(cFrameData, nHeaderSize) < 0) {
         fclose(pInFile);
         pInFile = NULL;
         fprintf(stderr, "invalid magic number or format version in input file\n");
         return 100;
      }
   }
   else {
      fseek(pInFile, 0, SEEK_END);
      nFileSize = (unsigned int)ftell(pInFile);
      fseek(pInFile, 0, SEEK_SET);

      if (nFileSize < 4) {
         fclose(pInFile);
         pInFile = NULL;
         fprintf(stderr, "invalid file size for raw block mode\n");
         return 100;
      }
   }

   FILE *pOutFile = fopen(pszOutFilename, "rb");
   if (!pOutFile) {
      fclose(pInFile);
      pInFile = NULL;
      fprintf(stderr, "error opening original uncompressed file\n");
      return 100;
   }

   unsigned char *pInBlock;
   unsigned char *pOutData;
   unsigned char *pCompareData;

   pInBlock = (unsigned char*)malloc(BLOCK_SIZE);
   if (!pInBlock) {
      fclose(pOutFile);
      pOutFile = NULL;

      fclose(pInFile);
      pInFile = NULL;
      fprintf(stderr, "error opening output file\n");
      return 100;
   }

   pOutData = (unsigned char*)malloc(BLOCK_SIZE * 2);
   if (!pOutData) {
      free(pInBlock);
      pInBlock = NULL;

      fclose(pOutFile);
      pOutFile = NULL;

      fclose(pInFile);
      pInFile = NULL;
      fprintf(stderr, "error opening output file\n");
      return 100;
   }

   pCompareData = (unsigned char*)malloc(BLOCK_SIZE);
   if (!pCompareData) {
      free(pOutData);
      pOutData = NULL;

      free(pInBlock);
      pInBlock = NULL;

      fclose(pOutFile);
      pOutFile = NULL;

      fclose(pInFile);
      pInFile = NULL;
      fprintf(stderr, "error opening output file\n");
      return 100;
   }

   int nDictionaryDataSize = 0;
   if (pszDictionaryFilename) {
      FILE *pDictionaryFile = fopen(pszDictionaryFilename, "rb");
      if (!pDictionaryFile) {
         free(pCompareData);
         pCompareData = NULL;

         free(pOutData);
         pOutData = NULL;

         free(pInBlock);
         pInBlock = NULL;

         fclose(pOutFile);
         pOutFile = NULL;

         fclose(pInFile);
         pInFile = NULL;

         fprintf(stderr, "error opening dictionary file\n");
         return 100;
      }

      nDictionaryDataSize = (int)fread(pOutData + BLOCK_SIZE, 1, BLOCK_SIZE - 1, pDictionaryFile);
      if (nDictionaryDataSize < 0)
         nDictionaryDataSize = 0;

      fclose(pDictionaryFile);
      pDictionaryFile = NULL;
   }

   if (nOptions & OPT_VERBOSE) {
      nStartTime = lzsa_get_time();
   }

   int nDecompressionError = 0;
   bool bComparisonError = false;
   int nPrevDecompressedSize = 0;

   if (nDictionaryDataSize) {
      nPrevDecompressedSize = nDictionaryDataSize;
   }

   while (!feof(pInFile) && !nDecompressionError && !bComparisonError) {
      unsigned int nBlockSize = 0;
      int nIsUncompressed = 0;

      if (nPrevDecompressedSize != 0) {
         memcpy(pOutData + BLOCK_SIZE - nPrevDecompressedSize, pOutData + BLOCK_SIZE, nPrevDecompressedSize);
      }

      int nBytesToCompare = (int)fread(pCompareData, 1, BLOCK_SIZE, pOutFile);

      if ((nOptions & OPT_RAW) == 0) {
         const int nFrameSize = lzsa_get_frame_size();

         memset(cFrameData, 0, 16);
         if (fread(cFrameData, 1, nFrameSize, pInFile) == nFrameSize) {
            if (lzsa_decode_frame(cFrameData, nFrameSize, &nBlockSize, &nIsUncompressed) < 0) {
               nDecompressionError = 1;
               nBlockSize = 0;
            }
         }
         else {
            nBlockSize = 0;
         }
      }
      else {
         if (nFileSize >= 4)
            nBlockSize = nFileSize - 4;
         nFileSize = 0;
      }

      if (nBlockSize != 0) {
         int nDecompressedSize = 0;

         if ((int)nBlockSize > BLOCK_SIZE) {
            fprintf(stderr, "block size %d > max size %d\n", nBlockSize, BLOCK_SIZE);
            break;
         }
         if (fread(pInBlock, 1, nBlockSize, pInFile) == nBlockSize) {
            if (nIsUncompressed) {
               memcpy(pOutData + BLOCK_SIZE, pInBlock, nBlockSize);
               nDecompressedSize = nBlockSize;
            }
            else {
               unsigned int nBlockOffs = 0;

               nDecompressedSize = lzsa_expand_block(pInBlock, nBlockSize, pOutData, BLOCK_SIZE, BLOCK_SIZE);
               if (nDecompressedSize < 0) {
                  nDecompressionError = nDecompressedSize;
                  break;
               }
            }

            if (nDecompressedSize == nBytesToCompare) {
               nKnownGoodSize = nOriginalSize;

               nOriginalSize += (long long)nDecompressedSize;

               if (memcmp(pOutData + BLOCK_SIZE, pCompareData, nBytesToCompare))
                  bComparisonError = true;
               nPrevDecompressedSize = nDecompressedSize;
               nDecompressedSize = 0;
            }
            else {
               bComparisonError = true;
               break;
            }
         }
         else {
            break;
         }
      }
      else {
         break;
      }
   }

   free(pCompareData);
   pCompareData = NULL;

   free(pOutData);
   pOutData = NULL;

   free(pInBlock);
   pInBlock = NULL;

   fclose(pOutFile);
   pOutFile = NULL;

   fclose(pInFile);
   pInFile = NULL;

   if (nDecompressionError) {
      fprintf(stderr, "decompression error for '%s'\n", pszInFilename);
      return 100;
   }
   else if (bComparisonError) {
      fprintf(stderr, "error comparing compressed file '%s' with original '%s' starting at %lld\n", pszInFilename, pszOutFilename, nKnownGoodSize);
      return 100;
   }
   else {
      if (nOptions & OPT_VERBOSE) {
         nEndTime = lzsa_get_time();
         double fDelta = ((double)(nEndTime - nStartTime)) / 1000000.0;
         double fSpeed = ((double)nOriginalSize / 1048576.0) / fDelta;
         fprintf(stdout, "Compared '%s' in %g seconds, %g Mb/s\n",
            pszInFilename, fDelta, fSpeed);
      }

      return 0;
   }
}

/*---------------------------------------------------------------------------*/

int main(int argc, char **argv) {
   int i;
   const char *pszInFilename = NULL;
   const char *pszOutFilename = NULL;
   const char *pszDictionaryFilename = NULL;
   bool bArgsError = false;
   bool bCommandDefined = false;
   bool bVerifyCompression = false;
   bool bMinMatchDefined = false;
   char cCommand = 'z';
   int nMinMatchSize = MIN_MATCH_SIZE;
   unsigned int nOptions = OPT_FAVOR_RATIO;

   for (i = 1; i < argc; i++) {
      if (!strcmp(argv[i], "-d")) {
         if (!bCommandDefined) {
            bCommandDefined = true;
            cCommand = 'd';
         }
         else
            bArgsError = true;
      }
      else if (!strcmp(argv[i], "-z")) {
         if (!bCommandDefined) {
            bCommandDefined = true;
            cCommand = 'z';
         }
         else
            bArgsError = true;
      }
      else if (!strcmp(argv[i], "-c")) {
         if (!bVerifyCompression) {
            bVerifyCompression = true;
         }
         else
            bArgsError = true;
      }
      else if (!strcmp(argv[i], "-D")) {
         if (!pszDictionaryFilename && (i + 1) < argc) {
            pszDictionaryFilename = argv[i + 1];
            i++;
         }
         else
            bArgsError = true;
      }
      else if (!strncmp(argv[i], "-D", 2)) {
         if (!pszDictionaryFilename) {
            pszDictionaryFilename = argv[i] + 2;
         }
         else
            bArgsError = true;
      }
      else if (!strcmp(argv[i], "-m")) {
         if (!bMinMatchDefined && (i + 1) < argc) {
            char *pEnd = NULL;
            nMinMatchSize = (int)strtol(argv[i + 1], &pEnd, 10);
            if (pEnd && pEnd != argv[i + 1] && (nMinMatchSize >= MIN_MATCH_SIZE && nMinMatchSize < MATCH_RUN_LEN)) {
               i++;
               bMinMatchDefined = true;
               nOptions &= (~OPT_FAVOR_RATIO);
            }
            else {
               bArgsError = true;
            }
         }
         else
            bArgsError = true;
      }
      else if (!strncmp(argv[i], "-m", 2)) {
         if (!bMinMatchDefined) {
            char *pEnd = NULL;
            nMinMatchSize = (int)strtol(argv[i] + 2, &pEnd, 10);
            if (pEnd && pEnd != (argv[i]+2) && (nMinMatchSize >= MIN_MATCH_SIZE && nMinMatchSize < MATCH_RUN_LEN)) {
               bMinMatchDefined = true;
               nOptions &= (~OPT_FAVOR_RATIO);
            }
            else {
               bArgsError = true;
            }
         }
         else
            bArgsError = true;
      }
      else if (!strcmp(argv[i], "--prefer-ratio")) {
         if (!bMinMatchDefined) {
            nMinMatchSize = MIN_MATCH_SIZE;
            bMinMatchDefined = true;
         }
         else
            bArgsError = true;
      }
      else if (!strcmp(argv[i], "--prefer-speed")) {
         if (!bMinMatchDefined) {
            nMinMatchSize = 3;
            nOptions &= (~OPT_FAVOR_RATIO);
            bMinMatchDefined = true;
         }
         else
            bArgsError = true;
      }
      else if (!strcmp(argv[i], "-v")) {
         if ((nOptions & OPT_VERBOSE) == 0) {
            nOptions |= OPT_VERBOSE;
         }
         else
            bArgsError = true;
      }
      else if (!strcmp(argv[i], "-r")) {
         if ((nOptions & OPT_RAW) == 0) {
            nOptions |= OPT_RAW;
         }
         else
            bArgsError = true;
      }
      else {
         if (!pszInFilename)
            pszInFilename = argv[i];
         else {
            if (!pszOutFilename)
               pszOutFilename = argv[i];
            else
               bArgsError = true;
         }
      }
   }

   if (bArgsError || !pszInFilename || !pszOutFilename) {
      fprintf(stderr, "usage: %s [-c] [-d] [-v] [-r] <infile> <outfile>\n", argv[0]);
      fprintf(stderr, "       -c: check resulting stream after compressing\n");
      fprintf(stderr, "       -d: decompress (default: compress)\n");
      fprintf(stderr, "       -v: be verbose\n");
      fprintf(stderr, "       -r: raw block format (max. 64 Kb files)\n");
      fprintf(stderr, "       -D <filename>: use dictionary file\n");
      fprintf(stderr, "       -m <value>: minimum match size (3-14) (default: 3)\n");
      fprintf(stderr, "       --prefer-ratio: favor compression ratio (default)\n");
      fprintf(stderr, "       --prefer-speed: favor decompression speed (same as -m3)\n");
      return 100;
   }

   if (cCommand == 'z') {
      int nResult = lzsa_compress(pszInFilename, pszOutFilename, pszDictionaryFilename, nOptions, nMinMatchSize);
      if (nResult == 0 && bVerifyCompression) {
         nResult = lzsa_compare(pszOutFilename, pszInFilename, pszDictionaryFilename, nOptions);
      }
   }
   else if (cCommand == 'd') {
      return lzsa_decompress(pszInFilename, pszOutFilename, pszDictionaryFilename, nOptions);
   }
   else {
      return 100;
   }
}
