/*
 * lzsa.c - command line compression utility for the LZSA format
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
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <sys/timeb.h>
#else
#include <sys/time.h>
#endif
#include "lib.h"

#define OPT_VERBOSE     1
#define OPT_RAW         2
#define OPT_FAVOR_RATIO 4

#define TOOL_VERSION "0.6.0"

/*---------------------------------------------------------------------------*/

static long long do_get_time() {
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

static void compression_progress(long long nOriginalSize, long long nCompressedSize) {
   if (nOriginalSize >= 1024 * 1024) {
      fprintf(stdout, "\r%lld => %lld (%g %%)     \b\b\b\b\b", nOriginalSize, nCompressedSize, (double)(nCompressedSize * 100.0 / nOriginalSize));
      fflush(stdout);
   }
}

static int do_compress(const char *pszInFilename, const char *pszOutFilename, const char *pszDictionaryFilename, const unsigned int nOptions, const int nMinMatchSize, const int nFormatVersion) {
   long long nStartTime = 0LL, nEndTime = 0LL;
   long long nOriginalSize = 0LL, nCompressedSize = 0LL;
   int nCommandCount = 0;
   int nFlags;
   lzsa_status_t nStatus;

   nFlags = 0;
   if (nOptions & OPT_FAVOR_RATIO)
      nFlags |= LZSA_FLAG_FAVOR_RATIO;
   if (nOptions & OPT_RAW)
      nFlags |= LZSA_FLAG_RAW_BLOCK;

   if (nOptions & OPT_VERBOSE) {
      nStartTime = do_get_time();
   }

   nStatus = lsza_compress_file(pszInFilename, pszOutFilename, pszDictionaryFilename, nFlags, nMinMatchSize, nFormatVersion, compression_progress, &nOriginalSize, &nCompressedSize, &nCommandCount);

   if ((nOptions & OPT_VERBOSE)) {
      nEndTime = do_get_time();
   }

   switch (nStatus) {
      case LZSA_ERROR_SRC: fprintf(stderr, "error reading '%s'\n", pszInFilename); break;
      case LZSA_ERROR_DST: fprintf(stderr, "error writing '%s'\n", pszOutFilename); break;
      case LZSA_ERROR_DICTIONARY: fprintf(stderr, "error reading dictionary '%s'\n", pszDictionaryFilename); break;
      case LZSA_ERROR_MEMORY: fprintf(stderr, "out of memory\n"); break;
      case LZSA_ERROR_COMPRESSION: fprintf(stderr, "internal compression error\n"); break;
      case LZSA_ERROR_RAW_TOOLARGE: fprintf(stderr, "error: raw blocks can only be used with files <= 64 Kb\n"); break;
      case LZSA_ERROR_RAW_UNCOMPRESSED: fprintf(stderr, "error: data is incompressible, raw blocks only support compressed data\n"); break;
      case LZSA_OK: break;
      default: fprintf(stderr, "unknown compression error %d\n", nStatus); break;
   }

   if (nStatus)
      return 100;

   if ((nOptions & OPT_VERBOSE)) {
      double fDelta = ((double)(nEndTime - nStartTime)) / 1000000.0;
      double fSpeed = ((double)nOriginalSize / 1048576.0) / fDelta;
      fprintf(stdout, "\rCompressed '%s' in %g seconds, %.02g Mb/s, %d tokens (%g bytes/token), %lld into %lld bytes ==> %g %%\n",
         pszInFilename, fDelta, fSpeed, nCommandCount, (double)nOriginalSize / (double)nCommandCount,
         nOriginalSize, nCompressedSize, (double)(nCompressedSize * 100.0 / nOriginalSize));
   }

   return 0;
}

/*---------------------------------------------------------------------------*/

static int do_decompress(const char *pszInFilename, const char *pszOutFilename, const char *pszDictionaryFilename, const unsigned int nOptions, int nFormatVersion) {
   long long nStartTime = 0LL, nEndTime = 0LL;
   long long nOriginalSize = 0LL, nCompressedSize = 0LL;
   lzsa_status_t nStatus;
   int nFlags;

   nFlags = 0;
   if (nOptions & OPT_RAW)
      nFlags |= LZSA_FLAG_RAW_BLOCK;

   if (nOptions & OPT_VERBOSE) {
      nStartTime = do_get_time();
   }

   nStatus = lzsa_decompress_file(pszInFilename, pszOutFilename, pszDictionaryFilename, nFlags, nFormatVersion, &nOriginalSize, &nCompressedSize);

   switch (nStatus) {
   case LZSA_ERROR_SRC: fprintf(stderr, "error reading '%s'\n", pszInFilename); break;
   case LZSA_ERROR_DST: fprintf(stderr, "error writing '%s'\n", pszOutFilename); break;
   case LZSA_ERROR_DICTIONARY: fprintf(stderr, "error reading dictionary '%s'\n", pszDictionaryFilename); break;
   case LZSA_ERROR_MEMORY: fprintf(stderr, "out of memory\n"); break;
   case LZSA_ERROR_DECOMPRESSION: fprintf(stderr, "internal decompression error\n"); break;
   case LZSA_ERROR_FORMAT: fprintf(stderr, "invalid magic number or format version in input file\n"); break;
   case LZSA_OK: break;
   default: fprintf(stderr, "unknown decompression error %d\n", nStatus); break;
   }

   if (nStatus) {
      fprintf(stderr, "decompression error for '%s'\n", pszInFilename);
      return 100;
   }
   else {
      if (nOptions & OPT_VERBOSE) {
         nEndTime = do_get_time();
         double fDelta = ((double)(nEndTime - nStartTime)) / 1000000.0;
         double fSpeed = ((double)nOriginalSize / 1048576.0) / fDelta;
         fprintf(stdout, "Decompressed '%s' in %g seconds, %g Mb/s\n",
            pszInFilename, fDelta, fSpeed);
      }

      return 0;
   }
}

/*---------------------------------------------------------------------------*/

typedef struct {
   FILE *f;
   void *pCompareDataBuf;
   size_t nCompareDataSize;
} compare_stream_t;

void comparestream_close(lzsa_stream_t *stream) {
   if (stream->obj) {
      compare_stream_t *pCompareStream = (compare_stream_t *)stream->obj;
      if (pCompareStream->pCompareDataBuf) {
         free(pCompareStream->pCompareDataBuf);
         pCompareStream->pCompareDataBuf = NULL;
      }
      
      fclose(pCompareStream->f);
      free(pCompareStream);

      stream->obj = NULL;
      stream->read = NULL;
      stream->write = NULL;
      stream->eof = NULL;
      stream->close = NULL;
   }
}

size_t comparestream_read(lzsa_stream_t *stream, void *ptr, size_t size) {
   return 0;
}

size_t comparestream_write(lzsa_stream_t *stream, void *ptr, size_t size) {
   compare_stream_t *pCompareStream = (compare_stream_t *)stream->obj;

   if (!pCompareStream->pCompareDataBuf || pCompareStream->nCompareDataSize < size) {
      pCompareStream->nCompareDataSize = size;
      pCompareStream->pCompareDataBuf = realloc(pCompareStream->pCompareDataBuf, pCompareStream->nCompareDataSize);
      if (!pCompareStream->pCompareDataBuf)
         return 0;
   }

   size_t nReadBytes = fread(pCompareStream->pCompareDataBuf, 1, size, pCompareStream->f);
   if (nReadBytes != size) {
      return 0;
   }

   if (memcmp(ptr, pCompareStream->pCompareDataBuf, size)) {
      return 0;
   }

   return size;
}

int comparestream_eof(lzsa_stream_t *stream) {
   compare_stream_t *pCompareStream = (compare_stream_t *)stream->obj;
   return feof(pCompareStream->f);
}

int comparestream_open(lzsa_stream_t *stream, const char *pszCompareFilename, const char *pszMode) {
   compare_stream_t *pCompareStream;

   pCompareStream = (compare_stream_t*)malloc(sizeof(compare_stream_t));
   if (!pCompareStream)
      return -1;

   pCompareStream->pCompareDataBuf = NULL;
   pCompareStream->nCompareDataSize = 0;
   pCompareStream->f = (void*)fopen(pszCompareFilename, pszMode);

   if (pCompareStream->f) {
      stream->obj = pCompareStream;
      stream->read = comparestream_read;
      stream->write = comparestream_write;
      stream->eof = comparestream_eof;
      stream->close = comparestream_close;
      return 0;
   }
   else
      return -1;
}

static int do_compare(const char *pszInFilename, const char *pszOutFilename, const char *pszDictionaryFilename, const unsigned int nOptions, int nFormatVersion) {
   lzsa_stream_t inStream, compareStream;
   long long nStartTime = 0LL, nEndTime = 0LL;
   long long nOriginalSize = 0LL;
   long long nCompressedSize = 0LL;
   void *pDictionaryData = NULL;
   int nDictionaryDataSize = 0;
   lzsa_status_t nStatus;
   int nFlags;

   if (lzsa_filestream_open(&inStream, pszInFilename, "rb") < 0) {
      fprintf(stderr, "error opening compressed input file\n");
      return 100;
   }

   if (comparestream_open(&compareStream, pszOutFilename, "rb") < 0) {
      fprintf(stderr, "error opening original uncompressed file\n");
      inStream.close(&inStream);
      return 100;
   }

   nStatus = lzsa_dictionary_load(pszDictionaryFilename, &pDictionaryData, &nDictionaryDataSize);
   if (nStatus) {
      compareStream.close(&compareStream);
      inStream.close(&inStream);
      fprintf(stderr, "error reading dictionary '%s'\n", pszDictionaryFilename);
      return 100;
   }

   nFlags = 0;
   if (nOptions & OPT_RAW)
      nFlags |= LZSA_FLAG_RAW_BLOCK;

   if (nOptions & OPT_VERBOSE) {
      nStartTime = do_get_time();
   }

   nStatus = lzsa_decompress_stream(&inStream, &compareStream, pDictionaryData, nDictionaryDataSize, nFlags, nFormatVersion, &nOriginalSize, &nCompressedSize);

   switch (nStatus) {
   case LZSA_ERROR_SRC: fprintf(stderr, "error reading '%s'\n", pszInFilename); break;
   case LZSA_ERROR_DST: fprintf(stderr, "error comparing compressed file '%s' with original '%s'\n", pszInFilename, pszOutFilename); break;
   case LZSA_ERROR_MEMORY: fprintf(stderr, "out of memory\n"); break;
   case LZSA_ERROR_DECOMPRESSION: fprintf(stderr, "internal decompression error\n"); break;
   case LZSA_ERROR_FORMAT: fprintf(stderr, "invalid magic number or format version in input file\n"); break;
   case LZSA_OK: break;
   default: fprintf(stderr, "unknown decompression error %d\n", nStatus); break;
   }

   lzsa_dictionary_free(&pDictionaryData);
   compareStream.close(&compareStream);
   inStream.close(&inStream);

   if (nStatus) {
      return 100;
   }
   else {
      if (nOptions & OPT_VERBOSE) {
         nEndTime = do_get_time();
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
   bool bFormatVersionDefined = false;
   char cCommand = 'z';
   int nMinMatchSize = 0;
   unsigned int nOptions = OPT_FAVOR_RATIO;
   int nFormatVersion = 1;

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
            if (pEnd && pEnd != argv[i + 1] && (nMinMatchSize >= 2 && nMinMatchSize <= 5)) {
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
            if (pEnd && pEnd != (argv[i]+2) && (nMinMatchSize >= 2 && nMinMatchSize <= 5)) {
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
            nMinMatchSize = 0;
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
      else if (!strcmp(argv[i], "-f")) {
         if (!bFormatVersionDefined && (i + 1) < argc) {
            char *pEnd = NULL;
            nFormatVersion = (int)strtol(argv[i + 1], &pEnd, 10);
            if (pEnd && pEnd != argv[i + 1] && (nFormatVersion >= 1 && nFormatVersion <= 2)) {
               i++;
               bFormatVersionDefined = true;
            }
            else {
               bArgsError = true;
            }
         }
         else
            bArgsError = true;
      }
      else if (!strncmp(argv[i], "-f", 2)) {
         if (!bFormatVersionDefined) {
            char *pEnd = NULL;
            nFormatVersion = (int)strtol(argv[i] + 2, &pEnd, 10);
            if (pEnd && pEnd != (argv[i] + 2) && (nFormatVersion >= 1 && nFormatVersion <= 2)) {
               bFormatVersionDefined = true;
            }
            else {
               bArgsError = true;
            }
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
      fprintf(stderr, "lzsa command-line tool v" TOOL_VERSION " by Emmanuel Marty and spke\n");
      fprintf(stderr, "usage: %s [-c] [-d] [-v] [-r] <infile> <outfile>\n", argv[0]);
      fprintf(stderr, "       -c: check resulting stream after compressing\n");
      fprintf(stderr, "       -d: decompress (default: compress)\n");
      fprintf(stderr, "       -v: be verbose\n");
      fprintf(stderr, "       -f <value>: LZSA compression format (1-2)\n");
      fprintf(stderr, "       -r: raw block format (max. 64 Kb files)\n");
      fprintf(stderr, "       -D <filename>: use dictionary file\n");
      fprintf(stderr, "       -m <value>: minimum match size (3-5) (default: 3)\n");
      fprintf(stderr, "       --prefer-ratio: favor compression ratio (default)\n");
      fprintf(stderr, "       --prefer-speed: favor decompression speed (same as -m3)\n");
      return 100;
   }

   if (cCommand == 'z') {
      int nResult = do_compress(pszInFilename, pszOutFilename, pszDictionaryFilename, nOptions, nMinMatchSize, nFormatVersion);
      if (nResult == 0 && bVerifyCompression) {
         nResult = do_compare(pszOutFilename, pszInFilename, pszDictionaryFilename, nOptions, nFormatVersion);
      }
   }
   else if (cCommand == 'd') {
      return do_decompress(pszInFilename, pszOutFilename, pszDictionaryFilename, nOptions, nFormatVersion);
   }
   else {
      return 100;
   }
}
