;
;  Speed-optimized LZSA2 decompressor by spke & uniabis (210 bytes)
;
;  ver.00 by spke for LZSA 1.0.0 (02-07/06/2019, 218 bytes);
;  ver.01 by spke for LZSA 1.0.5 (24/07/2019, added support for backward decompression);
;  ver.02 by spke for LZSA 1.0.6 (27/07/2019, fixed a bug in the backward decompressor);
;  ver.03 by uniabis (30/07/2019, 213(-5) bytes, +3.8% speed and support for Hitachi HD64180);
;  ver.04 by spke for LZSA 1.0.7 (01/08/2019, 214(+1) bytes, +0.2% speed and small re-organization of macros);
;  ver.05 by spke (27/08/2019, 216(+2) bytes, +1.1% speed);
;  ver.06 by spke for LZSA 1.1.0 (26/09/2019, added full revision history);
;  ver.07 by spke for LZSA 1.1.1 (10/10/2019, +0.2% speed and an option for unrolled copying of long matches);
;  ver.08 by spke (07-08/04/2022, 210(-6) bytes, +1.7% speed, using self-modifying code by default)
;
;  The data must be compressed using the command line compressor by Emmanuel Marty
;  The compression is done as follows:
;
;  lzsa.exe -f2 -r <sourcefile> <outfile>
;
;  where option -r asks for the generation of raw (frame-less) data.
;
;  The decompression is done in the standard way:
;
;  ld hl,FirstByteOfCompressedData
;  ld de,FirstByteOfMemoryForDecompressedData
;  call DecompressLZSA2
;
;  Backward compression is also supported; you can compress files backward using:
;
;  lzsa.exe -f2 -r -b <sourcefile> <outfile>
;
;  and decompress the resulting files using:
;
;  ld hl,LastByteOfCompressedData
;  ld de,LastByteOfMemoryForDecompressedData
;  call DecompressLZSA2
;
;  (do not forget to uncomment the BACKWARD_DECOMPRESS option in the decompressor).
;
;  Of course, LZSA2 compression algorithms are (c) 2019 Emmanuel Marty,
;  see https://github.com/emmanuel-marty/lzsa for more information
;
;  Drop me an email if you have any comments/ideas/suggestions: zxintrospec@gmail.com
;
;  This software is provided 'as-is', without any express or implied
;  warranty.  In no event will the authors be held liable for any damages
;  arising from the use of this software.
;
;  Permission is granted to anyone to use this software for any purpose,
;  including commercial applications, and to alter it and redistribute it
;  freely, subject to the following restrictions:
;
;  1. The origin of this software must not be misrepresented; you must not
;     claim that you wrote the original software. If you use this software
;     in a product, an acknowledgment in the product documentation would be
;     appreciated but is not required.
;  2. Altered source versions must be plainly marked as such, and must not be
;     misrepresented as being the original software.
;  3. This notice may not be removed or altered from any source distribution.

;	DEFINE	UNROLL_LONG_MATCHES						; uncomment for faster decompression of very compressible data (+38 bytes)
;	DEFINE	BACKWARD_DECOMPRESS						; uncomment for data compressed with option -b (+5 bytes, -3.2% speed)

	IFNDEF	BACKWARD_DECOMPRESS

		MACRO NEXT_HL
		inc hl
		ENDM

		MACRO ADD_OFFSET
		add hl,de
		ENDM

		MACRO COPY1
		ldi
		ENDM

		MACRO COPYBC
		ldir
		ENDM

	ELSE

		MACRO NEXT_HL
		dec hl
		ENDM

		MACRO ADD_OFFSET
		; HL = DE - HL
		ld a,e : sub l : ld l,a
		ld a,d : sbc h : ld h,a						; 6*4 = 24t / 6 bytes
		ENDM

		MACRO COPY1
		ldd
		ENDM

		MACRO COPYBC
		lddr
		ENDM

	ENDIF

@DecompressLZSA2:
		; A' stores next nibble as %1111.... or assumed to contain trash
		; B is assumed to be 0 in many places
		ld b,0 : scf : exa : jr ReadToken




ManyLiterals:	ld a,18 : add (hl) : NEXT_HL : jr nc,CopyMoreLiterals
		ld c,(hl) : NEXT_HL
		ld a,b : ld b,(hl)
		jr ReadToken.NEXTHLuseBC




MoreLiterals:	ld b,(hl) : NEXT_HL
		scf : exa : jr nc,.noUpdate

			; nibbles are read left-to-right; spare nibbles are kept in AF'
			; and flag NC indicates that a nibble is available
			ld a,(hl) : or a : exa
			ld a,(hl) : NEXT_HL
			rrca : rrca : rrca : rrca

.noUpdate	or #F0
		;sub #F0-3 : cp 15+3 : jr z,ManyLiterals
		inc a : jr z,ManyLiterals : sub #F0-3+1

CopyMoreLiterals:	ld c,a : ld a,b : ld b,0
			COPY1
			COPY1
			COPYBC

		or a : jp p,CASE0xx

		cp %11000000 : jr c,CASE10x

		; "111": repeated offset
CASE11x		cp %11100000 : jr nc,MatchLen

		; "110": 16-bit offset
CASE110:	ld b,(hl) : NEXT_HL : jr ReadOffsetC





Literals0011:	jr nz,MoreLiterals

		; if "LL" of the byte token is equal to 0,
		; there are no literals to copy
NoLiterals:	or (hl) : NEXT_HL
		jp m,CASE1xx

		; short (5 or 9 bit long) offsets
CASE0xx		cp %01000000 : jr c,CASE00x

			; "01x": the case of the 9-bit offset
CASE01x:		dec b : cp %01100000 : rl b

ReadOffsetC		ld c,(hl) : NEXT_HL

SaveOffset		ld (CopyMatch.PrevOffset),bc : ld b,0

MatchLen		inc a : and %00000111 : jr z,LongerMatch : inc a

CopyMatch:		ld c,a
.useC			push hl
.PrevOffset		EQU $+1 : ld hl,0
			ADD_OFFSET
			COPY1
			COPYBC
.popSrc			pop hl

		; compressed data stream contains records
		; each record begins with the byte token "XYZ|LL|MMM"
ReadToken:	ld a,(hl) : and %00011000 : jp pe,Literals0011		; process the cases 00 and 11 separately

			rrca : rrca : rrca

			ld c,a : ld a,(hl)					; token is re-read for further processing
.NEXTHLuseBC		NEXT_HL
			COPYBC

		; the token and literals are followed by the offset
		or a : jp p,CASE0xx

CASE1xx		cp %11000000 : jr nc,CASE11x

		; "10x": the case of the 13-bit offset
CASE10x:	ld c,a : exa : jr nc,.noUpdate

			ld a,(hl) : or a : exa
			ld a,(hl) : NEXT_HL
			rrca : rrca : rrca : rrca

.noUpdate	or #F0 : ld b,a : ld a,c
		cp %10100000 : dec b : rl b : jr ReadOffsetC


		

		; "00x": the case of the 5-bit offset
CASE00x:	ld b,a : exa : jr nc,.noUpdate

			ld a,(hl) : or a : exa
			ld a,(hl) : NEXT_HL
			rrca : rrca : rrca : rrca

.noUpdate	or #F0 : ld c,a : ld a,b
		cp %00100000 : rl c
		ld b,#FF : jr SaveOffset





LongerMatch:	scf : exa : jr nc,.noUpdate

			ld a,(hl) : or a : exa
			ld a,(hl) : NEXT_HL
			rrca : rrca : rrca : rrca

.noUpdate	or #F0 : sub #F0-9 : cp 15+9 : jr c,CopyMatch

	IFNDEF	UNROLL_LONG_MATCHES

LongMatch:	add (hl) : NEXT_HL : jr nc,CopyMatch
		ld c,(hl) : NEXT_HL
		ld b,(hl) : NEXT_HL : jr nz,CopyMatch.useC
		ret

	ELSE

LongMatch:	add (hl) : NEXT_HL : jr c,VeryLongMatch

		ld c,a
.useC		push hl
		ld hl,(CopyMatch.PrevOffset)
		ADD_OFFSET

		; this is an unrolled equivalent of LDIR
		xor a : sub c
		and 8-1 : add a
		ld (.jrOffset),a : jr nz,$+2
.jrOffset	EQU $-1
.fastLDIR	DUP 8
		COPY1
		EDUP
		jp pe,.fastLDIR
		jp CopyMatch.popSrc

VeryLongMatch:	ld c,(hl) : NEXT_HL
		ld b,(hl) : NEXT_HL : jr nz,LongMatch.useC
		ret

	ENDIF





