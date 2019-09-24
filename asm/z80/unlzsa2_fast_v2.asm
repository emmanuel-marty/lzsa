;
;  Speed-optimized LZSA2 decompressor by spke (v.2 02-27/08/2019, 216 bytes);
;  with improvements by uniabis (30/07/2019, -5 bytes, +3% speed and support for Hitachi HD64180).
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

;	DEFINE	BACKWARD_DECOMPRESS						; uncomment for data compressed with option -b
;	DEFINE	HD64180								; uncomment for systems using Hitachi HD64180

	IFNDEF	BACKWARD_DECOMPRESS

		MACRO NEXT_HL
		inc hl
		ENDM

		MACRO ADD_OFFSET
		ex de,hl : add hl,de
		ENDM

		MACRO BLOCKCOPY
		ldir
		ENDM

		MACRO COPY_MATCH
		ldi : ldir
		ENDM

	ELSE

		MACRO NEXT_HL
		dec hl
		ENDM

		MACRO ADD_OFFSET
		ex de,hl : ld a,e : sub l : ld l,a
		ld a,d : sbc h : ld h,a						; 4*4+3*4 = 28t / 7 bytes
		ENDM

		MACRO BLOCKCOPY
		lddr
		ENDM

		MACRO COPY_MATCH
		ldd : lddr
		ENDM

	ENDIF

	IFNDEF	HD64180

		MACRO LD_IX_DE
		ld ixl,e : ld ixh,d
		ENDM

		MACRO LD_DE_IX
		ld e,ixl : ld d,ixh
		ENDM

	ELSE

		MACRO LD_IX_DE
		push de : pop ix
		ENDM

		MACRO LD_DE_IX
		push ix : pop de
		ENDM

	ENDIF

@DecompressLZSA2:
		; A' stores next nibble as %1111.... or assumed to contain trash
		; B is assumed to be 0
		ld b,0 : scf : exa : jr ReadToken




ManyLiterals:	ld a,18 : add (hl) : NEXT_HL : jr nc,CopyLiterals
		ld c,(hl) : NEXT_HL
		ld a,b : ld b,(hl)
		jr ReadToken.NEXTHLuseBC




MoreLiterals:	ld b,(hl) : NEXT_HL
		scf : exa : jr nc,.noUpdate

			ld a,(hl) : or #F0 : exa
			ld a,(hl) : NEXT_HL : or #0F
			rrca : rrca : rrca : rrca

.noUpdate	;sub #F0-3 : cp 15+3 : jr z,ManyLiterals
		inc a : jr z,ManyLiterals : sub #F0-3+1

CopyLiterals:	ld c,a : ld a,b : ld b,0
		BLOCKCOPY
		push de : or a : jp p,CASE0xx ;: jr CASE1xx

		cp %11000000 : jr c,CASE10x

CASE11x		cp %11100000 : jr c,CASE110

		; "111": repeated offset
CASE111:	LD_DE_IX : jr MatchLen




Literals0011:	jr nz,MoreLiterals

		; if "LL" of the byte token is equal to 0,
		; there are no literals to copy
NoLiterals:	or (hl) : NEXT_HL
		push de : jp m,CASE1xx

		; short (5 or 9 bit long) offsets
CASE0xx		ld d,#FF : cp %01000000 : jr c,CASE00x

		; "01x": the case of the 9-bit offset
CASE01x:	cp %01100000 : rl d

ReadOffsetE	ld e,(hl) : NEXT_HL

SaveOffset:	LD_IX_DE

MatchLen:	inc a : and %00000111 : jr z,LongerMatch : inc a

CopyMatch:	ld c,a
.useC		ex (sp),hl						; BC = len, DE = offset, HL = dest, SP ->[dest,src]
		ADD_OFFSET						; BC = len, DE = dest, HL = dest-offset, SP->[src]
		COPY_MATCH : pop hl

		; compressed data stream contains records
		; each record begins with the byte token "XYZ|LL|MMM"
ReadToken:	ld a,(hl) : and %00011000 : jp pe,Literals0011		; process the cases 00 and 11 separately

		rrca : rrca : rrca

		ld c,a : ld a,(hl)					; token is re-read for further processing
.NEXTHLuseBC	NEXT_HL
		BLOCKCOPY

		; the token and literals are followed by the offset
		push de : or a : jp p,CASE0xx

CASE1xx		cp %11000000 : jr nc,CASE11x

		; "10x": the case of the 5-bit offset
CASE10x:	ld c,a : exa : jr nc,.noUpdate

			ld a,(hl) : or #F0 : exa
			ld a,(hl) : NEXT_HL : or #0F
			rrca : rrca : rrca : rrca

.noUpdate	ld d,a : ld a,c
		cp %10100000 : rl d
		dec d : dec d : jr ReadOffsetE


		
		; "110": 16-bit offset
CASE110:	ld d,(hl) : NEXT_HL : jr ReadOffsetE




		; "00x": the case of the 5-bit offset
CASE00x:	ld c,a : exa : jr nc,.noUpdate

			ld a,(hl) : or #F0 : exa
			ld a,(hl) : NEXT_HL : or #0F
			rrca : rrca : rrca : rrca

.noUpdate	ld e,a : ld a,c
		cp %00100000 : rl e : jr SaveOffset





LongerMatch:	scf : exa : jr nc,.noUpdate

			ld a,(hl) : or #F0 : exa
			ld a,(hl) : NEXT_HL : or #0F
			rrca : rrca : rrca : rrca

.noUpdate	sub #F0-9 : cp 15+9 : jr c,CopyMatch

LongMatch:	add (hl) : NEXT_HL : jr nc,CopyMatch
		ld c,(hl) : NEXT_HL
		ld b,(hl) : NEXT_HL : jr nz,CopyMatch.useC
		pop de : ret











