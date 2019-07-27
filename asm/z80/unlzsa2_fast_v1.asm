;
;  Speed-optimized LZSA2 decompressor by spke (v.1 02-07/06/2019, 218 bytes)
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
;  Of course, LZSA2 compression algorithm is (c) 2019 Emmanuel Marty,
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

;	DEFINE	BACKWARD_DECOMPRESS

	IFDEF	BACKWARD_DECOMPRESS

		MACRO NEXT_HL
		dec hl
		ENDM

		MACRO ADD_OFFSET
		or a : sbc hl,de
		ENDM

		MACRO BLOCKCOPY
		lddr
		ENDM

	ELSE

		MACRO NEXT_HL
		inc hl
		ENDM

		MACRO ADD_OFFSET
		add hl,de
		ENDM

		MACRO BLOCKCOPY
		ldir
		ENDM

	ENDIF

@DecompressLZSA2:
		; A' stores next nibble as %1111.... or assumed to contain trash
		; B is assumed to be 0
		xor a : ld b,a : exa : jr ReadToken





LongerMatch:	exa : jp m,.noUpdate

			ld a,(hl) : or #F0 : exa
			ld a,(hl) : NEXT_HL : or #0F
			rrca : rrca : rrca : rrca

.noUpdate	sub #F0-9 : cp 15+9 : jr c,CopyMatch
		;inc a : jr z,LongMatch : sub #F0-9+1 : jp CopyMatch

LongMatch:	;ld a,24 : 
		add (hl) : NEXT_HL : jr nc,CopyMatch
		ld c,(hl) : NEXT_HL
		ld b,(hl) : NEXT_HL
		jr nz,CopyMatch.useC
		pop de : ret




ManyLiterals:	ld a,18 : 
		add (hl) : NEXT_HL : jr nc,CopyLiterals
		ld c,(hl) : NEXT_HL
		ld a,b : ld b,(hl) : NEXT_HL
		jr CopyLiterals.useBC




MoreLiterals:	ld b,(hl) : NEXT_HL
		exa : jp m,.noUpdate

			ld a,(hl) : or #F0 : exa
			ld a,(hl) : NEXT_HL : or #0F
			rrca : rrca : rrca : rrca

.noUpdate	;sub #F0-3 : cp 15+3 : jr z,ManyLiterals
		inc a : jr z,ManyLiterals : sub #F0-3+1

CopyLiterals:	ld c,a
.useC		ld a,b : ld b,0
.useBC		BLOCKCOPY
		push de : or a : jp p,CASE0xx : jr CASE1xx





		; if "LL" of the byte token is equal to 0,
		; there are no literals to copy
NoLiterals:	xor (hl) : NEXT_HL
		push de : jp m,CASE1xx

		; short (5 or 9 bit long) offsets
CASE0xx		ld d,#FF : cp %01000000 : jr c,CASE00x

		; "01x": the case of the 9-bit offset
CASE01x:	cp %01100000 : rl d

ReadOffsetE:	ld e,(hl) : NEXT_HL

SaveOffset:	ld ixl,e : ld ixh,d

MatchLen:	inc a : and %00000111 : jr z,LongerMatch : inc a

CopyMatch:	ld c,a
.useC		ex (sp),hl : push hl					; BC = len, DE = offset, HL = dest, SP ->[dest,src]
		ADD_OFFSET : pop de					; BC = len, DE = dest, HL = dest-offset, SP->[src]
		BLOCKCOPY : pop hl

		; compressed data stream contains records
		; each record begins with the byte token "XYZ|LL|MMM"
ReadToken:	ld a,(hl) : and %00011000 : jr z,NoLiterals

		jp pe,MoreLiterals					; 00 has already been processed; this identifies the case of 11
		rrca : rrca : rrca

		ld c,a : ld a,(hl) : NEXT_HL				; token is re-read for further processing
		BLOCKCOPY

		; the token and literals are followed by the offset
		push de : or a : jp p,CASE0xx

CASE1xx		cp %11000000 : jr nc,CASE11x

		; "10x": the case of the 5-bit offset
CASE10x:	ld c,a : xor a
		exa : jp m,.noUpdate

			ld a,(hl) : or #F0 : exa
			ld a,(hl) : NEXT_HL : or #0F
			rrca : rrca : rrca : rrca

.noUpdate	ld d,a : ld a,c
		cp %10100000 : rl d
		dec d : dec d : jr ReadOffsetE

		; "00x": the case of the 5-bit offset
CASE00x:	ld c,a : xor a
		exa : jp m,.noUpdate

			ld a,(hl) : or #F0 : exa
			ld a,(hl) : NEXT_HL : or #0F
			rrca : rrca : rrca : rrca

.noUpdate	ld e,a : ld a,c
		cp %00100000 : rl e : jp SaveOffset

		; two remaining cases
CASE11x		cp %11100000 : jr c,CASE110

		; "111": repeated offset
CASE111:	ld e,ixl : ld d,ixh : jr MatchLen

		; "110": 16-bit offset
CASE110:	ld d,(hl) : NEXT_HL : jr ReadOffsetE













