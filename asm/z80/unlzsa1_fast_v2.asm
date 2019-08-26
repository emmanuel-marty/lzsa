;
;  Speed-optimized LZSA1 decompressor by spke (v.2 03/04/2019-27/08/2019; 111 bytes);
;  with improvements by uniabis (30/07/2019,22/08/2019; -3 bytes, +3% speed).
;
;  The data must be compressed using the command line compressor by Emmanuel Marty
;  The compression is done as follows:
;
;  lzsa.exe -f1 -r <sourcefile> <outfile>
;
;  where option -r asks for the generation of raw (frame-less) data.
;
;  The decompression is done in the standard way:
;
;  ld hl,FirstByteOfCompressedData
;  ld de,FirstByteOfMemoryForDecompressedData
;  call DecompressLZSA1
;
;  Backward compression is also supported; you can compress files backward using:
;
;  lzsa.exe -f1 -r -b <sourcefile> <outfile>
;
;  and decompress the resulting files using:
;
;  ld hl,LastByteOfCompressedData
;  ld de,LastByteOfMemoryForDecompressedData
;  call DecompressLZSA1
;
;  (do not forget to uncomment the BACKWARD_DECOMPRESS option in the decompressor).
;
;  Of course, LZSA compression algorithms are (c) 2019 Emmanuel Marty,
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

	IFNDEF	BACKWARD_DECOMPRESS

		MACRO NEXT_HL
		inc hl
		ENDM

		MACRO ADD_OFFSET
		ex de,hl : add hl,de
		ENDM

		MACRO COPY_MATCH
		ldi : ldi : ldir
		ENDM

		MACRO BLOCKCOPY
		ldir
		ENDM

	ELSE

		MACRO NEXT_HL
		dec hl
		ENDM

		MACRO ADD_OFFSET
		ex de,hl : ld a,e : sub l : ld l,a
		ld a,d : sbc h : ld h,a						; 4*4+3*4 = 28t / 7 bytes
		ENDM

		MACRO COPY_MATCH
		ldd : ldd : lddr
		ENDM

		MACRO BLOCKCOPY
		lddr
		ENDM

	ENDIF

@DecompressLZSA1:
		ld b,0 : jr ReadToken

NoLiterals:	xor (hl)
		push de : NEXT_HL : ld e,(hl) : jp m,LongOffset

 		; short matches have length 0+3..14+3
ShortOffset:	ld d,#FF : add 3 : cp 15+3 : jr nc,LongerMatch

		; placed here this saves a JP per iteration
CopyMatch:	ld c,a
.UseC		NEXT_HL : ex (sp),hl						; BC = len, DE = offset, HL = dest, SP ->[dest,src]
		ADD_OFFSET							; BC = len, DE = dest, HL = dest-offset, SP->[src]
		COPY_MATCH : pop hl						; BC = 0, DE = dest, HL = src
	
ReadToken:	; first a byte token "O|LLL|MMMM" is read from the stream,
		; where LLL is the number of literals and MMMM is
		; a length of the match that follows after the literals
		ld a,(hl) : and #70 : jr z,NoLiterals

		cp #70 : jr z,MoreLiterals					; LLL=7 means 7+ literals...
		rrca : rrca : rrca : rrca					; LLL<7 means 0..6 literals...

		ld c,a : ld a,(hl)
		NEXT_HL : BLOCKCOPY

		; next we read the first byte of the offset
		push de : ld e,(hl)
		; the top bit of token is set if the offset contains two bytes
		and #8F : jp p,ShortOffset

LongOffset:	; read second byte of the offset
		NEXT_HL : ld d,(hl)
		add -128+3 : cp 15+3 : jp c,CopyMatch

		; MMMM=15 indicates a multi-byte number of literals
LongerMatch:	NEXT_HL : add (hl) : jr nc,CopyMatch

		; the codes are designed to overflow;
		; the overflow value 1 means read 1 extra byte
		; and overflow value 0 means read 2 extra bytes
.code1		ld b,a : NEXT_HL : ld c,(hl) : jr nz,CopyMatch.UseC
.code0		NEXT_HL : ld b,(hl)

		; the two-byte match length equal to zero
		; designates the end-of-data marker
		ld a,b : or c : jr nz,CopyMatch.UseC
		pop de : ret

MoreLiterals:	; there are three possible situations here
		xor (hl) : exa
		ld a,7 : NEXT_HL : add (hl) : jr c,ManyLiterals

CopyLiterals:	ld c,a
.UseC		NEXT_HL : BLOCKCOPY

		push de : ld e,(hl)
		exa : jp p,ShortOffset : jr LongOffset

ManyLiterals:
.code1		ld b,a : NEXT_HL : ld c,(hl) : jr nz,CopyLiterals.UseC
.code0		NEXT_HL : ld b,(hl) : jr CopyLiterals.UseC


