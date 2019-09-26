;
;  Size-optimized LZSA2 decompressor by spke & uniabis (140 bytes)
;
;  ver.00 by spke for LZSA 1.0.0 (02-09/06/2019, 145 bytes);
;  ver.01 by spke for LZSA 1.0.5 (24/07/2019, added support for backward decompression);
;  ver.02 by uniabis (30/07/2019, 144(-1) bytes, +3.3% speed and support for Hitachi HD64180);
;  ver.03 by spke for LZSA 1.0.7 (01/08/2019, 140(-4) bytes, -1.4% speed and small re-organization of macros);
;  ver.04 by spke for LZSA 1.1.0 (26/09/2019, removed usage of IY, added full revision history)
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
;

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

	ELSE

		MACRO NEXT_HL
		dec hl
		ENDM

		MACRO ADD_OFFSET
		push hl : or a : sbc hl,de : pop de				; 11+4+15+10 = 40t / 5 bytes
		ENDM

		MACRO BLOCKCOPY
		lddr
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
		xor a : ld b,a : exa : jr ReadToken

CASE00x:	call ReadNibble
		ld e,a : ld a,c
		cp %00100000 : rl e : jr SaveOffset

CASE0xx		ld d,#FF : cp %01000000 : jr c,CASE00x

CASE01x:	cp %01100000 : rl d

OffsetReadE:	ld e,(hl) : NEXT_HL
		
SaveOffset:	LD_IX_DE

MatchLen:	and %00000111 : add 2 : cp 9 : call z,ExtendedCode

CopyMatch:	ld c,a
		ex (sp),hl							; BC = len, DE = -offset, HL = dest, SP -> [src]
		ADD_OFFSET							; BC = len, DE = dest, HL = dest+(-offset), SP -> [src]
		BLOCKCOPY							; BC = 0, DE = dest
		pop hl								; HL = src

ReadToken:	ld a,(hl) : NEXT_HL : push af
		and %00011000 : jr z,NoLiterals

		rrca : rrca : rrca
		call pe,ExtendedCode

		ld c,a
		BLOCKCOPY

NoLiterals:	pop af : push de
		or a : jp p,CASE0xx

CASE1xx		cp %11000000 : jr nc,CASE11x

CASE10x:	call ReadNibble
		ld d,a : ld a,c
		cp %10100000 : rl d
		dec d : dec d : DB #CA ; jr OffsetReadE				; #CA is JP Z,.. to skip all commands in CASE110 before jr OffsetReadE

CASE110:	ld d,(hl) : NEXT_HL : jr OffsetReadE

CASE11x		cp %11100000 : jr c,CASE110

CASE111:	LD_DE_IX : jr MatchLen

ExtendedCode:	call ReadNibble : inc a : jr z,ExtraByte
		sub #F0+1 : add c : ret
ExtraByte	ld a,15 : add c : add (hl) : NEXT_HL : ret nc
		ld a,(hl) : NEXT_HL
		ld b,(hl) : NEXT_HL : ret nz
		pop de : pop de							; RET is not needed, because RET from ReadNibble is sufficient

ReadNibble:	ld c,a : xor a : exa : ret m
UpdateNibble	ld a,(hl) : or #F0 : exa
		ld a,(hl) : NEXT_HL : or #0F
		rrca : rrca : rrca : rrca : ret
















