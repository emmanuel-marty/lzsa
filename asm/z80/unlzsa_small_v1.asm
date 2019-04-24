;
;  Size-optimized LZSA decompressor by spke (v.1 23/04/2019, 69 bytes)
;
;  The data must be comressed using the command line compressor by Emmanuel Marty
;  The compression is done as follows:
;
;  lzsa.exe -r <sourcefile> <outfile>
;
;  where option -r asks for the generation of raw (frame-less) data.
;
;  The decompression is done in the standard way:
;
;  ld hl,CompressedData
;  ld de,WhereToDecompress
;  call DecompressLZSA
;
;  Of course, LZSA compression algorithm is (c) 2019 Emmanuel Marty,
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

@DecompressLZSA:
		ld b,0

		; first a byte token "O|LLL|MMMM" is read from the stream,
		; where LLL is the number of literals and MMMM is
		; a length of the match that follows after the literals
ReadToken:	ld a,(hl) : exa : ld a,(hl) : inc hl
		and #70 : jr z,NoLiterals

		rrca : rrca : rrca : rrca					; LLL<7 means 0..6 literals...
		cp #07 : call z,ReadLongBA					; LLL=7 means 7+ literals...

		ld c,a : ldir

		; next we read the low byte of the -offset
NoLiterals:	push de : ld e,(hl) : inc hl : ld d,#FF
		; the top bit of token is set if
		; the offset contains the high byte as well
		exa : or a : jp p,ShortOffset

LongOffset:	ld d,(hl) : inc hl

		; last but not least, the match length is read
ShortOffset:	and #0F : add 3							; MMMM<15 means match lengths 0+3..14+3
		cp 15+3 : call z,ReadLongBA					; MMMM=15 means lengths 14+3+
		ld c,a

		ex (sp),hl : push hl						; BC = len, DE = -offset, HL = dest, SP ->[dest,src]
		add hl,de : pop de						; BC = len, DE = dest, HL = dest+(-offset), SP->[src]
		ldir : pop hl							; BC = 0, DE = dest, HL = src
		jr ReadToken

		; a standard routine to read extended codes
		; into registers B (higher byte) and A (lower byte).
ReadLongBA:	add (hl) : inc hl : ret nc

		; the codes are designed to overflow;
		; the overflow value 1 means read 1 extra byte
		; and overflow value 0 means read 2 extra bytes
.code1:		ld b,a : ld a,(hl) : inc hl : ret nz
.code0:		ld c,a : ld b,(hl) : inc hl

		; the two-byte match length equal to zero
		; designates the end-of-data marker
		or b : ld a,c : ret nz
		pop de : pop de : ret

