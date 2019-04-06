;
;  Speed-optimized LZSA decompressor by spke (v.1 03-05/04/2019, 122 bytes)
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
		ld b,0 : jr ReadToken

MoreLiterals:	; there are three possible situations here
		; 1) a byte 0..253 is added to LLL and that is it or
		; 2) a byte 254 is followed by another byte to add or
		; 3) a byte 255 is followed by a word to be used
		ld a,7 : add (hl) : inc hl : jp nc,CopyLiterals

.Overflow	; we get here if the literals length byte plus 7 is greater than 255
		inc b : cp 5 : jr c,CopyLiterals : jr nz,.Code255		; 5 is 7+254 modulo 256
.Code254	add (hl) : inc hl : jr nc,CopyLiterals : inc b : jr CopyLiterals
.Code255	ld c,(hl) : inc hl : ld b,(hl) : inc hl : jr CopyLiterals.UseC

		; placed here this saves a JP per iteration
CopyMatchNC	scf								; flag C for SBC HL,DE below must be set!!!
CopyMatch:	ld c,a
.UseC		ex (sp),hl : push hl						; BC = len, DE = offset, HL = dest, SP ->[dest,src]
		sbc hl,de : pop de						; BC = len, DE = dest, HL = dest-offset, SP->[src]
		ldir : pop hl							; BC = 0, DE = dest, HL = src
	
ReadToken:	; first a byte token "O|LLL|MMMM" is read from the stream,
		; where LLL is the number of literals and MMMM is
		; a length of the match that follows after the literals
		ld a,(hl) : exa : ld a,(hl) : inc hl				; token is read twice to be re-used later
		and #70 : jr z,NoLiterals

		cp #70 : jr z,MoreLiterals					; LLL=7 means 7+ literals...
		rrca : rrca : rrca : rrca					; LLL<7 means 0..6 literals...

CopyLiterals:	ld c,a
.UseC		ldir

NoLiterals:	; next we read the first byte of the offset
		push de : ld e,(hl) : inc hl
		; the top bit of token is set if the offset contains two bytes
		exa : and #8F : jp m,LongOffset

ShortOffset:	ld d,b								; we keep B=0 for situations like this

		; short matches have length 0+3..14+3
ReadMatchLen:	add 3 : cp 15+3 : jp c,CopyMatch

		; MMMM=15 indicates a multi-byte number of literals
		; there are three possible situations here
		; 1) a byte 0..253 is added to MMMM and that is it or
		; 2) a byte 254 is followed by another byte to add or
		; 3) a byte 255 is followed by a word to be used
LongerMatch:	add (hl) : inc hl : jp nc,CopyMatchNC

.Overflow	; we get here if the match length byte plus 15+3 is greater than 255
		inc b : cp 16 : jr c,CopyMatch : jr nz,.Code255			; 16 is 15+3+254 modulo 256
.Code254	add (hl) : inc hl : jr nc,CopyMatchNC : inc b : jr CopyMatch
.Code255	ld c,(hl) : inc hl : ld b,(hl) : inc hl
		; two-byte match length that is equal to zero is the marker for End-of-Data (EOD)
.CheckEOD	ld a,b : or c : jr nz,CopyMatch.UseC
		pop de : ret

LongOffset:	; read second byte of the offset
		ld d,(hl) : inc hl
		add -128+3 : cp 15+3 : jp c,CopyMatch
		add (hl) : inc hl : jp nc,CopyMatchNC
		jr LongerMatch.Overflow


