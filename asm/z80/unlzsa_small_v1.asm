;
;  Size-optimized LZSA decompressor by spke (v.1 19/04/2019, 81 bytes)
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

@DecompressLZSA:
		ld b,0

ReadToken:	; first a byte token "O|LLL|MMMM" is read from the stream,
		; where LLL is the number of literals and MMMM is
		; a length of the match that follows after the literals
		ld a,(hl) : exa : ld a,(hl) : inc hl
		and #70 : jr z,NoLiterals

		rrca : rrca : rrca : rrca					; LLL<7 means 0..6 literals...
		cp #07 : call z,ReadLongBA					; LLL=7 means 7+ literals...

		ld c,a : ldir

		; next we read the first byte of the offset
NoLiterals:	push de : ld e,(hl) : inc hl : ld d,b
		; the top bit of token is set if the offset contains two bytes
		exa : or a : jp p,ShortOffset

LongOffset:	ld d,(hl) : inc hl

ShortOffset:	and #0F : add 3 : cp 15+3 : call z,ReadLongBA

		ld c,a
		ex (sp),hl : push hl						; BC = len, DE = offset, HL = dest, SP ->[dest,src]
		scf : sbc hl,de : pop de					; BC = len, DE = dest, HL = dest-offset, SP->[src]
		ldir : pop hl							; BC = 0, DE = dest, HL = src
		jr ReadToken

ReadLongBA:	ld c,(hl) : inc hl
		add c : jr nc,$+3 : inc b
		inc c : jr z,.Code255
		inc c : ret nz

.Code254	add (hl) : inc hl : ret nc : inc b : ret

.Code255	ld c,(hl) : inc hl : ld b,(hl) : inc hl

		ld a,b : or c : ld a,c : ret nz
		pop de : pop de : ret
